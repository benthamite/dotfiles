import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, CODEX, Call  # noqa: E402
from agent_guard.engine import decide as engine_decide  # noqa: E402
from agent_guard.policies import elisp_cookie  # noqa: E402

COOKIE = ";;; -*- lexical-binding: t -*-"


def decision(runtime, tool, tool_input):
    return elisp_cookie.decide(Call.from_payload(runtime, {"tool_name": tool, "tool_input": tool_input})).decision


def bash(command, runtime=CLAUDE):
    return decision(runtime, "Bash", {"command": command})


class HeredocTests(unittest.TestCase):
    # The 2026-11-13 agent-emacs session's commands, shortened.
    def test_quoted_delimiter_without_cookie_denied(self):
        self.assertEqual(bash("S=/tmp/s; cat > $S/tg-send.el <<'EOF'\n(defun my-tg-send (f) f)\nEOF\n"
                              "emacsclient --eval \"(load \\\"$S/tg-send.el\\\")\""), "deny")

    def test_unquoted_delimiter_without_cookie_denied(self):
        self.assertEqual(bash("cat > $S/poll.el <<EOF\n(let* ((x 1)) x)\nEOF"), "deny")

    def test_cookie_allows(self):
        self.assertEqual(bash(f"cat > $S/poll.el <<'EOF'\n{COOKIE}\n(let* ((x 1)) x)\nEOF"), "allow")
        self.assertEqual(bash("cat > a.el <<'EOF'\n;;; a.el --- x  -*- lexical-binding: nil; -*-\nEOF"), "allow")

    def test_cookie_after_shebang_allows(self):
        self.assertEqual(bash(f"cat > a.el <<'EOF'\n#!/usr/bin/env emacs --script\n{COOKIE}\nEOF"), "allow")

    def test_cookie_on_second_line_without_shebang_denied(self):
        self.assertEqual(bash(f"cat > a.el <<'EOF'\n(foo)\n{COOKIE}\nEOF"), "deny")

    def test_redirect_after_heredoc_and_tee(self):
        self.assertEqual(bash("cat <<'EOF' > \"x/a.el\"\n(foo)\nEOF"), "deny")
        self.assertEqual(bash("tee a.el <<'EOF' >/dev/null\n(foo)\nEOF"), "deny")

    def test_dash_heredoc_strips_tabs(self):
        self.assertEqual(bash(f"cat > a.el <<-EOF\n\t{COOKIE}\n\tEOF"), "allow")

    def test_not_checked(self):
        self.assertEqual(bash("cat >> a.el <<'EOF'\n(foo)\nEOF"), "allow")
        self.assertEqual(bash("tee -a a.el <<'EOF'\n(foo)\nEOF"), "allow")
        self.assertEqual(bash("cat > a.elc <<'EOF'\n(foo)\nEOF"), "allow")
        self.assertEqual(bash("cat > notes.txt <<'EOF'\nsee a.el\nEOF"), "allow")
        self.assertEqual(bash("emacsclient --eval '(load \"/tmp/a.el\")' 2>/dev/null"), "allow")

    def test_codex_shell(self):
        self.assertEqual(bash("cat > a.el <<'EOF'\n(foo)\nEOF", CODEX), "deny")


class WriteTests(unittest.TestCase):
    def test_write(self):
        self.assertEqual(decision(CLAUDE, "Write", {"file_path": "/tmp/a.el", "content": "(foo)\n"}), "deny")
        self.assertEqual(decision(CLAUDE, "Write", {"file_path": "/tmp/a.el", "content": f"{COOKIE}\n(foo)"}), "allow")
        self.assertEqual(decision(CLAUDE, "Write", {"file_path": "/tmp/a.org", "content": "(foo)"}), "allow")

    def test_edit_not_checked(self):
        self.assertEqual(decision(CLAUDE, "Edit", {"file_path": "a.el", "old_string": "a", "new_string": "b"}), "allow")

    def test_codex_add_file(self):
        bad = "*** Begin Patch\n*** Add File: a.el\n+(foo)\n*** End Patch"
        good = f"*** Begin Patch\n*** Add File: a.el\n+{COOKIE}\n+(foo)\n*** End Patch"
        update = "*** Begin Patch\n*** Update File: a.el\n@@\n-(a)\n+(b)\n*** End Patch"
        self.assertEqual(decision(CODEX, "apply_patch", {"patch": bad}), "deny")
        self.assertEqual(decision(CODEX, "apply_patch", {"patch": good}), "allow")
        self.assertEqual(decision(CODEX, "apply_patch", {"patch": update}), "allow")


class EngineTests(unittest.TestCase):
    def test_registered_for_claude_bash(self):
        call = Call.from_payload(CLAUDE, {"tool_name": "Bash",
                                          "tool_input": {"command": "cat > /tmp/x/a.el <<'EOF'\n(foo)\nEOF"}})
        result = engine_decide(call)
        self.assertEqual(result.decision, "deny")
        self.assertIn("lexical-binding", result.reason)


if __name__ == "__main__":
    unittest.main()
