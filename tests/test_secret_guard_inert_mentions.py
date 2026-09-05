"""False-positive regressions for the secret-output guard (2026-09-05).

The 2026-08-31 guard denied a protected tool name (`pbpaste`, `pass`,
`security`) anywhere in unquoted command text and scanned heredoc bodies fed
to interpreters as outer-shell command words. That blocked searches such as
`grep -c pbpaste README.md` and Python heredocs whose prefix carried `;`,
`&&` or an expanded argument, or whose body contained `?`, `*` or `[`.

These tests pin the corrected boundary for all three guard entry points:
- inert mentions as arguments of read-only text tools are allowed;
- a name in command-word position, behind xargs/find -exec/env, joined with
  `=`, or inside a shell or interpreter body is still denied;
- Python and other non-shell interpreter bodies are not glob-scanned, but
  their protected names are still scanned.
"""

from __future__ import annotations

import json
import subprocess
import unittest
from pathlib import Path


DOTFILES = Path("/Users/pablostafforini/My Drive/dotfiles")
GUARDS = {
    "claude-standalone": DOTFILES / "claude" / "hooks" / "block-secret-leak.sh",
    "claude-dispatcher": DOTFILES / "claude" / "hooks" / "pretooluse-bash.sh",
    "codex": DOTFILES / "codex" / "hooks" / "block-secret-leak.sh",
}


def run_guard(guard: Path, command: str) -> dict | None:
    payload = json.dumps({"tool_name": "Bash", "tool_input": {"command": command}, "cwd": str(Path.cwd())})
    result = subprocess.run(["bash", str(guard)], input=payload, capture_output=True, text=True, check=True)
    return json.loads(result.stdout) if result.stdout.strip() else None


def decision(output: dict | None) -> str:
    if output is None:
        return "allow"
    return output.get("hookSpecificOutput", {}).get("permissionDecision", "allow")


DRIVE_PROBE = '''S="/tmp/x"; /Users/pablostafforini/.local/share/uv/tools/gdoc/bin/python3 - "$S" <<'EOF'
import sys, json
d = json.load(open(sys.argv[1]))
r = s.get('https://www.googleapis.com/drive/v3/files/abc/revisions/def?alt=media')
print(d['token'], r.status_code, *sys.argv)
EOF'''

ALLOWED = {
    "grep mention": "cd /tmp && grep -c pbpaste README.md",
    "grep -rn pass": "grep -rn pass docs/ | head -5",
    "rg with flags": "rg -n --hidden security ~/notes",
    "git log -S": "git log -S pbpaste --oneline -- claude/hooks",
    "git grep": "git grep -n pass -- '*.md'",
    "wc on a file named after the tool": "wc -l pbpaste.md",
    "ls listing": "ls -la /usr/bin/pbpaste",
    "cat man page copy": "cat docs/pass.md",
    "wrapped grep": "A=1 env FOO=2 grep pass file.txt",
    "python heredoc after sequence": "cd /tmp && python3 - <<'EOF'\nif True:\n    pass\nEOF",
    "python heredoc after assignment": "S=1; python3 - <<'EOF'\nx = [1, 2][0] if 3 * 4 else None\nEOF",
    "python heredoc with expanded script argument": "S=/tmp/x; python3 - \"$S\" <<'EOF'\nimport sys\nprint(sys.argv[1])\nEOF",
    "python heredoc with url query and indexing": DRIVE_PROBE,
    "node heredoc with glob characters": "node <<'EOF'\nconst x = a[0] ? b : c;\nconsole.log(x, '*');\nEOF",
    "sqlite heredoc with glob characters": "sqlite3 db.sqlite <<'EOF'\nselect * from t where k like 'a?%';\nEOF",
    "emacs batch heredoc": "emacs --batch <<'EOF'\n(message \"%s\" (car '(a b)))\nEOF",
    "existing audited clipboard wrapper": "op-clipboard-store --vault Automations --title 'X' --prefix sk-or-v1- --min 40 --max 120",
}

DENIED = {
    "bare": "pbpaste",
    "piped": "pbpaste | tee /tmp/leak",
    "pass show": "pass show fixture/credential",
    "security keychain": "security find-generic-password -w -s fixture",
    "xargs": "printf x | xargs pbpaste",
    "xargs with option": "printf x | xargs -0 pbpaste",
    "find -exec": "find . -name x -exec pbpaste {} \\;",
    "git pager": "git -c core.pager=pbpaste log",
    "git config write": "git config core.pager pbpaste",
    "env wrapper": "env FOO=1 pbpaste",
    "sudo wrapper": "sudo pbpaste",
    "path form": "/usr/bin/pbpaste",
    "bash -c": "bash -c 'pbpaste'",
    "command substitution": "X=$(pbpaste); echo done",
    "bash heredoc body": "bash <<'EOF'\npbpaste\nEOF",
    "bash heredoc glob body": "bash <<'EOF'\np?ss show fixture/credential\nEOF",
    "node heredoc naming the tool": "node <<'EOF'\nrequire('child_process').execSync('pbpaste')\nEOF",
    "python heredoc naming the tool": "cd /tmp && python3 - <<'EOF'\nimport subprocess\nsubprocess.run(['pass', 'show', 'x'])\nEOF",
    "python heredoc via expanded interpreter": "PY=python3; $PY - <<'EOF'\npass\nEOF",
    "python heredoc piped": "cat x | python3 - <<'EOF'\npass\nEOF",
    "unknown program argument": "mytool pbpaste",
    "grep then execution": "grep -c pbpaste README.md; pbpaste",
}


class InertMentionTests(unittest.TestCase):
    def test_inert_mentions_and_interpreter_bodies_are_allowed(self):
        for name, guard in GUARDS.items():
            for label, command in ALLOWED.items():
                with self.subTest(guard=name, case=label):
                    self.assertEqual(decision(run_guard(guard, command)), "allow")

    def test_executable_positions_and_shell_bodies_stay_denied(self):
        for name, guard in GUARDS.items():
            for label, command in DENIED.items():
                with self.subTest(guard=name, case=label):
                    self.assertEqual(decision(run_guard(guard, command)), "deny")


if __name__ == "__main__":
    unittest.main()
