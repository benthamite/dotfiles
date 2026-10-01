import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, CODEX, Call  # noqa: E402
from agent_guard.policies import destructive  # noqa: E402

RM_RF = "rm -" + "rf"


def bash(command, runtime=CLAUDE, tool="Bash"):
    return Call.from_payload(runtime, {"tool_name": tool, "tool_input": {"command": command}, "cwd": "/"})


def label(result):
    return result.reason.split("\n", 1)[0] if result.decision == "deny" else "allow"


class RuleTests(unittest.TestCase):
    def assertLabel(self, command, expected):
        self.assertEqual(label(destructive.decide(bash(command))), expected, command)

    def test_each_rule(self):
        cases = {
            f"{RM_RF} /tmp/x": "BLOCKED: rm -rf detected.",
            "rm -Rf x": "BLOCKED: rm -rf detected.",  # intended: stricter than the hook
            "rm -fR x": "BLOCKED: rm -rf detected.",
            "rm -rF x": "allow",
            "git push -f origin": "BLOCKED: git push --force detected.",
            "git clone u": "BLOCKED: git clone detected.",
            "gh repo clone a/b": "BLOCKED: git clone detected.",
            "git reset --hard": "BLOCKED: git reset --hard detected.",
            "git clean -xdf": "BLOCKED: git clean -f detected.",
            "git checkout -- .": "BLOCKED: git checkout -- . detected.",
            "git branch -D b": "BLOCKED: git branch -D detected.",
            "gh repo edit a --visibility private": "BLOCKED: GitHub repo visibility change detected.",
            "gh repo delete a/b": "BLOCKED: gh repo delete detected.",
            "dropdb x": "BLOCKED: dropdb detected.",
            "bq rm d.t": "BLOCKED: bq rm detected.",
            "aws s3 rm s3://b --recursive": "BLOCKED: aws s3 rm --recursive detected.",
            "op item remove x": "BLOCKED: op item delete detected.",
            "git status": "allow",
        }
        for command, expected in cases.items():
            self.assertLabel(command, expected)

    def test_first_rule_wins_and_reason_text(self):
        result = destructive.decide(bash(f"git reset --hard; {RM_RF} x"))
        self.assertEqual(result.reason, "BLOCKED: rm -rf detected.\n\nUse 'trash' instead of 'rm -rf' to allow "
                         "recovery.\n\nIf you are certain this is the right action, ask the user to run it manually "
                         "with `!`.")

    def test_force_with_lease_exempts_only_its_own_push(self):
        self.assertLabel("git push --force-with-lease", "allow")
        self.assertLabel("git push --force-with-lease -f", "allow")
        # Intended: the hook let a lease anywhere clear every force flag.
        self.assertLabel("git push --force-with-lease && git push -f", "BLOCKED: git push --force detected.")
        self.assertLabel("git push --force-with-lease\ngit push --force", "BLOCKED: git push --force detected.")
        self.assertLabel("git push -f x git push --force-with-lease", "BLOCKED: git push --force detected.")
        self.assertLabel("git push --force-with-lease git push -f", "BLOCKED: git push --force detected.")
        self.assertLabel("git push origin; [ -f x ]", "allow")

    def test_clone_hatch_must_be_unquoted(self):
        self.assertLabel("ALLOW_CLONE=1 git clone u", "allow")
        self.assertLabel("echo 'ALLOW_CLONE=1'; git clone u", "BLOCKED: git clone detected.")

    def test_flags_stay_within_one_segment(self):
        self.assertLabel("aws s3 rm s3://b | grep -r x", "allow")

    def test_matching_is_per_line(self):
        self.assertLabel("git push\n--force", "allow")

    def test_long_command_with_early_match_is_denied(self):
        # Intended difference 15: the hook's `echo | grep -q` under pipefail lost
        # this to SIGPIPE once the text outgrew the pipe buffer.
        self.assertLabel(f"{RM_RF} x\n" + "x\n" * 200_001, "BLOCKED: rm -rf detected.")


class GrepClassTests(unittest.TestCase):
    """BSD grep's \\b uses iswalnum(), \\s the measured space set."""

    def test_word_boundary(self):
        self.assertEqual(label(destructive.decide(bash(f"²{RM_RF} x"))), "BLOCKED: rm -rf detected.")
        self.assertEqual(label(destructive.decide(bash(f"\U00011f02{RM_RF} x"))), "allow")
        self.assertEqual(label(destructive.decide(bash(f"x{RM_RF} x"))), "allow")

    def test_space_classes(self):
        self.assertEqual(label(destructive.decide(bash("rm -rf x"))), "BLOCKED: rm -rf detected.")
        self.assertEqual(label(destructive.decide(bash("rm\u0085-rf x"))), "allow")


class MaskTests(unittest.TestCase):
    def test_quoted_literals_are_blanked(self):
        self.assertEqual(destructive.mask_quoted(f"echo '{RM_RF}' \"{RM_RF}\" $'{RM_RF}'"), "echo '' \"\" $''")

    def test_expanding_double_quotes_are_kept(self):
        self.assertEqual(destructive.mask_quoted('echo "$(x)"'), 'echo "$(x)"')

    def test_interpreter_word_disables_masking(self):
        self.assertEqual(destructive.mask_quoted(f"bash -c '{RM_RF} x'"), f"bash -c '{RM_RF} x'")

    def test_interpreter_check_reads_bracket_backslash_t_literally(self):
        # grep's `[ \t]` is space, backslash or t: a TAB after the word masks, a t does not.
        self.assertEqual(destructive.mask_quoted(f"bashtest '{RM_RF}'"), f"bashtest '{RM_RF}'")
        self.assertEqual(destructive.mask_quoted(f"eval\t'{RM_RF}'"), "eval\t''")

    def test_heredocs(self):
        self.assertEqual(destructive.mask_quoted(f"cat <<'EOF'\n{RM_RF} x\nEOF\ny"), "cat <<'EOF'\nEOF\ny")
        self.assertEqual(destructive.mask_quoted(f"cat <<EOF\n{RM_RF} x\nEOF"), f"cat <<EOF\n{RM_RF} x\nEOF")
        self.assertEqual(destructive.mask_quoted(f"cat <<-'E'\n\t{RM_RF}\n\tE\n"), "cat <<-'E'\n\tE")

    def test_unterminated_input_is_copied_verbatim(self):
        for text in (f"'{RM_RF}", f'"{RM_RF}', f"$'{RM_RF}", f"cat <<'EOF'\n{RM_RF}"):
            self.assertEqual(destructive.mask_quoted(text), text)


class PayloadTests(unittest.TestCase):
    def test_claude_reads_only_bash(self):
        self.assertEqual(destructive.decide(bash(RM_RF, tool="exec_command")).decision, "allow")

    def test_codex_shell_tools_and_fields(self):
        call = Call.from_payload(CODEX, {"tool_name": "exec_command", "tool_input": {"cmd": f"{RM_RF} x"}})
        self.assertEqual(destructive.decide(call).decision, "deny")
        call = Call.from_payload(CODEX, {"tool_name": "apply_patch", "tool_input": {"command": f"{RM_RF} x"}})
        self.assertEqual(destructive.decide(call).decision, "allow")

    def test_dispatcher_check(self):
        self.assertEqual(destructive.dispatcher_check(bash("git status")), [])
        self.assertEqual(len(destructive.dispatcher_check(bash("dropdb x"))), 1)


if __name__ == "__main__":
    unittest.main()
