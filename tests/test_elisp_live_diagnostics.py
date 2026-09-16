"""A pending live check must permit diagnosis without accepting completion."""

import base64
import importlib.util
import json
import os
import shlex
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location(
    "elisp_diagnostics", ROOT / "claude/hooks/lib-elisp-diagnostics.py")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class InspectionCommands(unittest.TestCase):
    def test_recovery_tests_and_status_queries_are_narrow(self):
        tests = [f"python3 -I -B {shlex.quote(str(ROOT / 'tests' / name))}"
                 for name in ('test_elpaca_rebuild_protocol.py', 'test_elisp_live_diagnostics.py')]
        expression = ('(let (rows) (maphash (lambda (token status) '
                      '(when (eq (plist-get status :package) (quote files-extras)) '
                      '(push (list token (plist-get status :state)) rows))) '
                      'elpaca-extras--build-reload-statuses) (seq-take rows 10))')
        for command in tests + ["emacsclient -e " + shlex.quote(expression),
                               'emacsclient -e \'(elpaca-extras-build-reload-status "files-extras-1")\'']:
            self.assertTrue(MODULE.inspection_command(command), command)
        for command in (
            tests[0] + ' extra', tests[0].replace('-I ', ''),
            tests[0].replace(str(ROOT), '/tmp/other'),
            tests[0].replace('test_elpaca_rebuild_protocol.py', 'arbitrary.py'),
            'python3 -I -B -c "print(1)"',
            "emacsclient -e " + shlex.quote(expression.replace('(seq-take rows 10)', '(delete-file "file")')),
            'emacsclient -e \'(elpaca-extras-rebuild-and-reload (quote files-extras))\'',
            'emacsclient -e \'(progn (elpaca-extras-build-reload-status "x") (delete-file "file"))\'',
        ):
            self.assertFalse(MODULE.inspection_command(command), command)

    def test_literal_diagnostics(self):
        for command in (
            "cat '/tmp/source with spaces.el'", "head -100 source.el",
            "tail -30 build.log", "wc -l source.el", "ls -l /tmp",
            "stat source.el", "ps -p 79361 -o pid=,command=", "pgrep Emacs",
            "grep -n 'rebuild' source.el", "rg --no-config -n 'callback' source.el",
            "sed -n '110,145p' source.el",
            "git --no-pager --no-optional-locks -c core.fsmonitor=false -C '/tmp/a b' status --short",
            "git --no-pager --no-optional-locks -c core.fsmonitor=false diff --no-ext-diff --no-textconv -- source.el",
            "git --no-pager log --no-ext-diff --no-textconv -3 --oneline",
            "git --no-pager --no-optional-locks -c core.fsmonitor=false ls-files",
            "git --no-pager rev-parse HEAD",
        ):
            with self.subTest(command=command):
                self.assertTrue(MODULE.inspection_command(command))

    def test_stash_operations_reach_a_clean_tree_without_running_code(self):
        for command in (
            "git stash", "git stash push -u -m 'audit fixes awaiting commit' -- '*.el'",
            "git -C '/tmp/a b' stash push --keep-index -- lisp/example.el",
            "git stash pop", "git stash apply --index", "git stash drop",
            "git stash list", "git --no-pager stash show --stat",
        ):
            with self.subTest(command=command):
                self.assertTrue(MODULE.inspection_command(command))
        for command in (
            "git stash push --patch", "git stash branch topic", "git stash clear",
            "git stash pop stash@{1}", "git stash push; touch saved.el",
            "git stash push -m \"$(touch saved.el)\"", "git stash create",
        ):
            with self.subTest(command=command):
                self.assertFalse(MODULE.inspection_command(command))

    def test_mutations_and_ambiguous_forms_stay_blocked(self):
        for command in (
            "cat source.el > saved.el", "cat source.el; touch saved.el",
            "cat source.el | sh", "cat $(touch saved.el)", "cat `touch saved.el`",
            "cat source.el\ntouch saved.el", "env cat source.el", "sh -c 'cat source.el'",
            "python3 -c 'print(1)'", "git commit -am fix", "git branch -D work",
            "git -c alias.inspect='!touch saved.el' inspect", "git diff --output=saved.el",
            "git diff --no-ext-diff --no-textconv --output saved.el",
            "git diff --no-ext-diff --no-textconv --ext-diff",
            "git diff --no-ext-diff --no-textconv --textconv", "git show HEAD",
            "rg --no-config --pre 'touch saved.el' x", "rg --no-config --pre=sh x",
            "rg --no-config --hostname-bin=sh x", "rg --no-config -z x",
            "rg x source.el", "sed -n '1w saved.el' source.el", "sed -i '' source.el",
            "sed -n '1p; e touch saved.el' source.el", "sort -o saved.el source.el",
            "/tmp/arbitrary/cat source.el", "printf '%s' elisp-live-verify",
            "cat 'unterminated", "",
            "rg -- --no-config", "rg -e --no-config source.el",
            "git --no-pager log -- --no-ext-diff --no-textconv",
            "git --no-pager log --format --no-ext-diff --no-textconv",
            "git log --no-ext-diff --no-textconv",
            "git status --short", "git --no-pager status --short",
            "git --no-pager -c core.fsmonitor=false status --short",
            "git --no-pager --no-optional-locks status --short",
            "git --no-pager --no-optional-locks -c core.fsmonitor=sh status",
            "git -C --no-pager log --no-ext-diff --no-textconv",
            "cat *.el", "cat ~/source.el", "cat source{1,2}.el",
        ):
            with self.subTest(command=command):
                self.assertFalse(MODULE.inspection_command(command))

    def test_nested_tool_references_are_lexed(self):
        good = 'const r = await tools.exec_command({cmd:"cat source.el"}); text(r);'
        self.assertTrue(MODULE.nested_inspection_source(good))
        self.assertTrue(MODULE.nested_inspection_source(
            good + ' text("tools.apply_patch"); /* tools . apply_patch() */'))
        for suffix in (
            'await tools . apply_patch("fixture");',
            'await tools/*gap*/.apply_patch("fixture");',
            'await tools["apply_patch"]("fixture");',
            'const alias = tools; alias.apply_patch("fixture");',
            'const {apply_patch} = tools;',
            'const alias = tools.exec_command;',
            'await tools . exec_command({cmd:"cat source.el"});',
            'text(`${tools.apply_patch("fixture")}`);',
            'globalThis["tools"]["apply_patch"]("fixture");',
        ):
            with self.subTest(suffix=suffix):
                self.assertFalse(MODULE.nested_inspection_source(good + suffix))

    def test_pending_identity_and_malformed_state_are_distinct(self):
        with tempfile.TemporaryDirectory() as directory:
            marker = Path(directory) / "pending"
            encode = lambda value: base64.b64encode(value.encode()).decode()
            marker.write_text(f"{encode('/tmp/package')}:{'a' * 40}:{encode('ebib-extras')}\n")
            self.assertIn("ebib-extras at aaaaaaaaaaaa in /tmp/package", MODULE.pending_description(str(marker)))
            marker.write_text("malformed:state\n")
            self.assertIn("needs inspection", MODULE.pending_description(str(marker)))

    def test_paired_helpers_match(self):
        self.assertEqual((ROOT / "claude/hooks/lib-elisp-diagnostics.py").read_bytes(),
                         (ROOT / "codex/hooks/lib-elisp-diagnostics.py").read_bytes())


class PendingGate(unittest.TestCase):
    def setUp(self):
        self.session = f"diagnostic-gate-{os.getpid()}-{self._testMethodName}"
        self.marker = Path(f"/tmp/claude-elisp-verify-needed-{self.session}")
        encode = lambda value: base64.b64encode(value.encode()).decode()
        self.pending = f"{encode(str(ROOT))}:{'a' * 40}:{encode('ebib-extras')}\n"
        self.marker.write_text(self.pending)
        self.addCleanup(self.marker.unlink, missing_ok=True)

    def gate(self, tool, command, nested=False):
        payload = {"session_id": self.session, "cwd": str(ROOT)}
        if nested:
            payload.update(tool_name="functions.exec", tool_input=
                           "const r = await tools.exec_command(" + json.dumps({"cmd": command}) + "); text(r.output);")
        elif tool == "claude":
            payload.update(tool_name="Bash", tool_input={"command": command})
        else:
            payload.update(tool_name="exec_command", tool_input={"cmd": command})
        result = subprocess.run(
            ["bash", str(ROOT / tool / "hooks/require-elisp-verify-after-commit.sh")],
            input=json.dumps(payload), text=True, capture_output=True, check=True,
        )
        self.assertEqual(self.marker.read_text(), self.pending)
        return result.stdout

    def test_read_only_diagnosis_is_allowed_with_debt_retained(self):
        for command in (
            "cat source.el", "rg --no-config -n callback source.el",
            "git --no-pager --no-optional-locks -c core.fsmonitor=false status --short",
            "elisp-live-verify ebib-extras -- '(ebib-extras-example)'",
        ):
            for tool, nested in (("claude", False), ("codex", False), ("codex", True)):
                with self.subTest(tool=tool, nested=nested, command=command):
                    self.assertEqual(self.gate(tool, command, nested), "")

    def test_stash_is_allowed_with_debt_retained(self):
        for command in ("git stash push -u -m parked -- '*.el'", "git stash pop"):
            for tool, nested in (("claude", False), ("codex", False), ("codex", True)):
                with self.subTest(tool=tool, nested=nested, command=command):
                    self.assertEqual(self.gate(tool, command, nested), "")

    def test_recovery_commands_leave_pending_identity_unchanged(self):
        command = f"python3 -I -B {shlex.quote(str(ROOT / 'tests/test_elpaca_rebuild_protocol.py'))}"
        status = 'emacsclient -e \'(elpaca-extras-build-reload-status "files-extras-1")\''
        for tool, nested in (("claude", False), ("codex", False), ("codex", True)):
            for allowed in (command, status):
                self.assertEqual(self.gate(tool, allowed, nested), "")
            for denied in (command + ' extra', command.replace(str(ROOT), '/tmp/other'),
                           status.replace('build-reload-status', 'rebuild-and-reload')):
                self.assertIn('permissionDecision', self.gate(tool, denied, nested))

    def test_mutation_denial_names_outstanding_work(self):
        for tool, nested in (("claude", False), ("codex", False), ("codex", True)):
            with self.subTest(tool=tool, nested=nested):
                output = self.gate(tool, "touch saved.el", nested)
                self.assertIn("permissionDecision", output)
                self.assertIn("ebib-extras", output)
                self.assertIn("aaaaaaaaaaaa", output)

    def test_composed_mutation_and_forged_helper_mention_are_denied(self):
        for command in ("cat source.el; touch saved.el", "printf '%s' elisp-live-verify"):
            for tool in ("claude", "codex"):
                self.assertIn("permissionDecision", self.gate(tool, command))

    def test_safety_switches_must_be_options_at_live_gate(self):
        for command in ("rg -- --no-config", "rg -e --no-config source.el",
                        "git --no-pager log -- --no-ext-diff --no-textconv",
                        "git log --no-ext-diff --no-textconv", "git status"):
            for tool, nested in (("claude", False), ("codex", False), ("codex", True)):
                self.assertIn("permissionDecision", self.gate(tool, command, nested))

    def test_mixed_other_tools_do_not_inherit_diagnostic_permission(self):
        for suffix in ('await tools . apply_patch("fixture");',
                       'await tools/*gap*/.apply_patch("fixture");',
                       'const alias = tools; alias.apply_patch("fixture");'):
            payload = {"session_id": self.session, "cwd": str(ROOT),
                       "tool_name": "functions.exec", "tool_input":
                       'await tools.exec_command({cmd:"cat source.el"});' + suffix}
            result = subprocess.run(
                ["bash", str(ROOT / "codex/hooks/require-elisp-verify-after-commit.sh")],
                input=json.dumps(payload), text=True, capture_output=True, check=True)
            self.assertIn("permissionDecision", result.stdout)
            self.assertEqual(self.marker.read_text(), self.pending)

    def test_active_claude_dispatcher_permits_inspection_without_clearing_debt(self):
        payload = {"session_id": self.session, "tool_name": "Bash",
                   "tool_input": {"command": "cat source.el", "workdir": str(ROOT)}}
        result = subprocess.run(
            ["bash", str(ROOT / "claude/hooks/pretooluse-bash.sh")],
            input=json.dumps(payload), text=True, capture_output=True, check=True,
        )
        self.assertNotIn("permissionDecision", result.stdout)
        self.assertEqual(self.marker.read_text(), self.pending)


if __name__ == "__main__":
    unittest.main()
