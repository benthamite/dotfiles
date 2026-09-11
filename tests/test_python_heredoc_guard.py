"""Synthetic guard inputs; only the known document edit runs in owned fixtures."""

import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
HELPERS = [ROOT / provider / "hooks/lib-python-heredoc.py" for provider in ("claude", "codex")]
ORIGINAL_DENIED_COMMAND = """PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=tests python3 - <<'PY'
import io
from contextlib import redirect_stdout
from types import SimpleNamespace
from unittest import mock
import test_orchestrate_review as t
case = t.StageAtomicRunTests()
case.setUp()
try:
    case.create_run()
    def ambiguous(*args, **kwargs):
        case.acknowledge(*args, **kwargs)
        raise t.orchestrator.EmacsClientError('fixture transport error')
    with mock.patch.object(t.orchestrator.session, 'submit_to_agent', side_effect=ambiguous):
        try:
            t.orchestrator.submit(case.submit_args('spec'))
        except t.orchestrator.EmacsClientError:
            pass
    real_marker = t.orchestrator.session._marker_delivered
    def replace_after_read(*args, **kwargs):
        delivered = real_marker(*args, **kwargs)
        new_file = case.directory / 'replacement-during-receipt.jsonl'
        new_file.write_bytes(case.agent1_transcript.read_bytes())
        new_file.replace(case.agent1_transcript)
        return delivered
    with mock.patch.object(t.orchestrator.session, '_marker_delivered', side_effect=replace_after_read), redirect_stdout(io.StringIO()):
        t.orchestrator.reconcile_submission(SimpleNamespace(run_file=str(case.run_file), delivered=True))
    print({'reconciliation_after_same_path_replacement': t.orchestrator.load_run(case.run_file)['status']})
finally:
    case.doCleanups()
PY
"""
DOCUMENT_EDIT_BODY = '''import json
from pathlib import Path
p=Path('architectural-issues.md');s=p.read_text();old="  Independent112-test and root67-test selections passed. No rebuild/load.\\n";assert old in s
s=s.replace(old,"  Independent112-test and root67-test selections passed. A later distinct-take\\n  census exposed two suffix bounds missed by the initial guard: Babo... Zeira...\\n  2000173586 and Ceferino200018516 print numeric date followed by `or earlier`.\\n  The guard now recognizes that bound too. Independent full comparison changes\\n  only those two rows (102 to104 unresolved), preserving all earlier unresolved\\n  rows and338 year rows;19 independent and37 root focused tests pass. No load.\\n");p.write_text(s)
p=Path('backend/data/reconciliation/case_verdicts/2026-09-08-continuous/rie-payaso-dahr-qualified-dates.json');r=json.loads(p.read_text());r['suffix_bound_followup']={'reviewer':'dahr_suffix_bound_review','source_ids':['2000173586','200018516'],'raw_dates':['11/18/1931 or earlierRio de Janeiro, Brazil [unconfirmed]','9/6/1934 or earlierBuenos Aires, Argentina [unconfirmed]'],'invariant':'A numeric date followed by or earlier remains a bound even when another take has an exact date.','production_rows':3847,'additional_changed_rows':2,'unresolved_before':102,'unresolved_after':104,'all_prior_unresolved_and_338_year_rows_unchanged':True,'complete_take_evidence_preserved':True,'independent_tests':19,'root_tests':37,'date_identity_decisions':False,'rebuild_or_load':False};p.write_text(json.dumps(r,ensure_ascii=False,indent=2)+'\\n')'''
DOCUMENT_UPDATE_BODY = '''from pathlib import Path
import json
path = Path('fixture.md')
content = path.read_text()
old = 'Checks remain pending.'
new = 'The tests pass.'
assert old in content
path.write_text(content.replace(old, new))
report_path = Path('fixture.json')
report = json.loads(report_path.read_text())
report.update({'status': 'The tests pass.'})
report_path.write_text(json.dumps(report, indent=2) + '\\n')'''
spec = importlib.util.spec_from_file_location("python_heredoc_policy", HELPERS[0])
policy = importlib.util.module_from_spec(spec)
spec.loader.exec_module(policy)


def heredoc(body, prefix="python3 -", delimiter="'PY'", suffix="", strip=False):
    name = delimiter.strip("\"'")
    return f"{prefix} <<{'-' if strip else ''}{delimiter}\n{body}\n{name}{suffix}"


class PythonProjectionTests(unittest.TestCase):
    def test_paired_helpers_are_identical(self):
        self.assertEqual(HELPERS[0].read_bytes(), HELPERS[1].read_bytes())

    def test_only_pass_ast_spans_are_exempt_and_outside_bytes_are_preserved(self):
        body = "try:\n    raise ValueError\nexcept ValueError:\n    pass"
        self.assertEqual(policy._project_body(body, False), body.replace("    pass", "        "))
        command = heredoc(body, suffix="\nprintf done\n")
        masked = bytes(10 if byte == 10 else 32 for byte in body.encode()).decode()
        self.assertEqual(policy.project_command(command), heredoc(masked, suffix="\nprintf done\n"))

    def test_utf8_byte_columns_nested_blocks_and_inline_statements(self):
        body = '@decorator\ndef outer():\n    def inner():\n        name = "é🌱"; pass\n    pass'
        self.assertEqual(policy._project_body(body, False), body.replace("pass", "    "))

    def test_tab_stripped_heredoc_preserves_original_layout(self):
        command = heredoc("\tif True:\n\t\tpass", strip=True)
        # <<- strips every leading tab, so this body is invalid Python and
        # must stay unchanged, not be guessed into another indentation level.
        self.assertEqual(policy.project_command(command), command)
        body = "\tif True:\n\t    pass"
        self.assertEqual(policy._project_body(body, True), body.replace("pass", "    "))

    def test_strings_multiline_strings_and_comments_are_not_pass_nodes(self):
        for body in ('text = "pass"', 'text = """first\npass\nlast"""',
                     '# tests pass\npass', 'text = "\\x70ass"', 'text = "pa" "ss"'):
            with self.subTest(body=body):
                with self.assertRaises(policy.ProtectedPythonReference):
                    policy.project_command(heredoc(body))

    def test_unknown_malformed_unclosed_and_ambiguous_heredocs_are_unchanged(self):
        commands = [heredoc("if :\n    pass"), heredoc("pass", delimiter="PY"),
                    heredoc("pass", prefix="bash"), heredoc("pass", prefix="sudo python3 -"),
                    heredoc("pass", prefix="python3 -c something"),
                    "python3 - <<'PY'\npass", "echo before\n" + heredoc("pass"),
                    "bash <<'SH'; " + heredoc("pass"),
                    "echo '" + heredoc("pass") + "'"]
        for command in commands:
            with self.subTest(command=command):
                self.assertEqual(policy.project_command(command), command)

    def test_consecutive_bodies_and_python_shift_are_projected(self):
        command = heredoc("value = 1 << 2\npass") + "\n\n" + heredoc("pass", delimiter="'OTHER'")
        self.assertEqual(policy.project_command(command),
                         heredoc(" " * 14 + "\n    ") + "\n\n" + heredoc("    ", delimiter="'OTHER'"))

    def test_unknown_shell_tail_cannot_expose_a_fake_python_opener(self):
        tail = "\nbash <<'SH'\n" + heredoc("pass") + "\nSH"
        self.assertEqual(policy.project_command(heredoc("pass") + tail), heredoc("    ") + tail)

    def test_shell_text_after_the_terminator_is_never_projected(self):
        for tail in ("\npass", "\nsecurity find-generic-password -w -s fixture", "\npbpaste"):
            command = heredoc("pass", suffix=tail)
            self.assertEqual(policy.project_command(command), heredoc("    ", suffix=tail))


class NativePythonHeredocGuardTests(unittest.TestCase):
    def setUp(self):
        self.scratch = tempfile.TemporaryDirectory(prefix="python-heredoc-guard-", dir="/tmp")
        self.addCleanup(self.scratch.cleanup)
        self.directory = Path(self.scratch.name)

    def guard(self, provider, command, tool="Bash", hook=None):
        if tool == "functions.exec":
            values = {"input": "text(await tools.exec_command({cmd: " + json.dumps(command) + "}));"}
        else:
            values = {"command" if tool == "Bash" else "cmd": command}
        result = subprocess.run(["/bin/bash", str(hook or ROOT/provider/"hooks/block-secret-leak.sh")],
            input=json.dumps({"tool_name": tool, "tool_input": values, "cwd": str(self.directory)}),
            text=True, capture_output=True, timeout=15, cwd=self.directory)
        self.assertEqual(result.returncode, 0, result.stderr)
        output = json.loads(result.stdout) if result.stdout.strip() else {}
        return output.get("hookSpecificOutput", {}).get("permissionDecision", "allow")

    def assert_hooks(self, command, expected):
        for provider, tool in (("claude", "Bash"), ("codex", "Bash"),
                               ("codex", "functions.exec_command"), ("codex", "functions.exec")):
            with self.subTest(provider=provider, tool=tool, command=command):
                self.assertEqual(self.guard(provider, command, tool), expected)

    def test_original_incident_shape_and_minimal_pass_are_allowed(self):
        self.assert_hooks(heredoc("try:\n    raise ValueError\nexcept ValueError:\n    pass"), "allow")
        self.assert_hooks(heredoc(
            "from unittest import mock\ntry:\n    with mock.patch.object(object, 'method'):\n"
            "        try:\n            raise ValueError\n        except ValueError:\n            pass\n"
            "finally:\n    print('fixture complete')",
            prefix="PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=tests python3 -"), "allow")

    def test_exact_original_denied_command_with_varargs_is_allowed(self):
        self.assert_hooks(ORIGINAL_DENIED_COMMAND, "allow")

    def test_consecutive_tangodb_python_heredocs_are_allowed(self):
        command = heredoc("print('fixture first')", prefix="PYENV_VERSION=3.11.9 pyenv exec python -")
        command += "\n" + heredoc(
            "import json\npaths=[]\n"
            "for p in paths:\n print('\\nFILE',p)\n"
            " try:walk(json.load(open('data/'+p)))\n except FileNotFoundError:pass")
        self.assert_hooks(command, "allow")

    def test_consecutive_heredocs_keep_protected_references_and_shell_tails_denied(self):
        safe = heredoc("pass")
        unsafe = heredoc('import os\nos.system("\\x70ass")')
        for command in (safe + "\n" + unsafe, unsafe + "\n" + safe,
                        safe + "\n" + safe + "\npass show fixture",
                        safe + "\nbash <<'SH'\n" + heredoc("pass") + "\nSH",
                        safe + "\n" + heredoc("if :\n pass"),
                        safe + "\npython3 - <<'PY'\npass"):
            self.assert_hooks(command, "deny")

    def test_pyenv_exec_python_source_is_not_outer_shell_syntax(self):
        for prefix in ("pyenv exec python -", "/opt/homebrew/bin/pyenv exec python3 -B -",
                       "env PYENV_VERSION=3.11.9 pyenv exec python -"):
            for body in ("def flush(self):\n    pass",
                         "items = [1, 2]\nprint([item for item in items])"):
                self.assert_hooks(heredoc(body, prefix=prefix), "allow")

    def test_pyenv_tangodb_read_only_count_command(self):
        command = """pyenv exec python - <<'PY'
import json
from pathlib import Path
base=Path('backend/data/reconciliation/case_verdicts/2026-09-08-continuous')
ids={v.get('pair_id') for p in base.glob('*.json') if (v:=json.loads(p.read_text())).get('pair_id')}
merges=json.loads(Path('backend/data/reconciliation/recording_merges.json').read_text())['merges']
print('Continuous-session merge corrections:',sum(m.get('judgment',{}).get('pair_id') in ids for m in merges))
print('Verdict artifacts:',len(list(base.glob('*.json'))))
PY"""
        self.assert_hooks(command, "allow")

    def test_pyenv_exec_keeps_credential_and_unknown_interpreter_checks(self):
        for body in ('import subprocess\nsubprocess.run(["pass", "show", "fixture"])',
                     'import os\nos.system("\\x70ass")'):
            self.assert_hooks(heredoc(body, prefix="pyenv exec python -"), "deny")
        for prefix in ("pyenv exec bash", "pyenv exec python -c something",
                       "pyenv exec $PY -", "pyenv which python -"):
            self.assert_hooks(heredoc("pass", prefix=prefix), "deny")
        self.assert_hooks(heredoc("pass", prefix="pyenv exec python -", suffix="\npbpaste"), "deny")

    def test_literal_prefixes_quoted_delimiters_and_nested_pass_blocks_are_allowed(self):
        body = '@decorator\ndef outer():\n    name = "é🌱"; pass\n    def inner():\n        pass'
        for prefix in ("python3", "/usr/bin/python3 -B -", "env PYTHONPATH=tests python3 -",
                       "env -i -u FIXTURE PYTHONPATH=tests python3 -", "command python3 -"):
            for delimiter in ("'PY'", '"PY"'):
                with self.subTest(prefix=prefix, delimiter=delimiter):
                    self.assert_hooks(heredoc(body, prefix=prefix, delimiter=delimiter), "allow")

    def test_python_subprocess_os_and_exec_credential_literals_are_denied(self):
        for name in ("pass", "security", "pbpaste"):
            for body in (f'import subprocess\nsubprocess.run(["{name}"])',
                         f'import os\nos.system("{name}")',
                         f'import os\nos.execvp("{name}", ["{name}"])',
                         f'exec("import os; os.system(\\\"{name}\\\")")'):
                with self.subTest(name=name, body=body):
                    self.assert_hooks(heredoc(body), "deny")
                    self.assert_hooks(heredoc("pass\n" + body), "deny")

    def test_decoded_and_adjacent_python_string_literals_are_denied(self):
        for body in ('import os\nos.system("\\x70ass")',
                     'import os\nos.system("pa" "ss")'):
            self.assert_hooks(heredoc(body), "deny")

    def test_nested_astral_unicode_does_not_skip_credential_classification(self):
        command = heredoc('import subprocess\ntext = "🌱"\nsubprocess.run(["pass"])')
        self.assert_hooks(command, "deny")

    def test_invalid_nested_unicode_and_nul_are_denied(self):
        for field in ("cmd", "workdir"):
            for value in ("\ud800", "\udc00", "a\0b"):
                with self.subTest(field=field, codepoints=[ord(char) for char in value]):
                    values = {"cmd": "true", field: value}
                    program = "text(await tools.exec_command(" + json.dumps(values) + "));"
                    result = subprocess.run(["/bin/bash", str(ROOT/"codex/hooks/block-secret-leak.sh")],
                        input=json.dumps({"tool_name": "functions.exec", "tool_input": {"input": program}}),
                        text=True, capture_output=True, timeout=15, cwd=self.directory)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(json.loads(result.stdout)["hookSpecificOutput"]["permissionDecision"], "deny")
                    self.assertEqual(result.stderr, "")

    def test_failed_or_invalid_extraction_refuses_even_after_partial_output(self):
        copied = self.directory / "copied-codex"
        copied.mkdir()
        for name in ("block-secret-leak.sh", "lib-heredoc.sh", "lib-python-heredoc.py",
                     "lib-codex-paths.sh", "lib-codex-hook-json.sh"):
            shutil.copyfile(ROOT/"codex/hooks"/name, copied/name)
        library = copied/"lib-codex-hook-json.sh"
        original = library.read_text()
        record = json.dumps({"cmd": "true", "workdir": None, "ambiguous": False})
        for output, status in (("", 7), (record, 7), ("{broken", 0),
                               (json.dumps({"cmd": [], "workdir": None, "ambiguous": False}), 0)):
            with self.subTest(output=output, status=status):
                library.write_text(original + "\ncodex_nested_exec_contexts() {\n"
                    + "  printf '%s\\0' '" + output + "'\n  return " + str(status) + "\n}\n")
                self.assertEqual(self.guard("codex", "true", "functions.exec",
                                            hook=copied/"block-secret-leak.sh"), "deny")

    def test_cached_commands_preserve_trailing_lf_and_backslash_lf_bytes(self):
        copied = self.directory / "inspect-codex-cache"
        copied.mkdir()
        for name in ("block-secret-leak.sh", "lib-heredoc.sh", "lib-python-heredoc.py",
                     "lib-codex-paths.sh", "lib-codex-hook-json.sh"):
            shutil.copyfile(ROOT/"codex/hooks"/name, copied/name)
        hook = copied/"block-secret-leak.sh"
        # Stop this owned hook copy after real extraction/validation/caching.
        # Inspect NUL-framed bytes instead of executing any proposed command.
        marker = "# --- Allowlist: commands that do not return secret-manager output ---"
        inspector = ('for secret_cached in "${SECRET_NESTED_COMMANDS[@]}"; do\n'
                     '  printf "%s\\0" "$secret_cached"\ndone\nexit 0\n')
        source = hook.read_text()
        self.assertEqual(source.count(marker), 1)
        hook.write_text(source.replace(marker, inspector + marker))
        commands = ["printf fixture\\\n", "printf fixture\n\n\n", "printf é🌱\n", "", "printf dot."]
        program = "\n".join("await tools.exec_command(" + json.dumps({"cmd": command}) + ");"
                            for command in commands)
        result = subprocess.run(["/bin/bash", str(hook)],
            input=json.dumps({"tool_name": "functions.exec", "tool_input": {"input": program}}).encode(),
            capture_output=True, timeout=15, cwd=self.directory)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, b"")
        self.assertEqual(result.stdout, b"".join(command.encode() + b"\0" for command in commands))

    def test_safe_data_mentions_remain_conservatively_denied(self):
        for body in ('print("pass")', 'text = """first\npass\nlast"""', '# pass is a keyword\npass'):
            self.assert_hooks(heredoc(body), "deny")

    def test_reported_document_and_json_edit_is_allowed_and_runs_only_in_fixture(self):
        command = heredoc(DOCUMENT_EDIT_BODY,
                          prefix="/Users/pablostafforini/.pyenv/versions/3.11.9/bin/python -")
        self.assert_hooks(command, "allow")
        document = self.directory / "architectural-issues.md"
        document.write_text("  Independent112-test and root67-test selections passed. No rebuild/load.\n")
        evidence = self.directory / "backend/data/reconciliation/case_verdicts/2026-09-08-continuous/rie-payaso-dahr-qualified-dates.json"
        evidence.parent.mkdir(parents=True)
        evidence.write_text('{"preserved": true}\n')
        # The synthetic negative controls below are never executed. This one
        # known document edit executes only against owned temporary files.
        result = subprocess.run(["python3", "-"], input=DOCUMENT_EDIT_BODY,
                                cwd=self.directory, text=True, capture_output=True, timeout=15)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("root focused tests pass. No load.", document.read_text())
        data = json.loads(evidence.read_text())
        self.assertTrue(data["preserved"])
        self.assertEqual(data["suffix_bound_followup"]["unresolved_after"], 104)

    def test_document_literals_and_comments_are_allowed_in_closed_edit_language(self):
        bodies = [
            'from pathlib import Path\nPath("/tmp/documentation-fixture.md").write_text("root focused tests pass. No load.")',
            'from pathlib import Path\n# security and pbpaste are documented here\nPath("docs/pass.md").write_text("pa" "ss", encoding="utf-8")',
            'from pathlib import Path\nPath("docs/security.org").write_text("\\x70ass")',
        ]
        for body in bodies:
            self.assert_hooks(heredoc(body), "allow")

    def test_reported_mapping_update_is_allowed_and_runs_only_in_fixture(self):
        command = heredoc(DOCUMENT_UPDATE_BODY)
        self.assert_hooks(command, "allow")
        program = ('text(await tools.exec_command({cmd:' + json.dumps(command)
                   + ',workdir:' + json.dumps(str(self.directory)) + ',max_output_tokens:1000}));')
        result = subprocess.run(["/bin/bash", str(ROOT/"codex/hooks/block-secret-leak.sh")],
            input=json.dumps({"tool_name": "functions.exec", "tool_input": {"input": program},
                              "cwd": str(self.directory)}), text=True, capture_output=True,
            timeout=15, cwd=self.directory)
        self.assertEqual(result.returncode, 0, result.stderr)
        output = json.loads(result.stdout) if result.stdout.strip() else {}
        self.assertEqual(output.get("hookSpecificOutput", {}).get("permissionDecision", "allow"), "allow")
        document = self.directory / "fixture.md"
        report = self.directory / "fixture.json"
        document.write_text("Checks remain pending.")
        report.write_text('{"preserved": true}\n')
        result = subprocess.run(["python3", "-"], input=DOCUMENT_UPDATE_BODY,
                                cwd=self.directory, text=True, capture_output=True, timeout=15)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(document.read_text(), "The tests pass.")
        self.assertEqual(json.loads(report.read_text()), {"preserved": True, "status": "The tests pass."})

    def test_mapping_update_requires_bound_data_literal_dict_and_statement_context(self):
        prefix = 'from pathlib import Path\nimport json\nreport = {}\n'
        write = '\nPath("fixture.json").write_text(json.dumps(report) + "pass")'
        calls = [
            'report.update(status="pass")', 'report.update({"status": "pass"}, {})',
            'report.update(report)', 'report.update({**report})',
            'report.update({"status": unknown()})', 'unknown.update({"status": "pass"})',
            'json.loads("{}").update({"status": "pass"})',
            'report = unknown\nreport.update({"status": "pass"})',
            'report.update = unknown\nreport.update({"status": "pass"})',
            'update = report.update\nupdate({"status": "pass"})',
            'result = report.update({"status": "pass"})',
            'report.update({"nested": report.update({"status": "pass"})})',
        ]
        for call in calls:
            self.assert_hooks(heredoc(prefix + call + write), "deny")
        # Mutating in-memory data alone must not satisfy the required write.
        self.assert_hooks(heredoc(prefix + 'report.update({"status": "pass"})'), "deny")

    def test_document_exception_rejects_unknown_calls_bindings_and_execution(self):
        safe = 'from pathlib import Path\nPath("fixture.md").write_text("pass")'
        bodies = [
            safe + '\nexec("print(1)")', safe + '\neval("1")',
            safe + '\nunknown()', safe + '\nimport subprocess',
            safe + '\nimport os\nos.system("\\x70ass")',
            'from pathlib import Path\nPath = arbitrary\nPath("fixture.md").write_text("pass")',
            'from pathlib import Path as P\nP("fixture.md").write_text("pass")',
            'from pathlib import *\nPath("fixture.md").write_text("pass")',
            'from pathlib import Path\np=Path("fixture.md")\nf=p.write_text\nf("pass")',
            'import json\nfrom pathlib import Path\njson.loads = eval\nPath("fixture.md").write_text(json.loads("pass"))',
            'import json\nfrom pathlib import Path\nPath("fixture.md").write_text(json.dumps({},default=eval)+"pass")',
            'from pathlib import Path\nPath("fixture.py").write_text("pass")',
            'from pathlib import Path\nPath("fixture.md").write_text(f"pass {unknown()}")',
            'from pathlib import Path\nPath("fixture.md").write_text("pass", **options)',
        ]
        for body in bodies:
            self.assert_hooks(heredoc(body), "deny")

    def test_document_exception_rejects_shell_exterior_and_preserves_secret_checks(self):
        safe = 'from pathlib import Path\nPath("fixture.md").write_text("pass")'
        commands = [heredoc(safe, suffix="\npython3 fixture.md"),
                    heredoc(safe, suffix="\npbpaste"), heredoc(safe) + "\n" + heredoc("pass"),
                    heredoc(safe, prefix="true; python3 -"),
                    heredoc(safe, prefix="PYTHONPATH=/tmp python3 -"),
                    heredoc(safe, delimiter="PY")]
        for command in commands:
            self.assert_hooks(command, "deny")
        marker = "-" * 5 + "BEGIN PRIVATE KEY" + "-" * 5
        self.assert_hooks(heredoc(safe.replace('"pass"', repr(marker + " pass"))), "deny")

    def test_document_exception_requires_one_complete_literal_orchestration(self):
        command = heredoc('from pathlib import Path\nPath("fixture.md").write_text("pass")')
        call = 'await tools.exec_command({cmd: ' + json.dumps(command) + '})'
        quoted = 'await tools.exec_command(' + json.dumps({"cmd": command}) + ')'
        programs = [
            ('text(' + quoted + ');', "allow"),
            (quoted + '; await tools.exec_command({cmd: "true"});', "deny"),
            (quoted.replace('})', ', cmd: "true"})') + ';', "deny"),
            (call.replace('})', ', "cmd": "true"})') + ';', "deny"),
            (quoted.replace('})', ', "max_output_tokens": "1000"})') + ';', "deny"),
            (quoted.replace('})', ', "workdir": 1000})') + ';', "deny"),
            (quoted.replace('})', ', "shell": "/bin/sh"})') + ';', "deny"),
            (quoted.replace('})', ', "yield_time_ms": false})') + ';', "deny"),
            (call + '; await tools.exec_command({cmd: "sh fixture.md"});', "deny"),
            ('await tools.exec_command({cmd: "true"});' + call + ';', "deny"),
            (call + '; unknown();', "deny"),
            ('const payload = {cmd: ' + json.dumps(command) + '}; await tools.exec_command(payload);', "deny"),
            (call.replace('})', ', shell: "/bin/sh"})') + ';', "deny"),
            (call.replace('})', ', unknown: sideEffect()})') + ';', "deny"),
            ('text(' + call.replace('})', ', workdir: "/tmp", max_output_tokens: 1000, yield_time_ms: 1000})') + ');', "allow"),
            (call + ';', "allow"),
        ]
        for program, expected in programs:
            with self.subTest(program=program):
                result = subprocess.run(["/bin/bash", str(ROOT/"codex/hooks/block-secret-leak.sh")],
                    input=json.dumps({"tool_name": "functions.exec", "tool_input": {"input": program},
                                      "cwd": str(self.directory)}), text=True, capture_output=True,
                    timeout=15, cwd=self.directory)
                self.assertEqual(result.returncode, 0, result.stderr)
                output = json.loads(result.stdout) if result.stdout.strip() else {}
                self.assertEqual(output.get("hookSpecificOutput", {}).get("permissionDecision", "allow"), expected)

    def test_shell_and_interpolation_controls_remain_denied(self):
        commands = ["pass show fixture/credential", "security find-generic-password -w -s fixture", "pbpaste",
                    heredoc("pass show fixture/credential", prefix="bash"),
                    heredoc("pass show fixture/credential", prefix="cat") + "\npass",
                    heredoc('text = "$(pass show fixture/credential)"', delimiter="PY"),
                    heredoc('text = "`pbpaste`"', delimiter="PY"),
                    heredoc("pass", suffix="\npass show fixture/credential"),
                    heredoc("pass", suffix="\n$(command -v pbpaste)"),
                    heredoc("pass", suffix="\np?ss show fixture/credential"),
                    heredoc("pass", prefix="sudo python3 -"),
                    heredoc("if :\n    pass")]
        for command in commands:
            self.assert_hooks(command, "deny")

    def test_original_content_still_reaches_secret_value_checks(self):
        # This public header is only a recognizable pattern, not a key.
        marker = "-" * 5 + "BEGIN PRIVATE KEY" + "-" * 5
        self.assert_hooks(heredoc(f"text = {marker!r}\npass"), "deny")

    def test_missing_projection_helper_denies_instead_of_silently_allowing(self):
        for provider in ("claude", "codex"):
            copied = self.directory / provider
            copied.mkdir()
            names = ["block-secret-leak.sh", "lib-heredoc.sh"]
            if provider == "codex":
                names += ["lib-codex-paths.sh", "lib-codex-hook-json.sh"]
            for name in names:
                shutil.copyfile(ROOT/provider/"hooks"/name, copied/name)
            self.assertEqual(self.guard(provider, heredoc("pass"), hook=copied/"block-secret-leak.sh"), "deny")


class NestedUnicodeDecoderTests(unittest.TestCase):
    def extract(self, program, mode):
        return subprocess.run(["/bin/bash", "-c", 'source "$1"; _codex_nested_exec_values "$2"',
            "unicode-decoder-fixture", str(ROOT/"codex/hooks/lib-codex-hook-json.sh"), mode],
            input=program.encode(), capture_output=True, timeout=15)

    def test_literal_and_escaped_bmp_and_astral_text_round_trip_in_both_modes(self):
        command = "printf é🌱"
        workdir = "/tmp/é🌱"
        for ascii_only in (True, False):
            program = "await tools.exec_command(" + json.dumps(
                {"cmd": command, "workdir": workdir}, ensure_ascii=ascii_only) + ");"
            for mode in ("commands", "contexts"):
                with self.subTest(ascii_only=ascii_only, mode=mode):
                    result = self.extract(program, mode)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stderr, b"")
                    if mode == "commands":
                        self.assertEqual(result.stdout, command.encode() + b"\0")
                    else:
                        record = json.loads(result.stdout.removesuffix(b"\0"))
                        self.assertEqual(record, {"cmd": command, "workdir": workdir, "ambiguous": False})

    def test_unpaired_surrogates_and_nul_fail_in_both_modes(self):
        for field in ("cmd", "workdir"):
            for value in ("\ud800", "\udc00", "a\0b"):
                program = "await tools.exec_command(" + json.dumps({"cmd": "true", field: value}) + ");"
                for mode in ("commands", "contexts"):
                    with self.subTest(field=field, mode=mode, codepoints=[ord(char) for char in value]):
                        result = self.extract(program, mode)
                        self.assertNotEqual(result.returncode, 0)
                        self.assertEqual(result.stdout, b"")


if __name__ == "__main__":
    unittest.main()
