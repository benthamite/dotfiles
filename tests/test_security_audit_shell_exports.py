from __future__ import annotations

import json
import subprocess
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
CLASSIFIERS = {
    tool: DOTFILES / f"macos/.{tool}/skills/security-audit/scripts/classify-shell-exports.py"
    for tool in ("claude", "codex")
}


class SecurityAuditShellExportTests(unittest.TestCase):
    def classify(self, content: str) -> list[dict[str, object]]:
        outputs = []
        with tempfile.TemporaryDirectory() as directory:
            fixture = Path(directory) / "synthetic-shell-fixture"
            fixture.write_text(content)
            original = fixture.read_bytes()
            for tool, script in CLASSIFIERS.items():
                with self.subTest(tool=tool):
                    result = subprocess.run(
                        ["python3", "-B", str(script), str(fixture)],
                        capture_output=True, text=True, check=False,
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stderr, "")
                    self.assertNotIn("synthetic-value", result.stdout)
                    outputs.append(json.loads(result.stdout))
            self.assertEqual(original, fixture.read_bytes())
            self.assertEqual([fixture], list(Path(directory).iterdir()))
        self.assertEqual(outputs[0], outputs[1])
        return outputs[0]

    def exports(self, content: str):
        findings = self.classify(content)
        self.assertFalse([row for row in findings if row["classification"] == "not-checked"], findings)
        return {row["name"]: row for row in findings}

    def test_multiple_export_assignments_and_names(self):
        rows = self.exports('''\
export DISPLAY_MODE="synthetic-value-mode" FIRST_TOKEN="synthetic-value-one"
SECOND_TOKEN='synthetic-value-two'
THIRD_API_KEY="synthetic-value-three"
export SECOND_TOKEN THIRD_API_KEY
''')
        self.assertEqual(set(rows), {"DISPLAY_MODE", "FIRST_TOKEN", "SECOND_TOKEN", "THIRD_API_KEY"})
        self.assertEqual(rows["DISPLAY_MODE"]["classification"], "non-secret")
        for name in ("FIRST_TOKEN", "SECOND_TOKEN", "THIRD_API_KEY"):
            self.assertEqual(rows[name]["classification"], "credential-literal")
            self.assertEqual(rows[name]["scope"], "global")

    def test_declaration_attributes_and_inline_conditions(self):
        rows = self.exports('''\
typeset -gx FIRST_TOKEN="synthetic-value-one"
declare -x SECOND_TOKEN="synthetic-value-two"
if true; then export THIRD_TOKEN="synthetic-value-three"; fi
true && export FOURTH_TOKEN="synthetic-value-four"
false || export FIFTH_TOKEN="synthetic-value-five"
''')
        self.assertEqual(len(rows), 5)
        self.assertEqual(rows["FIRST_TOKEN"]["certainty"], "unconditional")
        for name in ("THIRD_TOKEN", "FOURTH_TOKEN", "FIFTH_TOKEN"):
            self.assertEqual(rows[name]["certainty"], "conditional")
            self.assertEqual(rows[name]["scope"], "global")

    def test_called_and_uncalled_function_exports_are_potential_ambient(self):
        for declaration in ("load_tokens() {", "load_tokens()\n{", "function load_tokens {", "function load_tokens() {"):
            with self.subTest(declaration=declaration):
                for invocation in ("", "load_tokens\n"):
                    rows = self.exports(declaration + '\n export SERVICE_TOKEN="synthetic-value-one"\n}\n' + invocation)
                    row = rows["SERVICE_TOKEN"]
                    self.assertEqual(row["classification"], "credential-literal")
                    self.assertEqual(row["scope"], "function")
                    self.assertEqual(row["certainty"], "conditional")

    def test_explicit_locals_and_subshells_are_distinct(self):
        rows = self.exports('''\
run_service() {
 local FIRST_TOKEN="synthetic-value-one"
 export FIRST_TOKEN
 local -x SECOND_TOKEN="synthetic-value-two"
 typeset -x THIRD_TOKEN="synthetic-value-three"
 typeset -gx FOURTH_TOKEN="synthetic-value-four"
 INLINE_TOKEN="synthetic-value-inline" command
}
(export FIFTH_TOKEN="synthetic-value-five")
run_isolated() (export SIXTH_TOKEN="synthetic-value-six")
''')
        for name in ("FIRST_TOKEN", "SECOND_TOKEN", "THIRD_TOKEN"):
            self.assertEqual(rows[name]["scope"], "function-local")
        self.assertEqual(rows["FOURTH_TOKEN"]["scope"], "function")
        for name in ("FIFTH_TOKEN", "SIXTH_TOKEN"):
            self.assertEqual(rows[name]["scope"], "subshell")
        self.assertNotIn("INLINE_TOKEN", rows)

    def test_nested_function_cannot_assume_its_callers_locals(self):
        rows = self.exports('''\
outer() {
 local SERVICE_TOKEN="synthetic-value-one"
 inner() { export SERVICE_TOKEN="synthetic-value-two"; }
}
''')
        self.assertEqual(rows["SERVICE_TOKEN"]["scope"], "function")

    def test_brace_groups_preserve_explicit_function_locals(self):
        rows = self.exports('''\
run_service() {
 { local SERVICE_TOKEN="synthetic-value-one"; }
 export SERVICE_TOKEN
}
''')
        self.assertEqual(rows["SERVICE_TOKEN"]["scope"], "function-local")

    def test_conditional_local_does_not_hide_a_later_potential_export(self):
        rows = self.exports('''\
run_service() {
 true && { local SERVICE_TOKEN="synthetic-value-one"; }
 export SERVICE_TOKEN
}
''')
        self.assertEqual(rows["SERVICE_TOKEN"]["scope"], "function")

    def test_inline_conditional_local_does_not_hide_a_later_export(self):
        rows = self.exports('f() { false && local SERVICE_TOKEN=synthetic-value; export SERVICE_TOKEN=synthetic-value; }\n')
        self.assertEqual(rows["SERVICE_TOKEN"]["scope"], "function")
        self.assertEqual(rows["SERVICE_TOKEN"]["classification"], "credential-literal")

    def test_assignment_to_an_already_exported_variable_is_reported(self):
        findings = self.classify('export SERVICE_TOKEN=""; SERVICE_TOKEN=synthetic-value\n')
        self.assertEqual([row["classification"] for row in findings],
                         ["credential-empty", "credential-literal"])
        self.assertEqual(findings[-1]["scope"], "global")

    def test_conditional_writes_do_not_establish_empty_values(self):
        for content in (
            'SERVICE_TOKEN=synthetic-value; false && SERVICE_TOKEN=""; export SERVICE_TOKEN\n',
            'SERVICE_TOKEN=""; if unknown; then SERVICE_TOKEN=synthetic-value; fi; export SERVICE_TOKEN\n',
            'SERVICE_TOKEN=""; unknown && { SERVICE_TOKEN=synthetic-value; }; export SERVICE_TOKEN\n',
        ):
            rows = self.exports(content)
            self.assertEqual(rows["SERVICE_TOKEN"]["classification"], "credential-indirect")

    def test_conditional_exports_are_retained_as_possible_attributes(self):
        for prefix in ('unknown && export SERVICE_TOKEN="";',
                       'if unknown; then export SERVICE_TOKEN=""; fi;'):
            findings = self.classify(prefix + ' SERVICE_TOKEN=synthetic-value\n')
            self.assertEqual(findings[-1]["classification"], "credential-literal")
            self.assertEqual(findings[-1]["certainty"], "conditional")

    def test_conditional_subshell_export_attribute_does_not_escape(self):
        findings = self.classify('unknown && (export SERVICE_TOKEN=""); SERVICE_TOKEN=synthetic-value\n')
        self.assertEqual(len(findings), 1)
        self.assertEqual(findings[0]["scope"], "subshell")

    def test_negated_export_still_runs(self):
        rows = self.exports('! export SERVICE_TOKEN=synthetic-value\n')
        self.assertEqual(rows["SERVICE_TOKEN"]["classification"], "credential-literal")
        self.assertEqual(rows["SERVICE_TOKEN"]["certainty"], "unconditional")

    def test_quotes_substitutions_and_comments_do_not_expose_values(self):
        rows = self.exports('''\
# export COMMENT_TOKEN="synthetic-value-comment"
export PERSONAL_EMAIL="synthetic-value-email"
export FIRST_TOKEN='synthetic-value$dollar'
export SECOND_TOKEN="synthetic-value pass phrase"
export THIRD_TOKEN="$(pass env/synthetic-value-ref)"
export FOURTH_TOKEN="$SYNTHETIC_EXISTING_TOKEN"
export FIFTH_TOKEN="$(printf '%s' 'synthetic-value with } and {')"
export EMPTY_TOKEN=""
''')
        self.assertNotIn("COMMENT_TOKEN", rows)
        self.assertEqual(rows["PERSONAL_EMAIL"]["classification"], "identity")
        for name in ("FIRST_TOKEN", "SECOND_TOKEN"):
            self.assertEqual(rows[name]["classification"], "credential-literal")
        self.assertEqual(rows["THIRD_TOKEN"]["classification"], "credential-store-backed")
        for name in ("FOURTH_TOKEN", "FIFTH_TOKEN"):
            self.assertEqual(rows[name]["classification"], "credential-indirect")
        self.assertEqual(rows["EMPTY_TOKEN"]["classification"], "credential-empty")

    def test_multiline_strings_and_continuations(self):
        rows = self.exports('export FIRST_TOKEN="synthetic-value\nexport FALSE_TOKEN=synthetic-value\n"\nexport \\\n SECOND_TOKEN="synthetic-value-two"\n')
        self.assertEqual(set(rows), {"FIRST_TOKEN", "SECOND_TOKEN"})
        self.assertEqual(rows["FIRST_TOKEN"]["line"], 1)
        self.assertEqual(rows["SECOND_TOKEN"]["line"], 5)

    def test_heredoc_is_a_coverage_gap_not_an_export(self):
        findings = self.classify('export FIRST_TOKEN="synthetic-value-one"\ncat <<EOF\nexport FALSE_TOKEN="synthetic-value-two"\nEOF\n')
        self.assertEqual([row.get("name") for row in findings], ["FIRST_TOKEN", None])
        self.assertEqual(findings[-1]["classification"], "not-checked")
        self.assertEqual(findings[-1]["line"], 2)

    def test_unsupported_constructs_are_explicit_and_value_free(self):
        for content in (
            'export SERVICE_TOKEN="synthetic-value-unclosed\n',
            'eval "export SERVICE_TOKEN=synthetic-value"\n',
            'source synthetic-value-file\n',
            'export -n SERVICE_TOKEN\n',
            'declare -A SERVICE_TOKEN=synthetic-value\n',
            'case x in x) export SERVICE_TOKEN=synthetic-value;; esac\n',
            'export SERVICE_TOKEN=synthetic-value | command\n',
            '(( 1 ))\nexport SERVICE_TOKEN=synthetic-value\n',
            '"export" SERVICE_TOKEN=synthetic-value\n',
            'local -x SERVICE_TOKEN=synthetic-value\n',
            'export SERVICE_TOKEN=synthetic-value\\',
        ):
            with self.subTest(content=content):
                findings = self.classify(content)
                self.assertTrue(any(row["classification"] == "not-checked" for row in findings))

    def test_assignment_for_a_process_is_not_an_ambient_export(self):
        self.assertEqual(self.classify('SERVICE_TOKEN="synthetic-value-one" command\n'), [])

    def test_prefixed_builtin_export_is_still_an_export(self):
        for prefix in ('PREFIX=synthetic-value', 'command --', 'builtin', 'command -p'):
            rows = self.exports(prefix + ' export SERVICE_TOKEN=synthetic-value\n')
            self.assertEqual(set(rows), {"SERVICE_TOKEN"})
            self.assertEqual(rows["SERVICE_TOKEN"]["scope"], "global")

    def test_conditional_continuations_and_groups_stay_conditional(self):
        rows = self.exports('''\
true &&
 export FIRST_TOKEN=synthetic-value
true && { export SECOND_TOKEN=synthetic-value; export THIRD_TOKEN=synthetic-value; }
''')
        self.assertEqual(len(rows), 3)
        self.assertTrue(all(row["certainty"] == "conditional" for row in rows.values()))

    def test_classifier_never_executes_target_commands(self):
        with tempfile.TemporaryDirectory() as directory:
            marker = Path(directory) / "must-not-exist"
            rows = self.exports(f'export SERVICE_TOKEN="$(touch {marker})"\n')
            self.assertEqual(rows["SERVICE_TOKEN"]["classification"], "credential-indirect")
            self.assertFalse(marker.exists())

    def test_actual_shell_function_export_reaches_child(self):
        # Execute this fixed synthetic command only, never the inspected file.
        result = subprocess.run(
            ["/bin/zsh", "-f", "-c",
             "load_fixture() { export SYNTHETIC_AUDIT_TOKEN=synthetic-value; }; "
             "load_fixture; /bin/zsh -f -c "
             "'[[ ${SYNTHETIC_AUDIT_TOKEN-} == synthetic-value ]]'"],
            capture_output=True, text=True, check=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")

    def test_actual_shell_local_and_subshell_exports_do_not_escape(self):
        result = subprocess.run(
            ["/bin/zsh", "-f", "-c",
             "unset SYNTHETIC_AUDIT_LOCAL_TOKEN SYNTHETIC_AUDIT_SUBSHELL_TOKEN; "
             "load_fixture() { local -x SYNTHETIC_AUDIT_LOCAL_TOKEN=synthetic-value; }; "
             "load_fixture; (export SYNTHETIC_AUDIT_SUBSHELL_TOKEN=synthetic-value); "
             "[[ -z ${SYNTHETIC_AUDIT_LOCAL_TOKEN+x} && -z ${SYNTHETIC_AUDIT_SUBSHELL_TOKEN+x} ]]"],
            capture_output=True, text=True, check=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")

    def test_live_shell_confirms_reassignment_conditionals_and_negation(self):
        for setup in (
            'export SYNTHETIC_AUDIT_TOKEN=""; SYNTHETIC_AUDIT_TOKEN=synthetic-value',
            'f() { false && local SYNTHETIC_AUDIT_TOKEN=synthetic-value; export SYNTHETIC_AUDIT_TOKEN=synthetic-value; }; f',
            'SYNTHETIC_AUDIT_TOKEN=synthetic-value; false && SYNTHETIC_AUDIT_TOKEN=""; export SYNTHETIC_AUDIT_TOKEN',
            '! export SYNTHETIC_AUDIT_TOKEN=synthetic-value',
        ):
            result = subprocess.run(
                ["/bin/zsh", "-f", "-c", setup +
                 "; /bin/zsh -f -c '[[ ${SYNTHETIC_AUDIT_TOKEN-} == synthetic-value ]]'"],
                capture_output=True, text=True, check=False,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
