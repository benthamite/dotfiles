"""Exercise the real export hooks only against disposable, synthetic manuals."""
from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
HOOKS = tuple(ROOT / runtime / "hooks/regenerate-manual-after-edit.sh" for runtime in ("claude", "codex"))
NATIVE_EMACS = shutil.which("emacs")
NATIVE_MAKEINFO = shutil.which("makeinfo")
SENTINEL = "fixture-private-error-never-a-real-secret"
HEADER = "#+title: Fixture manual\n#+texinfo_filename: {info}\n\n* Overview\n"


@unittest.skipUnless(NATIVE_EMACS and shutil.which("jq"), "Native batch Emacs and jq are required")
class RegenerateManualHookTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="manual-hook-tests-", dir="/tmp")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.counter = 0
        self.trace = self.root / "trace"
        self.env = dict(os.environ, PATH=f"{self.bin}:{os.environ['PATH']}",
                        NATIVE_EMACS=NATIVE_EMACS, NATIVE_MAKEINFO=NATIVE_MAKEINFO or "",
                        MANUAL_TEST_TRACE=str(self.trace), MANUAL_TEST_MODE="native",
                        MANUAL_INFO_MODE="fake", MANUAL_TEST_SENTINEL=SENTINEL)
        self.script("emacs", '''#!/usr/bin/env bash
if [ "$MANUAL_EXPORT_MODE" = names ]; then exec "$NATIVE_EMACS" "$@"; fi
printf '%s\\n' "$MANUAL_EXPORT_OUTPUT" >> "$MANUAL_TEST_TRACE"
case "$MANUAL_TEST_MODE" in
  fail) printf '%s\\n' "$MANUAL_TEST_SENTINEL" >&2; exit 7 ;;
  replace) printf 'foreign replacement\\n' > "$MANUAL_TEST_REPLACE" ;;
  source-change)
    "$NATIVE_EMACS" "$@" || exit "$?"
    printf '\\nConcurrent source change\\n' >> "$MANUAL_EXPORT_SOURCE"
    exit 0 ;;
  source-metadata)
    "$NATIVE_EMACS" "$@" || exit "$?"
    chmod 600 "$MANUAL_EXPORT_SOURCE"
    exit 0 ;;
  source-replace)
    "$NATIVE_EMACS" "$@" || exit "$?"
    cp "$MANUAL_EXPORT_SOURCE" "$MANUAL_EXPORT_SOURCE.replacement"
    mv "$MANUAL_EXPORT_SOURCE.replacement" "$MANUAL_EXPORT_SOURCE"
    exit 0 ;;
  source-symlink)
    "$NATIVE_EMACS" "$@" || exit "$?"
    mv "$MANUAL_EXPORT_SOURCE" "$MANUAL_EXPORT_SOURCE.original"
    ln -s "$MANUAL_EXPORT_SOURCE.original" "$MANUAL_EXPORT_SOURCE"
    exit 0 ;;
esac
exec "$NATIVE_EMACS" "$@"
''')
        self.script("makeinfo", '''#!/usr/bin/env bash
printf 'makeinfo\\n' >> "$MANUAL_TEST_TRACE"
if [ "$MANUAL_INFO_MODE" = real ]; then exec "$NATIVE_MAKEINFO" "$@"; fi
while [ "$#" -gt 0 ]; do
  case "$1" in *.texi) input="$1" ;; esac
  if [ "$1" = -o ]; then output="$2"; shift; fi
  shift
done
if [ "$MANUAL_INFO_MODE" = replace-sibling ]; then
  cp "$input" "$MANUAL_INFO_INPUT_CAPTURE"
  printf '@include unreviewed-file\\n' > "$MANUAL_TEST_REPLACE"
fi
printf 'fixture info\\n' > "$output"
if [ "$MANUAL_INFO_MODE" = fail ]; then
  printf '%s\\n' "$MANUAL_TEST_SENTINEL" >&2
  exit 2
fi
''')

    def script(self, name, source):
        script = self.bin / name
        script.write_text(source)
        script.chmod(0o755)

    def manual(self, body="Body.\n", *, header=None, name="manual with spaces"):
        self.counter += 1
        directory = self.root / f"case {self.counter}"
        directory.mkdir()
        source = directory / f"{name}.org"
        source.write_text((HEADER.format(info=f"{name}.info") if header is None else header) + body)
        return source

    def run_hook(self, hook, source=None, *, payload=None, environment=None):
        if payload is None:
            payload = {"tool_name": "Edit", "tool_input": {"file_path": str(source)}}
        env = dict(self.env, **(environment or {}))
        result = subprocess.run(["bash", str(hook)], input=json.dumps(payload),
                                text=True, capture_output=True, cwd=self.root,
                                env=env, timeout=30)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn(SENTINEL, result.stdout + result.stderr)
        if not result.stdout.strip():
            return ""
        response = json.loads(result.stdout)
        self.assertEqual(response["hookSpecificOutput"]["hookEventName"], "PostToolUse")
        return response["hookSpecificOutput"]["additionalContext"]

    def assert_staging_clean(self):
        if self.trace.exists():
            for line in self.trace.read_text().splitlines():
                if line != "makeinfo":
                    self.assertFalse(Path(line).parent.exists(), line)

    def test_plain_manual_owned_output_defaults_and_printindex(self):
        for hook in HOOKS:
            with self.subTest(hook=hook):
                source = self.manual("Paragraph one.\nContinued text.\n\n#+findex: sample-command\n#+texinfo: @printindex fn\n")
                message = self.run_hook(hook, source)
                output = source.with_suffix(".texi").read_text()
                self.assertIn("Regenerated 1 .texi and 0 .info", message)
                self.assertIn("@title Fixture manual", output)
                self.assertNotIn("@*", output)
                self.assertIn("@printindex fn", output)
                self.assertIn(f"@setfilename {source.stem}.info", output)
                self.assertFalse(source.with_suffix(".info").exists())
        self.assert_staging_clean()

    def test_export_trigger_requires_texinfo_or_info_extension(self):
        for hook in HOOKS:
            for target, expected in [("report.pdf", False), ("report.md", False), ("manual.info", True)]:
                with self.subTest(hook=hook, target=target):
                    source = self.manual(header=f"#+title: Fixture\n#+export_file_name: {target}\n\n* Overview\n")
                    message = self.run_hook(hook, source)
                    selected = source.parent / "manual.texi" if expected else source.with_suffix(".texi")
                    self.assertEqual(selected.exists(), expected)
                    self.assertEqual(bool(message), expected)

    def test_babel_and_local_evaluation_never_execute(self):
        for hook in HOOKS:
            with self.subTest(hook=hook):
                marker = self.root / f"marker-{self.counter}"
                form = f'(write-region "executed" nil "{marker}")'
                source = self.manual(f"#+begin_src emacs-lisp :eval yes :exports results\n{form}\n#+end_src\n"
                                     f"Inline src_emacs-lisp[:eval yes]{{{form}}}.\n"
                                     f"\n# Local Variables:\n# eval: {form}\n# End:\n")
                (source.parent / ".dir-locals.el").write_text(f"((nil . ((eval . {form}))))\n")
                message = self.run_hook(hook, source)
                self.assertIn("Regenerated 1 .texi", message)
                self.assertFalse(marker.exists())

    def test_unreviewed_org_directives_are_explicitly_refused(self):
        for hook in HOOKS:
            for directive in ["#+macro: unsafe (eval (error \"unsafe\"))", "  #+BiNd: org-export-use-babel t",
                              "#+call: unsafe()", "#+include: \"unread-fixture.org\"",
                              "#+setupfile: https://example.invalid/never-read"]:
                with self.subTest(hook=hook, directive=directive):
                    source = self.manual(directive + "\n")
                    source.with_suffix(".texi").write_text("previous texi")
                    message = self.run_hook(hook, source)
                    self.assertIn("refused unsupported", message)
                    self.assertIn("Regenerated 0 .texi", message)
                    self.assertEqual(source.with_suffix(".texi").read_text(), "previous texi")

    def test_raw_texinfo_and_generated_external_directives_are_refused(self):
        unsafe = ["#+texinfo: @include fixture.txt", "#+texinfo: @printindex fn @include fixture.txt",
                  "#+texinfo_header: @macro sneaky", "#+texinfo_post_header: @alias sneaky = include",
                  "#+begin_export texinfo\n@verbatiminclude fixture.txt\n#+end_export",
                  "@@texinfo:@include fixture.txt@@", "[[file:fixture.png]]",
                  "#+cindex: @alias sneaky = include"]
        for hook in HOOKS:
            for body in unsafe:
                with self.subTest(hook=hook, body=body):
                    source = self.manual(body + "\n")
                    source.with_suffix(".info").write_text("previous info")
                    (source.parent / "fixture.txt").write_text(SENTINEL)
                    message = self.run_hook(hook, source)
                    self.assertIn("refused unsupported", message)
                    self.assertFalse(source.with_suffix(".texi").exists())
                    self.assertEqual(source.with_suffix(".info").read_text(), "previous info")

    def test_literal_directives_in_source_examples_stay_literal(self):
        for hook in HOOKS:
            source = self.manual("#+begin_src text\n#+include: not-a-directive\n@include not-a-directive\n"
                                 "@macro not-a-macro\n#+end_src\n")
            message = self.run_hook(hook, source)
            self.assertIn("Regenerated 1 .texi", message)
            self.assertIn("@@include", source.with_suffix(".texi").read_text())

    def test_unsafe_headers_are_refused_without_overwriting_artifacts(self):
        for hook in HOOKS:
            for target in ["../outside.info", str(self.root / "outside.info"), "https://example.invalid/out.info",
                           'arbitrary "quoted" name.info', "raw@name.info", "raw{value}.info"]:
                with self.subTest(hook=hook, target=target):
                    source = self.manual(header=f"#+title: Fixture\n#+texinfo_filename: {target}\n"
                                               f"#+export_file_name: {target}\n\n* Overview\n")
                    source.with_suffix(".texi").write_text("previous texi")
                    source.with_suffix(".info").write_text("previous info")
                    message = self.run_hook(hook, source)
                    self.assertIn("output-name preflight failed", message)
                    self.assertIn("Regenerated 0 .texi", message)
                    self.assertEqual(source.with_suffix(".texi").read_text(), "previous texi")
                    self.assertEqual(source.with_suffix(".info").read_text(), "previous info")
                    self.assertFalse((self.root / "outside.texi").exists())

    def test_standard_readme_preserves_declared_package_artifacts(self):
        for hook in HOOKS:
            source = self.manual(name="README", header="#+title: Package\n#+export_file_name: package.info\n"
                                 "#+texinfo_filename: package.info\n#+texinfo_dir_title: Package: (package)\n\n* Overview\n")
            selected_info = source.parent / "package.info"
            selected_info.write_text("previous package info")
            unrelated_info = source.parent / "README.info"
            unrelated_info.write_text("independent README info")
            message = self.run_hook(hook, source)
            self.assertIn("Regenerated 1 .texi and 1 .info", message)
            output = (source.parent / "package.texi").read_text()
            self.assertIn("@setfilename package.info", output)
            self.assertIn("(package)", output)
            self.assertEqual(selected_info.read_text(), "fixture info\n")
            self.assertEqual(unrelated_info.read_text(), "independent README info")
            self.assertFalse(source.with_suffix(".texi").exists())

    def test_export_and_info_declarations_are_independent(self):
        for hook in HOOKS:
            for export_name, texi_name in [(None, "README.texi"), ("guide.info", "guide.texi")]:
                with self.subTest(hook=hook, export_name=export_name):
                    export = f"#+export_file_name: {export_name}\n" if export_name else ""
                    source = self.manual(name="README", header=f"#+title: Package\n{export}"
                                         "#+texinfo_filename: package.info\n\n* Overview\n")
                    info = source.parent / "package.info"
                    info.write_text("previous package info")
                    message = self.run_hook(hook, source)
                    self.assertIn("Regenerated 1 .texi and 1 .info", message)
                    self.assertIn("@setfilename package.info", (source.parent / texi_name).read_text())
                    self.assertEqual(info.read_text(), "fixture info\n")
                    self.assertFalse((source.parent / "README.info").exists())

    def test_ambiguous_output_declarations_require_review(self):
        for hook in HOOKS:
            source = self.manual(header="#+title: Package\n#+texinfo_filename: package.info\n"
                                 "#+export_file_name: first.info\n#+export_file_name: second.info\n\n* Overview\n")
            message = self.run_hook(hook, source)
            self.assertIn("output-name preflight failed", message)
            self.assertEqual(list(source.parent.glob("*.texi")), [])

    def test_each_independent_header_is_validated_before_any_write(self):
        for hook in HOOKS:
            for export, info in [("../outside.info", "package.info"), ("package.info", "../outside.info")]:
                with self.subTest(hook=hook, export=export, info=info):
                    source = self.manual(name="README", header=f"#+title: Package\n#+export_file_name: {export}\n"
                                         f"#+texinfo_filename: {info}\n\n* Overview\n")
                    texi = source.parent / "package.texi"
                    info_file = source.parent / "package.info"
                    texi.write_text("previous selected texi")
                    info_file.write_text("previous selected info")
                    message = self.run_hook(hook, source)
                    self.assertIn("output-name preflight failed", message)
                    self.assertEqual(texi.read_text(), "previous selected texi")
                    self.assertEqual(info_file.read_text(), "previous selected info")

    def test_symlink_output_and_concurrent_replacement_are_preserved(self):
        for hook in HOOKS:
            source = self.manual()
            victim = source.parent / "independent"
            victim.write_text("independent bytes")
            source.with_suffix(".texi").symlink_to(victim)
            message = self.run_hook(hook, source)
            self.assertIn("not an ordinary", message)
            self.assertEqual(victim.read_text(), "independent bytes")
            source = self.manual()
            output = source.with_suffix(".texi")
            output.write_text("previous texi")
            message = self.run_hook(hook, source, environment={"MANUAL_TEST_MODE": "replace",
                                                             "MANUAL_TEST_REPLACE": str(output)})
            self.assertIn("publication failed", message)
            self.assertEqual(output.read_text(), "foreign replacement\n")

    def test_export_failure_preserves_old_artifacts_and_sanitizes_errors(self):
        for hook in HOOKS:
            source = self.manual()
            source.with_suffix(".texi").write_text("previous texi")
            source.with_suffix(".info").write_text("previous info")
            message = self.run_hook(hook, source, environment={"MANUAL_TEST_MODE": "fail"})
            self.assertIn("Texinfo export failed", message)
            self.assertIn("Regenerated 0 .texi", message)
            self.assertEqual(source.with_suffix(".texi").read_text(), "previous texi")
            self.assertEqual(source.with_suffix(".info").read_text(), "previous info")
        self.assert_staging_clean()

    def test_source_change_during_export_prevents_stale_publication(self):
        for hook in HOOKS:
            source = self.manual()
            source.with_suffix(".texi").write_text("previous texi")
            message = self.run_hook(hook, source, environment={"MANUAL_TEST_MODE": "source-change"})
            self.assertIn("Manual source changed", message)
            self.assertIn("Regenerated 0 .texi", message)
            self.assertEqual(source.with_suffix(".texi").read_text(), "previous texi")

    def test_source_metadata_change_does_not_reject_unchanged_content(self):
        for hook in HOOKS:
            source = self.manual()
            original = source.read_bytes()
            before = source.stat()
            message = self.run_hook(hook, source, environment={"MANUAL_TEST_MODE": "source-metadata"})
            self.assertEqual(source.read_bytes(), original)
            self.assertEqual(source.stat().st_mtime_ns, before.st_mtime_ns)
            self.assertNotEqual(source.stat().st_ctime_ns, before.st_ctime_ns)
            self.assertIn("Regenerated 1 .texi", message)
            self.assertIn("Failures: 0", message)

    def test_identical_content_source_replacement_is_rejected(self):
        for hook in HOOKS:
            for mode in ("source-replace", "source-symlink"):
                with self.subTest(hook=hook, mode=mode):
                    source = self.manual()
                    original = source.read_bytes()
                    source.with_suffix(".texi").write_text("previous texi")
                    message = self.run_hook(hook, source, environment={"MANUAL_TEST_MODE": mode})
                    self.assertEqual(source.read_bytes(), original)
                    self.assertIn("Manual source changed", message)
                    self.assertEqual(source.with_suffix(".texi").read_text(), "previous texi")

    def test_makeinfo_only_consumes_private_validated_texinfo(self):
        for hook in HOOKS:
            source = self.manual()
            source.with_suffix(".texi").write_text("previous texi")
            source.with_suffix(".info").write_text("previous info")
            captured = source.parent / "compiler-input"
            message = self.run_hook(hook, source, environment={
                "MANUAL_INFO_MODE": "replace-sibling", "MANUAL_INFO_INPUT_CAPTURE": str(captured),
                "MANUAL_TEST_REPLACE": str(source.with_suffix(".texi"))})
            self.assertIn("@title Fixture manual", captured.read_text())
            self.assertNotIn("@include unreviewed-file", captured.read_text())
            self.assertIn("publication failed", message)
            self.assertEqual(source.with_suffix(".info").read_text(), "previous info")

    def test_makeinfo_failure_preserves_old_info_and_reports_partial_result(self):
        for hook in HOOKS:
            source = self.manual()
            source.with_suffix(".info").write_text("previous info")
            message = self.run_hook(hook, source, environment={"MANUAL_INFO_MODE": "fail"})
            self.assertIn("Regenerated 1 .texi and 0 .info", message)
            self.assertIn("Info regeneration failed", message)
            self.assertIn("Failures: 1", message)
            self.assertEqual(source.with_suffix(".info").read_text(), "previous info")
        self.assert_staging_clean()

    @unittest.skipUnless(NATIVE_MAKEINFO, "Native makeinfo is required")
    def test_real_makeinfo_refreshes_only_existing_matching_info(self):
        for hook in HOOKS:
            source = self.manual(name="manual")
            source.with_suffix(".info").write_text("previous info")
            message = self.run_hook(hook, source, environment={"MANUAL_INFO_MODE": "real"})
            self.assertIn("Regenerated 1 .texi and 1 .info", message)
            self.assertIn("Fixture manual", source.with_suffix(".info").read_text())

    @unittest.skipUnless(NATIVE_MAKEINFO, "Native makeinfo is required")
    def test_real_info_internal_identity_matches_independent_declared_names(self):
        for hook in HOOKS:
            for export_name, info_name in [("reviewed.info", "reviewed.info"), ("guide.info", "reviewed.info"),
                                           ("guide with spaces.info", "reviewed with spaces.info")]:
                with self.subTest(hook=hook, export_name=export_name):
                    source = self.manual(name="README", header=f"#+title: Package\n#+export_file_name: {export_name}\n"
                                         f"#+texinfo_filename: {info_name}\n\n* Overview\n")
                    info = source.parent / info_name
                    info.write_text("previous info")
                    message = self.run_hook(hook, source, environment={"MANUAL_INFO_MODE": "real"})
                    self.assertIn("Regenerated 1 .texi and 1 .info", message)
                    generated = info.read_text()
                    header = " ".join(generated.split("\n\n", 1)[0].split())
                    self.assertIn(f"This is {info_name},", header)
                    self.assertIn(f"from {Path(export_name).stem}.texi.", header)
                    self.assertIn(f"File: {info_name},", generated)
                    self.assertNotIn("File: manual.info,", generated)
                    self.assertNotIn("File: guide.info,", generated)
        self.assert_staging_clean()

    def test_codex_patch_dispatch_continues_after_one_failed_manual(self):
        good, bad = self.manual(name="good"), self.manual("[[file:fixture.png]]\n", name="bad")
        patch = f"*** Begin Patch\n*** Update File: {bad}\n@@\n+Body\n*** Update File: {good}\n@@\n+Body\n*** End Patch"
        for tool_input in [{"patch": patch}, json.dumps({"input": patch})]:
            message = self.run_hook(HOOKS[1], payload={"tool_name": "apply_patch", "tool_input": tool_input})
            self.assertIn("Regenerated 1 .texi", message)
            self.assertIn("Failures: 1", message)
            self.assertTrue(good.with_suffix(".texi").exists())
            self.assertFalse(bad.with_suffix(".texi").exists())
        self.assert_staging_clean()

    def test_hook_export_implementations_match_after_runtime_dispatch(self):
        bodies = [hook.read_text().split("record_failure()", 1)[1] for hook in HOOKS]
        self.assertEqual(bodies[0], bodies[1])
        for hook in HOOKS:
            self.assertEqual(subprocess.run(["bash", "-n", str(hook)]).returncode, 0)


if __name__ == "__main__":
    for hook in HOOKS:
        print(f"tested-source {hook.relative_to(ROOT)} sha256={hashlib.sha256(hook.read_bytes()).hexdigest()}", flush=True)
    unittest.main()
