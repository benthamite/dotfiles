"""Native batch checks using only disposable packages and a fake registry."""

import base64
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
EMACS = shutil.which("emacs")


@unittest.skipUnless(EMACS, "native batch Emacs is unavailable")
class ElispBatchRunnerTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.home = self.root / "home"
        self.profile = self.home / ".config/emacs-profiles/test/elpaca"
        self.source = self.profile / "sources/example"
        self.source.mkdir(parents=True)
        self.build = self.profile / "builds/example"
        self.build.mkdir(parents=True)
        self.fake_bin = self.root / "bin"
        self.fake_bin.mkdir()
        self.tool = self.root / "dotfiles"
        for relative in ("claude/bin/batch-test.sh", "claude/bin/elisp-ert",
                         "claude/bin/elisp-source-revision", "bin/elpaca-package-resolve",
                         "claude/hooks/lib-elisp-evidence.sh"):
            target = self.tool / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(ROOT / relative, target)
        self.batch = self.tool / "claude/bin/batch-test.sh"
        self.ert = self.tool / "claude/bin/elisp-ert"
        self.env = os.environ.copy()
        self.env.update(HOME=str(self.home), PATH=f"{self.fake_bin}:{self.env['PATH']}",
                        DOTFILES_ROOT=str(self.tool), GIT_CONFIG_NOSYSTEM="1",
                        GIT_CONFIG_GLOBAL="/dev/null", FAKE_PACKAGE_ID="example",
                        FAKE_SOURCE=str(self.source), FAKE_BUILD=str(self.build),
                        FAKE_CALLS=str(self.root / "client-calls"), NATIVE_EMACS=EMACS,
                        ELISP_EVIDENCE_RECEIPT_DIR=str(self.root / "receipts"))
        client = self.fake_bin / "emacsclient"
        client.write_text('''#!/usr/bin/env python3
import base64, json, os, pathlib, subprocess, sys
form = " ".join(sys.argv[1:])
with open(os.environ["FAKE_CALLS"], "a") as log:
    log.write(form + "\\n")
encode = lambda value: base64.b64encode(value.encode()).decode()
if "elpaca-extras-resolve-package" in form:
    print(json.dumps(os.environ["FAKE_PACKAGE_ID"] + ":" + encode(os.environ["FAKE_SOURCE"]) + ":" + encode(pathlib.Path(os.environ["FAKE_SOURCE"]).name)))
elif "elpaca-batch-context-v1" in form:
    if os.environ.get("FAKE_NATIVE_CONTEXT"):
        setup = "(progn (setq elpaca-builds-directory " + json.dumps(str(pathlib.Path(os.environ["FAKE_BUILD"]).parent)) + ") (defun elpaca-get (_id) t) (defun elpaca<-source-dir (_e) " + json.dumps(os.environ["FAKE_SOURCE"]) + ") (defun elpaca<-build-dir (_e) " + json.dumps(os.environ["FAKE_BUILD"]) + "))"
        result = subprocess.run([os.environ["NATIVE_EMACS"], "-Q", "--batch", "--eval", setup, "--eval", "(prin1 " + sys.argv[-1] + ")"], text=True, capture_output=True)
        print(result.stdout, end="")
        print(result.stderr, end="", file=sys.stderr)
        sys.exit(result.returncode)
    print(json.dumps(encode(json.dumps(dict(id=os.environ["FAKE_PACKAGE_ID"], source=os.environ.get("FAKE_CONTEXT_SOURCE", os.environ["FAKE_SOURCE"]), builds=[os.environ["FAKE_BUILD"]], package_build=None if os.environ.get("FAKE_NO_OWN_BUILD") else os.environ["FAKE_BUILD"])))) )
else:
    print('"test"')
''')
        client.chmod(0o755)
        self.write("example.el", "(require 'example-helper)\n(provide 'example)\n")
        self.write("example-helper.el", "(defun example-value () 'current)\n(provide 'example-helper)\n")
        self.git("init", "-q")
        self.git("config", "user.email", "fixture@example.invalid")
        self.git("config", "user.name", "Fixture")
        self.git("add", ".")
        self.git("commit", "-qm", "fixture")
        self.test_file = self.root / "example-test.el"
        self.test_file.write_text("(require 'example)\n(ert-deftest example-current () (should (eq (example-value) 'current)))\n")

    def git(self, *args):
        return subprocess.run(["git", "-C", str(self.source), *args], env=self.env,
                              check=True, capture_output=True, text=True)

    def write(self, name, content):
        path = self.source / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        return path

    def run_helper(self, helper, *args):
        return subprocess.run([str(helper), *args], env=self.env, cwd=self.source,
                              text=True, capture_output=True, check=False, timeout=20)

    def stale_sibling(self):
        path = self.write("example-helper.el", "(defun example-value () 'stale)\n(provide 'example-helper)\n")
        compiled = subprocess.run([EMACS, "-Q", "--batch", "--eval",
                                   f"(byte-compile-file {json.dumps(str(path))})"],
                                  env=self.env, text=True, capture_output=True)
        self.assertEqual(compiled.returncode, 0, compiled.stderr)
        path.write_text("(defun example-value () 'current)\n(provide 'example-helper)\n")
        os.utime(path, (1, 1))
        return path.with_suffix(".elc")

    def test_batch_uses_source_sibling_even_when_its_timestamp_is_older(self):
        compiled = self.stale_sibling()
        before = compiled.read_bytes()
        result = self.run_helper(self.batch, "example", "(unless (eq (example-value) 'current) (error \"Stale sibling\"))")
        self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
        self.assertEqual(compiled.read_bytes(), before)
        self.assertIn("ELISP_TEST_EVIDENCE_V2:", result.stdout)

    def test_ert_uses_source_sibling_without_deleting_compiled_files(self):
        compiled = self.stale_sibling()
        before = compiled.read_bytes()
        result = self.run_helper(self.ert, "example", str(self.test_file))
        self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
        self.assertEqual(compiled.read_bytes(), before)
        self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_batch_resolves_checkout_label_and_lisp_main_file(self):
        alternate = self.root / "nonstandard-profile/repos/emacs-slack"
        alternate.parent.mkdir(parents=True)
        self.source.rename(alternate)
        self.source = alternate
        for path in (alternate / "example.el", alternate / "example-helper.el"):
            path.unlink()
        self.write("lisp/slack.el", "(provide 'slack)\n")
        self.env.update(FAKE_SOURCE=str(alternate), FAKE_PACKAGE_ID="slack")
        result = self.run_helper(self.batch, "emacs-slack")
        self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
        self.assertIn("ELISP_TEST_EVIDENCE_V2:", result.stdout)
        evidence = next(line for line in result.stdout.splitlines() if line.startswith("ELISP_TEST_EVIDENCE_V2:"))
        self.assertEqual(base64.b64decode(evidence.split(":")[2]).decode(), "emacs-slack")

    def test_ert_refuses_missing_source_instead_of_loading_build(self):
        for source in self.source.glob("*.el"):
            shutil.copy2(source, self.build / source.name)
        displaced = self.root / "displaced-source"
        self.source.rename(displaced)
        result = subprocess.run([str(self.ert), "example", str(self.test_file)],
                                env=self.env, cwd=self.root, capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0, result.stderr + result.stdout)

    def test_batch_rejects_unsafe_identifier_before_any_emacs_execution(self):
        result = self.run_helper(self.batch, "example') (error \"injected\") ;")
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)
        self.assertFalse((self.root / "client-calls").exists())

    def test_native_metadata_queries_actual_dependency_root(self):
        self.env["FAKE_NATIVE_CONTEXT"] = "1"
        result = self.run_helper(self.batch, "example")
        self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
        self.assertNotIn("init-current-profile", (self.root / "client-calls").read_text())

    def test_ert_supports_legacy_registry_source_and_selected_test(self):
        legacy = self.profile / "repos/example"
        legacy.parent.mkdir(parents=True)
        self.source.rename(legacy)
        self.source = legacy
        self.env["FAKE_SOURCE"] = str(legacy)
        result = self.run_helper(self.ert, "example", str(self.test_file), "example-current")
        self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
        self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_ambiguous_root_and_lisp_main_files_refuse_without_evidence(self):
        self.write("lisp/example.el", "(provide 'example)\n")
        result = self.run_helper(self.batch, "example")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("found 2", result.stderr)
        self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_ert_extra_preserves_existing_canonical_bytecode(self):
        extras = self.tool / "emacs/extras"
        extras.mkdir(parents=True)
        for name in ("example.el", "example-helper.el"):
            shutil.copy2(self.source / name, extras / name)
        compiled = extras / "example.elc"
        compiled.write_bytes(b"owned-preexisting-compiled-artifact")
        subprocess.run(["git", "init", "-q", str(self.tool)], env=self.env, check=True)
        subprocess.run(["git", "-C", str(self.tool), "add", "."], env=self.env, check=True)
        subprocess.run(["git", "-C", str(self.tool), "-c", "user.name=Fixture", "-c",
                        "user.email=fixture@example.invalid", "commit", "-qm", "fixture"], env=self.env, check=True)
        result = self.run_helper(self.ert, "example", str(self.test_file))
        self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
        self.assertEqual(compiled.read_bytes(), b"owned-preexisting-compiled-artifact")

    def test_missing_source_sibling_cannot_fall_back_to_own_build(self):
        shutil.copy2(self.source / "example-helper.el", self.build / "example-helper.el")
        (self.source / "example-helper.el").unlink()
        result = self.run_helper(self.batch, "example")
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_unknown_own_build_cannot_contaminate_source_check(self):
        self.env["FAKE_NO_OWN_BUILD"] = "1"
        shutil.copy2(self.source / "example-helper.el", self.build / "example-helper.el")
        (self.source / "example-helper.el").unlink()
        result = self.run_helper(self.batch, "example")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Invalid Elpaca dependency metadata", result.stderr)
        self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_registry_source_switch_cannot_mix_source_and_dependencies(self):
        other = self.root / "different-source"
        other.mkdir()
        self.env["FAKE_CONTEXT_SOURCE"] = str(other)
        result = self.run_helper(self.batch, "example")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Registry source changed", result.stderr)
        self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)


if __name__ == "__main__":
    unittest.main()
