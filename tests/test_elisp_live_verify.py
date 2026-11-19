"""elisp-live-verify on a dotfiles extras package, with a fake running Emacs."""

import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]


class ElispLiveVerifyExtrasTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name).resolve()
        self.tool = self.root / "dotfiles"
        for relative in ("claude/bin/elisp-live-verify", "claude/bin/elisp-evidence",
                         "bin/elpaca-package-resolve"):
            target = self.tool / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(ROOT / relative, target)
        self.calls = self.root / "calls"
        rebuild = self.tool / "claude/bin/elpaca-rebuild-wait"
        rebuild.write_text(f'#!/bin/sh\necho "rebuild $1" >> "{self.calls}"\n')
        rebuild.chmod(0o755)
        extras = self.tool / "emacs/extras"
        extras.mkdir(parents=True)
        (extras / "fresh-extras.el").write_text("(provide 'fresh-extras)\n")
        self.fake_bin = self.root / "bin"
        self.fake_bin.mkdir()
        client = self.fake_bin / "emacsclient"
        # The running Emacs: an Elpaca entry only when FAKE_REGISTERED is set;
        # a source load provides the feature; any other expression is true.
        client.write_text(f'''#!/bin/sh
form="$*"
echo "emacsclient $form" >> "{self.calls}"
case "$form" in
  *elpaca-get*) if [ -n "$FAKE_REGISTERED" ]; then echo t; else echo nil; fi ;;
  *) echo t ;;
esac
''')
        client.chmod(0o755)
        self.env = {key: value for key, value in os.environ.items()
                    if not key.startswith("GIT_")}
        self.env.update(PATH=f"{self.fake_bin}:{self.env['PATH']}",
                        DOTFILES_ROOT=str(self.tool), GIT_CONFIG_NOSYSTEM="1",
                        GIT_CONFIG_GLOBAL="/dev/null",
                        AGENT_ELISP_EVIDENCE_DIR=str(self.root / "evidence"))
        for args in (["init", "-q"], ["config", "user.email", "fixture@example.invalid"],
                     ["config", "user.name", "Fixture"], ["config", "core.hooksPath", "/dev/null"],
                     ["add", "."], ["commit", "-qm", "fixture"]):
            subprocess.run(["git", "-C", str(self.tool), *args], env=self.env,
                           check=True, capture_output=True, text=True)

    def verify(self):
        return subprocess.run(
            [str(self.tool / "claude/bin/elisp-live-verify"), "fresh-extras", "--",
             "(featurep 'fresh-extras)"],
            env=self.env, cwd=self.tool, text=True, capture_output=True, timeout=30)

    def test_undeclared_extras_package_is_loaded_from_its_source(self):
        result = self.verify()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("loaded its committed source", result.stdout)
        calls = self.calls.read_text()
        self.assertNotIn("rebuild", calls)
        self.assertIn(str(self.tool / "emacs/extras/fresh-extras.el"),
                      self.decoded_loads(calls))

    def test_declared_extras_package_is_rebuilt_by_elpaca(self):
        self.env["FAKE_REGISTERED"] = "1"
        result = self.verify()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("rebuild fresh-extras", self.calls.read_text())
        self.assertNotIn("loaded its committed source", result.stdout)

    def decoded_loads(self, calls):
        """Return the paths that the base64-encoded load forms name."""
        import base64
        import re
        return " ".join(base64.b64decode(match).decode()
                        for match in re.findall(r'base64-decode-string \\?"([A-Za-z0-9+/=]+)', calls))


if __name__ == "__main__":
    unittest.main()
