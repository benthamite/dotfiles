"""Exercise package resolution and its CLI failure path without live Emacs."""
from __future__ import annotations

import json
import os
import base64
import shutil
import shlex
from pathlib import Path
import subprocess
import tempfile
import unittest


HELPER = Path(__file__).resolve().parents[1] / "bin/elpaca-package-resolve"


class OnDiskResolveTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="elpaca-package-resolve-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name).resolve()
        self.profiles = self.root / "profiles"
        self.sources = self.profiles / "1.0.0" / "elpaca" / "sources"
        self.sources.mkdir(parents=True)
        (self.profiles / "active").symlink_to(self.profiles / "1.0.0")
        self.checkout = self.sources / "fixture"
        self.checkout.mkdir()
        subprocess.run(["git", "init", "-q", str(self.checkout)], check=True)
        self.dotfiles = self.root / "dotfiles"
        (self.dotfiles / "emacs" / "extras").mkdir(parents=True)
        commands = self.root / "commands"
        commands.mkdir()
        stub = commands / "emacsclient"
        stub.write_text('#!/bin/sh\necho called >> "$EMACSCLIENT_CALLS"\nexit 1\n')
        stub.chmod(0o755)
        self.calls = self.root / "emacsclient-calls"
        self.env = dict(
            os.environ,
            PATH=f"{commands}:{os.environ['PATH']}",
            EMACS_PROFILES_DIR=str(self.profiles),
            DOTFILES_ROOT=str(self.dotfiles),
            EMACSCLIENT_CALLS=str(self.calls),
        )

    def resolve(self, identifier):
        return subprocess.run([str(HELPER), "--on-disk", identifier], env=self.env,
                              text=True, capture_output=True, timeout=10, check=False)

    def assert_refused(self, result):
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")

    def test_resolves_checkout_in_active_profile_without_emacs(self):
        result = self.resolve("fixture")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout), {
            "label": "fixture", "id": "fixture",
            "source": str(self.checkout), "repo": str(self.checkout)})
        self.assertFalse(self.calls.exists())

    def test_follows_active_link_to_the_current_profile(self):
        newer = self.profiles / "2.0.0" / "elpaca" / "sources" / "fixture"
        newer.mkdir(parents=True)
        subprocess.run(["git", "init", "-q", str(newer)], check=True)
        (self.profiles / "active").unlink()
        (self.profiles / "active").symlink_to(self.profiles / "2.0.0")
        result = self.resolve("fixture")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout)["source"], str(newer))

    def test_missing_checkout_fails_loudly(self):
        result = self.resolve("absent")
        self.assert_refused(result)
        self.assertIn("No Elpaca checkout named absent", result.stderr)
        self.assertFalse(self.calls.exists())

    def test_missing_active_profile_fails_loudly(self):
        (self.profiles / "active").unlink()
        result = self.resolve("fixture")
        self.assert_refused(result)
        self.assertIn("No active Emacs profile", result.stderr)

    def test_non_git_directory_is_not_a_checkout(self):
        (self.sources / "plain").mkdir()
        self.assert_refused(self.resolve("plain"))

    def test_dot_segments_are_rejected(self):
        for identifier in [".", ".."]:
            with self.subTest(identifier=identifier):
                self.assert_refused(self.resolve(identifier))


class RegistryFailureTests(unittest.TestCase):
    setUp = OnDiskResolveTests.setUp

    def prepare_client(self, *, status=0, output="", delayed=False):
        commands = self.root / "commands"
        stub = commands / "emacsclient"
        body = 'echo called >> "$EMACSCLIENT_CALLS"\n'
        if delayed:
            body += 'exec /bin/sleep 2\n'
            timeout = shutil.which("timeout")
            self.assertIsNotNone(timeout)
            # Retain a real timeout child, shortening only its fixture wait.
            timer = commands / "timeout"
            timer.write_text('#!/bin/sh\n[ "$1" = 10 ] || exit 97\nshift\nexec ' +
                             shlex.quote(timeout) + ' 0.05 "$@"\n')
            timer.chmod(0o755)
        else:
            body += 'printf %s ' + shlex.quote(output) + '\nexit ' + str(status) + '\n'
        stub.write_text('#!/bin/sh\n' + body)
        stub.chmod(0o755)

    def invoke(self, helper="elpaca-package-resolve"):
        args = [str(HELPER.with_name(helper)), "fixture"]
        if helper == "agent-status":
            args[1] = "Controlled diagnostic fixture"
        return subprocess.run(args, env=self.env, text=True, capture_output=True,
                              timeout=3, check=False)

    def test_real_timeout_is_reported_through_full_status_wrapper(self):
        self.prepare_client(delayed=True)
        result = self.invoke("agent-status")
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stdout, "")
        self.assertIn("timeout status 124", result.stderr)
        self.assertIn("10-second client wait limit", result.stderr)
        self.assertIn("Emacs execution state is unknown", result.stderr)
        self.assertIn("No diagnostic output", result.stderr)
        self.assertEqual(self.calls.read_text().splitlines(), ["called"])

    def test_timeout_status_propagates_through_resolver_and_path(self):
        self.prepare_client(status=124)
        for helper in ["elpaca-package-resolve", "elpaca-package-path"]:
            with self.subTest(helper=helper):
                self.assertEqual(self.invoke(helper).returncode, 124)

    def test_silent_non_timeout_failure_retains_status(self):
        self.prepare_client(status=7)
        result = self.invoke()
        self.assertEqual(result.returncode, 7)
        self.assertIn("exit status 7", result.stderr)
        self.assertIn("No diagnostic output", result.stderr)
        self.assertNotIn("timeout status", result.stderr)

    def test_client_diagnostic_is_retained(self):
        diagnostic = "emacsclient: can't connect to server\n"
        self.prepare_client(status=1, output=diagnostic)
        result = self.invoke()
        self.assertEqual(result.returncode, 1)
        self.assertIn(diagnostic, result.stderr)
        self.assertIn("exit status 1", result.stderr)

    def test_refusal_text_still_drives_existing_wrapper_retries(self):
        self.prepare_client(status=1, output="emacsclient: can't connect to server\n")
        sleep = self.root / "commands" / "sleep"
        sleep.write_text('#!/bin/sh\nexit 0\n')
        sleep.chmod(0o755)
        result = self.invoke("agent-status")
        self.assertEqual(result.returncode, 1)
        self.assertIn("can't connect to server", result.stderr)
        self.assertEqual(self.calls.read_text().splitlines(), ["called"] * 5)

    def test_live_registry_success_still_resolves(self):
        encoded = lambda value: base64.b64encode(value.encode()).decode()
        self.prepare_client(output='"fixture:' + encoded(str(self.checkout)) + ':' + encoded('Fixture') + '"\n')
        result = self.invoke()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout)['source'], str(self.checkout))
        self.assertEqual(result.stderr, "")


if __name__ == "__main__":
    unittest.main()
