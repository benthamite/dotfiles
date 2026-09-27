"""Regression checks for account-local desktop Chrome reconciliation."""

import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch


SPEC = importlib.util.spec_from_file_location(
    "codex_browser_sync", Path(__file__).resolve().parents[1] / "bin/codex_browser_sync.py")
sync = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(sync)
RUN_CLI = sync._run


class BrowserSyncTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.home = self.root / "account"
        self.app = self.root / "Desktop.app"
        self.bundle = self.app / "Contents/Resources/plugins/openai-bundled/plugins/chrome"
        self.source = self.home / ".tmp/bundled-marketplaces/openai-bundled/plugins/chrome"
        self.cache = self.home / "plugins/cache/openai-bundled/chrome"
        self.write(self.bundle / ".codex-plugin/plugin.json", json.dumps({"name": "chrome", "version": "new"}))
        self.write(self.bundle / "scripts/browser-client.mjs", "current RPC client")
        self.write(self.bundle / "scripts/browser-service.mjs", "current service")
        self.write(self.bundle / "skills/control-chrome/SKILL.md", "current bootstrap")
        shutil.copytree(self.bundle, self.source)
        self.write(self.source / "scripts/browser-client.mjs", "old nativePipe client")
        self.write(self.source / ".codex-plugin/plugin.json", json.dumps({"name": "chrome", "version": "old"}))
        shutil.copytree(self.source, self.cache / "old")
        self.command = self.app / "Contents/Resources/cua_node/bin/node_repl"
        self.write(self.command, "signed runtime")
        self.service = self.home / "plugins/cache/browser/current/browser-service.mjs"
        self.write(self.service, "current service")
        self.marketplace = self.source.parent.parent
        self.write(self.marketplace / ".agents/plugins/marketplace.json", json.dumps({
            "plugins": [{"name": "chrome", "source": {"source": "local", "path": "./plugins/chrome"}}]}))
        self.version = "old"
        self.enabled = True
        self.fail_install = False
        self.remove_old_cache = False
        self.remove_all_old_caches = False
        self.change_old_cache = False
        self.calls = []
        self.source_override = None
        self.runtime = {"enabled": True, "transport": {"command": str(self.command), "env": {
            "NODE_REPL_TRUSTED_SERVICES": json.dumps({"browser": str(self.service)})}}}
        self.runner = patch.object(sync, "_run", side_effect=self.run_cli).start()
        self.signature = patch.object(sync, "_signature").start()
        self.addCleanup(patch.stopall)

    def write(self, path, text):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)

    def run_cli(self, executable, home, arguments, config_args=()):
        self.calls.append((arguments, config_args))
        if arguments[:2] == ["plugin", "list"]:
            return {"installed": [{"pluginId": "chrome@openai-bundled", "enabled": self.enabled,
                "version": self.version, "source": {"source": "local", "path": str(self.source)},
                "marketplaceSource": {"sourceType": "local", "source": str(self.source_override or self.marketplace)}}]}
        if arguments[:2] == ["mcp", "get"]:
            return self.runtime
        if arguments[:2] == ["plugin", "add"]:
            if self.remove_all_old_caches:
                for cached in self.cache.iterdir():
                    if cached.name != "new" and cached.is_dir():
                        shutil.rmtree(cached)
            if self.remove_old_cache:
                shutil.rmtree(self.cache / "old")
            if self.change_old_cache:
                self.write(self.cache / "old/scripts/browser-client.mjs", "installer changed old version")
            if self.fail_install:
                raise sync.BrowserSyncError("installer failed")
            shutil.copytree(self.source, self.cache / "new", dirs_exist_ok=True)
            self.version = "new"
            return {}
        self.fail("Unexpected CLI command")

    def synchronize(self, **kwargs):
        return sync.synchronize(self.home, self.root / "codex", **kwargs)

    def test_old_client_reconciled_and_second_launch_noop(self):
        old_files = sync._inventory(self.cache / "old")
        self.assertEqual(self.synchronize(), "updated")
        self.assertEqual(sync._inventory(self.source), sync._inventory(self.bundle))
        self.assertEqual(sync._inventory(self.cache / "new"), sync._inventory(self.bundle))
        self.assertEqual(sync._inventory(self.cache / "old"), old_files)
        backups = list((self.home / ".tmp/browser-sync").glob("previous-chrome-*"))
        self.assertEqual(len(backups), 1)
        self.assertEqual(sync._inventory(backups[0]), old_files)
        self.assertEqual(self.synchronize(), "current")
        self.signature.assert_called_once_with(self.app)
        self.assertEqual(sum(args[:2] == ["plugin", "add"] for args, _ in self.calls), 1)

    def test_stale_source_repaired_when_installed_client_current(self):
        shutil.copytree(self.bundle, self.cache / "new")
        self.version = "new"
        self.assertEqual(self.synchronize(), "updated")
        self.assertFalse(any(args[:2] == ["plugin", "add"] for args, _ in self.calls))

    def test_installer_removing_old_cache_restores_running_session_imports(self):
        original = sync._inventory(self.cache / "old")
        self.remove_old_cache = True
        self.assertEqual(self.synchronize(), "updated")
        self.assertEqual(sync._inventory(self.cache / "old"), original)
        self.assertEqual(sync._inventory(self.cache / "new"), sync._inventory(self.bundle))

    def test_installer_changing_old_cache_preserves_and_restores_it(self):
        original = sync._inventory(self.cache / "old")
        self.change_old_cache = True
        self.assertEqual(self.synchronize(), "updated")
        self.assertEqual(sync._inventory(self.cache / "old"), original)
        self.assertEqual(len(list((self.home / ".tmp/browser-sync").glob("changed-cache-*"))), 1)

    def test_all_older_versions_restored_for_long_running_sessions(self):
        shutil.copytree(self.cache / "old", self.cache / "older")
        self.write(self.cache / "older/scripts/browser-client.mjs", "even older session client")
        before = {name: sync._inventory(self.cache / name) for name in ("old", "older")}
        self.remove_all_old_caches = True
        self.assertEqual(self.synchronize(), "updated")
        for name, inventory in before.items():
            self.assertEqual(sync._inventory(self.cache / name), inventory)

    def test_failed_installer_restores_removed_old_cache(self):
        original = sync._inventory(self.cache / "old")
        self.remove_old_cache = True
        self.fail_install = True
        with self.assertRaisesRegex(sync.BrowserSyncError, "installer failed"):
            self.synchronize()
        self.assertEqual(sync._inventory(self.cache / "old"), original)

    def test_same_version_repair_keeps_new_valid_contents(self):
        shutil.copytree(self.bundle, self.cache / "new")
        self.write(self.cache / "new/scripts/browser-client.mjs", "stale bytes under current version")
        self.version = "new"
        self.assertEqual(self.synchronize(), "updated")
        self.assertEqual(sync._inventory(self.cache / "new"), sync._inventory(self.bundle))
        backups = list((self.home / ".tmp/browser-sync").glob("previous-cache-*"))
        self.assertEqual(len(backups), 1)
        self.assertEqual(sync._inventory(backups[0]), sync._inventory(self.cache / "old"))

    def test_service_mismatch_rejected_before_writes(self):
        self.service.write_text("different runtime protocol")
        before = sync._inventory(self.home)
        with self.assertRaisesRegex(sync.BrowserSyncError, "runtime changed"):
            self.synchronize()
        self.assertEqual(before, sync._inventory(self.home))
        self.signature.assert_not_called()

    def test_cross_account_source_rejected_before_writes(self):
        self.source_override = self.root / "other-account/.tmp/bundled-marketplaces/openai-bundled"
        before = sync._inventory(self.home)
        with self.assertRaisesRegex(sync.BrowserSyncError, "account-local"):
            self.synchronize()
        self.assertEqual(before, sync._inventory(self.home))

    def test_symlinked_source_rejected(self):
        target = self.root / "shared-chrome"
        self.source.rename(target)
        self.source.symlink_to(target, target_is_directory=True)
        before = sync._inventory(target)
        with self.assertRaisesRegex(sync.BrowserSyncError, "symlinked"):
            self.synchronize()
        self.assertEqual(before, sync._inventory(target))

    def test_failed_installer_restores_original_source(self):
        original = sync._inventory(self.source)
        self.fail_install = True
        with self.assertRaisesRegex(sync.BrowserSyncError, "installer failed"):
            self.synchronize()
        self.assertEqual(sync._inventory(self.source), original)
        self.assertEqual(list((self.home / ".tmp/browser-sync").glob("stage-*")), [])
        self.assertEqual(list((self.home / ".tmp/browser-sync").glob("previous-chrome-*")), [])

    def test_invalid_signature_prevents_source_change(self):
        original = sync._inventory(self.source)
        self.signature.side_effect = sync.BrowserSyncError("signature failed")
        with self.assertRaisesRegex(sync.BrowserSyncError, "signature failed"):
            self.synchronize()
        self.assertEqual(sync._inventory(self.source), original)

    def test_disabled_chrome_is_skipped_without_runtime_inspection(self):
        self.enabled = False
        self.assertEqual(self.synchronize(), "skipped")
        self.assertEqual(len(self.calls), 1)

    def test_effective_config_arguments_forwarded_everywhere(self):
        flags = ("-c", "example=true")
        self.synchronize(config_args=flags)
        self.assertTrue(all(seen == flags for _, seen in self.calls))

    def test_cli_failure_does_not_disclose_captured_secrets(self):
        with patch.object(sync.subprocess, "run", return_value=subprocess.CompletedProcess(
                [], 1, stdout="secret-env", stderr="secret-env")):
            with self.assertRaisesRegex(sync.BrowserSyncError, "command failed") as caught:
                RUN_CLI("codex", self.home, ["mcp", "get", "node_repl", "--json"])
            self.assertNotIn("secret-env", str(caught.exception))

    def test_unsupported_profile_fails_explicitly(self):
        with self.assertRaisesRegex(sync.BrowserSyncError, "cannot inspect --profile"):
            RUN_CLI("codex", self.home, ["plugin", "list", "--json"], ("-p", "work"))

    def test_timeout_does_not_disclose_secret_override(self):
        with patch.object(sync.subprocess, "run", side_effect=subprocess.TimeoutExpired(
                ["codex", "-c", "token=secret-value"], 120, output="secret-value")):
            with self.assertRaisesRegex(sync.BrowserSyncError, "timed out") as caught:
                RUN_CLI("codex", self.home, ["plugin", "list", "--json"], ("-c", "token=secret-value"))
            self.assertNotIn("secret-value", str(caught.exception))

    def test_launch_error_does_not_disclose_diagnostic(self):
        with patch.object(sync.subprocess, "run", side_effect=OSError("secret-value")):
            with self.assertRaisesRegex(sync.BrowserSyncError, "could not start") as caught:
                RUN_CLI("codex", self.home, ["plugin", "list", "--json"])
            self.assertNotIn("secret-value", str(caught.exception))


if __name__ == "__main__":
    unittest.main()
