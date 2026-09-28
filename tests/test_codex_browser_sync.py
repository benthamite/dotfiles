"""Regression checks for account-local desktop Chrome reconciliation."""

import importlib.util
from contextlib import contextmanager
import hashlib
import json
import os
from pathlib import Path
import plistlib
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch


sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "bin"))
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
        self.node = self.command.parent / "node"
        self.write(self.node, "signed node")
        self.modules = self.app / "Contents/Resources/cua_node/lib/node_modules"
        self.modules.mkdir(parents=True)
        self.app_cli = self.app / "Contents/Resources/codex"
        self.write(self.app_cli, "signed app CLI")
        self.write(self.app_cli.parent / "codex-code-mode-host", "signed app helper")
        (self.app / "Contents/Info.plist").write_bytes(plistlib.dumps({"CFBundleShortVersionString": "new"}))
        self.write(self.app / "Contents/_CodeSignature/CodeResources", "app seal")
        self.service = self.home / "plugins/cache/openai-bundled/browser/old/scripts/browser-service.mjs"
        self.write(self.service, "current service")
        self.marketplace = self.source.parent.parent
        self.write(self.marketplace / ".agents/plugins/marketplace.json", json.dumps({
            "plugins": [{"name": "chrome", "source": {"source": "local", "path": "./plugins/chrome"}}]}))
        self.version = "old"
        self.enabled = True
        self.fail_install = False
        self.remove_new_cache = False
        self.remove_latest_alias = False
        self.remove_old_cache = False
        self.remove_all_old_caches = False
        self.change_old_cache = False
        self.calls = []
        self.source_override = None
        self.runtime = {"enabled": True, "transport": {"command": str(self.command), "env": {
            "NODE_REPL_TRUSTED_SERVICES": json.dumps({"browser": str(self.service)}),
            "NODE_REPL_NODE_PATH": str(self.node), "NODE_REPL_NODE_MODULE_DIRS": str(self.modules),
            "NODE_REPL_TRUSTED_CODE_PATHS": os.pathsep.join((str(self.home), str(self.modules))),
            "BROWSER_USE_CODEX_APP_VERSION": "old"}}}
        self.runtime["transport"]["env"]["CODEX_CLI_PATH"] = str(self.app_cli)
        self.runner = patch.object(sync, "_run", side_effect=self.run_cli).start()
        self.signature = patch.object(sync, "_signature").start()
        patch.object(sync, "preserve_chrome_preference", side_effect=self.preserve_preference).start()
        patch.object(sync, "snapshot_runtime", side_effect=self.snapshot).start()
        patch.object(sync, "snapshot_tree", side_effect=self.snapshot).start()
        patch.object(sync, "snapshot_app_cli", side_effect=self.snapshot_cli).start()
        self.addCleanup(patch.stopall)

    def write(self, path, text):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)

    def snapshot(self, source, store):
        if source.name == "cua_node":
            store = self.root / "retained-runtime"
        identity = hashlib.sha256(repr(sync._inventory(source)).encode()).hexdigest()
        target = store / identity / "assets"
        if not target.exists():
            target.parent.mkdir(parents=True)
            shutil.copytree(source, target)
        return target

    def snapshot_cli(self, source, store):
        names = ("codex", "codex-code-mode-host")
        identity = hashlib.sha256(b"".join((source / name).read_bytes() for name in names)).hexdigest()
        target = self.root / "retained-cli" / identity / "assets"
        if not target.exists():
            target.mkdir(parents=True)
            for name in names:
                shutil.copy2(source / name, target / name)
        return target

    @contextmanager
    def preserve_preference(self, executable, home, config_args=()):
        before = self.enabled
        try:
            yield
        finally:
            self.enabled = before

    def run_cli(self, executable, home, arguments, config_args=()):
        self.calls.append((arguments, config_args))
        if arguments[:2] == ["plugin", "list"]:
            return {"installed": [{"pluginId": "chrome@openai-bundled", "enabled": self.enabled,
                "version": self.version, "source": {"source": "local", "path": str(self.source)},
                "marketplaceSource": {"sourceType": "local", "source": str(self.source_override or self.marketplace)}}]}
        if arguments[:2] == ["mcp", "get"]:
            return self.runtime
        if arguments[:2] == ["plugin", "add"]:
            self.enabled = True
            next_version = json.loads((self.source / ".codex-plugin/plugin.json").read_text())["version"]
            if self.remove_latest_alias:
                (self.cache / "latest").unlink()
            if self.remove_all_old_caches:
                for cached in self.cache.iterdir():
                    if cached.name != next_version and cached.is_dir():
                        shutil.rmtree(cached)
            if self.remove_old_cache:
                shutil.rmtree(self.cache / "old")
            if self.change_old_cache:
                self.write(self.cache / "old/scripts/browser-client.mjs", "installer changed old version")
            if self.remove_new_cache:
                shutil.rmtree(self.cache / next_version)
            if self.fail_install:
                raise sync.BrowserSyncError("installer failed")
            shutil.copytree(self.source, self.cache / next_version, dirs_exist_ok=True)
            self.version = next_version
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

    def test_same_version_reinstall_preserves_existing_latest_alias(self):
        self.version = "new"
        shutil.copytree(self.source, self.cache / "new")
        alias = self.cache / "latest"
        alias.symlink_to("new")
        self.remove_latest_alias = True
        self.assertEqual(self.synchronize(), "updated")
        self.assertEqual(os.readlink(alias), "new")
        self.assertEqual(sync._inventory(alias.resolve()), sync._inventory(self.bundle))

    def test_upgrade_restores_latest_target_removed_by_installer(self):
        alias = self.cache / "latest"
        alias.symlink_to("old")
        original = sync._inventory(self.cache / "old")
        self.remove_old_cache = True
        self.assertEqual(self.synchronize(), "updated")
        self.assertEqual(os.readlink(alias), "old")
        self.assertEqual(sync._inventory(alias.resolve()), original)

    def test_failed_install_restores_existing_latest_alias(self):
        alias = self.cache / "latest"
        alias.symlink_to("old")
        self.remove_latest_alias = True
        self.remove_old_cache = True
        self.fail_install = True
        with self.assertRaisesRegex(sync.BrowserSyncError, "installer failed"):
            self.synchronize()
        self.assertEqual(os.readlink(alias), "old")
        self.assertTrue(alias.is_dir())

    def test_absent_latest_alias_is_not_created(self):
        self.assertEqual(self.synchronize(), "updated")
        self.assertFalse((self.cache / "latest").is_symlink())

    def test_external_latest_alias_rejected_before_install(self):
        (self.cache / "latest").symlink_to(self.source)
        with self.assertRaisesRegex(sync.BrowserSyncError, "outside"):
            self.synchronize()
        self.assertFalse(any(args[:2] == ["plugin", "add"] for args, _ in self.calls))

    def test_missing_latest_target_rejected_before_install(self):
        (self.cache / "latest").symlink_to("missing")
        with self.assertRaisesRegex(sync.BrowserSyncError, "no installed target"):
            self.synchronize()
        self.assertFalse(any(args[:2] == ["plugin", "add"] for args, _ in self.calls))

    def test_recovery_preserves_valid_installer_alias(self):
        shutil.copytree(self.source, self.cache / "new")
        alias = self.cache / "latest"
        alias.symlink_to("new")
        sync._restore_cache_alias(self.cache, "old")
        self.assertEqual(os.readlink(alias), "new")

    def test_recovery_never_creates_dangling_alias(self):
        with self.assertRaisesRegex(sync.BrowserSyncError, "target is missing"):
            sync._restore_cache_alias(self.cache, "missing")
        self.assertFalse((self.cache / "latest").is_symlink())

    def test_non_symlink_latest_rejected_before_install(self):
        self.write(self.cache / "latest", "unexpected file")
        with self.assertRaisesRegex(sync.BrowserSyncError, "not a symlink"):
            self.synchronize()
        self.assertFalse(any(args[:2] == ["plugin", "add"] for args, _ in self.calls))

    def test_failed_same_version_install_restores_alias_and_target(self):
        self.version = "new"
        shutil.copytree(self.source, self.cache / "new")
        original = sync._inventory(self.cache / "new")
        alias = self.cache / "latest"
        alias.symlink_to("new")
        self.remove_latest_alias = True
        self.remove_new_cache = True
        self.fail_install = True
        with self.assertRaisesRegex(sync.BrowserSyncError, "installer failed"):
            self.synchronize()
        self.assertEqual(os.readlink(alias), "new")
        self.assertEqual(sync._inventory(alias.resolve()), original)

    def test_recovery_failure_keeps_original_install_error(self):
        (self.cache / "latest").symlink_to("old")
        self.fail_install = True
        with patch.object(sync, "_restore_cache_alias", side_effect=OSError("recovery denied")):
            with self.assertRaisesRegex(sync.BrowserSyncError,
                                        "installer failed.*recovery also failed.*recovery denied"):
                self.synchronize()

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

    def test_custom_service_rejected_before_asset_writes(self):
        self.runtime["transport"]["env"]["NODE_REPL_TRUSTED_SERVICES"] = json.dumps({"browser": str(self.root / "custom.mjs")})
        before = sync._inventory(self.home)
        with self.assertRaisesRegex(sync.BrowserSyncError, "Custom browser service"):
            self.synchronize()
        after = sync._inventory(self.home)
        after.pop(".tmp/browser-sync/lock", None)
        self.assertEqual(before, after)
        self.signature.assert_not_called()

    def test_cross_account_source_rejected_before_writes(self):
        self.source_override = self.root / "other-account/.tmp/bundled-marketplaces/openai-bundled"
        before = sync._inventory(self.home)
        with self.assertRaisesRegex(sync.BrowserSyncError, "account-local"):
            self.synchronize()
        after = sync._inventory(self.home)
        after.pop(".tmp/browser-sync/lock", None)
        self.assertEqual(before, after)

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

    def test_disabled_node_repl_preserves_user_choice(self):
        self.runtime["enabled"] = False
        original = sync._inventory(self.source)
        self.assertEqual(sync.prepare_launch(self.home, self.root / "codex"), [])
        self.assertEqual(sync._inventory(self.source), original)
        self.signature.assert_not_called()
        self.assertFalse(any(args[:2] == ["plugin", "add"] for args, _ in self.calls))

    def test_effective_config_arguments_forwarded_everywhere(self):
        flags = ("-c", "example=true")
        self.synchronize(config_args=flags)
        self.assertTrue(all(seen == flags for _, seen in self.calls))

    @unittest.skipUnless(Path("/opt/homebrew/bin/codex").is_file(), "requires the installed Codex CLI")
    def test_cd_applies_project_configuration_to_real_cli(self):
        project = self.root / "project"
        self.write(project / ".codex/config.toml", '[mcp_servers.node_repl]\ncommand = "PROJECT"\n')
        self.write(self.home / "config.toml", '[mcp_servers.node_repl]\ncommand = "BASE"\n'
                   + '[projects.' + json.dumps(str(project)) + ']\ntrust_level = "trusted"\n')
        result = RUN_CLI(Path("/opt/homebrew/bin/codex"), self.home,
                         ["mcp", "get", "node_repl", "--json"], ("-C", str(project)))
        self.assertEqual(result["transport"]["command"], "PROJECT")

    def launch_values(self, **options):
        overrides = sync.prepare_launch(self.home, self.root / "codex", **options)
        self.assertEqual(overrides[::2], ["-c"] * (len(overrides) // 2))
        return {name: json.loads(value) for name, value in
                (item.split("=", 1) for item in overrides[1::2])}

    def test_only_aware_clients_suppress_mutable_chrome_plugin(self):
        self.assertNotIn("plugins.chrome@openai-bundled.enabled", self.launch_values())
        values = self.launch_values(pin_client=True)
        self.assertIs(values["plugins.chrome@openai-bundled.enabled"], False)
        services = json.loads(values["mcp_servers.node_repl.env.NODE_REPL_TRUSTED_SERVICES"])
        self.assertIn(".browser-runtimes", services["browser"])
        self.assertIs(self.enabled, True)

    def test_client_pinning_rejects_bundle_without_skill(self):
        (self.bundle / "skills/control-chrome/SKILL.md").unlink()
        with self.assertRaisesRegex(sync.BrowserSyncError, "lacks its browser skill"):
            self.launch_values(pin_client=True)

    def test_app_releases_refresh_client_service_and_version_as_unit(self):
        original_runtime = json.dumps(self.runtime, sort_keys=True)
        self.service.write_text("old registered service")
        self.remove_all_old_caches = True
        retained = []
        for release in ("new", "next"):
            self.write(self.bundle / ".codex-plugin/plugin.json", json.dumps({"name": "chrome", "version": release}))
            self.write(self.bundle / "scripts/browser-service.mjs", release + " service")
            self.write(self.bundle / "scripts/browser-client.mjs", release + " client")
            self.command.write_text(release + " runtime")
            self.app_cli.write_text(release + " matching app CLI")
            (self.app / "Contents/Info.plist").write_bytes(plistlib.dumps({"CFBundleShortVersionString": release}))
            values = self.launch_values()
            retained.append(values)
            services = json.loads(values["mcp_servers.node_repl.env.NODE_REPL_TRUSTED_SERVICES"])
            self.assertTrue(Path(services["browser"]).is_relative_to(self.home / ".browser-runtimes"))
            self.assertEqual(Path(services["browser"]).read_text(), release + " service")
            self.assertEqual(values["mcp_servers.node_repl.env.BROWSER_USE_CODEX_APP_VERSION"], release)
            self.assertEqual(sync._inventory(self.cache / release), sync._inventory(self.bundle))
        self.assertTrue((self.cache / "new/scripts/browser-client.mjs").exists())
        self.assertEqual(self.service.read_text(), "old registered service")
        self.assertEqual(json.dumps(self.runtime, sort_keys=True), original_runtime)
        self.assertEqual(Path(retained[0]["mcp_servers.node_repl.command"]).read_text(), "new runtime")
        previous_service = json.loads(retained[0]["mcp_servers.node_repl.env.NODE_REPL_TRUSTED_SERVICES"])["browser"]
        self.assertEqual(Path(previous_service).read_text(), "new service")
        self.assertEqual(Path(retained[0]["mcp_servers.node_repl.env.CODEX_CLI_PATH"]).read_text(), "new matching app CLI")
        self.assertEqual(Path(retained[1]["mcp_servers.node_repl.env.CODEX_CLI_PATH"]).read_text(), "next matching app CLI")

    def test_other_services_preserved_and_trust_roots_not_overridden(self):
        self.runtime["transport"]["env"]["NODE_REPL_TRUSTED_SERVICES"] = json.dumps({
            "browser": str(self.service), "sky": "@oai/sky/service"})
        values = self.launch_values()
        services = json.loads(values["mcp_servers.node_repl.env.NODE_REPL_TRUSTED_SERVICES"])
        self.assertEqual(services["sky"], "@oai/sky/service")
        roots = values["mcp_servers.node_repl.env.NODE_REPL_TRUSTED_CODE_PATHS"].split(os.pathsep)
        self.assertEqual(roots[0], str(self.home))
        self.assertEqual(roots[1], values["mcp_servers.node_repl.env.NODE_REPL_NODE_MODULE_DIRS"])
        self.assertTrue(Path(roots[1]).is_relative_to(self.root / "retained-runtime"))

    def test_missing_trust_for_new_service_fails_without_widening(self):
        self.runtime["transport"]["env"]["NODE_REPL_TRUSTED_CODE_PATHS"] = str(self.modules)
        original = sync._inventory(self.source)
        with self.assertRaisesRegex(sync.BrowserSyncError, "outside existing trusted"):
            self.launch_values()
        self.assertEqual(sync._inventory(self.source), original)

    def test_custom_node_runtime_path_is_not_replaced(self):
        self.runtime["transport"]["env"]["NODE_REPL_NODE_PATH"] = "/custom/node"
        with self.assertRaisesRegex(sync.BrowserSyncError, "Custom browser runtime"):
            self.launch_values()

    def test_custom_browser_helper_cli_is_not_replaced(self):
        self.runtime["transport"]["env"]["CODEX_CLI_PATH"] = "/custom/codex"
        with self.assertRaisesRegex(sync.BrowserSyncError, "Custom browser helper CLI"):
            self.launch_values()

    def test_secret_like_service_value_never_enters_override_argv(self):
        self.runtime["transport"]["env"]["NODE_REPL_TRUSTED_SERVICES"] = json.dumps({
            "browser": str(self.service), "other": "secret-value"})
        with self.assertRaisesRegex(sync.BrowserSyncError, "unsupported value") as caught:
            self.launch_values()
        self.assertNotIn("secret-value", str(caught.exception))

    def test_app_replacement_during_retention_fails_closed(self):
        def changing_snapshot(source, store):
            result = self.snapshot(source, store)
            (self.app / "Contents/_CodeSignature/CodeResources").write_text("replacement generation")
            return result
        with patch.object(sync, "snapshot_runtime", side_effect=changing_snapshot):
            with self.assertRaisesRegex(sync.BrowserSyncError, "changed while preparing"):
                self.launch_values()

    def test_retention_failure_keeps_actionable_safe_diagnostic(self):
        with patch.object(sync, "snapshot_runtime", side_effect=sync.RuntimeSnapshotError("Source directory changed during inventory")):
            with self.assertRaisesRegex(sync.BrowserSyncError, "Source directory changed during inventory"):
                self.launch_values()

    def test_app_replacement_during_install_does_not_mix_generations(self):
        def changing_install(executable, home, arguments, config_args=()):
            result = self.run_cli(executable, home, arguments, config_args)
            if arguments[:2] == ["plugin", "add"]:
                (self.app / "Contents/_CodeSignature/CodeResources").write_text("new generation")
            return result
        with patch.object(sync, "_run", side_effect=changing_install):
            with self.assertRaisesRegex(sync.BrowserSyncError, "changed while installing"):
                self.launch_values()

    def test_cli_failure_does_not_disclose_captured_secrets(self):
        with patch.object(sync.subprocess, "run", return_value=subprocess.CompletedProcess(
                [], 1, stdout="secret-env", stderr="secret-env")):
            with self.assertRaisesRegex(sync.BrowserSyncError, "command failed") as caught:
                RUN_CLI("codex", self.home, ["mcp", "get", "node_repl", "--json"])
            self.assertNotIn("secret-env", str(caught.exception))

    def test_plugin_commands_omit_unsupported_profile_flag(self):
        with patch.object(sync.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, stdout="{}")) as run:
            RUN_CLI("codex", self.home, ["plugin", "list", "--json"], ("-p", "work", "-c", "example=true"))
            self.assertNotIn("-p", run.call_args.args[0])
            self.assertIn("example=true", run.call_args.args[0])

    def test_profile_disabled_chrome_skips_runtime_reconciliation(self):
        with patch.object(sync, "chrome_enabled", return_value=False):
            self.assertEqual(sync.prepare_launch(self.home, self.root / "codex", ("-p", "work")), [])
        self.assertEqual(len(self.calls), 1)

    def test_profile_can_enable_base_disabled_chrome(self):
        self.enabled = False
        shutil.copytree(self.bundle, self.cache / "new")
        self.version = "new"
        with patch.object(sync, "chrome_enabled", return_value=True):
            self.assertTrue(sync.prepare_launch(self.home, self.root / "codex", ("-p", "work")))

    def test_profile_only_upgrade_preserves_base_disabled_preference(self):
        self.enabled = False
        with patch.object(sync, "chrome_enabled", return_value=True):
            self.assertTrue(sync.prepare_launch(self.home, self.root / "codex", ("-p", "work")))
        self.assertEqual(sync._inventory(self.source), sync._inventory(self.bundle))
        self.assertFalse(self.enabled)

    def test_profile_preference_restored_even_if_installation_raises(self):
        self.enabled = False
        self.fail_install = True
        before = sync._inventory(self.source)
        with patch.object(sync, "chrome_enabled", return_value=True):
            with self.assertRaisesRegex(sync.BrowserSyncError, "installer failed"):
                sync.prepare_launch(self.home, self.root / "codex", ("-p", "work"))
        self.assertFalse(self.enabled)
        self.assertEqual(sync._inventory(self.source), before)

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
