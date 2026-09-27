"""Profile-aware Chrome discovery must not infer availability from user text."""

import importlib.util
import io
import json
from pathlib import Path
import subprocess
import unittest
from unittest.mock import Mock, patch


SPEC = importlib.util.spec_from_file_location(
    "codex_profile_plugins", Path(__file__).resolve().parents[1] / "bin/codex_profile_plugins.py")
profiles = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(profiles)
ROOT = Path("/home/example/.codex/plugins/cache/openai-bundled/chrome/1.0")


def catalogue(enabled=True):
    entry = ("- chrome:control-chrome: Browser control. "
             "(file: r0/skills/control-chrome/SKILL.md)\n") if enabled else ""
    return {"type": "message", "role": "developer", "content": [
        {"type": "input_text", "text": (
            "<skills_instructions>\n## Skills\n### Skill roots\n"
            f"- `r0` = `{ROOT}`\n### Available skills\n{entry}</skills_instructions>")}]}


class ProfilePluginTests(unittest.TestCase):
    def test_cwd_splitter_preserves_opaque_values_and_profile(self):
        args = ["-c", "--cd", "-p", "work", "-C", "relative", "--enable", "-C"]
        with patch.object(profiles.os, "getcwd", return_value="/launch"):
            self.assertEqual(profiles.split_working_directory(args),
                             (["-c", "--cd", "-p", "work", "--enable", "-C"], "/launch/relative"))

    def test_cwd_splitter_attached_forms_and_no_cwd(self):
        for arg in ("-C/target", "-C=/target", "--cd=/target"):
            self.assertEqual(profiles.split_working_directory([arg, "--profile=work"]),
                             (["--profile=work"], "/target"))
        self.assertEqual(profiles.split_working_directory(["-c", "x=y"]), (["-c", "x=y"], None))
        with self.assertRaises(profiles.ProfilePluginsError):
            profiles.split_working_directory(["--cd"])

    def test_profile_diagnostic_uses_explicit_cwd_without_cd_flag(self):
        result = subprocess.CompletedProcess([], 0, json.dumps([catalogue()]), "")
        with patch.object(profiles.subprocess, "run", return_value=result) as run:
            self.assertTrue(profiles.chrome_enabled("codex", "/home/example/.codex",
                                                   ["-p", "work", "--cd=/project"], ROOT))
            self.assertEqual(run.call_args.args[0], ["codex", "-p", "work", "debug", "prompt-input"])
            self.assertEqual(run.call_args.kwargs["cwd"], "/project")

    def test_restore_false_and_absent_with_post_install_version(self):
        for original in (False, None):
            with self.subTest(original=original):
                states = [dict(value=original, file="/home/example/.codex/config.toml", version="before"),
                          dict(value=True, file="/home/example/.codex/config.toml", version="after"),
                          dict(value=original, file="/home/example/.codex/config.toml", version="restored")]
                with patch.object(profiles, "_PreferenceServer") as server_class, \
                        patch.object(profiles, "_read_chrome_preference", side_effect=states):
                    server = server_class.return_value
                    server.request.return_value = {"status": "ok"}
                    with profiles.preserve_chrome_preference("codex", "/home/example/.codex", ["-p", "work"]):
                        pass
                    params = server.request.call_args.args[1]
                    self.assertEqual(params["expectedVersion"], "after")
                    self.assertIs(params["edits"][0]["value"], original)
                    server.close.assert_called_once()

    def test_restore_runs_when_installer_raises(self):
        states = [dict(value=False, file="/home/config.toml", version="before"),
                  dict(value=True, file="/home/config.toml", version="after"),
                  dict(value=False, file="/home/config.toml", version="restored")]
        with patch.object(profiles, "_PreferenceServer") as server_class, \
                patch.object(profiles, "_read_chrome_preference", side_effect=states):
            server_class.return_value.request.return_value = {"status": "ok"}
            with self.assertRaisesRegex(RuntimeError, "installer failed"):
                with profiles.preserve_chrome_preference("codex", "/home"):
                    raise RuntimeError("installer failed")
            server_class.return_value.request.assert_called_once()

    def test_unchanged_true_requires_no_config_write(self):
        state = dict(value=True, file="/home/config.toml", version="same")
        with patch.object(profiles, "_PreferenceServer") as server_class, \
                patch.object(profiles, "_read_chrome_preference", return_value=state):
            with profiles.preserve_chrome_preference("codex", "/home"):
                pass
            server_class.return_value.request.assert_not_called()

    def test_different_concurrent_preference_edit_is_not_overwritten(self):
        states = [dict(value=True, file="/home/config.toml", version="before"),
                  dict(value=False, file="/home/config.toml", version="after")]
        with patch.object(profiles, "_PreferenceServer") as server_class, \
                patch.object(profiles, "_read_chrome_preference", side_effect=states):
            with self.assertRaises(profiles.ProfilePluginsError):
                with profiles.preserve_chrome_preference("codex", "/home"):
                    pass
            server_class.return_value.request.assert_not_called()

    def test_read_uses_user_layer_not_effective_or_project_value(self):
        server = Mock()
        server.request.return_value = {
            "config": {"plugins": {"chrome@openai-bundled": {"enabled": True}}},
            "layers": [
                {"name": {"type": "project", "file": "/project/config.toml"},
                 "config": {"plugins": {"chrome@openai-bundled": {"enabled": True}}}},
                {"name": {"type": "user", "file": "/home/config.toml", "profile": None},
                 "version": "v1", "config": {"plugins": {"chrome@openai-bundled": {"enabled": False}}}}]}
        state = profiles._read_chrome_preference(server, Path("/home"))
        self.assertIs(state["value"], False)
        server.request.return_value["layers"][1]["name"]["file"] = "/other/config.toml"
        with self.assertRaises(profiles.ProfilePluginsError):
            profiles._read_chrome_preference(server, Path("/home"))

    def test_rpc_error_content_is_not_exposed(self):
        server = profiles._PreferenceServer.__new__(profiles._PreferenceServer)
        server.sequence = 0
        server.process = Mock(stdin=io.BytesIO())
        server.pending = b'{"id":1,"error":{"message":"PRIVATE_CONFIG_VALUE"}}\n'
        with self.assertRaises(profiles.ProfilePluginsError) as caught:
            server.request("config/read", {"includeLayers": True})
        self.assertNotIn("PRIVATE_CONFIG_VALUE", str(caught.exception))

    def test_successful_overridden_write_still_verifies_persisted_leaf(self):
        states = [dict(value=False, file="/home/config.toml", version="before"),
                  dict(value=True, file="/home/config.toml", version="after"),
                  dict(value=False, file="/home/config.toml", version="restored")]
        with patch.object(profiles, "_PreferenceServer") as server_class, \
                patch.object(profiles, "_read_chrome_preference", side_effect=states):
            server_class.return_value.request.return_value = {"status": "okOverridden"}
            with profiles.preserve_chrome_preference("codex", "/home"):
                pass

    def test_split_preserves_opaque_option_values(self):
        args = ["-c", "-p", "--profile", "work", "--enable", "example"]
        self.assertEqual(profiles.split_profile_args(args),
                         (["-c", "-p", "--enable", "example"], True))
        self.assertEqual(args[2:4], ["--profile", "work"])

    def test_attached_profile_forms(self):
        for arg in ("-pwork", "-p=work", "--profile=work"):
            self.assertEqual(profiles.split_profile_args([arg]), ([], True))
        self.assertEqual(profiles.split_profile_args(["-c", "model=example"]),
                         (["-c", "model=example"], False))

    def test_missing_value_fails(self):
        with self.assertRaises(profiles.ProfilePluginsError):
            profiles.split_profile_args(["-p"])

    def test_only_exact_active_catalogue_entry_counts(self):
        self.assertTrue(profiles._chrome_in_prompt([catalogue()], ROOT))
        self.assertFalse(profiles._chrome_in_prompt([catalogue(False)], ROOT))
        user = catalogue()
        user["role"] = "user"
        self.assertFalse(profiles._chrome_in_prompt([user, catalogue(False)], ROOT))

    def test_real_catalogue_trailing_newline_and_description_colon(self):
        message = catalogue()
        message["content"][0]["text"] = message["content"][0]["text"].replace(
            "Browser control.", "Existing state: tabs and extensions.") + "\n"
        self.assertTrue(profiles._chrome_in_prompt([message], ROOT))

    def test_unknown_root_does_not_report_disabled(self):
        message = catalogue()
        message["content"][0]["text"] = message["content"][0]["text"].replace(
            "file: r0/", "file: r9/")
        with self.assertRaises(profiles.ProfilePluginsError):
            profiles._chrome_in_prompt([message], ROOT)

    def test_wrong_installation_and_duplicate_catalogues_fail(self):
        for prompt in ([catalogue(), catalogue()], [catalogue()]):
            expected = ROOT if len(prompt) == 2 else ROOT.parent / "other"
            with self.assertRaises(profiles.ProfilePluginsError):
                profiles._chrome_in_prompt(prompt, expected)

    def test_no_catalogue_or_changed_structure_does_not_guess(self):
        for prompt in ([], {}, [{"role": "developer", "content": []}]):
            with self.assertRaises(profiles.ProfilePluginsError):
                profiles._chrome_in_prompt(prompt, ROOT)

    def test_subprocess_preserves_profile_and_never_reveals_failures(self):
        marker = "PRIVATE_DIAGNOSTIC_VALUE"
        errors = [subprocess.TimeoutExpired([marker], 1, output=marker),
                  OSError(marker)]
        for error in errors:
            with patch.object(profiles.subprocess, "run", side_effect=error):
                with self.assertRaises(profiles.ProfilePluginsError) as caught:
                    profiles.chrome_enabled("codex", "/home/example/.codex", ["-p", "work"], ROOT)
                self.assertNotIn(marker, str(caught.exception))
        result = subprocess.CompletedProcess([], 0, json.dumps([catalogue()]), marker)
        with patch.object(profiles.subprocess, "run", return_value=result) as run:
            self.assertTrue(profiles.chrome_enabled("codex", "/home/example/.codex", ["-p", "work"], ROOT))
            self.assertEqual(run.call_args.args[0], ["codex", "-p", "work", "debug", "prompt-input"])


if __name__ == "__main__":
    unittest.main()
