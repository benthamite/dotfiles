import contextlib
import importlib.machinery
import importlib.util
import io
import json
import pathlib
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "chrome-profile-open"


def load_module():
    loader = importlib.machinery.SourceFileLoader("chrome_profile_open", str(SCRIPT))
    spec = importlib.util.spec_from_loader("chrome_profile_open", loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules["chrome_profile_open"] = module
    spec.loader.exec_module(module)
    return module


class ChromeProfileOpenTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()

    def write_local_state(self, chrome_root, profiles):
        chrome_root.mkdir(parents=True, exist_ok=True)
        payload = {"profile": {"info_cache": profiles}}
        (chrome_root / "Local State").write_text(json.dumps(payload))

    def test_discovers_profiles_from_chrome_local_state(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            chrome_root = pathlib.Path(temp_dir) / "Chrome"
            self.write_local_state(
                chrome_root,
                {
                    "Default": {
                        "name": "Personal",
                        "user_name": "pablo.stafforini@gmail.com",
                    },
                    "Profile 4": {
                        "name": "Trajectory Labs",
                        "user_name": "pablo.stafforini@trajectorylabs.net",
                    },
                },
            )

            profiles = self.mod.discover_profiles(chrome_root)

        self.assertEqual(
            [(p.directory, p.name, p.user_name) for p in profiles],
            [
                ("Default", "Personal", "pablo.stafforini@gmail.com"),
                ("Profile 4", "Trajectory Labs", "pablo.stafforini@trajectorylabs.net"),
            ],
        )

    def test_setup_persists_alias_and_open_command_uses_it(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Chrome"
            config_path = temp / "profiles.json"
            self.write_local_state(
                chrome_root,
                {
                    "Profile 4": {
                        "name": "Trajectory Labs",
                        "user_name": "pablo.stafforini@trajectorylabs.net",
                    }
                },
            )

            self.mod.write_alias_config(
                config_path, chrome_root, "trajectory", "Profile 4"
            )
            config = self.mod.load_alias_config(config_path, "trajectory", chrome_root)
            command = self.mod.build_chrome_command(
                config,
                ["http://127.0.0.1:8770/"],
                chrome_path=pathlib.Path(
                    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
                ),
            )

        self.assertEqual(config.alias, "trajectory")
        self.assertEqual(config.profile_directory, "Profile 4")
        self.assertIn("--profile-directory=Profile 4", command)
        self.assertEqual(command[-1], "http://127.0.0.1:8770/")

    def test_macos_default_opener_uses_launch_services_new_instance(self):
        config = self.mod.OpenConfig(
            alias="trajectory",
            profile_directory="Profile 4",
            profile_name="Trajectory Labs",
            user_name="pablo.stafforini@trajectorylabs.net",
            chrome_root=pathlib.Path("/tmp/Chrome"),
        )

        with mock.patch.object(self.mod.sys, "platform", "darwin"):
            command = self.mod.build_chrome_command(
                config,
                ["http://127.0.0.1:8770/"],
            )

        self.assertEqual(
            command[:5],
            ["/usr/bin/open", "-n", "-b", "com.google.Chrome", "--args"],
        )
        self.assertIn("--profile-directory=Profile 4", command)
        self.assertIn("--user-data-dir=/tmp/Chrome", command)
        self.assertEqual(command[-1], "http://127.0.0.1:8770/")

    def test_custom_chrome_path_keeps_direct_executable_launch(self):
        config = self.mod.OpenConfig(
            alias="trajectory",
            profile_directory="Profile 4",
            profile_name="Trajectory Labs",
            user_name="pablo.stafforini@trajectorylabs.net",
            chrome_root=pathlib.Path("/tmp/Chrome"),
        )

        with mock.patch.object(self.mod.sys, "platform", "darwin"):
            command = self.mod.build_chrome_command(
                config,
                ["http://127.0.0.1:8770/"],
                chrome_path=pathlib.Path("/tmp/fake-chrome"),
            )

        self.assertEqual(command[0], "/tmp/fake-chrome")
        self.assertIn("--profile-directory=Profile 4", command)
        self.assertIn("--user-data-dir=/tmp/Chrome", command)
        self.assertEqual(command[-1], "http://127.0.0.1:8770/")

    def test_load_alias_refuses_missing_recorded_identity(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Chrome"
            config_path = temp / "profiles.json"
            original = {"name": "Research", "user_name": "owner@example.test"}
            self.write_local_state(chrome_root, {"Profile 2": original})
            self.mod.write_alias_config(config_path, chrome_root, "research", "Profile 2")

            for field in ("name", "user_name"):
                for state in ("empty", "null", "missing"):
                    with self.subTest(field=field, state=state):
                        current = dict(original)
                        if state == "missing":
                            del current[field]
                        else:
                            current[field] = "" if state == "empty" else None
                        self.write_local_state(chrome_root, {"Profile 2": current})
                        with self.assertRaisesRegex(
                            self.mod.ConfigError, "profile identity changed"
                        ):
                            self.mod.load_alias_config(config_path, "research", chrome_root)
                        output, error = io.StringIO(), io.StringIO()
                        with (
                            mock.patch.object(self.mod.subprocess, "run") as launch,
                            contextlib.redirect_stdout(output),
                            contextlib.redirect_stderr(error),
                        ):
                            status = self.mod.main([
                                "--config", str(config_path),
                                "--chrome-root", str(chrome_root),
                                "research", "https://example.test/document",
                            ])
                        self.assertEqual(status, 2)
                        self.assertIn("profile identity changed", error.getvalue())
                        self.assertEqual(output.getvalue(), "")
                        launch.assert_not_called()

    def test_load_alias_binds_canonical_inspected_root(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Actual Chrome"
            linked_root = temp / "Chrome Alias"
            config_path = temp / "profiles.json"
            self.write_local_state(
                chrome_root,
                {"Profile 2": {"name": "Research", "user_name": "owner@example.test"}},
            )
            linked_root.symlink_to(chrome_root, target_is_directory=True)
            self.mod.write_alias_config(config_path, linked_root, "research", "Profile 2")

            config = self.mod.load_alias_config(config_path, "research")
            command = self.mod.build_chrome_command(
                config, ["https://example.test/document"],
                chrome_path=pathlib.Path("/tmp/fake-chrome"),
            )

            self.assertEqual(config.chrome_root, chrome_root.resolve())
            self.assertIn(f"--user-data-dir={chrome_root.resolve()}", command)

    def test_cli_root_precedence_binds_inspection_and_launch(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            roots = {name: temp / f"{name} Chrome" for name in ("flag", "env", "config", "default")}
            config_path = temp / "profiles.json"
            for chrome_root in roots.values():
                self.write_local_state(
                    chrome_root,
                    {"Profile 2": {"name": "Research", "user_name": "owner@example.test"}},
                )
            cases = (
                ("config", False, False, True),
                ("env", False, True, True),
                ("flag", True, True, True),
                ("default", False, False, False),
            )
            for expected, explicit, environment, stored in cases:
                with self.subTest(expected=expected):
                    self.mod.write_alias_config(config_path, roots["config"], "research", "Profile 2")
                    if not stored:
                        payload = json.loads(config_path.read_text())
                        del payload["chromeRoot"]
                        config_path.write_text(json.dumps(payload))
                    argv = ["--config", str(config_path), "--dry-run", "research", "https://example.test/document"]
                    if explicit:
                        argv.extend(["--chrome-root", str(roots["flag"])])
                    environ = {"CHROME_PROFILE_OPEN_CHROME_ROOT": str(roots["env"])} if environment else {}
                    output, error = io.StringIO(), io.StringIO()
                    with (
                        mock.patch.dict(self.mod.os.environ, environ, clear=True),
                        mock.patch.object(self.mod, "DEFAULT_CHROME_ROOT", roots["default"]),
                        mock.patch.object(self.mod, "discover_profiles", wraps=self.mod.discover_profiles) as discover,
                        mock.patch.object(self.mod.subprocess, "run") as launch,
                        contextlib.redirect_stdout(output),
                        contextlib.redirect_stderr(error),
                    ):
                        status = self.mod.main(argv)
                    self.assertEqual(status, 0, error.getvalue())
                    launch.assert_not_called()
                    discover.assert_called_once_with(roots[expected].resolve())
                    command = json.loads(output.getvalue())
                    self.assertIn(f"--user-data-dir={roots[expected].resolve()}", command)

    def test_setup_and_list_keep_default_root_without_override(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Default Chrome"
            config_path = temp / "profiles.json"
            self.write_local_state(
                chrome_root,
                {"Profile 2": {"name": "Research", "user_name": "owner@example.test"}},
            )
            with (
                mock.patch.dict(self.mod.os.environ, {}, clear=True),
                mock.patch.object(self.mod, "DEFAULT_CHROME_ROOT", chrome_root),
                mock.patch.object(self.mod.subprocess, "run") as launch,
                contextlib.redirect_stdout(io.StringIO()),
            ):
                self.assertEqual(self.mod.main(["--list-profiles"]), 0)
                self.assertEqual(self.mod.main([
                    "--setup", "research", "--profile-directory", "Profile 2",
                    "--config", str(config_path),
                ]), 0)
            launch.assert_not_called()
            self.assertEqual(json.loads(config_path.read_text())["chromeRoot"], str(chrome_root))

    def test_multiple_aliases_share_one_config(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Chrome"
            config_path = temp / "profiles.json"
            self.write_local_state(
                chrome_root,
                {
                    "Profile 2": {
                        "name": "Epoch",
                        "user_name": "pablo@epoch.ai",
                    },
                    "Profile 4": {
                        "name": "Trajectory Labs",
                        "user_name": "pablo.stafforini@trajectorylabs.net",
                    },
                },
            )

            self.mod.write_alias_config(config_path, chrome_root, "epoch", "Profile 2")
            self.mod.write_alias_config(
                config_path, chrome_root, "trajectory", "Profile 4"
            )

            epoch = self.mod.load_alias_config(config_path, "epoch", chrome_root)
            trajectory = self.mod.load_alias_config(
                config_path, "trajectory", chrome_root
            )

        self.assertEqual(epoch.profile_directory, "Profile 2")
        self.assertEqual(trajectory.profile_directory, "Profile 4")

    def test_load_alias_fails_closed_when_alias_missing(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Chrome"
            config_path = temp / "profiles.json"
            self.write_local_state(
                chrome_root,
                {
                    "Profile 4": {
                        "name": "Trajectory Labs",
                        "user_name": "pablo.stafforini@trajectorylabs.net",
                    }
                },
            )
            self.mod.write_alias_config(
                config_path, chrome_root, "trajectory", "Profile 4"
            )

            with self.assertRaisesRegex(self.mod.ConfigError, "Alias 'epoch'"):
                self.mod.load_alias_config(config_path, "epoch", chrome_root)

    def test_load_alias_fails_closed_when_profile_no_longer_exists(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Chrome"
            config_path = temp / "profiles.json"
            self.write_local_state(
                chrome_root,
                {
                    "Profile 4": {
                        "name": "Trajectory Labs",
                        "user_name": "pablo.stafforini@trajectorylabs.net",
                    }
                },
            )
            self.mod.write_alias_config(
                config_path, chrome_root, "trajectory", "Profile 4"
            )
            self.write_local_state(
                chrome_root,
                {
                    "Default": {
                        "name": "Personal",
                        "user_name": "pablo.stafforini@gmail.com",
                    }
                },
            )

            with self.assertRaisesRegex(self.mod.ConfigError, "no longer exists"):
                self.mod.load_alias_config(config_path, "trajectory", chrome_root)

    def test_load_alias_fails_closed_when_account_changes(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Chrome"
            config_path = temp / "profiles.json"
            self.write_local_state(
                chrome_root,
                {
                    "Profile 4": {
                        "name": "Trajectory Labs",
                        "user_name": "pablo.stafforini@trajectorylabs.net",
                    }
                },
            )
            self.mod.write_alias_config(
                config_path, chrome_root, "trajectory", "Profile 4"
            )
            self.write_local_state(
                chrome_root,
                {
                    "Profile 4": {
                        "name": "Personal",
                        "user_name": "pablo.stafforini@gmail.com",
                    }
                },
            )

            with self.assertRaisesRegex(self.mod.ConfigError, "profile identity changed"):
                self.mod.load_alias_config(config_path, "trajectory", chrome_root)

    def test_cli_dry_run_opens_alias(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Chrome"
            config_path = temp / "profiles.json"
            self.write_local_state(
                chrome_root,
                {
                    "Profile 4": {
                        "name": "Trajectory Labs",
                        "user_name": "pablo.stafforini@trajectorylabs.net",
                    }
                },
            )
            self.mod.write_alias_config(
                config_path, chrome_root, "trajectory", "Profile 4"
            )

            result = subprocess.run(
                [
                    sys.executable,
                    str(SCRIPT),
                    "--config",
                    str(config_path),
                    "--chrome-root",
                    str(chrome_root),
                    "--dry-run",
                    "trajectory",
                    "http://127.0.0.1:8770/",
                ],
                text=True,
                capture_output=True,
            )

        self.assertEqual(result.returncode, 0, result.stderr)
        command = json.loads(result.stdout)
        self.assertEqual(
            command[:5],
            ["/usr/bin/open", "-n", "-b", "com.google.Chrome", "--args"],
        )
        self.assertIn("--profile-directory=Profile 4", command)
        self.assertIn(f"--user-data-dir={chrome_root.resolve()}", command)
        self.assertEqual(command[-1], "http://127.0.0.1:8770/")


if __name__ == "__main__":
    unittest.main()
