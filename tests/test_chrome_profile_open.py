import importlib.machinery
import importlib.util
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
        self.assertEqual(command[-1], "http://127.0.0.1:8770/")

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
        self.assertEqual(command[-1], "http://127.0.0.1:8770/")


if __name__ == "__main__":
    unittest.main()
