import importlib.machinery
import importlib.util
import json
import pathlib
import sys
import tempfile
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "trajectory-open"


def load_module():
    loader = importlib.machinery.SourceFileLoader("trajectory_open", str(SCRIPT))
    spec = importlib.util.spec_from_loader("trajectory_open", loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules["trajectory_open"] = module
    spec.loader.exec_module(module)
    return module


class TrajectoryOpenTest(unittest.TestCase):
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

    def test_setup_persists_profile_directory_and_open_command_uses_it(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Chrome"
            config_path = temp / "trajectory-open.json"
            self.write_local_state(
                chrome_root,
                {
                    "Profile 4": {
                        "name": "Trajectory Labs",
                        "user_name": "pablo.stafforini@trajectorylabs.net",
                    }
                },
            )

            self.mod.write_config(config_path, chrome_root, "Profile 4")
            config = self.mod.load_config(config_path, chrome_root)
            command = self.mod.build_chrome_command(
                config,
                ["http://127.0.0.1:8770/"],
                chrome_path=pathlib.Path("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"),
            )

        self.assertEqual(config.profile_directory, "Profile 4")
        self.assertIn("--profile-directory=Profile 4", command)
        self.assertEqual(command[-1], "http://127.0.0.1:8770/")

    def test_load_config_fails_closed_when_missing(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)

            with self.assertRaisesRegex(self.mod.ConfigError, "not configured"):
                self.mod.load_config(temp / "missing.json", temp / "Chrome")

    def test_load_config_fails_closed_when_profile_no_longer_exists(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Chrome"
            config_path = temp / "trajectory-open.json"
            self.write_local_state(
                chrome_root,
                {
                    "Profile 4": {
                        "name": "Trajectory Labs",
                        "user_name": "pablo.stafforini@trajectorylabs.net",
                    }
                },
            )
            self.mod.write_config(config_path, chrome_root, "Profile 4")
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
                self.mod.load_config(config_path, chrome_root)

    def test_write_config_rejects_unknown_profile_directory(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            chrome_root = temp / "Chrome"
            self.write_local_state(
                chrome_root,
                {
                    "Default": {
                        "name": "Personal",
                        "user_name": "pablo.stafforini@gmail.com",
                    }
                },
            )

            with self.assertRaisesRegex(self.mod.ConfigError, "Unknown Chrome profile"):
                self.mod.write_config(temp / "trajectory-open.json", chrome_root, "Profile 4")


if __name__ == "__main__":
    unittest.main()
