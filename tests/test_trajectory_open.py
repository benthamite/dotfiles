import json
import pathlib
import subprocess
import tempfile
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "trajectory-open"


class TrajectoryOpenWrapperTest(unittest.TestCase):
    def write_local_state(self, chrome_root, profiles):
        chrome_root.mkdir(parents=True, exist_ok=True)
        payload = {"profile": {"info_cache": profiles}}
        (chrome_root / "Local State").write_text(json.dumps(payload))

    def test_wrapper_dry_run_uses_trajectory_alias(self):
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
            config_path.write_text(
                json.dumps(
                    {
                        "version": 1,
                        "chromeRoot": str(chrome_root),
                        "aliases": {
                            "trajectory": {
                                "profileDirectory": "Profile 4",
                                "profileName": "Trajectory Labs",
                                "userName": "pablo.stafforini@trajectorylabs.net",
                            }
                        },
                    }
                )
            )

            result = subprocess.run(
                [
                    str(SCRIPT),
                    "--config",
                    str(config_path),
                    "--chrome-root",
                    str(chrome_root),
                    "--dry-run",
                    "http://127.0.0.1:8770/",
                ],
                text=True,
                capture_output=True,
            )

        self.assertEqual(result.returncode, 0, result.stderr)
        command = json.loads(result.stdout)
        self.assertIn("--profile-directory=Profile 4", command)
        self.assertEqual(command[-1], "http://127.0.0.1:8770/")

    def test_wrapper_setup_configures_trajectory_alias(self):
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

            result = subprocess.run(
                [
                    str(SCRIPT),
                    "--setup",
                    "--profile-directory",
                    "Profile 4",
                    "--config",
                    str(config_path),
                    "--chrome-root",
                    str(chrome_root),
                ],
                text=True,
                capture_output=True,
            )

            payload = json.loads(config_path.read_text())

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            payload["aliases"]["trajectory"]["profileDirectory"], "Profile 4"
        )


if __name__ == "__main__":
    unittest.main()
