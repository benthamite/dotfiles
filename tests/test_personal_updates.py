import importlib.util
import importlib.machinery
import pathlib
import tempfile
import unittest
from argparse import Namespace
from datetime import datetime, timezone


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "personal-updates"
POLICY_DIR = ROOT / "macos" / "personal-updates"


def load_module():
    loader = importlib.machinery.SourceFileLoader("personal_updates", str(SCRIPT))
    spec = importlib.util.spec_from_loader("personal_updates", loader)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class PersonalUpdatesStateTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()
        self.now = datetime(2026, 6, 7, 12, 0, tzinfo=timezone.utc)

    def test_scan_records_new_updates_with_first_seen_timestamp(self):
        state = {}
        updates = {
            "brew_formula": [
                {
                    "name": "ripgrep",
                    "installed": "14.1.0",
                    "available": "14.2.0",
                }
            ]
        }

        result = self.mod.merge_scan(state, updates, self.now)

        self.assertEqual(
            result,
            {
                "brew_formula": {
                    "ripgrep": {
                        "installed": "14.1.0",
                        "available": "14.2.0",
                        "first_seen": "2026-06-07T12:00:00+00:00",
                    }
                }
            },
        )

    def test_scan_preserves_first_seen_for_same_available_version(self):
        state = {
            "brew_formula": {
                "ripgrep": {
                    "installed": "14.1.0",
                    "available": "14.2.0",
                    "first_seen": "2026-06-01T12:00:00+00:00",
                }
            }
        }
        updates = {
            "brew_formula": [
                {
                    "name": "ripgrep",
                    "installed": "14.1.0",
                    "available": "14.2.0",
                }
            ]
        }

        result = self.mod.merge_scan(state, updates, self.now)

        self.assertEqual(
            result["brew_formula"]["ripgrep"]["first_seen"],
            "2026-06-01T12:00:00+00:00",
        )

    def test_scan_resets_first_seen_when_available_version_changes(self):
        state = {
            "brew_formula": {
                "ripgrep": {
                    "installed": "14.1.0",
                    "available": "14.2.0",
                    "first_seen": "2026-06-01T12:00:00+00:00",
                }
            }
        }
        updates = {
            "brew_formula": [
                {
                    "name": "ripgrep",
                    "installed": "14.1.0",
                    "available": "14.3.0",
                }
            ]
        }

        result = self.mod.merge_scan(state, updates, self.now)

        self.assertEqual(
            result["brew_formula"]["ripgrep"]["first_seen"],
            "2026-06-07T12:00:00+00:00",
        )

    def test_scan_drops_updates_that_are_no_longer_outdated(self):
        state = {
            "brew_formula": {
                "ripgrep": {
                    "installed": "14.1.0",
                    "available": "14.2.0",
                    "first_seen": "2026-06-01T12:00:00+00:00",
                }
            }
        }

        result = self.mod.merge_scan(state, {"brew_formula": []}, self.now)

        self.assertEqual(result, {"brew_formula": {}})

    def test_eligible_updates_respects_minimum_age_holds_and_excluded_casks(self):
        state = {
            "brew_formula": {
                "old": {
                    "installed": "1.0.0",
                    "available": "1.1.0",
                    "first_seen": "2026-05-30T12:00:00+00:00",
                },
                "new": {
                    "installed": "1.0.0",
                    "available": "1.1.0",
                    "first_seen": "2026-06-05T12:00:00+00:00",
                },
            },
            "brew_cask": {
                "aged-cask": {
                    "installed": "1.0.0",
                    "available": "1.1.0",
                    "first_seen": "2026-05-31T12:00:00+00:00",
                }
            },
        }

        result = self.mod.eligible_updates(
            state,
            min_age_days=7,
            now=self.now,
            holds={"brew_cask/aged-cask"},
            excluded_casks={"excluded-cask"},
        )

        self.assertEqual(
            result,
            {
                "brew_formula": ["old"],
            },
        )

    def test_eligible_updates_skips_excluded_casks(self):
        state = {
            "brew_cask": {
                "excluded-cask": {
                    "installed": "1.0.0",
                    "available": "1.1.0",
                    "first_seen": "2026-05-31T12:00:00+00:00",
                }
            }
        }

        result = self.mod.eligible_updates(
            state,
            min_age_days=7,
            now=self.now,
            excluded_casks={"excluded-cask"},
        )

        self.assertEqual(result, {})

    def test_load_holds_ignores_comments_and_blank_lines(self):
        text = """
        # held package
        visual-studio-code

        brew_formula/python
        """

        result = self.mod.parse_holds(text)

        self.assertEqual(result, {"visual-studio-code", "brew_formula/python"})

    def test_default_fast_brew_packages_reflect_existing_launchd_jobs(self):
        self.assertEqual(
            self.mod.DEFAULT_FAST_BREW_PACKAGES,
            ("claude-code@latest", "codex"),
        )

    def test_brew_upgrade_commands_targets_each_fast_lane_package(self):
        result = self.mod.brew_upgrade_commands(["claude-code@latest", "codex"])

        self.assertEqual(
            result,
            [
                ["brew", "upgrade", "claude-code@latest"],
                ["brew", "upgrade", "codex"],
            ],
        )

    def test_load_fast_brew_packages_extends_defaults(self):
        class FakePath:
            def exists(self):
                return True

            def read_text(self):
                return "extra-tool\ncodex\n"

        result = self.mod.load_fast_brew_packages(FakePath())

        self.assertEqual(result, ["claude-code@latest", "codex", "extra-tool"])

    def test_brew_update_uses_update_reset_before_update(self):
        commands = []

        self.mod.brew_update(run_command=lambda command, **_kwargs: commands.append(command))

        self.assertEqual(commands, [["brew", "update-reset", "-q"], ["brew", "update"]])

    def test_brew_upgrade_commands_use_greedy_for_casks(self):
        result = self.mod.brew_upgrade_commands_for_eligible(
            {"brew_formula": ["ripgrep"], "brew_cask": ["visual-studio-code"]}
        )

        self.assertEqual(
            result,
            [
                ["brew", "upgrade", "ripgrep"],
                ["brew", "upgrade", "--cask", "--greedy", "visual-studio-code"],
            ],
        )

    def test_run_sets_homebrew_no_install_cleanup(self):
        result = self.mod.run(
            [
                "python3",
                "-c",
                "import os; print(os.environ.get('HOMEBREW_NO_INSTALL_CLEANUP'))",
            ]
        )

        self.assertEqual(result.stdout.strip(), "1")

    def test_delayed_command_skips_upgrade_when_nothing_is_eligible(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = pathlib.Path(temp_dir)
            state_file = temp_path / "state.json"
            state_file.write_text(
                """
                {
                  "brew_formula": {
                    "new": {
                      "installed": "1.0.0",
                      "available": "1.1.0",
                      "first_seen": "2026-06-05T12:00:00+00:00"
                    }
                  }
                }
                """
            )
            args = Namespace(
                state_file=state_file,
                log_dir=temp_path / "logs",
                hold_file=temp_path / "hold.txt",
                excluded_casks_file=temp_path / "excluded-casks.txt",
                min_age_days=7,
                dry_run=False,
            )
            old_upgrade_brew = self.mod.upgrade_brew
            old_command_scan = self.mod.command_scan
            self.mod.upgrade_brew = lambda _eligible: self.fail("upgrade_brew called")
            self.mod.command_scan = lambda _args: self.fail("command_scan called")
            try:
                self.mod.command_delayed(args, now=self.now)
            finally:
                self.mod.upgrade_brew = old_upgrade_brew
                self.mod.command_scan = old_command_scan

    def test_upgrade_brew_runs_doctor_without_failing_job(self):
        calls = []
        old_run = self.mod.run
        self.mod.run = lambda command, **kwargs: calls.append((command, kwargs))
        try:
            self.mod.upgrade_brew({"brew_formula": ["ripgrep"]})
        finally:
            self.mod.run = old_run

        self.assertEqual(
            calls,
            [
                (["brew", "upgrade", "ripgrep"], {}),
                (["brew", "doctor"], {"check": False}),
            ],
        )

    def test_default_policy_files_live_in_macos_directory(self):
        self.assertEqual(self.mod.DEFAULT_POLICY_DIR, POLICY_DIR)
        self.assertEqual(self.mod.DEFAULT_HOLD_FILE, POLICY_DIR / "hold.txt")
        self.assertEqual(
            self.mod.DEFAULT_EXCLUDED_CASKS_FILE,
            POLICY_DIR / "excluded-casks.txt",
        )
        self.assertEqual(
            self.mod.DEFAULT_FAST_BREW_FILE,
            POLICY_DIR / "fast-brew.txt",
        )


if __name__ == "__main__":
    unittest.main()
