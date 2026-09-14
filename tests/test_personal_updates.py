import importlib.util
import importlib.machinery
import pathlib
import json
import tempfile
import subprocess
import sys
import hashlib
import unittest
from argparse import Namespace
from datetime import datetime, timezone
from unittest.mock import patch


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

    def test_delayed_rechecks_changed_version_before_upgrade(self):
        with tempfile.TemporaryDirectory() as directory:
            base = pathlib.Path(directory)
            args = Namespace(state_file=base / "state.json", log_dir=base / "logs",
                             hold_file=base / "holds", excluded_casks_file=base / "excluded",
                             min_age_days=7, dry_run=False)
            args.state_file.write_text(json.dumps({"brew_formula": {"old": {
                "installed": "1.0", "available": "1.1",
                "first_seen": "2026-05-01T00:00:00+00:00"}}}))
            newer = {"brew_formula": [{"name": "old", "installed": "1.0", "available": "2.0"}]}
            with patch.object(self.mod, "brew_outdated", return_value=newer), \
                 patch.object(self.mod, "observe_artifacts", return_value={}), \
                 patch.object(self.mod, "upgrade_brew") as upgrade:
                self.mod.command_delayed(args, now=self.now)
            upgrade.assert_not_called()

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
                ["brew", "upgrade", "--formula", "ripgrep"],
                ["brew", "upgrade", "--cask", "--greedy", "--require-sha", "visual-studio-code"],
            ],
        )

    def test_run_sets_homebrew_no_install_cleanup(self):
        result = self.mod.run(
            [
                sys.executable,
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
                with patch.object(self.mod, "brew_outdated", return_value={}), \
                     patch.object(self.mod, "observe_artifacts", return_value={}):
                    self.mod.command_delayed(args, now=self.now)
            finally:
                self.mod.upgrade_brew = old_upgrade_brew
                self.mod.command_scan = old_command_scan

    def test_upgrade_brew_runs_doctor_without_failing_job(self):
        calls = []
        old_run = self.mod.run
        self.mod.run = lambda command, **kwargs: calls.append((command, kwargs))
        try:
            with patch.object(self.mod, "brew_helper") as helper:
                self.mod.upgrade_brew({"brew_formula": ["ripgrep"]}, {"brew_formula": {}})
            helper.assert_called_once_with("upgrade", {"brew_formula": {}}, ["--formula", "ripgrep"])
        finally:
            self.mod.run = old_run

        self.assertEqual(
            calls,
            [
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

    def test_artifact_replacement_restarts_wait_even_at_same_version(self):
        old = {"brew_formula": {"tool": {"name": "tool", "available": "1.0",
                "artifact": "old", "first_seen": "2026-05-01T00:00:00+00:00"}}}
        observed = {"brew_formula": {"tool": {"name": "tool", "available": "1.0", "artifact": "new"}}}
        merged = self.mod.merge_observations(old, observed, self.now)
        self.assertEqual(merged["brew_formula"]["tool"]["first_seen"], self.now.isoformat())
        self.assertEqual(self.mod.age_qualified_artifacts(merged, 7, self.now, set(), set()),
                         {"brew_formula": {}, "brew_cask": {}})

    def test_delayed_installs_aged_unchanged_artifact_through_guard(self):
        with tempfile.TemporaryDirectory() as directory:
            base = pathlib.Path(directory)
            args = Namespace(state_file=base / "state.json", log_dir=base / "logs",
                             hold_file=base / "holds", excluded_casks_file=base / "excluded",
                             min_age_days=7, dry_run=False)
            observation = {"name": "tool", "available": "1.1", "artifact": "checksum-id",
                           "first_seen": "2026-05-01T00:00:00+00:00"}
            args.state_file.write_text(json.dumps({"brew_formula": {"tool": {
                "installed": "1.0", "available": "1.1", "first_seen": observation["first_seen"]}},
                "observations": {"brew_formula": {"tool": observation}}}))
            current = {"brew_formula": [{"name": "tool", "installed": "1.0", "available": "1.1"}]}
            with patch.object(self.mod, "brew_outdated", return_value=current) as outdated, \
                 patch.object(self.mod, "observe_artifacts", return_value={"brew_formula": {"tool": observation}}), \
                 patch.object(self.mod, "upgrade_brew") as upgrade, \
                 patch.object(self.mod, "command_scan"):
                self.mod.command_delayed(args, now=self.now)
            outdated.assert_called_once_with(refresh=False)
            upgrade.assert_called_once_with({"brew_formula": ["tool"]},
                                            {"brew_formula": {"tool": observation}, "brew_cask": {}})

    def test_legacy_state_does_not_grandfather_unobserved_artifacts(self):
        observed = {"brew_formula": {"tool": {"name": "tool", "available": "1.0", "artifact": "hash"}}}
        merged = self.mod.merge_observations({}, observed, self.now)
        self.assertEqual(self.mod.age_qualified_artifacts(merged, 7, self.now, set(), set())["brew_formula"], {})

    def test_implicit_dependency_holds_and_cask_exclusions_are_enforced(self):
        observations = {kind: {name: {"name": name, "available": "1", "artifact": "hash",
                        "first_seen": "2026-05-01T00:00:00+00:00"}}
                        for kind, name in [("brew_formula", "tap/tools/held"), ("brew_cask", "homebrew/cask/excluded")]}
        self.assertEqual(self.mod.age_qualified_artifacts(observations, 7, self.now, {"held"}, {"excluded"}),
                         {"brew_formula": {}, "brew_cask": {}})

    def test_helper_disables_auto_update_and_cleans_private_request(self):
        recorded = []
        def fake_run(command, **kwargs):
            request = pathlib.Path(command[5])
            self.assertEqual(request.stat().st_mode & 0o777, 0o600)
            self.assertEqual(json.loads(request.read_text()), {"brew_formula": {}})
            recorded.append(request)
            self.assertEqual(kwargs["env_overrides"]["HOMEBREW_NO_AUTO_UPDATE"], "1")
            self.assertEqual(kwargs["env_overrides"]["HOMEBREW_NO_ASK"], "1")
            return subprocess.CompletedProcess(command, 0, "{}", "")
        with patch.object(self.mod, "run", side_effect=fake_run):
            self.mod.brew_helper("upgrade", {"brew_formula": {}}, ["--formula", "tool"])
        self.assertFalse(recorded[0].exists())


class PersonalUpdatesBrewBoundaryTest(unittest.TestCase):
    """Run the real Ruby adapter against disposable installer implementations."""

    RUBY = "/opt/homebrew/Library/Homebrew/vendor/portable-ruby/current/bin/ruby"
    STUB = r'''
require "json"
class Item
  attr_accessor :version
  def initialize(name, version = "1.1", checksum = "a" * 64)
    @name, @version, @checksum = name, version, checksum
  end
  def full_name = @name
  def pkg_version = @version
  # VCS_SOURCE models a git-tag or svn-revision stable source: no download checksum.
  def stable = Struct.new(:checksum).new(ENV["VCS_SOURCE"] == "1" ? nil : @checksum)
  def head? = false
  def urls_hash = {"stable" => {"checksum" => stable.checksum}}
  def bottle_hash = {"files" => {"fixture" => {"sha256" => "b" * 64}}}
  def bottle_specification
    host = ENV["NO_BOTTLE"] == "1" ? nil : Struct.new(:checksum).new("b" * 64)
    collector = Object.new
    collector.define_singleton_method(:specification_for) { |_tag| host }
    Struct.new(:collector).new(collector)
  end
  def deps = []
  def sha256 = @checksum
  def url = "https://example.invalid/fixture"
end
module Utils
  module Bottles
    def self.tag = "fixture"
  end
end
module Formulary
  def self.factory(name) = Item.new(name)
end
class FormulaInstaller
  attr_reader :formula
  def initialize(formula, **_kwargs) = @formula = formula
  def pour_bottle? = ENV["SOURCE_BUILD"] != "1"
  def install
    if ENV["NEW_DEPENDENCY"] == "1"
      FormulaInstaller.new(Item.new("new-dependency", "9.0")).install
    end
    File.write(ENV.fetch("INSTALLED_MARKER"), formula.pkg_version)
  end
  def build = File.write(ENV.fetch("INSTALLED_MARKER"), "source")
end
module Cask
  class Installer
    attr_reader :cask
    def initialize(cask) = @cask = cask
    def fetch; end
    def install_artifacts(**_kwargs)
      raise "synthetic installation failure" if ENV["FAIL_NEW_CASK"] == "1" && cask.version == "1.1"
      File.write(ENV.fetch("INSTALLED_MARKER"), cask.version)
    end
    def revert_upgrade(**_kwargs)
      if ENV["UNRELATED_ROLLBACK"] == "1"
        Installer.new(Item.new("tool", "0.9")).install_artifacts
      elsif ENV["REPLACED_PREDECESSOR"] == "1"
        Installer.new(Item.new("tool", "1.0")).install_artifacts
      else
        install_artifacts
      end
    end
  end
  class Upgrade
    def self.upgrade_cask(old_cask, new_cask, **_kwargs)
      File.write(ENV.fetch("UNINSTALLED_MARKER"), old_cask.version)
      begin
        Installer.new(new_cask).install_artifacts
      rescue
        Installer.new(old_cask).revert_upgrade(predecessor: new_cask)
        raise
      end
    end
  end
end
module Homebrew
  def self.failed? = false
  module Cmd
    class UpgradeCmd
      def initialize(args) = @args = args
      def run
        item = Item.new("tool", ENV.fetch("CANDIDATE_VERSION", "1.1"), ENV.fetch("CHECKSUM", "a" * 64))
        if @args.include?("--cask")
          Cask::Upgrade.upgrade_cask(Item.new("tool", "1.0"), item)
        else
          installer = FormulaInstaller.new(item)
          item.version = ENV["AFTER_SELECTION_VERSION"] if ENV["AFTER_SELECTION_VERSION"]
          ENV["BUILD_FALLBACK"] == "1" ? installer.build : installer.install
        end
      end
    end
  end
end
'''

    def run_boundary(self, kind="brew_formula", **scenario):
        if not pathlib.Path(self.RUBY).exists():
            self.skipTest("Homebrew's Ruby runtime is not available")
        with tempfile.TemporaryDirectory() as directory:
            base = pathlib.Path(directory)
            (base / "cmd").mkdir()
            (base / "cask").mkdir()
            (base / "cmd" / "upgrade.rb").write_text(self.STUB)
            (base / "formulary.rb").write_text("")
            (base / "cask" / "cask_loader.rb").write_text("")
            source_checksum = None if scenario.get("VCS_SOURCE") == "1" else "a" * 64
            artifact = ["1.1", {"stable": {"checksum": source_checksum}},
                        {"files": {"fixture": {"sha256": "b" * 64}}}] if kind == "brew_formula" else \
                       ["1.1", "a" * 64, "https://example.invalid/fixture"]
            digest = hashlib.sha256(json.dumps(artifact, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
            policy = base / "policy.json"
            policy.write_text(json.dumps({kind: {"tool": {"available": "1.1", "artifact": digest}}}))
            installed, uninstalled = base / "installed", base / "uninstalled"
            env = {"PATH": "/usr/bin:/bin", "INSTALLED_MARKER": str(installed),
                   "UNINSTALLED_MARKER": str(uninstalled), **scenario}
            result = subprocess.run([self.RUBY, "-I", str(base), str(ROOT / "bin" / "personal-updates-brew.rb"),
                                     "upgrade", str(policy), "--formula" if kind == "brew_formula" else "--cask", "tool"],
                                    env=env, capture_output=True, text=True, timeout=20)
            return result, installed.read_text() if installed.exists() else None, uninstalled.exists()

    def run_observe(self, **scenario):
        if not pathlib.Path(self.RUBY).exists():
            self.skipTest("Homebrew's Ruby runtime is not available")
        with tempfile.TemporaryDirectory() as directory:
            base = pathlib.Path(directory)
            (base / "cmd").mkdir()
            (base / "cask").mkdir()
            (base / "cmd" / "upgrade.rb").write_text(self.STUB)
            (base / "formulary.rb").write_text("")
            (base / "cask" / "cask_loader.rb").write_text("")
            request = base / "request.json"
            request.write_text(json.dumps({"brew_formula": [{"name": "tool"}], "brew_cask": []}))
            result = subprocess.run([self.RUBY, "-I", str(base), str(ROOT / "bin" / "personal-updates-brew.rb"),
                                     "observe", str(request)], env={"PATH": "/usr/bin:/bin", **scenario},
                                    capture_output=True, text=True, timeout=20)
            self.assertEqual(result.returncode, 0, result.stderr)
            return json.loads(result.stdout)["brew_formula"]["tool"]

    def test_installs_exact_qualified_formula(self):
        result, installed, _ = self.run_boundary()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(installed, "1.1")

    def test_installs_exact_qualified_cask(self):
        result, installed, _ = self.run_boundary("brew_cask")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(installed, "1.1")

    def test_formula_change_after_python_check_is_rejected(self):
        result, installed, _ = self.run_boundary(CANDIDATE_VERSION="2.0")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("not age-qualified", result.stderr)
        self.assertIsNone(installed)

    def test_formula_change_after_installer_selection_is_rejected(self):
        result, installed, _ = self.run_boundary(AFTER_SELECTION_VERSION="2.0")
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(installed)

    def test_changed_cask_is_rejected_before_old_cask_uninstall(self):
        result, installed, uninstalled = self.run_boundary("brew_cask", CANDIDATE_VERSION="2.0")
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(installed)
        self.assertFalse(uninstalled)

    def test_same_version_changed_artifact_is_rejected(self):
        result, installed, _ = self.run_boundary(CHECKSUM="c" * 64)
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(installed)

    def test_unobserved_dependency_is_rejected(self):
        result, installed, _ = self.run_boundary(NEW_DEPENDENCY="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("new-dependency", result.stderr)
        self.assertIsNone(installed)

    def test_source_build_and_bottle_fallback_are_rejected(self):
        for scenario in ({"SOURCE_BUILD": "1"}, {"BUILD_FALLBACK": "1"}):
            with self.subTest(scenario=scenario):
                result, installed, _ = self.run_boundary(**scenario)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("Delayed update refused", result.stderr)
                self.assertIsNone(installed)

    def test_vcs_sourced_bottled_formula_is_identified_by_host_bottle(self):
        # Regression: aom, netpbm, bun, flyctl and glab pin a git tag or svn
        # revision, so their stable source has no checksum. They were reported
        # as unchecked forever, and every dependent upgrade was refused daily.
        observed = self.run_observe(VCS_SOURCE="1")
        self.assertRegex(observed["artifact"] or "", r"^[0-9a-f]{64}$")
        result, installed, _ = self.run_boundary(VCS_SOURCE="1")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(installed, "1.1")

    def test_vcs_sourced_formula_without_host_bottle_stays_unchecked(self):
        observed = self.run_observe(VCS_SOURCE="1", NO_BOTTLE="1")
        self.assertIsNone(observed["artifact"])
        result, installed, _ = self.run_boundary(VCS_SOURCE="1", NO_BOTTLE="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("not age-qualified", result.stderr)
        self.assertIsNone(installed)

    def test_checksum_free_cask_is_rejected(self):
        result, installed, uninstalled = self.run_boundary("brew_cask", CHECKSUM="no_check")
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(installed)
        self.assertFalse(uninstalled)

    def test_failed_cask_upgrade_restores_exact_recorded_predecessor(self):
        result, installed, uninstalled = self.run_boundary("brew_cask", FAIL_NEW_CASK="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("synthetic installation failure", result.stderr)
        self.assertTrue(uninstalled)
        self.assertEqual(installed, "1.0")

    def test_rollback_cannot_install_unrelated_old_version(self):
        result, installed, uninstalled = self.run_boundary("brew_cask", FAIL_NEW_CASK="1", UNRELATED_ROLLBACK="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("not age-qualified", result.stderr)
        self.assertTrue(uninstalled)
        self.assertIsNone(installed)

    def test_rollback_requires_recorded_instance_not_matching_version(self):
        result, installed, _ = self.run_boundary("brew_cask", FAIL_NEW_CASK="1", REPLACED_PREDECESSOR="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("not age-qualified", result.stderr)
        self.assertIsNone(installed)


if __name__ == "__main__":
    unittest.main()
