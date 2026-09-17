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

    def test_discovery_finds_brave_and_reports_actual_bundle_not_stale_receipt(self):
        brave = {"token": "brave-browser", "version": "1.95.102.0", "installed": "1.80.122.0",
                 "auto_updates": True, "bundle_short_version": "147.1.89.132", "bundle_version": "189.132"}
        def brew(command, **kwargs):
            self.assertEqual(kwargs["env_overrides"]["HOMEBREW_NO_AUTO_UPDATE"], "1")
            if command[1] == "outdated":
                # Homebrew's normal bundle comparison misses the Chromium
                # prefix; greedy discovery can still find the old receipt.
                casks = [{"name": "brave-browser", "current_version": brave["version"]}] \
                    if "--greedy-auto-updates" in command else []
                return subprocess.CompletedProcess(command, 0, json.dumps({"casks": casks}), "")
            self.assertEqual(command[1:], ["info", "--json=v2", "--cask", "--installed"])
            return subprocess.CompletedProcess(command, 0, json.dumps({"casks": [brave]}), "")
        with patch.object(self.mod, "run", side_effect=brew), patch.object(self.mod, "brew_update") as refresh:
            updates = self.mod.brew_outdated(refresh=False)
        refresh.assert_not_called()
        self.assertEqual(updates["brew_cask"], [{"name": "brave-browser", "available": "1.95.102.0",
                                               "installed": "147.1.89.132", "installed_receipt": "1.80.122.0"}])

    def test_bundle_discovery_detects_drift_even_when_receipt_is_current(self):
        item = {"token": "brave-browser", "version": "1.95.102.0", "installed": "1.95.102.0",
                "auto_updates": True, "bundle_short_version": "147.1.89.132", "bundle_version": "189.132"}
        update = self.mod.cask_updates([], [item])[0]
        self.assertEqual(update["installed"], "147.1.89.132")
        self.assertIn("verified reinstall is required", update["version_check_error"])

    def test_self_updated_or_newer_bundles_are_not_reinstalled_from_old_receipts(self):
        for name, available, actual in (("brave-browser", "1.95.102.0", "153.1.95.102"),
                                        ("brave-browser", "1.95.102.0", "1.95.102.0"),
                                        ("brave-browser", "1.95.102.0", "154.1.96.1"),
                                        ("google-chrome", "153.0.8010.48", "153.0.8010.48"),
                                        ("firefox", "156.0", "157.0")):
            with self.subTest(name=name, actual=actual):
                item = {"token": name, "version": available, "installed": "0.1",
                        "auto_updates": True, "bundle_short_version": actual}
                self.assertEqual(self.mod.cask_updates([{"name": name}], [item]), [])

    def test_bundle_comparison_checks_numeric_builds_and_limits_brave_normalization(self):
        cases = (("tool", "2.61-2057", "2.61", "2057", 0),
                 ("tool", "2026.9.2,2026.2995", "2026.9.2", "2026.2994", -1),
                 ("keyboard-maestro", "11.1.1,1111", "11.1.1", "11.1.1", 0),
                 ("plex-media-server", "1.43.4.10903,e5521bd8c", "1.43.4", "1.43.4.10903", 0),
                 ("plex-media-server", "1.43.4.10903,e5521bd8c", "1.43.4", "1.43.4.10904", 1),
                 ("descript", "114.0.4-release.20250509.32955", "114.0.4-release.20250509.32955", "20250509.32955", 0),
                 ("tool", "1.95.102.0", "147.1.89.132", "189.132", 1))
        for name, available, short, build, expected in cases:
            item = {"token": name, "version": available, "bundle_short_version": short, "bundle_version": build}
            self.assertEqual(self.mod.cask_bundle_comparison(item), (expected, None))

    def test_missing_or_incomparable_bundle_versions_are_explicitly_unchecked(self):
        for actual in (None, "preview-next"):
            item = {"token": "tool", "version": "2.0", "installed": "1.0", "auto_updates": True,
                    "bundle_short_version": actual}
            update = self.mod.cask_updates([], [item])[0]
            self.assertIn("version_check_error", update)
            state = self.mod.merge_scan({}, {"brew_cask": [update]}, self.now)
            self.assertEqual(state["brew_cask"]["tool"]["version_check_error"], update["version_check_error"])

    def test_delayed_defers_unchecked_bundle_even_when_artifact_is_age_qualified(self):
        with tempfile.TemporaryDirectory() as directory:
            base = pathlib.Path(directory)
            args = Namespace(state_file=base / "state.json", log_dir=base / "logs",
                             hold_file=base / "holds", excluded_casks_file=base / "excluded",
                             min_age_days=7, dry_run=False)
            observation = self.observation("tool", "2026-05-01T00:00:00+00:00")
            update = {"name": "tool", "installed": "unknown", "available": "1",
                      "version_check_error": "installed application bundle version is unavailable"}
            args.state_file.write_text(json.dumps({"brew_cask": {"tool": {
                **update, "first_seen": observation["first_seen"]}},
                "observations": {"brew_cask": {"tool": observation}}}))
            with patch.object(self.mod, "brew_outdated", return_value={"brew_cask": [update]}), \
                 patch.object(self.mod, "observe_artifacts", return_value={"brew_cask": {"tool": observation}}), \
                 patch.object(self.mod, "upgrade_brew") as upgrade, patch.object(sys, "stderr"):
                self.mod.command_delayed(args, now=self.now)
            upgrade.assert_not_called()
            log = "".join(path.read_text() for path in args.log_dir.iterdir())
            self.assertIn("delayed deferred brew_cask/tool: installed application bundle version is unavailable", log)

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

    def observation(self, name, first_seen, pending_deps=(), artifact="hash"):
        return {"name": name, "available": "1", "artifact": artifact,
                "first_seen": first_seen, "pending_deps": list(pending_deps)}

    def test_dependency_blockers_follow_pending_edges_transitively(self):
        aged, fresh = "2026-05-01T00:00:00+00:00", self.now.isoformat()
        observations = {"brew_formula": {
            "top": self.observation("top", aged, ["brew_formula/mid", "brew_formula/held"]),
            "mid": self.observation("mid", aged, ["brew_formula/leaf", "brew_formula/unobserved"]),
            "leaf": self.observation("leaf", fresh),
            "held": self.observation("held", aged),
            "current": self.observation("current", aged),
        }, "brew_cask": {}}
        allowed = self.mod.age_qualified_artifacts(observations, 7, self.now, {"held"}, set())
        self.assertEqual(self.mod.dependency_blockers("brew_formula", "top", observations, allowed),
                         ["brew_formula/held", "brew_formula/leaf", "brew_formula/unobserved"])
        self.assertEqual(self.mod.dependency_blockers("brew_formula", "current", observations, allowed), [])

    def test_delayed_defers_candidate_until_pending_dependency_qualifies(self):
        # Regression: imagemagick and graphviz were selected while their
        # dependency aom was still waiting, so the in-Homebrew guard raised a
        # hard error and the job exited 1 every day of the waiting period.
        aged = "2026-05-01T00:00:00+00:00"
        for dep_first_seen, expect_upgrade in ((self.now.isoformat(), False), (aged, True)):
            with self.subTest(dependency_qualified=expect_upgrade), tempfile.TemporaryDirectory() as directory:
                base = pathlib.Path(directory)
                args = Namespace(state_file=base / "state.json", log_dir=base / "logs",
                                 hold_file=base / "holds", excluded_casks_file=base / "excluded",
                                 min_age_days=7, dry_run=False)
                observations = {"brew_formula": {
                    "imagemagick": self.observation("imagemagick", aged, ["brew_formula/aom"]),
                    "aom": self.observation("aom", dep_first_seen)}}
                args.state_file.write_text(json.dumps({
                    "brew_formula": {"imagemagick": {"installed": "0", "available": "1", "first_seen": aged},
                                     "aom": {"installed": "0", "available": "1", "first_seen": dep_first_seen}},
                    "observations": observations}))
                current = {"brew_formula": [{"name": "imagemagick", "installed": "0", "available": "1"},
                                            {"name": "aom", "installed": "0", "available": "1"}]}
                observed = {kind: {name: {k: v for k, v in entry.items() if k != "first_seen"}
                                   for name, entry in entries.items()} for kind, entries in observations.items()}
                with patch.object(self.mod, "brew_outdated", return_value=current), \
                     patch.object(self.mod, "observe_artifacts", return_value=observed), \
                     patch.object(self.mod, "upgrade_brew") as upgrade, \
                     patch.object(self.mod, "command_scan"), \
                     patch.object(sys, "stderr"):
                    self.mod.command_delayed(args, now=self.now)
                log = "".join(path.read_text() for path in args.log_dir.iterdir())
                if expect_upgrade:
                    self.assertEqual(upgrade.call_args[0][0], {"brew_formula": ["aom", "imagemagick"]})
                    self.assertNotIn("deferred", log)
                else:
                    upgrade.assert_not_called()
                    self.assertIn("delayed deferred brew_formula/imagemagick: dependencies brew_formula/aom "
                                  "are unchecked or have not completed the waiting period", log)
                    self.assertNotIn("error", log)

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
  # PENDING_DEP models a runtime dependency; DEP_INSTALLED marks it current.
  # HIDDEN_DEP models an outdated dependency ("deep") reachable only through a
  # dependency that is already current ("mid").
  def deps
    dep = Struct.new(:to_formula) { def test? = false; def optional? = false; def build? = false }
    if ENV["HIDDEN_DEP"] == "1"
      return [dep.new(Item.new("mid"))] if @name == "tool"
      return [dep.new(Item.new("deep", "3.0"))] if @name == "mid"
    end
    return [] unless ENV["PENDING_DEP"] == "1" && @name == "tool"
    [dep.new(Item.new("dep", "2.0")), dep.new(Item.new("build-only")).tap { |d| d.define_singleton_method(:build?) { true } }]
  end
  # Mirrors Dependency.expand: a pruned dependency is dropped with its subtree;
  # every other dependency is kept and its own dependencies expanded.
  def recursive_dependencies(&block)
    deps.flat_map do |dep|
      next [] if block.call(self, dep) == Dependable::PRUNE
      dep.to_formula.recursive_dependencies(&block) + [dep]
    end.uniq { |dep| dep.to_formula.full_name }
  end
  def latest_version_installed? = !%w[dep deep].include?(@name) || ENV["DEP_INSTALLED"] == "1"
  def sha256 = @checksum
  def url = "https://example.invalid/fixture"
end
module Dependable
  PRUNE = :prune
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

    def run_observe(self, whole=False, **scenario):
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
            formulae = json.loads(result.stdout)["brew_formula"]
            return formulae if whole else formulae["tool"]

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

    def test_observe_records_dependencies_homebrew_would_install(self):
        formulae = self.run_observe(whole=True, PENDING_DEP="1")
        self.assertEqual(formulae["tool"]["pending_deps"], ["brew_formula/dep"])
        self.assertRegex(formulae["dep"]["artifact"] or "", r"^[0-9a-f]{64}$")
        self.assertNotIn("build-only", formulae)
        self.assertEqual(self.run_observe(PENDING_DEP="1", DEP_INSTALLED="1")["pending_deps"], [])

    def test_observe_records_outdated_dependencies_below_current_ones(self):
        # Regression: qt3d was selected while xz, reachable only through the
        # already-current zstd, was still waiting. Homebrew expands through a
        # current dependency, so the guard refused xz and the job exited 1.
        formulae = self.run_observe(whole=True, HIDDEN_DEP="1")
        self.assertEqual(formulae["tool"]["pending_deps"], ["brew_formula/deep"])
        self.assertEqual(formulae["mid"]["pending_deps"], ["brew_formula/deep"])
        self.assertRegex(formulae["deep"]["artifact"] or "", r"^[0-9a-f]{64}$")

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
