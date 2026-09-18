import importlib.machinery
import importlib.util
import io
import pathlib
import shutil
import tempfile
import unittest
from argparse import Namespace
from contextlib import redirect_stdout
from datetime import datetime, timedelta, timezone
from unittest.mock import patch


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "elpaca-cooldown"

LOCKFILE = """((mine :source "elpaca-menu-lock-file" :recipe
       (:host github :repo "benthamite/mine" :package "mine" :ref
              "1111111111111111111111111111111111111111"))
 (theirs :source "elpaca-menu-lock-file" :recipe
         (:package "theirs" :fetcher github :repo "someone/theirs" :branch
                   "main" :ref "2222222222222222222222222222222222222222"))
 (mirrored :source "elpaca-menu-lock-file" :recipe
           (:package "mirrored" :host gnu :repo
                     ("https://github.com/emacsmirror/gnu_elpa" . "mirrored")
                     :branch "externals/mirrored" :ref
                     "3333333333333333333333333333333333333333"))
 (forked :source "elpaca-menu-lock-file" :recipe
         (:package "forked" :host gnu :repo
                   ("https://github.com/upstream/forked" . "forked") :remotes
                   (("fork" :host github :repo "benthamite/forked" :branch "fix")
                    "origin")
                   :ref "4444444444444444444444444444444444444444"))
 (tagged :source "elpaca-menu-lock-file" :recipe
         (:package "tagged" :host github :repo "someone/tagged" :tag "1.0"
                   :ref "5555555555555555555555555555555555555555")))
"""


def load_module():
    loader = importlib.machinery.SourceFileLoader("elpaca_cooldown", str(SCRIPT))
    spec = importlib.util.spec_from_loader("elpaca_cooldown", loader)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class RemoteTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()

    def test_remote_url_by_host(self):
        cases = {
            "github": "https://github.com/a/b.git",
            "gitlab": "https://gitlab.com/a/b.git",
            "codeberg": "https://codeberg.org/a/b.git",
            "sourcehut": "https://git.sr.ht/~a/b",
        }
        for host, url in cases.items():
            self.assertEqual(self.mod.remote_url({"id": "p", "host": host, "repo": "a/b"}), url)

    def test_remote_url_prefers_explicit_urls(self):
        self.assertEqual(self.mod.remote_url({"id": "p", "url": "https://x.org/p.git"}), "https://x.org/p.git")
        self.assertEqual(
            self.mod.remote_url({"id": "p", "host": "gnu", "repo": "https://github.com/a/b"}),
            "https://github.com/a/b",
        )

    def test_remote_url_prefers_repo_over_an_inherited_url(self):
        package = {"id": "bbdb", "host": "github", "repo": "benthamite/bbdb",
                   "url": "https://git.savannah.nongnu.org/git/bbdb.git"}
        self.assertEqual(self.mod.remote_url(package), "https://github.com/benthamite/bbdb.git")
        self.assertEqual(self.mod.classify(package, {"benthamite"})[0], "own")

    def test_remote_url_rejects_unknown_hosts(self):
        with self.assertRaises(ValueError):
            self.mod.remote_url({"id": "p", "host": "bitbucket", "repo": "a/b"})
        with self.assertRaises(ValueError):
            self.mod.remote_url({"id": "p"})

    def test_remote_owner(self):
        self.assertEqual(self.mod.remote_owner("https://github.com/Benthamite/elpaca.git"), "benthamite")
        self.assertEqual(self.mod.remote_owner("https://git.sr.ht/~someone/pkg"), "someone")
        self.assertEqual(self.mod.remote_owner("https://depp.brause.cc/nov.el.git"), "")

    def test_classify(self):
        exempt = {"benthamite"}
        classify = self.mod.classify
        self.assertEqual(classify({"id": "a", "host": "github", "repo": "benthamite/a"}, exempt)[0], "own")
        self.assertEqual(classify({"id": "b", "host": "github", "repo": "someone/b"}, exempt)[0], "third-party")
        tagged = {"id": "c", "host": "github", "repo": "someone/c", "tag": "1.0"}
        self.assertEqual(classify(tagged, exempt)[0], "tagged")


class ObservationTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()
        self.now = datetime(2026, 9, 18, 12, 0, tzinfo=timezone.utc)

    def test_record_tip_appends_only_new_tips(self):
        observations = {}
        self.mod.record_tip(observations, "k", "aaa", self.now)
        self.mod.record_tip(observations, "k", "aaa", self.now + timedelta(days=1))
        self.mod.record_tip(observations, "k", "bbb", self.now + timedelta(days=2))
        self.assertEqual([entry["sha"] for entry in observations["k"]], ["aaa", "bbb"])
        self.assertEqual(observations["k"][0]["first_seen"], self.now.isoformat())

    def test_aged_tip_returns_newest_old_enough_observation(self):
        history = [
            {"sha": "old", "first_seen": (self.now - timedelta(days=20)).isoformat()},
            {"sha": "aged", "first_seen": (self.now - timedelta(days=7)).isoformat()},
            {"sha": "fresh", "first_seen": (self.now - timedelta(days=6, hours=23)).isoformat()},
        ]
        self.assertEqual(self.mod.aged_tip(history, self.now, 7)["sha"], "aged")

    def test_aged_tip_is_none_without_old_observations(self):
        history = [{"sha": "fresh", "first_seen": (self.now - timedelta(days=1)).isoformat()}]
        self.assertIsNone(self.mod.aged_tip(history, self.now, 7))
        self.assertIsNone(self.mod.aged_tip([], self.now, 7))


class DecideTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()
        self.now = datetime(2026, 9, 18, 12, 0, tzinfo=timezone.utc)
        self.package = {"id": "pkg", "ref": "pinned", "tag": None}

    def decide(self, kind, history=(), take_now=(), package=None):
        return self.mod.decide(
            package or self.package, kind, "url", "tip", list(history), self.now, 7, set(take_now)
        )

    def aged(self, sha, days):
        return {"sha": sha, "first_seen": (self.now - timedelta(days=days)).isoformat()}

    def test_own_packages_take_the_tip(self):
        self.assertEqual(self.decide("own"), ("tip", ("own", ""), False))

    def test_tagged_packages_keep_their_ref(self):
        package = {"id": "pkg", "ref": "pinned", "tag": "1.0"}
        self.assertEqual(self.decide("tagged", package=package), ("pinned", ("pinned to a tag", "1.0"), False))

    def test_third_party_without_aged_observation_is_held(self):
        ref, (category, _), check = self.decide("third-party", [self.aged("tip", 2)])
        self.assertEqual((ref, category, check), ("pinned", "held", False))

    def test_third_party_takes_aged_tip_not_current_tip(self):
        ref, (category, _), check = self.decide("third-party", [self.aged("aged", 9), self.aged("tip", 1)])
        self.assertEqual((ref, category, check), ("aged", "updated", True))

    def test_third_party_already_at_aged_tip_is_unchanged(self):
        ref, (category, _), check = self.decide("third-party", [self.aged("pinned", 30)])
        self.assertEqual((ref, category, check), ("pinned", "unchanged", False))

    def test_take_now_bypasses_the_cooldown_and_says_so(self):
        ref, (category, _), check = self.decide("third-party", [self.aged("tip", 1)], take_now=["pkg"])
        self.assertEqual((ref, category, check), ("tip", "taken now without cooldown", False))

    def test_take_now_does_not_apply_to_tagged_packages(self):
        package = {"id": "pkg", "ref": "pinned", "tag": "1.0"}
        self.assertEqual(self.decide("tagged", take_now=["pkg"], package=package)[0], "pinned")


class ReportTest(unittest.TestCase):
    def test_report_counts_every_group_and_lists_changes(self):
        mod = load_module()
        packages = [{"id": "a", "ref": "1" * 40}, {"id": "b", "ref": "2" * 40}, {"id": "c", "ref": "3" * 40}]
        decisions = {
            "a": ["9" * 40, ("updated", "tip first seen 2026-09-01")],
            "b": ["2" * 40, ("held", "no observation is old enough yet")],
            "c": ["3" * 40, ("unchanged", "")],
        }
        report = mod.format_report(packages, decisions, 7)
        self.assertIn("updated: 1", report)
        self.assertIn("held: 1", report)
        self.assertIn("unchanged: 1", report)
        self.assertIn("a: 1111111111 -> 9999999999 (tip first seen 2026-09-01)", report)
        self.assertIn("b: 2222222222 (no observation is old enough yet)", report)
        self.assertNotIn("\nc:", report)


class UnusedTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()
        self.directory = tempfile.TemporaryDirectory()
        self.log = pathlib.Path(self.directory.name) / "package-usage.log"

    def tearDown(self):
        self.directory.cleanup()

    def test_read_usage_log_keeps_last_load_and_first_date(self):
        self.log.write_text("2026-07-01 vundo\n2026-09-01 vundo\n2026-08-01 wgrep\n\n")
        last, first = self.mod.read_usage_log(self.log)
        self.assertEqual(last["vundo"].isoformat(), "2026-09-01")
        self.assertEqual(last["wgrep"].isoformat(), "2026-08-01")
        self.assertEqual(first.isoformat(), "2026-07-01")

    def test_unused_lists_stale_and_never_loaded_third_party_packages(self):
        date = datetime(2026, 12, 1).date
        packages = [
            {"id": "fresh", "host": "github", "repo": "someone/fresh"},
            {"id": "stale", "host": "github", "repo": "someone/stale"},
            {"id": "never", "host": "github", "repo": "someone/never"},
            {"id": "mine", "host": "github", "repo": "benthamite/mine"},
        ]
        last = {"fresh": datetime(2026, 11, 20).date(), "stale": datetime(2026, 9, 1).date()}
        unused = self.mod.unused_packages(packages, {"benthamite"}, last, date(), 60)
        self.assertEqual(unused, [("never", None), ("stale", datetime(2026, 9, 1).date())])

    def test_young_log_reports_nothing(self):
        today = self.mod.now_utc().date()
        self.log.write_text(f"{today.isoformat()} vundo\n")
        args = Namespace(usage_log=self.log, days=60, lockfile=None, policy_dir=None)
        with redirect_stdout(io.StringIO()) as output:
            self.mod.command_unused(args)
        self.assertIn("covers 0 days", output.getvalue())


@unittest.skipUnless(shutil.which("emacs"), "needs emacs")
class LockfileTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()
        self.directory = tempfile.TemporaryDirectory()
        self.lockfile = pathlib.Path(self.directory.name) / "lockfile.el"
        self.lockfile.write_text(LOCKFILE)

    def tearDown(self):
        self.directory.cleanup()

    def test_export_reports_the_tracked_remote(self):
        packages = {package["id"]: package for package in self.mod.export_lockfile(self.lockfile)}
        self.assertEqual(self.mod.remote_url(packages["theirs"]), "https://github.com/someone/theirs.git")
        self.assertEqual(packages["theirs"]["branch"], "main")
        self.assertEqual(self.mod.remote_url(packages["mirrored"]), "https://github.com/emacsmirror/gnu_elpa")
        self.assertEqual(packages["mirrored"]["branch"], "externals/mirrored")
        self.assertEqual(self.mod.remote_url(packages["forked"]), "https://github.com/benthamite/forked.git")
        self.assertEqual(packages["forked"]["branch"], "fix")
        self.assertEqual(packages["tagged"]["tag"], "1.0")

    def test_rewrite_changes_only_the_named_refs(self):
        out = pathlib.Path(self.directory.name) / "candidate.el"
        self.mod.rewrite_lockfile(self.lockfile, {"theirs": "a" * 40}, out)
        before = {package["id"]: package for package in self.mod.export_lockfile(self.lockfile)}
        after = {package["id"]: package for package in self.mod.export_lockfile(out)}
        self.assertEqual(after["theirs"]["ref"], "a" * 40)
        before["theirs"]["ref"] = "a" * 40
        self.assertEqual(before, after)

    def test_rewrite_rejects_unknown_packages(self):
        out = pathlib.Path(self.directory.name) / "candidate.el"
        with self.assertRaises(RuntimeError):
            self.mod.rewrite_lockfile(self.lockfile, {"absent": "a" * 40}, out)
        self.assertFalse(out.exists())

    def test_plan_holds_third_party_and_advances_own(self):
        out = pathlib.Path(self.directory.name) / "candidate.el"
        state = pathlib.Path(self.directory.name) / "observations.json"
        args = Namespace(
            lockfile=self.lockfile, policy_dir=ROOT / "emacs/elpaca-cooldown", state_file=state,
            out=out, report=None, min_age_days=7, take_now=[], drop=["mirrored"],
        )
        tips = lambda remotes: ({key: "f" * 40 for key in remotes}, {})
        with patch.object(self.mod, "fetch_tips", tips), redirect_stdout(io.StringIO()):
            self.mod.command_plan(args)
        refs = {package["id"]: package["ref"] for package in self.mod.export_lockfile(out)}
        self.assertEqual(refs["mine"], "f" * 40)
        self.assertEqual(refs["forked"], "f" * 40)
        self.assertEqual(refs["theirs"], "2" * 40)
        self.assertNotIn("mirrored", refs)
        self.assertEqual(refs["tagged"], "5" * 40)

    def test_verify_build_flags_drift_and_lists_new_packages(self):
        built = pathlib.Path(self.directory.name) / "built.el"
        built.write_text(LOCKFILE.replace("2" * 40, "e" * 40))
        args = Namespace(candidate=self.lockfile, built=built)
        with redirect_stdout(io.StringIO()) as output, self.assertRaises(RuntimeError):
            self.mod.command_verify_build(args)
        self.assertIn("DRIFT: theirs", output.getvalue())


if __name__ == "__main__":
    unittest.main()
