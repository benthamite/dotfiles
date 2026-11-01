"""Tests for exit-skill-audit's deterministic state: due logic, lock, receipts, resume.

Run with: python3 -B -m unittest discover -s <this directory> -p 'test_*.py'
"""

from __future__ import annotations

import io
import json
import os
import sys
import tempfile
import threading
import unittest
from contextlib import redirect_stdout
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import skill_audit as sa  # noqa: E402

SESSION_A = "aaaaaaaa-0000-0000-0000-000000000000"
SESSION_B = "bbbbbbbb-0000-0000-0000-000000000000"
NOW = "2026-12-05T10:00:00+00:00"

PROPOSALS = """# exit-skill-audit proposals: demo, 2026-12-05

## 1. Closeout misses commitments
Problem: three sessions surfaced commitments that were not filed.
Proposed change: add a check to step 2.
Recommendation: do
Decision: {d1}
{a1}
## 2. Shorten the report
Problem: reports restate the README.
Proposed change: drop the restatement.
Recommendation: skip
Decision: {d2}
"""


class Fixture(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        self.state_home = root / "state"
        self.targets = root / "targets"
        self.work = root / "work" / "ph-ps"
        self.work.mkdir(parents=True)
        self.other = root / "elsewhere"
        self.other.mkdir()
        (self.targets / "demo").mkdir(parents=True)
        (self.targets / "demo" / "target.json").write_text(json.dumps({
            "id": "demo", "skill": "/nonexistent/SKILL.md",
            "directories": [str(root / "work")], "interval_days": 30,
            "checks": "checks.md", "tests": "tests"}))
        self.receipts = root / "receipts"
        self.receipts.mkdir()
        self.env = {"XDG_STATE_HOME": str(self.state_home)}
        self.old_env = {k: os.environ.get(k) for k in self.env}
        os.environ.update(self.env)

    def tearDown(self):
        for key, value in self.old_env.items():
            if value is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = value
        self.tmp.cleanup()

    def run_cli(self, *argv: str) -> tuple[int, dict]:
        out = io.StringIO()
        with redirect_stdout(out):
            code = sa.main(["--targets-dir", str(self.targets), *argv])
        return code, json.loads(out.getvalue())

    def decide(self, cwd=None, session=SESSION_A, today="2026-12-05", now=NOW):
        return self.run_cli("decide", "--cwd", str(cwd or self.work),
                            "--session", session, "--today", today, "--now", now)

    def proposals(self, d1="pending", d2="pending", a1="") -> Path:
        path = Path(self.tmp.name) / "proposals.md"
        path.write_text(PROPOSALS.format(d1=d1, d2=d2, a1=a1))
        return path


class DueLogicTest(Fixture):
    def test_uncovered_directory_is_noop(self):
        code, result = self.decide(cwd=self.other)
        self.assertEqual(code, 0)
        self.assertEqual(result["action"], "no-op")
        self.assertIn("no exit-skill-audit target covers", result["reason"])

    def test_never_audited_is_due(self):
        _, result = self.decide()
        self.assertEqual(result["action"], "audit")
        self.assertEqual(result["target"], "demo")
        self.assertTrue(result["checks"].endswith("demo/checks.md"))

    def test_not_due_within_interval(self):
        self.run_cli("mark-audited", "--target", "demo", "--date", "2026-11-10",
                     "--note", "manual")
        _, result = self.decide(today="2026-12-09")
        self.assertEqual(result["action"], "no-op")
        self.assertIn("next audit due 2026-12-10", result["reason"])
        _, result = self.decide(today="2026-12-10")
        self.assertEqual(result["action"], "audit")

    def test_failed_attempt_retries_next_day_only(self):
        self.run_cli("mark-audited", "--target", "demo", "--date", "2026-10-01",
                     "--note", "old")
        self.run_cli("begin", "--target", "demo", "--session", SESSION_A,
                     "--today", "2026-12-05", "--now", NOW)
        self.run_cli("fail", "--target", "demo", "--session", SESSION_A,
                     "--today", "2026-12-05", "--reason", "subagent crashed")
        _, result = self.decide(today="2026-12-05")
        self.assertEqual(result["action"], "no-op")
        _, result = self.decide(today="2026-12-06")
        self.assertEqual(result["action"], "audit")


class LockTest(Fixture):
    def test_second_session_is_locked_out(self):
        code, _ = self.run_cli("begin", "--target", "demo", "--session", SESSION_A,
                               "--today", "2026-12-05", "--now", NOW)
        self.assertEqual(code, 0)
        _, result = self.decide(session=SESSION_B)
        self.assertEqual(result["action"], "no-op")
        self.assertIn(f"in progress in session {SESSION_A}", result["reason"])
        code, result = self.run_cli("claim", "--target", "demo", "--session",
                                    SESSION_B, "--now", NOW)
        self.assertEqual(code, 3)
        self.assertEqual(result["owner"]["session"], SESSION_A)

    def test_lock_is_reentrant_for_its_session(self):
        self.run_cli("claim", "--target", "demo", "--session", SESSION_A, "--now", NOW)
        code, result = self.run_cli("claim", "--target", "demo", "--session",
                                    SESSION_A, "--now", NOW)
        self.assertEqual(code, 0)
        self.assertEqual(result["lock"], "already held by this session")

    def test_stale_lock_is_broken_and_reported(self):
        self.run_cli("claim", "--target", "demo", "--session", SESSION_A,
                     "--now", "2026-12-01T00:00:00+00:00")
        code, result = self.run_cli("claim", "--target", "demo", "--session",
                                    SESSION_B, "--now", NOW)
        self.assertEqual(code, 0)
        self.assertIn(f"broke stale lock of session {SESSION_A}", result["lock"])

    def test_concurrent_claims_have_one_winner(self):
        codes = []
        sessions = [f"{i:08d}-race" for i in range(12)]

        def claim(session):
            lock = Path(self.state_home) / "exit-skill-audit" / "demo"
            lock.mkdir(parents=True, exist_ok=True)
            try:
                sa.acquire("demo", session, sa.dt.datetime.fromisoformat(NOW))
                codes.append(0)
            except sa.LockHeld:
                codes.append(3)

        threads = [threading.Thread(target=claim, args=(s,)) for s in sessions]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()
        self.assertEqual(codes.count(0), 1)
        self.assertEqual(codes.count(3), len(sessions) - 1)

    def test_concurrent_stale_breakers_have_one_winner(self):
        sa.acquire("demo", SESSION_A,
                   sa.dt.datetime.fromisoformat("2026-12-01T00:00:00+00:00"))
        codes = []

        def claim(session):
            try:
                sa.acquire("demo", session, sa.dt.datetime.fromisoformat(NOW))
                codes.append(0)
            except sa.LockHeld:
                codes.append(3)

        threads = [threading.Thread(target=claim, args=(f"{i:08d}-stale",))
                   for i in range(12)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()
        self.assertEqual(codes.count(0), 1)
        lock = sa.read_lock("demo")
        self.assertTrue(lock["session"].endswith("-stale"))
        leftovers = [p.name for p in sa.target_dir("demo").iterdir()
                     if p.name != "lock"]
        self.assertEqual(leftovers, [])

    def test_corrupt_recent_lock_counts_as_held(self):
        sa.target_dir("demo").mkdir(parents=True)
        sa.lock_path("demo").write_text("{not json")
        with self.assertRaises(sa.LockHeld):
            sa.acquire("demo", SESSION_B, sa.dt.datetime.now(sa.dt.timezone.utc))

    def test_release_refuses_another_sessions_lock(self):
        self.run_cli("claim", "--target", "demo", "--session", SESSION_A, "--now", NOW)
        code, result = self.run_cli("complete", "--target", "demo", "--session",
                                    SESSION_B, "--today", "2026-12-05")
        self.assertEqual(code, 2)
        self.assertIn("does not hold", result["error"])


class ProposalsTest(Fixture):
    def begin_and_hold(self, path: Path):
        self.run_cli("begin", "--target", "demo", "--session", SESSION_A,
                     "--today", "2026-12-05", "--now", NOW)
        return self.run_cli("hold", "--target", "demo", "--session", SESSION_A,
                            "--proposals", str(path), "--today", "2026-12-05")

    def test_hold_then_resume_instead_of_reaudit(self):
        code, result = self.begin_and_hold(self.proposals())
        self.assertEqual(code, 0)
        self.assertEqual(result["pending"], [1, 2])
        # Same session re-runs the chain after an unanswered hold: resume.
        _, result = self.decide(today="2027-02-01")
        self.assertEqual(result["action"], "resume")
        self.assertEqual(result["pending"], [1, 2])

    def test_abandoned_hold_resumes_in_another_session_after_stale(self):
        self.begin_and_hold(self.proposals())
        _, result = self.decide(session=SESSION_B, now="2026-12-05T12:00:00+00:00")
        self.assertEqual(result["action"], "no-op")
        _, result = self.decide(session=SESSION_B, now="2026-12-07T12:00:00+00:00")
        self.assertEqual(result["action"], "resume")

    def test_all_decided_means_resolve(self):
        path = self.proposals()
        self.begin_and_hold(path)
        path.write_text(PROPOSALS.format(d1="do", d2="skip",
                                         a1="Applied: ph-ps abc1234"))
        _, result = self.decide()
        self.assertEqual(result["action"], "resolve")
        code, result = self.run_cli("resolve", "--target", "demo", "--session",
                                    SESSION_A, "--today", "2026-12-06", "--now", NOW)
        self.assertEqual(code, 0)
        _, status = self.run_cli("status", "--target", "demo")
        self.assertIsNone(status["state"]["pending_proposals"])
        self.assertIsNone(status["lock"])
        _, result = self.decide(today="2026-12-06")
        self.assertEqual(result["action"], "no-op")
        self.assertIn("next audit due 2027-01-04", result["reason"])

    def test_resolve_refuses_approved_change_without_applied_line(self):
        path = self.proposals()
        self.begin_and_hold(path)
        path.write_text(PROPOSALS.format(d1="do", d2="skip", a1=""))
        code, result = self.run_cli("resolve", "--target", "demo", "--session",
                                    SESSION_A, "--today", "2026-12-06", "--now", NOW)
        self.assertEqual(code, 2)
        self.assertIn("approved without an Applied line [1]", result["error"])

    def test_invalid_decision_is_rejected(self):
        code, result = self.begin_and_hold(self.proposals(d1="maybe"))
        self.assertEqual(code, 2)
        self.assertIn("proposal 1 has decision 'maybe'", result["error"])

    def test_audit_with_no_proposals_completes(self):
        self.run_cli("begin", "--target", "demo", "--session", SESSION_A,
                     "--today", "2026-12-05", "--now", NOW)
        code, _ = self.run_cli("complete", "--target", "demo", "--session",
                               SESSION_A, "--today", "2026-12-05")
        self.assertEqual(code, 0)
        _, result = self.decide()
        self.assertEqual(result["action"], "no-op")


class ReceiptTest(Fixture):
    def test_writes_valid_json(self):
        path = self.receipts / "receipt.json"
        code, _ = self.run_cli("write-receipt", "--receipt-file", str(path),
                               "--status", "held", "--message", "2 proposals")
        self.assertEqual(code, 0)
        self.assertEqual(json.loads(path.read_text()),
                         {"status": "held", "message": "2 proposals"})
        self.assertEqual([p.name for p in self.receipts.iterdir()], ["receipt.json"])

    def test_refuses_existing_file(self):
        path = self.receipts / "receipt.json"
        path.write_text("{}")
        code, result = self.run_cli("write-receipt", "--receipt-file", str(path),
                                    "--status", "no-op", "--message", "x")
        self.assertEqual(code, 2)
        self.assertIn("already exists", result["error"])
        self.assertEqual(path.read_text(), "{}")

    def test_rejects_unknown_status_and_empty_message(self):
        path = self.receipts / "receipt.json"
        code, _ = self.run_cli("write-receipt", "--receipt-file", str(path),
                               "--status", "done", "--message", "x")
        self.assertEqual(code, 2)
        code, _ = self.run_cli("write-receipt", "--receipt-file", str(path),
                               "--status", "no-op", "--message", "  ")
        self.assertEqual(code, 2)
        self.assertFalse(path.exists())

    def test_every_status_accepted(self):
        for status in sa.RECEIPT_STATUSES:
            path = self.receipts / f"{status}.json"
            code, _ = self.run_cli("write-receipt", "--receipt-file", str(path),
                                   "--status", status, "--message", "m")
            self.assertEqual(code, 0, status)


class RealTargetsTest(unittest.TestCase):
    def test_shipped_targets_load(self):
        targets = sa.load_targets(sa.DEFAULT_TARGETS_DIR)
        ids = [t["id"] for t in targets]
        self.assertIn("forethought-closeout", ids)
        for target in targets:
            directory = Path(target["_dir"])
            self.assertTrue((directory / target["checks"]).is_file(), target["id"])
            if target.get("tests"):
                self.assertTrue((directory / target["tests"]).is_dir(), target["id"])


if __name__ == "__main__":
    unittest.main()
