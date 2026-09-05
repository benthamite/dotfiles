"""Exercise the documented overnight helper recipe with synthetic verdicts.

There is no autonomous overnight runner to test. This fixture supplies the
orchestrator's array materialization, worker-start decisions and accounting.
Triage/ledger are real subprocess CLIs. Walk uses its public main() commands
with established disposable-root injection before any call; no private queue
files are inspected. No real notes, workers, Emacs, services or schedules run.
"""

import contextlib
import importlib.util
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest
from collections import Counter
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SKILL = ROOT / "codex/skills/overnight-todos"
WALK = ROOT / "codex/skills/walk-list/walk.py"


class OvernightTodosIntegrationTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="overnight-integration-", dir="/tmp")
        self.addCleanup(temporary.cleanup)
        self.directory = Path(temporary.name).resolve()
        self.ledger_directory = self.directory / "ledger"
        self.ledger_directory.mkdir(mode=0o700)
        self.ledger = self.ledger_directory / "state.json"
        self.history = self.ledger_directory / "history.md"
        self.source = self.directory / "dump.json"
        self.triage_report = self.directory / "triage.md"
        self.classifications = self.directory / "classifications.json"
        self.filtered = self.directory / "filtered.json"
        self.queue = self.directory / "queue.json"
        self.walk_outputs = self.directory / "queue-evidence"
        self.walk_outputs.mkdir(mode=0o700)
        spec = importlib.util.spec_from_file_location("overnight_fixture_walk", WALK)
        self.walk = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.walk)
        # Same injection boundary as test_walk_list.py, before any command.
        self.walk.DATA_ROOT = self.directory / "queue-internal"
        self.walk.DATA_ROOT.mkdir(mode=0o700)
        self.walk.REGISTRY_PATH = self.walk.DATA_ROOT / "registry.json"
        self.walk.OUTPUT_ROOT = self.walk_outputs
        for directory in (self.directory, self.ledger_directory, self.walk.DATA_ROOT, self.walk_outputs):
            metadata = directory.stat()
            self.assertEqual(metadata.st_uid, os.getuid())
            self.assertEqual(metadata.st_mode & 0o777, 0o700)
        self.walk_commands = []
        self.notes = {}

    def private_write(self, path, data):
        with path.open("xb") as stream:
            os.fchmod(stream.fileno(), 0o600)
            stream.write(data)

    def fixture_record(self, identity, title, priority, effort=None, tags=None):
        note = self.directory / (identity + ".org")
        body = f"* TODO {title}\n:PROPERTIES:\n:ID: {identity}\n:END:\nSynthetic context only.\n".encode()
        self.private_write(note, body)
        self.notes[note] = body
        return {"id": identity, "file": str(note), "title": title, "todo": "TODO",
                "priority": priority, "effort": effort, "tags": tags or [], "olp": []}

    def cli(self, helper, *arguments):
        result = subprocess.run([sys.executable, str(SKILL / helper), *map(str, arguments)],
                                cwd=self.directory, text=True, capture_output=True,
                                timeout=10, check=False,
                                env=dict(os.environ, PYTHONDONTWRITEBYTECODE="1"))
        self.assertEqual(result.returncode, 0, result.stderr)
        return result.stdout

    def ledger_record(self, record, token, verdict):
        return self.cli("ledger.py", "record", "--ledger", self.ledger,
                        "--operation-id", token, "--id", record["id"],
                        "--file", record["file"], "--title", record["title"],
                        "--verdict", verdict)

    def prepare(self, cap, *, only_exclusions=False):
        records = [self.fixture_record("title-blocker", "Buy a fixture device", "1"),
                   self.fixture_record("remembered-blocker", "Check fixture credentials", "1")]
        if not only_exclusions:
            records += [self.fixture_record("first-candidate", "Fix a fixture typo", "1", "15m"),
                        self.fixture_record("later-candidate", "Check a fixture result", "7", "1:00"),
                        self.fixture_record("project", "Fix a fixture project", "2", tags=["project"])]
        self.input_ids = [record["id"] for record in records]
        self.ledger_record(records[1], "prior-fixture-operation",
                           "BLOCKED: fixture-only missing authority | ease=2 | suggested_next_step=review scope")
        ledger_before_filter = self.ledger.read_bytes()
        history_before_filter = self.history.read_bytes()
        self.private_write(self.source, (json.dumps(records) + "\n").encode())
        self.cli("triage.py", "--input", self.source, "--output", self.triage_report,
                 "--classifications-out", self.classifications, "--mode", "act", "--max-tasks", str(cap))
        self.cli("ledger.py", "filter", "--classifications", self.classifications,
                 "--ledger", self.ledger, "--output", self.filtered, "--skip-window-days", "14")
        self.assertEqual(self.ledger.read_bytes(), ledger_before_filter)
        self.assertEqual(self.history.read_bytes(), history_before_filter)
        filtered = json.loads(self.filtered.read_text())
        self.assertEqual([record["id"] for record in filtered["blocked"]], ["title-blocker"])
        self.assertEqual([record["id"] for record in filtered["still_blocked"]], ["remembered-blocker"])
        # Preserve the classifier's order within each eligible bucket.
        queued = filtered["candidate"] + filtered["investigate"]
        self.assertEqual(len(queued), len({record["id"] for record in queued}))
        if queued:
            self.queue_bytes = (json.dumps(queued, ensure_ascii=False) + "\n").encode()
            self.private_write(self.queue, self.queue_bytes)
        return filtered, queued

    def walk_call(self, command, *arguments, error=None):
        self.walk_commands.append(command)
        output = io.StringIO()
        previous = os.umask(0o077)
        try:
            with mock.patch.object(sys, "argv", [str(WALK), command, str(self.queue), *map(str, arguments)]), \
                    contextlib.redirect_stdout(output):
                if error is None:
                    self.walk.main()
                else:
                    with self.assertRaisesRegex(SystemExit, error):
                        self.walk.main()
        finally:
            os.umask(previous)
        return output.getvalue()

    def status(self):
        return json.loads(self.walk_call("pool-status"))

    def dispatch(self):
        output = self.walk_call("dispatch")
        metadata, disclosed = output.split("\n\n", 1)
        token = metadata.splitlines()[0].removeprefix("CLAIM_TOKEN: ")
        self.assertRegex(token, r"^[0-9a-f]{32}$")
        return token, json.loads(disclosed)

    def drain(self, cap, minutes):
        preview = self.walk_call("start", "--max-concurrent", "1")
        self.assertNotIn("CLAIM_TOKEN:", preview)
        initial = self.status()
        self.assertEqual((initial["done"], initial["in_flight"], initial["available_slots"]), (0, 0, 1))
        starts, tokens, dispatched_ids = [], [], []
        recovered = False
        while self.status()["remaining_to_claim"]:
            token, item = self.dispatch()
            tokens.append(token)
            dispatched_ids.append(item["id"])
            self.assertNotIn(item["id"], {"title-blocker", "remembered-blocker"})
            claimed = self.status()
            self.assertEqual((claimed["in_flight"], claimed["available_slots"]), (1, 0))
            self.walk_call("dispatch", error="pool full")
            # No worker or TODO action runs. This explicit fixture counter
            # represents only the documented admission decision.
            if len(starts) < cap and minutes > 0:
                starts.append(item["id"])
                verdict = "COMPLETED: synthetic result only | files_changed=[none] | refs=[none]"
            else:
                verdict = "DEFERRED: no fixture worker started | reason=run budget reached"
            self.walk_call("record", token, verdict)
            self.assertEqual(self.status()["in_flight"], 0)
            self.ledger_record(item, token, verdict)
            if not recovered:
                # Reconcile an already persisted verdict after a lost caller
                # acknowledgement; repeat bookkeeping, never the task/claim.
                before = (self.ledger.read_bytes(), self.history.read_bytes())
                self.assertIn("RECONCILED", self.ledger_record(item, token, verdict))
                self.assertEqual((self.ledger.read_bytes(), self.history.read_bytes()), before)
                # This public listing confirms index/verdict, not the full
                # token. Exact token evidence is available after restore.
                self.assertIn(f"[1] {verdict}", self.walk_call("show-decisions"))
                self.walk_call("record", token, verdict, error="already-recorded claim token")
                recovered = True
        final = self.status()
        self.assertEqual((final["done"], final["in_flight"], final["remaining_to_claim"]),
                         (final["total"], 0, 0))
        restored = self.walk_call("restore")
        paths = [line.removeprefix("Decisions preserved at: ") for line in restored.splitlines()
                 if line.startswith("Decisions preserved at: ")]
        self.assertEqual(len(paths), 1)
        evidence_path = Path(paths[0])
        evidence_path.relative_to(self.walk_outputs)
        self.assertEqual(evidence_path.stat().st_mode & 0o777, 0o600)
        evidence = json.loads(evidence_path.read_text())
        self.assertEqual(self.queue.read_bytes(), self.queue_bytes)
        decisions = evidence["decisions"]
        self.assertEqual([entry["index"] for entry in decisions], list(range(final["total"])))
        self.assertEqual([entry["claim_token"] for entry in decisions], tokens)
        self.assertEqual([entry["item"]["id"] for entry in decisions], dispatched_ids)
        recovered_entry = decisions[0]
        before = (self.ledger.read_bytes(), self.history.read_bytes())
        self.assertIn("RECONCILED", self.ledger_record(recovered_entry["item"], recovered_entry["claim_token"],
                                                      recovered_entry["decision"]))
        self.assertEqual((self.ledger.read_bytes(), self.history.read_bytes()), before)
        ledger = json.loads(self.ledger.read_text())
        self.assertEqual({event["operation_id"] for event in ledger["events"]}, set(tokens) | {"prior-fixture-operation"})
        self.assertEqual(ledger["history_rendered"], len(tokens) + 1)
        self.assertTrue(all(ledger["todos"][identity]["attempts"] == 1 for identity in dispatched_ids))
        self.assertEqual(Counter(event["request"]["id"] for event in ledger["events"]),
                         Counter(dispatched_ids + ["remembered-blocker"]))
        return decisions, starts

    def account(self, filtered, decisions, starts):
        accounted = [(record["id"], "title_heuristic") for record in filtered["blocked"]]
        accounted += [(record["id"], "unchanged_blocker") for record in filtered["still_blocked"]]
        accounted += [(entry["item"]["id"], entry["decision"].split(":", 1)[0]) for entry in decisions]
        self.assertEqual(Counter(identity for identity, _ in accounted), Counter(self.input_ids))
        summary = {"worker_starts": len(starts), "counts": dict(Counter(kind for _, kind in accounted)),
                   "accounted_ids": [identity for identity, _ in accounted]}
        report = self.directory / "final.md"
        self.private_write(report, ("# Synthetic helper-recipe report\n\nNo real workers or TODO actions ran.\n\n"
                                    + "```json\n" + json.dumps(summary, indent=2) + "\n```\n").encode())
        self.assertEqual(report.stat().st_mode & 0o777, 0o600)
        self.assertTrue(self.triage_report.exists())
        for artifact in (self.source, self.triage_report, self.classifications, self.filtered, self.ledger, self.history):
            self.assertEqual(artifact.stat().st_mode & 0o777, 0o600)
        for note, original in self.notes.items():
            self.assertEqual(note.read_bytes(), original)
        return summary

    def test_cap_one_dispatches_only_eligible_items_and_recovers_ledger_operation(self):
        filtered, queue = self.prepare(cap=1)
        self.assertEqual([record["id"] for record in queue], ["first-candidate", "later-candidate", "project"])
        decisions, starts = self.drain(cap=1, minutes=60)
        self.assertEqual(starts, ["first-candidate"])
        summary = self.account(filtered, decisions, starts)
        self.assertEqual(summary["counts"], {"title_heuristic": 1, "unchanged_blocker": 1,
                                            "COMPLETED": 1, "DEFERRED": 2})

    def test_zero_time_budget_records_every_queued_item_deferred_without_workers(self):
        filtered, _ = self.prepare(cap=25)
        decisions, starts = self.drain(cap=25, minutes=0)
        self.assertEqual(starts, [])
        summary = self.account(filtered, decisions, starts)
        self.assertEqual(summary["counts"], {"title_heuristic": 1, "unchanged_blocker": 1, "DEFERRED": 3})

    def test_zero_worker_cap_records_deferrals_and_does_not_count_claims_as_starts(self):
        filtered, _ = self.prepare(cap=0)
        decisions, starts = self.drain(cap=0, minutes=60)
        summary = self.account(filtered, decisions, starts)
        self.assertEqual(summary["worker_starts"], 0)
        self.assertEqual(summary["counts"]["DEFERRED"], 3)

    def test_exclusions_only_skip_walk_creation_and_still_account_for_every_input(self):
        filtered, queue = self.prepare(cap=1, only_exclusions=True)
        self.assertEqual(queue, [])
        self.assertEqual(self.walk_commands, [])
        summary = self.account(filtered, [], [])
        self.assertEqual(summary["counts"], {"title_heuristic": 1, "unchanged_blocker": 1})


if __name__ == "__main__":
    unittest.main()
