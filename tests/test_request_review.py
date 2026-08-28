import argparse
import importlib.util
import io
import json
import subprocess
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from types import SimpleNamespace
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
CODEX_SCRIPT = ROOT / "codex/skills/request-review/scripts/request_review.py"
CLAUDE_SCRIPT = ROOT / "claude/skills/request-review/scripts/request_review.py"
CODEX_SKILL = ROOT / "codex/skills/request-review/SKILL.md"
CLAUDE_SKILL = ROOT / "claude/skills/request-review/SKILL.md"


def load_module():
    spec = importlib.util.spec_from_file_location("request_review", CODEX_SCRIPT)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


reviewer = load_module()


class SkillPairingTests(unittest.TestCase):
    def test_paired_helpers_stay_identical(self):
        self.assertEqual(CODEX_SCRIPT.read_bytes(), CLAUDE_SCRIPT.read_bytes())

    def test_paired_skills_stay_identical_and_registered(self):
        self.assertEqual(CODEX_SKILL.read_bytes(), CLAUDE_SKILL.read_bytes())
        manifest = json.loads((ROOT / "ai-config-sync.json").read_text())
        paired = {
            entry["name"] for entry in manifest["skills"]
            if entry.get("status") == "paired"
        }
        self.assertIn("request-review", paired)

    def test_skill_encodes_single_pass_and_terminal_rules(self):
        skill = " ".join(CODEX_SKILL.read_text(encoding="utf-8").split())
        required = (
            "opposite backend",
            "git show",
            "terminal incomplete review",
            "exactly one",
            "never asked to re-review",
        )
        for rule in required:
            with self.subTest(rule=rule):
                self.assertIn(rule, skill)


class CrossReviewRunTests(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary_directory.cleanup)
        self.directory = Path(self.temporary_directory.name)
        self.repo = self.directory / "repo"
        self.repo.mkdir()
        self._git("init", "--quiet")
        plan = self.repo / "docs" / "plan.md"
        plan.parent.mkdir()
        plan.write_text("# The plan\n", encoding="utf-8")
        self._git("add", "docs/plan.md")
        self._git(
            "-c", "user.name=Test", "-c", "user.email=test@example.com",
            "commit", "--quiet", "-m", "add plan",
        )
        self.commit = self._git("rev-parse", "HEAD").strip()
        self.run_file = self.directory / "review-run.json"
        self.transcript = self.directory / "reviewer.jsonl"
        self.marker = f"REVIEW COMPLETE: {self.commit[:12]}"
        backend_probe = mock.patch.object(
            reviewer.session, "buffer_backend", return_value="codex"
        )
        backend_probe.start()
        self.addCleanup(backend_probe.stop)

    def _git(self, *argv):
        proc = subprocess.run(
            ["git", "-C", str(self.repo), *argv],
            text=True,
            capture_output=True,
            check=True,
        )
        return proc.stdout

    def init_args(self, **overrides):
        values = {
            "run_file": str(self.run_file),
            "repo": str(self.repo),
            "plan_path": "docs/plan.md",
            "plan_commit": self.commit,
            "caller_backend": "claude-code",
            "reviewer_buffer": "*codex:review*",
            "reviewer_backend": "codex",
            "reviewer_transcript": str(self.transcript),
        }
        values.update(overrides)
        return argparse.Namespace(**values)

    def waiting_reviewer(self):
        return mock.patch.object(
            reviewer.session,
            "buffer_state",
            return_value={
                "state": "awaiting-input",
                "buffer": "*codex:review*",
                "directory": str(self.repo) + "/",
            },
        )

    def matching_transcript(self, value=None):
        return mock.patch.object(
            reviewer.session, "agent_transcript_path", return_value=value
        )

    def create_run(self, **overrides):
        with (
            self.waiting_reviewer(),
            self.matching_transcript(),
            redirect_stdout(io.StringIO()),
        ):
            reviewer.init_review(self.init_args(**overrides))
        return reviewer.load_review(self.run_file)

    def submit(self):
        with (
            self.waiting_reviewer(),
            self.matching_transcript(),
            mock.patch.object(
                reviewer.session, "_wait_for_transcript_path", return_value=None
            ),
            redirect_stdout(io.StringIO()),
        ):
            reviewer.submit_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

    def write_reviewer_return(self, text):
        record = {
            "timestamp": "2026-08-24T12:00:00Z",
            "message": {"role": "assistant", "content": text},
        }
        with self.transcript.open("a", encoding="utf-8") as stream:
            stream.write(json.dumps(record) + "\n")

    def test_declared_backend_must_match_detected_backend(self):
        with (
            self.waiting_reviewer(),
            self.matching_transcript(),
            mock.patch.object(
                reviewer.session, "buffer_backend", return_value="claude-code"
            ),
            self.assertRaisesRegex(SystemExit, "actual backend is claude-code"),
        ):
            reviewer.init_review(self.init_args())

    def test_init_resolves_a_ref_name_to_the_full_commit_oid(self):
        branch = self._git("rev-parse", "--abbrev-ref", "HEAD").strip()
        state = self.create_run(plan_commit=branch)
        self.assertEqual(state["plan"]["commit"], self.commit)
        self.assertEqual(len(state["plan"]["commit"]), 40)

    def test_same_backend_reviewer_is_rejected(self):
        with self.assertRaisesRegex(SystemExit, "opposite of the caller backend"):
            reviewer.init_review(self.init_args(reviewer_backend="claude-code"))

    def test_uncommitted_plan_is_rejected(self):
        uncommitted = self.repo / "docs" / "draft.md"
        uncommitted.write_text("draft\n", encoding="utf-8")
        with self.assertRaisesRegex(SystemExit, "not committed"):
            reviewer.init_review(self.init_args(plan_path="docs/draft.md"))

    def test_unknown_commit_and_non_repo_are_rejected(self):
        with self.assertRaisesRegex(SystemExit, "does not resolve"):
            reviewer.init_review(self.init_args(plan_commit="0" * 40))
        outside = self.directory / "not-a-repo"
        outside.mkdir()
        with self.assertRaisesRegex(SystemExit, "not a git repository"):
            reviewer.init_review(self.init_args(repo=str(outside)))

    def test_plan_path_outside_repository_is_rejected(self):
        with self.assertRaisesRegex(SystemExit, "outside the repository"):
            reviewer.init_review(
                self.init_args(plan_path=str(self.directory / "elsewhere.md"))
            )

    def test_non_fresh_reviewer_transcript_is_rejected(self):
        self.transcript.write_text("history\n", encoding="utf-8")
        with self.waiting_reviewer(), self.matching_transcript():
            with self.assertRaisesRegex(SystemExit, "fresh reviewer session"):
                reviewer.init_review(self.init_args())

    def test_mismatched_buffer_transcript_identity_is_rejected(self):
        other = self.directory / "other.jsonl"
        other.write_text("", encoding="utf-8")
        with self.waiting_reviewer(), self.matching_transcript(str(other)):
            with self.assertRaisesRegex(SystemExit, "does not match the supplied"):
                reviewer.init_review(self.init_args())

    def test_reviewer_outside_plan_repository_is_rejected(self):
        with (
            mock.patch.object(
                reviewer.session,
                "buffer_state",
                return_value={
                    "state": "awaiting-input",
                    "buffer": "*codex:review*",
                    "directory": str(self.directory) + "/",
                },
            ),
            self.matching_transcript(),
            self.assertRaisesRegex(SystemExit, "not inside the plan"),
        ):
            reviewer.init_review(self.init_args())

    def test_init_records_immutable_anchor_and_private_file(self):
        blob = self._git("rev-parse", f"{self.commit}:docs/plan.md").strip()
        state = self.create_run(
            plan_path=str(self.repo / "docs" / "plan.md")
        )
        self.assertEqual(self.run_file.stat().st_mode & 0o777, 0o600)
        self.assertEqual(state["plan"]["path"], "docs/plan.md")
        self.assertEqual(state["plan"]["commit"], self.commit)
        self.assertEqual(state["plan"]["blob_sha"], blob)
        self.assertEqual(state["status"], "ready")
        self.assertFalse(state["restart_used"])

    def test_submit_sends_anchored_prompt_with_marker_contract(self):
        self.create_run()
        with (
            self.waiting_reviewer(),
            self.matching_transcript(),
            mock.patch.object(reviewer.session, "submit_to_agent") as submit,
            mock.patch.object(
                reviewer.session, "_wait_for_transcript_path", return_value=None
            ),
            redirect_stdout(io.StringIO()),
        ):
            reviewer.submit_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

        buffer, backend, prompt = submit.call_args.args
        self.assertEqual((buffer, backend), ("*codex:review*", "codex"))
        self.assertIn(f"show {self.commit}:docs/plan.md", prompt)
        self.assertIn("Do not read the working-tree copy", prompt)
        self.assertIn("do not request a revised plan", prompt)
        self.assertTrue(prompt.endswith(self.marker))
        state = reviewer.load_review(self.run_file)
        self.assertEqual(state["status"], "review-active")

    def test_submit_adopts_marker_bearing_transcript_created_on_delivery(self):
        placeholder = self.directory / "not-yet-created.jsonl"
        self.create_run(reviewer_transcript=str(placeholder))
        fresh = self.directory / "rollout-fresh.jsonl"
        filler = {
            "type": "response_item",
            "payload": {
                "type": "message",
                "role": "user",
                "content": [{"type": "input_text", "text": "unrelated"}],
            },
        }
        prompt_record = {
            "type": "response_item",
            "payload": {
                "type": "message",
                "role": "user",
                "content": [{"type": "input_text", "text": f"prompt\n{self.marker}"}],
            },
        }
        fresh.write_text(
            json.dumps(filler) + "\n" + json.dumps(prompt_record) + "\n",
            encoding="utf-8",
        )
        with (
            self.waiting_reviewer(),
            mock.patch.object(
                reviewer.session, "agent_transcript_path", return_value=None
            ),
            mock.patch.object(reviewer.session, "submit_to_agent"),
            mock.patch.object(
                reviewer.session,
                "_wait_for_transcript_path",
                return_value=str(fresh),
            ),
            redirect_stdout(io.StringIO()),
        ):
            reviewer.submit_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

        state = reviewer.load_review(self.run_file)
        self.assertEqual(state["reviewer"]["transcript"], str(fresh))
        self.assertEqual(
            state["submission"],
            {"transcript_offset": len(json.dumps(filler)) + 1},
        )
        self.assertEqual(state["status"], "review-active")

    def test_submit_keeps_binding_when_discovered_transcript_lacks_marker(self):
        placeholder = self.directory / "not-yet-created.jsonl"
        self.create_run(reviewer_transcript=str(placeholder))
        foreign = self.directory / "rollout-foreign.jsonl"
        foreign.write_text(
            json.dumps(
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "user",
                        "content": [{"type": "input_text", "text": "other session"}],
                    },
                }
            )
            + "\n",
            encoding="utf-8",
        )
        with (
            self.waiting_reviewer(),
            mock.patch.object(
                reviewer.session, "agent_transcript_path", return_value=None
            ),
            mock.patch.object(reviewer.session, "submit_to_agent"),
            mock.patch.object(
                reviewer.session,
                "_wait_for_transcript_path",
                return_value=str(foreign),
            ),
            redirect_stdout(io.StringIO()),
        ):
            reviewer.submit_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

        state = reviewer.load_review(self.run_file)
        self.assertEqual(state["reviewer"]["transcript"], str(placeholder))

    def test_finish_review_adopts_late_marker_bearing_transcript(self):
        placeholder = self.directory / "never-created.jsonl"
        self.create_run(reviewer_transcript=str(placeholder))
        with mock.patch.object(reviewer.session, "submit_to_agent"):
            self.submit()

        fresh = self.directory / "rollout-late.jsonl"
        prompt_record = {
            "type": "response_item",
            "payload": {
                "type": "message",
                "role": "user",
                "content": [{"type": "input_text", "text": f"prompt\n{self.marker}"}],
            },
        }
        reply_record = {
            "timestamp": "2026-08-24T12:00:00Z",
            "type": "response_item",
            "payload": {
                "type": "message",
                "content": [
                    {"type": "output_text", "text": f"Findings.\n{self.marker}"}
                ],
            },
        }
        fresh.write_text(
            json.dumps(prompt_record) + "\n" + json.dumps(reply_record) + "\n",
            encoding="utf-8",
        )

        output = io.StringIO()
        with (
            self.waiting_reviewer(),
            mock.patch.object(
                reviewer.session,
                "_wait_for_transcript_path",
                return_value=str(fresh),
            ),
            redirect_stdout(output),
        ):
            reviewer.finish_review(SimpleNamespace(run_file=str(self.run_file)))

        self.assertIn("REVIEW OUTCOME: complete", output.getvalue())
        state = reviewer.load_review(self.run_file)
        self.assertEqual(state["reviewer"]["transcript"], str(fresh))
        self.assertEqual(state["status"], "review-returned")

    def test_double_submission_is_rejected(self):
        self.create_run()
        with mock.patch.object(reviewer.session, "submit_to_agent"):
            self.submit()
        with self.assertRaisesRegex(SystemExit, "already active"):
            reviewer.submit_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

    def test_failed_submission_stays_pending_until_reconciled(self):
        self.create_run()
        with (
            self.waiting_reviewer(),
            self.matching_transcript(),
            mock.patch.object(
                reviewer.session,
                "submit_to_agent",
                side_effect=reviewer.EmacsClientError("ambiguous failure"),
            ),
            self.assertRaisesRegex(reviewer.EmacsClientError, "ambiguous failure"),
        ):
            reviewer.submit_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

        state = reviewer.load_review(self.run_file)
        self.assertIsNotNone(state["pending_submission"])
        with self.assertRaisesRegex(SystemExit, "requires reconciliation"):
            reviewer.submit_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )
        with self.assertRaisesRegex(SystemExit, "requires reconciliation"):
            reviewer.finish_review(SimpleNamespace(run_file=str(self.run_file)))

        with redirect_stdout(io.StringIO()):
            reviewer.reconcile_submission(
                SimpleNamespace(run_file=str(self.run_file), delivered=False)
            )
        state = reviewer.load_review(self.run_file)
        self.assertIsNone(state["pending_submission"])
        self.assertEqual(state["status"], "ready")

    def test_reconcile_delivered_activates_the_review(self):
        self.create_run()
        state = reviewer.load_review(self.run_file)
        state["pending_submission"] = {"transcript_offset": 0}
        reviewer.save_review(self.run_file, state)
        with redirect_stdout(io.StringIO()):
            reviewer.reconcile_submission(
                SimpleNamespace(run_file=str(self.run_file), delivered=True)
            )
        state = reviewer.load_review(self.run_file)
        self.assertEqual(state["status"], "review-active")
        self.assertEqual(state["submission"], {"transcript_offset": 0})

    def test_reconcile_not_delivered_fails_closed_on_advanced_transcript(self):
        self.create_run()
        state = reviewer.load_review(self.run_file)
        state["pending_submission"] = {"transcript_offset": 0}
        reviewer.save_review(self.run_file, state)
        self.transcript.write_text("delivered prompt record\n", encoding="utf-8")

        with self.assertRaisesRegex(SystemExit, "advanced past the recorded boundary"):
            reviewer.reconcile_submission(
                SimpleNamespace(run_file=str(self.run_file), delivered=False)
            )
        state = reviewer.load_review(self.run_file)
        self.assertIsNotNone(state["pending_submission"])

    def test_restart_burns_the_single_flag_before_external_contact(self):
        self.create_run()
        with mock.patch.object(reviewer.session, "submit_to_agent"):
            self.submit()

        with (
            self.waiting_reviewer(),
            mock.patch.object(
                reviewer.session, "agent_transcript_path", return_value=None
            ),
            mock.patch.object(
                reviewer.session,
                "submit_to_agent",
                side_effect=RuntimeError("crash before save"),
            ),
            self.assertRaisesRegex(RuntimeError, "crash before save"),
        ):
            reviewer.restart_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

        state = reviewer.load_review(self.run_file)
        self.assertTrue(state["restart_used"])
        with self.assertRaisesRegex(SystemExit, "restart was already used"):
            reviewer.restart_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

    def test_retry_delivery_requires_a_pending_submission(self):
        self.create_run()
        with (
            mock.patch.object(reviewer.session, "send_return_to_agent") as send_return,
            self.assertRaisesRegex(SystemExit, "only a pending submission"),
        ):
            reviewer.retry_delivery(SimpleNamespace(run_file=str(self.run_file)))
        send_return.assert_not_called()

    def test_finish_review_requires_return_and_marker_completion(self):
        self.create_run()
        with mock.patch.object(reviewer.session, "submit_to_agent"):
            self.submit()

        with (
            self.waiting_reviewer(),
            mock.patch.object(
                reviewer.session, "_wait_for_transcript_path", return_value=None
            ),
            self.assertRaisesRegex(SystemExit, "no returned assistant message"),
        ):
            reviewer.finish_review(SimpleNamespace(run_file=str(self.run_file)))

        with (
            mock.patch.object(
                reviewer.session, "buffer_state", return_value={"state": "busy"}
            ),
            mock.patch.object(
                reviewer.session, "_wait_for_transcript_path", return_value=None
            ),
            self.assertRaisesRegex(SystemExit, "reviewer is busy"),
        ):
            reviewer.finish_review(SimpleNamespace(run_file=str(self.run_file)))

        self.write_reviewer_return(f"1. High — finding.\n{self.marker}")
        output = io.StringIO()
        with self.waiting_reviewer(), redirect_stdout(output):
            reviewer.finish_review(SimpleNamespace(run_file=str(self.run_file)))

        self.assertIn("REVIEW OUTCOME: complete", output.getvalue())
        self.assertIn("High — finding", output.getvalue())
        state = reviewer.load_review(self.run_file)
        self.assertEqual(state["status"], "review-returned")
        self.assertEqual(state["return_evidence"]["kind"], "marker")

    def test_markerless_return_is_terminal_and_blocks_recontact(self):
        self.create_run()
        with mock.patch.object(reviewer.session, "submit_to_agent"):
            self.submit()
        self.write_reviewer_return("I stopped early without finishing.")

        output = io.StringIO()
        with self.waiting_reviewer(), redirect_stdout(output):
            reviewer.finish_review(SimpleNamespace(run_file=str(self.run_file)))

        self.assertIn("REVIEW OUTCOME: terminal-incomplete", output.getvalue())
        state = reviewer.load_review(self.run_file)
        self.assertEqual(state["status"], "review-incomplete")
        self.assertEqual(state["return_evidence"]["kind"], "markerless")

        with self.assertRaisesRegex(SystemExit, "never re-contacts"):
            reviewer.submit_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )
        with self.assertRaisesRegex(SystemExit, "only an active review"):
            reviewer.restart_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

    def test_completed_review_blocks_further_contact(self):
        self.create_run()
        with mock.patch.object(reviewer.session, "submit_to_agent"):
            self.submit()
        self.write_reviewer_return(f"Findings.\n{self.marker}")
        with self.waiting_reviewer(), redirect_stdout(io.StringIO()):
            reviewer.finish_review(SimpleNamespace(run_file=str(self.run_file)))

        with self.assertRaisesRegex(SystemExit, "never re-contacts"):
            reviewer.submit_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

    def test_restart_refuses_after_any_reviewer_output(self):
        self.create_run()
        with mock.patch.object(reviewer.session, "submit_to_agent"):
            self.submit()
        self.write_reviewer_return("partial output")

        with self.assertRaisesRegex(SystemExit, "already returned assistant output"):
            reviewer.restart_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

    def test_restart_runs_once_on_zero_output_process_loss(self):
        self.create_run()
        with mock.patch.object(reviewer.session, "submit_to_agent"):
            self.submit()

        fresh = self.directory / "fresh-reviewer.jsonl"
        with (
            self.waiting_reviewer(),
            mock.patch.object(
                reviewer.session,
                "agent_transcript_path",
                side_effect=(None, str(fresh)),
            ),
            mock.patch.object(reviewer.session, "submit_to_agent") as submit,
            redirect_stdout(io.StringIO()),
        ):
            reviewer.restart_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

        buffer, backend, prompt = submit.call_args.args
        self.assertEqual((buffer, backend), ("*codex:review*", "codex"))
        self.assertTrue(prompt.endswith(self.marker))
        state = reviewer.load_review(self.run_file)
        self.assertTrue(state["restart_used"])
        self.assertEqual(state["reviewer"]["transcript"], str(fresh))
        self.assertEqual(state["submission"], {"transcript_offset": 0})

        with self.assertRaisesRegex(SystemExit, "restart was already used"):
            reviewer.restart_review(
                SimpleNamespace(run_file=str(self.run_file), context_file=None)
            )

    def test_status_reports_run_and_reviewer(self):
        self.create_run()
        output = io.StringIO()
        with self.waiting_reviewer(), redirect_stdout(output):
            reviewer.status_cmd(
                SimpleNamespace(run_file=str(self.run_file), json=False)
            )
        self.assertIn("status=ready", output.getvalue())
        self.assertIn("awaiting-input", output.getvalue())


if __name__ == "__main__":
    unittest.main()
