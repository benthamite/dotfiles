import argparse
import hashlib
import importlib.util
import io
import json
import re
import tempfile
import unittest
import warnings
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path
from types import SimpleNamespace
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
CODEX_SCRIPT = (
    ROOT / "codex/skills/orchestrate-review/scripts/orchestrate_review.py"
)
CLAUDE_SCRIPT = (
    ROOT / "claude/skills/orchestrate-review/scripts/orchestrate_review.py"
)
CODEX_SKILL = ROOT / "codex/skills/orchestrate-review/SKILL.md"
CLAUDE_SKILL = ROOT / "claude/skills/orchestrate-review/SKILL.md"


class SkillDiscoveryTests(unittest.TestCase):
    def test_skill_is_registered_in_manifest_and_generated_inventory(self):
        manifest = json.loads((ROOT / "ai-config-sync.json").read_text())
        paired = {
            entry["name"] for entry in manifest["skills"]
            if entry.get("status") == "paired"
        }
        self.assertIn("orchestrate-review", paired)

        inventory = (ROOT / "agents/skill-inventory.org").read_text()
        self.assertIn("=orchestrate-review=", inventory)

    def test_openai_metadata_invokes_public_skill(self):
        for side in ("claude", "codex"):
            metadata = (
                ROOT
                / side
                / "skills/orchestrate-review/agents/openai.yaml"
            ).read_text()
            with self.subTest(side=side):
                self.assertIn("$orchestrate-review", metadata)


def load_module():
    spec = importlib.util.spec_from_file_location(
        "orchestrate_review", CODEX_SCRIPT
    )
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


orchestrator = load_module()


class SkillWorkflowTests(unittest.TestCase):
    def test_paired_helpers_compile_without_syntax_warnings(self):
        for script in (CODEX_SCRIPT, CLAUDE_SCRIPT):
            with self.subTest(script=script), warnings.catch_warnings():
                warnings.simplefilter("error", SyntaxWarning)
                compile(script.read_bytes(), str(script), "exec")

    def test_paired_skills_define_one_way_spec_plan_implementation_handoff(self):
        self.assertEqual(CODEX_SKILL.read_bytes(), CLAUDE_SKILL.read_bytes())
        skill = CODEX_SKILL.read_text(encoding="utf-8")
        normalized_skill = " ".join(skill.split())

        required_rules = (
            "Agent 1 creates the spec",
            "Agent 2 reviews the spec once",
            "Agent 1 creates the plan, incorporating the spec-review feedback",
            "Agent 2 reviews the plan once",
            "Agent 1 implements the plan, incorporating the plan-review feedback",
            "Do not send the artifact back for another review pass",
            "Agent 1 defaults to Claude/Fable and Agent 2 defaults to Codex",
            "swaps the entire role bundle",
            "delegate top-level stages sequentially, one stage per fresh Agent 1 session",
            "Internal plan tasks stay inside their stage's single session",
            "The entire stage is the smallest orchestration unit",
            "Never report progress as `Task N`",
            "Never inspect or steer Agent 1's internal tasks",
            "Never run independent acceptance gates at internal task boundaries",
            "Send exactly one initial implementation handoff",
            "Intermediate narration and tool activity are not returns",
            "Agent 1 owns implementation and stage-final verification",
            "diagnose the specific reason for the stop",
            "send one targeted steering message",
            "Never send a generic continuation prompt",
            "Never repeat a steering message",
        )
        for rule in required_rules:
            with self.subTest(rule=rule):
                self.assertIn(rule, normalized_skill)

        forbidden_rules = (
            "Repeat until convergence",
            "revises until approval",
            "planner/reviewer passes",
            "status: stage-4-implementation-tasks-4-8",
            "resume-stage",
            "A premature implementation return ends that run",
            "Run one independent stage-final acceptance pass",
        )
        for rule in forbidden_rules:
            with self.subTest(rule=rule):
                self.assertNotIn(rule, normalized_skill)


class StageAtomicRunTests(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary_directory.cleanup)
        self.directory = Path(self.temporary_directory.name)
        self.run_file = self.directory / "stage-run.json"
        self.prompt_file = self.directory / "prompt.txt"
        self.prompt_file.write_text("Phase context", encoding="utf-8")
        self.evidence_file = self.directory / "evidence.txt"
        self.evidence_file.write_text("Whole-phase completion evidence", encoding="utf-8")
        self.agent1_transcript = self.directory / "agent1.jsonl"
        self.agent2_transcript = self.directory / "agent2.jsonl"
        self.agent1_transcript.touch()
        self.agent2_transcript.touch()
        self.identities = {
            "*claude:stage-2*": {"backend": "claude-code", "session_id": "author-session",
                                  "transcript": str(self.agent1_transcript.resolve())},
            "*codex:stage-2*": {"backend": "codex", "session_id": "reviewer-session",
                                 "transcript": str(self.agent2_transcript.resolve())},
        }
        self.real_buffer_state = orchestrator.session.buffer_state
        # Every runtime call in this class is synthetic; an unmocked RPC fails.
        for patcher in (
            mock.patch.object(orchestrator.session, "run_emacs_eval",
                              side_effect=AssertionError("unexpected live Emacs RPC")),
            mock.patch.object(orchestrator.session, "buffer_state",
                              return_value={"state": "awaiting-input"}),
            mock.patch.object(orchestrator.session, "actor_identity", side_effect=self.actor_identity),
        ):
            patcher.start()
            self.addCleanup(patcher.stop)

    def actor_identity(self, buffer):
        return {"buffer": buffer, "directory": str(Path("/tmp/example-repo").resolve()),
                **self.identities[buffer],
                "state": orchestrator.session.buffer_state(buffer)["state"]}

    def append_record(self, path, record):
        with Path(path).open("a", encoding="utf-8") as stream:
            stream.write(json.dumps(record) + "\n")

    def append_user(self, path, prompt):
        self.append_record(path, {"type": "user", "message": {"role": "user", "content": prompt}})

    def init_args(self, **overrides):
        values = {
            "run_file": str(self.run_file),
            "repo": "/tmp/example-repo",
            "stage": "2",
            "agent1_buffer": "*claude:stage-2*",
            "agent1_backend": "claude-code",
            "agent1_transcript": str(self.agent1_transcript),
            "agent2_buffer": "*codex:stage-2*",
            "agent2_backend": "codex",
            "agent2_transcript": str(self.agent2_transcript),
            "adopt_implementation": False,
            "spec_commit": None,
            "plan_commit": None,
            "reviews_complete": False,
        }
        values.update(overrides)
        return argparse.Namespace(**values)

    def submit_args(self, phase):
        return argparse.Namespace(
            run_file=str(self.run_file),
            phase=phase,
            prompt_file=str(self.prompt_file),
        )

    def create_run(self, **overrides):
        with redirect_stdout(io.StringIO()):
            orchestrator.create_run(self.init_args(**overrides))
        return orchestrator.load_run(self.run_file)

    def test_claude_alias_is_rejected(self):
        with self.assertRaisesRegex(SystemExit, "invalid Agent 1 backend"):
            self.create_run(agent1_backend="claude")

    def test_codex_pending_prompt_uses_public_prompt_accessor(self):
        captured = {}

        def evaluate(expr):
            captured["expr"] = expr
            return "present"

        with mock.patch.object(orchestrator.session, "run_emacs_eval", side_effect=evaluate):
            orchestrator.session.pending_prompt_contains(
                "*codex:stage-2*", "codex", "marker"
            )

        self.assertIn("codex-prompt-input", captured["expr"])
        self.assertNotIn("re-search-backward", captured["expr"])

    def test_claude_transcript_path_uses_status_record(self):
        with mock.patch.object(
            orchestrator.session,
            "run_emacs_eval",
            return_value="/tmp/fresh-claude.jsonl",
        ) as evaluate:
            try:
                result = orchestrator.session.agent_transcript_path(
                    "*claude:stage-2*", "claude-code"
                )
            except SystemExit as error:
                self.fail(f"canonical Claude backend was rejected: {error}")

        self.assertEqual(result, "/tmp/fresh-claude.jsonl")
        self.assertIn("agent-claude--parse-status-file", evaluate.call_args.args[0])

    def submit_phase(self, phase):
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            redirect_stdout(io.StringIO()),
        ):
            orchestrator.submit(self.submit_args(phase))
        state = orchestrator.load_run(self.run_file)
        attempt = state["attempts"][-1]
        prompt = attempt["receipt"] + "\n\n" + orchestrator._phase_prompt(
            state, phase, self.prompt_file.read_text(), self.run_file)
        self.append_user(state[attempt["actor"]]["transcript"], prompt)

    def finish_args(self, phase):
        return argparse.Namespace(run_file=str(self.run_file), phase=phase)

    def write_phase_return(self, phase, marker=True):
        actor = orchestrator.PHASE_ACTOR[phase]
        transcript = (
            self.agent1_transcript if actor == "agent1" else self.agent2_transcript
        )
        if phase == "implementation":
            final_line = "STAGE COMPLETE: 2"
        else:
            final_line = f"PHASE COMPLETE: {phase}"
        text = "Returned phase evidence"
        if marker:
            text += f"\n{final_line}"
        record = {
            "timestamp": "2026-08-01T12:00:00Z",
            "message": {"role": "assistant", "content": text},
        }
        with transcript.open("a", encoding="utf-8") as stream:
            stream.write(json.dumps(record) + "\n")

    def finish_phase(self, phase):
        self.write_phase_return(phase)
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            redirect_stdout(io.StringIO()),
        ):
            orchestrator.finish_phase(self.finish_args(phase))

    def test_new_run_is_private_and_starts_with_spec(self):
        state = self.create_run()

        self.assertEqual(self.run_file.stat().st_mode & 0o777, 0o600)
        self.assertEqual(state["stage"], "2")
        self.assertEqual(state["expected_phase"], "spec")
        self.assertEqual(state["status"], "ready")
        self.assertEqual(state["submissions"], [])
        self.assertEqual(state["completions"], [])
        self.assertIsNone(state["pending_submission"])
        self.assertNotIn("resume_count", state)
        self.assertNotIn("latest_resume_offset", state)

    def test_run_requires_both_top_level_transcripts(self):
        with self.assertRaisesRegex(SystemExit, "Agent 1 transcript is missing"):
            self.create_run(agent1_transcript=None)
        with self.assertRaisesRegex(SystemExit, "Agent 2 transcript is missing"):
            self.create_run(agent2_transcript=None)

    def test_phase_sequence_routes_fixed_role_bundles_once(self):
        self.create_run()
        phases = (
            ("spec", "*claude:stage-2*", "claude-code"),
            ("spec-review", "*codex:stage-2*", "codex"),
            ("plan", "*claude:stage-2*", "claude-code"),
            ("plan-review", "*codex:stage-2*", "codex"),
            ("implementation", "*claude:stage-2*", "claude-code"),
        )

        with mock.patch.object(orchestrator.session, "submit_to_agent") as submit:
            for phase, buffer, backend in phases:
                with self.subTest(phase=phase), redirect_stdout(io.StringIO()):
                    self.submit_phase(phase)
                    sent_buffer, sent_backend, sent_prompt = submit.call_args.args
                    self.assertEqual(sent_buffer, buffer)
                    self.assertEqual(sent_backend, backend)
                    self.assertEqual(
                        submit.call_args.kwargs["one_pass"],
                        phase == "implementation",
                    )
                    if phase == "implementation":
                        self.assertIn("complete all of Stage 2", sent_prompt)
                        self.assertIn(
                            "Internal plan tasks are not orchestration checkpoints",
                            sent_prompt,
                        )
                        self.assertIn("cannot narrow this contract", sent_prompt)
                        self.assertIn("Phase context", sent_prompt)
                        self.assertTrue(sent_prompt.endswith("STAGE COMPLETE: 2"))
                    else:
                        self.assertTrue(
                            sent_prompt.endswith(f"PHASE COMPLETE: {phase}")
                        )
                    if phase != "implementation":
                        self.finish_phase(phase)

        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["status"], "implementation-active")
        self.assertIsNone(state["expected_phase"])
        self.assertEqual(
            [submission["phase"] for submission in state["submissions"]],
            [phase for phase, _buffer, _backend in phases],
        )
        self.assertEqual(
            [completion["phase"] for completion in state["completions"]],
            list(orchestrator.PHASES[:-1]),
        )

    def test_next_phase_is_blocked_until_previous_actor_returns_with_evidence(self):
        self.create_run()
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec")

        with self.assertRaisesRegex(SystemExit, "spec phase is still active"):
            orchestrator.submit(self.submit_args("spec-review"))

        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "busy"},
            ),
            self.assertRaisesRegex(SystemExit, "Agent 1 is busy"),
        ):
            orchestrator.finish_phase(self.finish_args("spec"))

        self.write_phase_return("spec", marker=False)
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            self.assertRaisesRegex(SystemExit, "completion marker is missing"),
        ):
            orchestrator.finish_phase(self.finish_args("spec"))

    def test_out_of_order_phase_fails_before_contacting_emacs(self):
        self.create_run()

        with (
            mock.patch.object(orchestrator.session, "submit_to_agent") as submit,
            self.assertRaisesRegex(SystemExit, "expected phase is spec"),
        ):
            orchestrator.submit(self.submit_args("plan"))

        submit.assert_not_called()

    def test_arbitrary_submission_is_impossible_after_implementation_starts(self):
        self.create_run()
        with (
            mock.patch.object(orchestrator.session, "submit_to_agent"),
            redirect_stdout(io.StringIO()),
        ):
            for phase in orchestrator.PHASES:
                self.submit_phase(phase)
                if phase != "implementation":
                    self.finish_phase(phase)

        with (
            mock.patch.object(orchestrator.session, "submit_to_agent") as submit,
            self.assertRaisesRegex(
                SystemExit,
                "implementation is active; leave Agent 1 alone",
            ),
        ):
            orchestrator.submit(self.submit_args("implementation"))

        submit.assert_not_called()

    def test_existing_implementation_requires_review_and_commit_evidence(self):
        with self.assertRaisesRegex(
            SystemExit, "adoption requires spec commit, plan commit, and both reviews"
        ):
            self.create_run(adopt_implementation=True)

        state = self.create_run(
            adopt_implementation=True,
            spec_commit="abc123",
            plan_commit="def456",
            reviews_complete=True,
        )
        self.assertEqual(state["expected_phase"], "implementation")
        self.assertEqual(state["adopted_evidence"]["spec_commit"], "abc123")
        self.assertEqual(state["adopted_evidence"]["plan_commit"], "def456")

    def start_implementation(self):
        self.create_run(
            adopt_implementation=True,
            spec_commit="abc123",
            plan_commit="def456",
            reviews_complete=True,
        )
        with (
            mock.patch.object(orchestrator.session, "submit_to_agent"),
            redirect_stdout(io.StringIO()),
        ):
            self.submit_phase("implementation")

    def return_implementation(self):
        self.finish_phase("implementation")

    def test_implementation_continuation_commands_are_not_public(self):
        for command in ("resume-stage", "switch-model", "ask", "interrupt"):
            with (
                self.subTest(command=command),
                redirect_stderr(io.StringIO()),
                self.assertRaises(SystemExit) as raised,
            ):
                orchestrator.main([command, "--run-file", str(self.run_file)])
            self.assertEqual(raised.exception.code, 2)

    def test_premature_implementation_return_allows_targeted_steering(self):
        self.start_implementation()
        record = {
            "timestamp": "2026-08-02T12:00:00Z",
            "message": {
                "role": "assistant",
                "content": "I am stopping at an internal checkpoint.",
            },
        }
        with self.agent1_transcript.open("a", encoding="utf-8") as stream:
            stream.write(json.dumps(record) + "\n")

        output = io.StringIO()
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            redirect_stdout(output),
        ):
            orchestrator.stage_return(SimpleNamespace(run_file=str(self.run_file)))

        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["status"], "implementation-stopped")
        self.assertIsNone(state["active_phase"])
        self.assertEqual(len(state["stop_evidence"]["sha256"]), 64)
        self.assertEqual(state["stop_evidence"]["kind"], "agent-return")
        original_stop_sha = state["stop_evidence"]["sha256"]
        self.assertIn("internal checkpoint", output.getvalue())

        with self.assertRaisesRegex(SystemExit, "use steer-stage"):
            orchestrator.submit(self.submit_args("implementation"))

        steering = self.directory / "steering.txt"
        steering.write_text(
            "Obstacle: You stopped at an internal checkpoint before the stage was complete.\n"
            "Resolution: No user decision is needed; use the existing plan and repository evidence.\n"
            "Whole-stage direction: Return to the whole Stage 2 outcome and complete its verification.",
            encoding="utf-8",
        )
        steering.chmod(0o600)
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            mock.patch.object(orchestrator.session, "submit_to_agent") as submit,
            redirect_stdout(io.StringIO()),
        ):
            orchestrator.steer_stage(
                SimpleNamespace(
                    run_file=str(self.run_file), prompt_file=str(steering)
                )
            )

        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["status"], "implementation-active")
        self.assertEqual(state["active_phase"], "implementation")
        self.assertIsNone(state["stop_evidence"])
        self.assertEqual(len(state["steering_prompts"]), 1)
        submitted_prompt = submit.call_args.args[2]
        self.assertIn("internal checkpoint", submitted_prompt)
        self.assertIn("whole Stage 2", submitted_prompt)

        state["status"] = "implementation-stopped"
        state["active_phase"] = None
        state["stop_evidence"] = {
            "kind": "agent-return",
            "sha256": original_stop_sha,
        }
        orchestrator.save_run(self.run_file, state)
        with self.assertRaisesRegex(SystemExit, "already received"):
            orchestrator.steer_stage(
                SimpleNamespace(
                    run_file=str(self.run_file), prompt_file=str(steering)
                )
            )

    def test_generic_or_repeated_stage_steering_is_rejected(self):
        self.start_implementation()
        record = {
            "timestamp": "2026-08-02T12:00:00Z",
            "message": {"role": "assistant", "content": "Checkpoint."},
        }
        self.append_record(self.agent1_transcript, record)
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            redirect_stdout(io.StringIO()),
        ):
            orchestrator.stage_return(SimpleNamespace(run_file=str(self.run_file)))

        generic = self.directory / "generic.txt"
        generic.write_text(
            "Please continue working on the whole stage and complete all remaining "
            "work and verification without stopping.",
            encoding="utf-8",
        )
        generic.chmod(0o600)
        with self.assertRaisesRegex(SystemExit, "Obstacle, Resolution"):
            orchestrator.steer_stage(
                SimpleNamespace(run_file=str(self.run_file), prompt_file=str(generic))
            )

    def test_ambiguous_steering_attempt_cannot_contact_agent_twice(self):
        self.start_implementation()
        record = {
            "timestamp": "2026-08-02T12:00:00Z",
            "message": {"role": "assistant", "content": "Capture failed."},
        }
        self.append_record(self.agent1_transcript, record)
        with (
            mock.patch.object(
                orchestrator.session, "buffer_state", return_value={"state": "awaiting-input"}
            ),
            redirect_stdout(io.StringIO()),
        ):
            orchestrator.stage_return(SimpleNamespace(run_file=str(self.run_file)))

        steering = self.directory / "steering-ambiguous.txt"
        steering.write_text(
            "Obstacle: The requested capture failed for a recoverable technical reason.\n"
            "Resolution: Use the alternate capture path already documented in the repository.\n"
            "Whole-stage direction: Continue ownership of the entire stage and its final verification.",
            encoding="utf-8",
        )
        steering.chmod(0o600)
        args = SimpleNamespace(run_file=str(self.run_file), prompt_file=str(steering))
        with (
            mock.patch.object(
                orchestrator.session, "buffer_state", return_value={"state": "awaiting-input"}
            ),
            mock.patch.object(
                orchestrator.session, "submit_to_agent", side_effect=RuntimeError("ambiguous")
            ) as submit,
            self.assertRaisesRegex(RuntimeError, "ambiguous"),
        ):
            orchestrator.steer_stage(args)
        with (
            mock.patch.object(orchestrator.session, "submit_to_agent") as second_submit,
            self.assertRaisesRegex(SystemExit, "pending submission requires reconciliation"),
        ):
            orchestrator.steer_stage(args)
        self.assertEqual(submit.call_count, 1)
        second_submit.assert_not_called()
        with redirect_stdout(io.StringIO()):
            orchestrator.reconcile_submission(SimpleNamespace(run_file=str(self.run_file), delivered=False))
        state = orchestrator.load_run(self.run_file)
        self.assertIsNone(state["pending_submission"])
        self.assertEqual(state["status"], "implementation-stopped")
        self.assertFalse(state["attempts"][-1]["acknowledged"])
        with mock.patch.object(orchestrator.session, "submit_to_agent") as third_submit:
            with self.assertRaisesRegex(SystemExit, "already received a steering attempt"):
                orchestrator.steer_stage(args)
            third_submit.assert_not_called()

    def test_new_stage_rejects_reused_agent1_transcript(self):
        self.agent1_transcript.write_text("prior stage", encoding="utf-8")
        with self.assertRaisesRegex(SystemExit, "fresh Agent 1 transcript"):
            self.create_run()

    def test_finish_implementation_rejects_busy_state_even_with_marker(self):
        self.start_implementation()
        self.write_phase_return("implementation")

        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "busy"},
            ),
            self.assertRaisesRegex(SystemExit, "Agent 1 is busy"),
        ):
            orchestrator.finish_phase(self.finish_args("implementation"))

        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["status"], "implementation-active")
        self.assertEqual(state["completions"][-1]["phase"], "plan-review")

    def test_finish_implementation_keeps_busy_state_without_current_marker(self):
        self.start_implementation()

        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "busy"},
            ),
            self.assertRaisesRegex(SystemExit, "Agent 1 is busy"),
        ):
            orchestrator.finish_phase(self.finish_args("implementation"))

    def test_complete_stage_requires_return_and_acceptance_evidence(self):
        self.start_implementation()
        args = SimpleNamespace(
            run_file=str(self.run_file),
            evidence_file=str(self.evidence_file),
        )

        with self.assertRaisesRegex(SystemExit, "implementation has not returned"):
            orchestrator.complete_stage(args)

        self.return_implementation()
        self.evidence_file.write_text("", encoding="utf-8")
        with self.assertRaisesRegex(SystemExit, "evidence file is empty"):
            orchestrator.complete_stage(args)

        self.evidence_file.write_text("Stage-wide acceptance passed", encoding="utf-8")
        with redirect_stdout(io.StringIO()):
            orchestrator.complete_stage(args)

        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["status"], "complete")
        self.assertIsNotNone(state["acceptance_evidence"])
        with self.assertRaisesRegex(SystemExit, "run is already complete"):
            orchestrator.submit(self.submit_args("implementation"))

    def test_failed_external_submission_stays_pending_until_reconciled(self):
        self.create_run()
        with (
            mock.patch.object(
                orchestrator.session,
                "submit_to_agent",
                side_effect=orchestrator.EmacsClientError("ambiguous failure"),
            ),
            self.assertRaisesRegex(orchestrator.EmacsClientError, "ambiguous failure"),
        ):
            self.submit_phase("spec")

        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["pending_submission"]["phase"], "spec")
        with self.assertRaisesRegex(SystemExit, "pending submission requires reconciliation"):
            orchestrator.submit(self.submit_args("spec"))

        args = SimpleNamespace(run_file=str(self.run_file), delivered=False)
        with redirect_stdout(io.StringIO()):
            orchestrator.reconcile_submission(args)
        self.assertIsNone(orchestrator.load_run(self.run_file)["pending_submission"])

    def test_submit_retries_only_return_when_prompt_remains_in_composer(self):
        self.create_run(
            agent1_buffer="*codex:stage-2*", agent1_backend="codex",
            agent1_transcript=str(self.agent2_transcript),
            agent2_buffer="*claude:stage-2*", agent2_backend="claude-code",
            agent2_transcript=str(self.agent1_transcript))
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            mock.patch.object(orchestrator.session, "run_emacs_eval", return_value="submitted"),
            mock.patch.object(
                orchestrator.session,
                "_wait_for_delivery",
                side_effect=(False, True),
            ) as wait_for_delivery,
            mock.patch.object(
                orchestrator.session,
                "pending_prompt_contains",
                return_value=True,
            ),
            mock.patch.object(
                orchestrator.session,
                "send_return_to_agent",
            ) as send_return,
            redirect_stdout(io.StringIO()),
        ):
            orchestrator.submit(self.submit_args("spec"))

        self.assertEqual(send_return.call_args.args, ("*codex:stage-2*", "codex"))
        self.assertEqual(send_return.call_args.kwargs["expected_identity"]["state"], "awaiting-input")
        self.assertEqual(wait_for_delivery.call_count, 2)
        state = orchestrator.load_run(self.run_file)
        self.assertIsNone(state["pending_submission"])
        self.assertEqual(state["status"], "phase-active")

    def test_submission_uses_agent_lifecycle_dispatch(self):
        captured = {}

        def submit_through_emacs(expr):
            captured["expr"] = expr
            return "submitted"

        with (
            mock.patch.object(
                orchestrator.session,
                "run_emacs_eval",
                side_effect=submit_through_emacs,
            ),
            mock.patch.object(
                orchestrator.session,
                "_wait_for_delivery",
                return_value=True,
            ),
        ):
            orchestrator.session.submit_to_agent(
                "*claude:stage-2*",
                "claude-code",
                "whole phase",
                transcript=str(self.agent1_transcript),
                transcript_offset=0,
                delivery_marker="PHASE COMPLETE: spec",
            )

        self.assertIn("(agent-submit", captured["expr"])
        self.assertNotIn("agent-claude-submit-command", captured["expr"])

    def test_submit_keeps_pending_when_return_retry_is_not_acknowledged(self):
        self.create_run(
            agent1_buffer="*codex:stage-2*", agent1_backend="codex",
            agent1_transcript=str(self.agent2_transcript),
            agent2_buffer="*claude:stage-2*", agent2_backend="claude-code",
            agent2_transcript=str(self.agent1_transcript))
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            mock.patch.object(orchestrator.session, "run_emacs_eval", return_value="submitted"),
            mock.patch.object(
                orchestrator.session,
                "_wait_for_delivery",
                side_effect=(False, False),
            ),
            mock.patch.object(
                orchestrator.session,
                "pending_prompt_contains",
                return_value=True,
            ),
            mock.patch.object(
                orchestrator.session,
                "send_return_to_agent",
            ),
            self.assertRaisesRegex(
                orchestrator.EmacsClientError,
                "delivery was not acknowledged",
            ),
        ):
            orchestrator.submit(self.submit_args("spec"))

        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["status"], "ready")
        self.assertEqual(state["pending_submission"]["phase"], "spec")

    def test_implementation_submission_never_retries_return(self):
        with (
            mock.patch.object(orchestrator.session, "run_emacs_eval", return_value="submitted"),
            mock.patch.object(orchestrator.session, "_wait_for_delivery", return_value=False),
            mock.patch.object(orchestrator.session, "pending_prompt_contains") as composer,
            mock.patch.object(orchestrator.session, "send_return_to_agent") as send_return,
            self.assertRaisesRegex(
                orchestrator.EmacsClientError,
                "implementation delivery was not independently acknowledged",
            ),
        ):
            orchestrator.session.submit_to_agent(
                "*claude:stage-2*",
                "claude-code",
                "whole stage",
                transcript=str(self.agent1_transcript),
                transcript_offset=0,
                delivery_marker="STAGE COMPLETE: 2",
                one_pass=True,
            )

        composer.assert_not_called()
        send_return.assert_not_called()

    def test_ambiguous_implementation_delivery_freezes_instead_of_resubmitting(self):
        self.create_run(
            adopt_implementation=True,
            spec_commit="abc123",
            plan_commit="def456",
            reviews_complete=True,
        )
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            mock.patch.object(
                orchestrator.session,
                "submit_to_agent",
                side_effect=orchestrator.EmacsClientError("ambiguous failure"),
            ),
            self.assertRaisesRegex(orchestrator.EmacsClientError, "ambiguous failure"),
        ):
            orchestrator.submit(self.submit_args("implementation"))

        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["pending_submission"]["phase"], "implementation")

        with redirect_stdout(io.StringIO()):
            orchestrator.reconcile_submission(
                SimpleNamespace(run_file=str(self.run_file), delivered=False)
            )

        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["status"], "implementation-stopped")
        self.assertIsNone(state["pending_submission"])
        with self.assertRaisesRegex(SystemExit, "ambiguous implementation delivery"):
            orchestrator.submit(self.submit_args("implementation"))

    def test_pending_implementation_delivery_cannot_retry_return(self):
        self.create_run(
            adopt_implementation=True,
            spec_commit="abc123",
            plan_commit="def456",
            reviews_complete=True,
        )
        state = orchestrator.load_run(self.run_file)
        orchestrator._new_attempt(state, "phase", "implementation", "context", "prompt",
                                  state["agent1"]["identity"])
        orchestrator.save_run(self.run_file, state)

        with (
            mock.patch.object(orchestrator.session, "send_return_to_agent") as send_return,
            self.assertRaisesRegex(SystemExit, "implementation delivery cannot be retried"),
        ):
            orchestrator.retry_delivery(
                SimpleNamespace(run_file=str(self.run_file))
            )

        send_return.assert_not_called()

    def test_retry_delivery_rejects_an_acknowledged_active_submission(self):
        self.create_run()
        state = orchestrator.load_run(self.run_file)
        state["submissions"].append(
            {"phase": "spec", "actor": "agent1", "transcript_offset": 0}
        )
        state["status"] = "phase-active"
        state["active_phase"] = "spec"
        state["expected_phase"] = None
        orchestrator.save_run(self.run_file, state)

        with (
            mock.patch.object(orchestrator.session, "send_return_to_agent") as send_return,
            self.assertRaisesRegex(SystemExit, "only a pending submission"),
        ):
            orchestrator.retry_delivery(
                SimpleNamespace(run_file=str(self.run_file))
            )

        send_return.assert_not_called()

    def test_restart_phase_moves_active_review_to_fresh_fixed_session(self):
        self.create_run()
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec")
        self.finish_phase("spec")
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec-review")

        fresh_transcript = self.directory / "fresh-agent2.jsonl"
        fresh_transcript.touch()
        self.identities["*codex:stage-2*"].update(
            session_id="fresh-reviewer", transcript=str(fresh_transcript.resolve()))
        args = SimpleNamespace(
            run_file=str(self.run_file),
            prompt_file=str(self.prompt_file),
        )
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            mock.patch.object(
                orchestrator.session,
                "agent_transcript_path",
                side_effect=(None, str(fresh_transcript)),
                create=True,
            ),
            mock.patch.object(orchestrator.session, "submit_to_agent") as submit,
            redirect_stdout(io.StringIO()),
        ):
            orchestrator.restart_phase(args)

        sent = submit.call_args
        self.assertEqual(sent.args[:2], ("*codex:stage-2*", "codex"))
        self.assertIn("Phase context", sent.args[2])
        self.assertEqual(sent.kwargs["transcript_offset"], 0)
        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["agent2"]["transcript"], str(fresh_transcript.resolve()))
        self.assertEqual(state["submissions"][-1]["transcript_offset"], 0)
        self.assertEqual(state["status"], "phase-active")

    def test_restart_phase_refuses_unrelated_static_marker_history(self):
        self.create_run()
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec")
        self.finish_phase("spec")
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec-review")

        fresh_transcript = self.directory / "fresh-agent2.jsonl"
        fresh_transcript.write_text(
            json.dumps(
                {
                    "type": "response_item",
                    "payload": {
                        "type": "message",
                        "role": "user",
                        "content": [
                            {
                                "type": "input_text",
                                "text": "PHASE COMPLETE: spec-review",
                            }
                        ],
                    },
                }
            )
            + "\n",
            encoding="utf-8",
        )
        self.identities["*codex:stage-2*"].update(
            session_id="fresh-reviewer", transcript=str(fresh_transcript.resolve()))
        args = SimpleNamespace(
            run_file=str(self.run_file),
            prompt_file=str(self.prompt_file),
        )
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            mock.patch.object(
                orchestrator.session,
                "agent_transcript_path",
                return_value=str(fresh_transcript),
                create=True,
            ),
            mock.patch.object(orchestrator.session, "submit_to_agent") as submit,
            redirect_stdout(io.StringIO()),
            self.assertRaisesRegex(SystemExit, "no static marker adoption"),
        ):
            orchestrator.restart_phase(args)

        submit.assert_not_called()
        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["agent2"]["transcript"], str(self.agent2_transcript.resolve()))

    def test_restart_phase_rejects_after_reviewer_returned_any_message(self):
        self.create_run()
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec")
        self.finish_phase("spec")
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec-review")
        self.write_phase_return("spec-review", marker=False)

        with self.assertRaisesRegex(SystemExit, "already returned assistant output"):
            orchestrator.restart_phase(
                SimpleNamespace(
                    run_file=str(self.run_file),
                    prompt_file=str(self.prompt_file),
                )
            )

    def test_submit_requires_fixed_destination_actor_to_be_awaiting(self):
        self.create_run()
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "busy"},
            ),
            mock.patch.object(orchestrator.session, "submit_to_agent") as submit,
            self.assertRaisesRegex(SystemExit, "Agent 1 is busy"),
        ):
            orchestrator.submit(self.submit_args("spec"))
        submit.assert_not_called()

    def test_submit_bootstraps_a_fresh_initialized_claude_session(self):
        self.create_run()
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                side_effect=(
                    {"state": "unknown"},
                    {"state": "awaiting-input"},
                    {"state": "awaiting-input"},
                ),
            ),
            mock.patch.object(
                orchestrator.session,
                "agent1_process_live",
                return_value=True,
            ),
            mock.patch.object(
                orchestrator.session,
                "claude_session_initialized",
                return_value=True,
                create=True,
            ),
            mock.patch.object(
                orchestrator.session,
                "reconcile_agent1_waiting",
            ) as reconcile,
            mock.patch.object(orchestrator.session, "submit_to_agent") as submit,
            redirect_stdout(io.StringIO()),
        ):
            orchestrator.submit(self.submit_args("spec"))

        reconcile.assert_called_once_with("*claude:stage-2*")
        submit.assert_called_once()

    def test_submit_does_not_bootstrap_unknown_claude_with_existing_history(self):
        self.create_run()
        self.agent1_transcript.write_text("existing turn\n", encoding="utf-8")
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "unknown"},
            ),
            mock.patch.object(
                orchestrator.session,
                "agent1_process_live",
            ) as process_live,
            mock.patch.object(
                orchestrator.session,
                "claude_session_initialized",
                create=True,
            ) as initialized,
            mock.patch.object(
                orchestrator.session,
                "reconcile_agent1_waiting",
            ) as reconcile,
            mock.patch.object(orchestrator.session, "submit_to_agent") as submit,
            self.assertRaisesRegex(SystemExit, "Agent 1 is unknown"),
        ):
            orchestrator.submit(self.submit_args("spec"))

        process_live.assert_not_called()
        initialized.assert_not_called()
        reconcile.assert_not_called()
        submit.assert_not_called()

    def test_submit_does_not_bootstrap_uninitialized_claude_process(self):
        self.create_run()
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "unknown"},
            ),
            mock.patch.object(
                orchestrator.session,
                "agent1_process_live",
                return_value=True,
            ),
            mock.patch.object(
                orchestrator.session,
                "claude_session_initialized",
                return_value=False,
                create=True,
            ),
            mock.patch.object(
                orchestrator.session,
                "reconcile_agent1_waiting",
            ) as reconcile,
            mock.patch.object(orchestrator.session, "submit_to_agent") as submit,
            self.assertRaisesRegex(SystemExit, "Agent 1 is unknown"),
        ):
            orchestrator.submit(self.submit_args("spec"))

        reconcile.assert_not_called()
        submit.assert_not_called()

    def test_stale_completion_marker_cannot_finish_new_submission(self):
        self.create_run()
        self.write_phase_return("spec")
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec")

        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            self.assertRaisesRegex(SystemExit, "after current submission"),
        ):
            orchestrator.finish_phase(self.finish_args("spec"))

        self.write_phase_return("spec")
        self.finish_phase("spec")

    def test_malformed_state_machine_prefix_is_rejected(self):
        self.create_run()
        state = json.loads(self.run_file.read_text(encoding="utf-8"))
        state["submissions"] = [
            {"phase": "plan", "actor": "agent1", "transcript_offset": 0}
        ]
        self.run_file.write_text(json.dumps(state), encoding="utf-8")
        self.run_file.chmod(0o600)

        with self.assertRaisesRegex(SystemExit, "submissions are not an exact phase prefix"):
            orchestrator.load_run(self.run_file)

    def test_implementation_status_suppresses_task_level_sources(self):
        self.start_implementation()
        args = argparse.Namespace(
            run_file=str(self.run_file),
            since=None,
        )

        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "busy", "buffer": "*claude:stage-2*"},
            ) as buffer_state,
            mock.patch.object(orchestrator, "git_status") as git_status,
            mock.patch.object(orchestrator.session, "transcript_messages") as transcripts,
        ):
            current = orchestrator.status(args)

        self.assertEqual(set(current), {"run", "agent1"})
        self.assertEqual(current["run"]["phase"], "implementation")
        self.assertEqual(current["agent1"]["state"], "busy")
        buffer_state.assert_called_once_with("*claude:stage-2*")
        git_status.assert_not_called()
        transcripts.assert_not_called()

    def test_implementation_blocks_transcript_and_caller_selected_state_bypasses(self):
        self.start_implementation()

        with self.assertRaisesRegex(SystemExit, "transcripts are unavailable during implementation"):
            orchestrator.transcript_cmd(
                SimpleNamespace(
                    run_file=str(self.run_file), actor="agent1", since=None, last=5
                )
            )

        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "busy", "buffer": "*claude:stage-2*"},
            ) as buffer_state,
            redirect_stdout(io.StringIO()),
        ):
            orchestrator.state_cmd(
                SimpleNamespace(run_file=str(self.run_file), actor="agent1", json=True)
            )
        buffer_state.assert_called_once_with("*claude:stage-2*")

    def test_stage_return_exposes_only_last_bounded_return_after_agent_awaits(self):
        self.start_implementation()
        transcript = self.agent1_transcript
        for text in ("internal progress", "Stage-wide blocked-unobserved result"):
            record = {
                "timestamp": "2026-08-02T12:00:00Z",
                "message": {"role": "assistant", "content": text},
            }
            with transcript.open("a", encoding="utf-8") as stream:
                stream.write(json.dumps(record) + "\n")

        output = io.StringIO()
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            redirect_stdout(output),
        ):
            orchestrator.stage_return(SimpleNamespace(run_file=str(self.run_file)))

        self.assertEqual(output.getvalue().strip(), "Stage-wide blocked-unobserved result")
        self.assertNotIn("internal progress", output.getvalue())

    def test_stage_return_rejects_busy_agent_and_completed_marker(self):
        self.start_implementation()
        args = SimpleNamespace(run_file=str(self.run_file))
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "busy"},
            ),
            self.assertRaisesRegex(SystemExit, "Agent 1 is busy"),
        ):
            orchestrator.stage_return(args)

        self.write_phase_return("implementation")
        with (
            mock.patch.object(
                orchestrator.session,
                "buffer_state",
                return_value={"state": "awaiting-input"},
            ),
            self.assertRaisesRegex(SystemExit, "use finish-phase"),
        ):
            orchestrator.stage_return(args)

    def test_internal_model_turn_continuation_is_fail_closed(self):
        with self.assertRaisesRegex(SystemExit, "one-pass implementation"):
            orchestrator.switch_model(
                SimpleNamespace(run_file=str(self.run_file), model="opus")
            )

    def test_raw_buffer_and_transcript_cli_bypasses_are_unavailable(self):
        with (
            redirect_stderr(io.StringIO()),
            self.assertRaises(SystemExit),
        ):
            orchestrator.main(["buffers"])
        with (
            redirect_stderr(io.StringIO()),
            self.assertRaises(SystemExit),
        ):
            orchestrator.main(["state", "--buffer", "*task-agent*"])
        with (
            redirect_stderr(io.StringIO()),
            self.assertRaises(SystemExit),
        ):
            orchestrator.main(["transcript", "--path", "/tmp/task.jsonl"])

    def test_emacs_json_transport_keeps_private_file_until_write(self):
        def emulate_emacs(expr):
            match = re.search(r'\(let \(\(out ("(?:[^"\\]|\\.)*")\)\)', expr)
            self.assertIsNotNone(match)
            path = Path(json.loads(match.group(1)))
            self.assertTrue(path.exists())
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)
            path.write_text('{"state":"busy"}', encoding="utf-8")
            return "nil"

        with mock.patch.object(orchestrator.session, "run_emacs_eval", side_effect=emulate_emacs):
            result = orchestrator.session.run_emacs_json("'((state . \"busy\"))")

        self.assertEqual(result, {"state": "busy"})

    def test_buffer_state_uses_authoritative_backend_display_state(self):
        captured = {}

        def return_state(expr):
            captured["expr"] = expr
            return {
                "buffer": "*codex:fresh*",
                "state": "awaiting-input",
                "directory": "/tmp/",
            }

        with mock.patch.object(orchestrator.session, "run_emacs_json", side_effect=return_state):
            state = self.real_buffer_state("*codex:fresh*")

        self.assertEqual(state["state"], "awaiting-input")
        self.assertIn("agent-session-display-state", captured["expr"])
        self.assertIn("background-waiting", captured["expr"])

    def acknowledge(self, buffer, backend, prompt, **kwargs):
        pending = orchestrator.load_run(self.run_file)["pending_submission"]
        self.assertEqual(pending["prompt_sha256"], hashlib.sha256(prompt.encode()).hexdigest())
        self.assertEqual(prompt.splitlines()[0], pending["receipt"])
        self.assertEqual(kwargs["expected_identity"]["state"], "awaiting-input")
        path = self.identities[buffer]["transcript"]
        self.append_user(path, prompt)
        return path

    def start_review(self):
        self.create_run()
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec")
        self.finish_phase("spec")
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec-review")

    def fresh_reviewer(self, *, metadata=False):
        path = self.directory / "new-reviewer.jsonl"
        path.touch()
        self.identities["*codex:stage-2*"].update(
            session_id="new-reviewer-session", transcript=str(path.resolve()))
        if metadata:
            self.append_record(path, {"type": "session_meta", "payload": {
                "id": "new-reviewer-session", "cwd": str(Path("/tmp/example-repo").resolve())}})
        return path

    def stop_and_steering(self):
        self.start_implementation()
        self.append_record(self.agent1_transcript, {"message": {
            "role": "assistant", "content": "Capture tool failed; stage remains unfinished."}})
        with redirect_stdout(io.StringIO()):
            orchestrator.stage_return(SimpleNamespace(run_file=str(self.run_file)))
        prompt = self.directory / "targeted-steering.txt"
        prompt.write_text(
            "Obstacle: The capture tool failed before final acceptance could complete.\n"
            "Resolution: Use the documented alternate capture method already in scope.\n"
            "Whole-stage direction: Complete the whole stage and all final acceptance checks.\n")
        prompt.chmod(0o600)
        return SimpleNamespace(run_file=str(self.run_file), prompt_file=str(prompt))

    def test_fixed_actor_drift_refuses_before_any_submission(self):
        self.create_run()
        original = dict(self.identities["*claude:stage-2*"])
        for key, value in (("backend", "codex"), ("session_id", "replacement"),
                           ("transcript", str(self.directory / "unrelated.jsonl")),
                           ("directory", str(self.directory / "wrong-project"))):
            with self.subTest(key=key):
                self.identities["*claude:stage-2*"] = {**original, key: value}
                with mock.patch.object(orchestrator.session, "submit_to_agent") as send:
                    with self.assertRaisesRegex(SystemExit, "identity changed"):
                        orchestrator.submit(self.submit_args("spec"))
                    send.assert_not_called()
                self.assertIsNone(orchestrator.load_run(self.run_file)["pending_submission"])

    def test_roles_cannot_alias_one_buffer_transcript_or_session(self):
        cases = ({"agent2_buffer": "*claude:stage-2*"},
                 {"agent2_transcript": str(self.agent1_transcript)},
                 {"agent2_backend": "claude-code"})
        for override in cases:
            with self.subTest(override=override):
                if "agent2_backend" in override:
                    self.identities["*codex:stage-2*"].update(
                        backend="claude-code", session_id="author-session")
                with self.assertRaisesRegex(SystemExit, "distinct sessions"):
                    self.create_run(**override)
                self.assertFalse(self.run_file.exists())

    def test_reversed_roles_allow_identity_matched_startup_metadata(self):
        self.append_record(self.agent2_transcript, {"type": "session_meta", "payload": {
            "id": "reviewer-session", "cwd": str(Path("/tmp/example-repo").resolve())}})
        state = self.create_run(
            agent1_buffer="*codex:stage-2*", agent1_backend="codex",
            agent1_transcript=str(self.agent2_transcript),
            agent2_buffer="*claude:stage-2*", agent2_backend="claude-code",
            agent2_transcript=str(self.agent1_transcript))
        self.assertEqual(state["agent1"]["identity"]["session_id"], "reviewer-session")
        with mock.patch.object(orchestrator.session, "submit_to_agent", side_effect=self.acknowledge), redirect_stdout(io.StringIO()):
            orchestrator.submit(self.submit_args("spec"))
        self.assertGreater(orchestrator.load_run(self.run_file)["submissions"][-1]["transcript_offset"], 0)

    def test_positive_receipt_cannot_be_discarded_as_not_delivered(self):
        self.create_run()
        def delivered_then_timeout(*args, **kwargs):
            self.acknowledge(*args, **kwargs)
            raise orchestrator.EmacsClientError("transport timeout")
        with mock.patch.object(orchestrator.session, "submit_to_agent", side_effect=delivered_then_timeout):
            with self.assertRaisesRegex(orchestrator.EmacsClientError, "timeout"):
                orchestrator.submit(self.submit_args("spec"))
        before = self.run_file.read_bytes()
        with self.assertRaisesRegex(SystemExit, "positively acknowledged"):
            orchestrator.reconcile_submission(SimpleNamespace(run_file=str(self.run_file), delivered=False))
        self.assertEqual(self.run_file.read_bytes(), before)
        with redirect_stdout(io.StringIO()):
            orchestrator.reconcile_submission(SimpleNamespace(run_file=str(self.run_file), delivered=True))
        self.assertEqual(len(orchestrator.load_run(self.run_file)["submissions"]), 1)

    def test_receipt_requires_entire_current_prompt_not_static_or_nonce_alone(self):
        self.create_run()
        with mock.patch.object(orchestrator.session, "submit_to_agent", side_effect=RuntimeError("unknown")):
            with self.assertRaises(RuntimeError):
                orchestrator.submit(self.submit_args("spec"))
        pending = orchestrator.load_run(self.run_file)["pending_submission"]
        for text in ("PHASE COMPLETE: spec", pending["receipt"], pending["receipt"] + "\nAltered task"):
            self.append_user(self.agent1_transcript, text)
        before = self.run_file.read_bytes()
        with self.assertRaisesRegex(SystemExit, "delivery.*acknowledged|receipt"):
            orchestrator.reconcile_submission(SimpleNamespace(run_file=str(self.run_file), delivered=True))
        self.assertEqual(self.run_file.read_bytes(), before)

    def test_restart_pending_is_durable_and_reconciles_once_after_timeout(self):
        self.start_review()
        fresh = self.fresh_reviewer(metadata=True)
        def delivered_then_timeout(*args, **kwargs):
            self.assertEqual(orchestrator.load_run(self.run_file)["pending_submission"]["kind"], "restart")
            self.acknowledge(*args, **kwargs)
            raise orchestrator.EmacsClientError("transport timeout")
        args = SimpleNamespace(run_file=str(self.run_file), prompt_file=str(self.prompt_file))
        with mock.patch.object(orchestrator.session, "submit_to_agent", side_effect=delivered_then_timeout) as send:
            with self.assertRaises(orchestrator.EmacsClientError):
                orchestrator.restart_phase(args)
            with self.assertRaisesRegex(SystemExit, "pending submission"):
                orchestrator.restart_phase(args)
            self.assertEqual(send.call_count, 1)
        with redirect_stdout(io.StringIO()):
            orchestrator.reconcile_submission(SimpleNamespace(run_file=str(self.run_file), delivered=True))
        state = orchestrator.load_run(self.run_file)
        self.assertEqual(state["agent2"]["transcript"], str(fresh.resolve()))
        self.assertEqual(len(state["submissions"]), 2)
        self.assertGreater(state["submissions"][-1]["transcript_offset"], 0)
        self.append_record(fresh, {"message": {"role": "assistant", "content": "Reviewed.\nPHASE COMPLETE: spec-review"}})
        with redirect_stdout(io.StringIO()):
            orchestrator.finish_phase(self.finish_args("spec-review"))

    def test_restart_refuses_changed_context_and_old_tool_output(self):
        self.start_review()
        self.fresh_reviewer()
        self.prompt_file.write_text("Different review request")
        args = SimpleNamespace(run_file=str(self.run_file), prompt_file=str(self.prompt_file))
        with mock.patch.object(orchestrator.session, "submit_to_agent") as send:
            with self.assertRaisesRegex(SystemExit, "original phase context"):
                orchestrator.restart_phase(args)
            self.prompt_file.write_text("Phase context")
            self.append_record(self.agent2_transcript, {"type": "response_item", "payload": {
                "type": "function_call", "name": "fixture-tool", "arguments": "{}"}})
            with self.assertRaisesRegex(SystemExit, "already returned assistant output"):
                orchestrator.restart_phase(args)
            send.assert_not_called()

    def test_delivered_steering_timeout_recovers_its_new_return(self):
        args = self.stop_and_steering()
        def delivered_then_timeout(*positional, **kwargs):
            self.assertEqual(orchestrator.load_run(self.run_file)["pending_submission"]["kind"], "steering")
            self.acknowledge(*positional, **kwargs)
            self.append_record(self.agent1_transcript, {"message": {
                "role": "assistant", "content": "The entire stage is verified.\nSTAGE COMPLETE: 2"}})
            raise orchestrator.EmacsClientError("receipt transport failed")
        with mock.patch.object(orchestrator.session, "submit_to_agent", side_effect=delivered_then_timeout) as send:
            with self.assertRaises(orchestrator.EmacsClientError):
                orchestrator.steer_stage(args)
            with self.assertRaisesRegex(SystemExit, "pending submission"):
                orchestrator.steer_stage(args)
            self.assertEqual(send.call_count, 1)
        before = self.run_file.read_bytes()
        with self.assertRaisesRegex(SystemExit, "positively acknowledged"):
            orchestrator.reconcile_submission(SimpleNamespace(run_file=str(self.run_file), delivered=False))
        self.assertEqual(self.run_file.read_bytes(), before)
        with redirect_stdout(io.StringIO()):
            orchestrator.reconcile_submission(SimpleNamespace(run_file=str(self.run_file), delivered=True))
        output = io.StringIO()
        with redirect_stdout(output):
            orchestrator.finish_phase(self.finish_args("implementation"))
        state = orchestrator.load_run(self.run_file)
        text = "The entire stage is verified.\nSTAGE COMPLETE: 2"
        self.assertTrue(output.getvalue().endswith(text + "\n"))
        self.assertEqual(state["completions"][-1]["evidence_sha256"], hashlib.sha256(text.encode()).hexdigest())
        self.assertNotIn(text, self.run_file.read_text())

    def test_acknowledged_steering_does_not_reuse_previous_stop(self):
        args = self.stop_and_steering()
        with mock.patch.object(orchestrator.session, "submit_to_agent", side_effect=self.acknowledge), redirect_stdout(io.StringIO()):
            orchestrator.steer_stage(args)
        before = self.run_file.read_bytes()
        with self.assertRaisesRegex(SystemExit, "no bounded implementation return"):
            orchestrator.stage_return(SimpleNamespace(run_file=str(self.run_file)))
        self.assertEqual(self.run_file.read_bytes(), before)

    def test_unrelated_later_user_and_final_cannot_complete_phase(self):
        self.create_run()
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec")
        self.write_phase_return("spec")
        self.append_user(self.agent1_transcript, "Unrelated later work")
        self.write_phase_return("spec")
        with self.assertRaisesRegex(SystemExit, "no returned assistant message"):
            orchestrator.finish_phase(self.finish_args("spec"))

    def test_transcript_replacement_refuses_receipt_and_terminal_acceptance(self):
        self.create_run()
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec")
        self.write_phase_return("spec")
        replacement = self.directory / "replacement.jsonl"
        replacement.write_bytes(self.agent1_transcript.read_bytes())
        replacement.replace(self.agent1_transcript)
        with self.assertRaisesRegex(orchestrator.EmacsClientError, "identity or pre-submit prefix changed"):
            orchestrator.finish_phase(self.finish_args("spec"))

    def test_pending_receipt_refuses_replaced_transcript_even_with_exact_prompt(self):
        self.create_run()
        def delivered_then_timeout(*args, **kwargs):
            self.acknowledge(*args, **kwargs)
            raise orchestrator.EmacsClientError("timeout")
        with mock.patch.object(orchestrator.session, "submit_to_agent", side_effect=delivered_then_timeout):
            with self.assertRaises(orchestrator.EmacsClientError):
                orchestrator.submit(self.submit_args("spec"))
        replacement = self.directory / "new-inode.jsonl"
        replacement.write_bytes(self.agent1_transcript.read_bytes())
        replacement.replace(self.agent1_transcript)
        before = self.run_file.read_bytes()
        with self.assertRaisesRegex(orchestrator.EmacsClientError, "identity or pre-submit prefix changed"):
            orchestrator.reconcile_submission(SimpleNamespace(run_file=str(self.run_file), delivered=True))
        self.assertEqual(self.run_file.read_bytes(), before)

    def replace_transcript_after_read(self, reader, path):
        def read_then_replace(*args, **kwargs):
            result = reader(*args, **kwargs)
            self.assertTrue(result)
            replacement = self.directory / "replacement-after-read.jsonl"
            replacement.write_bytes(path.read_bytes())
            replacement.replace(path)
            return result
        return read_then_replace

    def test_reconcile_refuses_transcript_replaced_during_receipt_read(self):
        self.create_run()
        def delivered_then_timeout(*args, **kwargs):
            self.acknowledge(*args, **kwargs)
            raise orchestrator.EmacsClientError("timeout")
        with mock.patch.object(orchestrator.session, "submit_to_agent", side_effect=delivered_then_timeout):
            with self.assertRaises(orchestrator.EmacsClientError):
                orchestrator.submit(self.submit_args("spec"))
        reader = orchestrator.session._marker_delivered
        before = self.run_file.read_bytes()
        with mock.patch.object(orchestrator.session, "_marker_delivered",
                               side_effect=self.replace_transcript_after_read(reader, self.agent1_transcript)):
            with self.assertRaisesRegex(orchestrator.EmacsClientError, "identity or pre-submit prefix changed"):
                orchestrator.reconcile_submission(SimpleNamespace(run_file=str(self.run_file), delivered=True))
        self.assertEqual(self.run_file.read_bytes(), before)

    def test_stage_return_refuses_transcript_replaced_during_terminal_read(self):
        self.start_implementation()
        self.append_record(self.agent1_transcript, {"message": {
            "role": "assistant", "content": "The capture failed; stage remains incomplete."}})
        reader = orchestrator.session.latest_transcript_return
        before = self.run_file.read_bytes()
        with mock.patch.object(orchestrator.session, "latest_transcript_return",
                               side_effect=self.replace_transcript_after_read(reader, self.agent1_transcript)):
            with self.assertRaisesRegex(orchestrator.EmacsClientError, "identity or pre-submit prefix changed"):
                orchestrator.stage_return(SimpleNamespace(run_file=str(self.run_file)))
        self.assertEqual(self.run_file.read_bytes(), before)

    def test_steering_refuses_transcript_replaced_during_terminal_read(self):
        args = self.stop_and_steering()
        reader = orchestrator.session.latest_transcript_return
        before = self.run_file.read_bytes()
        with mock.patch.object(orchestrator.session, "latest_transcript_return",
                               side_effect=self.replace_transcript_after_read(reader, self.agent1_transcript)), \
                mock.patch.object(orchestrator.session, "submit_to_agent") as send:
            with self.assertRaisesRegex(orchestrator.EmacsClientError, "identity or pre-submit prefix changed"):
                orchestrator.steer_stage(args)
            send.assert_not_called()
        self.assertEqual(self.run_file.read_bytes(), before)

    def test_pending_save_failure_prevents_external_contact(self):
        self.create_run()
        before = self.run_file.read_bytes()
        with mock.patch.object(orchestrator, "save_run", side_effect=OSError("fixture disk failure")), mock.patch.object(orchestrator.session, "submit_to_agent") as send:
            with self.assertRaises(OSError):
                orchestrator.submit(self.submit_args("spec"))
            send.assert_not_called()
        self.assertEqual(self.run_file.read_bytes(), before)

    def test_steering_refuses_a_recorded_return_replaced_by_new_activity(self):
        args = self.stop_and_steering()
        self.append_user(self.agent1_transcript, "Unrelated external prompt")
        self.append_record(self.agent1_transcript, {"message": {"role": "assistant", "content": "Unrelated return"}})
        with mock.patch.object(orchestrator.session, "submit_to_agent") as send:
            with self.assertRaisesRegex(SystemExit, "no bounded implementation return"):
                orchestrator.steer_stage(args)
            send.assert_not_called()

    def test_preexisting_prefix_edit_refuses_even_with_current_exact_receipt(self):
        self.create_run(adopt_implementation=True, spec_commit="abc", plan_commit="def", reviews_complete=True)
        self.append_record(self.agent1_transcript, {"type": "progress", "value": "before"})
        with mock.patch.object(orchestrator.session, "submit_to_agent", side_effect=self.acknowledge), redirect_stdout(io.StringIO()):
            orchestrator.submit(self.submit_args("implementation"))
        data = self.agent1_transcript.read_bytes().replace(b"before", b"edited")
        self.agent1_transcript.write_bytes(data)
        self.write_phase_return("implementation")
        with self.assertRaisesRegex(orchestrator.EmacsClientError, "identity or pre-submit prefix changed"):
            orchestrator.finish_phase(self.finish_args("implementation"))

    def legacy_state(self, state):
        state["version"] = 2
        for name in ("agent1", "agent2"):
            state[name].pop("identity", None)
        state.pop("attempts", None)
        state.pop("phase_context_sha256", None)
        self.run_file.write_text(json.dumps(state))

    def test_legacy_status_and_unambiguous_active_migration_preserve_evidence(self):
        self.create_run()
        with mock.patch.object(orchestrator.session, "submit_to_agent"):
            self.submit_phase("spec")
        self.write_phase_return("spec")
        self.legacy_state(orchestrator.load_run(self.run_file))
        output = io.StringIO()
        with redirect_stdout(output):
            orchestrator.run_status(SimpleNamespace(run_file=str(self.run_file), json=True))
        self.assertEqual(json.loads(output.getvalue())["version"], 2)
        with self.assertRaisesRegex(SystemExit, "migrate-run"):
            orchestrator.finish_phase(self.finish_args("spec"))
        with redirect_stdout(io.StringIO()):
            orchestrator.migrate_run(SimpleNamespace(run_file=str(self.run_file)))
            orchestrator.finish_phase(self.finish_args("spec"))
        state = orchestrator.load_run(self.run_file)
        self.assertTrue(state["legacy_history"])
        self.assertEqual(state["attempts"], [])
        self.assertEqual(state["expected_phase"], "spec-review")

    def test_legacy_ambiguous_migration_preserves_original_bytes(self):
        state = self.create_run()
        state["pending_submission"] = {"kind": "phase", "phase": "spec", "actor": "agent1", "transcript_offset": 0}
        self.legacy_state(state)
        before = self.run_file.read_bytes()
        with self.assertRaisesRegex(SystemExit, "ambiguous legacy"):
            orchestrator.migrate_run(SimpleNamespace(run_file=str(self.run_file)))
        self.assertEqual(self.run_file.read_bytes(), before)

    def test_injected_contract_respects_host_wait_and_authority(self):
        contract = orchestrator.IMPLEMENTATION_CONTRACT
        for forbidden in ("Python sleep", "10 minutes", "sleep is blocked", "circumvent"):
            self.assertNotIn(forbidden, contract)
        for required in ("obey guard denials", "host-required commentary", "Persistence never expands", "user or system authorization"):
            self.assertIn(required, contract)


class WatchVerdictTests(unittest.TestCase):
    def test_paired_helpers_stay_identical(self):
        self.assertEqual(CODEX_SCRIPT.read_bytes(), CLAUDE_SCRIPT.read_bytes())

    def render_once(self, current):
        args = argparse.Namespace(json=False, interval=0)
        output = io.StringIO()
        with (
            mock.patch.object(orchestrator, "status", return_value=current),
            mock.patch.object(
                orchestrator.time, "sleep", side_effect=StopIteration
            ),
            redirect_stdout(output),
            self.assertRaises(StopIteration),
        ):
            orchestrator.watch(args)
        return output.getvalue()

    def test_reviewer_prompt_does_not_become_a_verdict(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "reviewer_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "user",
                            "text": (
                                "Answer IMPLEMENTATION-READY or NOT READY "
                                "after reviewing the plan."
                            ),
                        }
                    ],
                },
            }
        )

        self.assertNotIn(" IMPLEMENTATION-READY", output)
        self.assertNotIn(" NOT READY", output)

    def test_reviewer_answer_at_the_start_is_reported(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "reviewer_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "assistant",
                            "text": "NOT READY\n\n1. Fix the focused test command.",
                        }
                    ],
                },
            }
        )

        self.assertIn(" NOT READY", output)

    def test_implementation_ready_at_the_start_is_reported(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "reviewer_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "assistant",
                            "text": "IMPLEMENTATION-READY\n\nAll blockers are resolved.",
                        }
                    ],
                },
            }
        )

        self.assertIn(" IMPLEMENTATION-READY", output)

    def test_first_line_not_ready_wins_over_ready_in_the_body(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "reviewer_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "assistant",
                            "text": (
                                "NOT READY\n\nFix this before the plan can be "
                                "IMPLEMENTATION-READY."
                            ),
                        }
                    ],
                },
            }
        )

        self.assertIn(" NOT READY", output)
        self.assertNotIn(" IMPLEMENTATION-READY", output)

    def test_planner_discussion_does_not_become_a_verdict(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "planner_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "message",
                            "text": "The reviewer should now return IMPLEMENTATION-READY.",
                        }
                    ],
                },
            }
        )

        self.assertNotIn(" IMPLEMENTATION-READY", output)

    def test_incidental_reviewer_discussion_does_not_become_a_verdict(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "reviewer_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "assistant",
                            "text": (
                                "I checked whether this should be "
                                "IMPLEMENTATION-READY, but one blocker remains."
                            ),
                        }
                    ],
                },
            }
        )

        self.assertNotIn(" IMPLEMENTATION-READY", output)

    def test_watch_recovers_after_transient_emacsclient_failure(self):
        error_type = getattr(orchestrator, "EmacsClientError", SystemExit)
        current = {
            "repo": {"head": "abc123 resumed"},
            "planner": {"state": "busy"},
        }
        args = argparse.Namespace(json=False, interval=0)
        output = io.StringIO()
        with (
            mock.patch.object(
                orchestrator,
                "status",
                side_effect=[
                    error_type("emacsclient failed (1): connection refused"),
                    current,
                ],
            ),
            mock.patch.object(
                orchestrator.time, "sleep", side_effect=[None, StopIteration]
            ),
            redirect_stdout(output),
            self.assertRaises(StopIteration),
        ):
            orchestrator.watch(args)

        rendered = output.getvalue()
        self.assertIn("monitor-error=emacsclient failed (1): connection refused", rendered)
        self.assertIn("abc123 resumed | planner=busy", rendered)

    def test_watch_propagates_second_consecutive_emacsclient_failure(self):
        error_type = orchestrator.EmacsClientError
        args = argparse.Namespace(json=False, interval=0)
        output = io.StringIO()
        with (
            mock.patch.object(
                orchestrator,
                "status",
                side_effect=[
                    error_type("emacsclient failed (1): connection refused"),
                    error_type("emacsclient failed (1): connection refused again"),
                ],
            ),
            mock.patch.object(orchestrator.time, "sleep", return_value=None),
            redirect_stdout(output),
            self.assertRaisesRegex(error_type, "connection refused again"),
        ):
            orchestrator.watch(args)

        self.assertIn("monitor-error=emacsclient failed (1): connection refused", output.getvalue())


class TranscriptMessageTests(unittest.TestCase):
    def test_claude_user_prompts_are_not_assistant_messages(self):
        records = [
            {
                "timestamp": "2026-08-01T16:17:01.630Z",
                "type": "user",
                "message": {
                    "role": "user",
                    "content": "Answer IMPLEMENTATION-READY or NOT READY.",
                },
            },
            {
                "timestamp": "2026-08-01T16:18:26.361Z",
                "type": "assistant",
                "message": {
                    "role": "assistant",
                    "content": [{"type": "text", "text": "NOT READY\n\nBlocker"}],
                },
            },
        ]
        with tempfile.TemporaryDirectory() as directory:
            transcript = Path(directory) / "claude.jsonl"
            transcript.write_text(
                "".join(json.dumps(record) + "\n" for record in records),
                encoding="utf-8",
            )

            messages = orchestrator.session.transcript_messages(transcript)

        self.assertEqual(
            messages,
            [
                {
                    "timestamp": "2026-08-01T16:18:26.361Z",
                    "kind": "assistant",
                    "text": "NOT READY\n\nBlocker",
                }
            ],
        )

if __name__ == "__main__":
    unittest.main()
