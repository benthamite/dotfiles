#!/usr/bin/env python3
"""Helpers for supervising live Emacs agent.el planner/reviewer loops."""

from __future__ import annotations

import argparse
import fcntl
import hashlib
import importlib.util
import json
import os
import re
import stat
import subprocess
import tempfile
import time
from contextlib import contextmanager
from pathlib import Path
from typing import Any

_LIB_FILE = (
    Path(__file__).resolve().parents[4] / "lib" / "python" / "agent_session_lib.py"
)
_LIB_SPEC = importlib.util.spec_from_file_location("agent_session_lib", _LIB_FILE)
if _LIB_SPEC is None or _LIB_SPEC.loader is None:
    raise SystemExit(f"cannot load shared session library: {_LIB_FILE}")
session = importlib.util.module_from_spec(_LIB_SPEC)
_LIB_SPEC.loader.exec_module(session)

EmacsClientError = session.EmacsClientError

RUN_VERSION = 2
PHASES = ("spec", "spec-review", "plan", "plan-review", "implementation")
PHASE_ACTOR = {
    "spec": "agent1",
    "spec-review": "agent2",
    "plan": "agent1",
    "plan-review": "agent2",
    "implementation": "agent1",
}
RUN_STATUSES = {
    "ready",
    "phase-active",
    "implementation-active",
    "implementation-stopped",
    "implementation-returned",
    "complete",
}

IMPLEMENTATION_CONTRACT = """STAGE-ATOMIC IMPLEMENTATION CONTRACT

Implement and complete all of Stage {stage}. Internal plan tasks are not orchestration checkpoints:
own their sequencing, tests, commits, corrections,
and recovery without returning for task-level supervision. Do not stop at an
internal task boundary. Return only when the entire stage and its stage-final
verification are complete, or when progress requires a user-only credential,
identity check, irreversible action, spending decision, destructive action, or
product choice that the repository and approved plan cannot determine. A false
technical premise, failed test, missing capture, or implementation obstacle is
not a user-only stop: adapt the plan and continue toward the whole-stage result.
Never end your turn to wait: a background command, detached job, subagent,
or reviewer that has not finished is not a reason to return. Ending the turn
is read as a stage stop and can only be reopened by a steering prompt. Wait
inside the turn with bounded polling loops (each well under the harness's
10-minute command limit, re-armed as needed) and continue when the result
lands. Shell `sleep` is blocked in this harness; waits through the Python
interpreter are not. Background waiters die after 10 minutes and never
re-invoke you.

Progress file (mandatory): append one line to {progress_file} at every
attempt start, every completed step, and every attempt failure (with the
failing step and its cause), e.g. "attempt 3 | treatment_regen | FAILED:
predecessor ambiguity". The orchestrator supervises the stage from this file
and stops it if the same landing cycle fails twice; keep the file honest and
current. Iteration must be cheap: when a check fails after an expensive
cycle (regen, rehearsal, import), fix and test the check against the cached
outputs of that cycle; do not rerun the cycle unless its inputs changed.

Stage context follows:

{context}

END STAGE CONTEXT

The context may describe internal tasks, but it cannot narrow this contract,
create task-level checkpoints, or authorize an early return.

Only after the complete stage and its verification are done, end the final
response with this exact line:
STAGE COMPLETE: {stage}"""

STEERING_CONTRACT = """TARGETED WHOLE-STAGE STEERING

This message responds to the specific reason you returned before completing
Stage {stage}. It is not an internal-task checkpoint or a new implementation
plan. Use the diagnosis below, then resume ownership of the entire stage,
including its final verification.

{context}

Return only when the complete stage is verified or a genuinely user-only
decision described by the implementation contract remains.
"""

PHASE_CONTRACT = """{context}

PHASE COMPLETION CONTRACT

Complete the entire {phase} phase before returning. This context cannot create
intermediate orchestration checkpoints. Only after the phase is complete, end
the final response with this exact line:
PHASE COMPLETE: {phase}"""


def _state_bytes(state: dict[str, Any]) -> bytes:
    return (json.dumps(state, ensure_ascii=False, indent=2, sort_keys=True) + "\n").encode(
        "utf-8"
    )


def _validate_role(role: dict[str, Any], name: str) -> None:
    if not isinstance(role, dict):
        raise SystemExit(f"invalid run state: {name} role is missing")
    if not isinstance(role.get("buffer"), str) or not role["buffer"]:
        raise SystemExit(f"invalid run state: {name} buffer is missing")
    if role.get("backend") not in session.VALID_BACKENDS:
        raise SystemExit(f"invalid run state: {name} backend is invalid")
    transcript = role.get("transcript")
    if not isinstance(transcript, str) or not transcript:
        raise SystemExit(f"invalid run state: {name} transcript is missing")


def _validate_phase_entries(
    entries: Any, *, name: str, allow_evidence: bool
) -> list[dict[str, Any]]:
    if not isinstance(entries, list):
        raise SystemExit(f"invalid run state: {name} must be a list")
    if len(entries) > len(PHASES):
        raise SystemExit(f"invalid run state: {name} are not an exact phase prefix")
    for index, entry in enumerate(entries):
        phase = PHASES[index]
        actor = PHASE_ACTOR[phase]
        expected_keys = (
            {"phase", "actor", "evidence_sha256"}
            if allow_evidence
            else {"phase", "actor", "transcript_offset"}
        )
        if not isinstance(entry, dict) or set(entry) != expected_keys:
            raise SystemExit(f"invalid run state: malformed {name} entry")
        if entry["phase"] != phase or entry["actor"] != actor:
            raise SystemExit(f"invalid run state: {name} are not an exact phase prefix")
        if allow_evidence and (
            not isinstance(entry["evidence_sha256"], str)
            or len(entry["evidence_sha256"]) != 64
        ):
            raise SystemExit(f"invalid run state: malformed {name} evidence")
        if not allow_evidence and (
            not isinstance(entry["transcript_offset"], int)
            or entry["transcript_offset"] < 0
        ):
            raise SystemExit(f"invalid run state: malformed {name} transcript offset")
    return entries


def _validate_pending(pending: Any) -> None:
    if pending is None:
        return
    if not isinstance(pending, dict) or set(pending) != {
        "kind",
        "phase",
        "actor",
        "transcript_offset",
    }:
        raise SystemExit("invalid run state: malformed pending submission")
    if pending["kind"] != "phase":
        raise SystemExit("invalid run state: malformed pending submission")
    if pending["phase"] not in PHASES:
        raise SystemExit("invalid run state: malformed pending submission")
    if pending["actor"] != PHASE_ACTOR[pending["phase"]]:
        raise SystemExit("invalid run state: malformed pending submission")
    if not isinstance(pending["transcript_offset"], int) or pending["transcript_offset"] < 0:
        raise SystemExit("invalid run state: malformed pending submission")


def validate_run(state: Any) -> dict[str, Any]:
    if not isinstance(state, dict) or state.get("version") != RUN_VERSION:
        raise SystemExit("invalid or unsupported orchestration run state")
    if not isinstance(state.get("stage"), str) or not state["stage"]:
        raise SystemExit("invalid run state: stage is missing")
    _validate_role(state.get("agent1"), "Agent 1")
    _validate_role(state.get("agent2"), "Agent 2")
    if not isinstance(state.get("repo"), str) or not state["repo"]:
        raise SystemExit("invalid run state: repository is missing")
    if state.get("status") not in RUN_STATUSES:
        raise SystemExit("invalid run state: status is unsupported")
    expected = state.get("expected_phase")
    if expected is not None and expected not in PHASES:
        raise SystemExit("invalid run state: expected phase is unsupported")
    submissions = _validate_phase_entries(
        state.get("submissions"), name="submissions", allow_evidence=False
    )
    completions = _validate_phase_entries(
        state.get("completions"), name="completions", allow_evidence=True
    )
    if len(completions) > len(submissions) or len(submissions) - len(completions) > 1:
        raise SystemExit("invalid run state: submissions and completions are incoherent")
    if submissions[: len(completions)] != [
        {
            "phase": entry["phase"],
            "actor": entry["actor"],
            "transcript_offset": submissions[index]["transcript_offset"],
        }
        for index, entry in enumerate(completions)
    ]:
        raise SystemExit("invalid run state: submissions and completions are incoherent")
    _validate_pending(state.get("pending_submission"))
    steering_prompts = state.get("steering_prompts", [])
    if not isinstance(steering_prompts, list) or any(
        not isinstance(entry, dict)
        or not isinstance(entry.get("prompt_sha256"), str)
        or len(entry["prompt_sha256"]) != 64
        or not isinstance(entry.get("stop_sha256"), str)
        or len(entry["stop_sha256"]) != 64
        for entry in steering_prompts
    ):
        raise SystemExit("invalid run state: steering prompts are incoherent")

    status = state["status"]
    if status != "implementation-stopped" and state.get("stop_evidence") is not None:
        raise SystemExit("invalid run state: stop evidence is incoherent")
    active_phase = state.get("active_phase")
    next_index = len(submissions)
    if status == "ready":
        required_expected = PHASES[next_index] if next_index < len(PHASES) else None
        coherent = (
            len(submissions) == len(completions)
            and active_phase is None
            and expected == required_expected
        )
    elif status in {"phase-active", "implementation-active"}:
        current = submissions[-1]["phase"] if submissions else None
        coherent = (
            len(submissions) == len(completions) + 1
            and active_phase == current
            and expected is None
            and (status == "implementation-active") == (current == "implementation")
        )
    elif status == "implementation-stopped":
        evidence = state.get("stop_evidence")
        coherent = (
            len(submissions) == len(PHASES)
            and len(completions) == len(PHASES) - 1
            and active_phase is None
            and expected is None
            and isinstance(evidence, dict)
            and evidence.get("kind") in {"agent-return", "delivery-ambiguous"}
            and isinstance(evidence.get("sha256"), str)
            and len(evidence["sha256"]) == 64
            and state.get("acceptance_evidence") is None
        )
    elif status == "implementation-returned":
        coherent = (
            len(submissions) == len(completions) == len(PHASES)
            and active_phase is None
            and expected is None
            and state.get("stop_evidence") is None
            and state.get("acceptance_evidence") is None
        )
    else:
        evidence = state.get("acceptance_evidence")
        coherent = (
            len(submissions) == len(completions) == len(PHASES)
            and active_phase is None
            and expected is None
            and isinstance(evidence, dict)
            and isinstance(evidence.get("sha256"), str)
            and len(evidence["sha256"]) == 64
            and state.get("stop_evidence") is None
        )
    if not coherent:
        raise SystemExit("invalid run state: phase lifecycle is incoherent")

    pending = state.get("pending_submission")
    if pending:
        if status != "ready" or pending["phase"] != expected:
            raise SystemExit("invalid run state: pending phase is incoherent")
    return state


def load_run(path: Path | str) -> dict[str, Any]:
    run_path = Path(path)
    try:
        metadata = run_path.lstat()
    except FileNotFoundError:
        raise SystemExit(f"orchestration run file does not exist: {run_path}") from None
    if run_path.is_symlink():
        raise SystemExit(f"orchestration run file must not be a symlink: {run_path}")
    if metadata.st_mode & 0o777 != 0o600:
        raise SystemExit(f"orchestration run file must have mode 0600: {run_path}")
    try:
        state = json.loads(run_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise SystemExit(f"cannot read orchestration run file {run_path}: {error}") from None
    return validate_run(state)


def _create_run_file(path: Path, state: dict[str, Any]) -> None:
    flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
    if hasattr(os, "O_NOFOLLOW"):
        flags |= os.O_NOFOLLOW
    try:
        fd = os.open(path, flags, 0o600)
    except OSError as error:
        raise SystemExit(f"cannot create orchestration run file {path}: {error}") from None
    try:
        os.fchmod(fd, 0o600)
        session._write_all(fd, _state_bytes(state))
        os.fsync(fd)
    finally:
        os.close(fd)


def save_run(path: Path | str, state: dict[str, Any]) -> None:
    run_path = Path(path)
    validate_run(state)
    if run_path.is_symlink():
        raise SystemExit(f"orchestration run file must not be a symlink: {run_path}")
    fd, temporary = tempfile.mkstemp(prefix=f".{run_path.name}.", dir=run_path.parent)
    temp_path = Path(temporary)
    try:
        os.fchmod(fd, 0o600)
        session._write_all(fd, _state_bytes(state))
        os.fsync(fd)
        os.close(fd)
        fd = -1
        os.replace(temp_path, run_path)
    finally:
        if fd >= 0:
            os.close(fd)
        temp_path.unlink(missing_ok=True)


@contextmanager
def run_lock(path: Path | str):
    run_path = Path(path)
    lock_path = run_path.with_name(run_path.name + ".lock")
    flags = os.O_RDWR | os.O_CREAT
    if hasattr(os, "O_NOFOLLOW"):
        flags |= os.O_NOFOLLOW
    try:
        fd = os.open(lock_path, flags, 0o600)
    except OSError as error:
        raise SystemExit(f"cannot open orchestration run lock {lock_path}: {error}") from None
    try:
        os.fchmod(fd, 0o600)
        fcntl.flock(fd, fcntl.LOCK_EX)
        yield
    finally:
        fcntl.flock(fd, fcntl.LOCK_UN)
        os.close(fd)


def create_run(args: argparse.Namespace) -> None:
    if args.agent1_backend not in session.VALID_BACKENDS:
        raise SystemExit("invalid Agent 1 backend")
    if args.agent2_backend not in session.VALID_BACKENDS:
        raise SystemExit("invalid Agent 2 backend")
    if not str(args.stage).strip():
        raise SystemExit("stage must not be empty")
    agent1_transcript = Path(args.agent1_transcript) if args.agent1_transcript else None
    if (
        not args.adopt_implementation
        and agent1_transcript is not None
        and agent1_transcript.exists()
        and agent1_transcript.stat().st_size
    ):
        raise SystemExit("a new stage requires a fresh Agent 1 transcript")
    adopted_evidence = None
    expected_phase = "spec"
    submissions: list[dict[str, str]] = []
    completions: list[dict[str, str]] = []
    if args.adopt_implementation:
        if not (args.spec_commit and args.plan_commit and args.reviews_complete):
            raise SystemExit(
                "adoption requires spec commit, plan commit, and both reviews"
            )
        expected_phase = "implementation"
        adopted_evidence = {
            "spec_commit": args.spec_commit,
            "plan_commit": args.plan_commit,
            "reviews_complete": True,
        }
        adopted_sha = hashlib.sha256(
            json.dumps(adopted_evidence, sort_keys=True).encode("utf-8")
        ).hexdigest()
        for phase in PHASES[:-1]:
            actor = PHASE_ACTOR[phase]
            submissions.append(
                {"phase": phase, "actor": actor, "transcript_offset": 0}
            )
            completions.append(
                {"phase": phase, "actor": actor, "evidence_sha256": adopted_sha}
            )
    state = {
        "version": RUN_VERSION,
        "repo": str(Path(args.repo).resolve()),
        "stage": str(args.stage),
        "agent1": {
            "buffer": args.agent1_buffer,
            "backend": args.agent1_backend,
            "transcript": args.agent1_transcript,
        },
        "agent2": {
            "buffer": args.agent2_buffer,
            "backend": args.agent2_backend,
            "transcript": args.agent2_transcript,
        },
        "status": "ready",
        "expected_phase": expected_phase,
        "active_phase": None,
        "submissions": submissions,
        "completions": completions,
        "pending_submission": None,
        "stop_evidence": None,
        "steering_prompts": [],
        "adopted_evidence": adopted_evidence,
        "acceptance_evidence": None,
    }
    validate_run(state)
    _create_run_file(Path(args.run_file), state)
    print(session.json_for_display(state))


def state_cmd(args: argparse.Namespace) -> None:
    run = load_run(args.run_file)
    if (
        run["status"]
        in {
            "implementation-active",
            "implementation-stopped",
            "implementation-returned",
        }
        and args.actor != "agent1"
    ):
        raise SystemExit(
            "only Agent 1 top-level state is available during implementation"
        )
    state = session.buffer_state(run[args.actor]["buffer"])
    if args.json:
        print(session.json_for_display(state))
    else:
        print(f"{state['state']:15} {state['buffer']} [{state['directory']}]")


def progress_file_for(run_file: str | Path) -> Path:
    """The supervision channel Agent 1 appends to during implementation."""
    return Path(str(run_file) + ".progress")


def latest_progress(run_file: str | Path) -> dict[str, Any] | None:
    p = progress_file_for(run_file)
    if not p.exists():
        return None
    lines = [l for l in p.read_text(errors="replace").splitlines() if l.strip()]
    return {
        "path": str(p),
        "lines": len(lines),
        "latest": lines[-1] if lines else "",
        "age_s": int(time.time() - p.stat().st_mtime),
    }


def _phase_prompt(
    state: dict[str, Any], phase: str, context: str, run_file: str | Path | None = None
) -> str:
    if phase == "implementation":
        progress = str(progress_file_for(run_file)) if run_file else "<run-file>.progress"
        return IMPLEMENTATION_CONTRACT.format(
            stage=state["stage"], context=context, progress_file=progress
        )
    return PHASE_CONTRACT.format(phase=phase, context=context)


def _delivery_marker(stage: str, phase: str) -> str:
    if phase == "implementation":
        return f"STAGE COMPLETE: {stage}"
    return f"PHASE COMPLETE: {phase}"


def _evidence_digest(path: Path | str) -> str:
    evidence_path = Path(path)
    try:
        data = evidence_path.read_bytes()
    except OSError as error:
        raise SystemExit(f"cannot read evidence file {evidence_path}: {error}") from None
    if not data.strip():
        raise SystemExit("evidence file is empty")
    return hashlib.sha256(data).hexdigest()


def _phase_evidence(state: dict[str, Any], phase: str) -> str:
    actor = PHASE_ACTOR[phase]
    transcript = state[actor].get("transcript")
    if not transcript:
        raise SystemExit(f"{actor} transcript was not recorded in the run")
    submission = state["submissions"][-1]
    if submission["phase"] != phase:
        raise SystemExit("phase submission boundary is incoherent")
    messages = session.transcript_messages(
        Path(transcript), offset=submission["transcript_offset"]
    )
    if not messages:
        raise SystemExit(
            "phase transcript contains no returned assistant message after current submission"
        )
    text = messages[-1]["text"]
    lines = [line.strip() for line in text.splitlines() if line.strip()]
    marker = (
        f"STAGE COMPLETE: {state['stage']}"
        if phase == "implementation"
        else f"PHASE COMPLETE: {phase}"
    )
    if not lines or lines[-1] != marker:
        raise SystemExit(f"{phase} completion marker is missing")
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def _require_no_pending(state: dict[str, Any]) -> None:
    if state["pending_submission"] is not None:
        raise SystemExit(
            "pending submission requires reconciliation before any further action"
        )


def _finalize_pending(state: dict[str, Any]) -> None:
    pending = state["pending_submission"]
    if pending is None:
        raise SystemExit("no pending submission to finalize")
    phase = pending["phase"]
    state["submissions"].append(
        {
            "phase": phase,
            "actor": pending["actor"],
            "transcript_offset": pending["transcript_offset"],
        }
    )
    state["status"] = (
        "implementation-active" if phase == "implementation" else "phase-active"
    )
    state["active_phase"] = phase
    state["expected_phase"] = None
    state["pending_submission"] = None


def _freeze_pending_implementation(state: dict[str, Any], reason: str) -> None:
    """Make an ambiguous implementation attempt permanently non-runnable."""
    pending = state["pending_submission"]
    if pending is None or pending["phase"] != "implementation":
        raise SystemExit("no pending implementation can be frozen")
    _finalize_pending(state)
    state["status"] = "implementation-stopped"
    state["active_phase"] = None
    state["stop_evidence"] = {
        "kind": "delivery-ambiguous",
        "sha256": hashlib.sha256(reason.encode("utf-8")).hexdigest()
    }


def submit(args: argparse.Namespace) -> None:
    prompt_path = Path(args.prompt_file)
    if not prompt_path.exists():
        raise SystemExit(f"prompt file does not exist: {prompt_path}")
    try:
        context = prompt_path.read_text(encoding="utf-8")
    except OSError as error:
        raise SystemExit(f"cannot read prompt file {prompt_path}: {error}") from None
    with run_lock(args.run_file):
        state = load_run(args.run_file)
        _require_no_pending(state)
        if state["status"] == "implementation-active":
            raise SystemExit("stage implementation is active; leave Agent 1 alone")
        if state["status"] == "implementation-stopped":
            if state.get("stop_evidence", {}).get("kind") == "delivery-ambiguous":
                raise SystemExit(
                    "ambiguous implementation delivery cannot be steered or resubmitted"
                )
            raise SystemExit(
                "implementation returned early; use steer-stage with a targeted diagnosis"
            )
        if state["status"] == "complete":
            raise SystemExit("orchestration run is already complete")
        if state["status"] != "ready":
            active = state.get("active_phase") or run_phase(state)
            raise SystemExit(f"{active} phase is still active")
        expected = state["expected_phase"]
        if args.phase != expected:
            raise SystemExit(
                f"expected phase is {expected}; refusing {args.phase} submission"
            )
        actor_name = PHASE_ACTOR[args.phase]
        actor = state[actor_name]
        live = session.buffer_state(actor["buffer"])
        live = session._bootstrap_fresh_claude_waiting(state, actor_name, live)
        if live.get("state") != "awaiting-input":
            label = "Agent 1" if actor_name == "agent1" else "Agent 2"
            raise SystemExit(
                f"{label} is {live.get('state', 'unknown')}; "
                "phase submission requires awaiting input"
            )
        prompt = _phase_prompt(state, args.phase, context, args.run_file)
        state["pending_submission"] = {
            "kind": "phase",
            "phase": args.phase,
            "actor": actor_name,
            "transcript_offset": session._transcript_offset(state, actor_name),
        }
        save_run(args.run_file, state)
        session.submit_to_agent(
            actor["buffer"],
            actor["backend"],
            prompt,
            transcript=actor["transcript"],
            transcript_offset=state["pending_submission"]["transcript_offset"],
            delivery_marker=_delivery_marker(state["stage"], args.phase),
            one_pass=args.phase == "implementation",
        )
        _finalize_pending(state)
        save_run(args.run_file, state)
    print(f"submitted stage={state['stage']} phase={args.phase} actor={actor_name}")


def run_phase(state: dict[str, Any]) -> str:
    if state["status"] in {
        "implementation-active",
        "implementation-stopped",
        "implementation-returned",
    }:
        return "implementation"
    if state["status"] == "complete":
        return "complete"
    if state["active_phase"] is not None:
        return state["active_phase"]
    return state["expected_phase"]


def finish_phase(args: argparse.Namespace) -> None:
    with run_lock(args.run_file):
        state = load_run(args.run_file)
        _require_no_pending(state)
        if state["status"] not in {"phase-active", "implementation-active"}:
            raise SystemExit("no submitted phase is awaiting completion")
        if state["active_phase"] != args.phase:
            raise SystemExit(
                f"active phase is {state['active_phase']}; refusing {args.phase} completion"
            )
        actor_name = PHASE_ACTOR[args.phase]
        live = session.buffer_state(state[actor_name]["buffer"])
        if live.get("state") != "awaiting-input":
            label = "Agent 1" if actor_name == "agent1" else "Agent 2"
            raise SystemExit(
                f"{label} is {live.get('state', 'unknown')}; "
                "phase completion requires awaiting input"
            )
        digest = _phase_evidence(state, args.phase)
        state["completions"].append(
            {
                "phase": args.phase,
                "actor": actor_name,
                "evidence_sha256": digest,
            }
        )
        state["active_phase"] = None
        if args.phase == "implementation":
            state["status"] = "implementation-returned"
            state["expected_phase"] = None
        else:
            state["status"] = "ready"
            state["expected_phase"] = PHASES[PHASES.index(args.phase) + 1]
        save_run(args.run_file, state)
    print(f"finished stage={state['stage']} phase={args.phase} actor={actor_name}")


def reconcile_submission(args: argparse.Namespace) -> None:
    with run_lock(args.run_file):
        state = load_run(args.run_file)
        pending = state["pending_submission"]
        if pending is None:
            raise SystemExit("no pending submission requires reconciliation")
        if pending["phase"] == "implementation" and (
            not args.delivered
            or not session._transcript_advanced(
                state[pending["actor"]]["transcript"],
                pending["transcript_offset"],
            )
        ):
            _freeze_pending_implementation(
                state, "implementation delivery outcome was not independently verified"
            )
            outcome = "implementation-stopped"
        elif args.delivered:
            _finalize_pending(state)
            outcome = "delivered"
        else:
            state["pending_submission"] = None
            outcome = "not-delivered"
        save_run(args.run_file, state)
    print(f"reconciled stage={state['stage']} outcome={outcome}")


def retry_delivery(args: argparse.Namespace) -> None:
    """Retry only Return for a concretely observed pending composer prompt."""
    with run_lock(args.run_file):
        state = load_run(args.run_file)
        pending = state["pending_submission"]
        if pending is None:
            raise SystemExit("only a pending submission is eligible for delivery retry")
        submission = pending

        actor = state[submission["actor"]]
        transcript = actor["transcript"]
        offset = submission["transcript_offset"]
        if submission["phase"] == "implementation":
            if session._transcript_advanced(transcript, offset):
                _finalize_pending(state)
                save_run(args.run_file, state)
                print(
                    f"delivery already observed stage={state['stage']} "
                    "phase=implementation"
                )
                return
            raise SystemExit(
                "implementation delivery cannot be retried; reconcile the pending "
                "attempt, which freezes the one-pass run without another agent contact"
            )
        if session._delivery_observed(actor["buffer"], transcript, offset):
            _finalize_pending(state)
            save_run(args.run_file, state)
            print(
                f"delivery already observed stage={state['stage']} "
                f"phase={submission['phase']}"
            )
            return

        live = session.buffer_state(actor["buffer"])
        if live.get("state") != "awaiting-input":
            raise SystemExit(
                f"{submission['actor']} is {live.get('state', 'unknown')}; "
                "delivery retry requires awaiting input"
            )
        marker = _delivery_marker(state["stage"], submission["phase"])
        if not session.pending_prompt_contains(
            actor["buffer"], actor["backend"], marker
        ):
            raise SystemExit(
                "exact pending phase marker is not present in the current composer; "
                "refusing delivery retry"
            )
        session.send_return_to_agent(actor["buffer"], actor["backend"])
        if not session._wait_for_delivery(
            actor["buffer"],
            transcript,
            offset,
            session.DELIVERY_RETRY_WAIT_SECONDS,
        ):
            raise EmacsClientError(
                "submission delivery was not acknowledged after retrying only the "
                "submit keystroke"
            )
        _finalize_pending(state)
        save_run(args.run_file, state)
    print(
        f"retried delivery stage={state['stage']} "
        f"phase={submission['phase']} actor={submission['actor']}"
    )


def restart_phase(args: argparse.Namespace) -> None:
    """Restart a non-implementation phase whose prior actor returned no output."""
    prompt_path = Path(args.prompt_file)
    try:
        context = prompt_path.read_text(encoding="utf-8")
    except OSError as error:
        raise SystemExit(f"cannot read prompt file {prompt_path}: {error}") from None
    with run_lock(args.run_file):
        state = load_run(args.run_file)
        _require_no_pending(state)
        if state["status"] != "phase-active" or state["active_phase"] == "implementation":
            raise SystemExit("only an active spec or plan phase can be restarted")
        phase = state["active_phase"]
        actor_name = PHASE_ACTOR[phase]
        actor = state[actor_name]
        submission = state["submissions"][-1]
        returned = session.transcript_messages(
            Path(actor["transcript"]), offset=submission["transcript_offset"]
        )
        if returned:
            raise SystemExit(
                "active phase already returned assistant output; refusing restart"
            )
        live = session.buffer_state(actor["buffer"])
        if live.get("state") != "awaiting-input":
            raise SystemExit(
                f"{actor_name} is {live.get('state', 'unknown')}; "
                "phase restart requires a fresh waiting session"
            )
        marker = _delivery_marker(state["stage"], phase)
        old_transcript = str(Path(actor["transcript"]).resolve())
        fresh = session.agent_transcript_path(actor["buffer"], actor["backend"])
        marker_offset = session._user_marker_offset(fresh, marker) if fresh else None
        if marker_offset is None:
            if fresh and str(Path(fresh).resolve()) == old_transcript:
                raise SystemExit(
                    "fixed actor still points at the failed transcript; "
                    "phase restart requires a fresh session"
                )
            starting_offset = session._transcript_offset(
                {actor_name: {"transcript": fresh or "/nonexistent"}}, actor_name
            )
            prompt = _phase_prompt(state, phase, context)
            session.submit_to_agent(
                actor["buffer"],
                actor["backend"],
                prompt,
                transcript=fresh or "/nonexistent",
                transcript_offset=starting_offset,
                delivery_marker=marker,
            )
            fresh = session._wait_for_transcript_path(
                actor["buffer"], actor["backend"], session.DELIVERY_RETRY_WAIT_SECONDS
            )
            if not fresh:
                raise EmacsClientError(
                    "fresh phase delivery was acknowledged but its transcript path "
                    "is not yet available"
                )
            marker_offset = starting_offset
        if str(Path(fresh).resolve()) == old_transcript:
            raise SystemExit("phase restart did not acquire a fresh transcript")
        actor["transcript"] = fresh
        submission["transcript_offset"] = marker_offset
        save_run(args.run_file, state)
    print(
        f"restarted stage={state['stage']} phase={phase} actor={actor_name} "
        "with fresh transcript"
    )


def run_status(args: argparse.Namespace) -> None:
    state = load_run(args.run_file)
    display = {
        "stage": state["stage"],
        "phase": run_phase(state),
        "status": state["status"],
        "pending_reconciliation": state["pending_submission"] is not None,
    }
    if getattr(args, "json", False):
        print(session.json_for_display(display))
    else:
        print(
            f"stage={display['stage']} phase={display['phase']} "
            f"status={display['status']} "
            f"pending-reconciliation={str(display['pending_reconciliation']).lower()}"
        )


def complete_stage(args: argparse.Namespace) -> None:
    with run_lock(args.run_file):
        state = load_run(args.run_file)
        _require_no_pending(state)
        if state["status"] == "implementation-active":
            raise SystemExit("implementation has not returned")
        if state["status"] != "implementation-returned":
            raise SystemExit("stage implementation is not active")
        digest = _evidence_digest(args.evidence_file)
        state["acceptance_evidence"] = {"sha256": digest}
        state["status"] = "complete"
        state["expected_phase"] = None
        save_run(args.run_file, state)
    print(f"completed stage={state['stage']}")


def transcript_cmd(args: argparse.Namespace) -> None:
    state = load_run(args.run_file)
    if state["status"] in {
        "implementation-active",
        "implementation-stopped",
        "implementation-returned",
    }:
        raise SystemExit("transcripts are unavailable during implementation")
    transcript = state[args.actor].get("transcript")
    if not transcript:
        raise SystemExit(f"{args.actor} transcript was not recorded in the run")
    messages = session.transcript_messages(Path(transcript), args.since)
    if args.last:
        messages = messages[-args.last :]
    print(json.dumps(messages, ensure_ascii=False, indent=2))


def stage_return(args: argparse.Namespace) -> None:
    """Record and print a genuine incomplete implementation return."""
    with run_lock(args.run_file):
        state = load_run(args.run_file)
        if state["status"] != "implementation-active":
            raise SystemExit("stage implementation is not active")
        live = session.buffer_state(state["agent1"]["buffer"])
        if live.get("state") != "awaiting-input":
            raise SystemExit(
                f"Agent 1 is {live.get('state', 'unknown')}; "
                "stage return is available only after Agent 1 awaits input"
            )
        text = _latest_implementation_return(state)
        lines = [line.strip() for line in text.splitlines() if line.strip()]
        marker = f"STAGE COMPLETE: {state['stage']}"
        if lines and lines[-1] == marker:
            raise SystemExit("stage return is complete; use finish-phase")
        state["status"] = "implementation-stopped"
        state["active_phase"] = None
        state["stop_evidence"] = {
            "kind": "agent-return",
            "sha256": hashlib.sha256(text.encode("utf-8")).hexdigest()
        }
        save_run(args.run_file, state)
    print(text)


def steer_stage(args: argparse.Namespace) -> None:
    """Send one novel, targeted response to a genuine incomplete stage return."""
    prompt_path = Path(args.prompt_file)
    if not prompt_path.exists():
        raise SystemExit(f"prompt file does not exist: {prompt_path}")
    if stat.S_IMODE(prompt_path.stat().st_mode) != 0o600:
        raise SystemExit("steer-stage prompt file must have mode 0600")
    context = prompt_path.read_text(encoding="utf-8").strip()
    fields = {}
    for name in ("Obstacle", "Resolution", "Whole-stage direction"):
        match = re.search(rf"(?m)^{re.escape(name)}:\s*(.+)$", context)
        if match:
            fields[name] = match.group(1).strip()
    if set(fields) != {"Obstacle", "Resolution", "Whole-stage direction"} or any(
        len(value) < 20 for value in fields.values()
    ):
        raise SystemExit(
            "steer-stage requires Obstacle, Resolution, and Whole-stage direction; "
            "generic continuation is not accepted"
        )
    prompt_sha = hashlib.sha256(context.encode("utf-8")).hexdigest()
    with run_lock(args.run_file):
        state = load_run(args.run_file)
        if state["status"] != "implementation-stopped":
            raise SystemExit("stage steering requires a recorded incomplete return")
        evidence = state["stop_evidence"]
        if evidence.get("kind") != "agent-return":
            raise SystemExit("ambiguous delivery is not an Agent 1 return and cannot be steered")
        prior = state.get("steering_prompts", [])
        if any(entry["stop_sha256"] == evidence["sha256"] for entry in prior):
            raise SystemExit("this incomplete return already received a steering attempt")
        if any(entry["prompt_sha256"] == prompt_sha for entry in prior):
            raise SystemExit("this steering message was already sent")
        live = session.buffer_state(state["agent1"]["buffer"])
        if live.get("state") != "awaiting-input":
            raise SystemExit(
                f"Agent 1 is {live.get('state', 'unknown')}; steering requires awaiting input"
            )
        prompt = STEERING_CONTRACT.format(stage=state["stage"], context=context)
        state.setdefault("steering_prompts", []).append(
            {
                "prompt_sha256": prompt_sha,
                "stop_sha256": evidence["sha256"],
            }
        )
        save_run(args.run_file, state)
        session.submit_to_agent(
            state["agent1"]["buffer"],
            state["agent1"]["backend"],
            prompt,
            transcript=state["agent1"]["transcript"],
            transcript_offset=session._transcript_offset(state, "agent1"),
            delivery_marker=f"TARGETED WHOLE-STAGE STEERING\n\nThis message responds",
            one_pass=True,
        )
        state["status"] = "implementation-active"
        state["active_phase"] = "implementation"
        state["stop_evidence"] = None
        save_run(args.run_file, state)
    print(f"steered stage={state['stage']} actor=agent1")


def _latest_implementation_return(state: dict[str, Any]) -> str:
    submission = state["submissions"][-1]
    messages = session.transcript_messages(
        Path(state["agent1"]["transcript"]),
        offset=submission["transcript_offset"],
    )
    if not messages:
        raise SystemExit("no bounded implementation return is available")
    return messages[-1]["text"]


def switch_model(args: argparse.Namespace) -> None:
    """Reject model-turn continuation for a one-pass implementation."""
    raise SystemExit("one-pass implementation cannot be continued on another model")


def git_status(repo: Path) -> dict[str, str]:
    def git(*argv: str) -> str:
        proc = subprocess.run(
            ["git", "-C", str(repo), *argv],
            text=True,
            capture_output=True,
            check=False,
        )
        if proc.returncode != 0:
            return proc.stderr.strip()
        return proc.stdout.strip()

    return {
        "status": git("status", "--short", "--branch"),
        "head": git("log", "--oneline", "-1"),
    }


def status(args: argparse.Namespace) -> dict[str, Any]:
    state = load_run(args.run_file)
    result: dict[str, Any] = {
        "run": {
            "stage": state["stage"],
            "phase": run_phase(state),
            "status": state["status"],
            "pending_reconciliation": state["pending_submission"] is not None,
        }
    }
    if state["status"] in {
        "implementation-active",
        "implementation-stopped",
        "implementation-returned",
    }:
        result["agent1"] = session.buffer_state(state["agent1"]["buffer"])
        progress = latest_progress(args.run_file)
        if progress is not None:
            result["progress"] = progress
        return result

    result["repo"] = git_status(Path(state["repo"]))
    for actor in ("agent1", "agent2"):
        result[actor] = session.buffer_state(state[actor]["buffer"])
        transcript = state[actor].get("transcript")
        if not transcript:
            continue
        p = Path(transcript)
        result[f"{actor}_transcript"] = {
            "path": str(p),
            "mtime": p.stat().st_mtime if p.exists() else None,
            "latest": session.transcript_messages(p, args.since)[-1:] if p.exists() else [],
        }
    return result


def status_cmd(args: argparse.Namespace) -> None:
    current = status(args)
    if args.json:
        print(session.json_for_display(current))
        return
    if "repo" in current:
        print("Repo:")
        print(current["repo"]["status"])
        print(f"HEAD {current['repo']['head']}")
    if "run" in current:
        item = current["run"]
        print(
            f"Run: stage={item['stage']} phase={item['phase']} "
            f"status={item['status']}"
        )
    for role in ("agent1", "agent2"):
        if role in current:
            item = current[role]
            print(f"{role.title()}: {item['state']} — {item['buffer']}")
    if "progress" in current:
        item = current["progress"]
        print(f"Progress: [{item['age_s']}s ago, {item['lines']} lines] {item['latest'][:200]}")
    for key in ("agent1_transcript", "agent2_transcript"):
        if key in current:
            item = current[key]
            latest = item["latest"][0] if item["latest"] else None
            if latest:
                text = latest["text"].replace("\n", " ")
                if len(text) > 180:
                    text = text[:177] + "..."
                print(f"{key}: mtime={item['mtime']} latest={latest['kind']} {text}")
            else:
                print(f"{key}: mtime={item['mtime']} latest=<none>")


def reviewer_verdict(transcript_key: str, latest: dict[str, str]) -> str | None:
    """Return a terminal reviewer verdict, never verdict vocabulary in chatter."""
    if transcript_key not in {"reviewer_transcript", "agent2_transcript"}:
        return None
    if latest.get("kind") not in {"assistant", "complete", "message"}:
        return None
    first_line = next(
        (line.strip() for line in latest.get("text", "").splitlines() if line.strip()),
        "",
    )
    if first_line in {"IMPLEMENTATION-READY", "NOT READY"}:
        return first_line
    return None


def watch(args: argparse.Namespace) -> None:
    last_rendered = ""
    while True:
        try:
            current = status(args)
        except EmacsClientError as error:
            message = " ".join(str(error).split())
            error_rendered = (
                session.json_for_display({"monitor_error": message})
                if args.json
                else f"monitor-error={message}"
            )
            if error_rendered != last_rendered:
                print(error_rendered, flush=True)
                last_rendered = error_rendered
            time.sleep(args.interval)
            current = status(args)
        if args.json:
            rendered = session.json_for_display(current)
        else:
            parts = []
            if "repo" in current:
                parts.append(current["repo"]["head"])
            if "run" in current:
                item = current["run"]
                parts.append(f"stage={item['stage']} phase={item['phase']}")
            for role in ("agent1", "agent2", "planner", "reviewer"):
                if role in current:
                    parts.append(f"{role}={current[role]['state']}")
            if "progress" in current:
                item = current["progress"]
                parts.append(
                    f"progress[{item['age_s']}s]={item['latest'][:120]}"
                )
            for key in (
                "agent1_transcript",
                "agent2_transcript",
                "planner_transcript",
                "reviewer_transcript",
            ):
                if key in current:
                    latest = (
                        current[key]["latest"][0]
                        if current[key]["latest"]
                        else None
                    )
                    if latest:
                        verdict_text = reviewer_verdict(key, latest)
                        verdict = f" {verdict_text}" if verdict_text else ""
                        parts.append(
                            f"{key}_mtime={current[key]['mtime']}{verdict}"
                        )
            rendered = " | ".join(parts)
        if rendered != last_rendered:
            print(rendered, flush=True)
            last_rendered = rendered
        time.sleep(args.interval)


def _raw_transcript_texts(path: Path, kinds: set[str]) -> list[tuple[str, str]]:
    out: list[tuple[str, str]] = []
    if not path.exists():
        return out
    for line in path.read_text(errors="replace").splitlines():
        try:
            d = json.loads(line)
        except Exception:
            continue
        if d.get("type") not in kinds:
            continue
        c = d.get("message", {}).get("content")
        if isinstance(c, str):
            txt = c
        elif isinstance(c, list):
            txt = " ".join(
                (x.get("text") or "") for x in c if isinstance(x, dict)
            )
        else:
            txt = ""
        if txt.strip():
            out.append((d["type"], txt))
    return out


def ask_cmd(args: argparse.Namespace) -> None:
    """Post a direct question to a run actor, confirm it landed, print the reply.

    Direct communication for supervision. Posting is confirmed from the actor's
    transcript (the message text appears there), never inferred from the
    terminal screen. Claude sessions queue a message until the current turn
    ends; pass --interrupt to end the turn first (ESC to the eat terminal).
    """
    state = load_run(args.run_file)
    actor = state[args.actor]
    text = Path(args.prompt_file).read_text()
    marker = text.strip().splitlines()[0][:80]
    transcript = Path(actor["transcript"])
    before = len(_raw_transcript_texts(transcript, {"assistant"}))
    if args.interrupt:
        session.run_emacs_eval(
            f'(with-current-buffer {session.elisp_string(actor["buffer"])} '
            f'(when (and (boundp (quote eat-terminal)) eat-terminal) '
            f'(eat-term-send-string eat-terminal "\\e")) t)'
        )
        time.sleep(4)
    session.run_emacs_eval(
        f'(progn (agent-submit {session.elisp_string(text)} '
        f'(get-buffer {session.elisp_string(actor["buffer"])})) t)'
    )
    deadline = time.time() + args.timeout
    posted = False
    while time.time() < deadline:
        if any(marker in t for k, t in _raw_transcript_texts(transcript, {"user"})):
            posted = True
            break
        time.sleep(5)
    print("posted" if posted else "NOT CONFIRMED POSTED (still queued or lost)")
    while time.time() < deadline:
        replies = _raw_transcript_texts(transcript, {"assistant"})
        if len(replies) > before:
            print("reply:")
            print(replies[-1][1])
            return
        time.sleep(5)
    print("no reply within timeout")


def interrupt_cmd(args: argparse.Namespace) -> None:
    """Send ESC to a run actor's eat terminal to end its current turn."""
    state = load_run(args.run_file)
    actor = state[args.actor]
    session.run_emacs_eval(
        f'(with-current-buffer {session.elisp_string(actor["buffer"])} '
        f'(when (and (boundp (quote eat-terminal)) eat-terminal) '
        f'(eat-term-send-string eat-terminal "\\e")) t)'
    )
    print(f"interrupt sent to {actor['buffer']}")


def add_status_args(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--run-file", required=True)
    parser.add_argument("--since")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)

    p = sub.add_parser("state", help="Inspect one fixed run actor buffer")
    p.add_argument("--run-file", required=True)
    p.add_argument("--actor", required=True, choices=("agent1", "agent2"))
    p.add_argument("--json", action="store_true", help="Emit structured JSON")
    p.set_defaults(func=state_cmd)

    p = sub.add_parser("init-run", help="Create a guarded stage orchestration run")
    p.add_argument("--run-file", required=True)
    p.add_argument("--repo", required=True)
    p.add_argument("--stage", required=True)
    p.add_argument("--agent1-buffer", required=True)
    p.add_argument(
        "--agent1-backend", required=True, choices=sorted(session.VALID_BACKENDS)
    )
    p.add_argument("--agent1-transcript", required=True)
    p.add_argument("--agent2-buffer", required=True)
    p.add_argument(
        "--agent2-backend", required=True, choices=sorted(session.VALID_BACKENDS)
    )
    p.add_argument("--agent2-transcript", required=True)
    p.add_argument("--adopt-implementation", action="store_true")
    p.add_argument("--spec-commit")
    p.add_argument("--plan-commit")
    p.add_argument("--reviews-complete", action="store_true")
    p.set_defaults(func=create_run)

    p = sub.add_parser("run-status", help="Inspect guarded stage run state")
    p.add_argument("--run-file", required=True)
    p.add_argument("--json", action="store_true", help="Emit structured JSON")
    p.set_defaults(func=run_status)

    p = sub.add_parser("submit", help="Submit the next guarded stage phase")
    p.add_argument("--run-file", required=True)
    p.add_argument("--phase", required=True, choices=PHASES)
    p.add_argument("--prompt-file", required=True)
    p.set_defaults(func=submit)

    p = sub.add_parser(
        "finish-phase", help="Record one phase returned by its fixed top-level actor"
    )
    p.add_argument("--run-file", required=True)
    p.add_argument("--phase", required=True, choices=PHASES)
    p.set_defaults(func=finish_phase)

    p = sub.add_parser(
        "reconcile-submission",
        help="Resolve a submission whose delivery outcome is ambiguous",
    )
    p.add_argument("--run-file", required=True)
    outcome = p.add_mutually_exclusive_group(required=True)
    outcome.add_argument("--delivered", action="store_true")
    outcome.add_argument("--not-delivered", action="store_false", dest="delivered")
    p.set_defaults(func=reconcile_submission)

    p = sub.add_parser(
        "retry-delivery",
        help="Retry only Return when the exact phase prompt remains in the composer",
    )
    p.add_argument("--run-file", required=True)
    p.set_defaults(func=retry_delivery)

    p = sub.add_parser(
        "restart-phase",
        help="Restart a returned-empty review or authoring phase in a fresh session",
    )
    p.add_argument("--run-file", required=True)
    p.add_argument("--prompt-file", required=True)
    p.set_defaults(func=restart_phase)

    p = sub.add_parser(
        "complete-stage", help="Close a run after stage-final verification"
    )
    p.add_argument("--run-file", required=True)
    p.add_argument("--evidence-file", required=True)
    p.set_defaults(func=complete_stage)

    p = sub.add_parser("transcript", help="Extract one fixed run actor's transcript")
    p.add_argument("--run-file", required=True)
    p.add_argument("--actor", required=True, choices=("agent1", "agent2"))
    p.add_argument("--since")
    p.add_argument("--last", type=int, default=5)
    p.set_defaults(func=transcript_cmd)

    p = sub.add_parser(
        "stage-return",
        help="Read only the latest bounded return from awaiting Agent 1",
    )
    p.add_argument("--run-file", required=True)
    p.set_defaults(func=stage_return)

    ask_p = sub.add_parser("ask", help="Post a direct question to an actor, confirm it posted, print the reply")

    ask_p.add_argument("--run-file", required=True)

    ask_p.add_argument("--actor", choices=["agent1", "agent2"], default="agent1")

    ask_p.add_argument("--prompt-file", required=True)

    ask_p.add_argument("--interrupt", action="store_true", help="End the actor's current turn first (ESC to eat)")

    ask_p.add_argument("--timeout", type=int, default=300)

    ask_p.set_defaults(func=ask_cmd)

    int_p = sub.add_parser("interrupt", help="Send ESC to an actor's eat terminal")

    int_p.add_argument("--run-file", required=True)

    int_p.add_argument("--actor", choices=["agent1", "agent2"], default="agent1")

    int_p.set_defaults(func=interrupt_cmd)


    p = sub.add_parser(
        "steer-stage",
        help="Send targeted whole-stage steering after a genuine incomplete return",
    )
    p.add_argument("--run-file", required=True)
    p.add_argument("--prompt-file", required=True)
    p.set_defaults(func=steer_stage)

    p = sub.add_parser("status", help="Collect repo, buffer, and transcript status once")
    add_status_args(p)
    p.add_argument("--json", action="store_true", help="Emit structured JSON")
    p.set_defaults(func=status_cmd)

    p = sub.add_parser("watch", help="Poll repo, buffer, and transcript status")
    add_status_args(p)
    p.add_argument("--interval", type=float, default=20.0)
    p.add_argument("--json", action="store_true", help="Emit structured JSON on each changed poll")
    p.set_defaults(func=watch)

    args = parser.parse_args(argv)
    try:
        args.func(args)
    except EmacsClientError as error:
        parser.exit(1, f"{error}\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
