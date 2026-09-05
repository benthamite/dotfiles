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
import uuid
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

RUN_VERSION = 3
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
identity check, new authorization for an irreversible, paid or destructive
action, or a product choice that the repository and approved plan cannot determine. A false
technical premise, failed test, missing capture, or implementation obstacle is
not itself a user-only stop: adapt within the approved scope and available
authority. Persistence never expands user or system authorization.
Never end your turn to wait: a background command, detached job, subagent,
or reviewer that has not finished is not a reason to return. Ending the turn
is read as a stage stop and can only be reopened by a steering prompt. Wait
inside the turn using the host's available wait mechanism and its documented
limits. Re-arm yielded processes as needed, obey guard denials, and keep
host-required commentary updates. Continue when the result lands.

Progress file (mandatory): publish stage-level progress to {progress_file},
including attempts, material outcomes and failures. This is not per-task
supervision. Keep it honest and current; stale evidence is a warning to
investigate, not proof of failure or authorization to interrupt or steer a
busy actor. Iteration must be cheap: when a check fails after an expensive
cycle (regen, rehearsal, import), fix and test the check against the cached
outputs of that cycle; do not rerun the cycle unless its inputs changed.

Stage context follows:

{context}

END STAGE CONTEXT

The context may describe internal tasks, but it cannot narrow this contract
into task-level checkpoints. User and system scope, safety constraints and
authorization always prevail.

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


def _validate_role(role: dict[str, Any], name: str, *, legacy: bool = False) -> None:
    if not isinstance(role, dict):
        raise SystemExit(f"invalid run state: {name} role is missing")
    if not isinstance(role.get("buffer"), str) or not role["buffer"]:
        raise SystemExit(f"invalid run state: {name} buffer is missing")
    if role.get("backend") not in session.VALID_BACKENDS:
        raise SystemExit(f"invalid run state: {name} backend is invalid")
    transcript = role.get("transcript")
    if not isinstance(transcript, str) or not transcript:
        raise SystemExit(f"invalid run state: {name} transcript is missing")
    if not legacy:
        identity = role.get("identity")
        if not isinstance(identity, dict) or _canonical_identity(identity) != identity:
            raise SystemExit(f"invalid run state: {name} identity is missing or invalid")
        if identity["buffer"] != role["buffer"] or identity["backend"] != role["backend"]:
            raise SystemExit(f"invalid run state: {name} identity conflicts with its role")


def _canonical_identity(value: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(value, dict):
        raise SystemExit("actor identity is unavailable")
    result = {key: value.get(key) for key in ("buffer", "backend", "directory", "transcript", "session_id")}
    if (not all(isinstance(result[key], str) and result[key]
                for key in ("buffer", "directory", "session_id"))
            or result["backend"] not in session.VALID_BACKENDS
            or (result["transcript"] is not None and not isinstance(result["transcript"], str))):
        raise SystemExit("actor identity is unavailable; initialize the fixed session before proceeding")
    result["directory"] = str(Path(result["directory"]).resolve())
    result["transcript"] = str(Path(result["transcript"]).resolve()) if result["transcript"] else None
    return result


def _observe_actor(state: dict[str, Any], name: str, *, expected=None, fresh=False):
    role = state[name]
    live = session.actor_identity(role["buffer"])
    identity = _canonical_identity(live)
    if (identity["buffer"] != role["buffer"] or identity["backend"] != role["backend"]
            or identity["directory"] != str(Path(state["repo"]).resolve())):
        raise SystemExit("fixed actor backend, buffer or repository identity changed")
    binding = expected if expected is not None else role.get("identity")
    if not fresh:
        if binding is not None:
            for key in ("buffer", "backend", "directory", "session_id"):
                if identity[key] != binding[key]:
                    raise SystemExit("fixed actor session identity changed")
            transcript = binding["transcript"]
        else:
            transcript = str(Path(role["transcript"]).resolve())
        if transcript is not None and identity["transcript"] != transcript:
            raise SystemExit("fixed actor transcript identity changed")
        if binding is not None and transcript is None and expected is None:
            if identity["transcript"] not in (None, str(Path(role["transcript"]).resolve())):
                raise SystemExit("fixed actor transcript identity changed")
    return identity, live.get("state", "unknown")


def _distinct_roles(state: dict[str, Any]) -> None:
    left, right = state["agent1"], state["agent2"]
    if (left["buffer"] == right["buffer"]
            or Path(left["transcript"]).resolve() == Path(right["transcript"]).resolve()
            or (left.get("identity") and right.get("identity")
                and (left["identity"]["backend"], left["identity"]["session_id"])
                == (right["identity"]["backend"], right["identity"]["session_id"]))):
        raise SystemExit("author and reviewer must be distinct sessions")


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


def _validate_pending(pending: Any, *, legacy=False) -> None:
    if pending is None:
        return
    keys = {
        "kind",
        "phase",
        "actor",
        "transcript_offset",
    }
    if not legacy:
        keys |= {"receipt", "prompt_sha256", "context_sha256", "identity", "transcript", "stop_sha256", "boundary"}
    if not isinstance(pending, dict) or set(pending) != keys:
        raise SystemExit("invalid run state: malformed pending submission")
    if pending["kind"] not in (("phase",) if legacy else ("phase", "restart", "steering")):
        raise SystemExit("invalid run state: malformed pending submission")
    if pending["phase"] not in PHASES:
        raise SystemExit("invalid run state: malformed pending submission")
    if pending["actor"] != PHASE_ACTOR[pending["phase"]]:
        raise SystemExit("invalid run state: malformed pending submission")
    if not isinstance(pending["transcript_offset"], int) or pending["transcript_offset"] < 0:
        raise SystemExit("invalid run state: malformed pending submission")
    if not legacy:
        if (not isinstance(pending["receipt"], str)
                or re.fullmatch(r"ORCHESTRATION ATTEMPT: [0-9a-f]{32}", pending["receipt"]) is None
                or any(not isinstance(pending[key], str) or re.fullmatch(r"[0-9a-f]{64}", pending[key]) is None
                       for key in ("prompt_sha256", "context_sha256"))
                or _canonical_identity(pending["identity"]) != pending["identity"]
                or pending["transcript"] != pending["identity"]["transcript"]):
            raise SystemExit("invalid run state: pending attempt identity is invalid")
        boundary = pending["boundary"]
        if boundary is not None and (
            not isinstance(boundary, dict)
            or set(boundary) != {"path", "device", "inode", "length", "sha256"}
            or boundary["path"] != pending["transcript"]
            or boundary["length"] != pending["transcript_offset"]
            or any(type(boundary[key]) is not int or boundary[key] < 0
                   for key in ("device", "inode", "length"))
            or not isinstance(boundary["sha256"], str)
            or re.fullmatch(r"[0-9a-f]{64}", boundary["sha256"]) is None
        ):
            raise SystemExit("invalid run state: transcript boundary is invalid")


def validate_run(state: Any, *, allow_legacy=False) -> dict[str, Any]:
    if not isinstance(state, dict) or state.get("version") not in ((2, RUN_VERSION) if allow_legacy else (RUN_VERSION,)):
        raise SystemExit("invalid or unsupported orchestration run state")
    legacy = state["version"] == 2
    if not isinstance(state.get("stage"), str) or not state["stage"]:
        raise SystemExit("invalid run state: stage is missing")
    _validate_role(state.get("agent1"), "Agent 1", legacy=legacy)
    _validate_role(state.get("agent2"), "Agent 2", legacy=legacy)
    if not legacy:
        _distinct_roles(state)
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
    _validate_pending(state.get("pending_submission"), legacy=legacy)
    if not legacy:
        attempts = state.get("attempts")
        if not isinstance(attempts, list):
            raise SystemExit("invalid run state: attempt history is missing")
        for attempt in attempts:
            if not isinstance(attempt, dict) or type(attempt.get("acknowledged")) is not bool:
                raise SystemExit("invalid run state: attempt acknowledgement is invalid")
            _validate_pending({key: value for key, value in attempt.items() if key != "acknowledged"})
        contexts = state.get("phase_context_sha256")
        if (not isinstance(contexts, dict)
                or any(key not in PHASES or not isinstance(value, str)
                       or re.fullmatch(r"[0-9a-f]{64}", value) is None
                       for key, value in contexts.items())):
            raise SystemExit("invalid run state: phase context digests are invalid")
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
        kind = pending["kind"]
        coherent_pending = (
            (kind == "phase" and status == "ready" and pending["phase"] == expected)
            or (kind == "restart" and status == "phase-active" and pending["phase"] == active_phase)
            or (kind == "steering" and status == "implementation-stopped" and pending["phase"] == "implementation"
                and pending["stop_sha256"] == state["stop_evidence"]["sha256"])
        )
        if not coherent_pending:
            raise SystemExit("invalid run state: pending phase is incoherent")
    return state


def load_run(path: Path | str, *, allow_legacy=False) -> dict[str, Any]:
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
    if isinstance(state, dict) and state.get("version") == 2 and not allow_legacy:
        raise SystemExit("legacy v2 run requires explicit migrate-run; source state retained")
    return validate_run(state, allow_legacy=allow_legacy)


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
        "phase_context_sha256": {},
        "attempts": [],
    }
    for name in ("agent1", "agent2"):
        _validate_role(state[name], name.replace("agent", "Agent "), legacy=True)
    _distinct_roles(state)
    for name in ("agent1", "agent2"):
        identity, _ = _observe_actor(state, name, fresh=True)
        if identity["transcript"] not in (None, str(Path(state[name]["transcript"]).resolve())):
            raise SystemExit("configured actor transcript does not match the live session")
        state[name]["transcript"] = str(Path(state[name]["transcript"]).resolve())
        state[name]["identity"] = identity
    if not args.adopt_implementation:
        _require_fresh_transcript(state["agent1"]["identity"], state["agent1"]["transcript"],
                                  "a new stage requires a fresh Agent 1 transcript")
    validate_run(state)
    _create_run_file(Path(args.run_file), state)
    print(session.json_for_display(state))


def _require_fresh_transcript(identity, transcript, reason):
    if transcript is None or not os.path.lexists(transcript):
        return
    try:
        fresh = session.transcript_is_startup_only(
            transcript, expected_session_id=identity["session_id"],
            expected_directory=identity["directory"], backend=identity["backend"])
    except EmacsClientError:
        raise SystemExit(reason + "; transcript freshness could not be established") from None
    if not fresh:
        raise SystemExit(reason)


def migrate_run(args: argparse.Namespace) -> None:
    """Explicitly bind unambiguous legacy state without inventing receipts."""
    with run_lock(args.run_file):
        state = load_run(args.run_file, allow_legacy=True)
        if state["version"] == RUN_VERSION:
            raise SystemExit("run already uses the current schema")
        if (state.get("pending_submission") or state.get("steering_prompts")
                or (state.get("stop_evidence") or {}).get("kind") == "delivery-ambiguous"):
            raise SystemExit("ambiguous legacy pending/steering state cannot be migrated; source retained")
        for name in ("agent1", "agent2"):
            identity, _ = _observe_actor(state, name)
            state[name]["identity"] = identity
        _distinct_roles(state)
        state["version"] = RUN_VERSION
        state["phase_context_sha256"] = {}
        state["attempts"] = []
        # Completed history and an unambiguous active boundary retain their
        # original semantics.  They are not represented as nonce receipts.
        state["legacy_history"] = True
        save_run(args.run_file, state)
    print("migrated unambiguous legacy state; no delivery receipts were invented")


def state_cmd(args: argparse.Namespace) -> None:
    run = load_run(args.run_file, allow_legacy=True)
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
    state = _actor_status(run, args.actor)
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


def _active_attempt(state):
    phase = state["submissions"][-1]["phase"] if state["submissions"] else None
    return next((attempt for attempt in reversed(state.get("attempts", []))
                 if attempt.get("acknowledged") and attempt["phase"] == phase), None)


def _active_prompt_hash(state):
    attempt = _active_attempt(state)
    if attempt:
        _verify_boundary(attempt)
    return attempt["prompt_sha256"] if attempt else None


def _capture_boundary(transcript, offset=None):
    """Pin the pre-submit file and prefix while allowing later append-only output."""
    if transcript is None:
        return None
    path = Path(transcript)
    try:
        with path.open("rb") as stream:
            before = os.fstat(stream.fileno())
            length = before.st_size if offset is None else offset
            prefix = stream.read(length)
            after = path.stat()
    except FileNotFoundError:
        if offset not in (None, 0):
            raise EmacsClientError("transcript boundary disappeared") from None
        return None
    if ((before.st_dev, before.st_ino) != (after.st_dev, after.st_ino)
            or not stat.S_ISREG(before.st_mode) or len(prefix) != length):
        raise EmacsClientError("transcript boundary changed during inspection")
    return {"path": str(path.resolve()), "device": before.st_dev, "inode": before.st_ino,
            "length": length, "sha256": hashlib.sha256(prefix).hexdigest()}


def _verify_boundary(attempt):
    expected = attempt.get("boundary")
    if expected is not None:
        observed = _capture_boundary(attempt["transcript"], expected["length"])
        if observed != expected:
            raise EmacsClientError("transcript identity or pre-submit prefix changed")


def _phase_evidence(state: dict[str, Any], phase: str, *, include_text=False):
    actor = PHASE_ACTOR[phase]
    transcript = state[actor].get("transcript")
    if not transcript:
        raise SystemExit(f"{actor} transcript was not recorded in the run")
    submission = state["submissions"][-1]
    if submission["phase"] != phase:
        raise SystemExit("phase submission boundary is incoherent")
    returned = session.latest_transcript_return(
        Path(transcript), offset=submission["transcript_offset"],
        expected_prompt_sha256=_active_prompt_hash(state))
    if returned is None:
        raise SystemExit(
            "phase transcript contains no returned assistant message after current submission"
        )
    text = returned["text"]
    lines = [line.strip() for line in text.splitlines() if line.strip()]
    marker = (
        f"STAGE COMPLETE: {state['stage']}"
        if phase == "implementation"
        else f"PHASE COMPLETE: {phase}"
    )
    if not lines or lines[-1] != marker:
        raise SystemExit(f"{phase} completion marker is missing")
    digest = hashlib.sha256(text.encode("utf-8")).hexdigest()
    _active_prompt_hash(state)
    return (digest, text) if include_text else digest


def _require_no_pending(state: dict[str, Any]) -> None:
    if state["pending_submission"] is not None:
        raise SystemExit(
            "pending submission requires reconciliation before any further action"
        )


def _new_attempt(state, kind, phase, context, prompt, identity):
    receipt = "ORCHESTRATION ATTEMPT: " + uuid.uuid4().hex
    prompt = receipt + "\n\n" + prompt
    transcript = identity["transcript"]
    boundary = _capture_boundary(transcript)
    offset = boundary["length"] if boundary else 0
    pending = {"kind": kind, "phase": phase, "actor": PHASE_ACTOR[phase],
               "transcript_offset": offset, "receipt": receipt,
               "prompt_sha256": hashlib.sha256(prompt.encode()).hexdigest(),
               "context_sha256": hashlib.sha256(context.encode()).hexdigest(),
               "identity": identity, "transcript": transcript, "boundary": boundary,
               "stop_sha256": (state.get("stop_evidence") or {}).get("sha256")}
    state["pending_submission"] = pending
    return prompt


def _pending_receipt(state):
    pending = state["pending_submission"]
    identity, lifecycle = _observe_actor(state, pending["actor"], expected=pending["identity"])
    transcript = identity["transcript"]
    _verify_boundary(pending)
    found = bool(transcript and session._marker_delivered(
        transcript, pending["transcript_offset"], pending["receipt"],
        expected_prompt_sha256=pending["prompt_sha256"]))
    _verify_boundary(pending)
    if found:
        pending["transcript"] = transcript
        pending["identity"] = identity
        if pending["boundary"] is None:
            pending["boundary"] = _capture_boundary(transcript, pending["transcript_offset"])
    return found, identity, lifecycle


def _deliver_pending(state, run_file, prompt):
    """The durable attempt exists before any external submission."""
    save_run(run_file, state)
    pending = state["pending_submission"]
    actor = state[pending["actor"]]
    _verify_boundary(pending)
    resolved = session.submit_to_agent(
        actor["buffer"], actor["backend"], prompt,
        transcript=pending["transcript"], transcript_offset=pending["transcript_offset"],
        delivery_marker=pending["receipt"],
        expected_identity=dict(pending["identity"], state="awaiting-input"),
        one_pass=pending["phase"] == "implementation")
    if isinstance(resolved, str) and resolved:
        pending["transcript"] = str(Path(resolved).resolve())
        pending["identity"] = dict(pending["identity"], transcript=pending["transcript"])
    if not pending["transcript"]:
        raise EmacsClientError("delivery has no resolved transcript; pending attempt retained")
    _verify_boundary(pending)
    if pending["boundary"] is None:
        pending["boundary"] = _capture_boundary(pending["transcript"], pending["transcript_offset"])
    _finalize_pending(state)
    save_run(run_file, state)


def _finalize_pending(state: dict[str, Any], *, acknowledged=True) -> None:
    pending = state["pending_submission"]
    if pending is None:
        raise SystemExit("no pending submission to finalize")
    phase = pending["phase"]
    actor = state[pending["actor"]]
    if pending["transcript"]:
        actor["transcript"] = pending["transcript"]
    actor["identity"] = pending["identity"]
    if pending["kind"] == "phase":
        state["submissions"].append({"phase": phase, "actor": pending["actor"],
                                     "transcript_offset": pending["transcript_offset"]})
    else:
        state["submissions"][-1]["transcript_offset"] = pending["transcript_offset"]
    if pending["kind"] == "steering":
        state.setdefault("steering_prompts", []).append({
            "prompt_sha256": pending["context_sha256"], "stop_sha256": pending["stop_sha256"]})
    else:
        state.setdefault("phase_context_sha256", {})[phase] = pending["context_sha256"]
    state.setdefault("attempts", []).append(dict(pending, acknowledged=acknowledged))
    state["status"] = (
        "implementation-active" if phase == "implementation" else "phase-active"
    )
    state["active_phase"] = phase
    state["expected_phase"] = None
    state["stop_evidence"] = None
    state["pending_submission"] = None


def _freeze_pending_implementation(state: dict[str, Any], reason: str) -> None:
    """Make an ambiguous implementation attempt permanently non-runnable."""
    pending = state["pending_submission"]
    if pending is None or pending["phase"] != "implementation":
        raise SystemExit("no pending implementation can be frozen")
    _finalize_pending(state, acknowledged=False)
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
        identity, lifecycle = _observe_actor(state, actor_name)
        if lifecycle == "unknown":
            session._bootstrap_fresh_claude_waiting(state, actor_name, {"state": lifecycle})
            identity, lifecycle = _observe_actor(state, actor_name)
        if lifecycle != "awaiting-input":
            label = "Agent 1" if actor_name == "agent1" else "Agent 2"
            raise SystemExit(
                f"{label} is {lifecycle}; "
                "phase submission requires awaiting input"
            )
        prompt = _new_attempt(state, "phase", args.phase, context,
                              _phase_prompt(state, args.phase, context, args.run_file), identity)
        _deliver_pending(state, args.run_file, prompt)
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
        _, lifecycle = _observe_actor(state, actor_name)
        if lifecycle != "awaiting-input":
            label = "Agent 1" if actor_name == "agent1" else "Agent 2"
            raise SystemExit(
                f"{label} is {lifecycle}; "
                "phase completion requires awaiting input"
            )
        digest, returned_text = _phase_evidence(state, args.phase, include_text=True)
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
    if args.phase == "implementation":
        print("--- validated implementation return (text not retained in run file) ---")
        print(returned_text)


def reconcile_submission(args: argparse.Namespace) -> None:
    with run_lock(args.run_file):
        state = load_run(args.run_file)
        pending = state["pending_submission"]
        if pending is None:
            raise SystemExit("no pending submission requires reconciliation")
        delivered, _, lifecycle = _pending_receipt(state)
        if delivered and not args.delivered:
            raise SystemExit("delivery is positively acknowledged; --not-delivered cannot discard its receipt")
        if args.delivered:
            if not delivered:
                raise SystemExit("exact current-attempt delivery receipt is missing; pending submission retained")
            _finalize_pending(state)
            outcome = "delivered"
        elif lifecycle != "awaiting-input":
            raise SystemExit("not-delivered reconciliation requires the fixed actor awaiting input")
        elif pending["phase"] == "implementation" and pending["kind"] == "phase":
            _freeze_pending_implementation(
                state, "implementation delivery outcome was not independently verified"
            )
            outcome = "implementation-stopped"
        else:
            # This explicit operator assertion does not silently retry a send.
            # Preserve the failed steering attempt's at-most-once constraint.
            if pending["kind"] == "steering":
                state.setdefault("steering_prompts", []).append({
                    "prompt_sha256": pending["context_sha256"], "stop_sha256": pending["stop_sha256"]})
            state.setdefault("attempts", []).append(dict(pending, acknowledged=False))
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
        delivered, identity, lifecycle = _pending_receipt(state)
        if delivered:
            _finalize_pending(state)
            save_run(args.run_file, state)
            print(
                f"delivery already observed stage={state['stage']} "
                f"phase={submission['phase']}"
            )
            return
        if submission["phase"] == "implementation":
            raise SystemExit("implementation delivery cannot be retried; reconcile the pending attempt without another contact")
        if lifecycle != "awaiting-input":
            raise SystemExit(
                f"{submission['actor']} is {lifecycle}; "
                "delivery retry requires awaiting input"
            )
        if not session.pending_prompt_contains(
            actor["buffer"], actor["backend"], submission["receipt"],
            expected_prompt_sha256=submission["prompt_sha256"]
        ):
            raise SystemExit(
                "exact pending attempt is not present in the current composer; "
                "refusing delivery retry"
            )
        session.send_return_to_agent(actor["buffer"], actor["backend"],
                                     expected_identity=dict(identity, state="awaiting-input"),
                                     expected_prompt_sha256=submission["prompt_sha256"])
        transcript = identity["transcript"] or session._wait_for_transcript_path(
            actor["buffer"], actor["backend"], session.DELIVERY_RETRY_WAIT_SECONDS)
        if not transcript:
            raise EmacsClientError("retry has no resolved transcript; pending attempt retained")
        if not session._wait_for_delivery(
            transcript,
            submission["transcript_offset"],
            submission["receipt"],
            session.DELIVERY_RETRY_WAIT_SECONDS,
            expected_prompt_sha256=submission["prompt_sha256"],
        ):
            raise EmacsClientError(
                "submission delivery was not acknowledged after retrying only the "
                "submit keystroke"
            )
        delivered, _, _ = _pending_receipt(state)
        if not delivered:
            raise EmacsClientError("current actor delivery could not be reconciled; pending attempt retained")
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
        _active_prompt_hash(state)
        returned = session.transcript_has_output(
            Path(actor["transcript"]), offset=submission["transcript_offset"]
        )
        if returned:
            raise SystemExit(
                "active phase already returned assistant output; refusing restart"
            )
        identity, lifecycle = _observe_actor(state, actor_name, fresh=True)
        if lifecycle != "awaiting-input":
            raise SystemExit(
                f"{actor_name} is {lifecycle}; "
                "phase restart requires a fresh waiting session"
            )
        if (identity["session_id"] == actor["identity"]["session_id"]
                or identity["transcript"] == str(Path(actor["transcript"]).resolve())):
            raise SystemExit("phase restart requires a fresh fixed-role session")
        other = state["agent2" if actor_name == "agent1" else "agent1"]
        if (identity["backend"], identity["session_id"]) == (other["backend"], other["identity"]["session_id"]):
            raise SystemExit("restart cannot reuse the other role's session")
        expected_context = state.get("phase_context_sha256", {}).get(phase)
        if expected_context != hashlib.sha256(context.encode()).hexdigest():
            raise SystemExit("restart requires the original phase context; legacy runs without its digest cannot restart")
        _require_fresh_transcript(identity, identity["transcript"],
                                  "fresh restart transcript already has history; no static marker adoption is permitted")
        prompt = _new_attempt(state, "restart", phase, context, _phase_prompt(state, phase, context), identity)
        _deliver_pending(state, args.run_file, prompt)
    print(
        f"restarted stage={state['stage']} phase={phase} actor={actor_name} "
        "with fresh transcript"
    )


def run_status(args: argparse.Namespace) -> None:
    state = load_run(args.run_file, allow_legacy=True)
    display = {
        "stage": state["stage"],
        "phase": run_phase(state),
        "status": state["status"],
        "pending_reconciliation": state["pending_submission"] is not None,
        "version": state["version"],
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
        _require_no_pending(state)
        if state["status"] != "implementation-active":
            raise SystemExit("stage implementation is not active")
        _, lifecycle = _observe_actor(state, "agent1")
        if lifecycle != "awaiting-input":
            raise SystemExit(
                f"Agent 1 is {lifecycle}; "
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
        _require_no_pending(state)
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
        identity, lifecycle = _observe_actor(state, "agent1")
        if lifecycle != "awaiting-input":
            raise SystemExit(
                f"Agent 1 is {lifecycle}; steering requires awaiting input"
            )
        current_return = _latest_implementation_return(state)
        if hashlib.sha256(current_return.encode("utf-8")).hexdigest() != evidence["sha256"]:
            raise SystemExit("recorded incomplete return changed before steering")
        prompt = _new_attempt(state, "steering", "implementation", context,
                              STEERING_CONTRACT.format(stage=state["stage"], context=context), identity)
        _deliver_pending(state, args.run_file, prompt)
    print(f"steered stage={state['stage']} actor=agent1")


def _latest_implementation_return(state: dict[str, Any]) -> str:
    submission = state["submissions"][-1]
    returned = session.latest_transcript_return(
        Path(state["agent1"]["transcript"]),
        offset=submission["transcript_offset"],
        expected_prompt_sha256=_active_prompt_hash(state),
    )
    _active_prompt_hash(state)
    if returned is None:
        raise SystemExit("no bounded implementation return is available")
    return returned["text"]


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


def _actor_status(state, name):
    if state["version"] == 2:
        return session.buffer_state(state[name]["buffer"])
    identity, lifecycle = _observe_actor(state, name)
    return {"buffer": identity["buffer"], "state": lifecycle, "directory": identity["directory"]}


def status(args: argparse.Namespace) -> dict[str, Any]:
    state = load_run(args.run_file, allow_legacy=True)
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
        result["agent1"] = _actor_status(state, "agent1")
        progress = latest_progress(args.run_file)
        if progress is not None:
            result["progress"] = progress
        return result

    result["repo"] = git_status(Path(state["repo"]))
    for actor in ("agent1", "agent2"):
        result[actor] = _actor_status(state, actor)
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

    p = sub.add_parser("migrate-run", help="Explicitly bind unambiguous v2 state without inventing receipts")
    p.add_argument("--run-file", required=True)
    p.set_defaults(func=migrate_run)

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
