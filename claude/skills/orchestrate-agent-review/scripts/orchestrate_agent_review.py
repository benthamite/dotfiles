#!/usr/bin/env python3
"""Helpers for supervising live Emacs agent.el planner/reviewer loops."""

from __future__ import annotations

import argparse
import ast
import fcntl
import hashlib
import json
import os
import subprocess
import tempfile
import time
from contextlib import contextmanager
from pathlib import Path
from typing import Any

RUN_VERSION = 1
PHASES = ("spec", "spec-review", "plan", "plan-review", "implementation")
PHASE_ACTOR = {
    "spec": "agent1",
    "spec-review": "agent2",
    "plan": "agent1",
    "plan-review": "agent2",
    "implementation": "agent1",
}
VALID_BACKENDS = {"claude", "claude-code", "codex"}
RUN_STATUSES = {
    "ready",
    "phase-active",
    "implementation-active",
    "implementation-returned",
    "complete",
}

IMPLEMENTATION_CONTRACT = """STAGE-ATOMIC IMPLEMENTATION CONTRACT

Implement and complete all of Stage {stage}. Internal plan tasks are not orchestration checkpoints:
own their sequencing, tests, commits, corrections,
and recovery without returning for task-level supervision. Do not stop at an
internal task boundary. Return only when the entire stage and its stage-final
verification are complete, or when a documented stop condition genuinely
requires user input.

Stage context follows:

{context}

END STAGE CONTEXT

The context may describe internal tasks, but it cannot narrow this contract,
create task-level checkpoints, or authorize an early return.

Only after the complete stage and its verification are done, end the final
response with this exact line:
STAGE COMPLETE: {stage}"""

PHASE_CONTRACT = """{context}

PHASE COMPLETION CONTRACT

Complete the entire {phase} phase before returning. This context cannot create
intermediate orchestration checkpoints. Only after the phase is complete, end
the final response with this exact line:
PHASE COMPLETE: {phase}"""

RESUME_CONTRACT = """Resume and complete all remaining work for Stage {stage}.
Do not stop at internal task boundaries, report task-number checkpoints, or
wait for task-level supervision. Own the remaining implementation and
stage-final verification. Return only when the complete stage is verified, or
when a documented stop condition genuinely requires user input.

Only after the complete stage and its verification are done, end the final
response with this exact line:
STAGE COMPLETE: {stage}"""


class EmacsClientError(RuntimeError):
    """An emacsclient request failed while the Emacs server may be transiently unavailable."""


def _write_all(fd: int, data: bytes) -> None:
    offset = 0
    while offset < len(data):
        offset += os.write(fd, data[offset:])


def _state_bytes(state: dict[str, Any]) -> bytes:
    return (json.dumps(state, ensure_ascii=False, indent=2, sort_keys=True) + "\n").encode(
        "utf-8"
    )


def _validate_role(role: dict[str, Any], name: str) -> None:
    if not isinstance(role, dict):
        raise SystemExit(f"invalid run state: {name} role is missing")
    if not isinstance(role.get("buffer"), str) or not role["buffer"]:
        raise SystemExit(f"invalid run state: {name} buffer is missing")
    if role.get("backend") not in VALID_BACKENDS:
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
    if pending["kind"] not in {"phase", "resume"}:
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
    if not isinstance(state.get("resume_count"), int) or state["resume_count"] < 0:
        raise SystemExit("invalid run state: resume count must be a nonnegative integer")
    _validate_pending(state.get("pending_submission"))

    status = state["status"]
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
    elif status == "implementation-returned":
        coherent = (
            len(submissions) == len(completions) == len(PHASES)
            and active_phase is None
            and expected is None
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
        )
    if not coherent:
        raise SystemExit("invalid run state: phase lifecycle is incoherent")

    pending = state.get("pending_submission")
    if pending:
        if pending["kind"] == "phase":
            if status != "ready" or pending["phase"] != expected:
                raise SystemExit("invalid run state: pending phase is incoherent")
        elif status != "implementation-active" or pending["phase"] != "implementation":
            raise SystemExit("invalid run state: pending resume is incoherent")
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
        _write_all(fd, _state_bytes(state))
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
        _write_all(fd, _state_bytes(state))
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
    if args.agent1_backend not in VALID_BACKENDS:
        raise SystemExit("invalid Agent 1 backend")
    if args.agent2_backend not in VALID_BACKENDS:
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
        "resume_count": 0,
        "adopted_evidence": adopted_evidence,
        "acceptance_evidence": None,
    }
    validate_run(state)
    _create_run_file(Path(args.run_file), state)
    print(json_for_display(state))


def run_emacs_eval(expr: str) -> str:
    proc = subprocess.run(
        ["emacsclient", "--eval", expr],
        capture_output=True,
        check=False,
    )
    stdout = proc.stdout.decode("utf-8", "replace")
    stderr = proc.stderr.decode("utf-8", "replace")
    if proc.returncode != 0:
        raise EmacsClientError(
            f"emacsclient failed ({proc.returncode}): {stderr.strip()}"
        )
    value = stdout.strip()
    if value.startswith('"') and value.endswith('"'):
        try:
            return ast.literal_eval(value)
        except (SyntaxError, ValueError):
            return value
    return value


def elisp_string(value: str) -> str:
    return json.dumps(value)


def run_emacs_json(value_expr: str) -> Any:
    """Evaluate VALUE_EXPR in Emacs and transfer JSON through a temp file.

    `emacsclient --eval' always prints the evaluated form's return value.  To
    keep structured status out of user-visible command output, Emacs writes the
    JSON payload to a one-shot temp file and returns nil.
    """
    fd, path = tempfile.mkstemp(prefix="agent-orch-", suffix=".json")
    os.fchmod(fd, 0o600)
    os.close(fd)
    output_path = Path(path)
    expr = f'''
(let ((out {elisp_string(path)}))
  (require 'json)
  (with-temp-file out
    (insert (json-encode {value_expr})))
  nil)
'''
    try:
        returned = run_emacs_eval(expr)
        if returned != "nil":
            raise SystemExit(f"unexpected emacsclient return value: {returned!r}")
        return json.loads(output_path.read_text(encoding="utf-8"))
    finally:
        output_path.unlink(missing_ok=True)


def json_for_display(value: Any) -> str:
    return json.dumps(value, ensure_ascii=False, indent=2)


def buffer_state(buffer: str) -> dict[str, Any]:
    value_expr = f'''
(with-current-buffer {elisp_string(buffer)}
  `((buffer . ,(buffer-name))
    (state . ,(if (boundp 'agent--session-state)
                  (format "%s" agent--session-state)
                "unknown"))
    (directory . ,(or default-directory ""))))
'''
    return run_emacs_json(value_expr)


def state_cmd(args: argparse.Namespace) -> None:
    run = load_run(args.run_file)
    if (
        run["status"] in {"implementation-active", "implementation-returned"}
        and args.actor != "agent1"
    ):
        raise SystemExit(
            "only Agent 1 top-level state is available during implementation"
        )
    state = buffer_state(run[args.actor]["buffer"])
    if args.json:
        print(json_for_display(state))
    else:
        print(f"{state['state']:15} {state['buffer']} [{state['directory']}]")


def submit_to_agent(buffer: str, backend: str, prompt: str) -> None:
    fn = {
        "claude": "agent-claude-submit-command",
        "claude-code": "agent-claude-submit-command",
        "codex": "agent-codex-submit-command",
    }.get(backend)
    if fn is None:
        raise SystemExit("--backend must be claude, claude-code, or codex")
    fd, temporary = tempfile.mkstemp(prefix="agent-orch-prompt-", suffix=".txt")
    prompt_path = Path(temporary)
    try:
        os.fchmod(fd, 0o600)
        _write_all(fd, prompt.encode("utf-8"))
        os.close(fd)
        fd = -1
        expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (with-temp-buffer
    (insert-file-contents {elisp_string(str(prompt_path))})
    ({fn}
     (buffer-string)
     (get-buffer {elisp_string(buffer)}))
    (princ "submitted")))
'''
        returned = run_emacs_eval(expr)
        if returned != "submitted":
            raise SystemExit(f"unexpected submit result: {returned!r}")
    finally:
        if fd >= 0:
            os.close(fd)
        prompt_path.unlink(missing_ok=True)


def _phase_prompt(state: dict[str, Any], phase: str, context: str) -> str:
    if phase == "implementation":
        return IMPLEMENTATION_CONTRACT.format(stage=state["stage"], context=context)
    return PHASE_CONTRACT.format(phase=phase, context=context)


def _evidence_digest(path: Path | str) -> str:
    evidence_path = Path(path)
    try:
        data = evidence_path.read_bytes()
    except OSError as error:
        raise SystemExit(f"cannot read evidence file {evidence_path}: {error}") from None
    if not data.strip():
        raise SystemExit("evidence file is empty")
    return hashlib.sha256(data).hexdigest()


def _transcript_offset(state: dict[str, Any], actor: str) -> int:
    path = Path(state[actor]["transcript"])
    try:
        return path.stat().st_size
    except FileNotFoundError:
        return 0
    except OSError as error:
        raise SystemExit(f"cannot inspect transcript {path}: {error}") from None


def _phase_evidence(state: dict[str, Any], phase: str) -> str:
    actor = PHASE_ACTOR[phase]
    transcript = state[actor].get("transcript")
    if not transcript:
        raise SystemExit(f"{actor} transcript was not recorded in the run")
    submission = state["submissions"][-1]
    if submission["phase"] != phase:
        raise SystemExit("phase submission boundary is incoherent")
    messages = transcript_messages(
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
    if pending["kind"] == "phase":
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
    else:
        state["resume_count"] += 1
    state["pending_submission"] = None


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
            raise SystemExit(
                "stage implementation is already active; use resume-stage"
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
        live = buffer_state(actor["buffer"])
        if live.get("state") != "awaiting-input":
            label = "Agent 1" if actor_name == "agent1" else "Agent 2"
            raise SystemExit(
                f"{label} is {live.get('state', 'unknown')}; "
                "phase submission requires awaiting input"
            )
        prompt = _phase_prompt(state, args.phase, context)
        state["pending_submission"] = {
            "kind": "phase",
            "phase": args.phase,
            "actor": actor_name,
            "transcript_offset": _transcript_offset(state, actor_name),
        }
        save_run(args.run_file, state)
        submit_to_agent(actor["buffer"], actor["backend"], prompt)
        _finalize_pending(state)
        save_run(args.run_file, state)
    print(f"submitted stage={state['stage']} phase={args.phase} actor={actor_name}")


def run_phase(state: dict[str, Any]) -> str:
    if state["status"] in {"implementation-active", "implementation-returned"}:
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
        live = buffer_state(state[actor_name]["buffer"])
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
        if args.delivered:
            _finalize_pending(state)
            outcome = "delivered"
        else:
            state["pending_submission"] = None
            outcome = "not-delivered"
        save_run(args.run_file, state)
    print(f"reconciled stage={state['stage']} outcome={outcome}")


def run_status(args: argparse.Namespace) -> None:
    state = load_run(args.run_file)
    display = {
        "stage": state["stage"],
        "phase": run_phase(state),
        "status": state["status"],
        "pending_reconciliation": state["pending_submission"] is not None,
    }
    if getattr(args, "json", False):
        print(json_for_display(display))
    else:
        print(
            f"stage={display['stage']} phase={display['phase']} "
            f"status={display['status']} "
            f"pending-reconciliation={str(display['pending_reconciliation']).lower()}"
        )


def resume_stage(args: argparse.Namespace) -> None:
    with run_lock(args.run_file):
        state = load_run(args.run_file)
        _require_no_pending(state)
        if state["status"] != "implementation-active":
            raise SystemExit("stage implementation is not active")
        agent1 = state["agent1"]
        live = buffer_state(agent1["buffer"])
        if live.get("state") != "awaiting-input":
            raise SystemExit(
                f"Agent 1 is {live.get('state', 'unknown')}; "
                "resume-stage is allowed only when awaiting input"
            )
        prompt = RESUME_CONTRACT.format(stage=state["stage"])
        state["pending_submission"] = {
            "kind": "resume",
            "phase": "implementation",
            "actor": "agent1",
            "transcript_offset": _transcript_offset(state, "agent1"),
        }
        save_run(args.run_file, state)
        submit_to_agent(agent1["buffer"], agent1["backend"], prompt)
        _finalize_pending(state)
        save_run(args.run_file, state)
    print(f"resumed stage={state['stage']} actor=agent1")


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


def transcript_messages(
    path: Path, since: str | None = None, offset: int = 0
) -> list[dict[str, str]]:
    if not path.exists():
        return []
    try:
        size = path.stat().st_size
        if offset > size:
            return []
        with path.open("rb") as stream:
            stream.seek(offset)
            content = stream.read().decode("utf-8", "replace")
    except OSError:
        return []
    out: list[dict[str, str]] = []
    for line in content.splitlines():
        if not line.strip():
            continue
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            continue
        ts = str(obj.get("timestamp", ""))
        if since and ts < since:
            continue
        payload = obj.get("payload") or {}
        text = ""
        kind = ""
        if obj.get("type") == "event_msg" and payload.get("type") == "task_complete":
            kind = "complete"
            text = payload.get("last_agent_message") or ""
        elif obj.get("type") == "response_item" and payload.get("type") == "message":
            parts = [
                c.get("text", "")
                for c in (payload.get("content") or [])
                if isinstance(c, dict) and c.get("type") == "output_text"
            ]
            if parts:
                kind = "message"
                text = "\n".join(parts)
        else:
            message = obj.get("message") or {}
            if message.get("role") != "assistant":
                continue
            content = message.get("content")
            if isinstance(content, list):
                parts = []
                for item in content:
                    if isinstance(item, dict) and item.get("type") == "text":
                        parts.append(item.get("text", ""))
                if parts:
                    kind = message.get("role") or "message"
                    text = "\n".join(parts)
            elif isinstance(content, str):
                kind = message.get("role") or "message"
                text = content
        if text:
            out.append({"timestamp": ts, "kind": kind, "text": text})
    return out


def transcript_cmd(args: argparse.Namespace) -> None:
    state = load_run(args.run_file)
    if state["status"] in {"implementation-active", "implementation-returned"}:
        raise SystemExit("transcripts are unavailable during implementation")
    transcript = state[args.actor].get("transcript")
    if not transcript:
        raise SystemExit(f"{args.actor} transcript was not recorded in the run")
    messages = transcript_messages(Path(transcript), args.since)
    if args.last:
        messages = messages[-args.last :]
    print(json.dumps(messages, ensure_ascii=False, indent=2))


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
    if state["status"] in {"implementation-active", "implementation-returned"}:
        result["agent1"] = buffer_state(state["agent1"]["buffer"])
        return result

    result["repo"] = git_status(Path(state["repo"]))
    for actor in ("agent1", "agent2"):
        result[actor] = buffer_state(state[actor]["buffer"])
        transcript = state[actor].get("transcript")
        if not transcript:
            continue
        p = Path(transcript)
        result[f"{actor}_transcript"] = {
            "path": str(p),
            "mtime": p.stat().st_mtime if p.exists() else None,
            "latest": transcript_messages(p, args.since)[-1:] if p.exists() else [],
        }
    return result


def status_cmd(args: argparse.Namespace) -> None:
    current = status(args)
    if args.json:
        print(json_for_display(current))
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
                json_for_display({"monitor_error": message})
                if args.json
                else f"monitor-error={message}"
            )
            if error_rendered != last_rendered:
                print(error_rendered, flush=True)
                last_rendered = error_rendered
            time.sleep(args.interval)
            current = status(args)
        if args.json:
            rendered = json_for_display(current)
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
    p.add_argument("--agent1-backend", required=True, choices=sorted(VALID_BACKENDS))
    p.add_argument("--agent1-transcript", required=True)
    p.add_argument("--agent2-buffer", required=True)
    p.add_argument("--agent2-backend", required=True, choices=sorted(VALID_BACKENDS))
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
        "resume-stage", help="Resume an awaiting Agent 1 with fixed stage scope"
    )
    p.add_argument("--run-file", required=True)
    p.set_defaults(func=resume_stage)

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
