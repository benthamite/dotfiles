#!/usr/bin/env python3
"""Guarded single-phase cross-model plan review over one Emacs agent.el session.

The current agent session authors a plan, hands it to a fresh session of the
opposite backend for exactly one review pass, and implements the plan itself
while adjudicating the findings. This helper guards the reviewer half: an
immutable committed-plan anchor, an enforced opposite-backend identity
invariant, delivery acknowledgment, one marker-gated completion, terminal
markerless returns, and a single zero-output process-loss restart.
"""

from __future__ import annotations

import argparse
import fcntl
import hashlib
import importlib.util
import json
import os
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

REVIEW_VERSION = 1
REVIEW_STATUSES = {"ready", "review-active", "review-returned", "review-incomplete"}

REVIEW_PROMPT = """INDEPENDENT PLAN REVIEW

Read the plan exactly as committed. Run:

  git -C {repo} show {commit}:{path}

Do not read the working-tree copy or any other version; this review is
anchored to commit {commit} (blob {blob}).

Independently review this implementation plan. Identify concrete correctness
gaps, missing verification, sequencing problems, and scope contradictions the
author should account for during implementation. Return prioritized findings
with reasons. This is the only review pass; the author will adjudicate every
finding while implementing, so do not request a revised plan or another
review round.
{context}
REVIEW COMPLETION CONTRACT

Complete the entire review before returning. Only after the review is
complete, end the final response with this exact line:
{marker}"""


def _review_marker(commit: str) -> str:
    return f"REVIEW COMPLETE: {commit[:12]}"


def _git(repo: str, *argv: str) -> subprocess.CompletedProcess:
    return subprocess.run(
        ["git", "-C", repo, *argv],
        text=True,
        capture_output=True,
        check=False,
    )


def validate_review(state: Any) -> dict[str, Any]:
    if not isinstance(state, dict) or state.get("version") != REVIEW_VERSION:
        raise SystemExit("invalid or unsupported cross-review run state")
    if not isinstance(state.get("repo"), str) or not state["repo"]:
        raise SystemExit("invalid review state: repository is missing")
    plan = state.get("plan")
    if (
        not isinstance(plan, dict)
        or set(plan) != {"path", "commit", "blob_sha"}
        or not all(isinstance(plan[key], str) and plan[key] for key in plan)
    ):
        raise SystemExit("invalid review state: plan anchor is malformed")
    if state.get("caller_backend") not in session.VALID_BACKENDS:
        raise SystemExit("invalid review state: caller backend is invalid")
    reviewer = state.get("reviewer")
    if not isinstance(reviewer, dict):
        raise SystemExit("invalid review state: reviewer role is missing")
    if not isinstance(reviewer.get("buffer"), str) or not reviewer["buffer"]:
        raise SystemExit("invalid review state: reviewer buffer is missing")
    if reviewer.get("backend") not in session.VALID_BACKENDS:
        raise SystemExit("invalid review state: reviewer backend is invalid")
    if reviewer["backend"] == state["caller_backend"]:
        raise SystemExit("invalid review state: reviewer backend equals caller backend")
    transcript = reviewer.get("transcript")
    if not isinstance(transcript, str) or not transcript:
        raise SystemExit("invalid review state: reviewer transcript is missing")
    if state.get("status") not in REVIEW_STATUSES:
        raise SystemExit("invalid review state: status is unsupported")
    pending = state.get("pending_submission")
    if pending is not None and (
        not isinstance(pending, dict)
        or set(pending) != {"transcript_offset"}
        or not isinstance(pending["transcript_offset"], int)
        or pending["transcript_offset"] < 0
    ):
        raise SystemExit("invalid review state: malformed pending submission")
    submission = state.get("submission")
    if submission is not None and (
        not isinstance(submission, dict)
        or set(submission) != {"transcript_offset"}
        or not isinstance(submission["transcript_offset"], int)
        or submission["transcript_offset"] < 0
    ):
        raise SystemExit("invalid review state: malformed submission record")
    if not isinstance(state.get("restart_used"), bool):
        raise SystemExit("invalid review state: restart flag is malformed")
    evidence = state.get("return_evidence")
    if evidence is not None and (
        not isinstance(evidence, dict)
        or set(evidence) != {"kind", "sha256"}
        or evidence["kind"] not in {"marker", "markerless"}
        or not isinstance(evidence["sha256"], str)
        or len(evidence["sha256"]) != 64
    ):
        raise SystemExit("invalid review state: return evidence is malformed")

    status = state["status"]
    coherent = {
        "ready": submission is None and evidence is None,
        "review-active": pending is None and submission is not None and evidence is None,
        "review-returned": (
            pending is None
            and submission is not None
            and evidence is not None
            and evidence["kind"] == "marker"
        ),
        "review-incomplete": (
            pending is None
            and submission is not None
            and evidence is not None
            and evidence["kind"] == "markerless"
        ),
    }[status]
    if pending is not None and status != "ready":
        coherent = False
    if not coherent:
        raise SystemExit("invalid review state: review lifecycle is incoherent")
    return state


def load_review(path: Path | str) -> dict[str, Any]:
    run_path = Path(path)
    try:
        metadata = run_path.lstat()
    except FileNotFoundError:
        raise SystemExit(f"cross-review run file does not exist: {run_path}") from None
    if run_path.is_symlink():
        raise SystemExit(f"cross-review run file must not be a symlink: {run_path}")
    if metadata.st_mode & 0o777 != 0o600:
        raise SystemExit(f"cross-review run file must have mode 0600: {run_path}")
    try:
        state = json.loads(run_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise SystemExit(f"cannot read cross-review run file {run_path}: {error}") from None
    return validate_review(state)


def _state_bytes(state: dict[str, Any]) -> bytes:
    return (
        json.dumps(state, ensure_ascii=False, indent=2, sort_keys=True) + "\n"
    ).encode("utf-8")


def _create_review_file(path: Path, state: dict[str, Any]) -> None:
    flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
    if hasattr(os, "O_NOFOLLOW"):
        flags |= os.O_NOFOLLOW
    try:
        fd = os.open(path, flags, 0o600)
    except OSError as error:
        raise SystemExit(f"cannot create cross-review run file {path}: {error}") from None
    try:
        os.fchmod(fd, 0o600)
        session._write_all(fd, _state_bytes(state))
        os.fsync(fd)
    finally:
        os.close(fd)


def save_review(path: Path | str, state: dict[str, Any]) -> None:
    run_path = Path(path)
    validate_review(state)
    if run_path.is_symlink():
        raise SystemExit(f"cross-review run file must not be a symlink: {run_path}")
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
def review_lock(path: Path | str):
    run_path = Path(path)
    lock_path = run_path.with_name(run_path.name + ".lock")
    flags = os.O_RDWR | os.O_CREAT
    if hasattr(os, "O_NOFOLLOW"):
        flags |= os.O_NOFOLLOW
    try:
        fd = os.open(lock_path, flags, 0o600)
    except OSError as error:
        raise SystemExit(f"cannot open cross-review run lock {lock_path}: {error}") from None
    try:
        os.fchmod(fd, 0o600)
        fcntl.flock(fd, fcntl.LOCK_EX)
        yield
    finally:
        fcntl.flock(fd, fcntl.LOCK_UN)
        os.close(fd)


def _resolve_plan_anchor(repo: str, plan_path: str, plan_commit: str) -> dict[str, str]:
    """Anchor the review to an immutable committed blob, never the working tree."""
    top = _git(repo, "rev-parse", "--show-toplevel")
    if top.returncode != 0:
        raise SystemExit(f"not a git repository: {repo}")
    repo_root = Path(top.stdout.strip()).resolve()

    commit = _git(repo, "rev-parse", "--verify", f"{plan_commit}^{{commit}}")
    if commit.returncode != 0:
        raise SystemExit(f"plan commit does not resolve in {repo_root}: {plan_commit}")
    full_commit = commit.stdout.strip()

    candidate = Path(plan_path)
    if candidate.is_absolute():
        try:
            relative = candidate.resolve().relative_to(repo_root)
        except ValueError:
            raise SystemExit(
                f"plan path is outside the repository {repo_root}: {plan_path}"
            ) from None
    else:
        relative = Path(plan_path)
    relative_str = relative.as_posix()

    exists = _git(repo, "cat-file", "-e", f"{full_commit}:{relative_str}")
    if exists.returncode != 0:
        raise SystemExit(
            f"plan {relative_str} is not committed at {full_commit[:12]}; "
            "commit the plan before requesting a review"
        )
    blob = _git(repo, "rev-parse", f"{full_commit}:{relative_str}")
    if blob.returncode != 0:
        raise SystemExit(f"cannot resolve plan blob for {relative_str}")
    return {
        "repo": str(repo_root),
        "path": relative_str,
        "commit": full_commit,
        "blob_sha": blob.stdout.strip(),
    }


def _require_reviewer_identity(
    state: dict[str, Any], *, fresh_transcript: bool
) -> None:
    """Enforce the opposite-backend fresh-reviewer identity invariant live."""
    reviewer = state["reviewer"]
    transcript = Path(reviewer["transcript"])
    if fresh_transcript and transcript.exists() and transcript.stat().st_size:
        raise SystemExit(
            "reviewer transcript already has history; a cross review requires a "
            "fresh reviewer session"
        )
    detected = session.buffer_backend(reviewer["buffer"])
    if detected is not None and detected != reviewer["backend"]:
        raise SystemExit(
            f"reviewer buffer's actual backend is {detected}, not the declared "
            f"{reviewer['backend']}; the opposite-backend invariant is not met"
        )
    probe = session.agent_transcript_path(reviewer["buffer"], reviewer["backend"])
    if probe and str(Path(probe).resolve()) != str(transcript.resolve()):
        raise SystemExit(
            "reviewer buffer's configured transcript does not match the supplied "
            "transcript path"
        )
    live = session.buffer_state(reviewer["buffer"])
    directory = live.get("directory") or ""
    if directory:
        resolved = Path(directory).expanduser().resolve()
        repo_root = Path(state["repo"]).resolve()
        if resolved != repo_root and repo_root not in resolved.parents:
            raise SystemExit(
                f"reviewer session directory {resolved} is not inside the plan "
                f"repository {repo_root}"
            )


def init_review(args: argparse.Namespace) -> None:
    if args.caller_backend not in session.VALID_BACKENDS:
        raise SystemExit("invalid caller backend")
    if args.reviewer_backend not in session.VALID_BACKENDS:
        raise SystemExit("invalid reviewer backend")
    if args.reviewer_backend == args.caller_backend:
        raise SystemExit(
            "reviewer backend must be the opposite of the caller backend; "
            "a cross review never uses the caller's own backend"
        )
    anchor = _resolve_plan_anchor(args.repo, args.plan_path, args.plan_commit)
    state = {
        "version": REVIEW_VERSION,
        "repo": anchor["repo"],
        "plan": {
            "path": anchor["path"],
            "commit": anchor["commit"],
            "blob_sha": anchor["blob_sha"],
        },
        "caller_backend": args.caller_backend,
        "reviewer": {
            "buffer": args.reviewer_buffer,
            "backend": args.reviewer_backend,
            "transcript": args.reviewer_transcript,
        },
        "status": "ready",
        "pending_submission": None,
        "submission": None,
        "restart_used": False,
        "return_evidence": None,
    }
    validate_review(state)
    _require_reviewer_identity(state, fresh_transcript=True)
    _create_review_file(Path(args.run_file), state)
    print(session.json_for_display(state))


def _review_prompt(state: dict[str, Any], context: str) -> str:
    block = f"\n{context.strip()}\n\n" if context.strip() else "\n"
    return REVIEW_PROMPT.format(
        repo=state["repo"],
        commit=state["plan"]["commit"],
        path=state["plan"]["path"],
        blob=state["plan"]["blob_sha"],
        context=block,
        marker=_review_marker(state["plan"]["commit"]),
    )


def _read_context(args: argparse.Namespace) -> str:
    if not getattr(args, "context_file", None):
        return ""
    context_path = Path(args.context_file)
    try:
        return context_path.read_text(encoding="utf-8")
    except OSError as error:
        raise SystemExit(f"cannot read context file {context_path}: {error}") from None


def submit_review(args: argparse.Namespace) -> None:
    context = _read_context(args)
    with review_lock(args.run_file):
        state = load_review(args.run_file)
        if state["pending_submission"] is not None:
            raise SystemExit(
                "pending submission requires reconciliation before any further action"
            )
        if state["status"] in {"review-returned", "review-incomplete"}:
            raise SystemExit(
                "the single review pass already returned; a cross review never "
                "re-contacts the reviewer"
            )
        if state["status"] != "ready":
            raise SystemExit("the review is already active")
        reviewer = state["reviewer"]
        _require_reviewer_identity(state, fresh_transcript=True)
        live = session.buffer_state(reviewer["buffer"])
        live = session._bootstrap_fresh_claude_waiting(state, "reviewer", live)
        if live.get("state") != "awaiting-input":
            raise SystemExit(
                f"reviewer is {live.get('state', 'unknown')}; "
                "review submission requires awaiting input"
            )
        prompt = _review_prompt(state, context)
        marker = _review_marker(state["plan"]["commit"])
        state["pending_submission"] = {
            "transcript_offset": session._transcript_offset(state, "reviewer")
        }
        save_review(args.run_file, state)
        session.submit_to_agent(
            reviewer["buffer"],
            reviewer["backend"],
            prompt,
            transcript=reviewer["transcript"],
            transcript_offset=state["pending_submission"]["transcript_offset"],
            delivery_marker=marker,
        )
        state["submission"] = state["pending_submission"]
        state["pending_submission"] = None
        state["status"] = "review-active"
        _adopt_marker_transcript(state)
        save_review(args.run_file, state)
    print(
        f"submitted review plan={state['plan']['path']} "
        f"commit={state['plan']['commit'][:12]}"
    )


def _adopt_marker_transcript(state: dict[str, Any]) -> None:
    """Bind the reviewer's real transcript once it provably carries this run.

    A fresh Codex session materializes its rollout transcript only on first
    delivery (and may write the prompt record asynchronously), so the recorded
    path can lag reality. Adopt the discovered transcript only when it
    contains this run's exact marker-bearing prompt; anything else keeps the
    recorded binding.
    """
    reviewer = state["reviewer"]
    if state["submission"] is None or Path(reviewer["transcript"]).exists():
        return
    fresh = session._wait_for_transcript_path(
        reviewer["buffer"],
        reviewer["backend"],
        session.DELIVERY_RETRY_WAIT_SECONDS,
    )
    if not fresh:
        return
    marker = _review_marker(state["plan"]["commit"])
    marker_offset = session._user_marker_offset(fresh, marker)
    if marker_offset is not None:
        reviewer["transcript"] = fresh
        state["submission"] = {"transcript_offset": marker_offset}


def reconcile_submission(args: argparse.Namespace) -> None:
    with review_lock(args.run_file):
        state = load_review(args.run_file)
        pending = state["pending_submission"]
        if pending is None:
            raise SystemExit("no pending submission requires reconciliation")
        if args.delivered:
            state["submission"] = pending
            state["pending_submission"] = None
            state["status"] = "review-active"
            outcome = "delivered"
        else:
            if session._transcript_advanced(
                state["reviewer"]["transcript"], pending["transcript_offset"]
            ):
                raise SystemExit(
                    "the reviewer transcript advanced past the recorded boundary; "
                    "an unnoticed delivery may have happened, so reconcile with "
                    "--delivered instead of clearing the submission"
                )
            state["pending_submission"] = None
            outcome = "not-delivered"
        save_review(args.run_file, state)
    print(f"reconciled outcome={outcome}")


def retry_delivery(args: argparse.Namespace) -> None:
    """Retry only Return for a concretely observed pending composer prompt."""
    with review_lock(args.run_file):
        state = load_review(args.run_file)
        pending = state["pending_submission"]
        if pending is None:
            raise SystemExit("only a pending submission is eligible for delivery retry")
        reviewer = state["reviewer"]
        transcript = reviewer["transcript"]
        offset = pending["transcript_offset"]
        if session._delivery_observed(reviewer["buffer"], transcript, offset):
            state["submission"] = pending
            state["pending_submission"] = None
            state["status"] = "review-active"
            save_review(args.run_file, state)
            print("delivery already observed")
            return
        live = session.buffer_state(reviewer["buffer"])
        if live.get("state") != "awaiting-input":
            raise SystemExit(
                f"reviewer is {live.get('state', 'unknown')}; "
                "delivery retry requires awaiting input"
            )
        marker = _review_marker(state["plan"]["commit"])
        if not session.pending_prompt_contains(
            reviewer["buffer"], reviewer["backend"], marker
        ):
            raise SystemExit(
                "exact pending review marker is not present in the current composer; "
                "refusing delivery retry"
            )
        session.send_return_to_agent(reviewer["buffer"], reviewer["backend"])
        if not session._wait_for_delivery(
            reviewer["buffer"],
            transcript,
            offset,
            session.DELIVERY_RETRY_WAIT_SECONDS,
        ):
            raise EmacsClientError(
                "submission delivery was not acknowledged after retrying only the "
                "submit keystroke"
            )
        state["submission"] = pending
        state["pending_submission"] = None
        state["status"] = "review-active"
        save_review(args.run_file, state)
    print("retried delivery")


def finish_review(args: argparse.Namespace) -> None:
    """Record the single bounded reviewer return, complete or terminal-incomplete."""
    with review_lock(args.run_file):
        state = load_review(args.run_file)
        if state["pending_submission"] is not None:
            raise SystemExit(
                "pending submission requires reconciliation before any further action"
            )
        if state["status"] != "review-active":
            raise SystemExit("no submitted review is awaiting completion")
        reviewer = state["reviewer"]
        _adopt_marker_transcript(state)
        save_review(args.run_file, state)
        live = session.buffer_state(reviewer["buffer"])
        if live.get("state") != "awaiting-input":
            raise SystemExit(
                f"reviewer is {live.get('state', 'unknown')}; "
                "review completion requires awaiting input"
            )
        messages = session.transcript_messages(
            Path(reviewer["transcript"]),
            offset=state["submission"]["transcript_offset"],
        )
        if not messages:
            raise SystemExit(
                "review transcript contains no returned assistant message after the "
                "current submission"
            )
        text = messages[-1]["text"]
        lines = [line.strip() for line in text.splitlines() if line.strip()]
        marker = _review_marker(state["plan"]["commit"])
        digest = hashlib.sha256(text.encode("utf-8")).hexdigest()
        if lines and lines[-1] == marker:
            state["status"] = "review-returned"
            state["return_evidence"] = {"kind": "marker", "sha256": digest}
            save_review(args.run_file, state)
            outcome = "complete"
        else:
            state["status"] = "review-incomplete"
            state["return_evidence"] = {"kind": "markerless", "sha256": digest}
            save_review(args.run_file, state)
            outcome = "terminal-incomplete"
    print(f"REVIEW OUTCOME: {outcome}")
    print(text)


def restart_review(args: argparse.Namespace) -> None:
    """Restart the review once after reviewer process loss with zero output."""
    context = _read_context(args)
    with review_lock(args.run_file):
        state = load_review(args.run_file)
        if state["pending_submission"] is not None:
            raise SystemExit(
                "pending submission requires reconciliation before any further action"
            )
        if state["status"] != "review-active":
            raise SystemExit("only an active review can be restarted")
        if state["restart_used"]:
            raise SystemExit(
                "the single review restart was already used; report the blocker "
                "instead of contacting the reviewer again"
            )
        reviewer = state["reviewer"]
        returned = session.transcript_messages(
            Path(reviewer["transcript"]),
            offset=state["submission"]["transcript_offset"],
        )
        if returned:
            raise SystemExit(
                "the review already returned assistant output; refusing restart"
            )
        live = session.buffer_state(reviewer["buffer"])
        if live.get("state") != "awaiting-input":
            raise SystemExit(
                f"reviewer is {live.get('state', 'unknown')}; "
                "review restart requires a fresh waiting session"
            )
        marker = _review_marker(state["plan"]["commit"])
        old_transcript = str(Path(reviewer["transcript"]).resolve())
        # Burn the single restart before any external contact, so a crash
        # between submission and the final save can never permit a second one.
        state["restart_used"] = True
        save_review(args.run_file, state)
        fresh = session.agent_transcript_path(reviewer["buffer"], reviewer["backend"])
        marker_offset = session._user_marker_offset(fresh, marker) if fresh else None
        if marker_offset is None:
            if fresh and str(Path(fresh).resolve()) == old_transcript:
                raise SystemExit(
                    "reviewer still points at the failed transcript; "
                    "review restart requires a fresh session"
                )
            starting_offset = session._transcript_offset(
                {"reviewer": {"transcript": fresh or "/nonexistent"}}, "reviewer"
            )
            prompt = _review_prompt(state, context)
            session.submit_to_agent(
                reviewer["buffer"],
                reviewer["backend"],
                prompt,
                transcript=fresh or "/nonexistent",
                transcript_offset=starting_offset,
                delivery_marker=marker,
            )
            fresh = session._wait_for_transcript_path(
                reviewer["buffer"],
                reviewer["backend"],
                session.DELIVERY_RETRY_WAIT_SECONDS,
            )
            if not fresh:
                raise EmacsClientError(
                    "fresh review delivery was acknowledged but its transcript path "
                    "is not yet available"
                )
            marker_offset = starting_offset
        if str(Path(fresh).resolve()) == old_transcript:
            raise SystemExit("review restart did not acquire a fresh transcript")
        reviewer["transcript"] = fresh
        state["submission"] = {"transcript_offset": marker_offset}
        save_review(args.run_file, state)
    print("restarted review with fresh transcript")


def review_status(args: argparse.Namespace) -> dict[str, Any]:
    state = load_review(args.run_file)
    result: dict[str, Any] = {
        "run": {
            "plan": f"{state['plan']['path']}@{state['plan']['commit'][:12]}",
            "status": state["status"],
            "pending_reconciliation": state["pending_submission"] is not None,
        },
        "reviewer": session.buffer_state(state["reviewer"]["buffer"]),
    }
    transcript = Path(state["reviewer"]["transcript"])
    result["reviewer_transcript"] = {
        "path": str(transcript),
        "mtime": transcript.stat().st_mtime if transcript.exists() else None,
    }
    return result


def status_cmd(args: argparse.Namespace) -> None:
    current = review_status(args)
    if args.json:
        print(session.json_for_display(current))
        return
    item = current["run"]
    print(
        f"Review: plan={item['plan']} status={item['status']} "
        f"pending-reconciliation={str(item['pending_reconciliation']).lower()}"
    )
    reviewer = current["reviewer"]
    print(f"Reviewer: {reviewer['state']} — {reviewer['buffer']}")
    transcript = current["reviewer_transcript"]
    print(f"Transcript: mtime={transcript['mtime']}")


def watch(args: argparse.Namespace) -> None:
    last_rendered = ""
    failures = 0
    while True:
        try:
            current = review_status(args)
            failures = 0
        except EmacsClientError as error:
            failures += 1
            message = " ".join(str(error).split())
            if failures > 1:
                raise
            rendered = (
                session.json_for_display({"monitor_error": message})
                if args.json
                else f"monitor-error={message}"
            )
            if rendered != last_rendered:
                print(rendered, flush=True)
                last_rendered = rendered
            time.sleep(args.interval)
            continue
        if args.json:
            rendered = session.json_for_display(current)
        else:
            item = current["run"]
            rendered = (
                f"plan={item['plan']} status={item['status']} "
                f"reviewer={current['reviewer']['state']} "
                f"transcript_mtime={current['reviewer_transcript']['mtime']}"
            )
        if rendered != last_rendered:
            print(rendered, flush=True)
            last_rendered = rendered
        time.sleep(args.interval)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)

    p = sub.add_parser(
        "init-review", help="Create a guarded single-pass cross-review run"
    )
    p.add_argument("--run-file", required=True)
    p.add_argument("--repo", required=True)
    p.add_argument("--plan-path", required=True)
    p.add_argument("--plan-commit", required=True)
    p.add_argument(
        "--caller-backend", required=True, choices=sorted(session.VALID_BACKENDS)
    )
    p.add_argument("--reviewer-buffer", required=True)
    p.add_argument(
        "--reviewer-backend", required=True, choices=sorted(session.VALID_BACKENDS)
    )
    p.add_argument("--reviewer-transcript", required=True)
    p.set_defaults(func=init_review)

    p = sub.add_parser("submit-review", help="Submit the single guarded review pass")
    p.add_argument("--run-file", required=True)
    p.add_argument("--context-file")
    p.set_defaults(func=submit_review)

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
        help="Retry only Return when the exact review prompt remains in the composer",
    )
    p.add_argument("--run-file", required=True)
    p.set_defaults(func=retry_delivery)

    p = sub.add_parser(
        "finish-review",
        help="Record the bounded reviewer return; markerless returns are terminal",
    )
    p.add_argument("--run-file", required=True)
    p.set_defaults(func=finish_review)

    p = sub.add_parser(
        "restart-review",
        help="Restart once after reviewer process loss with zero assistant output",
    )
    p.add_argument("--run-file", required=True)
    p.add_argument("--context-file")
    p.set_defaults(func=restart_review)

    p = sub.add_parser("status", help="Collect review and reviewer status once")
    p.add_argument("--run-file", required=True)
    p.add_argument("--json", action="store_true", help="Emit structured JSON")
    p.set_defaults(func=status_cmd)

    p = sub.add_parser("watch", help="Poll review and reviewer status")
    p.add_argument("--run-file", required=True)
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
