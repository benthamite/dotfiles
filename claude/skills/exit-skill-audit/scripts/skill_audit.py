#!/usr/bin/env python3
"""Deterministic state for exit-skill-audit: due logic, lock, proposals, receipts.

The model-led audit lives in SKILL.md. This script owns everything that must
not depend on the model's judgment: which target a session covers, whether an
audit is due, the per-target lock, the pending-proposals state and the
before-exit receipt.

State lives under ${XDG_STATE_HOME:-~/.local/state}/exit-skill-audit/<target>/:
  state.json            last audit date, status, pending proposals path
  lock                  JSON {session, started_at}; one session per target
  proposals/DATE.md     proposals written by an audit
  runs/DATE/            subagent reports for that audit

Every subcommand prints one JSON object on stdout. Exit status 0 means the
operation succeeded; 2 means a usage or validation error; 3 means the lock is
held by another session.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import re
import sys
import tempfile
import uuid
from pathlib import Path

SKILL_DIR = Path(__file__).resolve().parent.parent
DEFAULT_TARGETS_DIR = SKILL_DIR / "targets"
RECEIPT_STATUSES = ("no-op", "held", "success", "failure")
DECISIONS = ("pending", "do", "skip", "defer")
# A lock older than this belongs to a session that was abandoned while it
# audited or held proposals; the next session may take it over and says so.
STALE_LOCK_HOURS = 24
PROPOSAL_HEADING = re.compile(r"^## (\d+)\. (.+)$")
FIELD = re.compile(r"^(Decision|Applied|Recommendation):\s*(.*)$")


class AuditError(Exception):
    """A validation failure reported as exit status 2."""


class LockHeld(Exception):
    """The target's lock belongs to another live session (exit status 3)."""


# ---------------------------------------------------------------- paths


def state_root() -> Path:
    base = os.environ.get("XDG_STATE_HOME")
    root = Path(base) if base else Path.home() / ".local" / "state"
    if not root.is_absolute():
        raise AuditError("XDG_STATE_HOME must be an absolute path")
    return root / "exit-skill-audit"


def target_dir(target: str) -> Path:
    if not re.fullmatch(r"[a-z0-9][a-z0-9-]*", target):
        raise AuditError(f"invalid target id {target!r}")
    return state_root() / target


def expand(path: str) -> Path:
    return Path(os.path.expanduser(path)).resolve()


# -------------------------------------------------------------- targets


def load_targets(targets_dir: Path) -> list[dict]:
    targets = []
    for spec in sorted(targets_dir.glob("*/target.json")):
        data = json.loads(spec.read_text(encoding="utf-8"))
        if data.get("id") != spec.parent.name:
            raise AuditError(f"{spec}: id must equal its directory name")
        for key in ("skill", "directories", "interval_days", "checks"):
            if key not in data:
                raise AuditError(f"{spec}: missing {key}")
        if not isinstance(data["interval_days"], int) or data["interval_days"] < 1:
            raise AuditError(f"{spec}: interval_days must be a positive integer")
        data["_dir"] = str(spec.parent)
        targets.append(data)
    return targets


def covers(target: dict, cwd: Path) -> bool:
    for directory in target["directories"]:
        root = expand(directory)
        if cwd == root or root in cwd.parents:
            return True
    return False


# ---------------------------------------------------------------- state


def read_state(target: str) -> dict:
    path = target_dir(target) / "state.json"
    if not path.exists():
        return {}
    return json.loads(path.read_text(encoding="utf-8"))


def write_json_atomic(path: Path, data: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    fd, tmp = tempfile.mkstemp(dir=path.parent, prefix=".tmp-", suffix=".json")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as stream:
            json.dump(data, stream, indent=2, sort_keys=True)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(tmp, path)
    except BaseException:
        if os.path.exists(tmp):
            os.unlink(tmp)
        raise


def write_state(target: str, state: dict) -> None:
    write_json_atomic(target_dir(target) / "state.json", state)


def parse_date(value: str) -> dt.date:
    try:
        return dt.date.fromisoformat(value)
    except ValueError as error:
        raise AuditError(f"invalid date {value!r}") from error


def next_due(state: dict, interval_days: int) -> dt.date | None:
    """Return the first date an audit is due, or None if one is due now."""
    last = state.get("last_audit")
    due = parse_date(last) + dt.timedelta(days=interval_days) if last else None
    # A failed attempt is retried the next day, not on every exit.
    if state.get("last_status") == "failed" and state.get("last_attempt"):
        retry = parse_date(state["last_attempt"]) + dt.timedelta(days=1)
        due = max(due, retry) if due else retry
    return due


# ----------------------------------------------------------------- lock


def lock_path(target: str) -> Path:
    return target_dir(target) / "lock"


def read_lock(target: str) -> dict | None:
    path = lock_path(target)
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        return None
    except json.JSONDecodeError:
        # Locks are written whole (see create_lock), so this is damage, not a
        # write in progress. Treat it as held, aged by the file's mtime.
        try:
            mtime = dt.datetime.fromtimestamp(path.stat().st_mtime, dt.timezone.utc)
        except FileNotFoundError:
            return None
        return {"session": "unreadable", "started_at": mtime.isoformat()}


def lock_is_stale(lock: dict, now: dt.datetime) -> bool:
    started = dt.datetime.fromisoformat(lock["started_at"])
    return now - started > dt.timedelta(hours=STALE_LOCK_HOURS)


def create_lock(path: Path, payload: str) -> bool:
    """Create PATH with PAYLOAD atomically; return False if it already exists.

    The content is written to a private temporary file first and then
    hard-linked into place, so no reader ever sees a partial lock.
    """
    fd, tmp = tempfile.mkstemp(dir=path.parent, prefix=".lock-")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as stream:
            stream.write(payload)
            stream.flush()
            os.fsync(stream.fileno())
        try:
            os.link(tmp, path)
        except FileExistsError:
            return False
        return True
    finally:
        os.unlink(tmp)


def acquire(target: str, session: str, now: dt.datetime) -> dict:
    """Take TARGET's lock for SESSION; return a note on what happened."""
    path = lock_path(target)
    path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    payload = json.dumps({"session": session, "started_at": now.isoformat()})
    note = "acquired"
    for _ in range(3):
        if create_lock(path, payload):
            return {"lock": note}
        lock = read_lock(target)
        if lock is None:
            continue  # Released between our attempt and the read.
        if lock.get("session") == session:
            return {"lock": "already held by this session"}
        if not lock_is_stale(lock, now):
            raise LockHeld(lock)
        # Move the stale lock aside, then check that what we moved is the lock
        # we judged stale: a faster contender may have replaced it already.
        graveyard = path.with_name(f"lock.stale-{uuid.uuid4().hex}")
        try:
            os.rename(path, graveyard)
        except FileNotFoundError:
            continue
        try:
            moved = json.loads(graveyard.read_text(encoding="utf-8"))
        except json.JSONDecodeError:
            moved = {"session": "unreadable"}
        if moved.get("session") != lock.get("session") or (
                moved.get("started_at", lock.get("started_at")) != lock.get("started_at")):
            # We took a fresh lock: put it back if nobody has taken the path.
            try:
                os.link(graveyard, path)
            except FileExistsError:
                pass
            graveyard.unlink()
            raise LockHeld(moved)
        graveyard.unlink()
        note = (f"broke stale lock of session {lock.get('session')} "
                f"from {lock.get('started_at')}")
    raise LockHeld(read_lock(target) or {})


def release(target: str, session: str) -> None:
    lock = read_lock(target)
    if lock is None:
        return
    if lock.get("session") != session:
        raise AuditError(f"lock belongs to session {lock.get('session')}, not {session}")
    lock_path(target).unlink()


def require_lock(target: str, session: str) -> None:
    lock = read_lock(target)
    if lock is None or lock.get("session") != session:
        raise AuditError(f"session {session} does not hold the {target} lock")


# ------------------------------------------------------------ proposals


def parse_proposals(path: Path) -> list[dict]:
    """Return the numbered proposals in PATH with their decision fields."""
    proposals: list[dict] = []
    for line in path.read_text(encoding="utf-8").splitlines():
        heading = PROPOSAL_HEADING.match(line)
        if heading:
            proposals.append({"number": int(heading.group(1)),
                              "title": heading.group(2).strip()})
            continue
        field = FIELD.match(line)
        if field and proposals:
            proposals[-1][field.group(1).lower()] = field.group(2).strip()
    for expected, proposal in enumerate(proposals, start=1):
        if proposal["number"] != expected:
            raise AuditError(f"{path}: proposals must be numbered 1, 2, 3 ...")
        decision = proposal.get("decision")
        if decision not in DECISIONS:
            raise AuditError(f"{path}: proposal {expected} has decision "
                             f"{decision!r}; expected one of {', '.join(DECISIONS)}")
        if proposal.get("recommendation") not in ("do", "skip"):
            raise AuditError(f"{path}: proposal {expected} needs "
                             "'Recommendation: do' or 'Recommendation: skip'")
    return proposals


def summarize(proposals: list[dict]) -> dict:
    pending = [p["number"] for p in proposals if p["decision"] == "pending"]
    unapplied = [p["number"] for p in proposals
                 if p["decision"] == "do" and not p.get("applied")]
    return {"total": len(proposals), "pending": pending, "unapplied": unapplied}


# --------------------------------------------------------------- decide


def decide(targets: list[dict], cwd: Path, session: str,
           today: dt.date, now: dt.datetime) -> dict:
    """Choose the single action for this exit, by priority.

    resolve (every pending proposal decided) > resume (proposals still await
    Pablo) > audit (a due target) > no-op.
    """
    matching = [t for t in targets if covers(t, cwd)]
    if not matching:
        return {"action": "no-op", "reason": f"no exit-skill-audit target covers {cwd}"}
    candidates: dict[str, list[dict]] = {"resolve": [], "resume": [], "audit": []}
    reasons = []
    for target in matching:
        tid = target["id"]
        lock = read_lock(tid)
        if lock and lock.get("session") != session and not lock_is_stale(lock, now):
            reasons.append(f"{tid}: in progress in session {lock.get('session')} "
                           f"since {lock.get('started_at')}")
            continue
        state = read_state(tid)
        pending_path = state.get("pending_proposals")
        if pending_path:
            summary = summarize(parse_proposals(Path(pending_path)))
            action = "resume" if summary["pending"] else "resolve"
            candidates[action].append({"target": tid, "proposals": pending_path,
                                       **summary})
            continue
        due = next_due(state, target["interval_days"])
        if due is None or today >= due:
            candidates["audit"].append({
                "target": tid, "skill": target["skill"],
                "checks": str(Path(target["_dir"]) / target["checks"]),
                "tests": (str(Path(target["_dir"]) / target["tests"])
                          if target.get("tests") else None),
                "last_audit": state.get("last_audit")})
        else:
            reasons.append(f"{tid}: next audit due {due.isoformat()}")
    for action in ("resolve", "resume", "audit"):
        if candidates[action]:
            return {"action": action, **candidates[action][0]}
    return {"action": "no-op", "reason": "; ".join(reasons)}


# ------------------------------------------------------------- receipts


def write_receipt(path: Path, status: str, message: str) -> None:
    """Publish one receipt at PATH atomically; refuse an existing file."""
    if status not in RECEIPT_STATUSES:
        raise AuditError(f"status must be one of {', '.join(RECEIPT_STATUSES)}")
    if not message.strip():
        raise AuditError("message must not be empty")
    if not path.is_absolute():
        raise AuditError("receipt path must be absolute")
    if os.path.lexists(path):
        raise AuditError(f"{path} already exists; refusing to overwrite")
    if not path.parent.is_dir():
        raise AuditError(f"receipt directory {path.parent} does not exist")
    fd, tmp = tempfile.mkstemp(dir=path.parent, prefix=".receipt-", suffix=".tmp")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as stream:
            json.dump({"status": status, "message": message}, stream)
            stream.flush()
            os.fsync(stream.fileno())
        try:
            os.link(tmp, path)  # Atomic, and fails if PATH appeared meanwhile.
        except FileExistsError as error:
            raise AuditError(f"{path} already exists; refusing to overwrite") from error
    finally:
        os.unlink(tmp)


# ------------------------------------------------------------- commands


def now_utc(args) -> dt.datetime:
    if getattr(args, "now", None):
        return dt.datetime.fromisoformat(args.now)
    return dt.datetime.now(dt.timezone.utc)


def today_local(args) -> dt.date:
    return parse_date(args.today) if getattr(args, "today", None) else dt.date.today()


def target_spec(args) -> dict:
    for target in load_targets(Path(args.targets_dir)):
        if target["id"] == args.target:
            return target
    raise AuditError(f"unknown target {args.target!r}")


def cmd_decide(args) -> dict:
    return decide(load_targets(Path(args.targets_dir)), expand(args.cwd),
                  args.session, today_local(args), now_utc(args))


def cmd_begin(args) -> dict:
    target_spec(args)
    note = acquire(args.target, args.session, now_utc(args))
    today = today_local(args).isoformat()
    base = target_dir(args.target)
    proposals = base / "proposals" / f"{today}.md"
    if proposals.exists():
        raise AuditError(f"{proposals} already exists; resolve or move it first")
    run_dir = base / "runs" / today
    run_dir.mkdir(parents=True, exist_ok=True, mode=0o700)
    proposals.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    state = read_state(args.target)
    state["last_attempt"] = today
    state["last_status"] = "running"
    write_state(args.target, state)
    return {**note, "proposals": str(proposals), "run_dir": str(run_dir)}


def cmd_hold(args) -> dict:
    require_lock(args.target, args.session)
    path = Path(args.proposals).resolve()
    summary = summarize(parse_proposals(path))
    if not summary["total"]:
        raise AuditError("no proposals found; use 'complete' for an audit with none")
    state = read_state(args.target)
    state.update(last_audit=today_local(args).isoformat(), last_status="held",
                 pending_proposals=str(path))
    write_state(args.target, state)
    # The lock stays with this session while Pablo decides, so no other
    # session presents the same proposals at the same time.
    return {"status": "held", **summary}


def cmd_complete(args) -> dict:
    require_lock(args.target, args.session)
    state = read_state(args.target)
    state.update(last_audit=today_local(args).isoformat(), last_status="clean",
                 pending_proposals=None)
    write_state(args.target, state)
    release(args.target, args.session)
    return {"status": "clean"}


def cmd_claim(args) -> dict:
    target_spec(args)
    return acquire(args.target, args.session, now_utc(args))


def cmd_resolve(args) -> dict:
    state = read_state(args.target)
    path = state.get("pending_proposals")
    if not path:
        raise AuditError(f"{args.target} has no pending proposals")
    summary = summarize(parse_proposals(Path(path)))
    if summary["pending"] or summary["unapplied"]:
        raise AuditError(f"not resolvable: undecided {summary['pending']}, "
                         f"approved without an Applied line {summary['unapplied']}")
    acquire(args.target, args.session, now_utc(args))
    state.update(last_status="resolved", pending_proposals=None,
                 resolved_on=today_local(args).isoformat(),
                 last_resolved_proposals=path)
    write_state(args.target, state)
    release(args.target, args.session)
    return {"status": "resolved", **summary}


def cmd_fail(args) -> dict:
    state = read_state(args.target)
    state.update(last_status="failed", last_attempt=today_local(args).isoformat(),
                 last_failure=args.reason)
    write_state(args.target, state)
    lock = read_lock(args.target)
    if lock and lock.get("session") == args.session:
        release(args.target, args.session)
    return {"status": "failed"}


def cmd_mark_audited(args) -> dict:
    target_spec(args)
    state = read_state(args.target)
    if state.get("pending_proposals"):
        raise AuditError("pending proposals exist; resolve them first")
    state.update(last_audit=parse_date(args.date).isoformat(), last_status="clean",
                 note=args.note)
    write_state(args.target, state)
    return {"status": "clean", "last_audit": state["last_audit"]}


def cmd_status(args) -> dict:
    target_spec(args)
    return {"state": read_state(args.target), "lock": read_lock(args.target),
            "state_dir": str(target_dir(args.target))}


def cmd_receipt(args) -> dict:
    write_receipt(Path(args.receipt_file), args.status, args.message)
    return {"written": args.receipt_file, "status": args.status}


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--targets-dir", default=str(DEFAULT_TARGETS_DIR))
    sub = parser.add_subparsers(dest="command", required=True)

    def add(name, func, *, target=True, session=True, today=False, now=False):
        p = sub.add_parser(name)
        p.set_defaults(func=func)
        if target:
            p.add_argument("--target", required=True)
        if session:
            p.add_argument("--session", required=True)
        if today:
            p.add_argument("--today", help="local date, YYYY-MM-DD (tests)")
        if now:
            p.add_argument("--now", help="ISO timestamp with offset (tests)")
        return p

    add("decide", cmd_decide, target=False, today=True, now=True).add_argument(
        "--cwd", required=True)
    add("begin", cmd_begin, today=True, now=True)
    add("claim", cmd_claim, now=True)
    add("hold", cmd_hold, today=True).add_argument("--proposals", required=True)
    add("complete", cmd_complete, today=True)
    add("resolve", cmd_resolve, today=True, now=True)
    add("fail", cmd_fail, today=True).add_argument("--reason", required=True)
    p = add("mark-audited", cmd_mark_audited, session=False)
    p.add_argument("--date", required=True)
    p.add_argument("--note", required=True)
    add("status", cmd_status, session=False)
    p = add("write-receipt", cmd_receipt, target=False, session=False)
    p.add_argument("--receipt-file", required=True)
    p.add_argument("--status", required=True)
    p.add_argument("--message", required=True)
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    if getattr(args, "session", None) == "":
        print(json.dumps({"error": "--session must not be empty"}))
        return 2
    try:
        result = args.func(args)
    except LockHeld as error:
        print(json.dumps({"error": "locked", "owner": error.args[0]}))
        return 3
    except (AuditError, OSError, json.JSONDecodeError) as error:
        print(json.dumps({"error": str(error)}))
        return 2
    print(json.dumps(result, indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main())
