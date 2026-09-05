#!/usr/bin/env python3
"""Private, idempotent verdict ledger. Filter is read-only; v1 writes need migrate.

Each private directory owns one ledger and its adjacent history.md. Version 2
keeps the authoritative verdict events in the ledger. History is a derived view:
a failed history write is reported and can be reconciled by repeating record with
the SAME --operation-id and request. This is not a two-file atomic transaction.
"""
from __future__ import annotations

import argparse
import base64
import binascii
import fcntl
import hashlib
import html
import json
import math
import os
import re
import stat
import sys
import tempfile
from contextlib import contextmanager
from datetime import datetime, timedelta, timezone
from pathlib import Path

DEFAULT_SKIP_WINDOW_DAYS = 14
KINDS = {"COMPLETED", "FAILED", "BLOCKED", "DEFERRED"}
HISTORY_HEADER = b"# Overnight TODOs - history\n\n"


class LedgerError(ValueError):
    """A selected input or pending operation cannot safely be accepted."""


def _read(path):
    """Read one stable regular leaf, retaining identity for optimistic rechecks."""
    path = Path(path)
    try:
        before = path.lstat()
    except FileNotFoundError:
        return None
    if not stat.S_ISREG(before.st_mode):
        raise LedgerError("selected input is not a regular, non-symlink file")
    fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
    with os.fdopen(fd, "rb") as stream:
        opened = os.fstat(stream.fileno())
        if not stat.S_ISREG(opened.st_mode) or (opened.st_dev, opened.st_ino) != (before.st_dev, before.st_ino):
            raise LedgerError("selected input changed or is not a regular file")
        data = stream.read()
        after = os.fstat(stream.fileno())
    current = path.lstat()
    signature = lambda info: (info.st_dev, info.st_ino, info.st_size, info.st_mtime_ns)
    if not signature(before) == signature(opened) == signature(after) == signature(current):
        raise LedgerError("selected input changed while being read")
    return (signature(current), data)


def _recheck(path, expected):
    if _read(path) != expected:
        raise LedgerError("selected output changed since inspection")


def _private_parent(path):
    parent = Path(path).parent
    if not parent.exists():
        parent.mkdir(mode=0o700)
    info = parent.lstat()
    if (not stat.S_ISDIR(info.st_mode) or info.st_uid != os.geteuid()
            or stat.S_IMODE(info.st_mode) != 0o700):
        raise LedgerError("ledger/output directory must be owned, private (0700), and non-symlink")


def _atomic_write(path, data, expected):
    """Replace only the inspected leaf; preserve unexpected staging replacements."""
    path = Path(path)
    fd, temporary = tempfile.mkstemp(prefix="." + path.name + "-", dir=path.parent)
    stage = Path(temporary)
    owned = os.fstat(fd)
    try:
        with os.fdopen(fd, "wb") as stream:
            stream.write(data)
            stream.flush()
            os.fsync(stream.fileno())
        _recheck(path, expected)
        if expected is None:
            os.link(stage, path)  # no-clobber publication of a new file
        else:
            os.replace(stage, path)
        observed = _read(path)
        if observed is None or observed[1] != data:
            raise LedgerError("output could not be verified after publication")
        return observed
    finally:
        try:
            current = stage.lstat()
        except FileNotFoundError:
            current = None
        if current and (current.st_dev, current.st_ino) == (owned.st_dev, owned.st_ino):
            stage.unlink()


@contextmanager
def _ledger_lock(path):
    if Path(path).name == "history.md":
        raise LedgerError("ledger and adjacent history must be different files")
    _private_parent(path)
    lock_path = Path(str(path) + ".lock")
    fd = os.open(lock_path, os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
    try:
        opened = os.fstat(fd)
        if (not stat.S_ISREG(opened.st_mode) or stat.S_IMODE(opened.st_mode) != 0o600
                or opened.st_uid != os.geteuid()):
            raise LedgerError("ledger lock must be an owned private regular file")
        fcntl.flock(fd, fcntl.LOCK_EX)
        current = lock_path.lstat()
        if (opened.st_dev, opened.st_ino) != (current.st_dev, current.st_ino):
            raise LedgerError("ledger lock was replaced")
        yield
    finally:
        os.close(fd)


def _json(data):
    def unique_keys(items):
        result = {}
        for key, value in items:
            if key in result:
                raise LedgerError("duplicate JSON object keys; original retained")
            result[key] = value
        return result
    def reject_constant(_value):
        raise LedgerError("non-finite JSON number; original retained")
    def finite_float(text):
        value = float(text)
        if not math.isfinite(value):
            reject_constant(text)
        return value
    try:
        return json.loads(data.decode("utf-8"), object_pairs_hook=unique_keys,
                          parse_constant=reject_constant, parse_float=finite_float)
    except (UnicodeError, json.JSONDecodeError):
        raise LedgerError("selected JSON input is invalid; original retained") from None


def _empty():
    return {"version": 2, "updated_at": None, "todos": {}, "events": [],
            "history_base": "", "history_base_exists": False, "history_rendered": 0}


def _validate(value):
    if (not isinstance(value, dict) or type(value.get("version")) is not int
            or value["version"] not in (1, 2) or not isinstance(value.get("todos"), dict)):
        raise LedgerError("unsupported ledger schema; original retained")
    for key, entry in value["todos"].items():
        if (not isinstance(key, str) or not key or not isinstance(entry, dict)
                or entry.get("last_verdict") not in KINDS | {"UNKNOWN"}
                or type(entry.get("attempts")) is not int or entry["attempts"] < 1
                or not isinstance(entry.get("last_reason"), str)
                or not isinstance(entry.get("last_attempted_at"), str)
                or (entry.get("heading_hash") is not None
                    and (not isinstance(entry["heading_hash"], str)
                         or re.fullmatch(r"sha256:[0-9a-f]{64}", entry["heading_hash"]) is None))
                or (entry.get("last_ease") is not None
                    and (type(entry["last_ease"]) is not int or not 1 <= entry["last_ease"] <= 5))):
            raise LedgerError("invalid ledger entry; original retained")
    if value["version"] == 2:
        events = value.get("events")
        if (not isinstance(events, list) or type(value.get("history_rendered")) is not int
                or not 0 <= value["history_rendered"] <= len(events)
                or type(value.get("history_base_exists")) is not bool
                or not isinstance(value.get("history_base"), str)):
            raise LedgerError("invalid history recovery state")
        try:
            base64.b64decode(value["history_base"], validate=True)
        except (ValueError, binascii.Error):
            raise LedgerError("invalid preserved history") from None
        seen = set()
        for event in events:
            if (not isinstance(event, dict) or not isinstance(event.get("operation_id"), str)
                    or event["operation_id"] in seen or not isinstance(event.get("request"), dict)
                    or not isinstance(event.get("history_text"), str)):
                raise LedgerError("invalid operation history")
            seen.add(event["operation_id"])
    return value


def load_ledger(path: str) -> dict:
    snapshot = _read(path)
    return _empty() if snapshot is None else _validate(_json(snapshot[1]))


def save_ledger(path, value, expected):
    value["updated_at"] = datetime.now(timezone.utc).isoformat()
    _validate(value)
    return _atomic_write(path, (json.dumps(value, ensure_ascii=False, indent=2) + "\n").encode(), expected)


def heading_hash(file_path: str, org_id: str) -> str | None:
    """Hash a proved file/subtree ID, including its ancestor and file context.

    This is a conservative non-evaluating Org subset, not a general Org parser.
    Block contents and non-property ID text cannot claim identity. Ambiguous or
    unsupported property placement returns None, which never suppresses work.
    """
    if not isinstance(org_id, str) or not org_id:
        return None
    try:
        snapshot = _read(file_path)
        if snapshot is None:
            return None
        text = snapshot[1].decode("utf-8")
    except (OSError, UnicodeError, LedgerError):
        return None
    lines = text.splitlines(keepends=True)
    active = []
    block = None
    for index, line in enumerate(lines):
        if block:
            if re.match(r"^\s*#\+end_" + re.escape(block) + r"\s*$", line, re.I):
                block = None
            continue
        start = re.match(r"^\s*#\+begin_([A-Za-z0-9_]+)(?:\s|$)", line, re.I)
        if start:
            block = start.group(1)
            continue
        # Dynamic blocks and unmatched block endings are unsupported.
        if re.match(r"^\s*#\+(?:begin:|end:|end_)", line, re.I):
            return None
        active.append(index)
    if block:
        return None
    active_set = set(active)
    headings = [(i, len(match.group(1))) for i in active
                if (match := re.match(r"^(\*+)[ \t]+", lines[i]))]
    first = headings[0][0] if headings else len(lines)
    regions = [(-1, 0, first)] + [
        (i, level, headings[n + 1][0] if n + 1 < len(headings) else len(lines))
        for n, (i, level) in enumerate(headings)]
    matches = []
    for start, level, section_end in regions:
        cursor = start + 1
        # Native Org permits leading blanks/comments, not metadata keywords.
        if start == -1:
            while cursor < section_end and cursor in active_set and (
                not lines[cursor].strip() or re.match(r"^\s*#(?:\s|$)", lines[cursor])):
                cursor += 1
        else:
            while cursor < section_end and cursor in active_set and re.fullmatch(
                    r"\s*(?:(?:SCHEDULED|DEADLINE|CLOSED):[ \t]*"
                    r"(?:<\d{4}-\d{2}-\d{2}[^>\n]*>|\[\d{4}-\d{2}-\d{2}[^\]\n]*\])\s*)+",
                    lines[cursor]):
                cursor += 1
        if cursor not in active_set or cursor >= section_end or lines[cursor].strip().upper() != ":PROPERTIES:":
            continue
        cursor += 1
        ids = []
        while cursor < section_end and cursor in active_set and lines[cursor].strip().upper() != ":END:":
            prop = re.fullmatch(r"\s*:([A-Za-z0-9_@#%+.-]+):[ \t]*(.*?)[ \t]*\r?\n?", lines[cursor])
            if prop is None:
                return None
            if prop.group(1).upper() == "ID":
                ids.append(prop.group(2))
            cursor += 1
        if cursor >= section_end or cursor not in active_set:
            return None
        if org_id in ids:
            if len(ids) != 1:
                return None
            matches.append((start, level))
    if len(matches) != 1:
        return None
    start, level = matches[0]
    if start == -1:
        pieces = [text]
    else:
        end = next((i for i, depth in headings if i > start and depth <= level), len(lines))
        ancestors = []
        for i, depth in headings:
            if i >= start:
                break
            while ancestors and ancestors[-1][1] >= depth:
                ancestors.pop()
            ancestors.append((i, depth))
        pieces = ["".join(lines[:first])]
        for i, _depth in ancestors:
            section_end = next((j for j, _ in headings if j > i), len(lines))
            pieces.append("".join(lines[i:section_end]))
        pieces.append("".join(lines[start:end]))
    return "sha256:" + hashlib.sha256(json.dumps(pieces, ensure_ascii=False).encode()).hexdigest()


def parse_verdict(verdict_text: str) -> tuple[str, int | None]:
    if not isinstance(verdict_text, str) or "\n" in verdict_text or "\r" in verdict_text:
        raise LedgerError("verdict must be a single line")
    match = re.fullmatch(r"(COMPLETED|FAILED|BLOCKED|DEFERRED):[ \t]*(\S.*)", verdict_text.strip(), re.I)
    if not match or not match.group(2).split("|", 1)[0].strip():
        raise LedgerError("verdict needs a known kind, colon and nonempty summary")
    kind = match.group(1).upper()
    fields = [part.strip() for part in match.group(2).split("|")[1:]]
    ease_fields = [part for part in fields if re.match(r"ease\b", part, re.I)]
    if len(ease_fields) > 1 or any(re.fullmatch(r"ease\s*=\s*[1-5]", part, re.I) is None for part in ease_fields):
        raise LedgerError("ease must be one integer from 1 to 5")
    ease = int(ease_fields[0].split("=")[1]) if ease_fields else None
    if kind == "BLOCKED" and ease is None:
        raise LedgerError("BLOCKED verdict requires ease=1 through ease=5")
    return kind, ease


def _window(skip_window_days):
    if type(skip_window_days) is not int or skip_window_days < 0:
        raise LedgerError("skip window must be a nonnegative integer")
    now = datetime.now(timezone.utc)
    try:
        return now, now - timedelta(days=skip_window_days)
    except OverflowError:
        raise LedgerError("skip window is outside the supported date range") from None


def should_skip(entry: dict, current_hash: str | None, skip_window_days: int) -> bool:
    now, cutoff = _window(skip_window_days)
    if entry.get("last_verdict") != "BLOCKED" or not current_hash or entry.get("heading_hash") != current_hash:
        return False
    last = entry.get("last_attempted_at")
    if not isinstance(last, str):
        return False
    try:
        when = datetime.fromisoformat(last.replace("Z", "+00:00"))
    except ValueError:
        return False
    if when.tzinfo is None or when.utcoffset() is None:
        return False
    return cutoff < when <= now


def cmd_filter(args):
    _window(args.skip_window_days)
    output = Path(args.output).resolve()
    if output in {Path(path).resolve() for path in
                  (args.classifications, args.ledger, _history_path(args.ledger), str(args.ledger) + ".lock")}:
        raise LedgerError("filtered output must not alias an input or ledger artifact")
    snapshot = _read(args.classifications)
    if snapshot is None:
        raise LedgerError("classifications input is missing")
    cls = _json(snapshot[1])
    if not isinstance(cls, dict):
        raise LedgerError("classifications must be an object")
    for bucket in ("blocked", "candidate", "investigate"):
        if not isinstance(cls.get(bucket), list) or any(
            not isinstance(rec, dict) or any(not isinstance(rec.get(key), str) or not rec[key]
                                             for key in ("id", "file", "title"))
            for rec in cls[bucket]):
            raise LedgerError("classifications contain invalid records")
    state = load_ledger(args.ledger)
    out = {"blocked": cls["blocked"], "candidate": [], "investigate": [], "still_blocked": []}
    for bucket in ("candidate", "investigate"):
        for original in cls[bucket]:
            rec = dict(original)
            current_hash = heading_hash(rec["file"], rec["id"])
            entry = state["todos"].get(rec["id"])
            if entry and should_skip(entry, current_hash, args.skip_window_days):
                rec.update(_ledger_entry=entry, _bucket=bucket)
                out["still_blocked"].append(rec)
            else:
                rec["_current_hash"] = current_hash
                out[bucket].append(rec)
    _private_parent(args.output)
    if _read(args.output) is not None:
        raise LedgerError("filtered output already exists")
    _atomic_write(args.output, (json.dumps(out, ensure_ascii=False) + "\n").encode(), None)
    for label, bucket in (("BLOCKED_TITLE", "blocked"), ("CANDIDATE_KEPT", "candidate"),
                          ("INVESTIGATE_KEPT", "investigate"), ("STILL_BLOCKED", "still_blocked")):
        print(f"{label}: {len(out[bucket])}")


def _history_path(path):
    return Path(path).parent / "history.md"


def _render_history(value, count):
    base = base64.b64decode(value["history_base"])
    if not value["history_base_exists"] and count:
        base = HISTORY_HEADER
    elif count and base and not base.endswith(b"\n"):
        base += b"\n"
    return base + "".join(event["history_text"] for event in value["events"][:count]).encode()


def _history_snapshot(value, path):
    observed = _read(path)
    done = value["history_rendered"]
    expected_absent = not value["history_base_exists"] and done == 0
    if observed is not None and expected_absent and not value["events"]:
        raise LedgerError("existing adjacent history requires explicit migration")
    if observed is None:
        if not expected_absent:
            raise LedgerError("history is missing; external change requires review")
    elif expected_absent and observed[1] != _render_history(value, len(value["events"])):
        raise LedgerError("unexpected history appeared; external edit retained")
    elif observed[1] not in {_render_history(value, done), _render_history(value, len(value["events"]))}:
        raise LedgerError("history differs from recorded recovery state; external edit retained")
    return observed


def _finish_history(path, value, ledger_snapshot):
    _recheck(path, ledger_snapshot)
    history = _history_path(path)
    snapshot = _history_snapshot(value, history)
    data = _render_history(value, len(value["events"]))
    if snapshot is None or snapshot[1] != data:
        _atomic_write(history, data, snapshot)
    elif value["history_rendered"] == len(value["events"]):
        return
    value["history_rendered"] = len(value["events"])
    save_ledger(path, value, ledger_snapshot)


def _request(args):
    if not isinstance(args.operation_id, str) or re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._:-]{0,127}", args.operation_id) is None:
        raise LedgerError("operation ID must be a stable nonempty token")
    if not isinstance(args.id, str) or not args.id.strip() or not isinstance(args.title, str) or not args.title.strip():
        raise LedgerError("record requires a nonempty ID and title")
    parse_verdict(args.verdict)
    return {"id": args.id, "file": str(Path(args.file).absolute()),
            "title": args.title, "verdict": args.verdict.strip()}


def cmd_record(args):
    request = _request(args)
    with _ledger_lock(args.ledger):
        snapshot = _read(args.ledger)
        value = _empty() if snapshot is None else _validate(_json(snapshot[1]))
        if value["version"] != 2:
            raise LedgerError("legacy ledger is read-only; run explicit migrate before recording")
        prior = next((event for event in value["events"] if event["operation_id"] == args.operation_id), None)
        if prior is not None and prior["request"] != request:
            raise LedgerError("operation ID already belongs to a different request")
        _history_snapshot(value, _history_path(args.ledger))
        if prior is None:
            kind, ease = parse_verdict(request["verdict"])
            current_hash = heading_hash(request["file"], request["id"])
            if current_hash is None and kind in {"COMPLETED", "BLOCKED"}:
                raise LedgerError("note ID/file identity could not be proved; no verdict recorded")
            previous = value["todos"].get(request["id"], {})
            now = datetime.now(timezone.utc).isoformat()
            value["todos"][request["id"]] = {
                "last_verdict": kind, "last_ease": ease, "last_reason": request["verdict"],
                "last_attempted_at": now, "heading_hash": current_hash,
                "attempts": previous.get("attempts", 0) + 1,
                "title": request["title"], "file": request["file"]}
            # Preserve exact data in the event request, but render it inert in Markdown.
            display = html.escape(json.dumps({"when": now, **request}, ensure_ascii=False), quote=False)
            display = re.sub(r"([\\`*_[\]{}()!#|])", r"\\\1", display)
            line = "- " + display + "\n"
            value["events"].append({"operation_id": args.operation_id, "request": request,
                                    "history_text": line})
            try:
                snapshot = save_ledger(args.ledger, value, snapshot)
            except (OSError, LedgerError):
                raise LedgerError("ledger publication failed or is uncertain; retry the SAME operation ID") from None
        try:
            _finish_history(args.ledger, value, snapshot)
        except (OSError, LedgerError):
            raise LedgerError("verdict is recorded; history/checkpoint is incomplete; retry the SAME operation ID") from None
    print(f"RECORDED: {args.id} {'RECONCILED' if prior else value['todos'][args.id]['last_verdict']}")


def cmd_migrate(args):
    with _ledger_lock(args.ledger):
        snapshot = _read(args.ledger)
        old = {"version": 1, "updated_at": None, "todos": {}} if snapshot is None else _validate(_json(snapshot[1]))
        if old["version"] == 2:
            raise LedgerError("ledger already uses version 2")
        history = _read(_history_path(args.ledger))
        value = _empty()
        value["todos"] = old["todos"]
        value["history_base_exists"] = history is not None
        value["history_base"] = base64.b64encode(history[1] if history else b"").decode("ascii")
        _recheck(_history_path(args.ledger), history)
        save_ledger(args.ledger, value, snapshot)
    print("MIGRATED: existing adjacent history preserved; no past events fabricated")


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="cmd", required=True)
    filtering = sub.add_parser("filter")
    filtering.add_argument("--classifications", required=True)
    filtering.add_argument("--ledger", required=True)
    filtering.add_argument("--output", required=True)
    filtering.add_argument("--skip-window-days", type=int, default=DEFAULT_SKIP_WINDOW_DAYS)
    filtering.set_defaults(func=cmd_filter)
    recording = sub.add_parser("record")
    for field in ("ledger", "id", "file", "title", "verdict", "operation-id"):
        recording.add_argument("--" + field, required=True)
    recording.set_defaults(func=cmd_record)
    migrating = sub.add_parser("migrate")
    migrating.add_argument("--ledger", required=True)
    migrating.set_defaults(func=cmd_migrate)
    args = parser.parse_args(argv)
    try:
        args.func(args)
    except (LedgerError, OSError):
        # Never include raw JSON, note content or filesystem exception payloads.
        error = sys.exc_info()[1]
        print(str(error) if isinstance(error, LedgerError) else "selected file operation failed; originals retained where possible",
              file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
