#!/usr/bin/env python3
"""Cross-run state ledger for the overnight-todos skill.

Tracks per-TODO verdicts so future runs do not re-attempt TODOs that were
blocked on stable content. A verdict is "forgotten" automatically when the
heading body changes (the content hash differs), so editing the heading
to clear the blocker re-queues it for the next run.

State file shape:

    {
      "version": 1,
      "updated_at": "<iso8601>",
      "todos": {
        "<org-id>": {
          "last_verdict": "COMPLETED|FAILED|BLOCKED|DEFERRED",
          "last_ease": 4,
          "last_reason": "<verdict-line text>",
          "last_attempted_at": "<iso8601>",
          "heading_hash": "sha256:<hex>",
          "attempts": <int>
        }
      }
    }

Usage:

  ledger.py filter --classifications IN.json --ledger STATE.json
                   --output FILTERED.json [--skip-window-days N]
  ledger.py record --ledger STATE.json --id ORG_ID --file FILE
                   --verdict TEXT
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

DEFAULT_SKIP_WINDOW_DAYS = 14
HEADING_LINE = re.compile(r"^\*+ ", re.MULTILINE)


def load_ledger(path: str) -> dict:
    p = Path(path)
    if not p.exists():
        return {"version": 1, "updated_at": None, "todos": {}}
    try:
        return json.loads(p.read_text())
    except json.JSONDecodeError:
        # Corrupt file -- back it up and start fresh rather than abort the run.
        backup = p.with_suffix(p.suffix + ".corrupt." + datetime.now().strftime("%Y%m%d%H%M%S"))
        p.rename(backup)
        return {"version": 1, "updated_at": None, "todos": {}}


def save_ledger(path: str, ledger: dict) -> None:
    ledger["updated_at"] = datetime.now(timezone.utc).isoformat()
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    Path(path).write_text(json.dumps(ledger, ensure_ascii=False, indent=2))


def heading_hash(file_path: str, org_id: str) -> str | None:
    """Return a sha256 of the heading body identified by ORG_ID, or None."""
    try:
        text = Path(file_path).read_text()
    except (OSError, UnicodeDecodeError):
        return None
    id_match = re.search(r"^:ID:\s+" + re.escape(org_id) + r"\s*$", text, re.MULTILINE)
    if not id_match:
        return None
    head_above = None
    for hm in HEADING_LINE.finditer(text[: id_match.start()]):
        head_above = hm
    if not head_above:
        return None
    start = head_above.start()
    next_head = HEADING_LINE.search(text[head_above.end():])
    end = head_above.end() + next_head.start() if next_head else len(text)
    body = text[start:end].strip()
    return "sha256:" + hashlib.sha256(body.encode("utf-8")).hexdigest()


def parse_verdict(verdict_text: str) -> tuple[str, int | None]:
    kind_match = re.match(r"^(COMPLETED|FAILED|BLOCKED|DEFERRED)\b", verdict_text.strip(), re.IGNORECASE)
    kind = kind_match.group(1).upper() if kind_match else "UNKNOWN"
    ease_match = re.search(r"\bease\s*=\s*(\d)\b", verdict_text)
    ease = int(ease_match.group(1)) if ease_match else None
    return kind, ease


def should_skip(entry: dict, current_hash: str | None, skip_window_days: int) -> bool:
    """Skip a candidate only if it was BLOCKED, content is unchanged, and the
    attempt is within the skip window."""
    if entry.get("last_verdict") != "BLOCKED":
        return False
    if not current_hash or entry.get("heading_hash") != current_hash:
        return False
    last = entry.get("last_attempted_at")
    if not last:
        return False
    try:
        when = datetime.fromisoformat(last.replace("Z", "+00:00"))
    except ValueError:
        return False
    cutoff = datetime.now(timezone.utc) - timedelta(days=skip_window_days)
    return when > cutoff


def cmd_filter(args):
    cls = json.loads(Path(args.classifications).read_text())
    ledger = load_ledger(args.ledger)
    todos_state = ledger.get("todos", {})
    out = {
        "blocked": cls.get("blocked", []),
        "candidate": [],
        "investigate": [],
        "still_blocked": [],
    }
    for bucket in ("candidate", "investigate"):
        for rec in cls.get(bucket, []):
            cur_hash = heading_hash(rec.get("file") or "", rec.get("id") or "")
            entry = todos_state.get(rec.get("id"))
            if entry and should_skip(entry, cur_hash, args.skip_window_days):
                rec["_ledger_entry"] = entry
                rec["_bucket"] = bucket
                out["still_blocked"].append(rec)
            else:
                rec["_current_hash"] = cur_hash
                out[bucket].append(rec)
    Path(args.output).write_text(json.dumps(out, ensure_ascii=False))
    print(f"BLOCKED_TITLE: {len(out['blocked'])}")
    print(f"CANDIDATE_KEPT: {len(out['candidate'])}")
    print(f"INVESTIGATE_KEPT: {len(out['investigate'])}")
    print(f"STILL_BLOCKED: {len(out['still_blocked'])}")


def cmd_record(args):
    ledger = load_ledger(args.ledger)
    ledger.setdefault("todos", {})
    kind, ease = parse_verdict(args.verdict)
    cur_hash = heading_hash(args.file, args.id) if args.file else None
    existing = ledger["todos"].get(args.id, {})
    ledger["todos"][args.id] = {
        "last_verdict": kind,
        "last_ease": ease,
        "last_reason": args.verdict.strip(),
        "last_attempted_at": datetime.now(timezone.utc).isoformat(),
        "heading_hash": cur_hash,
        "attempts": existing.get("attempts", 0) + 1,
    }
    save_ledger(args.ledger, ledger)
    print(f"RECORDED: {args.id} {kind}{(' ease=' + str(ease)) if ease else ''}")


def main(argv):
    p = argparse.ArgumentParser(description=__doc__)
    sub = p.add_subparsers(dest="cmd", required=True)
    pf = sub.add_parser("filter", help="Filter triaged candidates against the ledger")
    pf.add_argument("--classifications", required=True)
    pf.add_argument("--ledger", required=True)
    pf.add_argument("--output", required=True)
    pf.add_argument("--skip-window-days", type=int, default=DEFAULT_SKIP_WINDOW_DAYS)
    pf.set_defaults(func=cmd_filter)
    pr = sub.add_parser("record", help="Record one subagent verdict to the ledger")
    pr.add_argument("--ledger", required=True)
    pr.add_argument("--id", required=True)
    pr.add_argument("--file", required=True)
    pr.add_argument("--verdict", required=True)
    pr.set_defaults(func=cmd_record)
    args = p.parse_args(argv)
    args.func(args)


if __name__ == "__main__":
    main(sys.argv[1:])
