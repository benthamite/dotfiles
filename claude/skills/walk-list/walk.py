#!/usr/bin/env python3
"""walk-list: strict one-at-a-time OR N-at-a-time item processor.

On `start`, the input file is MOVED to ~/.claude/walk-list-data/<uuid>/source.json
and replaced at the original path with a stub. A PreToolUse hook blocks
every tool from accessing the protected store except walk.py itself.

Sequential mode (default, max_concurrent=1):
    walk.py start <file>
    walk.py next <file> <decision>          # repeat until done
    walk.py restore <file>

Pool mode (parallel subagent dispatch, max_concurrent=N):
    walk.py start <file> --max-concurrent 10
    # main session loop:
    walk.py pool-status <file>
    walk.py dispatch <file>                 # returns TOKEN + item JSON
    # ... spawn subagent with prompt using that TOKEN
    # subagent itself calls:
    walk.py record <file> <TOKEN> <decision>
    # main session dispatches more when slots free up
    walk.py restore <file>

Utilities:
    walk.py status <file>
    walk.py show-decisions <file>
    walk.py release-stale <file> <max-age-seconds>
    walk.py set-max-concurrent <file> <n>
    walk.py abort <file>
"""
import contextlib
import fcntl
import json
import shutil
import sys
import uuid
from datetime import datetime, timezone
from pathlib import Path


DATA_ROOT = Path.home() / ".claude" / "walk-list-data"
REGISTRY_PATH = DATA_ROOT / "registry.json"
STUB_MARKER = "_walk_locked"
DEFAULT_MAX = 1


def ensure_root() -> None:
    DATA_ROOT.mkdir(parents=True, exist_ok=True)


def load_registry() -> dict:
    if not REGISTRY_PATH.exists():
        return {}
    return json.loads(REGISTRY_PATH.read_text(encoding="utf-8"))


def save_registry(reg: dict) -> None:
    ensure_root()
    REGISTRY_PATH.write_text(json.dumps(reg, indent=2), encoding="utf-8")


def canonical_key(input_file: Path) -> str:
    return str(input_file.resolve(strict=False))


def resolve_session(input_file: Path) -> tuple[str, Path]:
    reg = load_registry()
    key = canonical_key(input_file)
    sid = reg.get(key)
    if not sid:
        for k, v in reg.items():
            if Path(k).name == input_file.name:
                sid = v
                break
    if not sid:
        raise SystemExit(
            f"ERROR: no active walk for {input_file}. Run: walk.py start {input_file}"
        )
    sdir = DATA_ROOT / sid
    if not sdir.exists():
        raise SystemExit(f"ERROR: registry points to missing session dir {sdir}")
    return sid, sdir


@contextlib.contextmanager
def locked_state(sdir: Path):
    """Read-modify-write the state file under an exclusive flock so concurrent
    walk.py invocations (e.g. subagents calling `record` simultaneously) don't
    clobber each other."""
    state_path = sdir / "state.json"
    with open(state_path, "r+", encoding="utf-8") as f:
        fcntl.flock(f.fileno(), fcntl.LOCK_EX)
        try:
            f.seek(0)
            data = json.load(f)
            data.setdefault("in_flight", {})
            data.setdefault("to_redispatch", [])
            data.setdefault("max_concurrent", 1)
            yield data
            f.seek(0)
            f.truncate()
            json.dump(data, f, indent=2, ensure_ascii=False)
            f.flush()
        finally:
            fcntl.flock(f.fileno(), fcntl.LOCK_UN)


def parse_items(raw: str) -> list:
    raw = raw.strip()
    if not raw:
        return []
    if raw.startswith("["):
        data = json.loads(raw)
        if not isinstance(data, list):
            raise SystemExit("ERROR: top-level JSON must be an array")
        return data
    items = []
    for line in raw.splitlines():
        line = line.strip()
        if not line:
            continue
        if line.startswith("{") or line.startswith("["):
            try:
                items.append(json.loads(line))
                continue
            except json.JSONDecodeError:
                pass
        items.append(line)
    return items


def load_source_items(sdir: Path) -> list:
    return parse_items((sdir / "source.json").read_text(encoding="utf-8"))


def is_stub(path: Path) -> bool:
    if not path.exists():
        return False
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
        return isinstance(data, dict) and data.get(STUB_MARKER) is True
    except (json.JSONDecodeError, OSError):
        return False


def print_item(index: int, total: int, item) -> None:
    print(f"=== ITEM {index + 1} OF {total} ===")
    if isinstance(item, (dict, list)):
        print(json.dumps(item, indent=2, ensure_ascii=False))
    else:
        print(item)


def cmd_start(input_file: Path, max_concurrent: int) -> None:
    if is_stub(input_file):
        sid, sdir = resolve_session(input_file)
        items = load_source_items(sdir)
        with locked_state(sdir) as state:
            max_c = state["max_concurrent"]
            cursor = state["cursor"]
            in_flight = state["in_flight"]
            done = len(state["decisions"])
            print(f"RESUMING walk (session {sid[:8]}). max_concurrent={max_c}.")
            print(
                f"Total: {len(items)}  Done: {done}  In-flight: {len(in_flight)}  "
                f"Cursor: {cursor}  Redispatch-queue: {len(state['to_redispatch'])}"
            )
            if done >= len(items) and not in_flight:
                print(f"WALK COMPLETE. Run: walk.py restore {input_file}")
                return
            if max_c == 1 and not in_flight and cursor < len(items):
                print()
                print_item(cursor, len(items), items[cursor])
                print()
                print(f"When done: walk.py next {input_file} '<decision>'")
            else:
                print(f"Use: walk.py dispatch {input_file}  (to claim items)")
        return

    if not input_file.exists():
        raise SystemExit(f"ERROR: input file not found: {input_file}")
    items = parse_items(input_file.read_text(encoding="utf-8"))
    if not items:
        raise SystemExit(f"ERROR: no items found in {input_file}")

    ensure_root()
    sid = uuid.uuid4().hex
    sdir = DATA_ROOT / sid
    sdir.mkdir(parents=True, exist_ok=False)
    source_path = sdir / "source.json"
    shutil.move(str(input_file), str(source_path))
    state = {
        "cursor": 0,
        "in_flight": {},
        "to_redispatch": [],
        "decisions": [],
        "max_concurrent": max_concurrent,
        "original_path": str(input_file),
    }
    (sdir / "state.json").write_text(
        json.dumps(state, indent=2, ensure_ascii=False), encoding="utf-8"
    )
    reg = load_registry()
    reg[canonical_key(input_file)] = sid
    save_registry(reg)

    stub = {
        STUB_MARKER: True,
        "message": "Locked by walk-list. Use walk.py commands to interact.",
        "session_prefix": sid[:8],
        "max_concurrent": max_concurrent,
        "commands": {
            "dispatch (pool)": f"python ~/.claude/skills/walk-list/walk.py dispatch {input_file}",
            "record (pool)": f"python ~/.claude/skills/walk-list/walk.py record {input_file} <token> '<decision>'",
            "pool-status": f"python ~/.claude/skills/walk-list/walk.py pool-status {input_file}",
            "next (sequential)": f"python ~/.claude/skills/walk-list/walk.py next {input_file} '<decision>'",
            "status": f"python ~/.claude/skills/walk-list/walk.py status {input_file}",
            "restore": f"python ~/.claude/skills/walk-list/walk.py restore {input_file}",
        },
    }
    input_file.write_text(json.dumps(stub, indent=2), encoding="utf-8")

    print(
        f"STARTED walk (session {sid[:8]}). {len(items)} items locked. "
        f"max_concurrent={max_concurrent}."
    )
    if max_concurrent == 1:
        print()
        print_item(0, len(items), items[0])
        print()
        print(f"When done: walk.py next {input_file} '<decision>'")
    else:
        print(
            f"Use: walk.py dispatch {input_file}  (claim up to {max_concurrent} items in flight)"
        )


def cmd_next(input_file: Path, decision: str) -> None:
    if not decision.strip():
        raise SystemExit("ERROR: decision text required.")
    sid, sdir = resolve_session(input_file)
    items = load_source_items(sdir)
    with locked_state(sdir) as state:
        if state["in_flight"]:
            raise SystemExit(
                "ERROR: in-flight claims exist. Use `walk.py record <token> <decision>` "
                "for pool-mode advance, or `walk.py release-stale` to reclaim stuck claims."
            )
        cursor = state["cursor"]
        if cursor >= len(items) and not state["to_redispatch"]:
            print(f"Already complete. Run: walk.py restore {input_file}")
            return
        # In sequential mode, consume either a redispatch-queue entry or cursor
        if state["to_redispatch"]:
            index = state["to_redispatch"].pop(0)
        else:
            index = cursor
            state["cursor"] = cursor + 1
        state["decisions"].append(
            {"index": index, "item": items[index], "decision": decision.strip()}
        )
        remaining = (len(items) - state["cursor"]) + len(state["to_redispatch"])
        print(f"Recorded decision for item {index + 1}. Remaining: {remaining}")
        if remaining == 0:
            print(f"WALK COMPLETE. Run: walk.py restore {input_file}")
            return
        # Show next item
        if state["to_redispatch"]:
            next_index = state["to_redispatch"][0]
        else:
            next_index = state["cursor"]
        print()
        print_item(next_index, len(items), items[next_index])
        print()
        print(f"When done: walk.py next {input_file} '<decision>'")


def cmd_dispatch(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    items = load_source_items(sdir)
    with locked_state(sdir) as state:
        max_c = state["max_concurrent"]
        if len(state["in_flight"]) >= max_c:
            raise SystemExit(
                f"ERROR: pool full ({len(state['in_flight'])}/{max_c} in flight). "
                "Wait for a record() call."
            )
        if state["to_redispatch"]:
            index = state["to_redispatch"].pop(0)
        else:
            cursor = state["cursor"]
            if cursor >= len(items):
                raise SystemExit("ERROR: no more items to dispatch.")
            index = cursor
            state["cursor"] = cursor + 1
        token = uuid.uuid4().hex
        state["in_flight"][token] = {
            "index": index,
            "item": items[index],
            "dispatched_at": datetime.now(timezone.utc).isoformat(),
        }
        in_flight_n = len(state["in_flight"])
        remaining = (len(items) - state["cursor"]) + len(state["to_redispatch"])
    print(f"CLAIM_TOKEN: {token}")
    print(f"INDEX: {index + 1} OF {len(items)}")
    print(f"IN_FLIGHT: {in_flight_n}/{max_c}  REMAINING_TO_CLAIM: {remaining}")
    print()
    item = items[index]
    if isinstance(item, (dict, list)):
        print(json.dumps(item, indent=2, ensure_ascii=False))
    else:
        print(item)


def cmd_record(input_file: Path, token: str, decision: str) -> None:
    if not decision.strip():
        raise SystemExit("ERROR: decision text required.")
    sid, sdir = resolve_session(input_file)
    with locked_state(sdir) as state:
        if token not in state["in_flight"]:
            raise SystemExit(
                f"ERROR: unknown or already-recorded claim token: {token}"
            )
        claim = state["in_flight"].pop(token)
        state["decisions"].append(
            {
                "index": claim["index"],
                "item": claim["item"],
                "decision": decision.strip(),
                "claim_token": token,
                "dispatched_at": claim.get("dispatched_at"),
                "recorded_at": datetime.now(timezone.utc).isoformat(),
            }
        )
        in_flight_n = len(state["in_flight"])
        done_n = len(state["decisions"])
    print(
        f"Recorded decision for item {claim['index'] + 1}. "
        f"in_flight={in_flight_n} done={done_n}"
    )


def cmd_pool_status(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    items = load_source_items(sdir)
    with locked_state(sdir) as state:
        total = len(items)
        in_flight = len(state["in_flight"])
        done = len(state["decisions"])
        cursor = state["cursor"]
        max_c = state["max_concurrent"]
        available = max_c - in_flight
        remaining_to_claim = (total - cursor) + len(state["to_redispatch"])
        out = {
            "session": sid[:8],
            "total": total,
            "done": done,
            "in_flight": in_flight,
            "max_concurrent": max_c,
            "available_slots": available,
            "remaining_to_claim": remaining_to_claim,
            "cursor": cursor,
            "redispatch_queue": len(state["to_redispatch"]),
        }
    print(json.dumps(out, indent=2))


def cmd_status(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    items = load_source_items(sdir)
    with locked_state(sdir) as state:
        total = len(items)
        print(f"Session: {sid[:8]}")
        print(f"Input: {input_file}")
        print(
            f"Total: {total}  Done: {len(state['decisions'])}  "
            f"In-flight: {len(state['in_flight'])}  Cursor: {state['cursor']}  "
            f"max_concurrent: {state['max_concurrent']}  "
            f"redispatch-queue: {len(state['to_redispatch'])}"
        )
        if state["in_flight"]:
            print("In-flight claims:")
            for tok, claim in state["in_flight"].items():
                print(
                    f"  item {claim['index'] + 1}  token {tok[:8]}  "
                    f"dispatched_at {claim['dispatched_at']}"
                )
        if state["decisions"]:
            last = sorted(state["decisions"], key=lambda x: x.get("recorded_at", x.get("index")))[-1]
            print(f"Last decision (item {last['index'] + 1}): {last['decision']}")


def cmd_show_decisions(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    with locked_state(sdir) as state:
        if not state["decisions"]:
            print("No decisions recorded.")
            return
        for d in sorted(state["decisions"], key=lambda x: x["index"]):
            print(f"[{d['index'] + 1}] {d['decision']}")


def cmd_release_stale(input_file: Path, max_age_seconds: str) -> None:
    sid, sdir = resolve_session(input_file)
    threshold = float(max_age_seconds)
    now = datetime.now(timezone.utc)
    released = []
    with locked_state(sdir) as state:
        new_in_flight = {}
        for token, claim in state["in_flight"].items():
            dispatched_at = datetime.fromisoformat(claim["dispatched_at"])
            age = (now - dispatched_at).total_seconds()
            if age > threshold:
                released.append((claim["index"], token, age))
                state["to_redispatch"].append(claim["index"])
            else:
                new_in_flight[token] = claim
        state["in_flight"] = new_in_flight
    if released:
        for idx, tok, age in released:
            print(
                f"Released claim on item {idx + 1} (token {tok[:8]}, age {age:.0f}s). "
                "Re-queued for dispatch."
            )
    else:
        print("No stale claims.")


def cmd_set_max_concurrent(input_file: Path, n: str) -> None:
    sid, sdir = resolve_session(input_file)
    with locked_state(sdir) as state:
        state["max_concurrent"] = int(n)
    print(f"max_concurrent set to {n}")


def cmd_restore(input_file: Path, preserve: bool = True) -> None:
    sid, sdir = resolve_session(input_file)
    source_path = sdir / "source.json"
    state_path = sdir / "state.json"
    with locked_state(sdir) as state:
        if state["in_flight"] and preserve:
            print(
                f"WARNING: {len(state['in_flight'])} in-flight claims will be discarded. "
                "Use `walk.py release-stale <file> 0` first if you want them re-queued."
            )
    if preserve and state_path.exists():
        out = input_file.with_suffix(input_file.suffix + ".walk-decisions.json")
        shutil.copy(str(state_path), str(out))
        print(f"Decisions preserved at: {out}")
    if input_file.exists():
        input_file.unlink()
    shutil.move(str(source_path), str(input_file))
    reg = load_registry()
    reg.pop(canonical_key(input_file), None)
    save_registry(reg)
    shutil.rmtree(sdir)
    print(f"Restored {input_file}.")


def cmd_abort(input_file: Path) -> None:
    cmd_restore(input_file, preserve=False)


def main() -> None:
    args = sys.argv[1:]
    if not args:
        print(__doc__)
        raise SystemExit(1)
    cmd = args[0]
    if cmd == "start":
        if len(args) < 2:
            raise SystemExit("Usage: walk.py start <file> [--max-concurrent N]")
        input_file = Path(args[1])
        max_c = DEFAULT_MAX
        rest = args[2:]
        if "--max-concurrent" in rest:
            i = rest.index("--max-concurrent")
            max_c = int(rest[i + 1])
        cmd_start(input_file, max_c)
    elif cmd == "next" and len(args) >= 3:
        cmd_next(Path(args[1]), " ".join(args[2:]))
    elif cmd == "dispatch" and len(args) == 2:
        cmd_dispatch(Path(args[1]))
    elif cmd == "record" and len(args) >= 4:
        cmd_record(Path(args[1]), args[2], " ".join(args[3:]))
    elif cmd == "pool-status" and len(args) == 2:
        cmd_pool_status(Path(args[1]))
    elif cmd == "status" and len(args) == 2:
        cmd_status(Path(args[1]))
    elif cmd == "show-decisions" and len(args) == 2:
        cmd_show_decisions(Path(args[1]))
    elif cmd == "release-stale" and len(args) == 3:
        cmd_release_stale(Path(args[1]), args[2])
    elif cmd == "set-max-concurrent" and len(args) == 3:
        cmd_set_max_concurrent(Path(args[1]), args[2])
    elif cmd == "restore" and len(args) == 2:
        cmd_restore(Path(args[1]), preserve=True)
    elif cmd == "abort" and len(args) == 2:
        cmd_abort(Path(args[1]))
    else:
        print(__doc__)
        raise SystemExit(1)


if __name__ == "__main__":
    main()
