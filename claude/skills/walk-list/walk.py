#!/usr/bin/env python3
"""walk-list: strict one-at-a-time item processor.

On `start`, the input file is MOVED to ~/.claude/walk-list-data/<uuid>/source.json
and replaced at the original path with a stub. A PreToolUse hook blocks
all tool access to ~/.claude/walk-list-data/ except for walk.py itself,
making it structurally impossible for the caller to see multiple items.

Usage:
    walk.py start <input-file>
    walk.py next <input-file> <decision>
    walk.py status <input-file>
    walk.py show-decisions <input-file>
    walk.py restore <input-file>     # end the walk, restore original file, keep decisions
    walk.py abort <input-file>       # end the walk, restore original file, discard decisions
"""
import hashlib
import json
import os
import shutil
import sys
import uuid
from pathlib import Path


DATA_ROOT = Path.home() / ".claude" / "walk-list-data"
REGISTRY_PATH = DATA_ROOT / "registry.json"
STUB_MARKER = "_walk_locked"


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
    """Registry key — absolute path. Lookup by both resolved and as-given."""
    return str(input_file.resolve(strict=False))


def resolve_session(input_file: Path) -> tuple[str, Path]:
    """Return (session_uuid, session_dir) for a locked input file.
    Raises SystemExit if not locked."""
    reg = load_registry()
    key = canonical_key(input_file)
    sid = reg.get(key)
    if not sid:
        # try by basename fallback — helps if user passes relative path
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
    print()


def load_session(sdir: Path) -> tuple[list, dict]:
    source_path = sdir / "source.json"
    state_path = sdir / "state.json"
    items = parse_items(source_path.read_text(encoding="utf-8"))
    state = json.loads(state_path.read_text(encoding="utf-8"))
    return items, state


def save_state(sdir: Path, state: dict) -> None:
    (sdir / "state.json").write_text(
        json.dumps(state, indent=2, ensure_ascii=False), encoding="utf-8"
    )


def cmd_start(input_file: Path) -> None:
    if is_stub(input_file):
        # already locked — resume
        sid, sdir = resolve_session(input_file)
        items, state = load_session(sdir)
        cursor = state["cursor"]
        if cursor >= len(items):
            print(f"WALK COMPLETE. {len(items)} items processed. Run: walk.py restore {input_file}")
            return
        print(f"RESUMING walk (session {sid[:8]}).")
        print_item(cursor, len(items), items[cursor])
        print_footer(input_file, len(items) - cursor)
        return

    if not input_file.exists():
        raise SystemExit(f"ERROR: input file not found: {input_file}")

    # Parse to validate + get count
    raw = input_file.read_text(encoding="utf-8")
    items = parse_items(raw)
    if not items:
        raise SystemExit(f"ERROR: no items found in {input_file}")

    ensure_root()
    sid = uuid.uuid4().hex
    sdir = DATA_ROOT / sid
    sdir.mkdir(parents=True, exist_ok=False)

    # Move original into protected dir
    source_path = sdir / "source.json"
    shutil.move(str(input_file), str(source_path))

    # Initialize state
    save_state(sdir, {"cursor": 0, "decisions": [], "original_path": str(input_file)})

    # Register
    reg = load_registry()
    reg[canonical_key(input_file)] = sid
    save_registry(reg)

    # Leave a stub at the original path
    stub = {
        STUB_MARKER: True,
        "message": "This file has been moved into the walk-list protected store and is not directly readable. Use `walk.py next <file> <decision>` to advance.",
        "commands": {
            "next": f"python ~/.claude/skills/walk-list/walk.py next {input_file} '<decision-text>'",
            "status": f"python ~/.claude/skills/walk-list/walk.py status {input_file}",
            "restore": f"python ~/.claude/skills/walk-list/walk.py restore {input_file}",
        },
    }
    input_file.write_text(json.dumps(stub, indent=2), encoding="utf-8")

    print(f"STARTED walk (session {sid[:8]}). {len(items)} items locked.")
    print_item(0, len(items), items[0])
    print_footer(input_file, len(items))


def cmd_next(input_file: Path, decision: str) -> None:
    if not decision.strip():
        raise SystemExit("ERROR: decision text required. Cannot advance empty.")
    sid, sdir = resolve_session(input_file)
    items, state = load_session(sdir)
    cursor = state["cursor"]
    if cursor >= len(items):
        print(f"Already complete. Run: walk.py restore {input_file}")
        return
    state["decisions"].append(
        {"index": cursor, "item": items[cursor], "decision": decision.strip()}
    )
    state["cursor"] = cursor + 1
    save_state(sdir, state)
    print(f"Recorded decision for item {cursor + 1}.")
    print()
    new_cursor = state["cursor"]
    if new_cursor >= len(items):
        print(f"WALK COMPLETE. {len(items)} items processed.")
        print(f"Run: walk.py restore {input_file}  (to unlock the original file).")
        return
    print_item(new_cursor, len(items), items[new_cursor])
    print_footer(input_file, len(items) - new_cursor)


def cmd_status(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    items, state = load_session(sdir)
    cursor = state["cursor"]
    print(f"Session: {sid[:8]}")
    print(f"Input: {input_file}")
    print(f"Total: {len(items)}  Processed: {cursor}  Remaining: {len(items) - cursor}")
    if state["decisions"]:
        last = state["decisions"][-1]
        print(f"Last decision (item {last['index'] + 1}): {last['decision']}")


def cmd_show_decisions(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    _, state = load_session(sdir)
    if not state["decisions"]:
        print("No decisions recorded.")
        return
    for d in state["decisions"]:
        print(f"[{d['index'] + 1}] {d['decision']}")


def cmd_restore(input_file: Path, preserve: bool = True) -> None:
    sid, sdir = resolve_session(input_file)
    source_path = sdir / "source.json"
    state_path = sdir / "state.json"
    # Save decisions to a sibling of the input file so they survive teardown
    if preserve and state_path.exists():
        out = input_file.with_suffix(input_file.suffix + ".walk-decisions.json")
        shutil.copy(str(state_path), str(out))
        print(f"Decisions preserved at: {out}")
    # Restore the original file
    if input_file.exists():
        input_file.unlink()
    shutil.move(str(source_path), str(input_file))
    # Remove registry + session dir
    reg = load_registry()
    reg.pop(canonical_key(input_file), None)
    save_registry(reg)
    shutil.rmtree(sdir)
    print(f"Restored {input_file}.")


def cmd_abort(input_file: Path) -> None:
    cmd_restore(input_file, preserve=False)


def print_footer(input_file: Path, remaining: int) -> None:
    print(f"Remaining: {remaining}")
    print(f"When done with this item: walk.py next {input_file} '<decision>'")


def main() -> None:
    args = sys.argv[1:]
    if not args:
        print(__doc__)
        raise SystemExit(1)
    cmd = args[0]
    if cmd == "start" and len(args) == 2:
        cmd_start(Path(args[1]))
    elif cmd == "next" and len(args) >= 3:
        cmd_next(Path(args[1]), " ".join(args[2:]))
    elif cmd == "status" and len(args) == 2:
        cmd_status(Path(args[1]))
    elif cmd == "show-decisions" and len(args) == 2:
        cmd_show_decisions(Path(args[1]))
    elif cmd == "restore" and len(args) == 2:
        cmd_restore(Path(args[1]), preserve=True)
    elif cmd == "abort" and len(args) == 2:
        cmd_abort(Path(args[1]))
    else:
        print(__doc__)
        raise SystemExit(1)


if __name__ == "__main__":
    main()
