#!/usr/bin/env python3
"""walk-list: strict one-at-a-time item processor.

Presents items from an input file one at a time. Will not advance to the
next item until a decision for the current item is recorded via `next`.

Usage:
    walk.py start <input-file>
    walk.py next <input-file> <decision>
    walk.py status <input-file>
    walk.py abort <input-file>
    walk.py show-decisions <input-file>
"""
import json
import sys
from pathlib import Path


def load_items(input_file: Path) -> list:
    """Parse the input file. Supports JSON array, JSON Lines, plain text."""
    if not input_file.exists():
        raise SystemExit(f"ERROR: input file not found: {input_file}")
    raw = input_file.read_text(encoding="utf-8").strip()
    if not raw:
        return []
    if raw.startswith("["):
        data = json.loads(raw)
        if not isinstance(data, list):
            raise SystemExit(f"ERROR: top-level JSON must be an array, got {type(data).__name__}")
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


def state_path(input_file: Path) -> Path:
    return input_file.parent / (input_file.name + ".walk-state.json")


def load_state(input_file: Path) -> dict:
    sp = state_path(input_file)
    if sp.exists():
        return json.loads(sp.read_text(encoding="utf-8"))
    return {"cursor": 0, "decisions": []}


def save_state(input_file: Path, state: dict) -> None:
    sp = state_path(input_file)
    sp.write_text(json.dumps(state, indent=2, ensure_ascii=False), encoding="utf-8")


def print_item(index: int, total: int, item) -> None:
    print(f"=== ITEM {index + 1} OF {total} ===")
    if isinstance(item, (dict, list)):
        print(json.dumps(item, indent=2, ensure_ascii=False))
    else:
        print(item)
    print()


def cmd_start(input_file: Path) -> None:
    items = load_items(input_file)
    if not items:
        print(f"No items in {input_file}.")
        return
    state = load_state(input_file)
    cursor = state["cursor"]
    if cursor >= len(items):
        print(f"WALK COMPLETE. {len(items)} items already processed.")
        print(f"State: {state_path(input_file)}")
        return
    print_item(cursor, len(items), items[cursor])
    print(f"Process this ONE item. Do NOT read {input_file} or look at other items.")
    print(f"When you have a decision, invoke: /walk-list next <decision-text>")
    print(f"State file: {state_path(input_file)}")
    print(f"Remaining: {len(items) - cursor}")


def cmd_next(input_file: Path, decision: str) -> None:
    if not decision or not decision.strip():
        raise SystemExit("ERROR: decision text is required. Cannot advance with an empty decision.")
    items = load_items(input_file)
    state = load_state(input_file)
    cursor = state["cursor"]
    if cursor >= len(items):
        print(f"Already complete. {len(items)} items processed.")
        return
    state["decisions"].append({
        "index": cursor,
        "item": items[cursor],
        "decision": decision.strip(),
    })
    state["cursor"] = cursor + 1
    save_state(input_file, state)
    print(f"Recorded decision for item {cursor + 1}.")
    print()
    new_cursor = state["cursor"]
    if new_cursor >= len(items):
        print(f"WALK COMPLETE. {len(items)} items processed.")
        print(f"Decisions saved to: {state_path(input_file)}")
        return
    print_item(new_cursor, len(items), items[new_cursor])
    print(f"Process this ONE item. Do NOT read {input_file} or look at other items.")
    print(f"When you have a decision, invoke: /walk-list next <decision-text>")
    print(f"Remaining: {len(items) - new_cursor}")


def cmd_status(input_file: Path) -> None:
    items = load_items(input_file)
    state = load_state(input_file)
    cursor = state["cursor"]
    print(f"Input: {input_file}")
    print(f"Total: {len(items)}")
    print(f"Processed: {cursor}")
    print(f"Remaining: {len(items) - cursor}")
    if state["decisions"]:
        last = state["decisions"][-1]
        print(f"Last decision (item {last['index'] + 1}): {last['decision']}")


def cmd_abort(input_file: Path) -> None:
    sp = state_path(input_file)
    if sp.exists():
        sp.unlink()
        print(f"Deleted state file: {sp}")
    else:
        print(f"No state file to delete.")


def cmd_show_decisions(input_file: Path) -> None:
    state = load_state(input_file)
    if not state["decisions"]:
        print("No decisions recorded.")
        return
    for d in state["decisions"]:
        print(f"[{d['index'] + 1}] {d['decision']}")


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
    elif cmd == "abort" and len(args) == 2:
        cmd_abort(Path(args[1]))
    elif cmd == "show-decisions" and len(args) == 2:
        cmd_show_decisions(Path(args[1]))
    else:
        print(__doc__)
        raise SystemExit(1)


if __name__ == "__main__":
    main()
