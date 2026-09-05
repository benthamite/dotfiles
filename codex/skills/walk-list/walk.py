#!/usr/bin/env python3
"""walk-list: strict one-at-a-time OR N-at-a-time item processor.

On `start`, the input file is MOVED to ~/.claude/walk-list-data/<uuid>/source.json
and replaced at the original path with a stub. Where installed, the hook rejects
recognized direct store access; it does not provide a universal access guarantee.

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
import functools
import io
import json
import math
import os
import shlex
import shutil
import sys
import tempfile
import uuid
from datetime import datetime, timezone
from pathlib import Path


DATA_ROOT = Path.home() / ".claude" / "walk-list-data"
REGISTRY_PATH = DATA_ROOT / "registry.json"
OUTPUT_ROOT = DATA_ROOT.parent / "walk-list-out"
STUB_MARKER = "_walk_locked"
DEFAULT_MAX = 1


def parse_positive_int(raw: str, label: str) -> int:
    try:
        value = int(raw)
    except ValueError as exc:
        raise SystemExit(f"ERROR: {label} must be a positive integer: {raw}") from exc
    if value < 1:
        raise SystemExit(f"ERROR: {label} must be a positive integer: {raw}")
    return value


def parse_non_negative_float(raw: str, label: str) -> float:
    try:
        value = float(raw)
    except ValueError as exc:
        raise SystemExit(f"ERROR: {label} must be a non-negative number: {raw}") from exc
    if not math.isfinite(value) or value < 0:
        raise SystemExit(f"ERROR: {label} must be a non-negative number: {raw}")
    return value


def walk_command(*args: str) -> str:
    script = Path(__file__).expanduser()
    if not script.is_absolute():
        script = script.absolute()
    return " ".join(shlex.quote(part) for part in ["python", str(script), *args])


def ensure_root() -> None:
    DATA_ROOT.mkdir(parents=True, exist_ok=True)


def load_registry() -> dict:
    if not REGISTRY_PATH.exists():
        return {}
    return json.loads(REGISTRY_PATH.read_text(encoding="utf-8"))


def save_registry(reg: dict) -> None:
    ensure_root()
    atomic_json(REGISTRY_PATH, reg)


def stage_bytes(path: Path, data: bytes) -> Path:
    """Prepare a complete sibling file; never truncate an existing destination."""
    fd, name = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    staged = Path(name)
    try:
        with os.fdopen(fd, "wb") as stream:
            stream.write(data)
            stream.flush()
            os.fsync(stream.fileno())
    except BaseException:
        staged.unlink(missing_ok=True)
        raise
    return staged


def atomic_json(path: Path, data: dict) -> None:
    encoded = json.dumps(data, indent=2, ensure_ascii=False).encode("utf-8")
    staged = stage_bytes(path, encoded)
    try:
        os.replace(staged, path)
    finally:
        staged.unlink(missing_ok=True)


def serialized_command(function):
    """Keep identity resolution and all lifecycle mutations in one lock domain."""
    @functools.wraps(function)
    def serialized(*args, **kwargs):
        ensure_root()
        with (DATA_ROOT / "registry.lock").open("a") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            try:
                output = io.StringIO()
                with contextlib.redirect_stdout(output):
                    result = function(*args, **kwargs)
                print(output.getvalue(), end="")
                return result
            finally:
                fcntl.flock(lock, fcntl.LOCK_UN)
    return serialized


def canonical_key(input_file: Path) -> str:
    return str(input_file.resolve(strict=False))


def resolve_session(input_file: Path) -> tuple[str, Path]:
    reg = load_registry()
    key = canonical_key(input_file)
    sid = reg.get(key)
    if not sid:
        raise SystemExit(
            f"ERROR: no active walk for {input_file}. Run: walk.py start {input_file}"
        )
    sdir = DATA_ROOT / sid
    if not sdir.exists():
        raise SystemExit(f"ERROR: registry points to missing session dir {sdir}")
    require_owned_stub(input_file, sid)
    return sid, sdir


@contextlib.contextmanager
def locked_state(sdir: Path):
    """Read-modify-write the state file under an exclusive flock so concurrent
    walk.py invocations (e.g. subagents calling `record` simultaneously) don't
    clobber each other."""
    state_path = sdir / "state.json"
    with open(sdir / "state.lock", "a", encoding="utf-8") as f:
        fcntl.flock(f.fileno(), fcntl.LOCK_EX)
        try:
            original = state_path.read_text(encoding="utf-8")
            data = json.loads(original)
            data.setdefault("in_flight", {})
            data.setdefault("to_redispatch", [])
            data.setdefault("max_concurrent", 1)
            yield data
            if data != json.loads(original):
                atomic_json(state_path, data)
        finally:
            fcntl.flock(f.fileno(), fcntl.LOCK_UN)


def parse_items(raw: str) -> list:
    raw = raw.strip()
    if not raw:
        return []
    if raw.startswith("["):
        try:
            data = json.loads(raw)
        except json.JSONDecodeError as exc:
            # Multiple JSONL arrays are not one top-level JSON document.
            lines = [line.strip() for line in raw.splitlines() if line.strip()]
            try:
                records = [json.loads(line) for line in lines]
            except json.JSONDecodeError:
                raise SystemExit("ERROR: malformed JSON array or JSONL input") from exc
            if len(records) > 1:
                return records
            raise SystemExit("ERROR: malformed JSON array input") from exc
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


def require_owned_stub(path: Path, sid: str) -> None:
    if path.is_symlink() or not is_stub(path):
        raise SystemExit(f"ERROR: input is not this walk's owned stub: {path}")
    stub = json.loads(path.read_text(encoding="utf-8"))
    matches = (stub["session_id"] == sid if "session_id" in stub
               else stub.get("session_prefix") == sid[:8])
    if not matches:
        raise SystemExit(f"ERROR: input stub belongs to another session: {path}")


def pending_index(state: dict, total: int):
    if state["to_redispatch"]:
        return state["to_redispatch"][0]
    return state["cursor"] if state["cursor"] < total else None


def file_identity(path: Path) -> tuple:
    stat = path.lstat()
    return stat.st_dev, stat.st_ino, stat.st_size, stat.st_mtime_ns


def print_item(index: int, total: int, item) -> None:
    print(f"=== ITEM {index + 1} OF {total} ===")
    if isinstance(item, (dict, list)):
        print(json.dumps(item, indent=2, ensure_ascii=False))
    else:
        print(item)


@serialized_command
def cmd_start(input_file: Path, max_concurrent: int) -> None:
    if input_file.is_symlink():
        raise SystemExit("ERROR: symlink inputs are not supported; use the real file path.")
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
            index = pending_index(state, len(items))
            if done >= len(items) and not in_flight and index is None:
                print(f"WALK COMPLETE. Run: walk.py restore {input_file}")
                return
            if max_c == 1 and not in_flight and index is not None:
                print()
                print_item(index, len(items), items[index])
                state["sequential_index"] = index
                print()
                print(f"When done: walk.py next {input_file} '<decision>'")
            else:
                print(f"Use: walk.py dispatch {input_file}  (to claim items)")
        return

    if not input_file.exists():
        raise SystemExit(f"ERROR: input file not found: {input_file}")
    key = canonical_key(input_file)
    reg = load_registry()
    if key in reg:
        raise SystemExit("ERROR: active walk exists but its input stub was replaced.")
    max_concurrent = parse_positive_int(str(max_concurrent), "max_concurrent")
    original_identity = file_identity(input_file)
    original_bytes = input_file.read_bytes()
    items = parse_items(original_bytes.decode("utf-8"))
    if not items:
        raise SystemExit(f"ERROR: no items found in {input_file}")

    ensure_root()
    sid = uuid.uuid4().hex
    sdir = DATA_ROOT / sid
    sdir.mkdir(parents=True, exist_ok=False)
    source_path = sdir / "source.json"
    # Preserve the original until source, state and stub are all staged.
    with source_path.open("xb") as source:
        source.write(original_bytes)
        source.flush()
        os.fsync(source.fileno())
    shutil.copystat(input_file, source_path)
    state = {
        "cursor": 0,
        "in_flight": {},
        "to_redispatch": [],
        "decisions": [],
        "max_concurrent": max_concurrent,
        "original_path": key,
        "sequential_index": 0 if max_concurrent == 1 else None,
    }
    atomic_json(sdir / "state.json", state)

    input_arg = str(input_file)
    stub = {
        STUB_MARKER: True,
        "message": "Locked by walk-list. Use walk.py commands to interact.",
        "session_prefix": sid[:8],
        "session_id": sid,
        "max_concurrent": max_concurrent,
        "commands": {
            "dispatch (pool)": walk_command("dispatch", input_arg),
            "record (pool)": walk_command("record", input_arg, "<token>", "<decision>"),
            "pool-status": walk_command("pool-status", input_arg),
            "next (sequential)": walk_command("next", input_arg, "<decision>"),
            "status": walk_command("status", input_arg),
            "restore": walk_command("restore", input_arg),
        },
    }
    staged_stub = stage_bytes(input_file, json.dumps(stub, indent=2).encode("utf-8"))
    try:
        if input_file.is_symlink() or file_identity(input_file) != original_identity:
            raise SystemExit("ERROR: input changed during start; original copy retained.")
        new_registry = dict(reg, **{key: sid})
        save_registry(new_registry)
        try:
            if input_file.is_symlink() or file_identity(input_file) != original_identity:
                raise SystemExit("ERROR: input changed during start; original copy retained.")
            os.replace(staged_stub, input_file)
        except BaseException:
            save_registry(reg)
            raise
    finally:
        staged_stub.unlink(missing_ok=True)

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


@serialized_command
def cmd_next(input_file: Path, decision: str) -> None:
    if not decision.strip():
        raise SystemExit("ERROR: decision text required.")
    sid, sdir = resolve_session(input_file)
    items = load_source_items(sdir)
    with locked_state(sdir) as state:
        if state["max_concurrent"] != 1:
            raise SystemExit("ERROR: next requires sequential mode; use dispatch/record.")
        if state["in_flight"]:
            raise SystemExit(
                "ERROR: in-flight claims exist. Use `walk.py record <token> <decision>` "
                "for pool-mode advance, or `walk.py release-stale` to reclaim stuck claims."
            )
        cursor = state["cursor"]
        if cursor >= len(items) and not state["to_redispatch"]:
            print(f"Already complete. Run: walk.py restore {input_file}")
            return
        index = pending_index(state, len(items))
        if "sequential_index" not in state:
            # Legacy sequential walks already exposed their cursor. Pool history
            # or redispatch requires an explicit resume before a sequential verdict.
            stub = json.loads(input_file.read_text(encoding="utf-8"))
            legacy_sequential = (not state["to_redispatch"]
                                 and stub.get("max_concurrent", 1) == 1
                                 and not any("claim_token" in d for d in state["decisions"]))
            state["sequential_index"] = index if legacy_sequential else None
        if state["sequential_index"] != index:
            raise SystemExit("ERROR: item not shown sequentially; run start to resume first.")
        # In sequential mode, consume either a redispatch-queue entry or cursor
        if state["to_redispatch"]:
            index = state["to_redispatch"].pop(0)
        else:
            index = cursor
            state["cursor"] = cursor + 1
        state["decisions"].append(
            {"index": index, "item": items[index], "decision": decision.strip(),
             "recorded_at": datetime.now(timezone.utc).isoformat()}
        )
        state["sequential_index"] = pending_index(state, len(items))
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


@serialized_command
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
        state["sequential_index"] = None
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


@serialized_command
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


@serialized_command
def cmd_pool_status(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    items = load_source_items(sdir)
    with locked_state(sdir) as state:
        total = len(items)
        in_flight = len(state["in_flight"])
        done = len(state["decisions"])
        cursor = state["cursor"]
        max_c = state["max_concurrent"]
        available = max(0, max_c - in_flight)
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


@serialized_command
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
            last = state["decisions"][-1]
            print(f"Last decision (item {last['index'] + 1}): {last['decision']}")


@serialized_command
def cmd_show_decisions(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    with locked_state(sdir) as state:
        if not state["decisions"]:
            print("No decisions recorded.")
            return
        for d in sorted(state["decisions"], key=lambda x: x["index"]):
            print(f"[{d['index'] + 1}] {d['decision']}")


@serialized_command
def cmd_release_stale(input_file: Path, max_age_seconds: str) -> None:
    sid, sdir = resolve_session(input_file)
    threshold = parse_non_negative_float(max_age_seconds, "age-seconds")
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
            state["sequential_index"] = None
    if released:
        for idx, tok, age in released:
            print(
                f"Released claim on item {idx + 1} (token {tok[:8]}, age {age:.0f}s). "
                "Re-queued for dispatch."
            )
    else:
        print("No stale claims.")


@serialized_command
def cmd_set_max_concurrent(input_file: Path, n: str) -> None:
    sid, sdir = resolve_session(input_file)
    max_concurrent = parse_positive_int(n, "max_concurrent")
    with locked_state(sdir) as state:
        if max_concurrent < len(state["in_flight"]):
            raise SystemExit("ERROR: cap cannot be lower than the number of in-flight claims.")
        if max_concurrent != state["max_concurrent"]:
            state["sequential_index"] = None
        state["max_concurrent"] = max_concurrent
    print(f"max_concurrent set to {max_concurrent}")


@serialized_command
def cmd_restore(input_file: Path, preserve: bool = True) -> None:
    sid, sdir = resolve_session(input_file)
    source_path = sdir / "source.json"
    with locked_state(sdir) as state:
        if state["in_flight"]:
            raise SystemExit("ERROR: in-flight claims exist; stop workers and record or release claims before closing.")
        total = len(load_source_items(sdir))
        if preserve and (state["to_redispatch"]
                         or state["cursor"] != total
                         or sorted(d["index"] for d in state["decisions"]) != list(range(total))):
            raise SystemExit("ERROR: walk unfinished; complete it or explicitly abort to discard progress.")
        snapshot = dict(state, session_id=sid)
    if preserve:
        OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
        out = OUTPUT_ROOT / f"{input_file.stem}.{sid}.{uuid.uuid4().hex}.walk-decisions.json"
        # Exclusive creation preserves earlier exports, including retry attempts.
        with out.open("x", encoding="utf-8") as stream:
            try:
                json.dump(snapshot, stream, indent=2, ensure_ascii=False)
                stream.flush()
                os.fsync(stream.fileno())
            except BaseException:
                out.unlink()
                raise
        print(f"Decisions preserved at: {out}")

    require_owned_stub(input_file, sid)
    original_stub = input_file.read_bytes()
    staged_source = stage_bytes(input_file, source_path.read_bytes())
    staged_stub = None
    try:
        shutil.copystat(source_path, staged_source)
        staged_stub = stage_bytes(input_file, original_stub)
        require_owned_stub(input_file, sid)
        restored_identity = file_identity(staged_source)
        os.replace(staged_source, input_file)
        try:
            reg = load_registry()
            reg.pop(canonical_key(input_file), None)
            save_registry(reg)
        except BaseException:
            # Never roll back over somebody else's replacement.
            if not input_file.is_symlink() and file_identity(input_file) == restored_identity:
                os.replace(staged_stub, input_file)
            raise
    finally:
        staged_source.unlink(missing_ok=True)
        if staged_stub is not None:
            staged_stub.unlink(missing_ok=True)

    # The source and completed evidence are safe. Never recursively erase
    # unexpected files that may have been added to a session directory.
    for name in ("source.json", "state.json", "state.lock"):
        (sdir / name).unlink(missing_ok=True)
    if any(sdir.iterdir()):
        print(f"Retained unexpected session files at: {sdir}")
    else:
        sdir.rmdir()
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
        if rest:
            if len(rest) != 2 or rest[0] != "--max-concurrent":
                raise SystemExit("Usage: walk.py start <file> [--max-concurrent N]")
            max_c = parse_positive_int(rest[1], "max_concurrent")
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
