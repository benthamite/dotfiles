"""Shared ledger of the unattended book-PDF queue.

Two writers share `state.json` in the queue directory: the scheduled batch in
stafforini.com (`scripts/download-missing-pdfs-batch.py`), which acquires and
stages PDFs, and the Emacs drain in `ebib-extras`, which attaches staged PDFs.
Each writer changes only its own book's record, and only under the exclusive
`state.lock` flock, re-reading the file first, so neither overwrites the
other's record. The batch's own `lock` (one batch at a time) is separate and
held for a whole run; `state.lock` is held only for one read-modify-write.

Record statuses:

- running: a worker is acquiring this book.
- staged: a reviewed PDF copy waits in the attempt directory for the drain.
- attaching: the drain claimed the attempt (`claim.json`) and may have started
  an Ebib operation (`operation.json`).
- attached: the PDF is installed and verified.
- attach-error: attachment failed or needs review; excluded from acquisition
  until Pablo clears the record.
- deferred, error: the worker's per-book outcomes.
"""

from __future__ import annotations

from contextlib import contextmanager
import fcntl
import json
import math
import os
from pathlib import Path
import tempfile
import time

STATE_FILE = "state.json"
LOCK_FILE = "state.lock"
STATUSES = frozenset({"running", "staged", "attaching", "attached",
                      "attach-error", "deferred", "error"})
# Records the acquisition batch must not select: the drain owns them.
DRAIN_OWNED = frozenset({"staged", "attaching", "attach-error"})
DEFAULT_LOCK_TIMEOUT = 30.0
ANY = object()


class StatusMismatch(Exception):
    """A conditional update found a record with another status."""


def empty_state() -> dict:
    return {"version": 1, "books": {}}


def validate(state, origin="queue state") -> dict:
    """Return STATE after checking its structure; raise ValueError otherwise."""
    if not isinstance(state, dict) or state.get("version") != 1 or not isinstance(state.get("books"), dict):
        raise ValueError(f"Invalid queue state: {origin}")
    for key, record in state["books"].items():
        if (not isinstance(record, dict)
                or not isinstance(record.get("last_attempt"), (int, float))
                or isinstance(record.get("last_attempt"), bool)
                or not math.isfinite(record["last_attempt"])
                or record.get("status") not in STATUSES):
            raise ValueError(f"Invalid queue record: {key}")
    return state


def read_state(state_dir: Path) -> dict:
    """Read the ledger in STATE_DIR; a missing file is an empty ledger."""
    path = Path(state_dir) / STATE_FILE
    if not path.exists():
        return empty_state()
    return validate(json.loads(path.read_text()), str(path))


def write_json(path: Path, value) -> None:
    """Replace one private file atomically on its own filesystem."""
    temporary = None
    try:
        with tempfile.NamedTemporaryFile("w", dir=path.parent, delete=False) as handle:
            temporary = Path(handle.name)
            json.dump(value, handle, indent=2)
            handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
        temporary.replace(path)
    finally:
        if temporary is not None and temporary.exists():
            temporary.unlink()


@contextmanager
def locked(state_dir: Path, timeout: float = DEFAULT_LOCK_TIMEOUT):
    """Hold the ledger's exclusive lock, waiting at most TIMEOUT seconds."""
    deadline = time.monotonic() + timeout
    with (Path(state_dir) / LOCK_FILE).open("a") as lock:
        while True:
            try:
                fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
                break
            except BlockingIOError:
                if time.monotonic() >= deadline:
                    raise TimeoutError(f"Queue ledger lock busy for {timeout:g}s: {state_dir}")
                time.sleep(0.05)
        try:
            yield
        finally:
            fcntl.flock(lock, fcntl.LOCK_UN)


def update_record(state_dir: Path, key: str, *, record=None, merge=None, delete=False,
                  expect_status=ANY, timeout: float = DEFAULT_LOCK_TIMEOUT) -> dict:
    """Change only KEY's record under the lock and return the new ledger.

    Exactly one of RECORD (replace), MERGE (update fields of an existing
    record) or DELETE applies. EXPECT_STATUS, when given, must equal the
    current record's status (None for an absent record), else StatusMismatch.
    """
    if sum((record is not None, merge is not None, bool(delete))) != 1:
        raise ValueError("Give exactly one of record, merge or delete")
    with locked(state_dir, timeout):
        state = read_state(state_dir)
        current = state["books"].get(key)
        if expect_status is not ANY:
            actual = current.get("status") if current else None
            if actual != expect_status:
                raise StatusMismatch(f"{key}: status is {actual}, expected {expect_status}")
        if delete:
            state["books"].pop(key, None)
        elif record is not None:
            state["books"][key] = dict(record)
        else:
            if current is None:
                raise StatusMismatch(f"{key}: no record to update")
            state["books"][key] = {**current, **merge}
        validate(state)
        write_json(Path(state_dir) / STATE_FILE, state)
        return state
