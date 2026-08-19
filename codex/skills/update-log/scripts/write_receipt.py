#!/usr/bin/env python3
"""Write one atomic Agent closeout receipt after validating evidence receipts."""

from __future__ import annotations

import argparse
import json
import os
import tempfile
from pathlib import Path


ACCEPTED_STATUSES = frozenset({"success", "no-op", "failure"})
SUCCESS_STATUSES = frozenset({"success", "no-op"})


def load_evidence(path: Path) -> dict[str, object]:
    """Return one valid successful JSON evidence receipt from PATH."""
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"cannot read evidence receipt {path}: {error}") from error
    if not isinstance(payload, dict):
        raise ValueError(f"evidence receipt {path} is not a JSON object")
    status = payload.get("status")
    if status not in SUCCESS_STATUSES:
        raise ValueError(f"evidence receipt {path} has non-success status {status!r}")
    if payload.get("ok") is False:
        raise ValueError(f"evidence receipt {path} reports ok=false")
    return payload


def write_atomic(path: Path, payload: dict[str, object]) -> None:
    """Atomically write PAYLOAD to PATH with owner-only permissions."""
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}.", suffix=".tmp", dir=path.parent
    )
    temporary_path = Path(temporary_name)
    try:
        os.fchmod(descriptor, 0o600)
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(payload, stream, indent=2, sort_keys=True)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary_path, path)
    finally:
        if temporary_path.exists():
            temporary_path.unlink()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--receipt-file", required=True, type=Path)
    parser.add_argument("--status", required=True, choices=sorted(ACCEPTED_STATUSES))
    parser.add_argument("--message", required=True)
    parser.add_argument("--evidence", action="append", default=[], type=Path)
    args = parser.parse_args()

    evidence = []
    try:
        if args.status in SUCCESS_STATUSES:
            evidence = [load_evidence(path) for path in args.evidence]
    except ValueError as error:
        write_atomic(
            args.receipt_file,
            {"schema_version": 1, "status": "failure", "message": str(error)},
        )
        return 1

    payload: dict[str, object] = {
        "schema_version": 1,
        "status": args.status,
        "message": args.message,
    }
    if evidence:
        payload["evidence"] = evidence
    write_atomic(args.receipt_file, payload)
    return 0 if args.status in SUCCESS_STATUSES else 1


if __name__ == "__main__":
    raise SystemExit(main())
