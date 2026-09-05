#!/usr/bin/env python3
"""Write one atomic Agent closeout receipt after validating evidence receipts."""

from __future__ import annotations

import argparse
import json
import math
import os
import stat
import sys
import tempfile
from pathlib import Path


ACCEPTED_STATUSES = frozenset({"success", "no-op", "failure"})
SUCCESS_STATUSES = frozenset({"success", "no-op"})
MAX_EVIDENCE_BYTES = 4 * 1024 * 1024
MAX_EVIDENCE_FILES = 16
MAX_RECEIPT_BYTES = 16 * 1024 * 1024
MAX_MESSAGE_CHARS = 16_384


def _object(pairs: list[tuple[str, object]]) -> dict[str, object]:
    result: dict[str, object] = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("evidence contains duplicate JSON keys")
        result[key] = value
    return result


def _finite_number(value: str) -> float:
    result = float(value)
    if not math.isfinite(result):
        raise ValueError("evidence contains a nonfinite JSON number")
    return result


def _signature(value: os.stat_result) -> tuple[int, ...]:
    return (value.st_dev, value.st_ino, value.st_mode, value.st_size,
            value.st_mtime_ns, value.st_ctime_ns)


def load_evidence(path: Path) -> dict[str, object]:
    """Return one valid successful JSON evidence receipt from PATH."""
    try:
        descriptor = os.open(path, os.O_RDONLY | os.O_NONBLOCK | os.O_NOFOLLOW)
        with os.fdopen(descriptor, "rb") as stream:
            before = os.fstat(stream.fileno())
            if not stat.S_ISREG(before.st_mode):
                raise ValueError("evidence must be a regular file")
            if before.st_size > MAX_EVIDENCE_BYTES:
                raise ValueError("evidence exceeds the 4 MiB limit")
            raw = stream.read(MAX_EVIDENCE_BYTES + 1)
            after = os.fstat(stream.fileno())
        if len(raw) > MAX_EVIDENCE_BYTES:
            raise ValueError("evidence exceeds the 4 MiB limit")
        if _signature(before) != _signature(after) or _signature(after) != _signature(path.lstat()):
            raise ValueError("evidence changed during reading")
        payload = json.loads(raw.decode("utf-8"), object_pairs_hook=_object,
                             parse_constant=_finite_number, parse_float=_finite_number)
    except (OSError, UnicodeError, json.JSONDecodeError, RecursionError) as error:
        raise ValueError("evidence is unreadable or malformed JSON") from error
    if not isinstance(payload, dict):
        raise ValueError("evidence must be a JSON object")
    status = payload.get("status")
    if not isinstance(status, str) or status not in SUCCESS_STATUSES:
        raise ValueError("evidence has a non-success status")
    if "ok" in payload and payload["ok"] is not True:
        raise ValueError("evidence ok must be the boolean true when present")
    if "schema_version" in payload and (
        type(payload["schema_version"]) is not int or payload["schema_version"] != 1
    ):
        raise ValueError("evidence schema_version must be integer 1 when present")
    return payload


def _make_parents(parent: Path) -> None:
    """Create only missing directories privately; leave existing modes alone."""
    missing = []
    candidate = parent
    while not candidate.exists():
        missing.append(candidate)
        candidate = candidate.parent
    for directory in reversed(missing):
        try:
            directory.mkdir(mode=0o700)
        except FileExistsError:
            if not directory.is_dir():
                raise


def write_atomic(path: Path, payload: dict[str, object]) -> None:
    """Publish complete private bytes once; never replace an existing target."""
    path = path.parent.resolve() / path.name
    _make_parents(path.parent)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}.", suffix=".tmp", dir=path.parent
    )
    temporary_path = Path(temporary_name)
    try:
        os.fchmod(descriptor, 0o600)
        with os.fdopen(descriptor, "wb") as stream:
            descriptor = -1  # The stream owns descriptor cleanup from here.
            size = 0
            for chunk in json.JSONEncoder(indent=2, sort_keys=True, allow_nan=False).iterencode(payload):
                encoded = chunk.encode("ascii")
                size += len(encoded)
                if size + 1 > MAX_RECEIPT_BYTES:
                    raise ValueError("receipt exceeds the 16 MiB limit")
                stream.write(encoded)
            stream.write(b"\n")
            stream.flush()
            os.fsync(stream.fileno())
        # Same-directory hard-link publication is atomic and fails for any
        # existing leaf, including a symlink or a concurrent writer's receipt.
        os.link(temporary_path, path)
    finally:
        if descriptor != -1:
            os.close(descriptor)
        if temporary_path.exists():
            temporary_path.unlink()
    # A failure here leaves the already-published receipt intact. The caller
    # reports publication uncertainty instead of replacing it with a new status.
    directory_descriptor = os.open(path.parent, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(directory_descriptor)
    finally:
        os.close(directory_descriptor)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--receipt-file", required=True, type=Path)
    parser.add_argument("--status", required=True, choices=sorted(ACCEPTED_STATUSES))
    parser.add_argument("--message", required=True)
    parser.add_argument("--evidence", action="append", default=[], type=Path)
    args = parser.parse_args(argv)

    evidence = []
    status, message = args.status, args.message
    try:
        if not message.strip() or len(message) > MAX_MESSAGE_CHARS:
            raise ValueError("message must be nonblank and at most 16384 characters")
        if len(args.evidence) > MAX_EVIDENCE_FILES:
            raise ValueError("at most 16 evidence files are supported")
        if args.status in SUCCESS_STATUSES:
            evidence = [load_evidence(path) for path in args.evidence]
    except ValueError as error:
        status, message = "failure", str(error)
        print(f"error: {message}", file=sys.stderr)

    payload: dict[str, object] = {
        "schema_version": 1,
        "status": status,
        "message": message,
    }
    if evidence:
        payload["evidence"] = evidence
    try:
        write_atomic(args.receipt_file, payload)
    except FileExistsError:
        print("error: receipt target already exists; preserved", file=sys.stderr)
        return 2
    except (OSError, ValueError, TypeError, RecursionError):
        print("error: receipt publication failed; inspect target before retrying", file=sys.stderr)
        return 2
    return 0 if status in SUCCESS_STATUSES else 1


if __name__ == "__main__":
    raise SystemExit(main())
