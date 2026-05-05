#!/usr/bin/env python3
"""Rewrite Codex session metadata so a session belongs to another project."""

from __future__ import annotations

import argparse
import json
import os
import sys
import tempfile
from collections import Counter
from pathlib import Path
from typing import Any


CODEX_HOME = Path(os.environ.get("CODEX_HOME", Path.home() / ".codex")).expanduser()
SESSIONS_DIR = CODEX_HOME / "sessions"
HISTORY_FILE = CODEX_HOME / "history.jsonl"
SESSION_INDEX_FILE = CODEX_HOME / "session_index.jsonl"
SHELL_SNAPSHOTS_DIR = CODEX_HOME / "shell_snapshots"


def load_jsonl(path: Path) -> list[tuple[str, Any | None]]:
    rows: list[tuple[str, Any | None]] = []
    with path.open(encoding="utf-8") as handle:
        for line in handle:
            raw = line.rstrip("\n")
            if not raw.strip():
                rows.append((raw, None))
                continue
            try:
                rows.append((raw, json.loads(raw)))
            except json.JSONDecodeError:
                rows.append((raw, None))
    return rows


def write_jsonl(path: Path, rows: list[tuple[str, Any | None]]) -> None:
    stat = path.stat()
    fd, tmp_name = tempfile.mkstemp(
        prefix=".rewrite-", suffix=".jsonl", dir=str(path.parent), text=True
    )
    tmp = Path(tmp_name)
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as handle:
            for raw, obj in rows:
                if obj is None:
                    handle.write(raw + "\n")
                else:
                    handle.write(
                        json.dumps(obj, ensure_ascii=False, separators=(",", ":"))
                        + "\n"
                    )
        os.chmod(tmp, stat.st_mode)
        os.replace(tmp, path)
    except Exception:
        try:
            tmp.unlink()
        except FileNotFoundError:
            pass
        raise


def iter_session_files() -> list[Path]:
    if not SESSIONS_DIR.exists():
        raise SystemExit(f"Missing Codex sessions directory: {SESSIONS_DIR}")
    return sorted(SESSIONS_DIR.rglob("*.jsonl"), key=lambda p: p.stat().st_mtime, reverse=True)


def session_meta(path: Path) -> dict[str, Any]:
    for _raw, obj in load_jsonl(path):
        if not isinstance(obj, dict):
            continue
        if obj.get("type") == "session_meta" and isinstance(obj.get("payload"), dict):
            return obj["payload"]
    return {}


def find_session(session_id: str) -> Path:
    filename_matches = [p for p in iter_session_files() if session_id in p.name]
    if len(filename_matches) == 1:
        return filename_matches[0]
    if len(filename_matches) > 1:
        joined = "\n".join(str(p) for p in filename_matches)
        raise SystemExit(f"Multiple filename matches for {session_id}:\n{joined}")

    content_matches = []
    for path in iter_session_files():
        meta = session_meta(path)
        if meta.get("id") == session_id:
            content_matches.append(path)

    if len(content_matches) == 1:
        return content_matches[0]
    if len(content_matches) > 1:
        joined = "\n".join(str(p) for p in content_matches)
        raise SystemExit(f"Multiple metadata matches for {session_id}:\n{joined}")
    raise SystemExit(f"No Codex session found for {session_id} under {SESSIONS_DIR}")


def rewrite_named_paths(obj: Any, old: str, new: str) -> int:
    count = 0
    if isinstance(obj, dict):
        for key, value in obj.items():
            if key in {"cwd", "project"} and value == old:
                obj[key] = new
                count += 1
            else:
                count += rewrite_named_paths(value, old, new)
    elif isinstance(obj, list):
        for item in obj:
            count += rewrite_named_paths(item, old, new)
    return count


def rewrite_all_cwd(obj: Any, new: str) -> tuple[int, Counter[str]]:
    count = 0
    old_values: Counter[str] = Counter()
    if isinstance(obj, dict):
        for key, value in obj.items():
            if key == "cwd" and isinstance(value, str):
                if value != new:
                    old_values[value] += 1
                    obj[key] = new
                    count += 1
            else:
                sub_count, sub_old_values = rewrite_all_cwd(value, new)
                count += sub_count
                old_values.update(sub_old_values)
    elif isinstance(obj, list):
        for item in obj:
            sub_count, sub_old_values = rewrite_all_cwd(item, new)
            count += sub_count
            old_values.update(sub_old_values)
    return count, old_values


def rewrite_session_to_project(path: Path, project: str) -> tuple[int, Counter[str]]:
    rows = load_jsonl(path)
    total = 0
    old_values: Counter[str] = Counter()
    for _raw, obj in rows:
        n, values = rewrite_all_cwd(obj, project)
        total += n
        old_values.update(values)
    if total:
        write_jsonl(path, rows)
    return total, old_values


def rewrite_jsonl_for_session(path: Path, session_id: str, project: str) -> int:
    if not path.exists():
        return 0
    rows = load_jsonl(path)
    total = 0
    for _raw, obj in rows:
        if not isinstance(obj, dict):
            continue
        if obj.get("session_id") == session_id or obj.get("sessionId") == session_id:
            for key in ("cwd", "project"):
                if key in obj and obj[key] != project:
                    obj[key] = project
                    total += 1
    if total:
        write_jsonl(path, rows)
    return total


def rewrite_jsonl_rename(path: Path, old: str, new: str) -> int:
    if not path.exists():
        return 0
    rows = load_jsonl(path)
    total = 0
    for _raw, obj in rows:
        total += rewrite_named_paths(obj, old, new)
    if total:
        write_jsonl(path, rows)
    return total


def list_recent(current_project: str) -> None:
    shown = 0
    for path in iter_session_files():
        meta = session_meta(path)
        cwd = meta.get("cwd")
        if cwd == current_project:
            continue
        session_id = meta.get("id", "<missing-id>")
        timestamp = meta.get("timestamp", "<missing-timestamp>")
        print(f"{session_id}\t{timestamp}\t{cwd}\t{path}")
        shown += 1
        if shown >= 10:
            break
    if not shown:
        print("No recent sessions outside the current project found.")


def shell_snapshots(session_id: str) -> list[Path]:
    if not SHELL_SNAPSHOTS_DIR.exists():
        return []
    return sorted(SHELL_SNAPSHOTS_DIR.glob(f"{session_id}*.sh"))


def single_session(session_id: str | None, project: str) -> int:
    if not session_id:
        list_recent(project)
        return 0

    session_file = find_session(session_id)
    before = session_meta(session_file).get("cwd")
    session_rewrites, old_values = rewrite_session_to_project(session_file, project)
    history_rewrites = rewrite_jsonl_for_session(HISTORY_FILE, session_id, project)
    index_rewrites = rewrite_jsonl_for_session(SESSION_INDEX_FILE, session_id, project)
    snapshots = shell_snapshots(session_id)

    print(f"session: {session_file}")
    print(f"previous session_meta cwd: {before}")
    print(f"session cwd fields rewritten: {session_rewrites}")
    print(f"previous cwd values: {dict(old_values)}")
    print(f"history path fields rewritten: {history_rewrites}")
    print(f"session_index path fields rewritten: {index_rewrites}")
    print(f"shell snapshots found: {len(snapshots)}")
    for snapshot in snapshots:
        print(f"shell snapshot: {snapshot}")
    return 0


def rename_project(old: str, new: str) -> int:
    if not Path(old).is_absolute() or not Path(new).is_absolute():
        raise SystemExit("--rename paths must be absolute")

    session_files = iter_session_files()
    rewritten_files = 0
    session_rewrites = 0
    for path in session_files:
        count = rewrite_jsonl_rename(path, old, new)
        if count:
            rewritten_files += 1
            session_rewrites += count

    history_rewrites = rewrite_jsonl_rename(HISTORY_FILE, old, new)
    index_rewrites = rewrite_jsonl_rename(SESSION_INDEX_FILE, old, new)

    print(f"session files scanned: {len(session_files)}")
    print(f"session files rewritten: {rewritten_files}")
    print(f"session path fields rewritten: {session_rewrites}")
    print(f"history path fields rewritten: {history_rewrites}")
    print(f"session_index path fields rewritten: {index_rewrites}")
    return 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("session_id", nargs="?", help="Codex session UUID to import")
    parser.add_argument(
        "--project",
        default=os.getcwd(),
        help="target project path for single-session mode; defaults to PWD",
    )
    parser.add_argument(
        "--rename",
        nargs=2,
        metavar=("OLD_PROJECT_PATH", "NEW_PROJECT_PATH"),
        help="rewrite an old project path to a new project path across all Codex logs",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.rename:
        return rename_project(*args.rename)
    return single_session(args.session_id, str(Path(args.project).resolve()))


if __name__ == "__main__":
    sys.exit(main())
