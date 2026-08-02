#!/usr/bin/env python3
"""Rewrite Claude Code session metadata so a session belongs to another project.

This is the Claude Code counterpart of the Codex adapter at
codex/skills/move-session-log/scripts/move_session_log.py and supports the
same interface:

    python3 move_session_log.py <session-id> [--project PATH] [--dry-run]
    python3 move_session_log.py --dry-run --rename OLD NEW
    python3 move_session_log.py --rename OLD NEW

The session-root layouts differ (Claude Code stores sessions in per-project
directories under ~/.claude/projects with encoded names, plus
~/.claude/history.jsonl and the ~/.claude.json projects map), but the
path-mapping semantics are byte-equivalent to the Codex adapter: a session
path field whose value is exactly OLD becomes exactly NEW; nothing else is
rewritten. tests/test_move_session_log_parity.py enforces the equivalence.

When CLAUDE_CONFIG_DIR is set, the config file lives at
$CLAUDE_CONFIG_DIR/.claude.json (matching Claude Code's own behavior);
otherwise at ~/.claude.json.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import sys
import tempfile
from pathlib import Path
from typing import Any


_ENV_CONFIG_DIR = os.environ.get("CLAUDE_CONFIG_DIR")
CONFIG_DIR = Path(_ENV_CONFIG_DIR).expanduser() if _ENV_CONFIG_DIR else Path.home() / ".claude"
PROJECTS_DIR = CONFIG_DIR / "projects"
HISTORY_FILE = CONFIG_DIR / "history.jsonl"
CLAUDE_JSON = (
    CONFIG_DIR / ".claude.json" if _ENV_CONFIG_DIR else Path.home() / ".claude.json"
)
# Same key set as the Codex adapter, so both sides rewrite the same fields.
PATH_FIELD_KEYS = {"cwd", "project", "workdir", "working_dir"}


def encode_project_path(path: str) -> str:
    """Claude Code's project-directory encoding: '/', '.', ' ' become '-'."""
    return re.sub(r"[/. ]", "-", path)


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


def rewrite_named_paths(obj: Any, old: str, new: str) -> int:
    """Rewrite every PATH_FIELD_KEYS value that equals OLD to NEW."""
    count = 0
    if isinstance(obj, dict):
        for key, value in obj.items():
            if key in PATH_FIELD_KEYS and value == old:
                obj[key] = new
                count += 1
            else:
                count += rewrite_named_paths(value, old, new)
    elif isinstance(obj, list):
        for item in obj:
            count += rewrite_named_paths(item, old, new)
    return count


def rewrite_jsonl_rename(path: Path, old: str, new: str, *, dry_run: bool = False) -> int:
    if not path.exists():
        return 0
    rows = load_jsonl(path)
    total = 0
    for _raw, obj in rows:
        total += rewrite_named_paths(obj, old, new)
    if total and not dry_run:
        write_jsonl(path, rows)
    return total


def project_jsonl_files(project_dir: Path) -> list[Path]:
    if not project_dir.is_dir():
        return []
    return sorted(project_dir.glob("*.jsonl")) + sorted(project_dir.glob("*/*.jsonl"))


def rewrite_claude_json_rename(old: str, new: str, *, dry_run: bool = False) -> str:
    if not CLAUDE_JSON.exists():
        return "no config file; skipped"
    data = json.loads(CLAUDE_JSON.read_text(encoding="utf-8"))
    projects = data.get("projects", {})
    if old in projects and new not in projects:
        if not dry_run:
            projects[new] = projects.pop(old)
            data["projects"] = projects
            CLAUDE_JSON.write_text(
                json.dumps(data, ensure_ascii=False, indent=2) + "\n",
                encoding="utf-8",
            )
        return f"migrated projects[{old!r}] -> projects[{new!r}]"
    if old in projects and new in projects:
        return "both old and new project entries exist; left as-is"
    return "no project entry for the old path; skipped"


def rename_project(old: str, new: str, *, dry_run: bool = False) -> int:
    if not Path(old).is_absolute() or not Path(new).is_absolute():
        raise SystemExit("--rename paths must be absolute")

    src = PROJECTS_DIR / encode_project_path(old)
    dst = PROJECTS_DIR / encode_project_path(new)
    if src.is_dir() and dst.is_dir():
        raise SystemExit(
            f"Both {src} and {dst} exist; refusing to merge. Investigate."
        )
    moved = "already moved or absent"
    if src.is_dir():
        if dry_run:
            moved = f"would move {src} -> {dst}"
        else:
            shutil.move(str(src), str(dst))
            moved = f"moved {src} -> {dst}"

    # In a dry run the source directory has not moved, so scan wherever the
    # session files actually are.
    scan_dir = dst if dst.is_dir() else src
    session_rewrites = 0
    rewritten_files = 0
    session_files = project_jsonl_files(scan_dir)
    for path in session_files:
        count = rewrite_jsonl_rename(path, old, new, dry_run=dry_run)
        if count:
            rewritten_files += 1
            session_rewrites += count

    history_rewrites = rewrite_jsonl_rename(HISTORY_FILE, old, new, dry_run=dry_run)
    claude_json_status = rewrite_claude_json_rename(old, new, dry_run=dry_run)

    print(f"dry run: {dry_run}")
    print(f"project dir: {moved}")
    print(f"session files scanned: {len(session_files)}")
    print(f"session files rewritten: {rewritten_files}")
    print(f"session path fields rewritten: {session_rewrites}")
    print(f"history path fields rewritten: {history_rewrites}")
    print(f"claude.json projects map: {claude_json_status}")
    return 0


def session_files_outside(current_dir: Path) -> list[Path]:
    if not PROJECTS_DIR.is_dir():
        raise SystemExit(f"Missing Claude projects directory: {PROJECTS_DIR}")
    files = [
        path
        for path in PROJECTS_DIR.glob("*/*.jsonl")
        if path.parent != current_dir
    ]
    return sorted(files, key=lambda p: p.stat().st_mtime, reverse=True)


def list_recent(current_dir: Path) -> None:
    shown = 0
    for path in session_files_outside(current_dir):
        print(f"{path.stem}\t{path.parent.name}\t{path}")
        shown += 1
        if shown >= 10:
            break
    if not shown:
        print("No recent sessions outside the current project found.")


def single_session(session_id: str | None, project: str, *, dry_run: bool = False) -> int:
    target_dir = PROJECTS_DIR / encode_project_path(project)
    if not session_id:
        list_recent(target_dir)
        return 0

    matches = [
        path
        for path in session_files_outside(target_dir)
        if path.stem == session_id
    ]
    if not matches:
        raise SystemExit(
            f"No session {session_id} found outside {target_dir} under {PROJECTS_DIR}"
        )
    if len(matches) > 1:
        joined = "\n".join(str(p) for p in matches)
        raise SystemExit(f"Multiple matches for {session_id}:\n{joined}")
    source = matches[0]
    source_subdir = source.parent / session_id

    moved: list[str] = []
    if not dry_run:
        target_dir.mkdir(parents=True, exist_ok=True)
        shutil.move(str(source), str(target_dir / source.name))
        moved.append(str(target_dir / source.name))
        if source_subdir.is_dir():
            shutil.move(str(source_subdir), str(target_dir / session_id))
            moved.append(str(target_dir / session_id))
        session_path = target_dir / source.name
    else:
        session_path = source

    # Rewrite cwd fields in the session and the history project field.
    rows = load_jsonl(session_path)
    session_rewrites = 0
    for _raw, obj in rows:
        if isinstance(obj, dict) and "cwd" in obj and obj["cwd"] != project:
            obj["cwd"] = project
            session_rewrites += 1
    if session_rewrites and not dry_run:
        write_jsonl(session_path, rows)

    history_rewrites = 0
    if HISTORY_FILE.exists():
        history_rows = load_jsonl(HISTORY_FILE)
        for _raw, obj in history_rows:
            if (
                isinstance(obj, dict)
                and obj.get("sessionId") == session_id
                and obj.get("project") != project
            ):
                obj["project"] = project
                history_rewrites += 1
        if history_rewrites and not dry_run:
            write_jsonl(HISTORY_FILE, history_rows)

    print(f"dry run: {dry_run}")
    print(f"session: {source}")
    print(f"moved: {moved if moved else 'nothing (dry run)'}")
    print(f"session cwd fields rewritten: {session_rewrites}")
    print(f"history project fields rewritten: {history_rewrites}")
    return 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("session_id", nargs="?", help="Claude session UUID to import")
    parser.add_argument(
        "--project",
        default=os.getcwd(),
        help="target project path for single-session mode; defaults to PWD",
    )
    parser.add_argument(
        "--rename",
        nargs=2,
        metavar=("OLD_PROJECT_PATH", "NEW_PROJECT_PATH"),
        help="rewrite an old project path to a new project path across all Claude logs",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="report matching rewrites without modifying Claude log files",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.rename:
        return rename_project(*args.rename, dry_run=args.dry_run)
    return single_session(
        args.session_id, str(Path(args.project).resolve()), dry_run=args.dry_run
    )


if __name__ == "__main__":
    sys.exit(main())
