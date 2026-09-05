#!/usr/bin/env python3
"""Plan and recoverably relocate offline Claude Code session metadata.

Applying requires --offline and a new absolute --backup-dir outside Google
Drive. The offline assertion does not lock out Claude writers: the shared
safety helper checks open handles and captured inputs, and records recoverable
partial failures. Run the nonmutating --dry-run first.

Only runtime-owned top-level session cwd and matching history project values
are changed. Project settings, including trust, stay untouched unless a
separately authorized --migrate-project-settings accompanies --rename.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import stat
import sys
from pathlib import Path
from typing import Any

from migration_safety import MigrationError, MigrationPlan


_ENV_CONFIG_DIR = os.environ.get("CLAUDE_CONFIG_DIR")
CONFIG_DIR = Path(_ENV_CONFIG_DIR).expanduser() if _ENV_CONFIG_DIR else Path.home() / ".claude"
PROJECTS_DIR = CONFIG_DIR / "projects"
HISTORY_FILE = CONFIG_DIR / "history.jsonl"
CLAUDE_JSON = CONFIG_DIR / ".claude.json" if _ENV_CONFIG_DIR else Path.home() / ".claude.json"
UUID_RE = re.compile(r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\Z")
Rows = list[tuple[str, dict[str, Any] | None]]


def encode_project_path(path: str) -> str:
    """Encode the bucket name; this mapping alone does not establish ownership."""
    return re.sub(r"[/. ]", "-", path)


def exists(path: Path) -> bool:
    """Include dangling links in collision and unsupported-input checks."""
    return os.path.lexists(path)


def absolute_project(path: str) -> str:
    if not isinstance(path, str) or not Path(path).is_absolute() or "\0" in path:
        raise MigrationError("Project paths must be absolute, non-NUL strings")
    return path


def session_uuid(value: str) -> str:
    if not UUID_RE.fullmatch(value):
        raise MigrationError("Session IDs must be exact lowercase UUIDs")
    return value


def unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("Duplicate JSON key")
        result[key] = value
    return result


def reject_constant(_value: str) -> None:
    raise ValueError("Nonstandard JSON number")


def strict_json(text: str) -> Any:
    return json.loads(text, object_pairs_hook=unique_object, parse_constant=reject_constant)


def read_rows(plan: MigrationPlan, path: Path) -> Rows:
    """Capture the complete input, rejecting ambiguous or incomplete JSONL."""
    try:
        text = plan.read(path).decode("utf-8")
    except UnicodeError:
        raise MigrationError(f"Non-UTF-8 JSONL input: {path}") from None
    rows: Rows = []
    # JSONL is delimited by LF, not by arbitrary Unicode line separators.
    for number, raw in enumerate(_lf_lines(text), 1):
        if not raw.strip(" \t\r\n"):
            rows.append((raw, None))
            continue
        try:
            obj = strict_json(raw)
            if not isinstance(obj, dict):
                raise ValueError("JSONL row is not an object")
        except (ValueError, RecursionError):
            raise MigrationError(f"Invalid JSONL object at {path}:{number}") from None
        rows.append((raw, obj))
    return rows


def _lf_lines(text: str) -> list[str]:
    parts = text.split("\n")
    return [part + "\n" for part in parts[:-1]] + ([parts[-1]] if parts[-1] else [])


def replace_field(raw: str, field: str, replacement: str) -> str:
    """Replace one top-level JSON token without reserializing payload bytes."""
    decoder = json.JSONDecoder()
    cursor = len(raw) - len(raw.lstrip()) + 1  # past the validated opening '{'
    while True:
        while raw[cursor].isspace():
            cursor += 1
        key, cursor = decoder.raw_decode(raw, cursor)
        while raw[cursor].isspace():
            cursor += 1
        cursor += 1  # colon in the already validated object
        while raw[cursor].isspace():
            cursor += 1
        start = cursor
        _value, cursor = decoder.raw_decode(raw, cursor)
        if key == field:
            return raw[:start] + json.dumps(replacement, ensure_ascii=False) + raw[cursor:]
        while raw[cursor].isspace():
            cursor += 1
        cursor += 1  # comma; the requested key is known to exist


def history_rows(plan: MigrationPlan) -> tuple[Rows, str]:
    if not exists(HISTORY_FILE):
        plan.expect_absent(HISTORY_FILE)
        return [], f"absent/unupdated: {HISTORY_FILE}"
    return read_rows(plan, HISTORY_FILE), f"captured: {HISTORY_FILE}"


def history_origin(rows: Rows, session_id: str) -> str | None:
    origin = None
    for _raw, obj in rows:
        if obj is not None and obj.get("sessionId") == session_id and "project" in obj:
            project = absolute_project(obj["project"])
            if origin is None:
                origin = project
    return origin


def session_origin(rows: Rows, session_id: str, *, require_identity: bool) -> str | None:
    """Later cwd values may reflect legitimate directory changes within a session."""
    matched = False
    origin = None
    for _raw, obj in rows:
        if obj is None:
            continue
        if "sessionId" in obj:
            if obj["sessionId"] != session_id:
                raise MigrationError("Transcript metadata conflicts with its exact session UUID")
            matched = True
        if "cwd" in obj:
            if obj.get("sessionId") != session_id:
                raise MigrationError("Unsupported cwd record without matching session identity")
            cwd = absolute_project(obj["cwd"])
            if origin is None:
                origin = cwd
    if require_identity and not matched:
        raise MigrationError("Transcript has no matching session UUID metadata")
    return origin


def establish_origin(rows: Rows, history: Rows, session_id: str) -> str:
    transcript = session_origin(rows, session_id, require_identity=True)
    recorded = history_origin(history, session_id)
    if transcript is not None and recorded is not None and transcript != recorded:
        raise MigrationError("Transcript and first history entry disagree about session origin")
    origin = transcript or recorded
    if origin is None:
        raise MigrationError("Cannot establish session origin from transcript or history")
    return origin


def rewrite_session(plan: MigrationPlan, path: Path, rows: Rows,
                    session_id: str, old: str, new: str) -> int:
    session_origin(rows, session_id, require_identity=False)
    count = 0
    output = []
    for raw, obj in rows:
        if obj is not None and obj.get("sessionId") == session_id and obj.get("cwd") == old:
            raw = replace_field(raw, "cwd", new)
            count += 1
        output.append(raw)
    if count:
        plan.rewrite(path, "".join(output).encode("utf-8"))
    return count


def rewrite_history(plan: MigrationPlan, rows: Rows, owners: dict[str, str], new: str) -> int:
    count = 0
    output = []
    for raw, obj in rows:
        if obj is not None:
            sid = obj.get("sessionId")
            old = owners.get(sid) if isinstance(sid, str) else None
            if old is not None and obj.get("project") == old:
                raw = replace_field(raw, "project", new)
                count += 1
        output.append(raw)
    if count:
        plan.rewrite(HISTORY_FILE, "".join(output).encode("utf-8"))
    return count


def plain_directory(path: Path) -> None:
    if path.is_symlink() or not path.is_dir():
        raise MigrationError(f"Expected a real directory, not a link or special file: {path}")


def projects_directory() -> None:
    # The profile's explicit store root may be shared by a symlink. Keep the
    # logical child paths so MigrationPlan pins that link and its target;
    # bucket and sidecar links remain unsupported.
    if not PROJECTS_DIR.is_dir():
        raise MigrationError("Missing or unsupported Claude projects directory")


def sidecar_rows(plan: MigrationPlan, directory: Path, session_id: str) -> list[tuple[Path, Rows]]:
    """Traverse sidecars without following links or interpreting tool-result text."""
    if not exists(directory):
        plan.expect_absent(directory)
        return []
    plain_directory(directory)
    found = []
    for current, directories, filenames in os.walk(directory, followlinks=False):
        for name in directories:
            plain_directory(Path(current) / name)
        for name in filenames:
            path = Path(current) / name
            if path.is_symlink() or not path.is_file():
                raise MigrationError(f"Unsupported linked or special sidecar file: {path}")
            if path.suffix == ".jsonl":
                relative = path.relative_to(directory)
                if relative.parts[0] == "tool-results":
                    continue  # Tool output is opaque, even when it resembles metadata.
                if (len(relative.parts) != 2 or relative.parts[0] != "subagents"
                        or not relative.name.startswith("agent-")):
                    raise MigrationError(f"Unsupported sidecar JSONL path: {path}")
                rows = read_rows(plan, path)
                session_origin(rows, session_id, require_identity=False)
                found.append((path, rows))
    return sorted(found)


def settings_rename(plan: MigrationPlan, old: str, new: str) -> str:
    if not exists(CLAUDE_JSON):
        plan.expect_absent(CLAUDE_JSON)
        return "no config file; unchanged"
    try:
        data = strict_json(plan.read(CLAUDE_JSON).decode("utf-8"))
    except (UnicodeError, ValueError, RecursionError):
        raise MigrationError("Invalid project settings JSON") from None
    if not isinstance(data, dict) or not isinstance(data.get("projects", {}), dict):
        raise MigrationError("Unsupported project settings schema")
    projects = data.get("projects", {})
    if any(not isinstance(value, dict) for value in projects.values()):
        raise MigrationError("Unsupported project settings entry schema")
    if old not in projects:
        return "no old project settings entry; unchanged"
    if new in projects:
        raise MigrationError("Destination project settings already exist; refusing to merge")
    projects[new] = projects.pop(old)
    plan.rewrite(CLAUDE_JSON, (json.dumps(data, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
    return "old project settings entry selected for explicit migration"


def build_rename_plan(old: str, new: str, *, migrate_project_settings: bool = False
                      ) -> tuple[MigrationPlan, dict[str, Any]]:
    absolute_project(old)
    absolute_project(new)
    plan = MigrationPlan()
    summary: dict[str, Any] = {"session files scanned": 0, "session cwd fields rewritten": 0,
                               "history project fields rewritten": 0,
                               "project settings": "unchanged (not requested)"}
    if old == new:
        summary["operation"] = "identical paths; no changes"
        return plan, summary
    if encode_project_path(old) == encode_project_path(new):
        raise MigrationError("Distinct project paths have the same encoded bucket; refusing ambiguity")
    projects_directory()
    source = PROJECTS_DIR / encode_project_path(old)
    destination = PROJECTS_DIR / encode_project_path(new)
    if exists(source) and exists(destination):
        raise MigrationError("Source and destination buckets both exist; refusing to merge")
    if not exists(source) and not exists(destination):
        raise MigrationError("Neither source nor destination session bucket exists")
    bucket = source if exists(source) else destination
    plain_directory(bucket)
    plan.watch_tree(bucket)
    history, summary["history store"] = history_rows(plan)
    transcripts: dict[str, tuple[Path, Rows]] = {}
    sidecars = []
    for path in sorted(bucket.iterdir()):
        if path.is_symlink():
            raise MigrationError(f"Unsupported linked bucket entry: {path}")
        if path.is_file() and path.suffix == ".jsonl" and UUID_RE.fullmatch(path.stem):
            transcripts[path.stem] = (path, read_rows(plan, path))
        elif path.is_dir() and UUID_RE.fullmatch(path.name):
            sidecars.append(path)
        else:
            raise MigrationError(f"Unsupported bucket artifact; ownership is unknown: {path}")
    if not transcripts:
        raise MigrationError("No identifiable sessions in the project bucket")
    if any(path.name not in transcripts for path in sidecars):
        raise MigrationError("Orphan session sidecar has no transcript ownership evidence")
    owners = {sid: establish_origin(rows, history, sid) for sid, (_path, rows) in transcripts.items()}
    expected = {old} if bucket == source else {old, new}
    if len(set(owners.values())) != 1 or not set(owners.values()).issubset(expected):
        raise MigrationError("Unknown or mixed project origins share this encoded bucket")
    for sid, (path, rows) in transcripts.items():
        files = [(path, rows), *sidecar_rows(plan, bucket / sid, sid)]
        for session_path, session_rows in files:
            summary["session files scanned"] += 1
            summary["session cwd fields rewritten"] += rewrite_session(plan, session_path, session_rows, sid, old, new)
    summary["history project fields rewritten"] = rewrite_history(plan, history, {sid: old for sid in owners}, new)
    if migrate_project_settings:
        summary["project settings"] = settings_rename(plan, old, new)
    if bucket == source:
        plan.move(source, destination)
        summary["operation"] = "move project bucket"
    else:
        summary["operation"] = "rewrite already relocated project bucket"
    plan.validate()
    return plan, summary


def session_candidates(session_id: str | None = None) -> list[Path]:
    projects_directory()
    found = []
    for directory in sorted(PROJECTS_DIR.iterdir()):
        if not directory.is_dir():
            continue
        candidates = directory.glob("*.jsonl") if session_id is None else [directory / f"{session_id}.jsonl"]
        for path in candidates:
            if exists(path) and UUID_RE.fullmatch(path.stem):
                plain_directory(directory)
                if path.is_symlink() or not path.is_file():
                    raise MigrationError(f"Unsupported linked or special transcript: {path}")
                found.append(path)
    return found


def validate_destination(plan: MigrationPlan, directory: Path, history: Rows, project: str,
                         incoming: set[str]) -> None:
    """An encoded name is insufficient evidence that an existing bucket is ours."""
    plain_directory(directory)
    def identity(path: Path) -> tuple[int, ...]:
        info = path.lstat()
        if not (stat.S_ISREG(info.st_mode) or stat.S_ISDIR(info.st_mode)):
            raise MigrationError("Destination contains a linked or special artifact")
        return (info.st_dev, info.st_ino, info.st_mode, info.st_uid, info.st_gid)

    resolved = directory.resolve(strict=True)
    directory_identity = identity(directory)

    def inventory() -> dict[str, tuple[int, ...]]:
        if directory.resolve(strict=True) != resolved or identity(directory) != directory_identity:
            raise MigrationError("Destination bucket identity changed")
        return {path.name: identity(path) for path in directory.iterdir()}

    # Only names and identities of unchanged auxiliary artifacts matter here.
    # Their bytes are not migration inputs. Owner transcripts are captured below.
    original = inventory()
    entries = [directory / name for name in sorted(original)]
    owners = set()
    sidecars = set()
    for path in entries:
        if path.is_symlink():
            raise MigrationError(f"Unsupported linked destination artifact: {path}")
        if path.suffix == ".jsonl":
            if not path.is_file() or not UUID_RE.fullmatch(path.stem):
                raise MigrationError(f"Unsupported destination transcript path: {path}")
            origin = establish_origin(read_rows(plan, path), history, path.stem)
            if origin != project:
                raise MigrationError("Existing destination bucket belongs to a different project origin")
            owners.add(path.stem)
        elif path.is_dir() and UUID_RE.fullmatch(path.name):
            sidecars.add(path.name)
    if sidecars - owners:
        raise MigrationError("Destination has an orphan session sidecar without ownership evidence")
    if entries and not owners:
        raise MigrationError("Nonempty destination has no independently verified project owner")

    def verify_destination() -> None:
        current = inventory()
        # The safety helper separately guards absence, no-overwrite publication,
        # and readback for incoming owned files. All preexisting entries stay put.
        if {name: value for name, value in current.items() if name not in incoming} != original:
            raise MigrationError("Destination bucket membership or ownership evidence changed")

    plan.add_check("verify existing Claude destination ownership", verify_destination)


def build_single_plan(session_id: str, project: str) -> tuple[MigrationPlan, dict[str, Any]]:
    session_uuid(session_id)
    absolute_project(project)
    plan = MigrationPlan()
    target_dir = PROJECTS_DIR / encode_project_path(project)
    matches = session_candidates(session_id)
    if len(matches) != 1:
        raise MigrationError(f"Expected one exact session in the store; found {len(matches)}")
    source = matches[0]
    rows = read_rows(plan, source)
    history, history_status = history_rows(plan)
    origin = establish_origin(rows, history, session_id)
    if exists(target_dir):
        if source.parent != target_dir:
            plan.expect_absent(target_dir / source.name)
            plan.expect_absent(target_dir / session_id)
        incoming = set() if source.parent == target_dir else {source.name, session_id}
        validate_destination(plan, target_dir, history, project, incoming)
    if source.parent == target_dir:
        if origin != project:
            raise MigrationError("Session is at the target but its origin metadata is inconsistent")
        sidecar = source.parent / session_id
        files = sidecar_rows(plan, sidecar, session_id)
        if exists(sidecar):
            plan.watch_tree(sidecar)
        plan.validate()
        return plan, {"operation": "session already belongs to target; no changes", "session": session_id,
                      "session files scanned": 1 + len(files), "session cwd fields rewritten": 0,
                      "history project fields rewritten": 0, "history store": history_status,
                      "project settings": "unchanged (not requested)"}
    if not exists(target_dir):
        plan.mkdir(target_dir)
    target = target_dir / source.name
    source_sidecar = source.parent / session_id
    target_sidecar = target_dir / session_id
    plan.expect_absent(target)
    plan.expect_absent(target_sidecar)
    if encode_project_path(origin) != source.parent.name:
        raise MigrationError("Session origin does not match its source bucket encoding")
    if origin != project and encode_project_path(origin) == encode_project_path(project):
        raise MigrationError("Distinct project paths have the same encoded bucket; refusing ambiguity")
    files = [(source, rows), *sidecar_rows(plan, source_sidecar, session_id)]
    rewrites = sum(rewrite_session(plan, path, parsed, session_id, origin, project) for path, parsed in files)
    history_rewrites = rewrite_history(plan, history, {session_id: origin}, project)
    plan.move(source, target)
    if exists(source_sidecar):
        plan.move(source_sidecar, target_sidecar)
    plan.validate()
    return plan, {"operation": "move one session and its sidecar", "session": session_id,
                  "session files scanned": len(files), "session cwd fields rewritten": rewrites,
                  "history project fields rewritten": history_rewrites,
                  "history store": history_status,
                  "project settings": "unchanged (not requested)"}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("session_id", nargs="?", help="exact lowercase Claude session UUID")
    parser.add_argument("--project", help="single-session target; defaults to PWD")
    parser.add_argument("--rename", nargs=2, metavar=("OLD", "NEW"))
    parser.add_argument("--dry-run", action="store_true", help="complete preflight without writes")
    parser.add_argument("--offline", action="store_true", help="assert all affected Claude profiles are stopped")
    parser.add_argument("--backup-dir", type=Path, help="new absolute recovery directory outside Google Drive")
    parser.add_argument("--migrate-project-settings", action="store_true",
                        help="separately authorized trust/settings migration; --rename only")
    args = parser.parse_args()
    if args.rename and (args.session_id or args.project is not None):
        parser.error("--rename cannot be combined with a session ID or --project")
    if args.migrate_project_settings and not args.rename:
        parser.error("--migrate-project-settings requires --rename and separate authorization")
    return args


def main() -> int:
    args = parse_args()
    try:
        if args.rename:
            plan, summary = build_rename_plan(*args.rename, migrate_project_settings=args.migrate_project_settings)
        elif args.session_id:
            plan, summary = build_single_plan(args.session_id, str(Path(args.project or os.getcwd()).resolve()))
        else:
            target = PROJECTS_DIR / encode_project_path(str(Path(args.project or os.getcwd()).resolve()))
            files = sorted((path for path in session_candidates() if path.parent != target),
                           key=lambda path: path.stat().st_mtime, reverse=True)
            for path in files[:10]:
                print(f"{path.stem}\t{path.parent.name}\t{path}")
            if not files:
                print("No sessions outside the current project found.")
            return 0
        if args.rename and args.rename[0] == args.rename[1]:
            print("Identical project paths; nothing changed.")
            return 0
        backup = plan.run(dry_run=args.dry_run, offline=args.offline, backup_dir=args.backup_dir)
        print(f"dry run: {args.dry_run}")
        for key, value in summary.items():
            print(f"{key}: {value}")
        print(f"configuration checked: {CONFIG_DIR.absolute()}")
        print("Coverage: selected Claude configuration and directly resolved aliases only; sibling/custom profile histories were not inventoried.")
        if backup is not None:
            print(f"recovery backup: {backup}")
        print("Resume behavior has not been tested by this metadata migration.")
        return 0
    except MigrationError as error:
        print(f"Migration refused or incomplete: {error}", file=sys.stderr)
        return 1
    except (OSError, ValueError, RecursionError):
        print("Migration refused: unreadable or unsupported store input; no success claimed.", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
