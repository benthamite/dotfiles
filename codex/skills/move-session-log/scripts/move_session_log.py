#!/usr/bin/env python3
"""Relocate known Codex project metadata with recoverable backups.

Historical messages, tool arguments and results are not routing metadata.
Unrelated Codex sessions may stay open in every mode. Only the sessions whose
rollouts are rewritten (those recording the old project path) must be closed,
and the invoking session must not be one of them. Other sessions keep
appending to the shared history.jsonl and session_index.jsonl, which the
safety helper replaces append-safely under an advisory lock, and keep
state_5.sqlite open, whose exact thread rows are updated inside a BEGIN
IMMEDIATE transaction after a consistent online-backup snapshot. Bulk
--rename and --offline imports still require the --offline assertion.
This adapter does not prove that an already-running Codex process has reloaded
the changed metadata. Discovery covers the selected home and matching sibling
.codex* homes, not every possible CODEX_HOME on the machine.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import sqlite3
import stat
import sys
import uuid
from contextlib import closing
from pathlib import Path
from typing import Any

from live_import import connect_database, database_identity, snapshot_database
from migration_safety import MigrationError, MigrationPlan


CODEX_HOME = Path(os.environ.get("CODEX_HOME", Path.home() / ".codex")).expanduser().absolute()
METADATA_FIELDS = ("cwd", "project")
SQLITE_COMPANIONS = ("-wal", "-shm", "-journal")
# Published OpenAI Codex timestamp/recency triggers do not fire on cwd updates.
# Hashes cover the complete SQL definition, with whitespace/case normalized.
# https://github.com/openai/codex/blob/main/codex-rs/state/migrations/0025_thread_timestamps_millis.sql
# https://github.com/openai/codex/blob/main/codex-rs/state/migrations/0039_threads_recency_at.sql
SAFE_THREAD_TRIGGERS = frozenset({
    "05cb2acd14ae36b7933ca986b895954e6d838bf0e7e19e1d14f4a58b56d8606b",
    "ed93274ae4659d4b39ab2e08f09bfbbd8096c57a03b4210fdb366e4105cb77d9",
    "ef776661ed0ae8a63bb1d3f3c5dd5353f6ca395c647e3bde7ed25a28bdb60430",
    "53e0579944effe81856f7237f4875b3022ea5b0df222fc773e3b3d01a8dd3ce7",
    "8d621ac64fd35a4bc8d882101fc2e8209cb612d3a345ce816f3452ea1f2c3e99",
})


def session_uuid(value: Any) -> str:
    if not isinstance(value, str):
        raise MigrationError("Session identity must be a complete canonical UUID")
    try:
        parsed = uuid.UUID(value)
    except ValueError:
        raise MigrationError("Session identity must be a complete canonical UUID") from None
    if str(parsed) != value:
        raise MigrationError("Session identity must be a complete canonical UUID")
    return value


def absolute_project(value: Any) -> str:
    if not isinstance(value, str) or "\0" in value or not Path(value).is_absolute():
        raise MigrationError("Project paths must be absolute")
    return value


def unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("duplicate JSON key")
        result[key] = value
    return result


def invalid_constant(_value: str) -> None:
    raise ValueError("non-JSON numeric constant")


def decode_row(raw: bytes) -> Any:
    try:
        return json.loads(raw.decode("utf-8"), object_pairs_hook=unique_object,
                          parse_constant=invalid_constant)
    except (ValueError, UnicodeError, RecursionError):
        raise MigrationError("Malformed or ambiguous JSONL; no migration was planned") from None


def lf_lines(data: bytes) -> list[bytes]:
    pieces = data.split(b"\n")
    return [piece + b"\n" for piece in pieces[:-1]] + ([pieces[-1]] if pieces[-1] else [])


def rows(data: bytes) -> list[tuple[bytes, Any]]:
    return [(raw, decode_row(raw) if raw.strip(b" \t\r\n") else None)
            for raw in lf_lines(data)]


def header(data: bytes) -> dict[str, Any]:
    first = next((raw for raw in lf_lines(data) if raw.strip(b" \t\r\n")), None)
    obj = decode_row(first) if first is not None else None
    if not isinstance(obj, dict) or obj.get("type") != "session_meta":
        raise MigrationError("A rollout has no supported session_meta header")
    payload = obj.get("payload")
    if not isinstance(payload, dict):
        raise MigrationError("A rollout has malformed session metadata")
    session_uuid(payload.get("id"))
    absolute_project(payload.get("cwd"))
    return payload


def captured_header(path: Path) -> dict[str, Any]:
    before = path.stat()
    if not stat.S_ISREG(before.st_mode):
        raise MigrationError("Session inventory contains a non-regular file")
    with path.open("rb") as handle:
        raw = handle.readline()
        while raw and not raw.strip(b" \t\r\n"):
            raw = handle.readline()
    after = path.stat()
    identity = lambda item: (item.st_dev, item.st_ino, item.st_size, item.st_mtime_ns, item.st_ctime_ns)
    if identity(before) != identity(after):
        raise MigrationError("Session inventory changed while it was being inspected")
    return header(raw)


def discover() -> tuple[list[Path], list[Path], list[Path]]:
    sessions = CODEX_HOME / "sessions"
    if not sessions.is_dir():
        raise MigrationError("The selected Codex sessions directory is missing or unavailable")
    shared = sessions.resolve(strict=True)
    homes = [CODEX_HOME]
    for candidate in sorted(CODEX_HOME.parent.glob(".codex*")):
        if candidate.is_dir() and (candidate / "sessions").is_dir():
            if (candidate / "sessions").resolve(strict=True) == shared:
                homes.append(candidate)
    homes = list(dict.fromkeys(path.resolve(strict=True) for path in homes))
    roots: list[Path] = []
    for home in homes:
        for root in (home / "sessions", home / "archived_sessions"):
            if os.path.lexists(root):
                if not root.is_dir():
                    raise MigrationError("A discovered session root is not a directory")
                roots.append(root.resolve(strict=True))
    roots = list(dict.fromkeys(roots))
    files: dict[Path, None] = {}

    def traversal_error(_error: OSError) -> None:
        raise MigrationError("Session inventory traversal was incomplete")

    for root in roots:
        for directory, subdirs, names in os.walk(root, onerror=traversal_error):
            if any((Path(directory) / child).is_symlink() for child in subdirs):
                raise MigrationError("Nested symlink directories make session discovery unsupported")
            for name in names:
                if name.endswith(".jsonl"):
                    path = Path(directory) / name
                    if path.is_symlink():
                        raise MigrationError("Rollout file symlinks are outside the supported discovery boundary")
                    if not path.is_file():
                        raise MigrationError("Session inventory contains an unavailable JSONL file")
                    files[path.resolve(strict=True)] = None
    return homes, roots, sorted(files)


def replace_path_tokens(raw: bytes, replacements: dict[tuple[str, ...], str]) -> bytes:
    """Replace only selected JSON value tokens in an already-validated row."""
    text = raw.decode("utf-8")
    decoder = json.JSONDecoder()
    spans: list[tuple[int, int, str]] = []

    def whitespace(position: int) -> int:
        while position < len(text) and text[position] in " \t\r\n":
            position += 1
        return position

    def visit(position: int, path: tuple[str, ...]) -> int:
        position = whitespace(position)
        if path in replacements:
            _value, end = decoder.raw_decode(text, position)
            spans.append((position, end, json.dumps(replacements[path], ensure_ascii=False)))
            return end
        if not any(target[:len(path)] == path for target in replacements):
            return decoder.raw_decode(text, position)[1]
        if text[position] != "{":
            raise MigrationError("A selected metadata token has an unsupported structure")
        position = whitespace(position + 1)
        while text[position] != "}":
            key, position = decoder.raw_decode(text, position)
            position = whitespace(position)
            if text[position] != ":":
                raise MigrationError("A selected metadata token could not be located")
            position = whitespace(visit(position + 1, path + (key,)))
            if text[position] == ",":
                position = whitespace(position + 1)
            elif text[position] != "}":
                raise MigrationError("A selected metadata token could not be located")
        return position + 1

    try:
        visit(0, ())
    except (ValueError, UnicodeError, RecursionError, IndexError):
        raise MigrationError("A selected metadata token could not be safely located") from None
    if len(spans) != len(replacements):
        raise MigrationError("Not every selected metadata token could be located")
    for start, end, replacement in reversed(spans):
        text = text[:start] + replacement + text[end:]
    return text.encode("utf-8")


Mapping = dict[str, str]


def remap_rollout(data: bytes, mapping: Mapping, identity: str) -> tuple[bytes, dict[str, int]]:
    """Map every routing cwd that exactly equals a mapped source path to its destination.

    The rollout's own identity is the first record. Forked and subagent threads
    copy their ancestors' session_meta records behind that header, and Codex
    0.129-0.133 re-append the thread's own record with a changed memory_mode, so
    a rollout may hold several session_meta records and an id may recur. Every
    record sharing an id must agree on cwd; ancestor records are remapped under
    the same exact-match rule as the owning record and turn_context, never by
    prefix. Returns the rewritten bytes and the remapped field count per source.
    """
    parsed = rows(data)
    meta = header(data)
    if meta["id"] != identity:
        raise MigrationError("The selected rollout identity changed")
    cwd_by_id: dict[str, Any] = {}
    output: list[bytes] = []
    counts = {old: 0 for old in mapping}
    for raw, obj in parsed:
        target = None
        if isinstance(obj, dict) and obj.get("type") in ("session_meta", "turn_context"):
            payload = obj.get("payload")
            if not isinstance(payload, dict):
                raise MigrationError("A rollout has malformed routing metadata")
            if "cwd" in payload and not isinstance(payload["cwd"], str):
                raise MigrationError("A rollout has an unsupported cwd value")
            if obj.get("type") == "session_meta":
                try:
                    record_id = session_uuid(payload.get("id"))
                except MigrationError:
                    raise MigrationError("A rollout has session metadata without a canonical identity") from None
                if cwd_by_id.setdefault(record_id, payload.get("cwd")) != payload.get("cwd"):
                    raise MigrationError("A rollout has conflicting session metadata for one identity")
            cwd = payload.get("cwd")
            if isinstance(cwd, str) and cwd in mapping and mapping[cwd] != cwd:
                counts[cwd] += 1
                target = mapping[cwd]
        output.append(replace_path_tokens(raw, {("payload", "cwd"): target}) if target is not None else raw)
    return b"".join(output), counts


def rewrite_rollout(data: bytes, old: str, new: str, identity: str) -> tuple[bytes, int]:
    """Single-mapping form of remap_rollout."""
    updated, counts = remap_rollout(data, {old: new}, identity)
    return updated, counts[old]


def remap_index(data: bytes, key: str, mapping: Mapping,
                identity: str | None) -> tuple[bytes, dict[str, int]]:
    output: list[bytes] = []
    counts = {old: 0 for old in mapping}
    for raw, obj in rows(data):
        changes: dict[tuple[str, ...], str] = {}
        if isinstance(obj, dict) and isinstance(obj.get(key), str):
            selected = obj[key] == identity if identity is not None else True
            if selected:
                for field in METADATA_FIELDS:
                    value = obj.get(field)
                    if isinstance(value, str) and value in mapping and mapping[value] != value:
                        session_uuid(obj[key])
                        changes[(field,)] = mapping[value]
                        counts[value] += 1
        output.append(replace_path_tokens(raw, changes) if changes else raw)
    return b"".join(output), counts


def rewrite_index(data: bytes, key: str, old: str, new: str,
                  identity: str | None) -> tuple[bytes, int]:
    """Single-mapping form of remap_index."""
    updated, counts = remap_index(data, key, {old: new}, identity)
    return updated, counts[old]


def validate_state_generation(home: Path) -> None:
    if any(path.name != "state_5.sqlite" for path in home.glob("state_*.sqlite")):
        raise MigrationError("Unsupported Codex state database generation is present")


def validate_thread_schema(conn: sqlite3.Connection) -> tuple[set[str], set[str]]:
    definition = conn.execute("SELECT type, sql FROM sqlite_schema WHERE name = 'threads'").fetchall()
    if (len(definition) != 1 or definition[0][0] != "table"
            or not isinstance(definition[0][1], str)
            or not definition[0][1].upper().startswith("CREATE TABLE ")):
        raise MigrationError("Unsupported Codex thread database schema")
    info = conn.execute("PRAGMA table_xinfo(threads)").fetchall()
    columns = {row[1] for row in info}
    if (not {"id", "cwd"}.issubset(columns)
            or any(len(row) != 7 or row[6] != 0 for row in info)
            or [(row[1], row[5]) for row in info if row[5]] != [("id", 1)]):
        raise MigrationError("Unsupported Codex thread columns or identity constraint")
    for (sql,) in conn.execute("SELECT sql FROM sqlite_schema WHERE type = 'trigger' AND tbl_name = 'threads'"):
        normalized = " ".join(sql.split()).rstrip(";").lower() if isinstance(sql, str) else ""
        if hashlib.sha256(normalized.encode()).hexdigest() not in SAFE_THREAD_TRIGGERS:
            raise MigrationError("Unsupported Codex thread trigger; no triggers were disabled")
    if any(row[2] and row[3] != "pk" for row in conn.execute("PRAGMA index_list(threads)")):
        raise MigrationError("Unsupported additional unique thread constraint")
    tables = {row[0] for row in conn.execute("SELECT name FROM sqlite_schema WHERE type = 'table'")}
    for table in tables:
        quoted = '"' + table.replace('"', '""') + '"'
        for row in conn.execute(f"PRAGMA foreign_key_list({quoted})"):
            if ((table == "threads" and row[3].lower() == "cwd")
                    or (row[2].lower() == "threads" and row[4] is not None and row[4].lower() == "cwd")):
                raise MigrationError("Unsupported cwd foreign-key dependency")
    return columns, tables


def cwd_only_authorizer(action: int, table: str | None, column: str | None,
                        _database: str | None, trigger: str | None) -> int:
    if action in (sqlite3.SQLITE_INSERT, sqlite3.SQLITE_DELETE):
        return sqlite3.SQLITE_DENY
    if action == sqlite3.SQLITE_UPDATE and (table, column, trigger) != ("threads", "cwd", None):
        return sqlite3.SQLITE_DENY
    return sqlite3.SQLITE_OK


def database_action(database: Path, bound: tuple[Any, ...], changes: list[tuple[str, str, str]]) -> None:
    """Update exact thread rows (id, original cwd, new cwd) in one transaction.

    Unrelated Codex connections may stay open; BEGIN IMMEDIATE serialises them.
    """
    if database_identity(database) != bound:
        raise MigrationError("Database location or inode changed after preflight")
    try:
        with closing(sqlite3.connect(database.resolve().as_uri() + "?mode=rw",
                                     uri=True, timeout=5)) as conn:
            conn.execute("BEGIN IMMEDIATE")
            validate_thread_schema(conn)
            cursor = conn.execute("SELECT * FROM threads ORDER BY id")
            columns = [column[0] for column in cursor.description]
            before = cursor.fetchall()
            id_column, cwd_column = columns.index("id"), columns.index("cwd")
            targets = {identity: new for identity, _original, new in changes}
            expected = [tuple(targets[row[id_column]] if index == cwd_column and row[id_column] in targets else value
                              for index, value in enumerate(row)) for row in before]
            for identity, original, _new in changes:
                current = conn.execute("SELECT cwd FROM threads WHERE id = ?", (identity,)).fetchall()
                if current != [(original,)]:
                    raise MigrationError("SQLite thread identity or cwd changed after preflight")
            conn.set_authorizer(cwd_only_authorizer)
            for identity, original, new in changes:
                cursor = conn.execute("UPDATE threads SET cwd = ? WHERE id = ? AND cwd = ?",
                                      (new, identity, original))
                if cursor.rowcount != 1:
                    raise MigrationError("SQLite thread update did not affect exactly one row")
            if conn.execute("SELECT * FROM threads ORDER BY id").fetchall() != expected:
                raise MigrationError("SQLite verification failed; transaction was not committed")
            conn.commit()
    except sqlite3.Error:
        raise MigrationError("SQLite migration failed; consult the backup journal for partial state") from None


def select_threads(conn: sqlite3.Connection, mapping: Mapping, identity: str | None) -> list[tuple[str, str]]:
    if identity is None:
        placeholders = ", ".join("?" for _old in mapping)
        selected = conn.execute(f"SELECT id, cwd FROM threads WHERE cwd IN ({placeholders}) ORDER BY id",
                                tuple(mapping)).fetchall()
    else:
        selected = conn.execute("SELECT id, cwd FROM threads WHERE id = ?", (identity,)).fetchall()
    if len({row[0] for row in selected}) != len(selected):
        raise MigrationError("Ambiguous duplicate SQLite thread identities")
    for selected_id, current in selected:
        session_uuid(selected_id)
        absolute_project(current)
    return selected


def plan_database(plan: MigrationPlan, database: Path, mapping: Mapping,
                  identity: str | None) -> tuple[dict[str, int], list[str]]:
    """Select exact thread rows from a read-only view of the live database.

    Unrelated Codex processes may keep the database open. Its WAL and shared
    memory are never copied as static inputs: the recovery backup is an
    online-backup snapshot, the selection is re-checked before every operation,
    and every mapping's rows are updated in one BEGIN IMMEDIATE transaction at
    apply time. Returns the changed row count per source path.
    """
    validate_state_generation(database.parent)
    counts = {old: 0 for old in mapping}
    if not os.path.lexists(database):
        resolved = database.resolve()
        if any(os.path.lexists(Path(str(resolved) + suffix)) for suffix in SQLITE_COMPANIONS):
            raise MigrationError("SQLite companion files exist without their database")
        plan.expect_absent(database)
        return counts, []
    bound = database_identity(database)
    try:
        with closing(connect_database(database)) as conn:
            conn.execute("BEGIN")
            columns, tables = validate_thread_schema(conn)
            if conn.execute("PRAGMA quick_check").fetchall() != [("ok",)]:
                raise MigrationError("SQLite integrity preflight failed")
            unupdated = ([f"{database}: threads.project_id"] if "project_id" in columns else [])
            unupdated += [f"{database}: {table}" for table in ("projects", "project_roots") if table in tables]
            selected = select_threads(conn, mapping, identity)
    except sqlite3.Error:
        raise MigrationError("Codex thread database could not be inspected") from None
    if database_identity(database) != bound:
        raise MigrationError("Database location changed during preflight")
    # A single-session import maps the session's own origin wherever its row
    # currently points; a rename maps each row by its exact current cwd.
    changes: list[tuple[str, str, str]] = []
    for selected_id, current in selected:
        source = next(iter(mapping)) if identity is not None else current
        new = mapping[source]
        if current != new:
            changes.append((selected_id, current, new))
            counts[source] += 1
    state = {"applied": False}

    def check_selection() -> None:
        """The selected rows still hold their preflight cwd, or ours once applied."""
        if database_identity(database) != bound:
            raise MigrationError("Database location or inode changed after preflight")
        expected = dict(selected)
        if state["applied"]:
            expected.update({selected_id: new for selected_id, _original, new in changes})
        with closing(connect_database(database)) as conn:
            current_rows = dict(select_threads(conn, mapping, identity))
            if identity is None and not state["applied"] and set(current_rows) - set(expected):
                raise MigrationError("A thread acquired a source project cwd after preflight")
            for selected_id, cwd in expected.items():
                if conn.execute("SELECT cwd FROM threads WHERE id = ?", (selected_id,)).fetchall() != [(cwd,)]:
                    raise MigrationError("Selected SQLite threads changed after preflight")

    def apply() -> None:
        database_action(database, bound, changes)
        state["applied"] = True

    plan.add_check(f"Codex thread selection: {database}", check_selection)
    if changes:
        plan.add_backup(f"Codex thread database snapshot: {database}", lambda: snapshot_database(database))
        plan.add_action(f"update Codex thread cwd: {database}", apply)
    return counts, unupdated


def directory_versions(roots: list[Path]) -> dict[Path, tuple[int, ...]]:
    """Identity, mtime, ctime and size of every directory an inventory change would touch.

    Creating, removing or renaming an entry updates its parent directory's
    mtime, so an unchanged snapshot proves that no rollout, session root or
    sibling profile appeared or disappeared, without rescanning every rollout.
    """
    def version(directory: Path) -> tuple[int, ...]:
        info = os.stat(directory)
        return (info.st_ino, info.st_mtime_ns, info.st_ctime_ns, info.st_size)

    def traversal_error(_error: OSError) -> None:
        raise MigrationError("Session inventory traversal was incomplete")

    versions: dict[Path, tuple[int, ...]] = {}
    for directory in (CODEX_HOME.parent, CODEX_HOME, *sorted(CODEX_HOME.parent.glob(".codex*"))):
        if directory.is_dir():
            versions[directory] = version(directory)
    for root in roots:
        for current, _subdirs, _names in os.walk(root, onerror=traversal_error):
            versions[Path(current)] = version(Path(current))
    return versions


def validate_mappings(pairs: list[tuple[str, str]]) -> Mapping:
    """Exact, distinct, non-chaining path mappings whose destinations exist."""
    if not pairs:
        raise MigrationError("A batch rename needs at least one OLD NEW mapping")
    mapping: Mapping = {}
    for old, new in pairs:
        absolute_project(old)
        absolute_project(new)
        if old == new:
            raise MigrationError(f"A mapping maps a path to itself: {old}")
        if old in mapping:
            raise MigrationError(f"Duplicate source path in mappings: {old}")
        mapping[old] = new
    destinations = list(mapping.values())
    if len(set(destinations)) != len(destinations):
        raise MigrationError("Two mappings share one destination path")
    chained = set(destinations) & set(mapping)
    if chained:
        raise MigrationError(f"A destination path is also a source path: {sorted(chained)[0]}")
    for new in destinations:
        if not Path(new).is_dir():
            raise MigrationError(f"Destination directory does not exist: {new}")
    return mapping


def read_mapping_file(path: Path) -> list[tuple[str, str]]:
    """One 'OLD<TAB>NEW' exact mapping per line; blank lines and # comments are skipped."""
    pairs = []
    for number, line in enumerate(path.read_text("utf-8").split("\n"), 1):
        line = line.rstrip("\r")
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        fields = line.split("\t")
        if len(fields) != 2 or not all(fields):
            raise MigrationError(f"Mapping file line {number} is not OLD<TAB>NEW")
        pairs.append((fields[0], fields[1]))
    return pairs


def plan_store(identity: str | None, mapping: Mapping | None, new: str | None
               ) -> tuple[MigrationPlan, dict[str, Any]]:
    """Scan the store once and plan every mapping's rewrites together.

    A single-session import passes IDENTITY and NEW; its mapping is the
    session's own recorded origin. A rename passes the validated MAPPING.
    """
    homes, roots, files = discover()
    versions = directory_versions(roots)
    inventory = {path: captured_header(path) for path in files}
    ids = [meta["id"] for meta in inventory.values()]
    if len(set(ids)) != len(ids):
        raise MigrationError("Multiple rollouts have the same session identity")
    if identity is not None:
        selected = [path for path, meta in inventory.items() if meta["id"] == identity]
        if len(selected) != 1:
            raise MigrationError("No unique rollout has the requested exact session identity")
        mapping = {inventory[selected[0]]["cwd"]: new}
    else:
        selected = files
    assert mapping is not None
    plan = MigrationPlan()

    def check_inventory() -> None:
        nonlocal versions
        for home in homes:
            validate_state_generation(home)
        current = directory_versions(roots)
        if current == versions:
            return  # No entry was created, removed or renamed under any inventoried directory.
        current_homes, current_roots, current_files = discover()
        if (current_homes, current_roots) != (homes, roots) or set(files) - set(current_files):
            raise MigrationError("Codex profile/session inventory changed after preflight")
        for path in sorted(set(current_files) - set(files)):
            # Sessions started elsewhere while this runs are unaffected. One
            # started in a source project, or one reusing the selected
            # identity, would be missed by this plan.
            meta = captured_header(path)
            if meta["cwd"] in mapping if identity is None else meta["id"] == identity:
                raise MigrationError("A session started inside a source project after preflight")
        versions = current

    plan.add_check("Codex profile/session inventory", check_inventory)
    per_mapping = {old: {"old": old, "new": target, "session_files_rewritten": 0, "session_fields": 0,
                         "history_fields": 0, "index_fields": 0, "database_rows": 0}
                   for old, target in mapping.items()}
    report: dict[str, Any] = {"homes": [str(path) for path in homes],
                              "session_roots": [str(path) for path in roots],
                              "session_files_scanned": len(files),
                              "session_files_rewritten": 0, "session_fields": 0,
                              "history_fields": 0, "index_fields": 0, "database_rows": 0,
                              "database_files_checked": 0,
                              "session": str(selected[0]) if identity is not None else None,
                              "unupdated_project_metadata": [],
                              "missing": [], "mappings": list(per_mapping.values())}

    def tally(counts: dict[str, int], field: str, files_field: str | None = None) -> int:
        for old, count in counts.items():
            per_mapping[old][field] += count
            if files_field is not None and count:
                per_mapping[old][files_field] += 1
        return sum(counts.values())

    for path in selected:
        data = plan.read(path)
        if header(data) != inventory[path]:
            raise MigrationError("Session metadata changed after inventory")
        updated, counts = remap_rollout(data, mapping, inventory[path]["id"])
        count = tally(counts, "session_fields", "session_files_rewritten")
        if count:
            plan.rewrite(path, updated)
            report["session_files_rewritten"] += 1
            report["session_fields"] += count
        else:
            # Nothing here records a source project: the session owning this
            # rollout may stay open and keep appending to it.
            plan.release(path)
    seen: set[Path] = set()
    for home in homes:
        for name, key, counter in (("history.jsonl", "session_id", "history_fields"),
                                   ("session_index.jsonl", "id", "index_fields")):
            # Every Codex session appends to these shared stores; capture them
            # append-only so unrelated sessions may continue writing.
            path = home / name
            resolved = path.resolve()
            if resolved in seen:
                if os.path.lexists(path):
                    plan.read(path, append_only=True)
                continue
            seen.add(resolved)
            if not os.path.lexists(path):
                plan.expect_absent(path)
                report["missing"].append(str(path))
                continue
            data = plan.read(path, append_only=True)
            updated, counts = remap_index(data, key, mapping, identity)
            count = tally(counts, counter)
            if count:
                plan.rewrite(path, updated)
                report[counter] += count
        database = home / "state_5.sqlite"
        resolved = database.resolve()
        if resolved in seen:
            if os.path.lexists(database):
                bound = database_identity(database)
                plan.add_check(f"Codex thread database alias: {database}",
                               lambda alias=database, bound=bound: database_identity(alias) == bound)
            continue
        seen.add(resolved)
        if not os.path.lexists(database):
            report["missing"].append(str(database))
        else:
            report["database_files_checked"] += 1
        counts, unupdated = plan_database(plan, database, mapping, identity)
        report["database_rows"] += tally(counts, "database_rows")
        report["unupdated_project_metadata"].extend(unupdated)
    plan.annotate("mappings", report["mappings"])
    plan.validate()
    check_inventory()
    return plan, report


def make_plan(identity: str | None, old: str | None, new: str) -> tuple[MigrationPlan, dict[str, Any]]:
    """Single-session import (IDENTITY, NEW) or single-mapping rename (OLD, NEW)."""
    absolute_project(new)
    if identity is not None:
        session_uuid(identity)
        return plan_store(identity, None, new)
    absolute_project(old)
    return plan_store(None, {old: new}, None)


def make_batch_plan(pairs: list[tuple[str, str]]) -> tuple[MigrationPlan, dict[str, Any]]:
    """Rename several exact project paths in one scan of the store."""
    return plan_store(None, validate_mappings(pairs), None)


def list_recent(project: str) -> None:
    _homes, _roots, files = discover()
    shown = 0
    for path in sorted(files, key=lambda item: item.stat().st_mtime_ns, reverse=True):
        meta = captured_header(path)
        if meta["cwd"] != project:
            print(f"{meta['id']}\t{meta['cwd']}\t{path}")
            shown += 1
            if shown == 10:
                break
    if not shown:
        print("No recent sessions outside the current project found in the discovered roots.")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("session_id", nargs="?", help="complete Codex session UUID")
    parser.add_argument("--project", help="single-session target; defaults to PWD")
    parser.add_argument("--rename", nargs=2, metavar=("OLD", "NEW"))
    parser.add_argument("--rename-file", type=Path, metavar="FILE",
                        help="batch rename in one scan: one exact 'OLD<TAB>NEW' mapping per line; "
                             "every NEW must be an existing directory")
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--offline", action="store_true",
                        help="assert that no Codex session is running in the affected project directories "
                             "(required for bulk rename)")
    parser.add_argument("--backup-dir", type=Path, help="new absolute private backup directory outside Drive")
    args = parser.parse_args()
    if (args.rename or args.rename_file) and (args.session_id or args.project is not None):
        parser.error("--rename/--rename-file cannot be combined with a session ID or --project")
    if args.rename and args.rename_file:
        parser.error("--rename and --rename-file are mutually exclusive")
    return args


def print_mappings(report: dict[str, Any]) -> None:
    for item in report["mappings"]:
        print(f"mapping: {item['old']} -> {item['new']}")
        print(f"  session files rewritten: {item['session_files_rewritten']}")
        print(f"  session path fields rewritten: {item['session_fields']}")
        print(f"  history path fields rewritten: {item['history_fields']}")
        print(f"  session_index path fields rewritten: {item['index_fields']}")
        print(f"  state_db thread cwd rows rewritten: {item['database_rows']}")


def main() -> int:
    args = parse_args()
    try:
        if args.rename_file:
            plan, report = make_batch_plan(read_mapping_file(args.rename_file))
        elif args.rename:
            old, new = args.rename
            absolute_project(old)
            plan, report = make_plan(None, old, new)
        else:
            project = str(Path(args.project or os.getcwd()).resolve())
            if not args.session_id:
                list_recent(project)
                return 0
            if args.offline:
                plan, report = make_plan(args.session_id, None, project)
            else:
                from live_import import make_live_plan
                plan, report = make_live_plan(args.session_id, project,
                                              adapter=sys.modules[__name__])
        backup = plan.run(dry_run=args.dry_run, offline=args.offline, backup_dir=args.backup_dir)
        print(f"dry run: {args.dry_run}")
        if report["session"] is not None:
            print(f"session: {report['session']}")
        print(f"session files scanned: {report['session_files_scanned']}")
        print(f"session files rewritten: {report['session_files_rewritten']}")
        print(f"session path fields rewritten: {report['session_fields']}")
        print(f"history path fields rewritten: {report['history_fields']}")
        print(f"session_index path fields rewritten: {report['index_fields']}")
        print(f"state_db thread cwd rows rewritten: {report['database_rows']}")
        print(f"state_db files checked: {report['database_files_checked']}")
        if args.rename_file:
            print_mappings(report)
        for home in report["homes"]:
            print(f"profile checked: {home}")
        for missing in report["missing"]:
            print(f"absent/unupdated metadata store: {missing}")
        for field in report["unupdated_project_metadata"]:
            print(f"unchanged project metadata (outside cwd-only migration): {field}")
        print("Coverage: selected CODEX_HOME and matching sibling .codex* profiles only; other custom homes were not inventoried.")
        print("Runtime resume/reload was not tested.")
        if backup is not None:
            print(f"backup directory: {backup}")
        return 0
    except MigrationError as error:
        print(f"Migration refused or incomplete: {error}", file=sys.stderr)
        backup = getattr(error, "backup_dir", None)
        if backup is not None:
            print(f"Recovery backup: {backup}", file=sys.stderr)
        return 1
    except (OSError, UnicodeError):
        print("Migration refused or incomplete: metadata filesystem operation failed", file=sys.stderr)
        return 1
    except sqlite3.Error as error:
        print(f"Migration refused or incomplete: SQLite preflight failed: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
