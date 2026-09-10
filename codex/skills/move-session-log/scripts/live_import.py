"""Import a closed rollout while unrelated Codex SQLite clients remain open.

SQLite's online backup API and BEGIN IMMEDIATE coordinate database access.
Ordinary JSONL replacements still require their own writers to be stopped.
"""

from contextlib import closing
import hashlib
import os
from pathlib import Path
import sqlite3
import stat
import tempfile
import time

from migration_safety import MigrationError, MigrationPlan, _capture, _check_writers, _private_file


def database_identity(path):
    resolved = path.resolve(strict=True)
    info = resolved.stat()
    if not stat.S_ISREG(info.st_mode):
        raise MigrationError("Database is not a regular file")
    alias = path.lstat()
    return (resolved, info.st_dev, info.st_ino, alias.st_dev, alias.st_ino)


def connect_database(path):
    return sqlite3.connect(path.resolve().as_uri() + "?mode=ro", uri=True, timeout=5)


def snapshot_database(path):
    """Read a consistent SQLite snapshot without copying live WAL/SHM bytes."""
    deadline = time.monotonic() + 15

    def progress(_status, _remaining, _total):
        if time.monotonic() > deadline:
            raise MigrationError("SQLite snapshot timed out; retry after current database write finishes")

    with tempfile.TemporaryDirectory(prefix="codex-live-import-", dir="/tmp") as temporary:
        target = Path(temporary) / "snapshot.sqlite"
        with closing(connect_database(path)) as source:
            with closing(sqlite3.connect(target)) as destination:
                source.backup(destination, pages=256, progress=progress, sleep=0.05)
                # Consolidate the owned destination; never checkpoint the source.
                destination.execute("PRAGMA journal_mode=DELETE")
        return target.read_bytes()


def inspect_database(path, adapter, identity):
    with closing(connect_database(path)) as conn:
        conn.execute("BEGIN")
        columns, tables = adapter.validate_thread_schema(conn)
        if conn.execute("PRAGMA quick_check").fetchall() != [("ok",)]:
            raise MigrationError("SQLite integrity preflight failed")
        selected = conn.execute("SELECT cwd FROM threads WHERE id = ?", (identity,)).fetchall()
        if len(selected) > 1:
            raise MigrationError("Ambiguous SQLite thread identity")
        if selected:
            adapter.absolute_project(selected[0][0])
        unchanged = [f"{path}: threads.project_id"] if "project_id" in columns else []
        unchanged.extend(f"{path}: {table}" for table in ("projects", "project_roots") if table in tables)
        return selected, unchanged


class LiveImportPlan(MigrationPlan):
    def __init__(self, adapter, identity, new):
        super().__init__()
        self.adapter, self.identity, self.new = adapter, identity, new
        self.databases = []
        self.connections = {}

    def validate(self):
        try:
            super().validate()
        except MigrationError as error:
            if "open writer" in str(error):
                for captured in self.files.values():
                    try:
                        _check_writers([captured["resolved"]])
                    except MigrationError as targeted:
                        if "open writer" in str(targeted):
                            raise MigrationError(f"Close the writer for {captured['path']} before importing") from None
                        raise
            raise

    def _rewrite(self, key, data):
        try:
            super()._rewrite(key, data)
        except MigrationError as error:
            raise MigrationError(f"Cannot replace {key}: {error}") from None

    def _backup(self, directory):
        super()._backup(directory)
        self.manifest["databaseSnapshots"] = []
        for number, record in enumerate(self.databases):
            if not record["changes"]:
                continue
            data = snapshot_database(record["path"])
            name = f"database-{number:06d}.sqlite"
            _private_file(self.backup_dir / name, data)
            self.manifest["databaseSnapshots"].append({
                "originalPath": str(record["path"]), "backup": name,
                "sha256": hashlib.sha256(data).hexdigest(),
                "threadId": self.identity, "originalCwd": record["selected"][0][0],
                "recovery": "Inspect target row and journal; never restore this whole database over newer activity.",
            })
            self._save_manifest()

    def _apply_database(self, record):
        conn = self.connections[record["path"]]
        cursor = conn.execute("SELECT * FROM threads ORDER BY id")
        columns = [item[0] for item in cursor.description]
        before = cursor.fetchall()
        id_column, cwd_column = columns.index("id"), columns.index("cwd")
        expected = [tuple(self.new if index == cwd_column and row[id_column] == self.identity else value
                          for index, value in enumerate(row)) for row in before]
        conn.set_authorizer(self.adapter.cwd_only_authorizer)
        result = conn.execute("UPDATE threads SET cwd = ? WHERE id = ? AND cwd = ?",
                              (self.new, self.identity, record["selected"][0][0]))
        if result.rowcount != 1 or conn.execute("SELECT * FROM threads ORDER BY id").fetchall() != expected:
            raise MigrationError("SQLite verification failed; transaction was not committed")
        conn.commit()

    def run(self, dry_run=False, offline=False, backup_dir=None):
        if dry_run or not (self.rewrites or self.actions):
            return super().run(dry_run=True, backup_dir=backup_dir)
        if backup_dir is None:
            raise MigrationError("Closed-session import requires an explicit private backup directory")
        # Validate before taking database locks so malformed files fail promptly.
        self.validate()
        try:
            for record in sorted(self.databases, key=lambda item: str(item["path"])):
                if database_identity(record["path"]) != record["identity"]:
                    raise MigrationError("Database location or inode changed after preflight")
                conn = sqlite3.connect(record["path"].resolve().as_uri() + "?mode=rw", uri=True, timeout=5)
                self.connections[record["path"]] = conn
                conn.execute("BEGIN IMMEDIATE")
                self.adapter.validate_thread_schema(conn)
                current = conn.execute("SELECT cwd FROM threads WHERE id = ?", (self.identity,)).fetchall()
                if current != record["selected"]:
                    raise MigrationError("Selected SQLite thread changed after preflight")
            # Here offline means the ordinary files alone: the caller has closed
            # the target session, and the inherited writer guard checks each file.
            return super().run(offline=True, backup_dir=backup_dir)
        except sqlite3.Error as error:
            raise MigrationError("SQLite import could not acquire or complete its transaction",
                                 self.backup_dir) from error
        finally:
            for conn in self.connections.values():
                conn.close()  # Rolls back every transaction not already committed.
            self.connections.clear()


def make_live_plan(identity, new, adapter=None):
    if adapter is None:
        import move_session_log as adapter
    adapter.session_uuid(identity)
    adapter.absolute_project(new)
    if identity in {os.environ.get("CODEX_THREAD_ID"), os.environ.get("CODEX_SESSION_ID")}:
        raise MigrationError("Cannot relocate the current active session; close it first")
    homes, roots, files = adapter.discover()
    selected = []
    identities = set()
    for path in files:
        # Read only the header of unrelated rollouts; appends elsewhere do not
        # invalidate an exact-session import.
        with path.open("rb") as stream:
            raw = stream.readline()
            while raw and not raw.strip():
                raw = stream.readline()
        meta = adapter.header(raw)
        if meta["id"] in identities:
            raise MigrationError("Multiple rollouts have the same session identity")
        identities.add(meta["id"])
        if meta["id"] == identity:
            selected.append((path, meta))
    if len(selected) != 1:
        raise MigrationError("No unique rollout has the requested exact session identity")
    path, meta = selected[0]
    old = meta["cwd"]
    plan = LiveImportPlan(adapter, identity, new)
    data = plan.read(path)
    if adapter.header(data) != meta:
        raise MigrationError("Selected rollout header changed during preflight")
    updated, count = adapter.rewrite_rollout(data, old, new, identity)
    plan.rewrite(path, updated)
    report = {"homes": list(map(str, homes)), "session_roots": list(map(str, roots)),
              "session_files_scanned": len(files), "session_files_rewritten": int(bool(count)),
              "session_fields": count, "history_fields": 0, "index_fields": 0,
              "database_rows": 0, "database_files_checked": 0, "session": str(path),
              "unupdated_project_metadata": [], "missing": []}
    seen = set()
    passive_indices = []
    aliases = []
    missing_paths = []
    for home in homes:
        adapter.validate_state_generation(home)
        for name, key, counter in (("history.jsonl", "session_id", "history_fields"),
                                   ("session_index.jsonl", "id", "index_fields")):
            index = home / name
            resolved = index.resolve()
            aliases.append((index, resolved))
            if resolved in seen:
                continue
            seen.add(resolved)
            if not os.path.lexists(index):
                report["missing"].append(str(index))
                missing_paths.append(index)
                continue
            captured = _capture(index)
            output, changed = adapter.rewrite_index(captured["data"], key, old, new, identity)
            if changed:
                if plan.read(index) != captured["data"]:
                    raise MigrationError("Shared routing metadata changed while preparing import")
                plan.rewrite(index, output)
                report[counter] += changed
            else:
                passive_indices.append((index, key))
        database = home / "state_5.sqlite"
        resolved = database.resolve()
        aliases.append((database, resolved))
        if resolved in seen:
            continue
        seen.add(resolved)
        if not os.path.lexists(database):
            report["missing"].append(str(database))
            missing_paths.append(database)
            continue
        bound = database_identity(database)
        rows, unchanged = inspect_database(database, adapter, identity)
        if database_identity(database) != bound:
            raise MigrationError("Database location changed during preflight")
        changes = bool(rows and rows != [(new,)])
        record = {"path": database, "identity": bound, "selected": rows, "changes": changes}
        plan.databases.append(record)
        report["database_files_checked"] += 1
        report["database_rows"] += int(changes)
        report["unupdated_project_metadata"].extend(unchanged)
        if changes:
            plan.add_action(f"update Codex thread cwd: {database}",
                            lambda record=record: plan._apply_database(record))

    def check_scope():
        current_homes, current_roots, _files = adapter.discover()
        if (current_homes, current_roots) != (homes, roots):
            raise MigrationError("Shared profile inventory changed after preflight")
        for alias, resolved in aliases:
            if alias.resolve() != resolved:
                raise MigrationError("Metadata alias changed after preflight")
        for missing in missing_paths:
            if os.path.lexists(missing):
                raise MigrationError("A previously absent metadata store appeared after preflight")
        for index, key in passive_indices:
            # Concurrent unrelated appends are allowed. A newly introduced
            # target routing field changes scope and must be planned afresh.
            captured = _capture(index)
            _, changed = adapter.rewrite_index(captured["data"], key, old, new, identity)
            if changed:
                raise MigrationError("Shared index acquired target routing metadata after preflight")
        for candidate in set(_files) - set(files):
            with candidate.open("rb") as stream:
                raw = stream.readline()
                while raw and not raw.strip():
                    raw = stream.readline()
            if adapter.header(raw)["id"] == identity:
                raise MigrationError("Another rollout acquired the selected session identity")
        for home in homes:
            adapter.validate_state_generation(home)
        for record in plan.databases:
            if database_identity(record["path"]) != record["identity"]:
                raise MigrationError("Database location changed after preflight")

    plan.add_check("closed-session import scope", check_scope)
    plan.validate()
    return plan, report
