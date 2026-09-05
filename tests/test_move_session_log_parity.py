"""Parity tests for the Claude and Codex session-relocation adapters.

Both move_session_log.py scripts must support

    python3 scripts/move_session_log.py --dry-run --rename OLD NEW
    python3 scripts/move_session_log.py --rename OLD NEW --offline --backup-dir NEW_PRIVATE_BACKUP

and must apply equivalent mapping to runtime-owned metadata: an exact-match OLD
path becomes exactly NEW, other values are untouched, and --dry-run modifies
nothing. Applying requires offline stores and private recovery backups.
The session-root layouts differ
(Codex: ~/.codex/sessions + history + index; Claude: ~/.claude/projects with
encoded directory names + history + ~/.claude.json), so the scripts are not
byte-identical; the mapping behavior must be.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import sqlite3
import subprocess
import tempfile
import unittest
import uuid
from contextlib import closing
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CODEX_SCRIPT = ROOT / "codex" / "skills" / "move-session-log" / "scripts" / "move_session_log.py"
CLAUDE_SCRIPT = ROOT / "claude" / "skills" / "move-session-log" / "scripts" / "move_session_log.py"

OLD = "/Users/example/My Drive/repos/sample-project"
NEW = "/Users/example/repos/sample-project"
OTHER = "/Users/example/elsewhere/unrelated-project"
SESSION_ID = "11111111-2222-3333-4444-555555555555"

PATH_KEYS = {"cwd", "project", "workdir", "working_dir"}


def encode_claude(path: str) -> str:
    return re.sub(r"[/. ]", "-", path)


def jsonl(rows: list[dict]) -> str:
    return "".join(json.dumps(row, ensure_ascii=False) + "\n" for row in rows)


def tree_digest(root: Path) -> dict[str, str]:
    digest = {}
    for path in sorted(root.rglob("*")):
        if path.is_file():
            digest[str(path.relative_to(root))] = hashlib.sha256(
                path.read_bytes()
            ).hexdigest()
    return digest


def collect_path_values(root: Path) -> list[str]:
    values: list[str] = []

    def walk(obj):
        if isinstance(obj, dict):
            for key, value in obj.items():
                if key in PATH_KEYS and isinstance(value, str):
                    values.append(value)
                else:
                    walk(value)
        elif isinstance(obj, list):
            for item in obj:
                walk(item)

    for path in sorted(root.rglob("*.jsonl")):
        for line in path.read_text().splitlines():
            if not line.strip():
                continue
            try:
                walk(json.loads(line))
            except json.JSONDecodeError:
                continue
    return values


class MoveSessionLogParityTest(unittest.TestCase):
    def make_codex_home(self, base: Path) -> Path:
        home = base / "codex-home"
        day = home / "sessions" / "2026" / "08" / "01"
        day.mkdir(parents=True)
        (day / f"rollout-2026-08-01T10-00-00-{SESSION_ID}.jsonl").write_text(
            jsonl(
                [
                    {
                        "type": "session_meta",
                        "payload": {
                            "id": SESSION_ID,
                            "timestamp": "2026-08-01T10:00:00Z",
                            "cwd": OLD,
                        },
                    },
                    {"type": "turn_context", "payload": {"cwd": OLD}},
                    {"type": "turn_context", "payload": {"cwd": OTHER}},
                ]
            )
        )
        (home / "history.jsonl").write_text(
            jsonl(
                [
                    {"session_id": SESSION_ID, "text": "hello", "cwd": OLD},
                    {"session_id": "other", "text": "hi", "cwd": OTHER},
                ]
            )
        )
        (home / "session_index.jsonl").write_text(
            jsonl([{"id": SESSION_ID, "cwd": OLD}])
        )
        return home

    def make_claude_config(self, base: Path) -> Path:
        config = base / "claude-config"
        project_dir = config / "projects" / encode_claude(OLD)
        project_dir.mkdir(parents=True)
        (project_dir / f"{SESSION_ID}.jsonl").write_text(
            jsonl(
                [
                    {"sessionId": SESSION_ID, "cwd": OLD, "type": "user"},
                    {"sessionId": SESSION_ID, "cwd": OLD, "type": "assistant"},
                    {"sessionId": SESSION_ID, "cwd": OTHER, "type": "user"},
                ]
            )
        )
        (config / "history.jsonl").write_text(
            jsonl(
                [
                    {"sessionId": SESSION_ID, "project": OLD, "display": "hello"},
                    {"sessionId": "other", "project": OTHER, "display": "hi"},
                ]
            )
        )
        (config / ".claude.json").write_text(
            json.dumps({"projects": {OLD: {"allowedTools": []}, OTHER: {}}}, indent=2)
        )
        return config

    def make_codex_state_db(self, home: Path, cwd: str = OLD) -> Path:
        database = home / "state_5.sqlite"
        with closing(sqlite3.connect(database)) as conn, conn:
            conn.execute(
                "CREATE TABLE threads (id TEXT PRIMARY KEY, rollout_path TEXT, cwd TEXT)"
            )
            conn.execute(
                "INSERT INTO threads VALUES (?, ?, ?)",
                (SESSION_ID, f"rollout-{SESSION_ID}.jsonl", cwd),
            )
        return database

    def run_codex(self, home: Path, *args: str) -> subprocess.CompletedProcess:
        if args and "--dry-run" not in args:
            args += ("--offline", "--backup-dir", str(home.parent / f"recovery-{uuid.uuid4()}"))
        result = subprocess.run(
            ["python3", str(CODEX_SCRIPT), *args],
            capture_output=True,
            text=True,
            env={
                **os.environ,
                "CODEX_HOME": str(home),
                "PYTHONWARNINGS": "always::ResourceWarning",
            },
        )
        self.assertNotIn("ResourceWarning", result.stderr)
        return result

    def run_claude(self, config: Path, *args: str) -> subprocess.CompletedProcess:
        if args and "--dry-run" not in args:
            args += ("--offline", "--backup-dir", str(config.parent / f"recovery-{uuid.uuid4()}"))
        return subprocess.run(
            ["python3", str(CLAUDE_SCRIPT), *args],
            capture_output=True,
            text=True,
            env={**os.environ, "CLAUDE_CONFIG_DIR": str(config)},
        )

    def test_dry_run_modifies_nothing_in_either_tree(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            codex_home = self.make_codex_home(base)
            claude_config = self.make_claude_config(base)
            before_codex = tree_digest(codex_home)
            before_claude = tree_digest(claude_config)

            result = self.run_codex(codex_home, "--dry-run", "--rename", OLD, NEW)
            self.assertEqual(result.returncode, 0, result.stderr)
            result = self.run_claude(claude_config, "--dry-run", "--rename", OLD, NEW)
            self.assertEqual(result.returncode, 0, result.stderr)

            self.assertEqual(tree_digest(codex_home), before_codex)
            self.assertEqual(tree_digest(claude_config), before_claude)

    def test_rename_applies_byte_equivalent_path_mapping(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            codex_home = self.make_codex_home(base)
            claude_config = self.make_claude_config(base)

            result = self.run_codex(codex_home, "--rename", OLD, NEW)
            self.assertEqual(result.returncode, 0, result.stderr)
            result = self.run_claude(claude_config, "--rename", OLD, NEW)
            self.assertEqual(result.returncode, 0, result.stderr)

            codex_values = collect_path_values(codex_home)
            claude_values = collect_path_values(claude_config)

            # Identical mapping semantics: OLD is gone from both trees, the
            # rewritten value is byte-identical to NEW on both sides, and
            # non-matching values survive untouched on both sides.
            for label, values in (("codex", codex_values), ("claude", claude_values)):
                with self.subTest(tool=label):
                    self.assertNotIn(OLD, values)
                    self.assertIn(NEW, values)
                    self.assertIn(OTHER, values)
            self.assertEqual(set(codex_values), set(claude_values))

    def test_claude_rename_moves_project_dir_and_claude_json_entry(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            claude_config = self.make_claude_config(base)
            result = self.run_claude(
                claude_config, "--rename", OLD, NEW, "--migrate-project-settings"
            )
            self.assertEqual(result.returncode, 0, result.stderr)

            self.assertFalse((claude_config / "projects" / encode_claude(OLD)).exists())
            new_dir = claude_config / "projects" / encode_claude(NEW)
            self.assertTrue(new_dir.is_dir())
            self.assertTrue((new_dir / f"{SESSION_ID}.jsonl").is_file())

            data = json.loads((claude_config / ".claude.json").read_text())
            self.assertIn(NEW, data["projects"])
            self.assertNotIn(OLD, data["projects"])
            self.assertIn(OTHER, data["projects"])

            history = [
                json.loads(line)
                for line in (claude_config / "history.jsonl").read_text().splitlines()
            ]
            self.assertEqual(
                [entry["project"] for entry in history], [NEW, OTHER]
            )

    def test_claude_rename_preserves_project_settings_by_default(self):
        with tempfile.TemporaryDirectory() as tmp:
            config = self.make_claude_config(Path(tmp))
            original = (config / ".claude.json").read_bytes()
            result = self.run_claude(config, "--rename", OLD, NEW)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual((config / ".claude.json").read_bytes(), original)

    def test_rewrite_preserves_history_symlink_on_both_sides(self):
        # Profiles may share one history store via a symlink (e.g.
        # ~/.claude-epoch/history.jsonl -> ~/.claude/history.jsonl); the
        # rewrite must go through the link, not replace it.
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            codex_home = self.make_codex_home(base)
            claude_config = self.make_claude_config(base)

            for home, history in (
                (codex_home, codex_home / "history.jsonl"),
                (claude_config, claude_config / "history.jsonl"),
            ):
                target = base / f"shared-{home.name}-history.jsonl"
                history.rename(target)
                history.symlink_to(target)

            result = self.run_codex(codex_home, "--rename", OLD, NEW)
            self.assertEqual(result.returncode, 0, result.stderr)
            result = self.run_claude(claude_config, "--rename", OLD, NEW)
            self.assertEqual(result.returncode, 0, result.stderr)

            for home in (codex_home, claude_config):
                history = home / "history.jsonl"
                with self.subTest(tool=home.name):
                    self.assertTrue(history.is_symlink())
                    target_text = history.resolve().read_text()
                    self.assertIn(NEW, target_text)
                    self.assertNotIn('"' + OLD + '"', target_text)

    def test_rename_requires_absolute_paths_on_both_sides(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            codex_home = self.make_codex_home(base)
            claude_config = self.make_claude_config(base)
            result = self.run_codex(codex_home, "--rename", "relative/x", NEW)
            self.assertNotEqual(result.returncode, 0)
            result = self.run_claude(claude_config, "--rename", "relative/x", NEW)
            self.assertNotEqual(result.returncode, 0)

    def test_codex_updates_every_profile_sharing_the_session_store(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            shared_sessions = base / "shared-sessions"
            day = shared_sessions / "2026" / "08" / "01"
            day.mkdir(parents=True)
            (day / f"rollout-2026-08-01T10-00-00-{SESSION_ID}.jsonl").write_text(
                jsonl(
                    [
                        {
                            "type": "session_meta",
                            "payload": {"id": SESSION_ID, "cwd": OLD},
                        }
                    ]
                )
            )

            homes = [base / ".codex-epoch", base / ".codex-epoch3"]
            for home in homes:
                home.mkdir()
                (home / "sessions").symlink_to(
                    shared_sessions, target_is_directory=True
                )
                (home / "history.jsonl").write_text("")
                (home / "session_index.jsonl").write_text("")
                self.make_codex_state_db(home)

            result = self.run_codex(homes[0], "--project", NEW, SESSION_ID)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("state_db thread cwd rows rewritten: 2", result.stdout)
            self.assertIn("state_db files checked: 2", result.stdout)

            for home in homes:
                with closing(sqlite3.connect(home / "state_5.sqlite")) as conn:
                    cwd = conn.execute(
                        "SELECT cwd FROM threads WHERE id = ?", (SESSION_ID,)
                    ).fetchone()[0]
                self.assertEqual(cwd, NEW)

            session_file = next(shared_sessions.rglob("*.jsonl"))
            session_file.write_text(
                jsonl(
                    [
                        {
                            "type": "session_meta",
                            "payload": {"id": SESSION_ID, "cwd": OLD},
                        }
                    ]
                )
            )
            for home in homes:
                with closing(sqlite3.connect(home / "state_5.sqlite")) as conn, conn:
                    conn.execute("UPDATE threads SET cwd = ?", (OLD,))

            result = self.run_codex(homes[0], "--rename", OLD, NEW)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("state_db thread cwd rows rewritten: 2", result.stdout)
            for home in homes:
                with closing(sqlite3.connect(home / "state_5.sqlite")) as conn:
                    cwd = conn.execute(
                        "SELECT cwd FROM threads WHERE id = ?", (SESSION_ID,)
                    ).fetchone()[0]
                self.assertEqual(cwd, NEW)

    def test_codex_single_session_finds_archived_session(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            codex_home = self.make_codex_home(base)
            archived = codex_home / "archived_sessions"
            archived.mkdir()
            active = next((codex_home / "sessions").rglob("*.jsonl"))
            archived_file = archived / active.name
            active.rename(archived_file)
            self.make_codex_state_db(codex_home)

            result = self.run_codex(codex_home, "--project", NEW, SESSION_ID)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn(f"session: {archived_file.resolve()}", result.stdout)
            self.assertNotIn(OLD, collect_path_values(archived))
            self.assertIn(NEW, collect_path_values(archived))

            with closing(sqlite3.connect(codex_home / "state_5.sqlite")) as conn:
                cwd = conn.execute(
                    "SELECT cwd FROM threads WHERE id = ?", (SESSION_ID,)
                ).fetchone()[0]
            self.assertEqual(cwd, NEW)

if __name__ == "__main__":
    unittest.main()
