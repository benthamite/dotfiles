"""Shared helpers for resolving live Codex plugin skill roots."""

from __future__ import annotations

import json
import os
import re
import subprocess
from pathlib import Path
from typing import Iterable


CODEX_PLUGIN_SKILLS_RE = re.compile(
    r"(?P<path>/[^\n`]*?\.codex[^/\n`]*/plugins/cache/"
    r"[^/\n`]+/[^/\n`]+/[^/\n`]+/skills)"
)


def plugin_path_is_orphaned(path: Path) -> bool:
    for parent in (path, *path.parents):
        if (parent / ".orphaned_at").exists():
            return True
        if parent.name == "cache":
            return False
    return False


def load_codex_plugin_list(codex_home: Path, codex_program: str = "codex") -> list[dict]:
    env = os.environ.copy()
    env["CODEX_HOME"] = str(codex_home)
    try:
        proc = subprocess.run(
            [codex_program, "plugin", "list", "--json"],
            check=False,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=env,
        )
    except OSError:
        return []
    if proc.returncode != 0:
        return []
    try:
        data = json.loads(proc.stdout)
    except json.JSONDecodeError:
        return []
    return codex_plugin_entries(data)


def codex_plugin_entries(plugin_data: object) -> list[dict]:
    if isinstance(plugin_data, dict):
        entries = plugin_data.get("installed") or []
    else:
        entries = plugin_data
    if not isinstance(entries, list):
        return []
    return [entry for entry in entries if isinstance(entry, dict)]


def codex_plugin_skill_roots(codex_home: Path, plugin_data: object) -> list[Path]:
    roots: list[Path] = []
    seen: set[Path] = set()
    for entry in codex_plugin_entries(plugin_data):
        if not (entry.get("installed") is True and entry.get("enabled") is True):
            continue
        name = entry.get("name")
        marketplace = entry.get("marketplaceName")
        version = entry.get("version")
        if not (isinstance(name, str) and isinstance(marketplace, str) and version):
            continue
        skills_root = (
            Path(codex_home)
            / "plugins"
            / "cache"
            / marketplace
            / name
            / str(version)
            / "skills"
        )
        if not skills_root.is_dir() or plugin_path_is_orphaned(skills_root):
            continue
        real = _real_path(skills_root)
        if real in seen:
            continue
        seen.add(real)
        roots.append(skills_root)
    return roots


def extract_codex_prompt_plugin_roots(prompt_text: str) -> list[Path]:
    roots: list[Path] = []
    seen: set[Path] = set()
    for match in CODEX_PLUGIN_SKILLS_RE.finditer(prompt_text):
        path = Path(match.group("path"))
        real = _real_path(path)
        if real in seen:
            continue
        seen.add(real)
        roots.append(path)
    return roots


def stale_codex_prompt_plugin_roots(
    prompt_text: str,
    active_roots: Iterable[Path],
) -> list[Path]:
    active = {_real_path(Path(root)) for root in active_roots}
    stale: list[Path] = []
    for root in extract_codex_prompt_plugin_roots(prompt_text):
        if _real_path(root) not in active:
            stale.append(root)
    return stale


def _real_path(path: Path) -> Path:
    try:
        return path.resolve()
    except OSError:
        return path.expanduser()
