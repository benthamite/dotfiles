#!/usr/bin/env python3
"""Check whether this skill has changed upstream, and update it on request.

    skill-update.py check
    skill-update.py apply --commit SHA [--overwrite-local]

`check` compares the installed copy with the published one and prints one JSON
object. It changes nothing. `apply` replaces the installed files with the
published ones at exactly the commit `check` reported, after verifying every
downloaded file against the hash GitHub listed for it. Replaced and removed
files are first copied to a backup directory, whose path is printed.

Statuses from `check`:

- `up-to-date`: the installed files equal the published ones.
- `update-available`: they differ, and no installed file was edited locally
  since the last update (or there is no record to tell; see `local_edits`).
- `local-edits`: they differ, and files listed in `locally_modified` were
  edited locally; an update would overwrite them.
- `source-checkout`: this copy lives in a git checkout of the upstream
  repository. Update it with git; nothing was fetched.
- `check-failed`: the published copy could not be read (`reason` says why).

Only the fixed upstream below is ever contacted, without credentials, and
nothing about this machine is sent beyond the request itself. An update
replaces instructions an agent will follow, so `apply` is run only after the
person has seen what changed and agreed.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

REPO = "benthamite/dotfiles"
BRANCH = "master"
# The published copy for Claude; a copy installed under a `.codex` directory
# follows the Codex one instead (select_upstream).
SKILL_PATH = "macos/.claude/skills/security-audit"
API = "https://api.github.com/repos/" + REPO
RAW = "https://raw.githubusercontent.com/" + REPO
MANIFEST = ".upstream.json"
IGNORED_NAMES = {MANIFEST, ".DS_Store"}
IGNORED_DIRS = {"__pycache__"}
TIMEOUT = 15
MAX_FILE_BYTES = 2_000_000


class UpdateError(Exception):
    pass


def fetch(url: str) -> bytes:
    """Return the body at URL, which must be on one of the two upstream hosts."""
    if not (url.startswith(API) or url.startswith(RAW)):
        raise UpdateError("refusing to fetch outside the upstream repository")
    request = urllib.request.Request(
        url, headers={"User-Agent": "security-audit-skill-update", "Accept": "application/vnd.github+json"}
    )
    try:
        with urllib.request.urlopen(request, timeout=TIMEOUT) as response:
            body = response.read(MAX_FILE_BYTES + 1)
    except urllib.error.HTTPError as error:
        raise UpdateError(f"upstream answered HTTP {error.code}") from None
    except (urllib.error.URLError, TimeoutError, OSError) as error:
        raise UpdateError(f"upstream unreachable ({type(error).__name__})") from None
    if len(body) > MAX_FILE_BYTES:
        raise UpdateError("upstream response is larger than expected")
    return body


def blob_sha(data: bytes) -> str:
    """Return the git blob hash of DATA, the hash GitHub lists for a file."""
    return hashlib.sha1(b"blob %d\0" % len(data) + data).hexdigest()


def safe_relative(path: str) -> bool:
    parts = Path(path).parts
    return bool(parts) and not Path(path).is_absolute() and ".." not in parts and "" not in parts


def local_files(root: Path) -> dict[str, str]:
    """Map each installed file's path, relative to ROOT, to its blob hash."""
    found = {}
    for directory, subdirs, names in os.walk(root):
        subdirs[:] = sorted(d for d in subdirs if d not in IGNORED_DIRS)
        for name in sorted(names):
            path = Path(directory) / name
            if name in IGNORED_NAMES or path.is_symlink():
                continue
            found[path.relative_to(root).as_posix()] = blob_sha(path.read_bytes())
    return found


def upstream_head(get=fetch) -> str:
    data = json.loads(get(f"{API}/commits/{BRANCH}"))
    sha = data.get("sha") if isinstance(data, dict) else None
    if not isinstance(sha, str) or len(sha) != 40 or any(c not in "0123456789abcdef" for c in sha):
        raise UpdateError("upstream returned no commit identifier")
    return sha


def upstream_files(commit: str, get=fetch) -> dict[str, str]:
    """Map each published file's path, relative to the skill, to its blob hash."""
    found: dict[str, str] = {}
    pending = [SKILL_PATH]
    while pending:
        directory = pending.pop()
        url = f"{API}/contents/{urllib.parse.quote(directory)}?ref={commit}"
        entries = json.loads(get(url))
        if not isinstance(entries, list):
            raise UpdateError("upstream returned an unexpected directory listing")
        for entry in entries:
            kind, path, sha = entry.get("type"), entry.get("path"), entry.get("sha")
            if not isinstance(path, str) or not path.startswith(SKILL_PATH + "/"):
                raise UpdateError("upstream listed a path outside the skill")
            relative = path[len(SKILL_PATH) + 1:]
            if not safe_relative(relative):
                raise UpdateError("upstream listed an unsafe path")
            if kind == "dir":
                pending.append(path)
            elif kind == "file" and isinstance(sha, str):
                if Path(relative).name not in IGNORED_NAMES:
                    found[relative] = sha
            else:
                # Symlinks and submodules are never installed.
                raise UpdateError(f"upstream holds an unsupported entry ({kind})")
    if "SKILL.md" not in found:
        raise UpdateError("upstream copy has no SKILL.md")
    return found


def recent_commits(get=fetch, limit: int = 5) -> list[dict[str, str]]:
    url = f"{API}/commits?sha={BRANCH}&path={urllib.parse.quote(SKILL_PATH)}&per_page={limit}"
    try:
        data = json.loads(get(url))
    except (UpdateError, ValueError):
        return []
    result = []
    for item in data if isinstance(data, list) else []:
        commit = item.get("commit", {})
        subject = str(commit.get("message", "")).splitlines()[0][:120] if commit.get("message") else ""
        result.append({"date": str(commit.get("committer", {}).get("date", ""))[:10], "subject": subject})
    return result


def read_manifest(root: Path) -> dict:
    try:
        data = json.loads((root / MANIFEST).read_text())
    except (OSError, ValueError):
        return {}
    return data if isinstance(data, dict) and isinstance(data.get("files"), dict) else {}


def compare(local: dict[str, str], upstream: dict[str, str], manifest: dict) -> dict:
    """Classify the difference between the installed and the published files."""
    added = sorted(p for p in upstream if p not in local)
    modified = sorted(p for p in upstream if p in local and local[p] != upstream[p])
    recorded = manifest.get("files") or {}
    # A file absent upstream is removed only if the last update installed it.
    removed = sorted(p for p in local if p not in upstream and p in recorded)
    if not (added or modified or removed):
        return {"status": "up-to-date"}
    if recorded:
        edited = sorted(
            p for p in modified + removed if recorded.get(p) is not None and recorded[p] != local[p]
        ) + sorted(p for p in modified if p not in recorded)
        local_edits = "known"
    else:
        edited = []
        local_edits = "unknown"
    return {
        "status": "local-edits" if edited else "update-available",
        "added": added,
        "modified": modified,
        "removed": removed,
        "locally_modified": sorted(set(edited)),
        "local_edits": local_edits,
    }


def is_source_checkout(root: Path) -> bool:
    try:
        url = subprocess.run(
            ["git", "-C", str(root), "config", "--get", "remote.origin.url"],
            capture_output=True, text=True, timeout=10,
        ).stdout.strip()
    except (OSError, subprocess.SubprocessError):
        return False
    return url.removesuffix(".git").rstrip("/").lower().endswith(REPO.lower())


def select_upstream(root: Path) -> None:
    """Follow the Codex copy upstream when ROOT is installed for Codex."""
    global SKILL_PATH
    tool = ".codex" if ".codex" in root.resolve().parts else ".claude"
    SKILL_PATH = f"macos/{tool}/skills/security-audit"


def check(root: Path, get=fetch) -> dict:
    select_upstream(root)
    if is_source_checkout(root):
        return {"status": "source-checkout"}
    try:
        commit = upstream_head(get)
        result = compare(local_files(root), upstream_files(commit, get), read_manifest(root))
    except UpdateError as error:
        return {"status": "check-failed", "reason": str(error)}
    except ValueError:
        return {"status": "check-failed", "reason": "upstream returned unreadable data"}
    result["commit"] = commit
    if result["status"] != "up-to-date":
        result["recent_upstream_commits"] = recent_commits(get)
    return result


def apply(root: Path, commit: str, overwrite_local: bool = False, get=fetch, backup_root: Path | None = None) -> dict:
    if len(commit) != 40 or any(c not in "0123456789abcdef" for c in commit):
        raise UpdateError("--commit must be the full identifier that `check` printed")
    select_upstream(root)
    if is_source_checkout(root):
        raise UpdateError("this copy is a git checkout of upstream; update it with git")
    upstream = upstream_files(commit, get)
    difference = compare(local_files(root), upstream, read_manifest(root))
    if difference["status"] == "up-to-date":
        write_manifest(root, commit, upstream)
        return {"status": "up-to-date", "commit": commit}
    if difference["locally_modified"] and not overwrite_local:
        raise UpdateError("locally edited files would be overwritten; pass --overwrite-local to accept that")
    # Download and verify everything before touching the installed copy.
    staging = Path(tempfile.mkdtemp(prefix="security-audit-update-"))
    try:
        for relative in difference["added"] + difference["modified"]:
            data = get(f"{RAW}/{commit}/{urllib.parse.quote(SKILL_PATH + '/' + relative)}")
            if blob_sha(data) != upstream[relative]:
                raise UpdateError(f"{relative} does not match the hash upstream listed for it")
            target = staging / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(data)
        backup_root = backup_root or Path.home() / ".cache" / "security-audit-skill"
        backup = backup_root / time.strftime("backup-%Y%m%d-%H%M%S")
        for relative in difference["modified"] + difference["removed"]:
            saved = backup / relative
            saved.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(root / relative, saved)
        for relative in difference["added"] + difference["modified"]:
            target = root / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            mode = target.stat().st_mode & 0o777 if target.exists() else 0o644
            os.replace(staging / relative, target)
            os.chmod(target, mode)
        for relative in difference["removed"]:
            (root / relative).unlink()
    finally:
        shutil.rmtree(staging, ignore_errors=True)
    write_manifest(root, commit, upstream)
    return {
        "status": "updated",
        "commit": commit,
        "added": difference["added"],
        "modified": difference["modified"],
        "removed": difference["removed"],
        "backup": str(backup) if (difference["modified"] or difference["removed"]) else None,
    }


def write_manifest(root: Path, commit: str, files: dict[str, str]) -> None:
    text = json.dumps({"repository": REPO, "commit": commit, "files": files}, indent=1, sort_keys=True)
    temporary = root / (MANIFEST + ".tmp")
    temporary.write_text(text + "\n")
    os.replace(temporary, root / MANIFEST)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--dir", type=Path, default=Path(__file__).resolve().parents[1],
                        help="the installed skill directory (default: this script's skill)")
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("check")
    applying = commands.add_parser("apply")
    applying.add_argument("--commit", required=True)
    applying.add_argument("--overwrite-local", action="store_true")
    args = parser.parse_args(argv)
    root = args.dir.resolve()
    if not (root / "SKILL.md").is_file():
        print(json.dumps({"status": "check-failed", "reason": "no SKILL.md in the skill directory"}))
        return 2
    if args.command == "check":
        result = check(root)
        print(json.dumps(result, indent=1))
        return 2 if result["status"] == "check-failed" else 0
    try:
        print(json.dumps(apply(root, args.commit, args.overwrite_local), indent=1))
    except UpdateError as error:
        print(json.dumps({"status": "update-failed", "reason": str(error)}))
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
