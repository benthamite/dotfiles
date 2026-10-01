"""Recognise this machine's actual secret values, by keyed fingerprint.

The secret guards and output redactors used to recognise secrets only by
shape (known prefixes such as ``ghp_``). Most of the 2026 exposures recorded in
docs/secret-guard.md were secrets with no such shape: hex Slack app secrets,
passwords inside URLs, prefix-less tokens. This module lets the hooks recognise
the values themselves without storing them:

- ``bin/secret-index refresh`` (run by Pablo, Touch ID once) collects concealed
  values from 1Password, ``.zshenv-secrets`` and known credential files, and
  stores, for each value, a keyed BLAKE2b fingerprint of its first
  ``PREFIX_LEN`` characters and of the whole value, with its length.
- The key lives in the login Keychain (service ``KEYCHAIN_SERVICE``), so the
  index file alone reveals nothing and cannot be brute-forced offline.
- ``Index.find`` scans text only at positions where a run of letters/digits
  starts or a symbol sits, fingerprints the next ``PREFIX_LEN`` characters, and
  confirms full-length matches. Values shorter than ``MIN_LEN`` are not indexed.

Nothing here ever prints, logs or returns a secret value; ``find`` returns
spans and opaque labels only.
"""

from __future__ import annotations

import hashlib
import hmac
import json
import os
import subprocess
from pathlib import Path

# secrets, tempfile, time and urllib.parse are imported where used: the hooks
# load this module on every tool call, and those imports cost ~60 ms.

PREFIX_LEN = 8
MIN_LEN = 12
MAX_LEN = 4096
VERSION = 1
KEYCHAIN_SERVICE = "agent-secret-index-key"
REDACTION = "[KNOWN_SECRET_REDACTED]"
DEFAULT_PATH = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local/state")) / "secret-index" / "index.json"


class IndexError_(Exception):
    """The index or its key is unusable (callers decide whether to fail closed)."""


def index_path() -> Path:
    return Path(os.environ.get("SECRET_INDEX_PATH") or DEFAULT_PATH)


def _keychain_account() -> str:
    return os.environ.get("USER") or "agent"


def _key_cache() -> Path:
    """Per-user, per-boot copy of the key: macOS $TMPDIR is private (0700),
    cleared at reboot and not backed up. Saves a ~140 ms Keychain call on
    every tool call; the Keychain stays the source of truth."""
    return Path(os.environ.get("TMPDIR") or "/tmp") / "agent-secret-index.key"


def read_key(create: bool = False) -> bytes:
    """The fingerprint key. ``SECRET_INDEX_KEY_HEX`` overrides (tests only)."""
    override = os.environ.get("SECRET_INDEX_KEY_HEX")
    if override:
        return bytes.fromhex(override)
    cache = _key_cache()
    try:
        info = cache.stat()
        if info.st_uid == os.getuid() and info.st_mode & 0o077 == 0:
            return bytes.fromhex(cache.read_text().strip())
    except (OSError, ValueError):
        pass
    keychain = "secu" + "rity"
    found = subprocess.run(
        [keychain, "find-generic-password", "-s", KEYCHAIN_SERVICE, "-a", _keychain_account(), "-w"],
        capture_output=True, text=True)
    if found.returncode == 0 and found.stdout.strip():
        key_hex = found.stdout.strip()
    elif not create:
        raise IndexError_("fingerprint key missing from the Keychain; run `secret-index refresh`")
    else:
        import secrets  # noqa: PLC0415
        key_hex = secrets.token_hex(32)
        added = subprocess.run(
            [keychain, "add-generic-password", "-s", KEYCHAIN_SERVICE, "-a", _keychain_account(),
             "-w", key_hex, "-U", "-T", "/usr/bin/" + keychain],
            capture_output=True, text=True)
        if added.returncode != 0:
            raise IndexError_("could not store the fingerprint key in the Keychain")
    try:
        fd = os.open(cache, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
        with os.fdopen(fd, "w") as handle:
            handle.write(key_hex)
    except OSError:
        pass  # the cache is an optimisation only
    return bytes.fromhex(key_hex)


def _fp(key: bytes, text: str) -> str:
    return hashlib.blake2b(text.encode("utf-8", "surrogatepass"), key=key, digest_size=16).hexdigest()


def variants(value: str) -> set[str]:
    """Spellings in which a value commonly reaches output."""
    import urllib.parse  # noqa: PLC0415

    out = {value}
    escaped = json.dumps(value)[1:-1]
    out.add(escaped)
    out.add(urllib.parse.quote(value, safe=""))
    out.add(urllib.parse.quote_plus(value, safe=""))
    return {v for v in out if MIN_LEN <= len(v) <= MAX_LEN}


def _candidate_starts(text: str):
    """Positions where a secret could plausibly begin.

    A run of letters/digits starting, or any non-alphanumeric, non-space
    character. Secrets embedded mid-word (``abcSECRET``) are not found; that
    is the accepted trade-off for speed.
    """
    prev_alnum = False
    for i, ch in enumerate(text):
        alnum = ch.isalnum()
        if (alnum and not prev_alnum) or (not alnum and not ch.isspace()):
            yield i
        prev_alnum = alnum


class Index:
    def __init__(self, key: bytes, entries: dict[str, list[list]]):
        self.key = key
        self.entries = entries  # prefix fp -> [[length, full fp, label], ...]

    @classmethod
    def load(cls, path: Path | None = None, key: bytes | None = None) -> "Index | None":
        """The index, or None when none has been built yet."""
        path = path or index_path()
        try:
            data = json.loads(path.read_text())
        except FileNotFoundError:
            return None
        except (OSError, ValueError) as error:
            raise IndexError_(f"unreadable index: {type(error).__name__}") from None
        if data.get("version") != VERSION or data.get("prefix_len") != PREFIX_LEN:
            raise IndexError_("index format mismatch; run `secret-index refresh`")
        return cls(key if key is not None else read_key(), data["entries"])

    def __len__(self) -> int:
        return sum(len(v) for v in self.entries.values())

    def find(self, text: str) -> list[tuple[int, int, str]]:
        """Non-overlapping (start, end, label) spans of known secrets."""
        if not self.entries or len(text) < MIN_LEN:
            return []
        spans: list[tuple[int, int, str]] = []
        next_free = 0
        n = len(text)
        for i in _candidate_starts(text):
            if i < next_free or i + MIN_LEN > n:
                continue
            hits = self.entries.get(_fp(self.key, text[i:i + PREFIX_LEN]))
            if not hits:
                continue
            for length, full, label in sorted(hits, key=lambda h: -h[0]):
                if i + length <= n and hmac.compare_digest(_fp(self.key, text[i:i + length]), full):
                    spans.append((i, i + length, label))
                    next_free = i + length
                    break
        return spans

    def mask(self, text: str) -> tuple[str, int]:
        spans = self.find(text)
        if not spans:
            return text, 0
        parts, last = [], 0
        for start, end, _label in spans:
            parts.append(text[last:start])
            parts.append(REDACTION)
            last = end
        parts.append(text[last:])
        return "".join(parts), len(spans)


@__import__("functools").lru_cache(maxsize=1)
def repository_names() -> frozenset[str]:
    """owner/repo slugs and names of local clones' remotes: public, and common
    in commands and output."""
    import re  # noqa: PLC0415

    names: set[str] = set()
    roots = [Path.home() / "repos", Path.home() / "repos/epoch", Path.home() / "My Drive/dotfiles"]
    for root in roots:
        candidates = [root] if (root / ".git").exists() else list(root.glob("*")) if root.is_dir() else []
        for repo in candidates:
            if not (repo / ".git").exists():
                continue
            found = subprocess.run(["git", "-C", str(repo), "remote", "-v"], capture_output=True, text=True)
            for url in re.findall(r"\s(\S+)\s\(fetch\)", found.stdout):
                m = re.search(r"[:/]([^/:]+/[^/]+?)(?:\.git)?$", url)
                if m:
                    names.update({m.group(1), m.group(1).split("/")[1]})
    # Repositories of the accounts and organisations gh is signed in to,
    # including ones not cloned here (read-only API; skipped when gh fails).
    owners = subprocess.run(["gh", "api", "user/orgs", "--jq", ".[].login"], capture_output=True, text=True)
    me = subprocess.run(["gh", "api", "user", "--jq", ".login"], capture_output=True, text=True)
    for owner in set(owners.stdout.split()) | set(me.stdout.split()):
        listed = subprocess.run(["gh", "repo", "list", owner, "--limit", "2000", "--json", "nameWithOwner",
                                 "--jq", ".[].nameWithOwner"], capture_output=True, text=True, timeout=120)
        for slug in listed.stdout.split():
            names.update({slug, slug.split("/")[-1]})
    return frozenset(names)


def public_strings() -> set[str]:
    """Identifiers that appear in ordinary output and must never count as secrets.

    2026-10-01: a 1Password item held the macOS username as a concealed field,
    so every command naming a home-directory path was denied and every path in
    output masked. These are excluded at build time and pruned by `prune`.
    """
    import socket  # noqa: PLC0415

    home = Path.home()
    out = {os.environ.get("USER", ""), str(home), home.name, socket.gethostname(),
           socket.gethostname().split(".")[0]}
    out.update(part for part in home.parts if part != "/")
    out.update(repository_names())
    for key in ("user.name", "user.email"):
        found = subprocess.run(["git", "config", "--global", key], capture_output=True, text=True)
        value = found.stdout.strip()
        if value:
            out.update({value, value.split("@")[0]})
    return {v for v in out if v}


EMAIL = __import__("re").compile(r"^[^@\s]+@[^@\s]+\.[A-Za-z]{2,}$")


IDENTIFIER = __import__("re").compile(r"^[A-Za-z .@_-]{1,23}$")
# owner/repo slugs, host:path and other digit-free names (2026-10-01: a
# 1Password field held a GitHub owner/repo slug).
NAME_PATH = __import__("re").compile(r"^[A-Za-z][A-Za-z._-]*([/:][A-Za-z._-]+)+$")


def plausible_secret(value: str) -> bool:
    """Shape rule: email addresses, and digit-free values under 24 characters
    made of letters, spaces and ``.@_-`` (usernames, names, domain names), are
    identifiers that fill ordinary output, not secrets worth masking.

    2026-10-01: before this rule, items whose concealed fields held a domain
    name, an author name or an email address matched 2,400 times in commit
    messages alone.
    """
    v = value.strip()
    return not (EMAIL.match(v) or IDENTIFIER.match(v) or (len(v) <= 64 and NAME_PATH.match(v)))


def build_entries(key: bytes, values: list[tuple[str, str]]) -> dict[str, list[list]]:
    entries: dict[str, list[list]] = {}
    seen: set[str] = set()
    public = public_strings()
    for value, label in values:
        if value.strip() in public or not plausible_secret(value):
            continue
        for v in variants(value.strip()):
            full = _fp(key, v)
            if full in seen:
                continue
            seen.add(full)
            entries.setdefault(_fp(key, v[:PREFIX_LEN]), []).append([len(v), full, label])
    return entries


def write(entries: dict[str, list[list]], sources: dict[str, int], path: Path | None = None) -> Path:
    import tempfile  # noqa: PLC0415
    import time  # noqa: PLC0415

    path = path or index_path()
    path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    os.chmod(path.parent, 0o700)
    data = {"version": VERSION, "prefix_len": PREFIX_LEN, "min_len": MIN_LEN,
            "refreshed": time.strftime("%Y-%m-%dT%H:%M:%S%z"), "sources": sources, "entries": entries}
    fd, tmp = tempfile.mkstemp(dir=path.parent, prefix=".index-")
    with os.fdopen(fd, "w") as handle:
        json.dump(data, handle)
    os.chmod(tmp, 0o600)
    os.replace(tmp, path)
    return path


def prune(path: Path | None = None) -> int:
    """Remove fingerprints of public identifiers from an existing index."""
    path = path or index_path()
    key = read_key()
    data = json.loads(path.read_text())
    doomed = {_fp(key, v) for p in public_strings() for v in variants(p) | {p}}
    removed = 0
    for prefix in list(data["entries"]):
        kept = [hit for hit in data["entries"][prefix] if hit[1] not in doomed]
        removed += len(data["entries"][prefix]) - len(kept)
        if kept:
            data["entries"][prefix] = kept
        else:
            del data["entries"][prefix]
    write(data["entries"], data.get("sources", {}), path)
    return removed


def add(value: str, label: str, path: Path | None = None) -> int:
    """Add one value to the existing index (creating key and index if needed)."""
    path = path or index_path()
    key = read_key(create=True)
    try:
        data = json.loads(path.read_text())
    except FileNotFoundError:
        data = {"entries": {}, "sources": {}}
    entries = data["entries"]
    known = {full for hits in entries.values() for _, full, _ in hits}
    added = 0
    for prefix, hits in build_entries(key, [(value, label)]).items():
        for hit in hits:
            if hit[1] not in known:
                entries.setdefault(prefix, []).append(hit)
                added += 1
    sources = data.get("sources", {})
    sources["added"] = sources.get("added", 0) + (1 if added else 0)
    write(entries, sources, path)
    return added


def mask_text(text: str) -> tuple[str, int]:
    """Mask with the installed index; unchanged when no index exists yet.

    Raises IndexError_ when an index exists but cannot be used, so callers
    can fail closed.
    """
    index = Index.load()
    if index is None:
        return text, 0
    return index.mask(text)
