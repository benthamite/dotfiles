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


def build_entries(key: bytes, values: list[tuple[str, str]]) -> dict[str, list[list]]:
    entries: dict[str, list[list]] = {}
    seen: set[str] = set()
    for value, label in values:
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
