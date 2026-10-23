"""Fresh Slack browser-session credentials, read from Pablo's Chrome.

slack.py borrows Pablo's browser login (the `xoxc-` token and the `d` cookie),
which Slack invalidates whenever that browser session ends. Copying them by
hand after each reset was the recurring chore this module removes: when Slack
rejects the stored credentials, slack.py asks it for the current ones.

- The `d` cookie comes from each Chrome profile's cookie database
  (`~/Library/Application Support/Google/Chrome/<profile>/Cookies`, read with
  SQLite's immutable flag, so Chrome's lock and files are untouched). Chrome
  encrypts it with AES-128-CBC under a key derived (PBKDF2-SHA1, salt
  "saltysalt", 1003 rounds) from the "Chrome Safe Storage" Keychain item. The
  first read makes macOS ask Pablo to allow Keychain access; "Always Allow"
  makes later reads unattended.
- The `xoxc-` token follows from the cookie: the workspace's `/ssb/redirect`
  page, fetched with that cookie, embeds it (`"api_token":"xoxc-…"`).
- The pair is checked with `auth.test` (the team's domain must match) and
  cached in ~/.cache/slack-chrome/<domain>.json (0600), so the Keychain and
  Chrome are read only when Slack rejects the cached pair.

Values are never printed. Profiles are discovered, not configured: every
profile with a slack.com `d` cookie is tried until one is signed in to the
requested workspace.
"""

import hashlib
import json
import os
import re
import sqlite3
import subprocess
import urllib.parse
import urllib.request
from pathlib import Path

CHROME = Path.home() / "Library/Application Support/Google/Chrome"
CACHE = Path.home() / ".cache/slack-chrome"
TIMEOUT = 20
_key = None


class Unavailable(Exception):
    """No Chrome profile yields working credentials for the workspace."""


def _safe_storage_key() -> bytes:
    global _key
    if _key is None:
        out = subprocess.run(
            ["/usr/bin/security", "find-generic-password", "-w",
             "-s", "Chrome Safe Storage", "-a", "Chrome"],
            capture_output=True, text=True, timeout=120)
        if out.returncode != 0 or not out.stdout.strip():
            raise Unavailable("Keychain access to Chrome Safe Storage was refused")
        _key = hashlib.pbkdf2_hmac("sha1", out.stdout.strip().encode(), b"saltysalt",
                                   1003, 16)
    return _key


def _decrypt(encrypted: bytes, host: str, db_version: int) -> str:
    from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
    if not encrypted.startswith(b"v10"):
        raise Unavailable("unsupported Chrome cookie encryption")
    dec = Cipher(algorithms.AES(_safe_storage_key()), modes.CBC(b" " * 16)).decryptor()
    plain = dec.update(encrypted[3:]) + dec.finalize()
    plain = plain[:-plain[-1]]  # PKCS#7 padding
    if db_version >= 24:  # Chrome prefixes SHA-256(host_key) since DB version 24
        if plain[:32] != hashlib.sha256(host.encode()).digest():
            raise Unavailable("Chrome cookie integrity check failed")
        plain = plain[32:]
    return plain.decode()


def _profile_cookies() -> list[tuple[str, str]]:
    """Return (profile, d cookie) for every Chrome profile signed in to Slack."""
    found = []
    for cookies in sorted(CHROME.glob("*/Cookies")):
        try:
            db = sqlite3.connect(f"file:{cookies}?immutable=1", uri=True)
            version = int((db.execute("SELECT value FROM meta WHERE key='version'")
                           .fetchone() or [0])[0])
            rows = db.execute("SELECT host_key, encrypted_value FROM cookies "
                              "WHERE name='d' AND host_key LIKE '%slack.com'").fetchall()
            db.close()
        except sqlite3.Error:
            continue
        for host, enc in rows:
            try:
                value = _decrypt(enc, host, version)
            except (Unavailable, ValueError, UnicodeDecodeError):
                continue
            if value.startswith("xoxd-"):
                found.append((cookies.parent.name, value))
    return found


def _request(url, cookie, data=None, token=None):
    headers = {"Cookie": f"d={urllib.parse.quote(cookie, safe='%')}",
               "User-Agent": "Mozilla/5.0 (Macintosh) slack.py"}
    if token:
        headers["Authorization"] = f"Bearer {token}"
    body = urllib.parse.urlencode(data).encode() if data is not None else None
    req = urllib.request.Request(url, data=body, headers=headers)
    with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
        return resp.read().decode("utf-8", "replace")


def _token_for(domain: str, cookie: str) -> str | None:
    try:
        # The workspace root no longer embeds it (2026-10-24); /ssb/redirect,
        # Slack's desktop-app handoff page, does.
        page = _request(f"https://{domain}/ssb/redirect", cookie)
    except OSError:
        return None
    match = re.search(r'"api_token":"(xoxc-[0-9A-Za-z-]+)"', page)
    return match.group(1) if match else None


def _verify(domain: str, token: str, cookie: str) -> bool:
    try:
        out = json.loads(_request("https://slack.com/api/auth.test", cookie, {}, token))
    except (OSError, ValueError):
        return False
    return out.get("ok") is True and out.get("url", "").rstrip("/").endswith(domain)


def _cache_path(domain: str) -> Path:
    return CACHE / f"{domain}.json"


def cached(domain: str) -> tuple[str, str] | None:
    try:
        data = json.loads(_cache_path(domain).read_text())
        return data["token"], data["cookie"]
    except (OSError, ValueError, KeyError):
        return None


def refresh(domain: str) -> tuple[str, str]:
    """Read current credentials for DOMAIN from Chrome, verify, cache, return them."""
    profiles = _profile_cookies()
    if not profiles:
        raise Unavailable("no Chrome profile has a Slack session cookie")
    for profile, cookie in profiles:
        token = _token_for(domain, cookie)
        if token and _verify(domain, token, cookie):
            CACHE.mkdir(parents=True, exist_ok=True, mode=0o700)
            path = _cache_path(domain)
            tmp = path.with_suffix(".tmp")
            fd = os.open(tmp, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
            with os.fdopen(fd, "w") as fh:
                json.dump({"token": token, "cookie": cookie, "profile": profile}, fh)
            os.replace(tmp, path)
            return token, cookie
    raise Unavailable(f"no Chrome profile is signed in to {domain}; sign in to "
                      "Slack in Chrome and run the command again")
