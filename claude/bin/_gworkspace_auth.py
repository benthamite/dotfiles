"""Shared OAuth helper for the gmail.py and sheets.py wrappers.

Each Google Workspace account has its own refresh token. Both accounts share
the same OAuth client (client_id, client_secret), differing only in which
account the refresh token authenticates as. Env vars (set via .zshenv-secrets):

    GOOGLE_WORKSPACE_CLIENT_ID                       (shared)
    GOOGLE_WORKSPACE_CLIENT_SECRET                   (shared)
    GOOGLE_WORKSPACE_REFRESH_TOKEN                   (account=epoch)
    GOOGLE_WORKSPACE_REFRESH_TOKEN_PERSONAL          (account=personal)

Access tokens are cached at /tmp/gworkspace-access-token-<account>.json for
the rest of their lifetime so back-to-back calls don't refresh on every
invocation.
"""

import json
import os
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

TOKEN_URL = "https://oauth2.googleapis.com/token"

# account name -> env var holding that account's refresh token
REFRESH_TOKEN_ENV = {
    "epoch": "GOOGLE_WORKSPACE_REFRESH_TOKEN",
    "personal": "GOOGLE_WORKSPACE_REFRESH_TOKEN_PERSONAL",
}


def _cache_path(account):
    return f"/tmp/gworkspace-access-token-{account}.json"


def _read_env(account):
    if account not in REFRESH_TOKEN_ENV:
        sys.stderr.write(
            f"ERROR: unknown account {account!r}; valid: {sorted(REFRESH_TOKEN_ENV)}\n"
        )
        sys.exit(1)
    try:
        return (
            os.environ["GOOGLE_WORKSPACE_CLIENT_ID"],
            os.environ["GOOGLE_WORKSPACE_CLIENT_SECRET"],
            os.environ[REFRESH_TOKEN_ENV[account]],
        )
    except KeyError as e:
        sys.stderr.write(f"ERROR: missing env var {e} (account={account})\n")
        sys.exit(1)


def _refresh_access_token(account):
    client_id, client_secret, refresh_token = _read_env(account)
    data = urllib.parse.urlencode(
        {
            "client_id": client_id,
            "client_secret": client_secret,
            "refresh_token": refresh_token,
            "grant_type": "refresh_token",
        }
    ).encode()
    try:
        resp = urllib.request.urlopen(urllib.request.Request(TOKEN_URL, data=data))
    except urllib.error.HTTPError as e:
        sys.stderr.write(
            f"ERROR: token refresh failed (account={account}): {e.read().decode()}\n"
        )
        sys.exit(1)
    body = json.loads(resp.read())
    return {
        "token": body["access_token"],
        "expires_at": time.time() + body["expires_in"] - 60,
    }


def get_access_token(account):
    """Return a valid access token for `account`, refreshing only when stale."""
    cache_path = _cache_path(account)
    if os.path.exists(cache_path):
        try:
            with open(cache_path) as f:
                cached = json.load(f)
            if cached.get("expires_at", 0) > time.time():
                return cached["token"]
        except (json.JSONDecodeError, KeyError):
            pass
    fresh = _refresh_access_token(account)
    fd = os.open(cache_path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w") as f:
        json.dump(fresh, f)
    return fresh["token"]


def api_request(method, url, body=None, headers=None, account="epoch"):
    """Make an authenticated request as `account`, return parsed JSON. Exits on HTTP error."""
    token = get_access_token(account)
    req_headers = {"Authorization": f"Bearer {token}"}
    if body is not None:
        req_headers["Content-Type"] = "application/json"
        body = json.dumps(body).encode()
    if headers:
        req_headers.update(headers)
    req = urllib.request.Request(url, data=body, method=method, headers=req_headers)
    try:
        resp = urllib.request.urlopen(req)
    except urllib.error.HTTPError as e:
        sys.stderr.write(
            f"ERROR: {method} {url} (account={account})\n  {e.read().decode()}\n"
        )
        sys.exit(1)
    raw = resp.read()
    return json.loads(raw) if raw else {}
