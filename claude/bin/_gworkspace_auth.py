"""Shared OAuth helper for the gmail.py and sheets.py wrappers.

Exchanges the Epoch Google Workspace refresh token for a short-lived access
token and exposes a single function `get_access_token()`. Credentials come
from these env vars (set via ~/.zshenv-secrets):

    GOOGLE_WORKSPACE_CLIENT_ID
    GOOGLE_WORKSPACE_CLIENT_SECRET
    GOOGLE_WORKSPACE_REFRESH_TOKEN

Tokens are cached in /tmp for the rest of their lifetime so back-to-back
script invocations don't refresh on every call.
"""

import json
import os
import sys
import time
import urllib.parse
import urllib.request

CACHE_PATH = "/tmp/gworkspace-access-token.json"
TOKEN_URL = "https://oauth2.googleapis.com/token"


def _read_env():
    try:
        return (
            os.environ["GOOGLE_WORKSPACE_CLIENT_ID"],
            os.environ["GOOGLE_WORKSPACE_CLIENT_SECRET"],
            os.environ["GOOGLE_WORKSPACE_REFRESH_TOKEN"],
        )
    except KeyError as e:
        sys.stderr.write(f"ERROR: missing env var {e}\n")
        sys.exit(1)


def _refresh_access_token():
    client_id, client_secret, refresh_token = _read_env()
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
        sys.stderr.write(f"ERROR: token refresh failed: {e.read().decode()}\n")
        sys.exit(1)
    body = json.loads(resp.read())
    return {"token": body["access_token"], "expires_at": time.time() + body["expires_in"] - 60}


def get_access_token():
    """Return a valid access token, refreshing only when the cached one is stale."""
    if os.path.exists(CACHE_PATH):
        try:
            with open(CACHE_PATH) as f:
                cached = json.load(f)
            if cached.get("expires_at", 0) > time.time():
                return cached["token"]
        except (json.JSONDecodeError, KeyError):
            pass
    fresh = _refresh_access_token()
    os.makedirs(os.path.dirname(CACHE_PATH), exist_ok=True)
    fd = os.open(CACHE_PATH, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w") as f:
        json.dump(fresh, f)
    return fresh["token"]


def api_request(method, url, body=None, headers=None):
    """Make an authenticated request, return parsed JSON. Exits on HTTP error."""
    token = get_access_token()
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
        sys.stderr.write(f"ERROR: {method} {url}\n  {e.read().decode()}\n")
        sys.exit(1)
    raw = resp.read()
    return json.loads(raw) if raw else {}
