#!/usr/bin/env python3
"""slack.py — thin Slack web-API wrapper using xoxc/xoxd browser-session tokens.

Replaces the slack-unofficial-epochai MCP server. Tokens are scraped from a
logged-in browser session and impersonate the user account, so
mark/unread/saved-message operations work as your account sees them (unlike a
bot OAuth token).

Programmatic callers may use call_notifier() for an explicitly authorized
Epoch Notifier private text message; it never changes the user CLI's sender.

Multiple workspaces are supported via the WORKSPACES registry and the global
`-w/--workspace` flag (or the SLACK_WORKSPACE env var; default: epoch):

  epoch       -> 1Password: op://Automations/Slack MCP - Epoch Unofficial
                 (.../xoxc_token -> Bearer token, .../xoxd_token -> 'd' cookie)
  trajectory  -> pass entry trajectory/slack.com/trajectorylabs
                 (token: field -> Bearer token, cookie: field -> 'd' cookie)
  altruismo-eficaz -> pass entry chrome/slack.com/altruismo-eficaz
                 (token: field -> Bearer token, cookie: field -> 'd' cookie)

Usage:
  slack.py [-w WORKSPACE] <subcommand> ...
  slack.py search '<query>' [--max=N] [--page=N] [--sort=score|timestamp]
      Fetch one search.messages page; max and page must each be 1..100.
      UI filters and collapsed nearby matches mean totals are not a full inventory.
  slack.py history <channel-id> [--limit=N] [--oldest=ts] [--latest=ts] [--cursor=...]
      Fetch a channel's recent messages (conversations.history).
  slack.py replies <channel-id> <thread-ts> [--limit=N] [--cursor=...] [--no-resolve-users]
      Fetch replies in a thread (conversations.replies), resolving user IDs by default.
  slack.py permalink <url> [--limit=N] [--cursor=...] [--no-resolve-users]
      Fetch an exact target plus one context page, with explicit coverage.
      Workspace URLs are bound to the selected account; workspaces without a
      configured domain require an extra auth.test identity read.
  slack.py channels [--types=public_channel,private_channel,im,mpim] [--cursor=...]
      List channels (conversations.list).
  slack.py users-search <query> [--max=N]
      Find users by name/handle (users.list + client-side filter).
  slack.py user-info <user-id>
      Fetch a single user's profile (users.info).
  slack.py mark <channel-id> <ts>
      Mark a channel as read up to <ts> (conversations.mark).
  slack.py unreads
      Per-channel unread counts via the internal client.counts endpoint.
  slack.py saved-list [--limit=N]
      Fetch one response from the internal saved.list endpoint.
  slack.py unsave <channel-id> <ts>
      Remove that exact message from Later using internal saved.delete.

Output: raw JSON from Slack on stdout. Non-zero exit on API error.
"""

import argparse
import http.client
import json
import os
import re
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request

API = "https://slack.com/api"
REQUEST_TIMEOUT = 30
_METHOD_RE = re.compile(r"[a-z][A-Za-z0-9_]*(?:\.[a-z][A-Za-z0-9_]*)+")
_SAFE_API_ERRORS = frozenset({
    "not_authed", "invalid_auth", "account_inactive", "token_revoked",
    "missing_scope", "channel_not_found", "not_in_channel", "ratelimited",
    "rate_limited", "restricted_action", "invalid_arguments", "invalid_arg_name",
    "method_not_supported_for_channel_type", "user_not_found", "access_denied",
})


def _fail(message, code=1):
    """Report caller-owned diagnostics; broker stderr only as one masked line."""
    sys.stderr.write(f"ERROR: {message}\n")
    raise SystemExit(code)


class _NoCredentialRedirects(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def _validate_credential_url(url, host):
    """Reject an untrusted origin before acquiring any account credentials."""
    if not isinstance(url, str) or url != url.strip() or "\\" in url:
        _fail("invalid Slack request URL", 2)
    if any(ord(character) < 32 or ord(character) == 127 for character in url):
        _fail("invalid Slack request URL", 2)
    try:
        parsed = urllib.parse.urlsplit(url)
        valid = (
            parsed.scheme == "https"
            and parsed.netloc in (host, f"{host}:443")
            and parsed.hostname == host
            and parsed.port in (None, 443)
            and parsed.username is None
            and parsed.password is None
            and not parsed.fragment
        )
    except ValueError:
        valid = False
    if not valid:
        _fail("refusing credentials for an untrusted Slack request URL", 2)


def _open_authenticated(request):
    """No redirects or retries: an uncertain write must be reconciled first."""
    try:
        opener = urllib.request.build_opener(_NoCredentialRedirects())
        return opener.open(request, timeout=REQUEST_TIMEOUT)
    except urllib.error.HTTPError as error:
        status = error.code
        error.close()
        _fail(f"Slack request failed with HTTP {status}")
    except (OSError, ValueError, http.client.HTTPException):
        _fail("Slack request failed before a response was received")

# Workspace registry. Each entry describes where this workspace's xoxc/xoxd
# session tokens come from. Two source kinds:
#   "op"   -> 1Password: read <op_path>/xoxc_token and <op_path>/xoxd_token.
#   "pass" -> pass entry with named fields <token_field> (xoxc) and
#             <cookie_field> (xoxd), e.g. the org-prefixed slack.com entries.
WORKSPACES = {
    "epoch": {
        "domain": "epochai.slack.com",
        "source": "op",
        "op_path": "op://Automations/Slack MCP - Epoch Unofficial",
    },
    "trajectory": {
        "source": "pass",
        "pass_entry": "trajectory/slack.com/trajectorylabs",
        "token_field": "token",
        "cookie_field": "cookie",
    },
    "altruismo-eficaz": {
        "source": "pass",
        "pass_entry": "chrome/slack.com/altruismo-eficaz",
        "token_field": "token",
        "cookie_field": "cookie",
    },
}
DEFAULT_WORKSPACE = "epoch"

# Selected workspace key, set from --workspace / SLACK_WORKSPACE in main().
_workspace = DEFAULT_WORKSPACE


def _pass_show(entry):
    try:
        out = subprocess.run(
            ["pass", "show", entry], check=False, capture_output=True,
            text=True, timeout=REQUEST_TIMEOUT,
        )
    except (OSError, UnicodeError, subprocess.TimeoutExpired):
        _fail("personal Slack credential lookup failed")
    if out.returncode == 0 and out.stdout:
        return out.stdout.splitlines()[0]
    return ""


def _broker_hint(stderr):
    """One line of broker stderr with token-shaped runs masked.

    `op read` errors name the reference and the fault ("does not have a field
    'token'", "could not connect"), never the value; the mask covers the
    unexpected case anyway. Without this, the 2026-09-10 failure — one cold
    lookup that a retry cleared — left nothing to diagnose.
    """
    for line in (stderr or "").splitlines():
        line = line.strip()
        if line:
            return re.sub(r"[A-Za-z0-9_+/=-]{20,}", "…", line)[:200]
    return "no diagnostic output"


def _op_read(path):
    if not path.startswith("op://Automations/"):
        _fail("Slack browser-session credentials must use the Automations broker", 2)
    try:
        out = subprocess.run(
            ["op-automations", "read", path], check=False, capture_output=True,
            text=True, timeout=REQUEST_TIMEOUT,
        )
    except subprocess.TimeoutExpired:
        _fail(f"Slack credential broker lookup timed out after {REQUEST_TIMEOUT}s")
    except (OSError, UnicodeError) as exc:
        _fail(f"Slack credential broker could not run: {type(exc).__name__}")
    if out.returncode != 0:
        _fail(
            f"Slack credential broker lookup failed (exit {out.returncode}): "
            f"{_broker_hint(out.stderr)}"
        )
    value = out.stdout.strip()
    if not value or any(character.isspace() for character in value):
        _fail("Slack credential broker returned an invalid credential")
    return value


def _pass_field(entry, field):
    """Return the value of a named field (e.g. "token: <value>") from a pass entry."""
    try:
        out = subprocess.run(
            ["pass", "show", entry], check=False, capture_output=True,
            text=True, timeout=REQUEST_TIMEOUT,
        )
    except (OSError, UnicodeError, subprocess.TimeoutExpired):
        _fail("personal Slack credential lookup failed")
    if out.returncode != 0:
        _fail("personal Slack credential lookup failed")
    prefix = f"{field}:"
    for line in out.stdout.splitlines():
        if line.startswith(prefix):
            return line[len(prefix):].strip()
    sys.stderr.write(f"ERROR: pass entry {entry} has no '{field}:' field\n")
    sys.exit(1)


_xoxc = None
_xoxd = None
_token_workspace = None


def _tokens():
    global _xoxc, _xoxd, _token_workspace
    if _xoxc is None or _token_workspace != _workspace:
        ws = WORKSPACES.get(_workspace)
        if ws is None:
            valid = ", ".join(sorted(WORKSPACES))
            sys.stderr.write(
                f"ERROR: unknown workspace '{_workspace}'. Valid: {valid}\n"
            )
            sys.exit(2)
        if ws["source"] == "op":
            xoxc = _op_read(f"{ws['op_path']}/xoxc_token")
            xoxd = _op_read(f"{ws['op_path']}/xoxd_token")
        elif ws["source"] == "pass":
            xoxc = _pass_field(ws["pass_entry"], ws["token_field"])
            xoxd = _pass_field(ws["pass_entry"], ws["cookie_field"])
        else:
            sys.stderr.write(
                f"ERROR: workspace '{_workspace}' has invalid source '{ws['source']}'\n"
            )
            sys.exit(2)
        if any(not value or any(c.isspace() for c in value) for value in (xoxc, xoxd)):
            _fail("Slack credential source returned an invalid credential")
        _xoxc, _xoxd = xoxc, xoxd
        _token_workspace = _workspace
        _user_cache.clear()
    return _xoxc, _xoxd


def _api_url(method):
    """Validate the exact API destination before acquiring credentials."""
    if not isinstance(method, str) or not _METHOD_RE.fullmatch(method):
        _fail("invalid Slack API method", 2)
    url = f"{API}/{method}"
    _validate_credential_url(url, "slack.com")
    if urllib.parse.urlsplit(url).path != f"/api/{method}" or urllib.parse.urlsplit(url).query:
        _fail("invalid Slack API base URL", 2)
    return url


def _post_form(url, params, headers):
    """Make one authenticated request and return only a successful JSON object."""
    body = urllib.parse.urlencode({k: v for k, v in params.items() if v is not None}).encode()
    req = urllib.request.Request(
        url,
        data=body,
        method="POST",
        headers={
            **headers,
            "Content-Type": "application/x-www-form-urlencoded; charset=utf-8",
        },
    )
    try:
        with _open_authenticated(req) as response:
            data = json.loads(response.read())
    except (OSError, ValueError, http.client.HTTPException):
        _fail("Slack returned an unreadable JSON response")
    if not isinstance(data, dict):
        _fail("Slack returned an invalid response object")
    if data.get("ok") is not True:
        error = data.get("error")
        suffix = f": {error}" if isinstance(error, str) and error in _SAFE_API_ERRORS else ""
        _fail(f"Slack API rejected the request{suffix}")
    return data


def call(method, **params):
    """POST to slack.com/api/<method> using the selected browser-session user."""
    url = _api_url(method)
    xoxc, xoxd = _tokens()
    return _post_form(url, params, {
        "Authorization": f"Bearer {xoxc}", "Cookie": f"d={xoxd}",
    })


def call_notifier(method, **params):
    """Authenticate Epoch Notifier or send one authorized private text message.

    The caller must verify bot identity and returned channel/ts/text. A transport
    failure can mean delivery is uncertain; reconcile it before retrying. This
    function never retries or falls back to another credential or sender.
    """
    if method not in ("auth.test", "chat.postMessage"):
        _fail("Epoch Notifier method is not allowed", 2)
    url = _api_url(method)
    if method == "auth.test":
        if params:
            _fail("Epoch Notifier auth.test accepts no parameters", 2)
    else:
        allowed = {
            "channel", "text", "unfurl_links", "unfurl_media", "mrkdwn",
            "parse", "link_names", "client_msg_id",
        }
        if set(params) - allowed:
            _fail("Epoch Notifier message has unsupported parameters", 2)
        recipient = params.get("channel")
        if not isinstance(recipient, str) or not re.fullmatch(r"[UW][A-Z0-9]+", recipient):
            _fail("Epoch Notifier requires one exact Slack user ID", 2)
        text = params.get("text")
        if not isinstance(text, str) or not text.strip():
            _fail("Epoch Notifier requires nonblank message text", 2)
    encoded_params = {
        key: str(value).lower() if isinstance(value, bool) else value
        for key, value in params.items()
    }
    token = _op_read("op://Automations/Slack - Epoch Notifier/credential")
    return _post_form(url, encoded_params, {"Authorization": f"Bearer {token}"})


def _permalink_ts_to_ts(permalink_ts):
    if not re.fullmatch(r"[0-9]{7,}", permalink_ts):
        raise ValueError("invalid Slack permalink timestamp")
    return f"{permalink_ts[:-6]}.{permalink_ts[-6:]}"


def parse_permalink(url):
    """Parse a canonical URL; bind configured domains without accessing services."""
    if not isinstance(url, str) or any(ord(c) <= 32 or ord(c) == 127 for c in url) or "\\" in url or "#" in url:
        raise ValueError("invalid Slack permalink URL")
    parsed = urllib.parse.urlsplit(url)
    if parsed.scheme != "https" or not re.fullmatch(r"[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.slack\.com", parsed.netloc):
        raise ValueError("Slack permalink requires a canonical HTTPS workspace URL")
    workspace = WORKSPACES.get(_workspace)
    if workspace is None:
        raise ValueError("unknown selected Slack workspace")
    if workspace.get("domain") and parsed.netloc != workspace["domain"]:
        raise ValueError("Slack permalink belongs to a different workspace")
    match = re.fullmatch(r"/archives/([CGD][A-Z0-9]{8,})/p([0-9]{7,})", parsed.path)
    if not match:
        raise ValueError("invalid Slack message permalink path")
    channel, timestamp = match.groups()
    message_ts = _permalink_ts_to_ts(timestamp)
    try:
        query = urllib.parse.parse_qs(parsed.query, keep_blank_values=True, strict_parsing=True)
    except ValueError:
        raise ValueError("malformed Slack permalink query") from None
    if set(query) - {"thread_ts", "cid"} or any(len(values) != 1 for values in query.values()):
        raise ValueError("invalid or duplicate Slack permalink query parameter")
    thread_ts = (query.get("thread_ts") or [None])[0]
    cid = (query.get("cid") or [None])[0]
    if thread_ts is not None and not re.fullmatch(r"[0-9]+\.[0-9]{6}", thread_ts):
        raise ValueError("invalid Slack thread timestamp")
    if cid is not None and cid != channel:
        raise ValueError("Slack permalink channel query does not match its path")

    return {
        "url": url,
        "team_domain": parsed.netloc.removesuffix(".slack.com"),
        "channel": channel,
        "message_ts": message_ts,
        "thread_ts": thread_ts,
    }


def _verify_permalink_workspace(info):
    """Resolve an unconfigured domain through the selected account, never another."""
    if WORKSPACES[_workspace].get("domain"):
        return
    identity = call("auth.test")
    url = identity.get("url")
    if not isinstance(url, str):
        _fail("Slack account did not return a workspace URL")
    if not re.fullmatch(r"https://[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.slack\.com/?", url):
        _fail("Slack account returned an invalid workspace URL")
    if urllib.parse.urlsplit(url).netloc != info["team_domain"] + ".slack.com":
        _fail("Slack permalink belongs to a different selected workspace", 2)


def _has_replies(message):
    try:
        return int(message.get("reply_count") or 0) > 0
    except (TypeError, ValueError):
        return False


def _message_rows(out):
    messages = out.get("messages")
    if not isinstance(messages, list) or any(not isinstance(item, dict) for item in messages):
        _fail("Slack returned an invalid message collection")
    if any(not isinstance(item.get("ts"), str) or not re.fullmatch(r"[0-9]+\.[0-9]{6}", item["ts"]) for item in messages):
        _fail("Slack returned an invalid message timestamp")
    return messages


def _exact_message(out, timestamp):
    matches = [item for item in _message_rows(out) if item.get("ts") == timestamp]
    if len(matches) != 1:
        _fail("Requested Slack message was not found exactly; use a verified thread permalink for a reply")
    return matches[0]


def _require_thread_membership(message, thread_ts):
    if message.get("ts") == thread_ts:
        if message.get("thread_ts", thread_ts) != thread_ts:
            _fail("Slack message does not belong to the requested thread")
    elif message.get("thread_ts") != thread_ts:
        _fail("Slack message does not belong to the requested thread")


def _page_coverage(out, cursor):
    metadata = out.get("response_metadata", {})
    if not isinstance(metadata, dict):
        _fail("Slack returned invalid pagination metadata")
    next_cursor = metadata.get("next_cursor", "")
    if not isinstance(next_cursor, str):
        _fail("Slack returned an invalid pagination cursor")
    has_more = out.get("has_more", False)
    if type(has_more) is not bool:
        _fail("Slack returned an invalid pagination status")
    limited = out.get("is_limited", False)
    if type(limited) is not bool:
        _fail("Slack returned an invalid history limit status")
    more = has_more or bool(next_cursor)
    return {
        "starts_at_beginning": not bool(cursor),
        "has_more": more,
        "next_cursor": next_cursor,
        "is_limited": limited,
        "complete": not cursor and not more and not limited,
    }


_user_cache = {}
_USER_ID_RE = re.compile(r"^[UW][A-Z0-9]{8,}$")


def _user_display(user):
    profile = user.get("profile") or {}
    return (
        profile.get("display_name")
        or profile.get("real_name")
        or user.get("real_name")
        or user.get("name")
        or user.get("id")
    )


def _resolve_user(user_id):
    if not user_id or user_id in _user_cache:
        return _user_cache.get(user_id)
    out = call("users.info", user=user_id)
    user = out.get("user") or {}
    info = {
        "id": user.get("id") or user_id,
        "name": user.get("name"),
        "real_name": user.get("real_name"),
        "display_name": _user_display(user),
    }
    _user_cache[user_id] = info
    return info


def _collect_user_ids(value):
    ids = set()
    if isinstance(value, dict):
        user_id = value.get("user")
        if isinstance(user_id, str) and _USER_ID_RE.match(user_id):
            ids.add(user_id)
        for key in ("reply_users", "reactions"):
            nested = value.get(key)
            if isinstance(nested, list):
                for item in nested:
                    ids.update(_collect_user_ids(item))
        for nested in value.values():
            if isinstance(nested, (dict, list)):
                ids.update(_collect_user_ids(nested))
    elif isinstance(value, list):
        for item in value:
            ids.update(_collect_user_ids(item))
    elif isinstance(value, str) and _USER_ID_RE.match(value):
        ids.add(value)
    return ids


def _annotate_message_users(message, users_by_id):
    user_id = message.get("user")
    if user_id in users_by_id:
        message["user_profile_resolved"] = users_by_id[user_id]

    reply_users = message.get("reply_users")
    if isinstance(reply_users, list):
        message["reply_users_resolved"] = [
            users_by_id[user_id]
            for user_id in reply_users
            if user_id in users_by_id
        ]

    for reaction in message.get("reactions") or []:
        reaction_users = reaction.get("users")
        if isinstance(reaction_users, list):
            reaction["users_resolved"] = [
                users_by_id[user_id]
                for user_id in reaction_users
                if user_id in users_by_id
            ]


def resolve_users_in_response(out):
    user_ids = _collect_user_ids(out.get("messages") or [])
    if not user_ids:
        out["users_resolved"] = {}
        return out

    users_by_id = {}
    for user_id in sorted(user_ids):
        info = _resolve_user(user_id)
        if info:
            users_by_id[user_id] = info

    for message in out.get("messages") or []:
        _annotate_message_users(message, users_by_id)
    out["users_resolved"] = users_by_id
    return out


def cmd_search(args):
    for name in ("max", "page"):
        value = getattr(args, name)
        if type(value) is not int or not 1 <= value <= 100:
            _fail(f"Search --{name} must be an integer from 1 to 100")
    out = call("search.messages", query=args.query, count=str(args.max),
               page=str(args.page), sort=args.sort)
    print(json.dumps(out, indent=2))


def cmd_history(args):
    out = call(
        "conversations.history",
        channel=args.channel,
        limit=str(args.limit),
        oldest=args.oldest,
        latest=args.latest,
        cursor=args.cursor,
    )
    print(json.dumps(out, indent=2))


def cmd_replies(args):
    if type(args.limit) is not int or args.limit <= 0:
        _fail("Slack page limit must be a positive integer", 2)
    if not re.fullmatch(r"[CGD][A-Z0-9]{8,}", args.channel) or not re.fullmatch(r"[0-9]+\.[0-9]{6}", args.thread_ts):
        _fail("Invalid Slack thread channel or timestamp", 2)
    out = call(
        "conversations.replies",
        channel=args.channel,
        ts=args.thread_ts,
        limit=str(args.limit),
        cursor=args.cursor,
    )
    for message in _message_rows(out):
        _require_thread_membership(message, args.thread_ts)
    if not args.cursor:
        _exact_message(out, args.thread_ts)
    out["coverage"] = _page_coverage(out, args.cursor)
    if args.resolve_users:
        resolve_users_in_response(out)
    print(json.dumps(out, indent=2))


def cmd_permalink(args):
    if type(args.limit) is not int or args.limit <= 0:
        _fail("Slack page limit must be a positive integer", 2)
    try:
        info = parse_permalink(args.url)
    except ValueError as e:
        sys.stderr.write(f"ERROR: {e}\n")
        sys.exit(2)

    _verify_permalink_workspace(info)
    thread_ts = info["thread_ts"]
    if thread_ts:
        target_result = call(
            "conversations.replies",
            channel=info["channel"],
            ts=thread_ts,
            oldest=info["message_ts"],
            latest=info["message_ts"],
            inclusive="true",
            limit="1",
        )
        target = _exact_message(target_result, info["message_ts"])
        _require_thread_membership(target, thread_ts)
    else:
        target_result = call(
            "conversations.history",
            channel=info["channel"],
            oldest=info["message_ts"],
            latest=info["message_ts"],
            inclusive="true",
            limit="1",
        )
        target = _exact_message(target_result, info["message_ts"])
        thread_ts = target.get("thread_ts") or (target["ts"] if _has_replies(target) else None)

    target_coverage = _page_coverage(target_result, None)
    if thread_ts:
        if not isinstance(thread_ts, str) or not re.fullmatch(r"[0-9]+\.[0-9]{6}", thread_ts):
            _fail("Slack returned an invalid thread timestamp")
        _require_thread_membership(target, thread_ts)
        info["thread_ts"] = thread_ts
        out = call(
            "conversations.replies", channel=info["channel"], ts=thread_ts,
            limit=str(args.limit), cursor=args.cursor,
        )
        for message in _message_rows(out):
            _require_thread_membership(message, thread_ts)
        if not args.cursor and not any(message["ts"] == thread_ts for message in out["messages"]):
            _fail("Slack thread context is missing its parent message")
        out["source"] = "conversations.replies"
        out["coverage"] = _page_coverage(out, args.cursor)
    else:
        if args.cursor:
            _fail("A standalone Slack message has no thread cursor")
        out = {"ok": True, "messages": [target], "source": "conversations.history"}
        out["coverage"] = _page_coverage(out, None)

    if target_coverage["is_limited"]:
        out["coverage"]["is_limited"] = True
        out["coverage"]["complete"] = False

    out["permalink"] = info
    if args.resolve_users:
        resolve_users_in_response(out)
        resolve_users_in_response({"messages": [target]})
    out["target_message"] = target
    print(json.dumps(out, indent=2))


def cmd_channels(args):
    out = call(
        "conversations.list",
        types=args.types,
        cursor=args.cursor,
        limit="200",
        exclude_archived="true",
    )
    print(json.dumps(out, indent=2))


def cmd_users_search(args):
    """users.list + client-side filter. Slack doesn't expose a server-side users.search."""
    cursor = None
    matches = []
    needle = args.query.lower().lstrip("@")
    while True:
        out = call("users.list", limit="200", cursor=cursor)
        for u in out.get("members", []) or []:
            name = (u.get("name") or "").lower()
            real = (u.get("real_name") or "").lower()
            display = ((u.get("profile") or {}).get("display_name") or "").lower()
            if needle in name or needle in real or needle in display:
                matches.append(u)
                if len(matches) >= args.max:
                    break
        if len(matches) >= args.max:
            break
        cursor = (out.get("response_metadata") or {}).get("next_cursor")
        if not cursor:
            break
    print(json.dumps({"ok": True, "members": matches}, indent=2))


def cmd_user_info(args):
    out = call("users.info", user=args.user_id)
    print(json.dumps(out, indent=2))


def cmd_mark(args):
    out = call("conversations.mark", channel=args.channel, ts=args.ts)
    print(json.dumps(out, indent=2))


def cmd_unreads(args):
    """Internal client.counts endpoint — returns per-channel unread state.
    The unofficial slack-mcp-server's conversations_unreads tool wraps this."""
    out = call("client.counts")
    print(json.dumps(out, indent=2))


def cmd_saved_list(args):
    """List "Save for later" items (internal saved.list endpoint; stars.list
    is the legacy API and returns nothing for Later items)."""
    out = call("saved.list", limit=args.limit)
    print(json.dumps(out, indent=2))


def cmd_unsave(args):
    """Remove a "Save for later" item (internal saved.delete endpoint).
    For message items, item_id is the CHANNEL id and ts the message ts."""
    if not re.fullmatch(r"[CGD][A-Z0-9]{8,}", args.channel):
        _fail("Unsave requires an exact channel ID")
    if not re.fullmatch(r"[0-9]+\.[0-9]{6}", args.ts):
        _fail("Unsave requires an exact message timestamp")
    out = call("saved.delete", item_id=args.channel, item_type="message", ts=args.ts)
    print(json.dumps(out, indent=2))


def cmd_file(args):
    """Download a Slack-hosted file (files.slack.com url_private*) to a local path."""
    _validate_credential_url(args.url, "files.slack.com")
    xoxc, xoxd = _tokens()
    req = urllib.request.Request(
        args.url,
        headers={"Authorization": f"Bearer {xoxc}", "Cookie": f"d={xoxd}"},
    )
    try:
        with _open_authenticated(req) as response:
            data = response.read()
            ctype = response.headers.get("Content-Type", "")
    except (OSError, ValueError, http.client.HTTPException):
        _fail("Slack file response could not be read")
    if ctype.startswith("text/html"):
        sys.stderr.write("ERROR: got an HTML page instead of the file (auth or access problem)\n")
        sys.exit(1)
    with open(args.output, "wb") as fh:
        fh.write(data)
    print(json.dumps({"ok": True, "path": args.output, "bytes": len(data), "content_type": ctype}))


def main():
    p = argparse.ArgumentParser(description="Slack web-API wrapper (xoxc/xoxd auth)")
    p.add_argument(
        "-w",
        "--workspace",
        default=os.environ.get("SLACK_WORKSPACE", DEFAULT_WORKSPACE),
        help=(
            "Workspace key (default: %(default)s; or set SLACK_WORKSPACE). "
            f"Valid: {', '.join(sorted(WORKSPACES))}."
        ),
    )
    sub = p.add_subparsers(dest="cmd", required=True)

    s = sub.add_parser("search")
    s.add_argument("query")
    s.add_argument("--max", type=int, default=20, help="Results per page (1..100)")
    s.add_argument("--page", type=int, default=1, help="Explicit result page (1..100); no automatic sweep")
    s.add_argument("--sort", choices=["score", "timestamp"], default="timestamp")
    s.set_defaults(func=cmd_search)

    h = sub.add_parser("history")
    h.add_argument("channel")
    h.add_argument("--limit", type=int, default=50)
    h.add_argument("--oldest")
    h.add_argument("--latest")
    h.add_argument("--cursor")
    h.set_defaults(func=cmd_history)

    r = sub.add_parser("replies")
    r.add_argument("channel")
    r.add_argument("thread_ts")
    r.add_argument("--limit", type=int, default=200)
    r.add_argument("--cursor")
    r.add_argument(
        "--no-resolve-users",
        action="store_false",
        dest="resolve_users",
        help="Do not enrich message user IDs with Slack profile names.",
    )
    r.set_defaults(resolve_users=True)
    r.set_defaults(func=cmd_replies)

    pl = sub.add_parser("permalink")
    pl.add_argument("url")
    pl.add_argument("--limit", type=int, default=200)
    pl.add_argument("--cursor")
    pl.add_argument(
        "--no-resolve-users",
        action="store_false",
        dest="resolve_users",
        help="Do not enrich message user IDs with Slack profile names.",
    )
    pl.set_defaults(resolve_users=True)
    pl.set_defaults(func=cmd_permalink)

    c = sub.add_parser("channels")
    c.add_argument("--types", default="public_channel,private_channel,im,mpim")
    c.add_argument("--cursor")
    c.set_defaults(func=cmd_channels)

    us = sub.add_parser("users-search")
    us.add_argument("query")
    us.add_argument("--max", type=int, default=20)
    us.set_defaults(func=cmd_users_search)

    ui = sub.add_parser("user-info")
    ui.add_argument("user_id")
    ui.set_defaults(func=cmd_user_info)

    m = sub.add_parser("mark")
    m.add_argument("channel")
    m.add_argument("ts")
    m.set_defaults(func=cmd_mark)

    sl = sub.add_parser("saved-list")
    sl.add_argument("--limit", type=int, default=50)
    sl.set_defaults(func=cmd_saved_list)

    un = sub.add_parser("unsave")
    un.add_argument("channel")
    un.add_argument("ts")
    un.set_defaults(func=cmd_unsave)

    u = sub.add_parser("unreads")
    u.set_defaults(func=cmd_unreads)

    f = sub.add_parser("file", help="download a files.slack.com url_private to OUTPUT")
    f.add_argument("url")
    f.add_argument("output")
    f.set_defaults(func=cmd_file)

    args = p.parse_args()
    global _workspace
    _workspace = args.workspace
    args.func(args)


if __name__ == "__main__":
    main()
