#!/usr/bin/env python3
"""slack.py — thin Slack web-API wrapper using xoxc/xoxd browser-session tokens.

Replaces the slack-unofficial-epochai MCP server. Auth tokens come from
1Password via `op read`:

  op://Automations/Slack MCP - Epoch Unofficial/xoxc_token  -> Bearer token
  op://Automations/Slack MCP - Epoch Unofficial/xoxd_token  -> 'd' cookie

This is the same auth model the unofficial slack-mcp-server uses; tokens
are scraped from a logged-in browser session and impersonate the user
account, so mark/unread/saved-message operations work as your account
sees them (unlike a bot OAuth token).

Usage:
  slack.py search '<query>' [--max=N] [--sort=score|timestamp]
      Search messages workspace-wide (search.messages).
  slack.py history <channel-id> [--limit=N] [--oldest=ts] [--latest=ts] [--cursor=...]
      Fetch a channel's recent messages (conversations.history).
  slack.py replies <channel-id> <thread-ts> [--limit=N] [--cursor=...] [--no-resolve-users]
      Fetch replies in a thread (conversations.replies), resolving user IDs by default.
  slack.py permalink <url> [--limit=N] [--cursor=...] [--no-resolve-users]
      Fetch the message or thread context for a Slack permalink, resolving user IDs by default.
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

Output: raw JSON from Slack on stdout. Non-zero exit on API error.
"""

import argparse
import json
import os
import re
import subprocess
import sys
import urllib.parse
import urllib.request

API = "https://slack.com/api"
TOKEN_OP_PATH = "op://Automations/Slack MCP - Epoch Unofficial"


def _op_read(path):
    out = subprocess.run(
        ["op", "read", path],
        check=False,
        capture_output=True,
        text=True,
    )
    if out.returncode != 0:
        sys.stderr.write(f"ERROR: op read {path}: {out.stderr.strip()}\n")
        sys.exit(1)
    return out.stdout.strip()


_xoxc = None
_xoxd = None


def _tokens():
    global _xoxc, _xoxd
    if _xoxc is None:
        _xoxc = _op_read(f"{TOKEN_OP_PATH}/xoxc_token")
        _xoxd = _op_read(f"{TOKEN_OP_PATH}/xoxd_token")
    return _xoxc, _xoxd


def call(method, **params):
    """POST to slack.com/api/<method> with the given form params."""
    xoxc, xoxd = _tokens()
    body = urllib.parse.urlencode({k: v for k, v in params.items() if v is not None}).encode()
    req = urllib.request.Request(
        f"{API}/{method}",
        data=body,
        method="POST",
        headers={
            "Authorization": f"Bearer {xoxc}",
            "Cookie": f"d={xoxd}",
            "Content-Type": "application/x-www-form-urlencoded; charset=utf-8",
        },
    )
    try:
        resp = urllib.request.urlopen(req)
    except urllib.error.HTTPError as e:
        sys.stderr.write(f"ERROR: HTTP {e.code} from {method}: {e.read().decode()}\n")
        sys.exit(1)
    data = json.loads(resp.read())
    if not data.get("ok"):
        sys.stderr.write(f"ERROR: slack API {method} failed: {data.get('error')}\n")
        sys.exit(1)
    return data


def _permalink_ts_to_ts(permalink_ts):
    if not permalink_ts.isdigit() or len(permalink_ts) <= 6:
        raise ValueError(f"invalid Slack permalink timestamp: {permalink_ts}")
    return f"{permalink_ts[:-6]}.{permalink_ts[-6:]}"


def parse_permalink(url):
    parsed = urllib.parse.urlparse(url)
    if parsed.scheme not in ("http", "https") or not parsed.netloc.endswith(".slack.com"):
        raise ValueError(f"not a Slack URL: {url}")

    parts = [part for part in parsed.path.split("/") if part]
    if len(parts) < 3 or parts[0] != "archives":
        raise ValueError(f"not a Slack message permalink: {url}")

    channel = parts[1]
    message_part = parts[2]
    if not message_part.startswith("p"):
        raise ValueError(f"Slack permalink is missing p<timestamp>: {url}")

    message_ts = _permalink_ts_to_ts(message_part[1:])
    query = urllib.parse.parse_qs(parsed.query)
    thread_ts = (query.get("thread_ts") or [None])[0]
    cid = (query.get("cid") or [None])[0]
    if cid and cid != channel:
        raise ValueError(f"Slack permalink channel mismatch: path={channel}, cid={cid}")

    return {
        "url": url,
        "team_domain": parsed.netloc.removesuffix(".slack.com"),
        "channel": channel,
        "message_ts": message_ts,
        "thread_ts": thread_ts,
    }


def _has_replies(message):
    try:
        return int(message.get("reply_count") or 0) > 0
    except (TypeError, ValueError):
        return False


_user_cache = {}
_USER_ID_RE = re.compile(r"^U[A-Z0-9]{8,}$")


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
    out = call("search.messages", query=args.query, count=str(args.max), sort=args.sort)
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
    out = call(
        "conversations.replies",
        channel=args.channel,
        ts=args.thread_ts,
        limit=str(args.limit),
        cursor=args.cursor,
    )
    if args.resolve_users:
        resolve_users_in_response(out)
    print(json.dumps(out, indent=2))


def cmd_permalink(args):
    try:
        info = parse_permalink(args.url)
    except ValueError as e:
        sys.stderr.write(f"ERROR: {e}\n")
        sys.exit(2)

    if info["thread_ts"]:
        out = call(
            "conversations.replies",
            channel=info["channel"],
            ts=info["thread_ts"],
            limit=str(args.limit),
            cursor=args.cursor,
        )
        out["source"] = "conversations.replies"
    else:
        out = call(
            "conversations.history",
            channel=info["channel"],
            latest=info["message_ts"],
            inclusive="true",
            limit="1",
        )
        out["source"] = "conversations.history"
        messages = out.get("messages") or []
        if messages and _has_replies(messages[0]):
            thread_ts = messages[0].get("thread_ts") or messages[0].get("ts")
            info["thread_ts"] = thread_ts
            out = call(
                "conversations.replies",
                channel=info["channel"],
                ts=thread_ts,
                limit=str(args.limit),
                cursor=args.cursor,
            )
            out["source"] = "conversations.replies"

    out["permalink"] = info
    if args.resolve_users:
        resolve_users_in_response(out)
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


def main():
    p = argparse.ArgumentParser(description="Slack web-API wrapper (xoxc/xoxd auth)")
    sub = p.add_subparsers(dest="cmd", required=True)

    s = sub.add_parser("search")
    s.add_argument("query")
    s.add_argument("--max", type=int, default=20)
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

    u = sub.add_parser("unreads")
    u.set_defaults(func=cmd_unreads)

    args = p.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
