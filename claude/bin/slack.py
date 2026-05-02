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
  slack.py replies <channel-id> <thread-ts> [--limit=N] [--cursor=...]
      Fetch replies in a thread (conversations.replies).
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
    r.set_defaults(func=cmd_replies)

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
