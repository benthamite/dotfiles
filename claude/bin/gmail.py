#!/usr/bin/env python3
"""gmail.py — thin Gmail API wrapper for the pablo@epoch.ai account.

Replaces the gmail_* tools from the google-workspace-epoch MCP server.
Auth is shared with sheets.py via _gworkspace_auth.py and uses the same
GOOGLE_WORKSPACE_* env vars.

Usage:
  gmail.py query <gmail-search-query> [--max=N]
      Search Gmail. Returns one line per match: <id>\\t<from>\\t<date>\\t<subject>.
  gmail.py get <message-id> [--format=full|metadata|raw]
      Fetch a message. Default format=full prints headers + plaintext body
      (or HTML stripped to text if no plaintext part exists).
  gmail.py raw <message-id>
      Print the raw Gmail API JSON for a message (debugging / unusual fields).
  gmail.py attachment <message-id> <attachment-id> <output-path>
      Download an attachment to <output-path>.
  gmail.py archive <message-id>
      Remove the INBOX label from a message.
  gmail.py draft --to=<addr> --subject=<s> [--body=<text> | --body-file=<path>] [--cc=<addr>] [--bcc=<addr>]
      Create a draft. Prints the new draft ID on stdout.
  gmail.py send --to=<addr> --subject=<s> [--body=<text> | --body-file=<path>] [--cc=<addr>] [--bcc=<addr>]
      Send an email. Prints the sent message ID.
  gmail.py reply <message-id> [--body=<text> | --body-file=<path>]
      Reply to a thread (preserves In-Reply-To and References).
  gmail.py send-draft <draft-id>
      Send a previously created draft.
"""

import argparse
import base64
import email.message
import email.utils
import os
import sys

sys.path.insert(0, os.path.dirname(__file__))
from _gworkspace_auth import api_request  # noqa: E402

API = "https://gmail.googleapis.com/gmail/v1/users/me"


def _decode_body(payload):
    """Pull a plaintext body out of a Gmail message payload, falling back to HTML."""

    def find(parts, mime):
        for p in parts:
            if p.get("mimeType") == mime and p.get("body", {}).get("data"):
                return p["body"]["data"]
            if "parts" in p:
                hit = find(p["parts"], mime)
                if hit:
                    return hit
        return None

    parts = [payload] if "parts" not in payload else payload["parts"]
    data = find(parts, "text/plain") or find(parts, "text/html")
    if not data and payload.get("body", {}).get("data"):
        data = payload["body"]["data"]
    if not data:
        return ""
    return base64.urlsafe_b64decode(data + "=" * (-len(data) % 4)).decode("utf-8", errors="replace")


def _headers(msg):
    return {h["name"].lower(): h["value"] for h in msg.get("payload", {}).get("headers", [])}


def cmd_query(args):
    params = {"q": args.query, "maxResults": str(args.max)}
    out = api_request("GET", f"{API}/messages?" + "&".join(f"{k}={_url(v)}" for k, v in params.items()))
    for m in out.get("messages", []) or []:
        meta = api_request("GET", f"{API}/messages/{m['id']}?format=metadata&metadataHeaders=From&metadataHeaders=Subject&metadataHeaders=Date")
        h = _headers(meta)
        print(f"{m['id']}\t{h.get('from', '?')}\t{h.get('date', '?')}\t{h.get('subject', '?')}")


def cmd_get(args):
    msg = api_request("GET", f"{API}/messages/{args.id}?format={args.format}")
    if args.format == "metadata":
        print_yaml(_headers(msg))
        return
    if args.format == "raw":
        print(msg.get("raw", ""))
        return
    h = _headers(msg)
    for k in ("from", "to", "cc", "date", "subject"):
        if k in h:
            print(f"{k.title()}: {h[k]}")
    print(f"Message-ID: {msg['id']}")
    print(f"Thread-ID: {msg.get('threadId')}")
    if msg.get("labelIds"):
        print(f"Labels: {', '.join(msg['labelIds'])}")
    print()
    print(_decode_body(msg.get("payload", {})))
    attachments = _list_attachments(msg.get("payload", {}))
    if attachments:
        print()
        print("Attachments:")
        for a in attachments:
            print(f"  - {a['filename']} ({a['mimeType']}, {a['size']} bytes) id={a['attachmentId']}")


def cmd_raw(args):
    import json as _json
    msg = api_request("GET", f"{API}/messages/{args.id}?format=full")
    print(_json.dumps(msg, indent=2))


def cmd_attachment(args):
    out = api_request("GET", f"{API}/messages/{args.message_id}/attachments/{args.attachment_id}")
    data = base64.urlsafe_b64decode(out["data"] + "=" * (-len(out["data"]) % 4))
    with open(args.output, "wb") as f:
        f.write(data)
    print(f"Wrote {len(data)} bytes to {args.output}")


def cmd_archive(args):
    api_request("POST", f"{API}/messages/{args.id}/modify", body={"removeLabelIds": ["INBOX"]})
    print(f"Archived {args.id}")


def cmd_draft(args):
    raw = _build_raw(args)
    out = api_request("POST", f"{API}/drafts", body={"message": {"raw": raw}})
    print(out["id"])


def cmd_send(args):
    raw = _build_raw(args)
    out = api_request("POST", f"{API}/messages/send", body={"raw": raw})
    print(out["id"])


def cmd_reply(args):
    parent = api_request("GET", f"{API}/messages/{args.id}?format=metadata&metadataHeaders=From&metadataHeaders=Subject&metadataHeaders=Message-ID&metadataHeaders=References")
    h = _headers(parent)
    subject = h.get("subject", "")
    if not subject.lower().startswith("re:"):
        subject = "Re: " + subject
    body = _read_body(args)
    msg = email.message.EmailMessage()
    msg["To"] = h.get("from", "")
    msg["Subject"] = subject
    if "message-id" in h:
        msg["In-Reply-To"] = h["message-id"]
        msg["References"] = (h.get("references", "") + " " + h["message-id"]).strip()
    msg.set_content(body)
    raw = base64.urlsafe_b64encode(msg.as_bytes()).decode().rstrip("=")
    out = api_request("POST", f"{API}/messages/send", body={"raw": raw, "threadId": parent.get("threadId")})
    print(out["id"])


def cmd_send_draft(args):
    out = api_request("POST", f"{API}/drafts/send", body={"id": args.id})
    print(out["id"])


def _build_raw(args):
    msg = email.message.EmailMessage()
    msg["To"] = args.to
    if args.cc:
        msg["Cc"] = args.cc
    if args.bcc:
        msg["Bcc"] = args.bcc
    msg["Subject"] = args.subject
    msg.set_content(_read_body(args))
    return base64.urlsafe_b64encode(msg.as_bytes()).decode().rstrip("=")


def _read_body(args):
    if args.body_file:
        with open(args.body_file) as f:
            return f.read()
    return args.body or ""


def _list_attachments(payload):
    out = []
    parts = [payload] if "parts" not in payload else payload["parts"]
    stack = list(parts)
    while stack:
        p = stack.pop()
        if "parts" in p:
            stack.extend(p["parts"])
            continue
        body = p.get("body", {})
        if body.get("attachmentId"):
            out.append({
                "filename": p.get("filename", ""),
                "mimeType": p.get("mimeType", ""),
                "size": body.get("size", 0),
                "attachmentId": body["attachmentId"],
            })
    return out


def print_yaml(d):
    for k, v in d.items():
        print(f"{k}: {v}")


def _url(v):
    import urllib.parse
    return urllib.parse.quote(str(v), safe="")


def main():
    p = argparse.ArgumentParser(description="Gmail API wrapper")
    sub = p.add_subparsers(dest="cmd", required=True)

    q = sub.add_parser("query")
    q.add_argument("query")
    q.add_argument("--max", type=int, default=20)
    q.set_defaults(func=cmd_query)

    g = sub.add_parser("get")
    g.add_argument("id")
    g.add_argument("--format", choices=["full", "metadata", "raw"], default="full")
    g.set_defaults(func=cmd_get)

    r = sub.add_parser("raw")
    r.add_argument("id")
    r.set_defaults(func=cmd_raw)

    a = sub.add_parser("attachment")
    a.add_argument("message_id")
    a.add_argument("attachment_id")
    a.add_argument("output")
    a.set_defaults(func=cmd_attachment)

    arc = sub.add_parser("archive")
    arc.add_argument("id")
    arc.set_defaults(func=cmd_archive)

    for name, fn in [("draft", cmd_draft), ("send", cmd_send)]:
        s = sub.add_parser(name)
        s.add_argument("--to", required=True)
        s.add_argument("--subject", required=True)
        s.add_argument("--body")
        s.add_argument("--body-file", dest="body_file")
        s.add_argument("--cc")
        s.add_argument("--bcc")
        s.set_defaults(func=fn)

    rep = sub.add_parser("reply")
    rep.add_argument("id")
    rep.add_argument("--body")
    rep.add_argument("--body-file", dest="body_file")
    rep.set_defaults(func=cmd_reply)

    sd = sub.add_parser("send-draft")
    sd.add_argument("id")
    sd.set_defaults(func=cmd_send_draft)

    args = p.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
