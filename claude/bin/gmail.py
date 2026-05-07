#!/usr/bin/env python3
"""gmail.py — thin Gmail API wrapper for one of two Google Workspace accounts.

Replaces the gmail_* tools from the (former) google-workspace-epoch MCP
server. Auth is shared with sheets.py via _gworkspace_auth.py and uses the
GOOGLE_WORKSPACE_* env vars; see that module for the per-account env-var
mapping.

Pass --account {epoch,personal} on any subcommand. Default is `epoch`.

Usage:
  gmail.py [--account epoch|personal] query <gmail-search-query> [--max=N]
      Search Gmail. Returns one line per match: <id>\\t<from>\\t<date>\\t<subject>.
  gmail.py [--account ...] get <message-id> [--format=full|metadata|raw]
      Fetch a message. Default format=full prints headers + plaintext body
      (or HTML stripped to text if no plaintext part exists).
  gmail.py [--account ...] raw <message-id>
      Print the raw Gmail API JSON for a message (debugging / unusual fields).
  gmail.py [--account ...] attachment <message-id> <attachment-id> <output-path>
      Download an attachment to <output-path>.
  gmail.py [--account ...] archive <message-id>
      Remove the INBOX label from a message.
  gmail.py [--account ...] draft --to=<addr> --subject=<s> [--body=<text> | --body-file=<path>] [--cc=<addr>] [--bcc=<addr>] [--attach=<path> ...]
      Create a draft. Prints the new draft ID on stdout.
  gmail.py [--account ...] send --to=<addr> --subject=<s> [--body=<text> | --body-file=<path>] [--cc=<addr>] [--bcc=<addr>] [--attach=<path> ...]
      Send an email. Prints the sent message ID.
  gmail.py [--account ...] reply <message-id> [--to=<addr>] [--body=<text> | --body-file=<path>] [--attach=<path> ...]
      Reply to a thread (preserves In-Reply-To and References).
  gmail.py [--account ...] send-draft <draft-id>
      Send a previously created draft.
"""

import argparse
import base64
import email.message
import json
import mimetypes
import os
import sys
import urllib.parse

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
    return base64.urlsafe_b64decode(data + "=" * (-len(data) % 4)).decode(
        "utf-8", errors="replace"
    )


def _headers(msg):
    return {h["name"].lower(): h["value"] for h in msg.get("payload", {}).get("headers", [])}


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
            out.append(
                {
                    "filename": p.get("filename", ""),
                    "mimeType": p.get("mimeType", ""),
                    "size": body.get("size", 0),
                    "attachmentId": body["attachmentId"],
                }
            )
    return out


def _print_yaml(d):
    for k, v in d.items():
        print(f"{k}: {v}")


def _qs(v):
    return urllib.parse.quote(str(v), safe="")


def _read_body(args):
    if args.body_file:
        with open(args.body_file) as f:
            return f.read()
    return args.body or ""


def _add_attachments(msg, paths):
    for path in paths or []:
        mime_type, _ = mimetypes.guess_type(path)
        if mime_type:
            maintype, subtype = mime_type.split("/", 1)
        else:
            maintype, subtype = "application", "octet-stream"
        with open(path, "rb") as f:
            msg.add_attachment(
                f.read(),
                maintype=maintype,
                subtype=subtype,
                filename=os.path.basename(path),
            )


def _build_raw(args):
    msg = email.message.EmailMessage()
    msg["To"] = args.to
    if args.cc:
        msg["Cc"] = args.cc
    if args.bcc:
        msg["Bcc"] = args.bcc
    msg["Subject"] = args.subject
    msg.set_content(_read_body(args))
    _add_attachments(msg, getattr(args, "attach", []))
    return base64.urlsafe_b64encode(msg.as_bytes()).decode().rstrip("=")


def cmd_query(args):
    params = {"q": args.query, "maxResults": str(args.max)}
    out = api_request(
        "GET",
        f"{API}/messages?" + "&".join(f"{k}={_qs(v)}" for k, v in params.items()),
        account=args.account,
    )
    for m in out.get("messages", []) or []:
        meta = api_request(
            "GET",
            f"{API}/messages/{m['id']}?format=metadata&metadataHeaders=From&metadataHeaders=Subject&metadataHeaders=Date",
            account=args.account,
        )
        h = _headers(meta)
        print(f"{m['id']}\t{h.get('from', '?')}\t{h.get('date', '?')}\t{h.get('subject', '?')}")


def cmd_get(args):
    msg = api_request(
        "GET", f"{API}/messages/{args.id}?format={args.format}", account=args.account
    )
    if args.format == "metadata":
        _print_yaml(_headers(msg))
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
    msg = api_request("GET", f"{API}/messages/{args.id}?format=full", account=args.account)
    print(json.dumps(msg, indent=2))


def cmd_attachment(args):
    out = api_request(
        "GET",
        f"{API}/messages/{args.message_id}/attachments/{args.attachment_id}",
        account=args.account,
    )
    data = base64.urlsafe_b64decode(out["data"] + "=" * (-len(out["data"]) % 4))
    with open(args.output, "wb") as f:
        f.write(data)
    print(f"Wrote {len(data)} bytes to {args.output}")


def cmd_archive(args):
    api_request(
        "POST",
        f"{API}/messages/{args.id}/modify",
        body={"removeLabelIds": ["INBOX"]},
        account=args.account,
    )
    print(f"Archived {args.id}")


def cmd_draft(args):
    raw = _build_raw(args)
    out = api_request(
        "POST", f"{API}/drafts", body={"message": {"raw": raw}}, account=args.account
    )
    print(out["id"])


def cmd_send(args):
    raw = _build_raw(args)
    out = api_request("POST", f"{API}/messages/send", body={"raw": raw}, account=args.account)
    print(out["id"])


def cmd_reply(args):
    parent = api_request(
        "GET",
        f"{API}/messages/{args.id}?format=metadata&metadataHeaders=From&metadataHeaders=Subject&metadataHeaders=Message-ID&metadataHeaders=References",
        account=args.account,
    )
    h = _headers(parent)
    subject = h.get("subject", "")
    if not subject.lower().startswith("re:"):
        subject = "Re: " + subject
    body = _read_body(args)
    msg = email.message.EmailMessage()
    msg["To"] = args.to or h.get("from", "")
    msg["Subject"] = subject
    if "message-id" in h:
        msg["In-Reply-To"] = h["message-id"]
        msg["References"] = (h.get("references", "") + " " + h["message-id"]).strip()
    msg.set_content(body)
    _add_attachments(msg, args.attach)
    raw = base64.urlsafe_b64encode(msg.as_bytes()).decode().rstrip("=")
    out = api_request(
        "POST",
        f"{API}/messages/send",
        body={"raw": raw, "threadId": parent.get("threadId")},
        account=args.account,
    )
    print(out["id"])


def cmd_send_draft(args):
    out = api_request(
        "POST", f"{API}/drafts/send", body={"id": args.id}, account=args.account
    )
    print(out["id"])


def main():
    # Shared --account flag, available on every subcommand.
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument(
        "--account",
        choices=["epoch", "personal"],
        default="epoch",
        help="Which Google Workspace account to authenticate as (default: epoch).",
    )

    p = argparse.ArgumentParser(description="Gmail API wrapper", parents=[common])
    sub = p.add_subparsers(dest="cmd", required=True)

    q = sub.add_parser("query", parents=[common])
    q.add_argument("query")
    q.add_argument("--max", type=int, default=20)
    q.set_defaults(func=cmd_query)

    g = sub.add_parser("get", parents=[common])
    g.add_argument("id")
    g.add_argument("--format", choices=["full", "metadata", "raw"], default="full")
    g.set_defaults(func=cmd_get)

    r = sub.add_parser("raw", parents=[common])
    r.add_argument("id")
    r.set_defaults(func=cmd_raw)

    a = sub.add_parser("attachment", parents=[common])
    a.add_argument("message_id")
    a.add_argument("attachment_id")
    a.add_argument("output")
    a.set_defaults(func=cmd_attachment)

    arc = sub.add_parser("archive", parents=[common])
    arc.add_argument("id")
    arc.set_defaults(func=cmd_archive)

    for name, fn in [("draft", cmd_draft), ("send", cmd_send)]:
        s = sub.add_parser(name, parents=[common])
        s.add_argument("--to", required=True)
        s.add_argument("--subject", required=True)
        s.add_argument("--body")
        s.add_argument("--body-file", dest="body_file")
        s.add_argument("--cc")
        s.add_argument("--bcc")
        s.add_argument("--attach", action="append", default=[])
        s.set_defaults(func=fn)

    rep = sub.add_parser("reply", parents=[common])
    rep.add_argument("id")
    rep.add_argument("--to")
    rep.add_argument("--body")
    rep.add_argument("--body-file", dest="body_file")
    rep.add_argument("--attach", action="append", default=[])
    rep.set_defaults(func=cmd_reply)

    sd = sub.add_parser("send-draft", parents=[common])
    sd.add_argument("id")
    sd.set_defaults(func=cmd_send_draft)

    args = p.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
