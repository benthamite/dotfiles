#!/usr/bin/env python3
"""Extract Google Search Console issue links from Gmail raw messages."""

from __future__ import annotations

import argparse
import base64
import html
import json
import re
import subprocess
import sys
import urllib.error
import urllib.request
from pathlib import Path
from typing import Any


GMAIL = Path.home() / "My Drive/dotfiles/claude/bin/gmail.py"


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):  # type: ignore[override]
        return None


def decode_body(data: str) -> str:
    padded = data + ("=" * ((4 - len(data) % 4) % 4))
    return base64.urlsafe_b64decode(padded.encode()).decode("utf-8", "replace")


def iter_parts(part: dict[str, Any]):
    yield part
    for child in part.get("parts", []) or []:
        yield from iter_parts(child)


def raw_message(message_id: str, account: str) -> dict[str, Any]:
    output = subprocess.check_output(
        ["python3", str(GMAIL), "raw", message_id, "--account", account],
        text=True,
    )
    return json.loads(output)


def header(msg: dict[str, Any], name: str) -> str:
    for item in msg.get("payload", {}).get("headers", []) or []:
        if item.get("name", "").lower() == name.lower():
            return item.get("value", "")
    return ""


def bodies(msg: dict[str, Any], mime_type: str) -> list[str]:
    out: list[str] = []
    for part in iter_parts(msg.get("payload", {})):
        if part.get("mimeType") != mime_type:
            continue
        data = part.get("body", {}).get("data")
        if data:
            out.append(decode_body(data))
    return out


def issue_label(msg: dict[str, Any]) -> str:
    text = "\n".join(bodies(msg, "text/plain"))
    match = re.search(r"issue:\s*'([^']+)'", text)
    if match:
        return match.group(1)
    subject = header(msg, "Subject")
    return subject or "(unknown)"


def property_name(msg: dict[str, Any]) -> str:
    text = "\n".join(bodies(msg, "text/plain"))
    match = re.search(r"property,\s*([^\s.]+(?:\.[^\s.]+)+)", text)
    if match:
        return match.group(1).rstrip(".")
    match = re.search(r"site\s+(https?://[^\s]+|[^\s]+)", header(msg, "Subject"))
    return match.group(1).rstrip("/") if match else ""


def view_issue_links(msg: dict[str, Any]) -> list[str]:
    links: list[str] = []
    for body in bodies(msg, "text/html"):
        for match in re.finditer(
            r"<a\b[^>]*\bhref=(\"[^\"]+\"|'[^']+'|[^\s>]+)[^>]*>.*?View issue details",
            body,
            flags=re.IGNORECASE | re.DOTALL,
        ):
            href = match.group(1).strip("\"'")
            href = html.unescape(href)
            if href.startswith("http") and href not in links:
                links.append(href)
    return links


def resolve_once(url: str) -> str:
    opener = urllib.request.build_opener(NoRedirect)
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    try:
        response = opener.open(req, timeout=20)
        return response.geturl()
    except urllib.error.HTTPError as exc:
        return exc.headers.get("Location") or url


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("message_ids", nargs="+")
    parser.add_argument("--account", default="personal", choices=["personal", "epoch"])
    parser.add_argument("--json", action="store_true", help="Emit JSON instead of TSV.")
    args = parser.parse_args()

    rows: list[dict[str, str]] = []
    for message_id in args.message_ids:
        msg = raw_message(message_id, args.account)
        links = view_issue_links(msg)
        if not links:
            rows.append(
                {
                    "message_id": message_id,
                    "thread_id": msg.get("threadId", ""),
                    "subject": header(msg, "Subject"),
                    "issue": issue_label(msg),
                    "property": property_name(msg),
                    "c_gle_url": "",
                    "issue_url": "",
                }
            )
            continue
        for link in links:
            rows.append(
                {
                    "message_id": message_id,
                    "thread_id": msg.get("threadId", ""),
                    "subject": header(msg, "Subject"),
                    "issue": issue_label(msg),
                    "property": property_name(msg),
                    "c_gle_url": link,
                    "issue_url": resolve_once(link),
                }
            )

    if args.json:
        print(json.dumps(rows, indent=2, ensure_ascii=False))
        return 0

    fields = ["message_id", "thread_id", "issue", "property", "issue_url", "subject"]
    print("\t".join(fields))
    for row in rows:
        print("\t".join((row.get(field, "") or "").replace("\t", " ") for field in fields))
    return 0


if __name__ == "__main__":
    sys.exit(main())
