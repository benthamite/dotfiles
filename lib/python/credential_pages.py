"""Decide whether a browser tool result's images may show a credential page.

Screenshots cannot be masked, and five of the nine browser exposures in
docs/secret-guard.md were screenshots of key, token or settings pages. The
output hooks therefore drop image blocks when the page could be one listed in
agents/credential-pages.txt.

The URL comes from the tool result itself: every Claude in Chrome result ends
with a "Tab Context" section listing the tabs and their URLs, and a batch's
input names the URLs it navigates to. This is deliberately conservative: an
image is dropped when any URL in the result or input matches the list, or when
no URL can be found at all.

Text is masked on those pages too. The shape and fingerprint layers of the
output redactor miss credentials with no known prefix that are not yet in this
machine's index, such as a freshly created key: on 2026-10-23 `get_page_text`
on Cloudflare's R2 token success page printed a new S3 access key ID and
secret (bare hex), although the page is listed and its screenshots were
stripped. So a browser tool's text from a listed page has every opaque
credential-like run masked (`mask_opaque`), outside URLs.
"""

from __future__ import annotations

import json
import re
from pathlib import Path

PAGES_FILE = Path(__file__).resolve().parents[2] / "agents/credential-pages.txt"
URL = re.compile(r"https?://[^\s\"'()<>\]\[]+")
NOTICE = ("[screenshot removed: the browser was on, or may have been on, a page that "
          "displays credentials (agents/credential-pages.txt). Page text from such a page "
          "has credential-like strings masked, but do not read a page that shows a new "
          "secret: copy it with the page's copy button and store it with op-clipboard-store.]")
OPAQUE_MASK = "[masked: credential-like string on a credential page]"
# A run of 20+ token characters with both a letter and a digit (hex keys, base64 secrets), or 16+ digits.
OPAQUE = re.compile(r"(?<![A-Za-z0-9_+/=-])(?:(?=[A-Za-z0-9_+/=-]*[0-9])(?=[A-Za-z0-9_+/=-]*[A-Za-z])"
                    r"[A-Za-z0-9_+/=-]{20,}|[0-9]{16,})(?![A-Za-z0-9_+/=-])")
BROWSER_TOOL = re.compile(r"claude-in-chrome|Claude_Browser|(^|__)browser_|playwright|control-chrome|node_repl")


def patterns(path: Path = PAGES_FILE) -> list[re.Pattern]:
    lines = path.read_text().splitlines()
    return [re.compile(line.strip()) for line in lines if line.strip() and not line.lstrip().startswith("#")]


def _urls(value, out: set[str]) -> None:
    if isinstance(value, str):
        out.update(URL.findall(value))
    elif isinstance(value, dict):
        if value.get("type") == "image":
            return
        for v in value.values():
            _urls(v, out)
    elif isinstance(value, list):
        for v in value:
            _urls(v, out)


def has_images(response) -> bool:
    if isinstance(response, dict):
        return response.get("type") == "image" or any(has_images(v) for v in response.values())
    if isinstance(response, list):
        return any(has_images(v) for v in response)
    return False


def images_unsafe(tool_input, response, pages: list[re.Pattern] | None = None) -> bool:
    """True when the result's images must be dropped."""
    if not has_images(response):
        return False
    pages = patterns() if pages is None else pages
    urls: set[str] = set()
    _urls(tool_input, urls)
    _urls(response, urls)
    if isinstance(tool_input, str):
        try:
            _urls(json.loads(tool_input), urls)
        except ValueError:
            pass
    if not urls:
        return True  # cannot tell where the browser was
    return any(p.search(u) for u in urls for p in pages)


def _matched(tool_input, response, pages: list[re.Pattern] | None) -> bool | None:
    """True/False when a URL in the input or result does/doesn't match the list; None when there is no URL."""
    pages = patterns() if pages is None else pages
    urls: set[str] = set()
    _urls(tool_input, urls)
    _urls(response, urls)
    if isinstance(tool_input, str):
        try:
            _urls(json.loads(tool_input), urls)
        except ValueError:
            pass
    if not urls:
        return None
    return any(p.search(u) for u in urls for p in pages)


def text_unsafe(tool_name: str, tool_input, response, pages: list[re.Pattern] | None = None) -> bool:
    """True when a browser tool's text must have opaque strings masked: a URL in it matches the list."""
    return bool(BROWSER_TOOL.search(tool_name or "")) and bool(_matched(tool_input, response, pages))


def mask_opaque(text: str) -> str:
    """Replace credential-like runs outside URLs with a notice."""
    out, last = [], 0
    for m in URL.finditer(text):
        out.append(OPAQUE.sub(OPAQUE_MASK, text[last:m.start()]))
        out.append(m.group(0))
        last = m.end()
    out.append(OPAQUE.sub(OPAQUE_MASK, text[last:]))
    return "".join(out)


def strip_images(response):
    """The same structure with every image block replaced by a text notice."""
    if isinstance(response, dict):
        if response.get("type") == "image":
            return {"type": "text", "text": NOTICE}
        return {k: strip_images(v) for k, v in response.items()}
    if isinstance(response, list):
        return [strip_images(v) for v in response]
    return response
