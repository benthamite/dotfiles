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
"""

from __future__ import annotations

import json
import re
from pathlib import Path

PAGES_FILE = Path(__file__).resolve().parents[2] / "agents/credential-pages.txt"
URL = re.compile(r"https?://[^\s\"'()<>\]\[]+")
NOTICE = ("[screenshot removed: the browser was on, or may have been on, a page that "
          "displays credentials (agents/credential-pages.txt). Use page text instead, "
          "which is masked.]")


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


def strip_images(response):
    """The same structure with every image block replaced by a text notice."""
    if isinstance(response, dict):
        if response.get("type") == "image":
            return {"type": "text", "text": NOTICE}
        return {k: strip_images(v) for k, v in response.items()}
    if isinstance(response, list):
        return [strip_images(v) for v in response]
    return response
