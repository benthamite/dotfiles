"""The [agent] prefix on messages an agent sends under Pablo's name.

Rule (global CLAUDE.md, "Messages you send"): every message an agent sends as
Pablo starts with "[agent] ", except text Pablo approved in a review file,
which goes out under his own name.  The send paths of wa-send, slack.py and
gmail.py call `apply`, so the rule does not depend on the agent remembering
it: without --reviewed the prefix is added; with --reviewed FILE the text
must appear in an item of that file that carries Pablo's comment, or the send
is refused.
"""

import re
import sys
from pathlib import Path

PREFIX = "[agent] "


def _norm(text: str) -> str:
    # Drop Markdown blockquote markers and collapse whitespace, so a draft
    # quoted in the review file matches the text as sent.
    lines = [re.sub(r"^\s*>\s?", "", line) for line in text.splitlines()]
    return re.sub(r"\s+", " ", " ".join(lines)).strip()


def apply(text: str, reviewed: str | None, tool: str) -> str:
    """Return TEXT as it must be sent, or exit if --reviewed does not hold."""
    if reviewed:
        body = text[len(PREFIX):] if text.startswith(PREFIX) else text
        path = Path(reviewed).expanduser()
        if not path.is_file():
            sys.exit(f"{tool}: --reviewed {reviewed}: no such file")
        content = path.read_text(errors="replace")
        # The review file is split into items by Markdown headings; Pablo
        # approves an item with a CriticMarkup comment ({>>send<<}) in it.
        sections = re.split(r"(?m)^#{1,3} ", content)
        hits = [sec for sec in sections if _norm(body) in _norm(sec)]
        if not hits:
            sys.exit(f"{tool}: the text is not in {reviewed}, so Pablo did not "
                     "approve it there; send it without --reviewed (it gets the "
                     "[agent] prefix) or put the exact text in the review file")
        if not any("{>>" in sec for sec in hits):
            sys.exit(f"{tool}: the text is in {reviewed} but its item has no "
                     "comment from Pablo ({>>...<<}); send it before removing his "
                     "comment, or without --reviewed")
        return body
    return text if text.startswith(PREFIX) else PREFIX + text
