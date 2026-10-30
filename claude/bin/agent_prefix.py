"""The [agent] prefix on messages an agent sends under Pablo's name.

Rule (global CLAUDE.md, "Messages you send"): every message an agent sends as
Pablo starts with "[agent] ", except text Pablo approved in a review file,
which goes out under his own name.  The send paths of wa-send, slack.py and
gmail.py call `apply`, so the rule does not depend on the agent remembering
it: without --reviewed the prefix is added; with --reviewed FILE the text
must appear in that file, or the send is refused.
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
        if _norm(body) not in _norm(path.read_text(errors="replace")):
            sys.exit(f"{tool}: the text is not in {reviewed}, so Pablo did not "
                     "approve it there; send it without --reviewed (it gets the "
                     "[agent] prefix) or put the exact text in the review file")
        return body
    return text if text.startswith(PREFIX) else PREFIX + text
