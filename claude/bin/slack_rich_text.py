"""Convert Slack mrkdwn with bullet lines into a rich_text block.

Slack's message composer stores lists as `rich_text_list` elements, which
Slack renders as real lists: wrapped lines stay indented under their bullet.
A literal "•" in plain mrkdwn renders as text, so wrapped lines start at the
margin. `to_blocks` turns text written the way agents draft check-ins into
the structure the composer itself produces.

Input conventions:
- A line starting with "• " (or "- ") is a top-level bullet. A line starting
  with "◦ " or "▪ ", or a bullet indented by four spaces per level, is
  nested; its level is the number of leading four-space groups, or 1 for
  "◦" and 2 for "▪" when unindented.
- Any other line is ordinary text; blank lines are kept.
- Inline *bold*, _italic_, ~strike~ (nestable), `code` and <url|label> links.
- &amp;, &lt; and &gt; are unescaped, since rich text holds raw characters.
"""

import re

BULLET = re.compile(r"^(?P<indent>(?: {4})*)(?P<glyph>[•◦▪\-]) (?P<body>.*)$")
GLYPH_LEVEL = {"•": 0, "-": 0, "◦": 1, "▪": 2}
STYLE = {"*": "bold", "_": "italic", "~": "strike"}
EMPHASIS = re.compile(r"(?<![\w*_~])([*_~])(?=\S)(.+?)(?<=\S)\1(?![\w*_~])")
TOKEN = re.compile(r"<(https?://[^|>\s]+)(?:\|([^>]+))?>|`([^`]+)`")


def to_blocks(text):
    """Return a list holding one rich_text block equivalent to TEXT."""
    elements, prose = [], []
    for line in _unescape(text).split("\n"):
        match = BULLET.match(line)
        if not match:
            prose.append(line)
            continue
        if prose:
            elements.append(_section("\n".join(prose) + "\n"))
            prose = []
        level = len(match["indent"]) // 4 or GLYPH_LEVEL[match["glyph"]]
        item = {"type": "rich_text_section", "elements": inline(match["body"])}
        last = elements[-1] if elements else None
        if last and last["type"] == "rich_text_list" and last["indent"] == level:
            last["elements"].append(item)
        else:
            elements.append({"type": "rich_text_list", "style": "bullet",
                             "indent": level, "border": 0, "elements": [item]})
    if prose and any(prose):
        elements.append(_section("\n".join(prose)))
    return [{"type": "rich_text", "elements": elements}]


def _section(text):
    return {"type": "rich_text_section", "elements": inline(text)}


def _unescape(text):
    return text.replace("&lt;", "<").replace("&gt;", ">").replace("&amp;", "&")


def inline(text, style=frozenset()):
    """Return rich_text elements for TEXT, with STYLE applied throughout."""
    out, pos = [], 0
    for match in TOKEN.finditer(text):
        out += _styled(text[pos:match.start()], style)
        url, label, code = match.groups()
        if code is not None:
            out.append(_element("text", code, style | {"code"}))
        else:
            link = {"type": "link", "url": url}
            if label:
                link["text"] = label
            if style:
                link["style"] = {name: True for name in sorted(style)}
            out.append(link)
        pos = match.end()
    return out + _styled(text[pos:], style)


def _styled(text, style):
    out, pos = [], 0
    for match in EMPHASIS.finditer(text):
        if match.start() > pos:
            out.append(_element("text", text[pos:match.start()], style))
        out += inline(match[2], style | {STYLE[match[1]]})
        pos = match.end()
    if pos < len(text):
        out.append(_element("text", text[pos:], style))
    return out


def _element(kind, text, style):
    element = {"type": kind, "text": text}
    if style:
        element["style"] = {name: True for name in sorted(style)}
    return element
