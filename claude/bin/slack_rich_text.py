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
- A blank line right after a list stands for the line break Slack's composer
  puts at the start of the text that follows a list (Slack shows a gap there);
  `from_blocks` writes that break back as a blank line, so the two round-trip.
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


GLYPH = {0: "•", 1: "◦"}
MARK = {"bold": "*", "italic": "_", "strike": "~", "code": "`"}


def from_blocks(blocks):
    """Return mrkdwn for the rich_text BLOCKS, in the form `to_blocks` reads.

    Slack replaces a message's plain `text` with a flattened copy (newlines
    become spaces) whenever it carries rich text, so this is how a reader
    recovers the lines and bullets. Return None when BLOCKS hold no
    rich_text block.
    """
    rich = [b for b in blocks or [] if b.get("type") == "rich_text"]
    if not rich:
        return None
    out = []
    for block in rich:
        for element in block.get("elements") or []:
            if element.get("type") == "rich_text_list":
                level = int(element.get("indent") or 0)
                glyph = GLYPH.get(level, "▪")
                for item in element.get("elements") or []:
                    out.append(f"{'    ' * level}{glyph} {render(item.get('elements'))}\n")
            else:
                out.append(render(element.get("elements")))
    return "".join(out).rstrip("\n")


def render(elements):
    """Return mrkdwn for inline rich_text ELEMENTS, merging runs of one style."""
    runs = []
    for element in elements or []:
        style = frozenset(k for k, v in (element.get("style") or {}).items() if v and k in MARK)
        for piece, piece_style in _split_space(_plain(element), style):
            runs.append((piece, piece_style))
    out, open_styles = [], []
    for index, (piece, style) in enumerate(runs):
        while open_styles and not set(open_styles) <= style:
            out.append(MARK[open_styles.pop()])
        for name in sorted(style - set(open_styles),
                           key=lambda name: (-_reach(runs, index, name), name)):
            out.append(MARK[name])
            open_styles.append(name)
        out.append(piece)
    out.extend(MARK[name] for name in reversed(open_styles))
    return "".join(out)


def _reach(runs, index, name):
    """Return how many runs from INDEX on keep style NAME without a break.

    Opening the longest-lasting style first makes it the outer marker, so
    `~*S1*: text~` round-trips instead of becoming `*~S1~*~: text~`.
    """
    count = 0
    for _, style in runs[index:]:
        if name not in style:
            break
        count += 1
    return count


def _split_space(text, style):
    """Keep surrounding whitespace and newlines outside style markers."""
    if not style:
        return [(text, style)]
    match = re.fullmatch(r"(\s*)(.*?)(\s*)", text, re.S)
    lead, body, trail = match.groups()
    return [(p, s) for p, s in ((lead, frozenset()), (body, style), (trail, frozenset())) if p]


def _plain(element):
    kind = element.get("type")
    if kind == "text":
        return element.get("text", "")
    if kind == "link":
        label = element.get("text")
        return f"<{element['url']}|{label}>" if label else f"<{element['url']}>"
    if kind == "user":
        return f"<@{element.get('user_id')}>"
    if kind == "channel":
        return f"<#{element.get('channel_id')}>"
    if kind == "emoji":
        return f":{element.get('name')}:"
    if kind == "broadcast":
        return f"<!{element.get('range')}>"
    return element.get("text", "")
