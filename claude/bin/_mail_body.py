"""Set an email's body: text/plain plus the HTML alternative clients display.

Shared by gmail.py and every script that builds its own message for Pablo
(launchd's responder-mail.py). Rule: service-access.md, "Gmail".

Plain bodies get the HTML Gmail itself sends: each line kept, nothing
interpreted. Without an HTML part, clients show a text/plain-only message
with a hard break every 70 or so characters (Gmail's mobile app, Outlook),
which looks machine-made (2026-11-03: Eitan noticed it on a FIAS email to
Galicia).

Markdown bodies (markdown=True) are rendered by pandoc as GitHub-flavoured
Markdown, keeping every line break as written, so a body that uses no
Markdown syntax looks as it would sent plain (2026-11-09: the Hinge audit
email showed its "#" headings and "-" bullets raw). The text/plain part
keeps the Markdown source.
"""

import html
import shutil
import subprocess

# launchd jobs run with a PATH that may lack Homebrew's bin.
PANDOC = shutil.which("pandoc") or "/opt/homebrew/bin/pandoc"


def markdown_html(text):
    """Render TEXT (GitHub-flavoured Markdown) to an HTML fragment."""
    result = subprocess.run(
        [PANDOC, "--from=gfm+hard_line_breaks", "--to=html", "--wrap=none"],
        input=text, capture_output=True, text=True, check=False, timeout=60)
    if result.returncode != 0:
        raise RuntimeError(f"pandoc failed ({result.returncode}): {result.stderr.strip()}")
    return result.stdout


def plain_html(text):
    """Render TEXT line by line, as Gmail's own composer would."""
    lines = [html.escape(line) for line in text.rstrip("\n").split("\n")]
    return "<br>\n".join(lines)


def set_body(msg, text, markdown=False, markdown_text=None):
    """Set TEXT as MSG's text/plain body and add its HTML alternative.

    With MARKDOWN, the HTML renders MARKDOWN_TEXT (default TEXT) as
    Markdown; pass MARKDOWN_TEXT when a line of TEXT must stay literal,
    such as an angle-bracketed reference a search will look for.
    """
    msg.set_content(text)
    body = markdown_html(markdown_text if markdown_text is not None else text) \
        if markdown else plain_html(text)
    msg.add_alternative(f'<div dir="ltr">{body}</div>\n', subtype="html")
