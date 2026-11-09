"""Email bodies carry an HTML part; Markdown bodies are rendered in it."""

import email
import email.message
import email.policy
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "claude/bin"))
import _mail_body  # noqa: E402


def parts(text, **kw):
    msg = email.message.EmailMessage()
    _mail_body.set_body(msg, text, **kw)
    parsed = email.message_from_bytes(msg.as_bytes(), policy=email.policy.default)
    return (parsed.get_body(("plain",)).get_content(),
            parsed.get_body(("html",)).get_content())


def test_plain_body_keeps_lines_and_interprets_nothing():
    plain, html = parts("# not a heading\n**a** <b>\nline")
    assert plain == "# not a heading\n**a** <b>\nline\n"
    assert "# not a heading<br>\n**a** &lt;b&gt;<br>\nline" in html


def test_markdown_body_is_rendered_and_plain_part_keeps_source():
    source = "# Changes\n\n- one (undo: `git revert 81f9902`)\nour corpus had it\n\n**bold**\n"
    plain, html = parts(source, markdown=True)
    assert plain == source
    assert "<h1" in html and "<li>one (undo: <code>git revert 81f9902</code>)<br />" in html
    assert "<strong>bold</strong>" in html
    assert "# Changes" not in html


def test_markdown_text_overrides_what_is_rendered():
    plain, html = parts("Ref: <x.y@host.local>\n", markdown=True, markdown_text="Ref: `<x.y@host.local>`\n")
    assert plain == "Ref: <x.y@host.local>\n"
    assert "Ref: <code>&lt;x.y@host.local&gt;</code>" in html and "mailto" not in html
