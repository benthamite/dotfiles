"""A reply sent with --reply-to keeps its thread's subject."""

import importlib.machinery
import importlib.util
import types
from pathlib import Path

import pytest

GMAIL = Path(__file__).resolve().parent.parent / "claude/bin/gmail.py"


def load():
    loader = importlib.machinery.SourceFileLoader("gmail_cli", str(GMAIL))
    spec = importlib.util.spec_from_loader("gmail_cli", loader)
    mod = importlib.util.module_from_spec(spec)
    loader.exec_module(mod)
    return mod


def args(subject):
    return types.SimpleNamespace(reply_to="m1", account="baish", subject=subject,
                                 to="a@b.c", cc=None, bcc=None, body="hi",
                                 body_file=None, attach=[])


def test_changed_subject_is_refused(monkeypatch):
    g = load()
    monkeypatch.setattr(g, "_reply_context",
                        lambda i, a: ("t1", {"subject": "Grant approved", "message-id": "<x>"}))
    with pytest.raises(SystemExit):
        g._build_message(args("Re: Grant approved (RG-1271)"))


def test_omitted_subject_uses_thread_subject(monkeypatch):
    g = load()
    monkeypatch.setattr(g, "_reply_context",
                        lambda i, a: ("t1", {"subject": "Grant approved", "message-id": "<x>"}))
    raw, thread = g._build_message(args(None))
    assert thread == "t1"


def new_mail(subject, **kw):
    a = args(subject)
    a.reply_to = None
    a.__dict__.update(kw)
    return a


def test_fake_re_subject_is_refused():
    g = load()
    with pytest.raises(SystemExit):
        g._build_message(new_mail("Re: Implementación segunda edición"))


def test_plain_new_subject_is_allowed():
    g = load()
    raw, thread = g._build_message(new_mail("Convenio firmado"))
    assert thread is None


def test_reply_from_another_account_threads_by_headers(monkeypatch):
    import base64
    import email
    g = load()
    seen = {}

    def ctx(i, a):
        seen["account"] = a
        return ("t-personal", {"subject": "Becas", "message-id": "<p1@x>"})

    monkeypatch.setattr(g, "_reply_context", ctx)
    a = args(None)
    a.reply_account = "personal"
    raw, thread = g._build_message(a)
    msg = email.message_from_bytes(base64.urlsafe_b64decode(raw + "=" * (-len(raw) % 4)))
    assert seen["account"] == "personal"
    assert thread is None  # the personal mailbox's thread ID means nothing in baish
    assert msg["In-Reply-To"] == "<p1@x>"
    assert msg["Subject"] == "Re: Becas"


def test_reply_account_needs_reply_to():
    g = load()
    with pytest.raises(SystemExit):
        g._build_message(new_mail("Convenio", reply_account="personal"))
