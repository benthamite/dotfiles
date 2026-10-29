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
