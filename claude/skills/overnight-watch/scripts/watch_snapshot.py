#!/usr/bin/env python3
"""Print one read-only snapshot of every live agent.el session.

For each session other than the caller's own, show what the overnight watcher
needs to decide whether it is working, legitimately waiting on Pablo, or stuck:
the agent.el state, how long ago its transcript and any subagent or workflow
transcript were written, its last assistant message and `Status:' line, and
the bottom of its Emacs terminal screen (where permission dialogs, API errors
and unsent composer text appear without reaching the transcript).

Composer text drawn in the faint face is Claude Code's prompt suggestion, not
input; the screen dump marks faint runs as {faint:...}.
"""
import json
from datetime import datetime
import os
import re
import subprocess
import sys
import tempfile
import time
from pathlib import Path

AGENT_SEND = Path.home() / "My Drive/dotfiles/bin/agent-send"
SESSIONS_CACHE = Path.home() / ".cache/overnight-watch/sessions.json"
SCREEN_LINES = 18
TEXT_CHARS = 1200

# Renders the bottom of a buffer, wrapping faint-face runs in {faint:...}.
SCREEN_ELISP = r"""
(with-current-buffer %s
  (save-excursion
    (goto-char (point-max))
    (forward-line (- %d))
    (let ((pos (point)) (out ""))
      (while (< pos (point-max))
        (let* ((next (next-single-property-change pos 'face nil (point-max)))
               (face (get-text-property pos 'face))
               (faint (and (listp face)
                           (memq 'eat-term-faint (plist-get face :inherit))))
               (text (buffer-substring-no-properties pos next)))
          (setq out (concat out (if faint (format "{faint:%%s}" text) text)))
          (setq pos next)))
      (let ((coding-system-for-write 'utf-8))
        (write-region out nil %s))
      t)))
"""


def age(path):
    try:
        return int(time.time() - os.path.getmtime(path))
    except OSError:
        return None


def fmt_age(seconds):
    if seconds is None:
        return "missing"
    return f"{seconds // 60}m{seconds % 60:02d}s ago"


def local(ts):
    """ISO UTC timestamp as local time."""
    try:
        return datetime.fromisoformat(ts.replace("Z", "+00:00")).astimezone().strftime("%Y-%m-%d %H:%M:%S")
    except (AttributeError, ValueError):
        return ts


def newest_subagent(transcript):
    """Newest write under the session's subagents directory (Claude only)."""
    base = Path(transcript).with_suffix("") / "subagents"
    newest = None
    if base.is_dir():
        for p in base.rglob("*.jsonl"):
            a = age(p)
            if a is not None and (newest is None or a < newest[0]):
                newest = (a, p)
    return newest


def last_assistant_text(transcript):
    """Last assistant prose in a Claude or Codex transcript."""
    try:
        with open(transcript, "rb") as f:
            f.seek(0, 2)
            f.seek(max(0, f.tell() - 2_000_000))
            lines = f.read().decode("utf-8", "replace").splitlines()
    except OSError:
        return None, None
    for line in reversed(lines):
        try:
            r = json.loads(line)
        except ValueError:
            continue
        # Claude Code
        if r.get("type") == "assistant":
            texts = [b.get("text", "") for b in r.get("message", {}).get("content", [])
                     if isinstance(b, dict) and b.get("type") == "text"]
            if texts:
                return r.get("timestamp"), "\n".join(texts)
        # Codex
        p = r.get("payload") or {}
        if p.get("type") == "message" and p.get("role") == "assistant":
            texts = [b.get("text", "") for b in p.get("content", [])
                     if isinstance(b, dict) and b.get("type") == "output_text"]
            if texts:
                return r.get("timestamp"), "\n".join(texts)
    return None, None


def screen(buffer):
    # emacsclient garbles long non-ASCII return values, so Emacs writes the
    # text to a private temporary file instead.
    fd, path = tempfile.mkstemp(prefix="overnight-watch-", suffix=".txt")
    os.close(fd)
    try:
        form = SCREEN_ELISP % (json.dumps(buffer), SCREEN_LINES, json.dumps(path))
        try:
            out = subprocess.run(["emacsclient", "--eval", form], capture_output=True,
                                 timeout=15)
        except subprocess.TimeoutExpired:
            return "(emacsclient timed out)"
        if out.returncode != 0:
            return f"(emacsclient failed: {out.stderr.decode('utf-8', 'replace').strip()[:200]})"
        text = Path(path).read_text(encoding="utf-8", errors="replace")
    finally:
        os.unlink(path)
    lines = [l.rstrip() for l in text.splitlines()]
    lines = [l for l in lines if l and not re.fullmatch(r"[─━\s]+", l)]
    return "\n".join(lines)


def main():
    own = os.environ.get("CLAUDE_BUFFER_NAME") or os.environ.get("CODEX_BUFFER_NAME")
    print(f"# snapshot {time.strftime('%Y-%m-%d %H:%M:%S')}; load {os.getloadavg()[0]:.0f}")
    listing = subprocess.run([str(AGENT_SEND), "--list", "--json"], capture_output=True, text=True)
    emacs_up = listing.returncode == 0
    if emacs_up:
        sessions = json.loads(listing.stdout)
        SESSIONS_CACHE.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
        SESSIONS_CACHE.write_text(listing.stdout)
    else:
        # Labeled fallback: during an Emacs server outage the sessions' own
        # processes keep running, so their transcripts still show progress.
        # Use the last successful listing and say so; screens are unavailable.
        print(f"# EMACS SERVER UNREACHABLE: {listing.stderr.strip()[-300:]}")
        if not SESSIONS_CACHE.exists():
            print("# no cached session list; cannot continue")
            return 1
        print(f"# using session list cached {fmt_age(age(SESSIONS_CACHE))}; "
              "states are stale and screens are skipped")
        sessions = json.loads(SESSIONS_CACHE.read_text())
    for s in sessions:
        if s["buffer"] == own:
            continue
        t = s.get("transcript") or ""
        print(f"\n=== {s['buffer']}")
        print(f"state={s['state']} dir={s['directory']}")
        print(f"transcript={t} written {fmt_age(age(t))}")
        sub = newest_subagent(t) if t else None
        if sub:
            print(f"newest subagent write {fmt_age(sub[0])}: {sub[1].name}")
        ts, text = last_assistant_text(t) if t else (None, None)
        if text:
            status = [l for l in text.splitlines() if l.startswith("Status:")]
            print(f"last assistant message at {local(ts)}:")
            print(text[-TEXT_CHARS:])
            print(f"STATUS LINE: {status[-1] if status else '(none)'}")
        if emacs_up:
            print("--- screen bottom")
            print(screen(s["buffer"]))


if __name__ == "__main__":
    sys.exit(main())
