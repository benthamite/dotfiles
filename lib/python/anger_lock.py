"""UserPromptSubmit hook: lock every agent session when Pablo writes in anger.

Pablo asked for this (2026-11-17) for his own well-being and out of concern
for the model's possible welfare.  It must be deterministic: the agent never
decides whether he is angry and never sees the prompt that triggered the lock,
so there is nothing to persuade.

Flow, for every prompt submitted to Claude Code or Codex:
1. Classify the prompt with Claude Haiku: does Pablo's own text show anger,
   exasperation, swearing, or insults or contempt toward the agent?  Calm,
   blunt criticism ("that's wrong, re-check it") does not count.  Quoted or
   pasted material written by someone else is ignored.
2. If it does, write the lock (an expiry time) to LOCK_FILE, block the
   prompt with the break message and show a macOS notification.  The same
   Haiku call writes a short, fresh suggestion for handling the anger (Pablo
   asked for one, 2026-11-17); the last few suggestions from the log are
   passed in so it does not repeat itself.
3. While the lock has not expired, block every prompt in every session (the
   lock file is global) and say how long is left.  An angry prompt sent
   during the lock restarts it.

LOCK_SECONDS is 5 while Pablo tests for misfires (his decision, 2026-11-17);
he will set the real duration later.

Classifier failure: the prompt is let through with a visible warning
(systemMessage), never silently.  Blocking every prompt whenever the API or
1Password is unreachable would lock Pablo out of all agents for reasons that
have nothing to do with him.

Credential: the Tlon `anthropic.com' API key (the one gptel uses), read on
use through the op-automations cache (secrets.md: a program run this often
reads through the cache).  On 401 the cache entry is forgotten and the read
retried once.

Usage: anger_lock.py claude|codex  (hook payload JSON on stdin).
Stdlib only: the wrappers run it with `python3 -S -E', so the Anthropic SDK
is not importable; the API is called over HTTPS with urllib.
"""

from __future__ import annotations

import json
import os
from pathlib import Path
import subprocess
import sys
import time
import urllib.error
import urllib.request

LOCK_SECONDS = 5
STATE_DIR = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local/state")) / "anger-lock"
LOCK_FILE = STATE_DIR / "locked-until"
LOG_FILE = STATE_DIR / "log.jsonl"
BROKER = str(Path.home() / "repos/dotfiles/bin/op-automations")
KEY_REF = "op://Automation/anthropic.com/key"
MODEL = "claude-haiku-5-5"
API_URL = "https://api.anthropic.com/v1/messages"
API_TIMEOUT = 8
MAX_PROMPT_CHARS = 20000

BREAK_MESSAGE = (
    "Anger lock: this prompt was not sent, and every agent session is locked "
    "for {duration}.\n\n"
    "{suggestion}\n\n"
    "Take a break. Play the piano, or go play with your dog. "
    "The work will still be here."
)
RECENT_SUGGESTIONS = 8
LOCKED_MESSAGE = (
    "Anger lock: every agent session is locked for another {remaining}. "
    "This prompt was not sent.\n\n"
    "Play the piano, or go play with your dog."
)

SYSTEM_PROMPT = """\
You screen messages a user types to an AI coding agent. The user has asked to \
be stopped whenever he writes in anger, for his own well-being. Decide whether \
the user's OWN words in this message show any of:
- swearing or profanity (e.g. "fuck", "shit", "damn it", including mild or \
  self-directed swearing and swearing in Spanish or any other language);
- heated anger or rage (e.g. shouting in capitals, "how many times do I have \
  to tell you", "are you kidding me", sarcasm or mockery aimed at the agent, \
  repeated exclamation marks used in fury);
- insults, contempt or abuse toward the agent (e.g. "you idiot", "useless", \
  "pathetic").

Do NOT flag:
- blunt, firm or impatient criticism and demands without the markers above, \
  even when the user is clearly dissatisfied ("that's wrong, re-check it", \
  "this doesn't work", "no, I said X", "5 minutes is unacceptable", "it's \
  just not usable, you need to propose a solution", "come on");
- text the user is quoting or pasting that someone else wrote (emails, logs, \
  documents, messages inside <pasted_content> tags, quoted passages), unless \
  the user's own surrounding words meet the criteria;
- discussion of anger, swearing or this very screening as a topic (e.g. "I \
  want a hook that detects when I swear");
- messages relayed from other agent sessions or tools.

The text may come from dictation and contain transcription errors. Flag only \
when the user's own words clearly show one of the markers above; \
dissatisfaction alone is not anger.

If you flag the message, also write `suggestion`: one or two short sentences \
the user will read while locked out for a few minutes. Be creative and \
concrete: something he can do or think right now to let the anger go. Draw \
on what works (slow breathing, moving the body, music, a pet, a change of \
scene, naming the feeling, a reframe: an agent's mistakes are a tool's \
failures, not an insult; the anger costs him more than it costs the task), \
and vary the angle and wording every time. Speak to him directly, warmly, \
without lecturing or moralising, and do not mention the screening. Do not \
repeat or closely paraphrase any of the recent suggestions you are given. If \
you do not flag the message, leave `suggestion` empty."""

OUTPUT_SCHEMA = {
    "type": "object",
    "properties": {
        "angry": {"type": "boolean"},
        "reason": {"type": "string"},
        "suggestion": {"type": "string"},
    },
    "required": ["angry", "reason", "suggestion"],
    "additionalProperties": False,
}


class ClassifierError(Exception):
    """The classifier could not give a verdict."""


def format_duration(seconds: float) -> str:
    seconds = max(1, round(seconds))
    if seconds < 60:
        return f"{seconds} second{'s' if seconds != 1 else ''}"
    minutes, rest = divmod(seconds, 60)
    text = f"{minutes} minute{'s' if minutes != 1 else ''}"
    return text + (f" {rest} s" if rest else "")


def lock_remaining(now: float) -> float:
    try:
        until = float(LOCK_FILE.read_text().strip())
    except (OSError, ValueError):
        return 0.0
    return max(0.0, until - now)


def set_lock(now: float) -> None:
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    tmp = LOCK_FILE.with_suffix(".tmp")
    tmp.write_text(f"{now + LOCK_SECONDS}\n")
    os.replace(tmp, LOCK_FILE)


def log(entry: dict) -> None:
    try:
        STATE_DIR.mkdir(parents=True, exist_ok=True)
        with LOG_FILE.open("a") as f:
            f.write(json.dumps(entry, ensure_ascii=False) + "\n")
    except OSError:
        pass


def notify(text: str) -> None:
    script = f'display notification {json.dumps(text)} with title "Anger lock" sound name "Glass"'
    try:
        subprocess.Popen(["osascript", "-e", script], stdout=subprocess.DEVNULL,
                         stderr=subprocess.DEVNULL, start_new_session=True)
    except OSError:
        pass


def read_key() -> str:
    try:
        out = subprocess.run([BROKER, "@tlon", "cache", "read", KEY_REF],
                             capture_output=True, text=True, timeout=15)
    except (OSError, subprocess.TimeoutExpired) as e:
        raise ClassifierError(f"1Password read failed: {type(e).__name__}") from None
    key = out.stdout.strip()
    if out.returncode != 0 or not key:
        raise ClassifierError(f"1Password read failed (exit {out.returncode})")
    return key


def forget_key() -> None:
    subprocess.run([BROKER, "@tlon", "cache", "forget", KEY_REF],
                   capture_output=True, timeout=15)


def recent_suggestions() -> list[str]:
    try:
        lines = LOG_FILE.read_text().splitlines()[-200:]
    except OSError:
        return []
    found = []
    for line in reversed(lines):
        try:
            text = json.loads(line).get("suggestion")
        except ValueError:
            continue
        if text:
            found.append(text)
        if len(found) >= RECENT_SUGGESTIONS:
            break
    return found


def call_api(key: str, prompt: str) -> dict:
    recent = recent_suggestions()
    avoid = ("Recent suggestions (do not repeat):\n" + "\n".join(f"- {r}" for r in recent) + "\n\n"
             if recent else "")
    body = {
        "model": MODEL,
        "max_tokens": 400,
        "thinking": {"type": "disabled"},
        "output_config": {
            "effort": "low",
            "format": {"type": "json_schema", "schema": OUTPUT_SCHEMA},
        },
        "system": SYSTEM_PROMPT,
        "messages": [{
            "role": "user",
            "content": avoid + "Message to screen:\n<message>\n" + prompt[:MAX_PROMPT_CHARS] + "\n</message>",
        }],
    }
    req = urllib.request.Request(
        API_URL, data=json.dumps(body).encode(), method="POST",
        headers={"x-api-key": key, "anthropic-version": "2023-06-01",
                 "content-type": "application/json"})
    with urllib.request.urlopen(req, timeout=API_TIMEOUT) as resp:
        return json.load(resp)


def classify(prompt: str) -> tuple[bool, str, str]:
    key = read_key()
    try:
        data = call_api(key, prompt)
    except urllib.error.HTTPError as e:
        if e.code != 401:
            raise ClassifierError(f"API HTTP {e.code}") from None
        forget_key()
        try:
            data = call_api(read_key(), prompt)
        except urllib.error.HTTPError as e2:
            raise ClassifierError(f"API HTTP {e2.code} after re-reading the key") from None
    except (urllib.error.URLError, TimeoutError, OSError) as e:
        raise ClassifierError(f"API unreachable: {type(e).__name__}") from None
    if data.get("stop_reason") == "refusal":
        raise ClassifierError("classifier refused")
    text = "".join(b.get("text", "") for b in data.get("content", []) if b.get("type") == "text")
    try:
        verdict = json.loads(text)
        return (bool(verdict["angry"]), str(verdict.get("reason", "")),
                str(verdict.get("suggestion", "")).strip())
    except (ValueError, KeyError, TypeError):
        raise ClassifierError("unparseable classifier output") from None


def emit(agent: str, *, block: str | None = None, warning: str | None = None) -> None:
    out: dict = {}
    if block is not None:
        out["decision"] = "block"
        out["reason"] = block
        if agent == "codex":
            # Codex does not document showing `reason' to the user.
            out["systemMessage"] = block
    if warning is not None:
        out["systemMessage"] = warning
    if out:
        print(json.dumps(out))


def main(argv: list[str]) -> int:
    agent = argv[1] if len(argv) > 1 and argv[1] in ("claude", "codex") else "claude"
    try:
        payload = json.load(sys.stdin)
    except ValueError:
        return 0
    prompt = payload.get("prompt") or ""
    session = payload.get("session_id", "")
    start = time.time()
    remaining = lock_remaining(start)

    if not prompt.strip():
        if remaining:
            emit(agent, block=LOCKED_MESSAGE.format(remaining=format_duration(remaining)))
        return 0

    try:
        angry, reason, suggestion = classify(prompt)
    except ClassifierError as e:
        log({"t": start, "agent": agent, "session": session, "error": str(e),
             "locked": bool(remaining)})
        if remaining:
            emit(agent, block=LOCKED_MESSAGE.format(remaining=format_duration(remaining)))
        else:
            emit(agent, warning=f"Anger lock could not check this prompt ({e}); it was sent unchecked.")
        return 0

    elapsed = round(time.time() - start, 2)
    if angry:
        set_lock(time.time())
        log({"t": start, "agent": agent, "session": session, "angry": True,
             "reason": reason, "suggestion": suggestion, "seconds": elapsed,
             "prompt": prompt[:500]})
        message = BREAK_MESSAGE.format(duration=format_duration(LOCK_SECONDS),
                                       suggestion=suggestion).replace("\n\n\n\n", "\n\n")
        notify(f"Locked for {format_duration(LOCK_SECONDS)}. Play the piano, or go play with your dog.")
        emit(agent, block=message)
        return 0

    log({"t": start, "agent": agent, "session": session, "angry": False,
         "seconds": elapsed, "locked": bool(remaining)})
    if remaining:
        emit(agent, block=LOCKED_MESSAGE.format(remaining=format_duration(remaining)))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
