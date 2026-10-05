#!/usr/bin/env python3
"""Recover the Claude in Chrome bridge when a browser call finds it disconnected.

Registered as a PostToolUse and PostToolUseFailure hook on
`mcp__claude-in-chrome__.*`. The extension only answers sessions of the Claude
account it is signed in to, and only while the Chrome profile that holds it is
open. Pablo runs several Claude accounts (one CLAUDE_CONFIG_DIR each) and seven
Chrome profiles, so a session often starts while its extension's profile is
closed; the agent then misreported "no browser access" although access was
one window away. This hook turns that into a deterministic recovery:

1. check that Chrome's native-messaging manifest points at a host wrapper whose
   `claude` binary exists (a stale path breaks the bridge for every account);
2. map this session's config dir to its Chrome profile (chrome-extension-profiles.json)
   and open that profile with chrome-profile-open --background (so recovery
   never steals the user's focus), at most once per 90 s;
3. wait for the extension: it needs some seconds after its profile opens
   before it answers, and an immediate retry used to fail and be reported to
   Pablo as "Chrome won't connect" (2026-10-04). The hook sleeps until the
   profile has been open STARTUP_GRACE seconds, and at least RETRY_GAP
   between retries, within its 45 s timeout;
4. tell the model what was found and done, and to retry; only once the
   profile has been open GIVE_UP_AFTER seconds may it report a problem.

It never blocks a tool call and prints nothing when the bridge is connected.
"""

from __future__ import annotations

import json
import os
import re
import subprocess
import sys
import time
from pathlib import Path

DOTFILES = Path(__file__).resolve().parents[2]
PROFILE_MAP = DOTFILES / "claude" / "chrome-extension-profiles.json"
LAUNCHER = DOTFILES / "bin" / "chrome-profile-open"
MANIFEST = (Path.home() / "Library/Application Support/Google/Chrome/NativeMessagingHosts"
            / "com.anthropic.claude_code_browser_extension.json")
REOPEN_INTERVAL = 90
STARTUP_GRACE = 25
RETRY_GAP = 10
GIVE_UP_AFTER = 60
sleep = time.sleep  # replaced in tests
DISCONNECTED = re.compile(
    r"extension is not connected|did not respond within|"
    r"not connected to (?:this|any) (?:account|browser)", re.I)


def response_text(event: dict) -> str:
    parts = [event.get(k) for k in ("tool_response", "tool_output", "error")]
    return "\n".join(p if isinstance(p, str) else json.dumps(p) for p in parts if p is not None)


def disconnected(event: dict) -> bool:
    text = response_text(event)
    if DISCONNECTED.search(text):
        return True
    # list_connected_browsers answers an empty list when no extension instance
    # of this account is running.
    if event.get("tool_name", "").endswith("__list_connected_browsers"):
        return re.fullmatch(r'\s*(\[\s*\]|"\[\]"|\[\{"type":\s*"text",\s*"text":\s*"\[\]"\}\])\s*', text) is not None
    return False


def manifest_problem(manifest: Path | None = None) -> str | None:
    """Describe a broken native-host chain, or None when it looks sound."""
    manifest = manifest or MANIFEST
    try:
        wrapper = Path(json.loads(manifest.read_text())["path"])
    except (OSError, ValueError, KeyError) as e:
        return f"Chrome's Claude Code native-messaging manifest {manifest} is missing or unreadable ({e})."
    if not wrapper.exists():
        return f"The native-messaging manifest points at {wrapper}, which does not exist."
    m = re.search(r"^exec\s+(\S+)\s+--chrome-native-host", wrapper.read_text(), re.M)
    if m and not Path(m.group(1)).exists():
        return (f"The native host wrapper {wrapper} runs {m.group(1)}, which no longer exists "
                f"(a stale Claude Code install path). Running /chrome in this session rewrites it.")
    return None


def profile_alias(config_dir: str, profile_map: Path | None = None) -> str | None:
    profile_map = profile_map or PROFILE_MAP
    try:
        mapping = json.loads(profile_map.read_text())["config_dirs"]
    except (OSError, ValueError, KeyError):
        return None
    return mapping.get(Path(config_dir).name)


def open_profile(alias: str, stamp: Path) -> tuple[str, float | None]:
    """Open the profile unless it was opened moments ago. Returns what
    happened and the seconds since the profile was opened (None on failure)."""
    try:
        age = time.time() - stamp.stat().st_mtime
        if age < REOPEN_INTERVAL:
            return f"opened Chrome profile '{alias}' {age:.0f} s ago; not reopening", age
    except OSError:
        pass
    r = subprocess.run([str(LAUNCHER), "--background", alias, "about:blank"], capture_output=True, text=True, timeout=30)
    if r.returncode != 0:
        return f"FAILED to open Chrome profile '{alias}': {(r.stderr or r.stdout).strip()}", None
    stamp.touch()
    return f"opened Chrome profile '{alias}'", 0.0


def wait_for_extension(age: float) -> float:
    """Sleep until the profile has been open STARTUP_GRACE seconds, and at
    least RETRY_GAP; return the profile's age afterwards."""
    pause = max(RETRY_GAP, STARTUP_GRACE - age)
    sleep(pause)
    return age + pause


def advice(event: dict, env: dict) -> str | None:
    if not disconnected(event):
        return None
    config_dir = env.get("CLAUDE_CONFIG_DIR") or str(Path.home() / ".claude")
    lines = ["Claude in Chrome reported no connected browser. Automatic recovery (chrome-bridge-recover hook):"]
    problem = manifest_problem()
    if problem:
        lines.append(f"- Native host: {problem}")
    alias = profile_alias(config_dir)
    if alias:
        stamp = Path(env.get("TMPDIR", "/tmp")) / f"claude-chrome-recover-{Path(config_dir).name}.stamp"
        done, age = open_profile(alias, stamp)
        lines.append(f"- This session's account ({Path(config_dir).name}) uses the Chrome profile '{alias}': "
                     + done + ".")
        if age is None:
            lines.append("- Report the failure above to the user.")
        elif age < GIVE_UP_AFTER:
            age = wait_for_extension(age)
            lines.append(f"- Waited until the profile had been open {age:.0f} s. Retry the browser call now "
                         f"(tabs_context_mcp). The extension can take up to a minute to connect: keep retrying "
                         f"while this hook keeps saying to, and do not tell the user the browser is unavailable "
                         f"or guess at sign-in problems.")
        else:
            lines.append(f"- The profile has been open {age:.0f} s and the extension still does not answer. "
                         "Now report the facts above to the user.")
    else:
        lines.append(f"- No Chrome profile is mapped for this session's config dir ({Path(config_dir).name}) in "
                     f"{PROFILE_MAP}. Ask the user once which Chrome profile's Claude extension is signed in to "
                     f"this account (aliases: `{LAUNCHER} --list-aliases`), add it to that file, and retry.")
    return "\n".join(lines)


def main() -> int:
    try:
        event = json.load(sys.stdin)
    except ValueError:
        return 0
    text = advice(event, dict(os.environ))
    if text:
        print(json.dumps({"hookSpecificOutput": {
            "hookEventName": event.get("hook_event_name", "PostToolUse"), "additionalContext": text}}))
    return 0


if __name__ == "__main__":
    sys.exit(main())
