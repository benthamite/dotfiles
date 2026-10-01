"""Hand-rolled LaunchAgent changes, ported from block-unregistered-launchagent.sh.

~/repos/launchd is the registry of record for launchd jobs, and its
bin/launchd-install.py does the whole install; this denies writing, loading
or unloading a job any other way. ALLOW_LAUNCHAGENT_CHANGE=1 is the override
for a change Pablo authorized.

The runtimes differ in the file-path branch: Claude reads one Edit/Write
path and, if there is one, never looks at a command; Codex checks every path
a call changes and then still checks the shell command. The command
classifier is the hook's embedded Python, unchanged.
"""

from __future__ import annotations

import os
import pwd
import re

from ..codex_payload import command_substitution, jq_text
from ..codex_paths import codex_changed_paths
from ..core import CODEX, Call, Result
from ..ere import ere, grep

_OVERRIDE = ere("(^|[[:space:]])ALLOW_LAUNCHAGENT_CHANGE=1([[:space:]]|$)")

INSTALL_GUIDANCE = """Install it through the launchd registry instead:

  python3 ~/repos/launchd/bin/launchd-install.py PLIST --purpose 'One sentence.'

That writes the canonical plist to ~/repos/launchd/agents/, symlinks the live
file, adds the registry/jobs.json entry, and runs the audit. Add --adopt for a
plist already sitting in ~/Library/LaunchAgents, --load to bootstrap it, and
--dry-run to see the plan first. Commit the launchd repo afterwards."""


def _file_reason(path: str) -> str:
    return f"""BLOCKED: writing a plist directly into ~/Library/LaunchAgents.

{path}

Live plists are symlinks to canonical files in ~/repos/launchd/agents/. Authoring
one in place creates a job the drift audit will flag as unregistered.

Write the plist somewhere else (the launchd repo, or a scratch path), then:

{INSTALL_GUIDANCE}"""


def _command_reason(kind: str, fragment: str) -> str:
    if kind == "write":
        return f"""BLOCKED: installing a plist into ~/Library/LaunchAgents by hand.

  {fragment}

Every job on this machine got here this way and had to be adopted into the
registry afterwards, always after the drift audit emailed about it.

{INSTALL_GUIDANCE}"""
    if kind == "load":
        return f"""BLOCKED: loading a LaunchAgent outside the launchd registry.

  {fragment}

A job loaded without a registry/jobs.json entry runs unmanaged until the next
drift audit finds it.

{INSTALL_GUIDANCE}

If the job is already registered and you are only reloading it, rerun with
ALLOW_LAUNCHAGENT_CHANGE=1 once Pablo has confirmed the reload."""
    return f"""BLOCKED: unloading or removing a LaunchAgent outside the launchd registry.

  {fragment}

If a job the registry still lists disappears, tomorrow's audit reports it as a
missing managed job. Retire the job and its registry entry together: remove the
managed_jobs entry in ~/repos/launchd/registry/jobs.json, delete the canonical
plist under agents/, and commit that change alongside the bootout.

For a deliberate temporary bootout that Pablo has confirmed, rerun with
ALLOW_LAUNCHAGENT_CHANGE=1."""


def classify(cmd: str, live_dir: str, home: str) -> tuple[str, str] | None:
    """The hook's embedded classifier: (kind, fragment) or None to allow."""
    # Drop heredoc bodies unless the body itself is being written into the live
    # directory — `cat > ~/Library/LaunchAgents/x.plist <<EOF` is exactly the
    # pattern this hook exists to stop, so the opener line is kept either way.
    kept, marker = [], None
    for line in cmd.split("\n"):
        if marker is not None:
            if line.strip() == marker:
                marker = None
            continue
        kept.append(line)
        opener = re.search(r"<<-?\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1", line)
        if opener:
            marker = opener.group(2)
    stripped = "\n".join(kept)

    # The sanctioned path. Anything routed through the installer is fine, including
    # the launchctl bootstrap it runs under --load.
    if re.search(r"launchd-install(?:\.py)?\b", stripped):
        return None

    live_patterns = [
        re.escape(live_dir),
        re.escape(live_dir.replace(home, "~")),
        r"~/Library/LaunchAgents",
        r"\$HOME/Library/LaunchAgents",
        r"Library/LaunchAgents",
    ]
    live_re = "(?:" + "|".join(live_patterns) + r")\S*"

    # Command position: start of string, or after a newline or shell operator.
    pos = r"(?:^|[\n;&|(\x60])\s*(?:[A-Za-z_][A-Za-z0-9_]*=\S*\s+)*"

    # 1. Writing into the live directory.
    write_verbs = r"(?:cp|mv|ln|install|touch|tee|rsync|plutil|defaults|sed|perl|python3?|ruby|awk)"
    for match in re.finditer(pos + write_verbs + r"\b[^\n;&|]*", stripped):
        fragment = match.group(0)
        if not re.search(live_re, fragment):
            continue
        # plutil -p and defaults read only inspect.
        if re.search(r"\bplutil\b[^\n]*\s-p\b", fragment):
            continue
        if re.search(r"\bdefaults\s+read\b", fragment):
            continue
        return "write", fragment.strip()

    # 2. Redirection into the live directory: `... > ~/Library/LaunchAgents/x.plist`
    match = re.search(r">>?\s*(" + live_re + ")", stripped)
    if match:
        return "write", match.group(0).strip()

    # 3. Loading or unloading a service.
    load_re = pos + r"launchctl\s+(bootstrap|bootout|load|unload|enable|disable|remove|submit)\b[^\n;&|]*"
    match = re.search(load_re, stripped)
    if match:
        kind = "load" if match.group(1) in ("bootstrap", "load", "enable", "submit") else "unload"
        return kind, match.group(0).strip()
    return None


def bash_home(env: dict) -> str:
    """$HOME in bash, which sets it from the password database when unset."""
    return env["HOME"] if "HOME" in env else pwd.getpwuid(os.getuid()).pw_dir


def _python_home(env: dict) -> str:
    """`os.path.expanduser("~")` in the hook's python3 child, which inherited ENV."""
    return bash_home(env).rstrip("/") or "/"


def decide(call: Call) -> Result:
    live_dir = bash_home(call.env) + "/Library/LaunchAgents"
    if call.runtime == CODEX:
        for path in codex_changed_paths(call.raw):
            if path.startswith(live_dir + "/"):
                return Result.deny(_file_reason(path))
    else:
        path = command_substitution(jq_text(call.tool_input.get("file_path")))
        if path:
            return Result.deny(_file_reason(path)) if path.startswith(live_dir + "/") else Result.allow()
    command = call.shell_command()
    if not command or ("LaunchAgents" not in command and "launchctl" not in command):
        return Result.allow()
    if grep(_OVERRIDE, command):
        return Result.allow()
    verdict = classify(command, live_dir, _python_home(call.env))
    if verdict is None:
        return Result.allow()
    return Result.deny(_command_reason(*verdict))
