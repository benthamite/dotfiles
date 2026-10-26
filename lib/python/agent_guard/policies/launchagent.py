"""Hand-rolled LaunchAgent changes, ported from block-unregistered-launchagent.sh.

~/repos/launchd is the registry of record for launchd jobs, and its
bin/launchd-install.py does the whole install; this denies writing, loading
or unloading a job any other way, and bin/launchd-retire.py is the only way to
remove one. ALLOW_LAUNCHAGENT_CHANGE=1 is the override for a load or unload
Pablo authorized; it never covers removing a live plist, because a job removed
outside the registry is reported as missing by the next audit.

The runtimes differ in the file-path branch: Claude reads one Edit/Write
path and, if there is one, never looks at a command; Codex checks every path
a call changes and then still checks the shell command. The command
classifier began as the hook's embedded Python; since then it tracks shell
variables holding a live path and judges plutil by where it writes.
"""

from __future__ import annotations

import os
import pwd
import re
import shlex

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

RETIRE_GUIDANCE = """Retire it through the launchd registry instead:

  python3 ~/repos/launchd/bin/launchd-retire.py LABEL --reason 'One sentence.'

That boots the job out, trashes its live and canonical plists, removes the
registry/jobs.json entry, and runs the audit. --dry-run shows the plan. Commit
the launchd repo afterwards."""


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

If the job is already registered and you are only reloading it, use the
reloader once Pablo has confirmed the reload; it records the load time the
audit judges the job's schedule from:

  python3 ~/repos/launchd/bin/launchd-reload.py LABEL..."""
    if kind == "remove":
        return f"""BLOCKED: removing a plist from ~/Library/LaunchAgents by hand.

  {fragment}

A job removed while the registry still lists it is reported as a missing
managed job by the next audit. ALLOW_LAUNCHAGENT_CHANGE=1 does not cover this.

{RETIRE_GUIDANCE}"""
    return f"""BLOCKED: unloading or removing a LaunchAgent outside the launchd registry.

  {fragment}

If a job the registry still lists disappears, the next audit reports it as a
missing managed job.

{RETIRE_GUIDANCE}

For a deliberate temporary bootout that Pablo has confirmed, rerun with
ALLOW_LAUNCHAGENT_CHANGE=1."""


# plutil options that take an argument, and how many (see `man plutil`).
_PLUTIL_ARGS = {
    "-convert": 1, "-create": 1, "-extract": 2, "-insert": 1, "-replace": 1,
    "-remove": 1, "-type": 1, "-expect": 1, "-o": 1, "-e": 1,
    "-bool": 1, "-integer": 1, "-float": 1, "-string": 1, "-date": 1,
    "-data": 1, "-xml": 1, "-json": 1,
}
_PLUTIL_READ_OPS = {None, "-p", "-lint", "-type", "-help"}
_PLUTIL_OPS = {"-p", "-lint", "-help", "-type", "-convert", "-create", "-extract",
               "-insert", "-replace", "-remove"}


def _plutil_targets(fragment: str) -> list[str]:
    """Paths a `plutil ...` fragment writes, per `man plutil` and observed behavior.

    -p, -lint (the default), -type and -help only read. Every other operation
    writes its result to `-o PATH` when given (`-o -` is stdout), else to
    FILE.EXT for `-e EXT`, else back over each input file. `-extract KEY raw`
    without -o prints to stdout; any other -extract format without -o
    overwrites the input file in place (checked against plutil on macOS 27).
    """
    text = fragment.strip().rstrip(")\x60").strip()
    try:
        tokens = shlex.split(text, posix=True)
    except ValueError:
        tokens = text.split()
    while tokens and re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*=.*", tokens[0]):
        tokens.pop(0)
    tokens = tokens[1:]  # plutil itself
    op = fmt = out = None
    files: list[str] = []
    i = 0
    while i < len(tokens):
        tok = tokens[i]
        if re.match(r"\d*[<>]", tok):
            # Redirections are judged by the redirection check; skip the operand.
            i += 2 if re.fullmatch(r"\d*[<>]{1,2}&?", tok) else 1
            continue
        if tok == "--":
            files.extend(tokens[i + 1:])
            break
        if tok in _PLUTIL_OPS and op is None:
            op = tok
            if tok == "-extract" and i + 2 < len(tokens):
                fmt = tokens[i + 2]
        if tok == "-o" and i + 1 < len(tokens):
            out = tokens[i + 1]
        if tok in _PLUTIL_ARGS:
            i += 1 + _PLUTIL_ARGS[tok]
            continue
        if not tok.startswith("-"):
            files.append(tok)
        i += 1
    if op in _PLUTIL_READ_OPS:
        return []
    if out is not None:
        return [] if out == "-" else [out]
    if op == "-extract" and fmt == "raw":
        return []
    return files


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

    # The sanctioned paths. Anything routed through the installer, the
    # reloader or the retirer is fine, including the launchctl calls they make.
    if re.search(r"launchd-(?:install|retire|reload)(?:\.py)?\b", stripped):
        return None

    live_patterns = [
        re.escape(live_dir),
        re.escape(live_dir.replace(home, "~")),
        r"~/Library/LaunchAgents",
        r"\$HOME/Library/LaunchAgents",
        r"Library/LaunchAgents",
    ]
    live_re = "(?:" + "|".join(live_patterns) + r")\S*"

    # A shell variable assigned a live path (`p=~/Library/LaunchAgents/x.plist`)
    # stands for that path wherever it is expanded later in the command.
    live_vars = {
        m.group(1)
        for m in re.finditer(r"(?:^|[\s;&|(\x60])([A-Za-z_][A-Za-z0-9_]*)=([^\s;&|]*)", stripped)
        if re.search(live_re, m.group(2))
    }
    live_ref = live_re
    if live_vars:
        names = "|".join(sorted(map(re.escape, live_vars)))
        live_ref = "(?:" + live_re + r"|\$\{?(?:" + names + r")\b\}?\S*)"

    # Command position: start of string, or after a newline or shell operator,
    # past any leading `NAME=value` assignments. A value stops at an operator,
    # so `p=x; plutil ...` puts plutil, not the assignment, in command position.
    pos = r"(?:^|[\n;&|(\x60])\s*(?:[A-Za-z_][A-Za-z0-9_]*=[^\s;&|]*\s+)*"

    # 1. Removing a live plist.
    remove_verbs = r"(?:rm|trash|unlink|srm)"
    for match in re.finditer(pos + remove_verbs + r"\b[^\n;&|]*", stripped):
        if re.search(live_ref, match.group(0)):
            return "remove", match.group(0).strip()

    # 2. Writing into the live directory.
    write_verbs = r"(?:cp|mv|ln|install|touch|tee|rsync|plutil|defaults|sed|perl|python3?|ruby|awk)"
    for match in re.finditer(pos + r"(" + write_verbs + r")\b[^\n;&|]*", stripped):
        fragment = match.group(0)
        if not re.search(live_ref, fragment):
            continue
        # plutil writes only where _plutil_targets says; defaults read only inspects.
        if match.group(1) == "plutil":
            if not any(re.search(live_ref, target) for target in _plutil_targets(fragment)):
                continue
        if re.search(r"\bdefaults\s+read\b", fragment):
            continue
        return "write", fragment.strip()

    # 3. Redirection into the live directory: `... > ~/Library/LaunchAgents/x.plist`
    match = re.search(r">>?\s*(" + live_ref + ")", stripped)
    if match:
        return "write", match.group(0).strip()

    # 4. Loading or unloading a service.
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
    verdict = classify(command, live_dir, _python_home(call.env))
    if verdict is None:
        return Result.allow()
    if verdict[0] != "remove" and grep(_OVERRIDE, command):
        return Result.allow()
    return Result.deny(_command_reason(*verdict))
