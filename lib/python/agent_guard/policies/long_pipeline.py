"""Unprepared multi-hour pipeline launches, ported from claude/hooks/block-long-pipeline-run-unchecked.sh.

Claude only, run through the Bash dispatcher. In the consensus-trader repo a
launch of scripts/setup_db.py --full/--target or scripts/refresh_data.sh is
denied unless the pipeline code is committed, checkpointing is on and the
command carries PREFLIGHT=ok.
"""

from __future__ import annotations

import subprocess

from ..codex_payload import command_substitution
from ..core import CLAUDE, Call, Result
from ..ere import ere, grep
from ..repo import repo_root

_LAUNCHES = (ere(r"(python3?|nohup)[^|]*setup_db\.py[^|]*(--full|--target)"),
             ere(r"(bash|sh|nohup|\./|source )[^|]*refresh_data\.sh"))
_NO_CHECKPOINT = ere(r"PIPELINE_CHECKPOINT=0")
_PREFLIGHT = ere(r"\bPREFLIGHT=ok\b")

# Intended difference: the hook built the reason with "\n" inside double
# quotes, so it showed backslash-n where these line breaks are.
_CHECKLIST = (
    "\n\nPre-flight checklist before committing hours of compute:\n  - Every analogous phase hardened? (e.g. ALL "
    "big-table scans chunked, not just the one that crashed last time — grep the code path, don't fix reactively.)"
    "\n  - Checkpoint/resume on (default) so any crash resumes in minutes?\n  - Runtime range quoted and (if >30 "
    "min) approved?\n  - Kill-switch / early-failure check defined?\n  - Code committed and tests green for the "
    "whole path being exercised?\n\nWhen all of the above hold, re-run with PREFLIGHT=ok prefixed.")


def decide(call: Call) -> Result:
    command = call.shell_command()
    if not any(grep(launch, command, echo=True) for launch in _LAUNCHES):
        return Result.allow()
    ctx = repo_root(command, call.raw, CLAUDE, call.env)
    root = ctx.root or ctx.pwd
    if not root.endswith("consensus-trader"):
        return Result.allow()
    failures = ""
    result = subprocess.run(["git", "-C", root, "status", "--porcelain", "--", "consensus_trader", "scripts"],
                            cwd=ctx.cwd, env=call.env, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    dirty = command_substitution(result.stdout.decode("utf-8", "surrogateescape"))
    if dirty:
        indented = "\n".join("      " + line for line in dirty.split("\n"))
        failures += "\n  [1] Uncommitted pipeline changes — commit before a multi-hour run:\n" + indented
    if grep(_NO_CHECKPOINT, command, echo=True):
        failures += ("\n  [2] PIPELINE_CHECKPOINT=0 disables resumability. Long runs must be checkpointed so a crash "
                     "costs one phase, not the night.")
    if not grep(_PREFLIGHT, command, echo=True):
        failures += ("\n  [3] Missing PREFLIGHT=ok. Confirm the pre-flight checklist, then prefix the command with "
                     "PREFLIGHT=ok.")
    if not failures:
        return Result.allow()
    return Result.deny("BLOCKED: long pipeline run is not pre-flight-ready." + failures + _CHECKLIST)
