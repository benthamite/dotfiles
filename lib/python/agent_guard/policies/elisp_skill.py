"""Elisp edits before the conventions skill, ported from claude/hooks/require-elisp-skill.sh.

Claude only. The PostToolUse hook track-elisp-skill.sh leaves a per-session
marker in /tmp when the elisp-conventions skill loads; without it, .el edits
are denied.
"""

from __future__ import annotations

import os

from ..codex_payload import command_substitution, jq_text
from ..core import Call, Result

REASON = ("BLOCKED: You must load the elisp-conventions skill before editing .el files. "
          "Run: Skill(skill: \"elisp-conventions\")")


def decide(call: Call) -> Result:
    path = command_substitution(jq_text(call.tool_input.get("file_path")))
    if not path.endswith(".el"):
        return Result.allow()
    session = command_substitution(jq_text(call.raw.get("session_id")))
    if os.path.isfile(f"/tmp/claude-elisp-skill-{session}"):
        return Result.allow()
    return Result.deny(REASON)
