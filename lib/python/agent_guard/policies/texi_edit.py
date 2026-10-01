"""Edits to generated Texinfo, ported from block-texi-edit.sh.

.texi files are exported from .org by ox-texinfo, so edits belong in the Org
source. Claude checks the Edit/Write path (`file_path`, then `filePath`);
Codex checks every path a call changes.
"""

from __future__ import annotations

from ..codex_payload import command_substitution, jq_alternative, jq_text
from ..codex_paths import codex_changed_paths
from ..core import CODEX, Call, Result

REASON = ("BLOCKED: .texi files are auto-generated from .org via ox-texinfo. Edit the corresponding .org file "
          "instead, then regenerate.")


def decide(call: Call) -> Result:
    if call.runtime == CODEX:
        paths = codex_changed_paths(call.raw)
    else:
        fields = call.tool_input
        paths = [command_substitution(jq_text(jq_alternative(fields.get("file_path"), fields.get("filePath"))))]
    if any(path.endswith(".texi") for path in paths):
        return Result.deny(REASON)
    return Result.allow()
