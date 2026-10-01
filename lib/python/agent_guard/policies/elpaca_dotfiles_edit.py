"""Edits to the Elpaca clone of dotfiles, ported from block-elpaca-dotfiles-edit.sh.

The clone at elpaca/sources/dotfiles/ is overwritten on the next sync, so an
edit there is always lost. Claude checks the Edit/Write file path; Codex
checks every path a call changes, apply_patch headers included.
"""

from __future__ import annotations

from ..codex_payload import command_substitution, jq_text
from ..codex_paths import codex_changed_paths
from ..core import CODEX, Call, Result

_CLONE = "elpaca/sources/dotfiles/"


def _reason(path: str) -> str:
    canonical = path[path.rindex(_CLONE) + len(_CLONE):]  # ${FILE_PATH##*elpaca/sources/dotfiles/}
    return ("BLOCKED: Do not edit the elpaca clone of dotfiles — it syncs via git and your changes will be "
            "overwritten.\n\nEdit the canonical file instead:\n  ~/My Drive/dotfiles/" + canonical
            + "\n\nThen commit and push in the dotfiles repo. The elpaca clone will pick up the change on next sync.")


def decide(call: Call) -> Result:
    if call.runtime == CODEX:
        paths = codex_changed_paths(call.raw)
    else:
        paths = [command_substitution(jq_text(call.tool_input.get("file_path")))]
    for path in paths:
        if _CLONE in path:
            return Result.deny(_reason(path))
    return Result.allow()
