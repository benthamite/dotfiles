"""New .el files must start with a `lexical-binding' cookie.

Emacs 31 warns on every load of a file without one, and agents load scratch
.el files into Pablo's Emacs (`emacsclient --eval '(load "…/poll.el")'`), so
each poll puts another warning in his *Warnings* buffer (2026-11-10 and
2026-11-13 sessions). The rule in the elisp-conventions skill did not reach
these writes: they were shell heredocs, which never load that skill.

Checked writes, for both runtimes:
- Claude Write (and Codex Write) of a `.el' path: the `content' field.
- Codex apply_patch `*** Add File: x.el': the first added lines.
- A shell heredoc redirected with `>' or `tee' (not append) to a `.el' path:
  the heredoc body.

Emacs reads the cookie from the first line, or the second after a `#!' line,
so only those are checked. `lexical-binding: nil' also counts. Edits to
existing files, appends and other ways of writing (printf, python) are not
checked.
"""

from __future__ import annotations

import re

from ..codex_paths import codex_patch_text
from ..codex_payload import command_substitution, jq_alternative, jq_text
from ..core import CODEX, Call, Result

REASON = ("BLOCKED: this writes a new .el file without a lexical-binding cookie. Emacs 31 shows a "
          "\"Missing 'lexical-binding' cookie\" warning in Pablo's Emacs every time such a file is loaded. "
          "Make its first line:\n\n;;; -*- lexical-binding: t -*-\n\n(after a #! line if there is one), then retry.")

COOKIE = re.compile(r"-\*-.*\blexical-binding:\s*(t|nil)\b.*-\*-")
_HEREDOC = re.compile(r"<<-?[ \t]*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1")
_TARGET = re.compile(r"(?:(?<![>&0-9])>|\btee(?:[ \t]+--?[A-Za-z-]+)*)[ \t]*(['\"]?)([^\s'\"|;&<>()]+\.el)\1(?=[\s;|&)]|$)")
_TEE_APPEND = re.compile(r"\btee\b[^|;&\n]*[ \t](?:-a|--append)\b")


def has_cookie(text: str) -> bool:
    lines = text.split("\n", 2)
    if lines and lines[0].startswith("#!"):
        lines = lines[1:]
    return bool(lines) and bool(COOKIE.search(lines[0]))


def _heredoc_violations(command: str) -> bool:
    lines = command.split("\n")
    for index, line in enumerate(lines):
        heredoc = _HEREDOC.search(line)
        if not heredoc or not _TARGET.search(line) or _TEE_APPEND.search(line):
            continue
        delimiter = heredoc.group(2)
        body = []
        for body_line in lines[index + 1:]:
            if body_line.strip() == delimiter:
                break
            body.append(body_line.lstrip("\t") if line[heredoc.start():].startswith("<<-") else body_line)
        if not has_cookie("\n".join(body)):
            return True
    return False


def _patch_violations(patch: str) -> bool:
    for match in re.finditer(r"^\*\*\* Add File: (.+\.el)[ \t]*$", patch, re.M):
        added = []
        for line in patch[match.end():].lstrip("\n").split("\n"):
            if not line.startswith("+"):
                break
            added.append(line[1:])
            if len(added) == 2:
                break
        if not has_cookie("\n".join(added)):
            return True
    return False


def decide(call: Call) -> Result:
    fields = call.tool_input
    if call.is_shell:
        violation = _heredoc_violations(call.shell_command())
    elif call.tool_name == "Write":
        path = command_substitution(jq_text(jq_alternative(fields.get("file_path"), fields.get("filePath"))))
        content = fields.get("content")
        violation = path.endswith(".el") and isinstance(content, str) and not has_cookie(content)
    elif call.runtime == CODEX and call.tool_name == "apply_patch":
        violation = _patch_violations(codex_patch_text(call.raw))
    else:
        violation = False
    return Result.deny(REASON) if violation else Result.allow()
