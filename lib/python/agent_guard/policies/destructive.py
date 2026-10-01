"""Hard-to-reverse shell commands, ported from block-destructive-command.sh.

The Claude and Codex hooks differ only in how they read the payload, and
claude/hooks/pretooluse-bash.sh inlines the same checks (check_destructive)
with identical patterns and reason text.

The checks match shell syntax with `grep -E` over `echo "$SCAN"`.

Intended differences, stricter than the hook: a capital-R recursive flag
(`rm -Rf`) counts as `rm -rf`, and --force-with-lease exempts only its own
push invocation.
"""

from __future__ import annotations

import re

from ..codex_payload import command_substitution, is_shell_tool, jq_text, tool_name
from ..core import CODEX, Call, Result
from ..ere import ere, grep, lines

_INTERPRETER = ere(r"(^|[ \t|;&(/])(eval|bash|sh|zsh|dash|ksh|xargs|ssh)([ \t]|$)")
_HEREDOC_DELIMITER_STOPS = " \t\n;|&<>()"


def mask_quoted(command: str) -> str:
    """`$(mask_quoted)`: blank inert quoted text so string literals cannot
    trigger a syntax check. Commands naming a shell interpreter are scanned
    raw, since their quoted arguments execute; any unterminated quote or
    heredoc copies the rest verbatim."""
    if grep(_INTERPRETER, command):
        return command
    cmd = command + "\n"
    n = len(cmd)
    out: list[str] = []
    queue: list[tuple[str, bool, bool]] = []  # (delimiter, strip tabs, mask body)
    i = 0
    while i < n:
        c = cmd[i]
        if c == "\n":
            out.append(c)
            i += 1
            # Heredoc bodies queued on this line start after this newline.
            while queue:
                delim, strip, mask_body = queue[0]
                j = i
                found = False
                while j < n:
                    line_end = cmd.index("\n", j)
                    test = cmd[j:line_end]
                    if strip:
                        test = test.lstrip("\t")
                    if test == delim:
                        found = True
                        break
                    j = line_end + 1
                if not found:
                    out.append(cmd[i:])
                    i = n
                    queue.clear()
                    break
                out.append(cmd[j if mask_body else i:line_end + 1])
                i = line_end + 1
                queue.pop(0)
            continue
        if c == "\\":
            out.append(cmd[i:i + 2])
            i += 2
            continue
        if c == "'":
            end = cmd.find("'", i + 1)
            if end < 0:
                out.append(cmd[i:])
                break
            out.append("''")
            i = end + 1
            continue
        if c == '"':
            j = i + 1
            span: list[str] = []
            closed = False
            while j < n:
                d = cmd[j]
                if d == "\\":
                    span.append(cmd[j:j + 2])
                    j += 2
                    continue
                if d == '"':
                    closed = True
                    break
                span.append(d)
                j += 1
            if not closed:
                out.append(cmd[i:])
                break
            text = "".join(span)
            out.append('"' + text + '"' if "$" in text or "`" in text else '""')
            i = j + 1
            continue
        if c == "$" and cmd[i + 1:i + 2] == "'":
            j = i + 2
            closed = False
            while j < n:
                d = cmd[j]
                if d == "\\":
                    j += 2
                    continue
                if d == "'":
                    closed = True
                    break
                j += 1
            if not closed:
                out.append(cmd[i:])
                break
            out.append("$''")
            i = j + 1
            continue
        if c == "<" and cmd[i + 1:i + 2] == "<" and cmd[i + 2:i + 3] != "<":
            j = i + 2
            strip = cmd[j:j + 1] == "-"
            if strip:
                j += 1
            while cmd[j:j + 1] in (" ", "\t") and j < n:
                j += 1
            quote = cmd[j:j + 1]
            if quote in ("'", '"'):
                end = cmd.find(quote, j + 1)
                if end < 0:
                    out.append(cmd[i:])
                    break
                queue.append((cmd[j + 1:end], strip, True))
                out.append(cmd[i:end + 1])
                i = end + 1
                continue
            k = j
            while k < n and cmd[k] not in _HEREDOC_DELIMITER_STOPS:
                k += 1
            if k > j:
                queue.append((cmd[j:k], strip, False))
            out.append(cmd[i:k])
            i = k
            continue
        out.append(c)
        i += 1
    return command_substitution("".join(out))


_PUSH = ere(r"\bgit\s+push")
_FORCE_FLAG = ere(r"(\s-f\b|\s--force\b)")
_CLONE = ere(r"\b(git\s+clone|gh\s+repo\s+clone)\b")
_ALLOW_CLONE = ere(r"(^|[[:space:]])ALLOW_CLONE=1([[:space:]]|$)")

# (pattern, label, suggestion) in the hooks' order; the first match denies.
_RULES = (
    (ere(r"\brm\s+(-[a-zA-Z]*[rR][a-zA-Z]*f|(-[a-zA-Z]*f[a-zA-Z]*[rR])|-rf|-fr)\b"),
     "rm -rf detected", "Use 'trash' instead of 'rm -rf' to allow recovery."),
    (None, "git push --force detected",
     "Force-pushing can overwrite upstream history. Use --force-with-lease for branch-scoped pushes, or confirm "
     "with the user."),
    (None, "git clone detected",
     "Only clone repositories the user has explicitly requested by URL or name. If this is a clone they asked for, "
     "prefix the command with ALLOW_CLONE=1. If it is not, ask them in chat first."),
    (ere(r"\bgit\s+reset\s+--hard\b"),
     "git reset --hard detected", "This discards uncommitted changes irreversibly. Consider 'git stash' instead."),
    (ere(r"\bgit\s+clean\s+.*-[a-zA-Z]*f"),
     "git clean -f detected", "This permanently deletes untracked files. Review 'git clean -n' first."),
    (ere(r"\bgit\s+checkout\s+--\s+\."),
     "git checkout -- . detected", "This discards all unstaged changes. Consider 'git stash' instead."),
    (ere(r"\bgit\s+branch\s+-D\b"),
     "git branch -D detected", "Force-deleting a branch can lose unmerged work. Use -d for safe delete."),
    (ere(r"\bgh\s+api\s+.*repos/.*visibility|gh\s+repo\s+edit\s+.*--visibility"),
     "GitHub repo visibility change detected",
     "Toggling a repo private permanently destroys all stars and watchers. NEVER do this."),
    (ere(r"\bgh\s+repo\s+delete\b"),
     "gh repo delete detected",
     "Deleting a GitHub repo is irreversible and destroys all stars, forks, and history. Confirm with the user first."),
    (ere(r"\bdropdb\b"), "dropdb detected", "Dropping a database is irreversible. Confirm with the user first."),
    (ere(r"\bbq\s+rm\b"), "bq rm detected", "BigQuery rm is irreversible. Confirm with the user first."),
    (ere(r"\baws\s+s3\s+rm[^|;&]*(\s--recursive\b|\s-r\b)"),
     "aws s3 rm --recursive detected", "Recursive S3 deletion is irreversible. Confirm with the user first."),
    (ere(r"\bop\s+item\s+(delete|remove)\b"),
     "op item delete detected", "Deleting a 1Password item is irreversible. Confirm with the user first."),
)


def _unleased_force_push(scan: str) -> bool:
    """A `git push` invocation (up to the next |, ;, & or `git push`) with
    -f/--force and no --force-with-lease of its own.

    Intended difference, stricter than the hook: there, one leased push
    anywhere in the command let every other push's force flag through.
    """
    for line in lines(scan, echo=True):
        starts = [match.start() for match in _PUSH.finditer(line)]
        for start, end in zip(starts, starts[1:] + [len(line)]):
            invocation = re.split("[|;&]", line[start:end], maxsplit=1)[0]
            if grep(_FORCE_FLAG, invocation) and "--force-with-lease" not in invocation:
                return True
    return False


def _matches(index: int, pattern, scan: str) -> bool:
    if index == 1:
        return _unleased_force_push(scan)
    if index == 2:
        # The hatch is read from the masked text, so a quoted one cannot smuggle a clone.
        return grep(_CLONE, scan, echo=True) and not grep(_ALLOW_CLONE, scan, echo=True)
    return grep(pattern, scan, echo=True)


def destructive_reason(command: str) -> str:
    """The deny reason for COMMAND, or "" to allow."""
    scan = mask_quoted(command) or command
    for index, (pattern, label, suggestion) in enumerate(_RULES):
        if _matches(index, pattern, scan):
            return (f"BLOCKED: {label}.\n\n{suggestion}\n\n"
                    "If you are certain this is the right action, ask the user to run it manually with `!`.")
    return ""


def decide(call: Call) -> Result:
    if call.runtime == CODEX:
        if not is_shell_tool(command_substitution(tool_name(call.raw))):
            return Result.allow()
    elif command_substitution(jq_text(call.raw.get("tool_name"))) != "Bash":
        return Result.allow()
    command = call.shell_command()
    reason = destructive_reason(command) if command else ""
    return Result.deny(reason) if reason else Result.allow()


def dispatcher_check(call: Call) -> list[str]:
    """check_destructive in pretooluse-bash.sh, run once the dispatcher has a
    non-empty Bash command. Its grep wrapper fails closed on grep errors,
    which these patterns cannot raise on jq's UTF-8 output."""
    command = call.shell_command()
    reason = destructive_reason(command) if command else ""
    return [reason] if reason else []
