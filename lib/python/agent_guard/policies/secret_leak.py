"""Secret-leak guard: port of {claude,codex}/hooks/block-secret-leak.sh.

Denies shell commands and file edits that would print a credential into agent
output or write a literal secret: secret-printing tools (`security`,
`pbpaste`), 1Password brokers in printing shapes, raw `op`, process and
function-body introspection, known secret values and shapes, and network
exfiltration. Both runtimes share one module; the Codex hook differs in its
payload fields, in classifying the literal nested commands of a
`functions.exec` program, and in having no Claude `Write` file exemption.

The shell guard's text tools are reproduced, not approximated, because their
quirks decided calls: `grep -E`, `sed -E` and `echo` come from agent_guard.ere,
`$(...)` drops trailing newlines (command_substitution), and the awk
projections are ported as they ran.

Intended differences: the hook's shell allowlist (grep/rg and git-crypt
commands, `$VAR` references, mentions of the encrypted secrets file) returned
before the known-value and secret-shape checks; here it skips only the
exfiltration checks. The opaque-token heuristic no longer denies; it is
logged (agent_guard.url_heuristic).
"""

from __future__ import annotations

import re
from collections import Counter

import secret_index

from .. import classifiers, url_heuristic
from ..codex_paths import codex_patch_content_for_scan
from ..codex_payload import (command_substitution, jq_alternative, jq_raw, jq_text, literal_exec_wrapper,
                             nested_exec_contexts)
from ..core import CODEX, CODEX_SHELL_TOOLS, Call, HookFailure, Result
from ..ere import grep, sed
from ..shell_text import _SINKS, mask_heredoc_bodies

# --- grep -E, sed -E and awk as the hook ran them ------------------------------


_AWK_WORD_BREAK = re.compile(r"[ \t\n\v\f\r\xa0;&|()!]")


def _added_lines(old: str, new: str) -> list[str]:
    """`diff <(printf '%s\\n' OLD) <(printf '%s\\n' NEW) | grep -E '^> '` where
    OLD's lines are a subsequence of NEW's: the lines only NEW has."""
    added = Counter(new.split("\n")) - Counter(old.split("\n"))
    return ["> " + line for line in added.elements()]


def mask_op_quoted_literals(text: str) -> str:
    """The hook's awk `mask_op_quoted_literals`: single-quoted spans and
    double-quoted spans without `$` or backquote become empty quotes."""
    text += "\n"
    out, i, n = [], 0, len(text)
    while i < n:
        char = text[i]
        if char == "\\":
            out.append(text[i:i + 2])
            i += 2
        elif char == "'":
            close = text.find("'", i + 1)
            if close < 0:
                out.append(text[i:])
                break
            out.append("''")
            i = close + 1
        elif char == '"':
            j, span, closed = i + 1, [], False
            while j < n:
                if text[j] == "\\":
                    span.append(text[j:j + 2])
                    j += 2
                    continue
                if text[j] == '"':
                    closed = True
                    break
                span.append(text[j])
                j += 1
            if not closed:
                out.append(text[i:])
                break
            span = "".join(span)
            out.append('"' + span + '"' if "$" in span or "`" in span else '""')
            i = j + 1
        else:
            out.append(char)
            i += 1
    return command_substitution("".join(out))


def normalize_shell_words(text: str) -> str:
    """The hook's awk `normalize_shell_words`: backslashes and quotes the
    shell would remove are removed when the quoted span is one plain word.
    Other quoted spans are deleted: the awk source meant `out "''"`, but
    inside the shell's single-quoted program that reads `out ""`."""
    text += "\n"
    out, i, n = [], 0, len(text)
    while i < n:
        char = text[i]
        if char == "\\" and i < n - 1:
            out.append(text[i + 1])
            i += 2
        elif char in "'\"":
            j, span, closed = i + 1, [], False
            while j < n:
                if char == '"' and text[j] == "\\" and j < n - 1:
                    span.append(text[j + 1])
                    j += 2
                    continue
                if text[j] == char:
                    closed = True
                    break
                span.append(text[j])
                j += 1
            if not closed:
                out.append(text[i:])
                break
            span = "".join(span)
            if not _AWK_WORD_BREAK.search(span) and (char == "'" or ("$" not in span and "`" not in span)):
                out.append(span)
            i = j + 1
        else:
            out.append(char)
            i += 1
    return command_substitution("".join(out))


# --- classifiers, with the exit-status contracts the hook relied on ------------

def _python_heredoc(command: str, *, document_edits: bool = True, brokers: bool = False) -> tuple[int, str]:
    """`lib-python-heredoc.py [--brokers|--no-document-edits]`: exit status
    (2 for a protected name in recognized Python) and captured stdout."""
    module = classifiers.load("python-heredoc")
    protected = module.PROTECTED
    if brokers:
        module.PROTECTED = module.BROKERS
    try:
        return 0, command_substitution(module.project_command(command, allow_document_edit=document_edits))
    except module.ProtectedPythonReference:
        return 2, ""
    except Exception:  # noqa: BLE001 - the script exited 1
        return 1, ""
    finally:
        module.PROTECTED = protected


def _inert_mentions(text: str) -> str:
    """`lib-inert-mentions.py`, which echoes its input on any error. Its caller's
    `|| return 0` only fires when the script cannot run at all, which here
    is a load failure, and that propagates (the call is denied)."""
    module = classifiers.load("inert-mentions")
    try:
        projected = module.mask_inert_mentions(text)
    except Exception:  # noqa: BLE001 - the script printed its input unchanged
        projected = text
    return command_substitution(projected)


def _jq_reason(result: dict, default: str) -> str:
    return command_substitution(jq_raw(jq_alternative(result.get("reason"), default)))


def process_output_denial(command: str) -> str | None:
    module = classifiers.load("process-output-policy")
    try:
        module.classify(command)
        return None
    except module.Denied as error:
        reason = str(error)
    except (ValueError, TypeError, KeyError, IndexError, module.TOKENS.Deny):
        reason = "process-output policy input could not be classified"
    except Exception:  # noqa: BLE001 - the script crashed
        reason = "process-output classifier failed"
    return command_substitution(reason)


def function_body_denial(command: str) -> str | None:
    if not grep("which|where|whence|type|functions|declare|local|export|readonly|eval", command):
        return None
    module = classifiers.load("function-body-policy")
    try:
        module.classify(command)
        return None
    except module.Denied as error:
        reason = str(error)
    except Exception:  # noqa: BLE001 - the script's own fail-closed reason
        reason = "function-body classifier received malformed input"
    return command_substitution(reason)


def op_policy_denial(command: str) -> str | None:
    """lib-op-policy.py's reason when COMMAND would print a 1Password secret."""
    if not grep("op-automations|op-desktop|OP_RUN_NO_MASKING", command_substitution(sed(command, _STRIP_QUOTES))):
        return None
    try:
        result = classifiers.load("op-policy").decide(command)
    except Exception:  # noqa: BLE001 - the hook's fallback for a failed classifier
        result = {"decision": "deny", "reason": "the 1Password policy classifier failed, so the command cannot be classified"}
    if jq_alternative(result.get("decision"), "deny") != "deny":
        return None
    return _jq_reason(result, "unclassified 1Password command")


_INTERPRETER_BROKER = (
    "a 1Password broker is named inside an interpreter program, which the guard cannot tell apart from running it; "
    "edit text with the Edit tool or a pathlib read_text/replace/write_text document edit, and run brokers as shell "
    "commands so their shape can be checked")
_INTERPRETER_PROGRAM = (r"(^|[^A-Za-z0-9_./-])(python[0-9.]*|node|nodejs|deno|bun|ruby|perl|php|lua|osascript)"
                        r"[[:space:]]+([^;&|]*[[:space:]])?-(c|e|-eval)[[:space:]]")
_PROCESS_START = (r"subprocess|os\.(system|popen|exec|spawn)|popen|Popen|child_process|exec(Sync|File)?[[:space:]]*\(|"
                  r"spawn|system[[:space:]]*\(|`|qx|%x|do[[:space:]]+shell[[:space:]]+script")


def op_command_denial(command: str) -> str | None:
    """Interpreter programs naming a broker are denied; the remaining shell
    text goes to lib-op-policy.py."""
    status, projected = _python_heredoc(command, brokers=True)
    if status == 2:
        return _INTERPRETER_BROKER
    if status != 0:
        projected = command
    shell_text = mask_heredoc_bodies(projected, "nonshell")
    body_lines = _added_lines(shell_text, mask_heredoc_bodies(projected))
    if (any(grep("op-automations|op-desktop", line) for line in body_lines)
            or (grep(_INTERPRETER_PROGRAM, shell_text) and grep("op-automations|op-desktop", shell_text)
                and grep(_PROCESS_START, shell_text))):
        return _INTERPRETER_BROKER
    return op_policy_denial(shell_text)


# --- shell-text rules ------------------------------------------------------------

_PROTECTED = r"(^|[^A-Za-z0-9_-])(pbpaste|security)([^A-Za-z0-9_-]|$)"
_LITERAL_ECHO = (
    r"^[[:space:]]*(([^;&|[:space:]]*/)?(command|env|sudo|timeout|nice|exec|nohup|time)([[:space:]]+[^;&|[:space:]]+)*"
    r"[[:space:]]+)?([^;&|[:space:]]*/)?(bash|sh|zsh|dash|ksh)[[:space:]]+-l?c[[:space:]]+Q[[:space:]]*"
    r"([A-Za-z_][A-Za-z0-9_]*=[^;&|[:space:]]*[[:space:]]+)*((command|sudo|exec|nohup|time)[[:space:]]+|"
    r"env([[:space:]]+[A-Za-z_][A-Za-z0-9_]*=[^;&|[:space:]]*)*[[:space:]]+)?(builtin[[:space:]]+)?"
    r"([^;&|[:space:]]*/)?(echo|printf)([[:space:]]+[^Q$;&|]*)?Q[[:space:]]*$")
_SQ_ECHO = _LITERAL_ECHO.replace("Q", "'")
_DQ_ECHO = _LITERAL_ECHO.replace("Q", '"')
_PLAIN_ECHO = (r"^[[:space:]]*([A-Za-z_][A-Za-z0-9_]*=[^;&|[:space:]]*[[:space:]]+)*((command|sudo|exec|nohup|time)"
               r"[[:space:]]+|env([[:space:]]+[A-Za-z_][A-Za-z0-9_]*=[^;&|[:space:]]*)*[[:space:]]+)?"
               r"(builtin[[:space:]]+)?([^;&|[:space:]]*/)?(echo|printf)([[:space:]]+[^;&|`]*)?[[:space:]]*$")
_SHELL_C = (r"""(^|[;&|(!][[:space:]]*|\$\([[:space:]]*)(([^;&|[:space:]]*/)?(command|env|sudo|timeout|nice|exec|"""
            r"""nohup|time)([[:space:]]+[^;&|[:space:]]+)*[[:space:]]+)?(([^;&|[:space:]]*/)?(bash|sh|zsh|dash|ksh)"""
            r"""[[:space:]]+-l?c|eval)[[:space:]]+["']""")
_SW_BOUNDARY = r"(^[[:space:]]*|[;&|(!`][[:space:]]*|\$\([[:space:]]*)"
_SW_WRAPPER = (r"(([^;&|[:space:]]*/)?(command|env|sudo|timeout|nice|exec|nohup|time|builtin|xargs)([[:space:]]+"
               r"(-[^;&|[:space:]]*|[A-Za-z_][A-Za-z0-9_]*=[^;&|[:space:]]*|[0-9]+[smhd]?))*[[:space:]]+|"
               r"[A-Za-z_][A-Za-z0-9_]*=([^;&|()`[:space:]]|\$\([^()]*\))*[[:space:]]+)")
_SW_EXECUTABLE = r"([^;&|[:space:]]*/)?(pbpaste|security)"
_SW_DELIMITER = r"([[:space:];|&)`]|$)"
_SW_GLOB = r"[^;&|[:space:]]*([?*]|\\[[^]]*)[^;&|[:space:]]*"
# sed -E "s/['\"\\\\]//g": quotes and backslashes removed.
_STRIP_QUOTES = (r"""['"\\]""", "", "g")
_NORMALIZE_RULES = (
    (r"(case[[:space:]]+[^;&|()]+[[:space:]]+in[[:space:]]*)\*[[:space:]]*\)", r"\1CASE_DEFAULT)", "g"),
    (r"(^[[:space:]]*|;;[[:space:]]*)\*[[:space:]]*\)", r"\1CASE_DEFAULT)", "g"),
    (r"\$\?", "EXIT_STATUS", "g"),
    (r"(^[[:space:]]*|[;&|(][[:space:]]*)([A-Za-z_][A-Za-z0-9_]*)=[^;&|[:space:]]*([;&|]|$)", r"\1\2=ASSIGNED\3", "g"),
)


# --- what the shell can expand as a program name -------------------------------
#
# The executable-glob rules look for a glob word after a command boundary in
# normalize_shell_words' text, which is not parsed as shell: `|` and `*` in
# `${q%%|*}`, quoted Python or SQL whose quotes it dropped or misnested, case
# patterns, `[!.]*` and heredoc bodies fed to Python or cat all read as a glob
# in a command position. _glob_scan keeps only text the shell itself expands;
# a glob rule denies only when it matches that text too.

_GLOB_DATA_CONSUMERS = {name.decode() for name in _SINKS} | {
    "node", "nodejs", "deno", "bun", "ruby", "perl", "php", "osascript", "emacs", "emacsclient", "sqlite3",
    "psql", "mysql", "jq", "yq", "Rscript", "lua", "luajit", "swift", "julia", "awk", "gawk", "mawk", "sed",
    "bc", "dc"}
_GLOB_PREFIX_WORDS = {"do", "then", "else", "elif", "if", "while", "until", "{", "!", "time", "command",
                      "builtin", "exec", "nohup", "nice", "env", "sudo", "timeout"}
_CASE_RULES = (
    (r"(case[[:space:]]+[^;&|()[:space:]]+[[:space:]]+in[[:space:]]+)\(?[^;&()]*\)", r"\1CASE_PATTERN)", "g"),
    (r"(;;[[:space:]]*)\(?[^;&()]*\)", r"\1CASE_PATTERN)", "g"),
)


class _Unparsed(Exception):
    """Shell text _glob_scan cannot follow."""


def _skip_single(text: str, i: int) -> int:
    """The index after the single-quoted span opening at I."""
    end = text.find("'", i + 1)
    if end < 0:
        raise _Unparsed
    return end + 1


def _close_backtick(text: str, i: int) -> int:
    """The index of the backquote ending the substitution whose body starts at I."""
    while i < len(text):
        if text[i] == "\\":
            i += 2
        elif text[i] == "`":
            return i
        else:
            i += 1
    raise _Unparsed


def _close(text: str, i: int, opening: str, closing: str) -> int:
    """The index of the CLOSING that ends the `$(` or `${` body starting at I."""
    depth, n = 1, len(text)
    while i < n:
        c = text[i]
        if c == "\\":
            i += 2
        elif c == "'":
            i = _skip_single(text, i)
        elif c == '"':
            i = _double_quoted(text, i)[0]
        elif c == "`":
            i = _close_backtick(text, i + 1) + 1
        elif closing == "}" and text.startswith("$(", i):
            i = _close(text, i + 2, "(", ")") + 1
        else:
            if c == opening:
                depth += 1
            elif c == closing:
                depth -= 1
                if depth == 0:
                    return i
            i += 1
    raise _Unparsed


def _expansion(text: str, i: int) -> tuple[int, str] | None:
    """The expansion starting at I (`$((`, `$(`, `${` or a backquote): the index
    after it and its scanned form; None if none starts there."""
    if text.startswith("$((", i):
        end = _close(text, i + 2, "(", ")")
        body = text[i + 3:end]
        if "$(" in body or "`" in body:
            raise _Unparsed
        return end + 1, "A"
    if text.startswith("$(", i):
        end = _close(text, i + 2, "(", ")")
        return end + 1, "$(" + _glob_scan(text[i + 2:end], True) + ")"
    if text.startswith("${", i):
        end = _close(text, i + 2, "{", "}")
        body = text[i + 2:end]
        # Its pattern operators are not command boundaries, but as (part of)
        # a program name an expansion with a pattern still names an unknown
        # program, so P keeps a `*` for one unless it is an assignment value.
        # A substitution inside it runs.
        if "$(" in body or "`" in body:
            return end + 1, "${" + _glob_scan(body, True) + "}"
        word = re.split(r"[ \t\n;&|()]", text[:i])[-1]
        named = re.search(r"[*?[]", body) and not re.match(r"[A-Za-z_][A-Za-z0-9_]*=", word)
        return end + 1, "P*" if named else "P"
    if text.startswith("`", i):
        end = _close_backtick(text, i + 1)
        return end + 1, "`" + _glob_scan(text[i + 1:end], True) + "`"
    return None


def _double_quoted(text: str, i: int) -> tuple[int, str]:
    """For the double-quoted span opening at I: the index after it, and Q
    followed by the expansions in it, scanned."""
    out, i, n = ["Q"], i + 1, len(text)
    while i < n:
        if text[i] == "\\":
            i += 2
        elif text[i] == '"':
            return i + 1, "".join(out)
        elif (found := _expansion(text, i)) is not None:
            i, scanned = found
            out.append(scanned)
        else:
            i += 1
    raise _Unparsed


def _feeds_data(segment: str) -> bool:
    """Whether the simple command SEGMENT (its scanned text so far) is a program
    that reads a heredoc as data or as non-shell source."""
    words = segment.split()
    k = 0
    while k < len(words):
        word = words[k]
        if (word in _GLOB_PREFIX_WORDS or word.startswith("-") or re.fullmatch(r"[0-9]+[smhd]?", word)
                or re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*=\S*", word)):
            k += 1
        elif re.search(r"(^|/)pyenv$", word) and words[k + 1:k + 2] == ["exec"]:
            k += 2
        else:
            program = word.rsplit("/", 1)[-1]
            return program in _GLOB_DATA_CONSUMERS or re.fullmatch(r"python[0-9.]*", program) is not None
    return False


def _glob_scan(text: str, nested: bool = False) -> str:
    """TEXT reduced to what the shell expands: a quoted span becomes Q followed
    by its expansions, `${...}` P (P* when it holds a pattern), `$((...))` A,
    an escaped character E, a `!` that is not a pipeline negation X; comments
    are dropped, and so are the bodies of quoted heredocs read by a program
    that is not a shell (unquoted ones keep their expansions); other bodies
    are kept as normalize_shell_words reads them. Substitutions are scanned in place, as
    NESTED text. Raises _Unparsed on text it cannot follow."""
    out: list[str] = []
    segment = 0                # where in OUT the current simple command starts
    heredocs: list[tuple[str, bool, bool, bool]] = []    # delimiter, strip tabs, quoted, data
    i, n = 0, len(text)
    while i < n:
        c = text[i]
        at_word_start = i == 0 or text[i - 1] in " \t\n;&|()"
        if c == "\n":
            out.append("\n")
            i += 1
            for delimiter, strip, quoted, data in heredocs:
                start = i
                while True:
                    if i >= n:
                        raise _Unparsed
                    end = text.find("\n", i)
                    end = n if end < 0 else end
                    line = text[i:end]
                    i = end + 1
                    if (line.lstrip("\t") if strip else line) == delimiter:
                        break
                body = text[start:i]
                if not data:
                    # Shell or unknown: read as the older normalization did.
                    out.append(normalize_shell_words(body))
                elif not quoted:
                    out.append(_double_quoted('"' + body.replace("\\", "\\\\").replace('"', '\\"') + '"', 0)[1])
                out.append("\n")
            heredocs = []
            segment = len(out)
        elif c == "\\":
            if not text.startswith("\\\n", i):
                out.append("E")
            i += 2
        elif c == "'":
            out.append("Q")
            i = _skip_single(text, i)
        elif c == '"':
            i, scanned = _double_quoted(text, i)
            out.append(scanned)
        elif c == "#" and at_word_start:
            end = text.find("\n", i)
            i = n if end < 0 else end
        elif (found := _expansion(text, i)) is not None:
            i, scanned = found
            out.append(scanned)
        elif c == "!":
            out.append("!" if at_word_start and text[i + 1:i + 2] in ("", " ", "\t", "\n") else "X")
            i += 1
        elif text.startswith("<<", i) and not text.startswith("<<<", i):
            j = i + 2
            strip = text.startswith("-", j)
            j += strip
            while j < n and text[j] in " \t":
                j += 1
            delimiter, quoted = [], False
            while j < n and text[j] not in " \t\n;&|()<>":
                if text[j] == "'":
                    end = _skip_single(text, j)
                    delimiter.append(text[j + 1:end - 1])
                    quoted, j = True, end
                elif text[j] == '"':
                    end = text.find('"', j + 1)
                    if end < 0:
                        raise _Unparsed
                    delimiter.append(text[j + 1:end])
                    quoted, j = True, end + 1
                elif text[j] == "\\":
                    delimiter.append(text[j + 1:j + 2])
                    quoted, j = True, j + 2
                else:
                    delimiter.append(text[j])
                    j += 1
            if not delimiter:
                raise _Unparsed
            # A body is data only when its reader and every program its output
            # is piped to on the rest of the line read data, nothing there
            # substitutes, and it is not inside a substitution, whose output
            # may run (lib-heredoc's masker never allowed a pipe).
            line_end = text.find("\n", j)
            rest = text[j:] if line_end < 0 else text[j:line_end]
            data = (not nested and not re.search(r"[(`]", rest) and _feeds_data("".join(out[segment:]))
                    and all(_feeds_data(stage) for stage in rest.split("|")[1:]))
            heredocs.append(("".join(delimiter), strip, quoted, data))
            out.append("<<D")
            i = j
        else:
            out.append(c)
            i += 1
            if c in ";&|()":
                segment = len(out)
    if heredocs:
        raise _Unparsed
    return "".join(out)


def glob_scan_text(text: str) -> str | None:
    """TEXT as the executable-glob rules read it (see _glob_scan), or None when
    the scan cannot follow it."""
    try:
        scanned = _glob_scan(text)
    except (_Unparsed, RecursionError):
        return None
    return command_substitution(sed(scanned + "\n", *_CASE_RULES, *_NORMALIZE_RULES))


def secret_output_issue(command: str, *, document_edits: bool = True) -> str | None:
    """How COMMAND may run a tool whose output is the secret itself: "protected"
    (a credential or clipboard tool name may execute) or "glob" (a program named
    by a glob could expand to one); None if neither."""
    status, raw = _python_heredoc(command, document_edits=document_edits)
    if status != 0:
        return "protected"
    raw = mask_heredoc_bodies(raw)
    # A heredoc body still present feeds a shell or an interpreter: program source.
    if grep(_PROTECTED, raw):
        body_lines = _added_lines(mask_heredoc_bodies(raw, "all"), raw)
        if any(grep(_PROTECTED, line) for line in body_lines):
            return "protected"
    # A literal mention printed by a simple nested echo/printf program is data.
    for literal_echo in (_SQ_ECHO, _DQ_ECHO):
        if grep(literal_echo, raw) and not grep(r"`|[<>]\(", raw):
            return None
    scan = mask_op_quoted_literals(raw)
    if grep(_PLAIN_ECHO, scan) and not grep(r"[;&|`]|\$\(|[<>]\(", scan):
        return None
    if grep(_PROTECTED, scan):
        if grep(_PROTECTED, _inert_mentions(scan)):
            return "protected"
    if grep(_SHELL_C, raw) and grep(_PROTECTED, raw):
        return "protected"
    if (grep(r"""[A-Za-z_][A-Za-z0-9_]*=[[:space:]]*["']([^"']*/)?(pbpaste|security)["']""", raw)
            and grep(r"\$\{?[A-Za-z_][A-Za-z0-9_]*\}?", raw)):
        return "protected"
    if grep(r"\$\([[:space:]]*(command[[:space:]]+-v|which|type[[:space:]]+-P)[[:space:]]+(pbpaste|security)"
             r"[[:space:]]*\)", raw):
        return "protected"
    normalized = command_substitution(
        sed(normalize_shell_words(mask_heredoc_bodies(raw, "nonshell")) + "\n", *_NORMALIZE_RULES))
    if grep(f"{_SW_BOUNDARY}({_SW_WRAPPER})*{_SW_EXECUTABLE}{_SW_DELIMITER}", normalized):
        return "protected"
    if grep(f"(-exec|-execdir|-ok|-okdir)[[:space:]]+{_SW_EXECUTABLE}{_SW_DELIMITER}", normalized):
        return "protected"
    glob_text = None
    for rule in (f"{_SW_BOUNDARY}({_SW_WRAPPER})*{_SW_GLOB}{_SW_DELIMITER}",
                 f"find[[:space:]].*-exec[[:space:]]+{_SW_GLOB}{_SW_DELIMITER}"):
        if grep(rule, normalized):
            # Confirmed on the text the shell expands; kept if it cannot be parsed.
            glob_text = glob_text or glob_scan_text(mask_heredoc_bodies(raw, "nonshell")) or normalized
            if grep(rule, glob_text):
                return "glob"
    if grep(r"""(^|[[:space:];&|(!`])(([^;&|[:space:]]*/)?(bash|sh|zsh|dash|ksh)[[:space:]]+-l?c|eval)"""
            r"""[[:space:]]+["'][^"']*([?*]|\[[^]]*)""", raw):
        return "glob"
    return None


_OP_BIN = r"(/opt/homebrew/bin/|/usr/local/bin/|/usr/bin/)?op"
_ENV_PREFIX = r"((/usr/bin/|/bin/)?env)([[:space:]]+(-u[[:space:]]+[^[:space:]]+|-i|--|[A-Za-z_][A-Za-z0-9_]*=[^[:space:]]*))*"
_RO_BOUNDARY = r"(^[[:space:]]*|[;&|(!][[:space:]]*|\$\([[:space:]]*)"
_RO_WRAPPER = (rf"(command[[:space:]]+|{_ENV_PREFIX}[[:space:]]+|xargs([[:space:]]+[^;&|[:space:]]+)*[[:space:]]+|"
               r"(sudo|timeout|nice|exec|nohup|time)([[:space:]]+[^;&|[:space:]]+)*[[:space:]]+|"
               r"[A-Za-z_][A-Za-z0-9_]*=[^[:space:]]*[[:space:]]+)")


def contains_raw_shell_op_command(raw: str) -> bool:
    """Whether RAW runs the 1Password CLI `op` itself."""
    if grep(rf"""(^|[;&|(!][[:space:]]*|\$\([[:space:]]*)({_ENV_PREFIX}[[:space:]]+)?(((/bin/|/usr/bin/)?"""
             rf"""(bash|sh|zsh|dash|ksh))[[:space:]]+-l?c|eval)[[:space:]]+["'][^"']*{_OP_BIN}([[:space:]]+|["'])""",
             raw):
        return True
    scan = command_substitution(sed(mask_op_quoted_literals(raw),
                                    (r"(^|[;&|])[[:space:]]*(if|then|elif|while|until|do)[[:space:]]+", r"\1 ", "g")))
    return (grep(f"{_RO_BOUNDARY}{_OP_BIN}([[:space:]]+|$)", scan)
            or grep(f"{_RO_BOUNDARY}{_RO_WRAPPER}{_OP_BIN}([[:space:]]+|$)", scan)
            or grep(f"find[[:space:]].*-exec[[:space:]]+{_OP_BIN}([[:space:]]+|$)", scan)
            or grep(r"\$\([[:space:]]*command[[:space:]]+-v[[:space:]]+op[[:space:]]*\)", scan))


# Claude's hook also caught `op` as the cmd of a quoted JavaScript exec object.
_CMD_FIELD_OP = (r"""cmd[[:space:]]*:[[:space:]]*["'`][[:space:]]*((command|env|xargs|sudo|timeout)[[:space:]]+|"""
                 rf"""(bash|sh|zsh|dash|ksh)[[:space:]]+-l?c[[:space:]]+["'])?{_OP_BIN}([[:space:]]+|["'`])""")


def contains_normalized_raw_op(command: str) -> bool:
    """Quote and backslash removal can spell raw `op` without writing it."""
    return grep(r"(^[[:space:]]*|[;&|(!`][[:space:]]*|\$\([[:space:]]*)((([^;&|[:space:]]*/)?(command|env|sudo|"
                 r"timeout|nice|exec|nohup|time|builtin)([[:space:]]+[^;&|[:space:]]+)*[[:space:]]+|"
                 r"[A-Za-z_][A-Za-z0-9_]*=[^;&|[:space:]]*[[:space:]]+))*([^;&|[:space:]]*/)?op([[:space:]]|$)",
                 normalize_shell_words(command))


_REVEAL = r"(^|[[:space:]])--reveal([^[:alnum:]_-]|$)"


def contains_op_reveal_output(command: str) -> bool:
    return (grep(r"(^[[:space:]]*|[;&|(!][[:space:]]*)(op-automations|op-desktop|((/usr/bin/|/bin/)?env)"
                  rf"[[:space:]]+-u[[:space:]]+OP_SERVICE_ACCOUNT_TOKEN[[:space:]]+{_OP_BIN}|{_OP_BIN})[[:space:]]+",
                  command)
            and grep(_REVEAL, command))


# --- secret shapes ------------------------------------------------------------------

# Order matters: more specific patterns come first so the most precise label wins.
SECRET_PATTERNS = (
    (r"AKIA[0-9A-Z]{16}", "AWS access key"),
    (r"gh[ps]_[A-Za-z0-9_]{36,}", "GitHub token"),
    (r"github_pat_[A-Za-z0-9_]{22,}", "GitHub personal access token"),
    (r"xox[bporca]-[A-Za-z0-9-]{10,}", "Slack token"),
    (r"sk-ant-[A-Za-z0-9_-]{40,}", "Anthropic API key"),
    (r"(^|[^A-Za-z0-9])sk-[a-zA-Z0-9_-]{20,}", "API secret key (sk-...)"),
    (r"(sk|pk|rk)_(live|test)_[A-Za-z0-9]{20,}", "Stripe API key"),
    (r"lin_api_[A-Za-z0-9]{40,}", "Linear API key"),
    (r"pat[A-Za-z0-9]{14}\.[a-f0-9]{64}", "Airtable personal access token"),
    (r"-----BEGIN (RSA |EC |OPENSSH )?PRIVATE KEY-----", "private key"),
    (r'"client_secret"\s*:\s*"[A-Za-z0-9_-]{20,}"', "Google OAuth client secret"),
    (r"AIza[0-9A-Za-z_-]{35}", "Google API key"),
    (r"GOCSPX-[A-Za-z0-9_-]{20,}", "Google OAuth client secret"),
    (r"(^|[^A-Za-z0-9/])1//[A-Za-z0-9_-]{30,}", "Google OAuth refresh token"),
    (r"ya29\.[A-Za-z0-9_-]{20,}", "Google OAuth access token"),
    (r"glpat-[A-Za-z0-9_-]{20,}", "GitLab token"),
    (r"eyJ[A-Za-z0-9_-]+\.eyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+", "JWT"),
    (r"(postgres|postgresql|mysql|mongodb|mongodb\+srv|redis|amqp|amqps|mssql)://[^:/ ]+:[^@/ ]+@",
     "database connection string with embedded credentials"),
    (r"""(api[_-]?key|api[_-]?secret|secret[_-]?key|access[_-]?token|auth[_-]?token)\s*[=:]\s*['"][A-Za-z0-9/+=_-]{32,}""",
     "hardcoded secret value"),
)
_SENSITIVE_PATH = r"\.(ssh/id_|zshenv-secrets|gnupg/)|tokens\.json"

# --- deny texts -----------------------------------------------------------------------


def _deny_op_secret_output(tool: str, reason: str) -> Result:
    return Result.deny(
        "BLOCKED: " + tool + " would print a 1Password secret into agent output: " + reason + ".\n\nInvoking "
        "`op-automations` or `op-desktop` is fine; printing what they return is not. Allowed shapes: `op-automations "
        "run --env-file F -- <program>` (masked; not a shell or environment dumper); `X=$(op-automations read REF)` "
        "used by a non-printing command; `read REF > file`, or piped to `pbcopy`, `gh secret set`, `wrangler secret "
        "put`; `item get ID --format=json | jq` selecting only metadata keys; `document get`/`inject` with "
        "`--out-file`; writes without `--format`. Anything else is denied.")


def _deny_raw_op_command(tool: str) -> Result:
    return Result.deny(
        "BLOCKED: " + tool + " contains a direct 1Password CLI command, which can trigger a separate Touch ID prompt "
        "for every process.\n\nUse `op-desktop ...` for desktop-gated operations: personal-vault reads, item "
        "creates/edits, share links. It runs every command inside one authorized terminal session, so a whole task "
        "costs one Touch ID prompt instead of one per command.\n\nFor prompt-free read-only access to the Automations "
        "vault, use `op-automations ...` (for example `op-automations run --env-file .env.op -- <program>`). If the "
        "broker is unavailable, repair it rather than bypassing it with raw `op`.")


def _deny_op_reveal_output(tool: str) -> Result:
    return Result.deny(
        "BLOCKED: " + tool + " command would print a revealed 1Password field.\n\nDo not run `... --reveal` in an "
        "agent shell, including through `op-automations` or `op-desktop`. Capture the value with "
        "`X=$(op-automations read REF)` for a non-printing command, or write it to a mode-0600 file.")


def _deny_secret_output_command(tool: str, issue: str) -> Result:
    if issue == "glob":
        return Result.deny(
            "BLOCKED: " + tool + " runs a program named by a glob (`*`, `?` or `[...]` in a command position), "
            "which could expand to a credential or clipboard tool such as `security` or `pbpaste`.\n\nName the "
            "program literally. Globs in arguments are fine; this applies only where a word is run as a command, "
            "including after wrappers such as `env`, `xargs` or `find -exec`, and inside `bash -c` or `eval` strings.")
    return Result.deny(
        "BLOCKED: " + tool + " cannot be classified as safe from secret-printing credential or clipboard commands."
        "\n\nThis guard denies executable protected tool names and unclassified interpreter programs containing "
        "those names, including prose strings. A denial does not establish that a credential command was invoked. "
        "Only recognized inert mentions and the closed Python document-edit language are exempt.\n\nAgent shell "
        "commands may not call `security` or `pbpaste`, whose output is the secret itself; wrappers, nested shells, "
        "pipes, and redirects are not trusted containment. Epoch secrets live in 1Password: use "
        "`op-automations`/`op-desktop` in one of the allowed non-printing shapes.")


def _deny_process_output(reason: str) -> Result:
    return Result.deny(
        "BLOCKED: " + reason + ". Process arguments and environments can contain credentials. Use PID-only pgrep or "
        "ps with explicit safe columns such as pid,ppid,comm. Filtering or redirecting unsafe output does not remove "
        "this restriction.\n\nTo wait for a process, keep its PID (`$!`) and poll `kill -0 PID`, or bracket one "
        "character of a `pgrep -f` pattern, as in `pgrep -f '[w]orker.py run'`. Your command runs as "
        "`zsh -c '<command>'`, so an unbracketed `pgrep -f` pattern matches that shell itself and a wait loop on it "
        "never ends.")


def _deny_function_body(reason: str) -> Result:
    return Result.deny(
        "BLOCKED: " + reason + ". Shell function bodies can contain credentials, and output redaction only masks "
        "known credential shapes. To learn what a name is, use `whence -w NAME` or `command -v NAME`; to read a "
        "function, read its source file (subject to the sensitive-read guard).")


_DENY_UNCLASSIFIED_NESTED = Result.deny(
    "BLOCKED: nested exec commands could not be classified safely; tool execution denied.")


def _secret_pattern_denial(tool: str, content: str, pattern: str, label: str) -> Result | None:
    """check_pattern: deny when PATTERN matches `echo "$CONTENT"`."""
    if not grep(pattern, content, echo=True):
        return None
    return Result.deny(
        "BLOCKED: " + tool + " command would expose a secret (" + label + ").\n\nUse environment variables or "
        "`op://` references instead of literal secret values.\n\nIf this is a false positive, do not add an "
        "exemption to the guard: fetch a public URL with WebFetch instead, and read "
        "~/My Drive/dotfiles/docs/secret-guard.md before changing the guard. Do not bypass it.")


# --- payload ------------------------------------------------------------------------


def _claude_content(call: Call, tool: str) -> str | None:
    """The text Claude's hook scanned, or None for a tool it ignored."""
    if tool == "Bash":
        return call.shell_command()
    keys = {"Write": ("file_path", "content"), "Edit": ("file_path", "new_string"),
            "NotebookEdit": ("notebook_path", "new_source")}.get(tool)
    if keys is None:
        return None
    parts = [jq_alternative(call.tool_input.get(key), "") for key in keys]
    if not all(isinstance(part, str) for part in parts):
        raise HookFailure("jq cannot add a non-string field")
    return command_substitution(parts[0] + "\n" + parts[1])


def _jq_reparsed(text: str) -> str:
    """TEXT after json.dumps and jq's parser: escaped surrogate pairs join, a
    lone low surrogate becomes U+FFFD, and a lone high one is a parse error."""
    out, i = [], 0
    while i < len(text):
        code = ord(text[i])
        if 0xD800 <= code < 0xDC00:
            low = ord(text[i + 1]) if i + 1 < len(text) else 0
            if not 0xDC00 <= low < 0xE000:
                raise ValueError("invalid surrogate pair")
            out.append(chr(0x10000 + ((code - 0xD800) << 10) + (low - 0xDC00)))
            i += 2
            continue
        out.append("�" if 0xDC00 <= code < 0xE000 else text[i])
        i += 1
    return "".join(out)


def _nested_commands(content: str) -> list[str] | None:
    """The literal nested exec commands of a functions.exec program, or None
    when any call (or the extraction) cannot be classified."""
    try:
        contexts = nested_exec_contexts(content)
        commands = []
        for context in contexts:
            command, workdir = context["cmd"], context["workdir"]
            if (context["ambiguous"] is not False or not isinstance(command, str) or "\0" in command
                    or not (workdir is None or (isinstance(workdir, str) and "\0" not in workdir))):
                return None
            if workdir is not None:
                _jq_reparsed(workdir)
            commands.append(_jq_reparsed(command))
    except Exception:  # noqa: BLE001 - extractor or record parse failed
        return None
    return commands


# --- decision ---------------------------------------------------------------------------


def decide(call: Call) -> Result:
    codex = call.runtime == CODEX
    tool = command_substitution(jq_text(call.raw.get("tool_name")))
    shell = tool in CODEX_SHELL_TOOLS if codex else tool == "Bash"
    if codex:
        if shell:
            content = call.shell_command()
        elif tool in ("Write", "Edit", "apply_patch"):
            content = codex_patch_content_for_scan(call.raw)
        else:
            return Result.allow()
    else:
        content = _claude_content(call, tool)
        if content is None:
            return Result.allow()
    if not content:
        return Result.allow()

    nested: list[str] = []
    literal = False
    program = codex and tool == "functions.exec"
    if program:
        nested = _nested_commands(content)
        if nested is None:
            return _DENY_UNCLASSIFIED_NESTED
        literal = len(nested) == 1 and literal_exec_wrapper(content)

    if shell:
        result = _shell_allowlist(tool, content, nested if program else None, literal, codex)
        if result is not None:
            return result

    if not codex and tool == "Write":
        if command_substitution(jq_text(call.tool_input.get("file_path"))).endswith((".zshenv-secrets", ".env.local")):
            return Result.allow()

    known = _known_secret_label(content)
    if known is not None:
        result = _secret_pattern_denial(tool, content, ".", known)
        if result is not None:
            return result
    for pattern, label in SECRET_PATTERNS:
        result = _secret_pattern_denial(tool, content, pattern, label)
        if result is not None:
            return result

    if shell and not _skips_exfiltration_checks(content):
        result = _exfiltration(call, tool, content, nested[0] if literal else content)
        if result is not None:
            return result
    return Result.allow()


def _shell_allowlist(tool: str, content: str, nested: list[str] | None, literal: bool, codex: bool) -> Result | None:
    """The hook's shell-command denials, or None to continue. NESTED is the
    literal command list of a Codex functions.exec program, else None."""
    sources = [content] if nested is None else nested
    for source in sources:
        reason = process_output_denial(source)
        if reason is not None:
            return _deny_process_output(reason)
    for source in sources:
        reason = function_body_denial(source)
        if reason is not None:
            return _deny_function_body(reason)

    document_edits = nested is None or literal
    if nested is None or (not literal and _program_can_run_commands(content)):
        issue = secret_output_issue(content, document_edits=document_edits)
        if issue is not None:
            return _deny_secret_output_command(tool, issue)
    for source in nested or ():
        issue = secret_output_issue(source, document_edits=document_edits)
        if issue is not None:
            return _deny_secret_output_command(tool, issue)

    if nested is None:
        reason = op_command_denial(content)
        if reason is not None:
            return _deny_op_secret_output(tool, reason)
        if contains_normalized_raw_op(content):
            return _deny_raw_op_command(tool)
    else:
        residual = content
        for source in nested:
            reason = op_policy_denial(source)
            if reason is not None:
                return _deny_op_secret_output(tool, reason)
            if contains_normalized_raw_op(source):
                return _deny_raw_op_command(tool)
            residual = residual.replace(source, "")
        if grep("op-automations|op-desktop|OP_RUN_NO_MASKING", sed(residual, _STRIP_QUOTES)):
            return _deny_op_secret_output(tool, "a 1Password broker is named outside a literal nested exec command")

    if any(contains_op_reveal_output(source) for source in [content, *(nested or ())]):
        return _deny_op_reveal_output(tool)
    if ((not codex and grep(_CMD_FIELD_OP, content))
            or any(contains_raw_shell_op_command(source) for source in [content, *(nested or ())])):
        return _deny_raw_op_command(tool)
    if (grep(r"^\s*op\s+item\s+get\b", content, echo=True) and grep(_REVEAL, content, echo=True)
            and not grep(r"[|>]", content, echo=True)):
        return _deny_op_reveal_output(tool)
    return None


# What lets a functions.exec JavaScript program start a process: the exec
# tool however it is reached (by name, or through `tools` aliased or indexed
# by a computed key), Node process APIs, and code or module loading that can
# reach them without naming them (eval, Function, require, import()).
_RUNS_COMMANDS = re.compile(
    r"exec_command|child_process|execSync|execFile|spawn|\bfork\b|\bexec\s*\(|process\s*\.\s*(binding|dlopen)"
    r"|\beval\b|\bFunction\b|\brequire\b|\bimport\s*\(|\btools\b(?!\s*\.\s*[A-Za-z_$][\w$]*)")


def _program_can_run_commands(program: str) -> bool:
    """Whether the shell-text scan of a whole functions.exec program applies.

    Intended difference: the hook scanned every program as shell text, so a
    JavaScript `?`, `*` or `[` after `;`, `(`, `|` or a backquote read as an
    executable glob and denied plain browser automation (bash 3.2's
    unbound-variable exit had hidden this in production). The scan only
    matters where the program can run a command; its literal nested commands
    are still scanned one by one, dynamic ones are still denied, and the
    known-value and secret-shape checks still cover the whole program."""
    return _RUNS_COMMANDS.search(program) is not None


def _skips_exfiltration_checks(content: str) -> bool:
    """The hook's shell allowlist: git-crypt, grep/rg scans, `$VAR` references
    without a token prefix, and mentions of the encrypted secrets file.

    Intended difference, stricter than the hook: it exited here, before the
    known-value and secret-shape checks. Those now run on every command, so
    the allowlist skips only the exfiltration checks.
    """
    return (grep(r"^\s*git-crypt ", content, echo=True) or grep(r"^\s*(grep|rg|ripgrep)\s", content, echo=True)
            or (grep(r"\$\{?[A-Z_]+\}?", content, echo=True)
                and not grep(r"(AKIA|ghp_|ghs_|github_pat_|xox[bporca]-|sk-[a-zA-Z0-9]{20,}|-----BEGIN)", content,
                              echo=True))
            or grep(r"\.zshenv-secrets", content, echo=True))


def _known_secret_label(content: str) -> str | None:
    """`bin/secret-index contains`: the check_pattern label for a known secret
    value (status 0) or a failed check (status 2), None when there is none."""
    try:
        index = secret_index.Index.load()
        spans = index.find(content) if index is not None else []
    except Exception:  # noqa: BLE001 - the helper exited 2
        return "known-secret check failed"
    if not spans:
        return None
    labels = command_substitution(", ".join(sorted({label for _, _, label in spans})) + "\n")
    return f"contains a known secret value ({labels})"


def _exfiltration(call: Call, tool: str, content: str, entropy_command: str) -> Result | None:
    """Sensitive files piped or uploaded to a network tool. A network command
    carrying an opaque inline string is only logged (url_heuristic)."""
    if (grep(rf"(cat|base64|xxd)\s+[^\|;]*{_SENSITIVE_PATH}", content, echo=True)
            and grep(r"\|\s*(curl|wget|nc|ncat)\b", content, echo=True)):
        result = _secret_pattern_denial(tool, content, ".", "sensitive file piped to network tool (exfiltration risk)")
        if result is not None:
            return result
    if (grep(r"curl\s", content, echo=True) and grep(r"(-d\s*@|--data[a-z-]*\s*@)", content, echo=True)
            and grep(_SENSITIVE_PATH, content, echo=True)):
        result = _secret_pattern_denial(tool, content, ".", "curl uploading sensitive file (exfiltration risk)")
        if result is not None:
            return result
    _log_opaque_token(call, tool, content, entropy_command)
    return None


def _opaque_token_issue(entropy_command: str) -> str | None:
    """The hook's opaque-token gate, unchanged: a network tool in the command's
    network activation (outside terminal document prose and data-only Python
    names), then the former URL classifier on the local-read-path projection."""
    activation = mask_heredoc_bodies(entropy_command, "network")
    activation = command_substitution(classifiers.load("python-heredoc")._project_command(
        activation, git_shell=False, network_activation=True))
    if not grep(r"(^|[^A-Za-z0-9_.-])(curl|wget|nc|ncat)([^A-Za-z0-9_.-]|$)|\b(python[23]?\s.*urllib|node\s.*fetch)\b",
                activation, echo=True):
        return None
    scanned = command_substitution(classifiers.load("inert-mentions").mask_local_read_paths(entropy_command))
    return url_heuristic.finding(scanned)


def _log_opaque_token(call: Call, tool: str, content: str, entropy_command: str) -> None:
    """Log where the opaque-token heuristic would have been the only reason to
    deny. A failing projection used to deny as well, so it is logged too.
    Nothing else depends on these projections, so logging never denies."""
    try:
        issue = _opaque_token_issue(entropy_command)
    except Exception:  # noqa: BLE001 - the hook denied when a projection failed
        issue = "projection failed"
    try:
        if issue and grep(".", content, echo=True):
            url_heuristic.record(call.runtime, tool)
    except Exception:  # noqa: BLE001 - logging must never deny
        pass
