#!/usr/bin/env python3
"""1Password broker output policy for the secret-leak guards.

The brokers ``op-automations`` and ``op-desktop`` are the sanctioned way for
an agent shell to reach 1Password. This module decides whether one shell
command uses them in a shape whose stdout provably carries no credential.
It is an allowlist: every broker invocation must match one of the shapes in
``classify_invocation``; anything else, including a shape the tokenizer
cannot place, is denied.

Contract: read the (heredoc-masked) command on stdin, print one JSON object
``{"decision": "allow"}`` or ``{"decision": "deny", "reason": "..."}``.
Raw ``op``, ``pass``, ``security``, ``pbpaste`` and executable globs are the
calling hook's business, not this module's.

Plan and rationale: docs/superpowers/plans/2026-09-02-secret-guard-op-output-policy.md
"""

from __future__ import annotations

import json
import os
import re
import sys
from dataclasses import dataclass, field

BROKERS = {"op-automations", "op-desktop"}
WRAPPERS = {"env", "sudo", "command", "timeout", "nice", "exec", "nohup", "time", "builtin"}
SHELLS = {"bash", "sh", "zsh", "dash", "ksh"}
CONTROL_WORDS = {"if", "then", "elif", "else", "fi", "while", "until", "do", "done", "case", "esac"}
OPERATORS = {";", "&", "&&", "|", "||", "(", ")", "{", "}", "!", "\n", ";;"}
# Programs whose stdout is (or trivially can be) their arguments or their
# environment. A captured secret handed to one of these is printed.
PRINTERS = {
    "echo", "printf", "cat", "tee", "head", "tail", "less", "more", "xxd", "od",
    "base64", "jq", "awk", "sed", "cut", "tr", "rev", "fold", "node", "perl",
    "ruby", "eval", "xargs", "env", "printenv", "set", "export", "declare",
    "typeset",
} | SHELLS
# Programs `op run` may not launch: they print the environment they were given.
RUN_DENIED_PROGRAMS = {"env", "printenv", "set", "export", "declare", "typeset", "eval"} | SHELLS
# Words that may take a broker name as inert data.
DATA_COMMANDS = {"echo", "printf", "grep", "rg", "ripgrep", "git", "ls", "stat", "readlink"}
# Commands that may consume a piped secret without printing it.
CONSUMERS = (
    ("pbcopy",),
    ("gh", "secret", "set"),
    ("wrangler", "secret", "put"),
    ("op-desktop", "item", "create"),
    ("op-desktop", "item", "edit"),
    ("ssh-add", "-"),
    ("gpg", "--import"),
    ("docker", "login"),
)
SUBCOMMANDS = {
    "read", "run", "inject", "item", "document", "vault", "user", "group", "whoami",
    "signin", "signout", "account", "connect", "events-api", "service-account",
    "plugin", "completion", "update", "environment",
}
GLOBAL_FLAGS_NO_VALUE = {"--status", "--stop", "--version", "--help", "-h", "--no-color", "--debug", "--cache", "--iso-timestamps"}
META_JQ_KEYS = {
    "id", "title", "category", "vault", "name", "created_at", "updated_at",
    "last_edited_by", "tags", "version", "fields", "label", "purpose", "type",
    "section",
}
JQ_FORBIDDEN = ("..", "to_entries", "tojson", "tostring", "@", "env", "input", "value", "password", "$__loc__", "getpath", "paths", "leaf_paths", "keys", "with_entries", "map_values")


class Deny(Exception):
    """Raised with the reason a command is denied."""


@dataclass
class Word:
    text: str          # value after quote removal (expansions kept as text)
    quoted: bool       # any part was quoted
    expands: bool      # contains an unquoted-or-double-quoted `$` or backtick
    raw: str           # original source slice


@dataclass
class Redirect:
    fd: str            # "1", "2", "&" (both)
    op: str            # ">", ">>", "<", "<<<", ">&"
    target: str


@dataclass
class Simple:
    words: list[Word] = field(default_factory=list)
    redirects: list[Redirect] = field(default_factory=list)
    assignments: list[tuple[str, Word]] = field(default_factory=list)
    here_string: Word | None = None


@dataclass
class Capture:
    """A `$(...)` or backtick substitution lifted out of the outer command."""
    inner: str
    placeholder: str


# ----------------------------------------------------------------------------
# Tokenizer


def _lift_substitutions(text: str) -> tuple[str, list[Capture], bool]:
    """Replace `$(...)` and backtick substitutions with placeholders.

    Returns the rewritten text, the captures, and whether a process
    substitution `<(...)` / `>(...)` was seen (those are denied outright when
    they contain a broker; the caller checks).
    """
    out: list[str] = []
    captures: list[Capture] = []
    i, n = 0, len(text)
    in_single = False
    procsub = False
    while i < n:
        c = text[i]
        if in_single:
            out.append(c)
            if c == "'":
                in_single = False
            i += 1
            continue
        if c == "\\" and i + 1 < n:
            out.append(text[i:i + 2])
            i += 2
            continue
        if c == "'":
            in_single = True
            out.append(c)
            i += 1
            continue
        if c == "$" and text.startswith("$(", i) and not text.startswith("$((", i):
            depth, j = 0, i + 1
            while j < n:
                if text[j] == "(":
                    depth += 1
                elif text[j] == ")":
                    depth -= 1
                    if depth == 0:
                        break
                elif text[j] == "'":
                    k = text.find("'", j + 1)
                    j = n if k < 0 else k
                j += 1
            if j >= n:
                raise Deny("unterminated command substitution")
            inner = text[i + 2:j]
            ph = f"\x00SUBST{len(captures)}\x00"
            captures.append(Capture(inner, ph))
            out.append(ph)
            i = j + 1
            continue
        if c == "`":
            j = text.find("`", i + 1)
            if j < 0:
                raise Deny("unterminated backtick substitution")
            inner = text[i + 1:j]
            ph = f"\x00SUBST{len(captures)}\x00"
            captures.append(Capture(inner, ph))
            out.append(ph)
            i = j + 1
            continue
        if c in "<>" and text.startswith(c + "(", i):
            procsub = True
        out.append(c)
        i += 1
    return "".join(out), captures, procsub


def _tokenize(text: str) -> list[str | Word]:
    """Split into Words and operator strings, honouring quotes and escapes."""
    tokens: list[str | Word] = []
    i, n = 0, len(text)
    cur: list[str] = []
    raw: list[str] = []
    quoted = expands = False

    def flush() -> None:
        nonlocal cur, raw, quoted, expands
        if cur or quoted:
            tokens.append(Word("".join(cur), quoted, expands, "".join(raw)))
        cur, raw, quoted, expands = [], [], False, False

    while i < n:
        c = text[i]
        if c == "\\" and i + 1 < n:
            cur.append(text[i + 1])
            raw.append(text[i:i + 2])
            i += 2
            continue
        if c == "'":
            j = text.find("'", i + 1)
            if j < 0:
                raise Deny("unterminated single quote")
            cur.append(text[i + 1:j])
            raw.append(text[i:j + 1])
            quoted = True
            i = j + 1
            continue
        if c == '"':
            j = i + 1
            buf: list[str] = []
            while j < n and text[j] != '"':
                if text[j] == "\\" and j + 1 < n:
                    buf.append(text[j + 1])
                    j += 2
                    continue
                if text[j] in "$`":
                    expands = True
                buf.append(text[j])
                j += 1
            if j >= n:
                raise Deny("unterminated double quote")
            cur.append("".join(buf))
            raw.append(text[i:j + 1])
            quoted = True
            i = j + 1
            continue
        if c in " \t":
            flush()
            i += 1
            continue
        if c == "\n":
            flush()
            tokens.append("\n")
            i += 1
            continue
        two = text[i:i + 2]
        if two in ("&&", "||", ";;"):
            flush()
            tokens.append(two)
            i += 2
            continue
        if c in ";&|(){}":
            # `{`/`}` are operators only as separate words.
            if c in "{}" and (cur or (i + 1 < n and text[i + 1] not in " \t\n;&|")):
                cur.append(c)
                raw.append(c)
                i += 1
                continue
            flush()
            tokens.append(c)
            i += 1
            continue
        if c == "!" and not cur and (i + 1 >= n or text[i + 1] in " \t"):
            flush()
            tokens.append("!")
            i += 1
            continue
        if c in "<>":
            # Redirect operator glued to a leading fd digit, e.g. `2>`.
            flush_prefix = None
            if cur and "".join(cur).isdigit() and not quoted:
                flush_prefix = "".join(cur)
                cur, raw = [], []
            j = i
            while j < n and text[j] in "<>&":
                j += 1
            op = text[i:j]
            k = j
            while k < n and text[k] in " \t":
                k += 1
            m = k
            while m < n and text[m] not in " \t\n;&|()":
                m += 1
            target = text[k:m]
            flush()
            tokens.append(Word(f"\x01REDIR\x01{flush_prefix or ''}\x01{op}\x01{target}", False, "$" in target or "`" in target or "\x00" in target, text[i:m]))
            i = m
            continue
        if c in "$`":
            expands = True
        cur.append(c)
        raw.append(c)
        i += 1
    flush()
    return tokens


def _split_simple(tokens: list[str | Word]) -> list[list[Simple | str]]:
    """Group tokens into pipelines of Simple commands.

    Returns a list of pipelines; each pipeline is a list of Simple commands
    joined by `|`. Other operators end the pipeline.
    """
    pipelines: list[list[Simple]] = []
    pipeline: list[Simple] = []
    cur = Simple()
    at_command_start = True

    def end_simple() -> None:
        nonlocal cur, at_command_start
        if cur.words or cur.assignments or cur.redirects:
            pipeline.append(cur)
        cur = Simple()
        at_command_start = True

    def end_pipeline() -> None:
        nonlocal pipeline
        end_simple()
        if pipeline:
            pipelines.append(pipeline)
        pipeline = []

    for tok in tokens:
        if isinstance(tok, str):
            if tok == "|":
                end_simple()
            else:
                end_pipeline()
            continue
        if tok.text.startswith("\x01REDIR\x01"):
            _, _, fd, op, target = tok.text.split("\x01")
            if op == "<<<":
                cur.here_string = Word(target, False, tok.expands, tok.raw)
            else:
                cur.redirects.append(Redirect(fd or ("&" if op.startswith("&") else "1"), op, target))
            continue
        if at_command_start and tok.text in CONTROL_WORDS and not tok.quoted:
            continue  # `if`, `then`, `do` ... precede a command position
        m = re.match(r"^([A-Za-z_][A-Za-z0-9_]*)=(.*)$", tok.text, re.S)
        # `OP='op-automations'` is still an assignment: the name and `=` are
        # unquoted in the source even when the value is quoted.
        if at_command_start and m and tok.raw.startswith(m.group(1) + "="):
            cur.assignments.append((m.group(1), Word(m.group(2), False, tok.expands, tok.raw)))
            continue
        at_command_start = False
        cur.words.append(tok)
    end_pipeline()
    return pipelines


# ----------------------------------------------------------------------------
# Classification helpers


def _basename(word: str) -> str:
    return word.rsplit("/", 1)[-1]


WRAPPER_VALUE_FLAGS = {
    "env": {"-u", "--unset", "-C", "--chdir", "-S", "--split-string"},
    "sudo": {"-u", "-g", "-p", "-h", "-C", "-r", "-t", "-U"},
    "nice": {"-n", "--adjustment"},
    "timeout": {"-s", "--signal", "-k", "--kill-after"},
}


def _strip_wrappers(words: list[Word]) -> list[Word]:
    """Drop leading wrapper programs and their options (with values)."""
    i = 0
    while i < len(words) and _basename(words[i].text) in WRAPPERS and not words[i].quoted:
        wrapper = _basename(words[i].text)
        value_flags = WRAPPER_VALUE_FLAGS.get(wrapper, set())
        i += 1
        while i < len(words):
            t = words[i].text
            if t in value_flags:
                i += 2
                continue
            if t.startswith("-") or "=" in t or re.fullmatch(r"\d+[smhd]?", t):
                i += 1
                continue
            break
    return words[i:]


def _is_broker(word: Word) -> bool:
    return _basename(word.text) in BROKERS and not word.expands


def _regular_path(target: str, captured: set[str]) -> bool:
    if not target or target == "-":
        return False
    if target.startswith("&"):
        return False
    if target.startswith("/dev/") and target != "/dev/null":
        return False
    if target.startswith(("/proc/", "/dev/fd/")):
        return False
    if "\x00" in target:  # a substitution as the path
        return False
    for var in captured:
        if re.search(r"\$\{?" + re.escape(var) + r"\b", target):
            return False
    return True


def _stdout_redirect(simple: Simple, captured: set[str]) -> str | None:
    """Return 'file' when stdout goes to a regular path, 'bad' when it goes
    somewhere visible or unknown, None when it is not redirected."""
    result = None
    for r in simple.redirects:
        if r.fd in ("1", "&") and r.op in (">", ">>", "&>", "&>>"):
            result = "file" if _regular_path(r.target, captured) else "bad"
        elif r.fd == "1" and r.op.startswith(">&"):
            return "bad"
        elif r.op == ">&" or (r.fd == "&" and r.op == ">"):
            return "bad" if not _regular_path(r.target, captured) else "file"
    return result


def _flag_value(words: list[str], names: tuple[str, ...]) -> str | None:
    for i, w in enumerate(words):
        for name in names:
            if w == name and i + 1 < len(words):
                return words[i + 1]
            if w.startswith(name + "="):
                return w[len(name) + 1:]
    return None


def _jq_filter_is_metadata(filter_text: str) -> bool:
    text = filter_text.strip()
    if text in ("", ".", ".[]", ".[ ]"):
        return False
    lowered = text.lower()
    for bad in JQ_FORBIDDEN:
        if bad in lowered:
            return False
    keys = set(re.findall(r"\.([A-Za-z_][A-Za-z0-9_]*)", text))
    for group in re.findall(r"\{([^{}]*)\}", text):
        for part in group.split(","):
            part = part.strip()
            if not part:
                continue
            name = part.split(":", 1)[0].strip()
            if name.startswith("."):
                name = name[1:]
            if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", name or ""):
                keys.add(name)
    if not keys:
        return False
    return keys <= META_JQ_KEYS


def _matches_consumer(words: list[Word]) -> bool:
    words = _strip_wrappers(words)
    texts = [_basename(w.text) if i == 0 else w.text for i, w in enumerate(words)]
    for pattern in CONSUMERS:
        if tuple(texts[:len(pattern)]) == pattern:
            if pattern == ("docker", "login") and "--password-stdin" not in texts:
                return False
            return True
    return False


def _parse_global_flags(words: list[str]) -> tuple[list[str], list[str]]:
    """Return (global flags, remaining words) for a broker invocation."""
    flags: list[str] = []
    i = 0
    while i < len(words) and words[i].startswith("-"):
        flag = words[i]
        flags.append(flag)
        i += 1
        if "=" in flag or flag in GLOBAL_FLAGS_NO_VALUE:
            continue
        if i < len(words) and words[i] not in SUBCOMMANDS and not words[i].startswith("-"):
            flags.append(words[i])
            i += 1
    return flags, words[i:]


# ----------------------------------------------------------------------------
# Invocation classification


def classify_invocation(
    simple: Simple,
    words: list[Word],
    *,
    context: str,
    next_in_pipeline: Simple | None,
    captured: set[str],
) -> None:
    """Raise Deny unless this broker invocation is an allowed shape.

    ``context`` is "command" for a top-level simple command, "assign" for the
    body of `VAR=$(...)`, "argument" for a substitution used as an argument.
    """
    if any(a[0].startswith("OP_") for a in simple.assignments):
        raise Deny("OP_* environment assignment on a 1Password command (masking or auth override)")
    texts = [w.text for w in words[1:]]
    # Expansions in arguments (`--env-file "$ROOT/.env.op"`, `--out-file "$TMP"`)
    # do not change a shape's stdout; an expansion in the subcommand position
    # falls through to the unclassified denial below.
    if "--reveal" in texts:
        raise Deny("--reveal prints a concealed field")
    flags, rest = _parse_global_flags(texts)
    if not rest:
        if flags and all(f in ("--status", "--stop", "--version", "--help", "-h") for f in flags):
            return
        raise Deny("1Password command without a recognized subcommand")
    sub = rest[0]
    args = rest[1:]
    stdout = _stdout_redirect(simple, captured)
    if stdout == "bad":
        raise Deny(f"`{sub}` output redirected somewhere visible or unresolvable")
    if simple.here_string is not None and any(v in simple.here_string.text for v in captured):
        raise Deny("captured secret fed back through a here-string")

    if sub == "run":
        if "--no-masking" in args:
            raise Deny("`run --no-masking` prints injected values")
        if "--" not in args:
            raise Deny("`run` without `-- <program>`")
        program = args[args.index("--") + 1:][:1]
        if not program:
            raise Deny("`run` without a program")
        if _basename(program[0]) in RUN_DENIED_PROGRAMS:
            raise Deny(f"`run` may not launch `{_basename(program[0])}`, which prints its environment")
        if context != "command":
            raise Deny("`run` inside a substitution captures the program's output")
        return

    if sub == "read":
        out_file = _flag_value(args, ("--out-file", "-o"))
        if out_file is not None:
            if _regular_path(out_file, captured):
                return
            raise Deny("`read --out-file` target is not a regular file")
        if context in ("assign", "argument"):
            if next_in_pipeline is not None or simple.redirects:
                raise Deny("captured `read` with a pipe or redirect")
            return
        if stdout == "file":
            return
        if next_in_pipeline is not None and _matches_consumer(next_in_pipeline.words):
            return
        raise Deny("`read` output would reach agent output; capture it into a variable, a regular file, or a listed consumer")

    if sub == "item":
        verb = args[0] if args else ""
        rest_args = args[1:]
        if verb in ("get", "list"):
            if any(a == "--fields" or a.startswith("--fields=") for a in rest_args):
                raise Deny("`item get --fields` prints field values")
            if stdout == "file":
                return
            if next_in_pipeline is not None:
                nw = _strip_wrappers(next_in_pipeline.words)
                if nw and _basename(nw[0].text) == "jq":
                    filters = [w.text for w in nw[1:] if not w.text.startswith("-")]
                    if filters and _jq_filter_is_metadata(filters[0]) and _stdout_redirect(next_in_pipeline, captured) != "bad":
                        return
                    raise Deny("`item` output piped to a jq filter that is not limited to metadata keys")
            raise Deny(f"`item {verb}` output would reach agent output; redirect to a regular file or select metadata keys with jq")
        if verb in ("create", "edit"):
            if any(a == "--format" or a.startswith("--format=") for a in rest_args + flags):
                raise Deny(f"`item {verb} --format` prints field values")
            if stdout == "bad":
                raise Deny(f"`item {verb}` output redirected somewhere visible")
            return
        if verb == "delete":
            return
        if verb == "template" and rest_args[:1] in (["list"], ["get"]):
            return
        raise Deny(f"unclassified 1Password command `item {verb}`")

    if sub == "document":
        verb = args[0] if args else ""
        if verb == "get":
            out_file = _flag_value(args[1:], ("--out-file", "-o"))
            if out_file is not None and _regular_path(out_file, captured):
                return
            if out_file is None and stdout == "file":
                return
            raise Deny("`document get` must write to a regular file with --out-file")
        if verb in ("create", "edit", "delete", "list"):
            return
        raise Deny(f"unclassified 1Password command `document {verb}`")

    if sub == "inject":
        out_file = _flag_value(args, ("--out-file", "-o"))
        if out_file is not None and _regular_path(out_file, captured):
            return
        if out_file is None and stdout == "file":
            return
        raise Deny("`inject` must write to a regular file with --out-file")

    if sub == "whoami":
        return
    if sub in ("vault", "user", "group") and args[:1] in (["list"], ["get"]):
        return
    # Membership and permission listings (`vault user list`, `vault group list`,
    # `group user list`) print names, emails and permission flags, not secrets.
    if sub in ("vault", "group") and args[:2] in (["user", "list"], ["group", "list"]):
        return
    if sub == "vault" and args[:1] == ["create"]:
        return
    if sub == "account" and args[:1] == ["list"]:
        return
    raise Deny(f"unclassified 1Password command `{' '.join([sub] + args[:1])}`")


# ----------------------------------------------------------------------------
# Whole-command classification


def classify(command: str, *, context: str = "command", depth: int = 0) -> None:
    """Raise Deny when ``command`` could print a 1Password secret."""
    if depth > 3:
        raise Deny("substitution nesting too deep to classify")
    if _disables_masking(command):
        raise Deny("OP_RUN_NO_MASKING disables `op run` masking")
    if "${!" in command:
        raise Deny("indirect variable expansion cannot be classified")
    plain = _plain(command)
    text, captures, procsub = _lift_substitutions(command)
    if procsub and any(b in plain for b in BROKERS):
        raise Deny("1Password command inside a process substitution")
    tokens = _tokenize(text)
    pipelines = _split_simple(tokens)
    captured: set[str] = set()
    broker_valued_vars: set[str] = set()

    for pipeline in pipelines:
        for idx, simple in enumerate(pipeline):
            nxt = pipeline[idx + 1] if idx + 1 < len(pipeline) else None
            # Assignments: `VAR=$(broker ...)` captures; `OP=op-automations` is indirection.
            for name, value in simple.assignments:
                for cap in captures:
                    if cap.placeholder in value.text:
                        if value.text.strip() != cap.placeholder:
                            raise Deny("substitution mixed into an assignment value cannot be classified")
                        if any(b in cap.inner for b in BROKERS):
                            _classify_capture(cap.inner, "assign", depth)
                            captured.add(name)
                if _basename(value.text) in BROKERS:
                    broker_valued_vars.add(name)
            words = _strip_wrappers(simple.words)
            if not words:
                continue
            head = words[0]
            if head.expands and any(b in plain for b in BROKERS) and (broker_valued_vars or "\x00" in head.text):
                raise Deny("1Password broker reached through variable or command-discovery indirection")
            if _is_broker(head):
                # A broker in command position: the raw source must spell it
                # plainly; obfuscated spellings still reach here via the
                # tokenizer, which is fine, the shape rules apply regardless.
                classify_invocation(simple, words, context=context, next_in_pipeline=nxt, captured=captured)
                continue
            base = _basename(head.text)
            # Substitutions used as arguments.
            for cap in captures:
                if any(cap.placeholder in w.text for w in words) or any(cap.placeholder in r.target for r in simple.redirects):
                    if not any(b in cap.inner for b in BROKERS):
                        continue
                    if _is_printer(base) or any(cap.placeholder in r.target for r in simple.redirects):
                        raise Deny(f"captured 1Password output handed to `{base}` or a redirect")
                    _classify_capture(cap.inner, "argument", depth)
            # Same-call reuse of a captured variable.
            if captured and _is_printer(base):
                for w in words[1:]:
                    for var in captured:
                        if re.search(r"\$\{?" + re.escape(var) + r"\b", w.raw):
                            raise Deny(f"captured 1Password value handed to `{base}`")
            if captured and simple.here_string is not None:
                for var in captured:
                    if re.search(r"\$\{?" + re.escape(var) + r"\b", simple.here_string.raw):
                        raise Deny("captured 1Password value fed through a here-string")
            for r in simple.redirects:
                for var in captured:
                    if re.search(r"\$\{?" + re.escape(var) + r"\b", r.target):
                        raise Deny("captured 1Password value used as a redirect target")
            # Exactly `bash -n FILE` parses the file without executing it.
            # Extra options (especially `+n`) could re-enable execution.
            if (base == "bash" and len(words) == 3 and words[1].text == "-n"
                    and not words[2].expands and not words[2].text.startswith("-")):
                continue
            # Interpreter programs: a broker inside `bash -c '...'` or `eval`.
            if base in SHELLS or base == "eval":
                for w in words[1:]:
                    if any(b in w.text for b in BROKERS):
                        if _inert_literal_program(w.text):
                            continue
                        raise Deny("1Password broker named inside an interpreter program")
            # Residual broker words as arguments.
            for w in words[1:]:
                if _basename(w.text) in BROKERS and not w.quoted:
                    if base in DATA_COMMANDS:
                        continue
                    raise Deny(f"1Password broker passed as an argument to `{base}`")


def _classify_capture(inner: str, context: str, depth: int) -> None:
    text = inner.strip()
    tokens = _tokenize(_lift_substitutions(text)[0])
    pipelines = _split_simple(tokens)
    if len(pipelines) != 1 or len(pipelines[0]) != 1:
        raise Deny("captured 1Password command must be a single simple command")
    simple = pipelines[0][0]
    words = _strip_wrappers(simple.words)
    if not words or not _is_broker(words[0]):
        raise Deny("substitution names a 1Password broker outside command position")
    classify_invocation(simple, words, context=context, next_in_pipeline=None, captured=set())


def _inert_literal_program(program: str) -> bool:
    """`bash -c 'echo op-automations read'` is documentation, not a call."""
    if any(ch in program for ch in "$`;&|<>()"):
        return False
    words = program.split()
    if not words:
        return True
    return _basename(words[0]) in ("echo", "printf")


def _plain(command: str) -> str:
    """The command with quotes and backslashes removed, for presence checks.

    `op-'automations'` and `op\\-automations` name the broker as surely as the
    plain spelling does; the tokenizer joins them, so the presence check must
    see them too.
    """
    return re.sub(r"['\"\\\\]", "", command)


def _is_printer(base: str) -> bool:
    return base in PRINTERS or base.startswith("python")


def _disables_masking(command: str) -> bool:
    """`OP_RUN_NO_MASKING` assigned anywhere in the command.

    Only an assignment (`OP_RUN_NO_MASKING=1`, `export OP_RUN_NO_MASKING=…`,
    `env OP_RUN_NO_MASKING=1 …`) turns masking off; a commit message or a
    doc grep that merely names the variable does not.
    """
    return re.search(r"OP_RUN_NO_MASKING\s*=", command) is not None


def decide(command: str) -> dict:
    if _disables_masking(command):
        return {"decision": "deny", "reason": "OP_RUN_NO_MASKING disables `op run` masking"}
    if not any(b in _plain(command) for b in BROKERS):
        return {"decision": "allow"}
    try:
        classify(command)
    except Deny as exc:
        return {"decision": "deny", "reason": str(exc)}
    return {"decision": "allow"}


def main() -> int:
    command = sys.stdin.read()
    print(json.dumps(decide(command)))
    return 0


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--self-test":
        here = os.path.dirname(os.path.abspath(__file__))
        sys.exit(os.system(f'python3 -m pytest -q "{os.path.join(here, "..", "..", "tests", "test_op_policy.py")}"') >> 8)
    raise SystemExit(main())
