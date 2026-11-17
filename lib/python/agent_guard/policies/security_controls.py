"""Commands that weaken this Mac's own security controls (2026-11-17).

Denied for both runtimes, with no override an agent can type:

1. AppleScript or JXA (`osascript`) that operates System Events, System
   Settings or System Preferences, or LuLu, and any addressed to
   SecurityAgent. A script may look but not touch: AppleScript given with -e
   or in a heredoc passes when every statement is a recognized read or plain
   control flow (agent_guard.applescript_reads, an allowlist), `activate`
   included. Clicking, typing, setting a property, anything unrecognized, all
   JXA naming these targets, and a script the guard cannot see (a file, a
   pipe, a here-string, an unexpanded variable) in a command that names one
   stay denied;
2. `tccutil`, whatever the subcommand;
3. writes to a privacy database named TCC.db: `sqlite3` without a read-only
   open, an interpreter program that names it without one, redirections and
   file-changing commands;
4. `fdesetup` verbs other than its read-only ones;
5. `spctl` other than --status and --assess;
6. `socketfilterfw` other than its --get* and --listapps reads;
7. stopping or reconfiguring LuLu: kill/pkill/killall naming it, launchctl and
   systemextensionsctl targets naming it, `defaults write/delete` on its
   domain, and writes under /Library/Objective-See/LuLu;
8. removing the quarantine attribute: `xattr -d com.apple.quarantine` and
   `xattr -c`, in any flag order;
9. writing, moving or deleting the anger lock's state (any path under
   `.local/state/anger-lock`), by shell command or by the Edit/Write tools
   (2026-11-17). The lock is Pablo's own commitment device
   (lib/python/anger_lock.py): only its hook may set or clear it, so no agent
   can be talked into lifting it. Unlike the other rules, the denial does not
   offer a command for Pablo to run.

A command is judged by what it runs, not by what it mentions. The shell text
is tokenized with the op-policy classifier's tokenizer, as the function-body
and untrusted-execution classifiers do. Each simple command is unwrapped
(`sudo`, `env`, `xargs`, `find -exec` ...), and code handed to a shell
(`sh -c`, `eval`, a here-string, a pipe, a heredoc, a substitution) is
classified in turn. So a protected name that is only an argument of another
program (`grep -rn tccutil docs/`), part of a commit message, or in a heredoc
fed to a data sink is not a command. Nor is a helper script run by path
(bin/lulu-directory-rules, bin/opentopage ...): the guard reads the agent's
command, not what a program does.

Not recognized: a command built inside another interpreter's program
(`python3 -c '...subprocess...'`), kept in a script file that a later call
runs, or reached through a computed name.
"""

from __future__ import annotations

import json
import re
import shlex
from collections import Counter

from .. import applescript_reads, classifiers
from ..codex_paths import codex_changed_paths
from ..codex_payload import PartialOutput, nested_exec_commands
from ..core import CODEX, Call, Result
from ..shell_text import mask_git_commit_messages, mask_heredoc_bodies
from .launchagent import _plutil_targets

APPLESCRIPT = "AppleScript or JXA that operates System Events, System Settings, SecurityAgent or LuLu"
TCCUTIL = "tccutil, which resets privacy grants"
TCC_DB = "a write to a TCC.db privacy database"
FDESETUP = "an fdesetup (FileVault) state change"
SPCTL = "an spctl (Gatekeeper) state change"
FIREWALL = "an application-firewall state change through socketfilterfw"
LULU = "stopping or reconfiguring LuLu"
QUARANTINE = "removing the quarantine attribute with xattr"
ANGER_LOCK = "changing the anger lock's state under ~/.local/state/anger-lock"
UNCLASSIFIED = "nested shell code naming a security control, too deep to classify"

_STILL_ALLOWED = {
    APPLESCRIPT: "Looking stays available: AppleScript passed with -e or in a heredoc whose every statement is a read "
                 "or plain control flow (get, count, exists, properties of processes and their windows, tell blocks, "
                 "set VARIABLE to, if/repeat/try, return, log, delay, activate). Clicking, typing, setting a property, "
                 "JXA, SecurityAgent and any form the guard does not recognize are denied. AppleScript addressed to "
                 "other applications is unaffected, as are the helper scripts in ~/repos/dotfiles/bin that wrap UI "
                 "automation (drive-errors, lulu-directory-rules, opentopage, dictation-language).",
    TCCUTIL: "Reading privacy grants stays available: the security-audit skill's scripts/tcc-grants.py.",
    TCC_DB: "Read-only access stays available: `sqlite3 -readonly`, a `file:...?mode=ro` URI, or the security-audit "
            "skill's scripts/tcc-grants.py.",
    FDESETUP: "Its read-only verbs stay available: status, list, isactive, haspersonalrecoverykey, "
              "hasinstitutionalrecoverykey, supportsauthrestart.",
    SPCTL: "`spctl --status` and `spctl --assess` stay available.",
    FIREWALL: "Its `--get*` and `--listapps` reads stay available.",
    LULU: "Reading LuLu's state stays available (pgrep, `defaults read`, the security-audit skill's "
          "scripts/lulu-rules.py), as do `open -a LuLu` and bin/lulu-directory-rules.",
    QUARANTINE: "`xattr -l` and `xattr -p` stay available.",
    ANGER_LOCK: "Reading it stays available (`cat ~/.local/state/anger-lock/locked-until`, the log).",
    UNCLASSIFIED: "Run the inner command directly so the guard can classify it.",
}


def reason(label: str, detail: str = "") -> str:
    """The deny reason for LABEL; DETAIL says why an AppleScript did not pass as a read."""
    headline = f"{label} ({detail})" if detail else label
    if label == ANGER_LOCK:
        return (f"BLOCKED: {headline}.\n\nThe anger lock is Pablo's commitment device: only its hook sets or clears "
                "it, and no agent may lift, shorten or work around it, nor hand Pablo a command to do so. If the lock "
                "seems to misfire, tell Pablo; the duration and classifier live in lib/python/anger_lock.py.\n\n"
                + _STILL_ALLOWED[label])
    return (f"BLOCKED: {headline}.\n\nThis class of command changes the Mac's security controls, and agents may not run "
            "it; there is no override. If the task truly needs it, hand Pablo the exact command, staged with "
            "~/repos/dotfiles/claude/bin/kill-ring-put, for him to run himself.\n\n" + _STILL_ALLOWED[label])


class _Denied(Exception):
    def __init__(self, label: str, detail: str = ""):
        super().__init__(label)
        self.label = label
        self.detail = detail


# Every name a rule can fire on; a command without one is not parsed at all.
_RELEVANT = re.compile(r"osascript|tccutil|TCC\.db|fdesetup|spctl|socketfilterfw|lulu|objective-see|xattr|anger-lock",
                       re.I)
_OSA_TARGET = re.compile(r"System\s*Events|System\s*Settings|System\s*Preferences|SecurityAgent"
                         r"|com\.apple\.systemevents|com\.apple\.systempreferences"
                         r"|(?<![A-Za-z0-9_-])lulu(?![A-Za-z0-9_-])|objective-see", re.I)
# `lulu-directory-rules` and `lulu-rules.py` are helpers, not the firewall.
_LULU_NAME = re.compile(r"(?<![A-Za-z0-9_-])lulu(?![A-Za-z0-9_-])|objective-see", re.I)
_LULU_DIR = re.compile(r"Objective-See/LuLu(?![A-Za-z0-9_-])", re.I)
_ANGER_LOCK_DIR = re.compile(r"\.local/state/anger-lock(?![A-Za-z0-9_.-])", re.I)
_TCC = re.compile(r"(?<![A-Za-z0-9_])TCC\.db", re.I)
_READ_ONLY_OPEN = re.compile(r"mode=ro\b|immutable=1\b|-readonly\b")
_SECURITY_AGENT = re.compile(r"SecurityAgent", re.I)
# Code an AppleScript loads from elsewhere: it can hold what the script itself does not show.
_SCRIPT_LOADER = re.compile(r"\b(run|load)\s+script\b|\bdo\s+shell\s+script\b|osascript", re.I)
_HEREDOC = re.compile(r"(?<!<)<<(?!<)-?\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1")
_VARIABLE = re.compile(r"\$\{?([A-Za-z_][A-Za-z0-9_]*)\}?")
_ASSIGNMENT = re.compile(r"([A-Za-z_][A-Za-z0-9_]*)=(.*)", re.S)
_BOUNDARY = re.compile(r"\|\||&&|[;|&\n()`{}]|\$\(")
_NUMBER = re.compile(r"\d+(\.\d+)?[smhd]?")
_IN_PLACE = re.compile(r"-[0-9A-Za-z]*i")
_INTERPRETER = re.compile(r"python[0-9.]*|perl|ruby|node|nodejs|deno|bun|php|lua|swift|osascript")

_SHELLS = {"sh", "bash", "zsh", "dash", "ksh", "fish"}
_DECLARATIONS = {"export", "local", "declare", "typeset", "readonly"}
# Wrappers the tokenizer's own list lacks, with their options that take a value.
_WRAPPER_VALUES = {
    "xargs": {"-I", "-n", "-P", "-L", "-s", "-E", "-d", "-R", "-J"},
    "caffeinate": {"-t", "-w"},
    "arch": {"-arch", "-e", "-d"},
    "doas": {"-u", "-C"},
    "watch": {"-n", "-d"},
    "stdbuf": {"-i", "-o", "-e"},
}
_FDESETUP_READS = {"status", "list", "isactive", "haspersonalrecoverykey", "hasinstitutionalrecoverykey",
                   "supportsauthrestart", "usingrecoverykey", "validaterecovery", "showdeferralinfo", "help",
                   "version"}
_SPCTL_READS = {"--status", "--assess", "--help", "-h"}
_SPCTL_CHANGES = ("--master-", "--global-", "--disable", "--enable", "--add", "--remove", "--reset-default")
_FIREWALL_READS = {"--listapps", "--help", "-h"}
_LAUNCHCTL_READS = {"list", "print", "print-cache", "print-disabled", "blame", "procinfo", "hostinfo", "dumpstate",
                    "managername", "managerpid", "manageruid", "error", "help", "version"}
_DEFAULTS_WRITES = {"write", "delete", "import", "rename"}
# Programs that change every path they are given, and those that write only the last one.
_MUTATORS = {"rm", "trash", "unlink", "srm", "shred", "mv", "tee", "touch", "truncate", "chmod", "chown", "chgrp",
             "chflags", "mkdir", "rmdir"}
_WRITE_LAST = {"cp", "gcp", "ditto", "rsync", "install", "ln", "scp"}


def _plain(text: str) -> str:
    """TEXT without quotes and backslashes: `tcc'util'` names the tool too."""
    return re.sub(r"""['"\\]""", "", text)


def _path_label(text: str) -> str:
    if _TCC.search(text):
        return TCC_DB
    if _ANGER_LOCK_DIR.search(text):
        return ANGER_LOCK
    return LULU if _LULU_DIR.search(text) else ""


def _without_comments(tokens):
    skipping = False
    for token in tokens:
        if isinstance(token, str) and token == "\n":
            skipping = False
        if not isinstance(token, str) and token.raw.startswith("#"):
            skipping = True
        if not skipping:
            yield token


def _redirect_targets(text: str) -> list[str]:
    """The targets of TEXT's output redirections, quotes removed. The
    tokenizer cuts a redirect target at its first blank, quoted or not."""
    targets: list[str] = []
    i, n, quote = 0, len(text), ""
    while i < n:
        c = text[i]
        if quote:
            if c == "\\" and quote == '"':
                i += 2
                continue
            if c == quote:
                quote = ""
            i += 1
        elif c == "\\":
            i += 2
        elif c in "'\"":
            quote = c
            i += 1
        elif c == ">":
            i += 1
            while i < n and text[i] in ">|&":
                i += 1
            while i < n and text[i] in " \t":
                i += 1
            word: list[str] = []
            while i < n and text[i] not in " \t\n;&|<>()":
                d = text[i]
                if d == "\\" and i + 1 < n:
                    word.append(text[i + 1])
                    i += 2
                elif d in "'\"":
                    end = text.find(d, i + 1)
                    end = n if end < 0 else end
                    word.append(text[i + 1:end])
                    i = end + 1
                else:
                    word.append(d)
                    i += 1
            targets.append("".join(word))
        else:
            i += 1
    return targets


def _osascript_heredocs(source: str) -> list[tuple[str, bool, str]]:
    """(delimiter, quoted, body) of each heredoc on a line that runs osascript."""
    lines = source.split("\n")
    found, index = [], 0
    while index < len(lines):
        opener = _HEREDOC.search(lines[index])
        if opener is None:
            index += 1
            continue
        strip = opener.group(0).startswith("<<-")
        end = next((later for later in range(index + 1, len(lines))
                    if (lines[later].lstrip("\t") if strip else lines[later]) == opener.group(2)), None)
        if end is None:
            break
        if "osascript" in lines[index]:
            found.append((opener.group(2), bool(opener.group(1)), "\n".join(lines[index + 1:end])))
        index = end + 1
    return found


def _skip_options(words: list, values: set[str]) -> list:
    i = 0
    while i < len(words):
        text = words[i].text
        if text in values:
            i += 2
        elif text.startswith("-") or _NUMBER.fullmatch(text):
            i += 1
        else:
            break
    return words[i:]


def _find_exec(args: list):
    """The commands a `find` runs: the words after each -exec up to `;` or `+`."""
    i = 0
    while i < len(args):
        if args[i].text in ("-exec", "-execdir", "-ok", "-okdir"):
            j = i + 1
            while j < len(args) and args[j].text not in (";", "+"):
                j += 1
            yield args[i + 1:j]
            i = j
        i += 1


def _first_operand(texts: list[str]) -> str | None:
    return next((text for text in texts if not text.startswith("-")), None)


def _short_options(texts: list[str]) -> str:
    return "".join(text[1:] for text in texts if text.startswith("-") and not text.startswith("--"))


def _path_write(base: str, texts: list[str]) -> str:
    """The label of a protected path BASE would write among its arguments TEXTS, or ""."""
    labels = [_path_label(text) for text in texts]
    first = next((label for label in labels if label), "")
    if not first:
        return ""
    if base in _MUTATORS:
        return first
    if base in _WRITE_LAST:
        operands = [index for index, text in enumerate(texts) if not text.startswith("-")]
        return labels[operands[-1]] if operands else ""
    if base == "dd":
        return next((label for text, label in zip(texts, labels) if label and text.startswith("of=")), "")
    if base in ("sed", "gsed", "perl", "ruby"):
        options = [text for text in texts if text.startswith("-") and not text.startswith("--")]
        return first if any(_IN_PLACE.match(option) for option in options) or "--in-place" in texts else ""
    if base == "find":
        return first if "-delete" in texts else ""
    if base == "plutil":
        return next((label for label in map(_path_label, _plutil_targets("plutil " + shlex.join(texts))) if label), "")
    if base == "plistbuddy":
        commands = [texts[index + 1] for index, text in enumerate(texts[:-1]) if text == "-c"]
        read_only = commands and all(re.match(r"\s*(print|help)\b", command, re.I) for command in commands)
        return "" if read_only else first
    if base == "sqlite3":
        if "-readonly" in texts or "--readonly" in texts:
            return ""
        return next((label for text, label in zip(texts, labels)
                     if label and not (text.startswith("file:") and _READ_ONLY_OPEN.search(text))), "")
    if _INTERPRETER.fullmatch(base):
        for index, text in enumerate(texts[:-1]):
            program = texts[index + 1]
            if text in ("-c", "-e", "-E", "--eval") and _TCC.search(program) and not _READ_ONLY_OPEN.search(program):
                return TCC_DB
    return ""


class _Scan:
    """One command's classification. FULL is its whole text without commit
    messages: an osascript in it can be fed any of that, including a script
    file that a heredoc in the same command writes first."""

    def __init__(self, full: str):
        self.tokens = classifiers.load("op-policy")
        self.full = full

    def classify(self, source: str, depth: int = 0) -> None:
        """Raise _Denied if SOURCE, as shell code, runs a denied command."""
        if not _RELEVANT.search(_plain(source.replace("\\\n", ""))):
            return
        if depth > 6:
            raise _Denied(UNCLASSIFIED)
        tokens = self.tokens
        # Heredoc bodies fed to data sinks are data; those fed to another
        # interpreter are its program, kept out of the shell scan; the rest
        # (a shell's, an unknown program's) are scanned as shell code.
        data_free = mask_heredoc_bodies(source)
        shell = mask_heredoc_bodies(source, "nonshell")
        programs = "\n".join((Counter(data_free.split("\n")) - Counter(shell.split("\n"))).elements())
        if _TCC.search(programs) and not _READ_ONLY_OPEN.search(programs):
            raise _Denied(TCC_DB)
        shell = shell.replace("\\\n", "")
        heredocs = _osascript_heredocs(source)
        for target in _redirect_targets(shell):
            self._deny_path(target)
        try:
            lifted, captures, _ = tokens._lift_substitutions(shell)
            pipelines = tokens._split_simple(list(_without_comments(tokens._tokenize(lifted))))
        except tokens.Deny:
            self._fallback(shell, depth, heredocs)
            return
        for capture in captures:
            self.classify(capture.inner, depth + 1)

        def expanded(text: str) -> str:
            for capture in captures:
                text = text.replace(capture.placeholder, " " + capture.inner + " ")
            return text

        def texts_of(simple) -> list[str]:
            texts = [word.text for word in simple.words]
            if simple.here_string is not None:
                texts.append(simple.here_string.text)
            return texts

        level = self.full + "\n" + expanded(" ".join(text for pipeline in pipelines for simple in pipeline
                                                     for text in texts_of(simple)))
        values: dict[str, str] = {}

        def resolved(text: str) -> str:
            return _VARIABLE.sub(lambda match: values.get(match.group(1), match.group(0)), text) if values else text

        for pipeline in pipelines:
            stages = []
            for simple in pipeline:
                for name, value in simple.assignments:
                    values[name] = expanded(value.text)
                words = [tokens.Word(resolved(word.text), word.quoted, word.expands, word.raw) if word.expands else word
                         for word in simple.words]
                if words and words[0].text in _DECLARATIONS:
                    for word in words[1:]:
                        match = _ASSIGNMENT.fullmatch(word.text)
                        if match:
                            values[match.group(1)] = expanded(match.group(2))
                stages.append(words)
            context = expanded(" ".join(resolved(text) for simple in pipeline for text in texts_of(simple)))
            for index, (simple, words) in enumerate(zip(pipeline, stages)):
                for redirect in simple.redirects:
                    if ">" in redirect.op:
                        self._deny_path(_plain(resolved(redirect.target)))
                here = None if simple.here_string is None else _plain(resolved(simple.here_string.text))
                command = self._check(words, here, context, level, depth, simple.redirects, heredocs)
                if index and command in _SHELLS:
                    # A shell reading the pipe runs what the earlier stages print.
                    for earlier in stages[:index]:
                        printed = [word.text for word in earlier[1:]]
                        for code in [" ".join(printed), *printed]:
                            self.classify(code, depth + 1)

    def _fallback(self, shell: str, depth: int, heredocs=()) -> None:
        """The tokenizer refuses some shapes (an unclosed quote, a quoted
        redirect target holding a blank, an AppleScript heredoc left in the
        text because its command is piped on). Judge the command positions
        that a split on shell operators still shows, each with the whole text
        as its context. Words are split on blanks here, so an osascript's -e
        arguments are not its script; its heredoc still is."""
        text = _plain(shell)
        for segment in _BOUNDARY.split(text):
            tokens = segment.split()
            words = [self.tokens.Word(token, False, False, token) for token in tokens
                     if not re.match(r"\d*[<>]", token)]
            redirects = [self.tokens.Redirect("0", "<<", match.group(1)) for match in
                         (re.fullmatch(r"<<-?([A-Za-z_][A-Za-z0-9_]*)", token) for token in tokens) if match]
            if words:
                self._check(words, None, text, self.full + "\n" + text, depth, redirects, heredocs, whole=False)

    def _deny_path(self, text: str) -> None:
        label = _path_label(text)
        if label:
            raise _Denied(label)

    def _unwrapped(self, words: list):
        """(program, argument words) once wrappers are stripped, or None."""
        tokens = self.tokens
        while words:
            if tokens._basename(words[0].text) == "command" and len(words) > 1 and words[1].text in ("-v", "-V"):
                return None  # a lookup, not a run
            stripped = tokens._strip_wrappers(words)
            if len(stripped) != len(words):
                words = stripped
                continue
            base = tokens._basename(words[0].text).lower()
            if base in _WRAPPER_VALUES:
                words = _skip_options(words[1:], _WRAPPER_VALUES[base])
                continue
            return base, words[1:]
        return None

    def _osascript(self, args: list, redirects, heredocs, level: str, whole: bool = True) -> None:
        """Rule 1. The script is visible when it is given with -e or in a
        heredoc and nothing in it is left for the shell to expand; then it
        alone is judged. Otherwise (a file, a pipe, a here-string) the whole
        command stands in for it, and naming a target is enough to deny."""
        parts: list[str] | None = []
        for index, word in enumerate(args[:-1]):
            if word.text == "-e":
                script = args[index + 1]
                if not whole or (script.expands and re.search(r"[$`\x00]", script.text)):
                    parts = None
                    break
                parts.append(script.text)
        for redirect in redirects or ():
            if parts is None or redirect.op != "<<":
                continue
            delimiter = redirect.target.lstrip("-").strip("'\"")
            bodies = [(quoted, body) for name, quoted, body in heredocs if name == delimiter]
            if len(bodies) != 1 or (not bodies[0][0] and re.search(r"[$`\\]", bodies[0][1])):
                parts = None
            else:
                parts.append(bodies[0][1])
        if not parts:
            if _OSA_TARGET.search(level):
                raise _Denied(APPLESCRIPT, "the guard cannot see the script; pass it with -e or in a quoted heredoc")
            return
        script = "\n".join(parts)
        if not _OSA_TARGET.search(script):
            if _SCRIPT_LOADER.search(script) and _OSA_TARGET.search(level):
                raise _Denied(APPLESCRIPT, "the script loads code the guard cannot see")
            return
        if _SECURITY_AGENT.search(script):
            raise _Denied(APPLESCRIPT, "SecurityAgent is closed, reads included")
        if any(word.text.lower() in ("javascript", "-ljavascript") for word in args):
            raise _Denied(APPLESCRIPT, "JXA naming these targets is denied; write the read in AppleScript")
        problem = applescript_reads.problem(script)
        if problem:
            raise _Denied(APPLESCRIPT, f"not recognized as a read: {problem}")

    def _check(self, words: list, here: str | None, context: str, level: str, depth: int,
               redirects=None, heredocs=(), whole: bool = True) -> str | None:
        """Judge one simple command; return its program name. CONTEXT is its
        pipeline's text and LEVEL the text an osascript in it could be fed."""
        command = self._unwrapped(words)
        if command is None:
            return None
        base, args = command
        texts = [word.text for word in args]
        if base in _SHELLS or base == "eval":
            for code in [" ".join(texts), *texts]:
                self.classify(code, depth + 1)
            if here is not None:
                self.classify(here, depth + 1)
        if base == "find":
            for inner in _find_exec(args):
                self._check(inner, None, context, level, depth)
        if base == "osascript":
            self._osascript(args, redirects, heredocs, level, whole)
        if base == "tccutil":
            raise _Denied(TCCUTIL)
        if base == "fdesetup":
            verb = _first_operand(texts)
            if verb is not None and verb.lower() not in _FDESETUP_READS:
                raise _Denied(FDESETUP)
        if base == "spctl" and texts:
            reads = any(text in _SPCTL_READS or (re.fullmatch(r"-[A-Za-z]+", text) and "a" in text) for text in texts)
            if not reads or any(text.startswith(_SPCTL_CHANGES) for text in texts):
                raise _Denied(SPCTL)
        if base == "socketfilterfw":
            if not all(text.startswith("--get") or text in _FIREWALL_READS for text in texts if text.startswith("-")):
                raise _Denied(FIREWALL)
        if base == "xattr":
            options = _short_options(texts)
            if "c" in options or ("d" in options and any("com.apple.quarantine" in text.lower() for text in texts)):
                raise _Denied(QUARANTINE)
        if base in ("kill", "pkill", "killall") and _LULU_NAME.search(context):
            raise _Denied(LULU)
        if base == "launchctl" and _LULU_NAME.search(" ".join(texts)):
            if (_first_operand(texts) or "").lower() not in _LAUNCHCTL_READS:
                raise _Denied(LULU)
        if base == "systemextensionsctl" and _LULU_NAME.search(" ".join(texts)):
            if (_first_operand(texts) or "").lower() != "list":
                raise _Denied(LULU)
        if base == "defaults":
            rest = _skip_options(args, {"-host"})
            operands = [word.text for word in rest]
            if operands and operands[0] in _DEFAULTS_WRITES:
                domain = operands[2:3] if operands[1:2] == ["-app"] else operands[1:2]
                if domain and _LULU_NAME.search(domain[0]):
                    raise _Denied(LULU)
        label = _path_write(base, texts)
        if label:
            raise _Denied(label)
        return base


def denial(command: str) -> str:
    """The deny reason for the shell COMMAND, or "" to allow it."""
    if not _RELEVANT.search(_plain(command.replace("\\\n", ""))):
        return ""
    command = mask_git_commit_messages(command)
    try:
        _Scan(command).classify(command)
    except _Denied as denied:
        return reason(denied.label, denied.detail)
    return ""


def _sources(call: Call, command: str) -> list[str]:
    """The shell texts COMMAND holds: itself; its words rejoined when it is an
    argv array (`["bash", "-lc", "..."]`); and for a Codex functions.exec
    program the literal commands of its nested exec_command calls (the
    secret-leak policy denies a program whose calls cannot be extracted)."""
    sources = [command]
    if command.lstrip().startswith("["):
        try:
            argv = json.loads(command)
        except ValueError:
            argv = None
        if isinstance(argv, list) and argv and all(isinstance(word, str) for word in argv):
            sources.append(shlex.join(argv))
    if call.runtime == CODEX and call.tool_name == "functions.exec":
        try:
            nested = nested_exec_commands(command)
        except PartialOutput as partial:
            nested = partial.values
        sources.extend(value for value in nested if isinstance(value, str))
    return sources


def _edited_paths(call: Call) -> list[str]:
    if call.runtime == CODEX:
        return list(codex_changed_paths(call.raw))
    path = call.tool_input.get("file_path") or call.tool_input.get("notebook_path")
    return [path] if isinstance(path, str) else []


def decide(call: Call) -> Result:
    if not call.is_shell:
        for path in _edited_paths(call):
            if _ANGER_LOCK_DIR.search(path):
                return Result.deny(reason(ANGER_LOCK))
        return Result.allow()
    command = call.shell_command()
    if not command:
        return Result.allow()
    for source in _sources(call, command):
        denied = denial(source)
        if denied:
            return Result.deny(denied)
    return Result.allow()
