"""GitHub writes outside the allowlist: block-github-write-command.sh (both runtimes).

A hard gate for the incident class where an agent pushes, opens PRs, sets
secrets or otherwise mutates a repository after inferring permission from
context. Read-only inspection stays allowed. A write needs one unambiguous
target repository that matches agents/github-write-allowlist.txt (exact
OWNER/REPO or OWNER/*), a repo declared in the committed automations
registry, or a committed scoped grant (bin/github-operation-authorization).
Both lists are read from committed blobs, never the working tree, so a
blocked agent cannot add its own target and retry (2026-08-31).

The Codex copy differs in three ways, kept here: it classifies each literal
nested exec_command of a functions.exec program by re-running itself in that
call's workdir, its ambiguous-target advice is worded differently, and it
fails closed (the Claude copy had no EXIT trap; it only ever ran delegated
from pretooluse-bash.sh, which denies any failure).

Patterns are the hook's: `grep -E` ones go through `agent_guard.ere` (one line
at a time, BSD grep's classes), `[[ =~ ]]` ones through `_bash_ere` (the whole command, where
`[[:space:]]` includes the newline and `$` is the end of the string).
"""

from __future__ import annotations

import contextlib
import functools
import json
import locale
import os
import re
import shlex
import subprocess
from dataclasses import dataclass

from .. import classifiers, codex_payload, ere, repo
from ..codex_payload import bash_pwd, command_substitution, jq_raw, jq_text
from ..core import CLAUDE, CODEX, CODEX_SHELL_TOOLS, DOTFILES, Call, HookFailure, Result
from ..shell_text import BASH_SPACE, mask_git_commit_messages, mask_heredoc_bodies
from .sensitive_read import tool_name

_GRANT_ADVICE = (
    "\n\nAfter explicit user authorization, supported fork/PR commands may use committed scoped grants "
    "(agents/github-operation-authorizations.md). Otherwise GitHub writes require a target that matches an exact "
    "OWNER/REPO entry or OWNER/* account wildcard in the committed `~/My Drive/dotfiles/agents/github-write-allowlist.txt` "
    "(working-tree edits do not count), or is declared by an Epoch project via :REPOS: and committed to the "
    "automations registry.")
_AMBIGUOUS_ADVICE = {
    CLAUDE: "The guard blocks ambiguous GitHub writes. Make the target repo explicit and add it to the allowlist only "
            "if Pablo personally created it.",
    CODEX: "The guard blocks ambiguous GitHub writes. Make the target repo explicit; use the allowlist only after "
           "Pablo explicitly authorizes agent writes to it.",
}
_NOT_ALLOWLISTED_ADVICE = (
    "Do not infer write permission from org membership, affected-repo context, maintainer requests, or a general "
    "\"proceed\". If this is one of Pablo's own repos, declare it in the owning project's :REPOS: drawer property, "
    "re-run 'make import' in automations-dashboard, and commit the registry -- declarations are read from the "
    "committed registry, so an uncommitted one has no effect.")

# Agents publish to epoch-research through the Writer app, never with Pablo's
# login: GitHub notifies the pusher of every failed push-triggered run
# (ai-enablement docs/superpowers/plans/2026-10-10-agent-push-attribution.md).
# On since 2026-10-10, after the alert coverage canary caught an app-pushed failure.
EPOCH_PUSH_ENFORCED = True
_EPOCH_OWNER = "epoch-research/"
# Declared repos the Writer app is not installed on, where an agent may push a
# non-default branch itself. Refresh with `epoch_publish.py --writer-coverage`.
_WRITER_MISSING_PATH = "agents/epoch-writer-missing.txt"
_EPOCH_PUSH_ADVICE = (
    "Agents publish to epoch-research only as the Epoch Automations Writer app, so that failed push-triggered "
    "runs do not notify Pablo. Commit on the branch to publish (the default branch, or a topic branch for a pull "
    "request), then run `$EPOCH_SHARED_DIR/scripts/epoch_publish.py <checkout>` and, for a topic branch, "
    "`gh pr create --head BRANCH`. Only in a repo without the Writer installation (agents/epoch-writer-missing.txt) "
    "may an agent `git push` a named non-default branch itself; default branches, force pushes, --all, --mirror, "
    "deletions and tags are never agent pushes. Do not route the push through another path.")
# git push options that take a separate value, and ones that are never agent pushes.
_PUSH_VALUE_OPTIONS = {"-o", "--push-option", "--repo", "--receive-pack", "--exec"}
_PUSH_REFUSED_OPTIONS = {"-f", "--force", "--all", "--branches", "--mirror", "-d", "--delete", "--tags", "--prune",
                         "--force-if-includes"}

_ALLOWLIST_PATH = "agents/github-write-allowlist.txt"
_DECLARED_REPOS_PATH = "data/automations.json"
_REPO_CHARS = "A-Za-z0-9_.-"


def _bash_ere(pattern: str) -> re.Pattern:
    """A `[[ =~ ]]` pattern: `[:space:]` with the newline, `.` matching it too.
    Write `$` as `\\Z`."""
    return re.compile(pattern.replace("[:space:]", re.escape(BASH_SPACE)), re.DOTALL)


# `sh -c '…'`: drop the quote so the body is scanned as the command it is.
# Rules are the hook's `sed -E` expressions after shell double-quote removal.
_SHELL_C_QUOTE = (r"""((^|[[:space:];|&(])(bash|sh|zsh|dash|ksh)[[:space:]]+-[a-zA-Z]*c[a-zA-Z]*[[:space:]]+)['"]""",
                  r"\1", "g")
_URLISH = re.compile(rf"github\.com[:/]([{_REPO_CHARS}]+/[{_REPO_CHARS}]+)(\.git)?")
_REPO_FLAG = _bash_ere(rf"(^|[[:space:]])(--repo|-R)(=|[[:space:]]+)([{_REPO_CHARS}]+/[{_REPO_CHARS}]+)")
_GH_REPO_QUOTES = (r"""GH_REPO=['"]([^'"]+)['"]""", r"GH_REPO=\1", "g")
_GH_REPO_ENV = _bash_ere(rf"(^|[[:space:];|&])GH_REPO=([{_REPO_CHARS}]+/[{_REPO_CHARS}]+)(\Z|[[:space:]])")
_REPO_POSITIONAL = _bash_ere(r"(^|[[:space:]])gh[[:space:]]+repo[[:space:]]+"
                             r"(create|delete|edit|rename|archive|unarchive|sync)[[:space:]]+"
                             rf"([{_REPO_CHARS}]+/[{_REPO_CHARS}]+)")
_API_ENDPOINT = _bash_ere(rf"(^|[^{_REPO_CHARS}])/?repos/([{_REPO_CHARS}]+)/([{_REPO_CHARS}]+)(\Z|[^{_REPO_CHARS}])")
_DIRECTORY_CHANGE = ere.ere(r"(^|[[:space:];|&(])(cd|pushd)[[:space:]]")
_DASH_C = _bash_ere(r"(^|[[:space:];|&])([^[:space:];|&]*/)?git[[:space:]]+-C[[:space:]]+([^[:space:];|&]+)")
_CD_REST = _bash_ere(r"(^|[[:space:];|&])(cd|pushd)[[:space:]]+(.*)")
_GIT_PUSH = ere.ere(
    r"""(^|[[:space:];|&])("[^"]*/git"|'[^']*/git'|[^[:space:];|&]*/git|git)"""
    r"([[:space:]]+(-C|-c|--git-dir|--work-tree|--namespace)(=|[[:space:]]+)[^[:space:];|&]+|[[:space:]]+-[pP]"
    r"|[[:space:]]+--(paginate|no-pager|bare|no-replace-objects|literal-pathspecs|glob-pathspecs|noglob-pathspecs"
    r"|icase-pathspecs))*[[:space:]]+push\b")
_DRY_RUN = ere.ere(r"(^|[[:space:]])--dry-run([[:space:]]|$)")
_GH = r"(^|[[:space:];|&])gh[[:space:]]+"
_GH_PR = ere.ere(_GH + r"pr[[:space:]]+(close|comment|create|edit|reopen|merge|revert|review|ready|lock|unlock"
                   r"|update-branch)\b")
_GH_ISSUE = ere.ere(_GH + r"issue[[:space:]]+(close|comment|create|reopen|edit|lock|unlock|transfer|delete|pin|unpin"
                      r"|develop)\b")
_GH_SECRET = ere.ere(_GH + r"(secret|variable)[[:space:]]+(set|delete|remove)\b")
_NOT_REPO_SCOPED = ere.ere(r"(^|[[:space:]])--(org|env|app)(=|[[:space:]]+)")
_GH_WORKFLOW = ere.ere(_GH + r"workflow[[:space:]]+(run|enable|disable)\b")
_GH_RUN = ere.ere(_GH + r"run[[:space:]]+(cancel|delete|rerun)\b")
_GH_RELEASE = ere.ere(_GH + r"release[[:space:]]+(create|delete|delete-asset|edit|upload)\b")
_GH_REPO = ere.ere(_GH + r"repo[[:space:]]+(create|delete|edit|rename|archive|unarchive|sync)\b")
_GH_REPO_CREATE = ere.ere(_GH + r"repo[[:space:]]+create\b")
_GH_LABEL = ere.ere(_GH + r"(label|milestone)[[:space:]]+(create|delete|edit)\b")
_GH_GIST = ere.ere(_GH + r"gist[[:space:]]+(create|delete|edit|rename)\b")
_GH_REPOSITORY = ere.ere(_GH + r"(cache[[:space:]]+delete|discussion[[:space:]]+(comment|create|edit)|repo[[:space:]]+"
                           r"(autolink[[:space:]]+(create|delete)|deploy-key[[:space:]]+(add|delete)))\b")
_GH_REPOSITORY_MORE = ere.ere(_GH + r"(agent-task[[:space:]]+create|label[[:space:]]+clone)\b")
_GH_UNSCOPED = ere.ere(_GH + r"(codespace[[:space:]]+(create|delete|edit|rebuild|stop)|gpg-key[[:space:]]+(add|delete)"
                         r"|ssh-key[[:space:]]+(add|delete)|project[[:space:]]+(close|copy|create|delete|edit"
                         r"|field-create|field-delete|item-add|item-archive|item-create|item-delete|item-edit|link"
                         r"|mark-template|unlink)|repo[[:space:]]+fork)\b")
_GH_API = ere.ere(_GH + r"api\b")
_WRITE_METHOD = ere.ere(r"(^|[[:space:]])(--method|-X)(=|[[:space:]]+)(POST|PUT|PATCH|DELETE)\b")
_WRITE_WORD = ere.ere(r"(^|[[:space:]])(DELETE|PATCH|POST|PUT)\b")
_GRAPHQL = ere.ere(r"(^|[[:space:]])graphql([[:space:]]|$)")
_MUTATION = ere.ere(r"\bmutation\b")
_FIELD = ere.ere(r"(^|[[:space:]])(-f|-F|--field|--raw-field|--input)(=|[[:space:]]+)")
_GET_METHOD = ere.ere(r"(^|[[:space:]])(--method|-X)(=|[[:space:]]+)GET\b")

# compound_shell_command_p: a write piped only into these readers keeps one target.
_OUTPUT_FILTERS = {"tail", "head", "grep", "wc", "cat"}
# broker_secret_pipeline_repo
# Matched against one shlex token, so a quoted item name may contain spaces.
_REF = re.compile(r"op://.+")
_ACCOUNTS = {"@epoch", "@personal", "@tlon"}
_NO_NEWLINE = {"--no-newline", "-n"}
_PIPELINE_REPO = re.compile(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+")


class _Exit(Exception):
    """The hook's `exit 0`, carrying its decision."""

    def __init__(self, result: Result):
        super().__init__(result.reason)
        self.result = result


def _lower(text: str) -> str:
    """`tr '[:upper:]' '[:lower:]'` and awk's tolower: one character for one."""
    return "".join(lowered if len(lowered := char.lower()) == 1 else char for char in text)


def normalize_repo(value: str) -> str:
    for prefix in ("https://github.com/", "http://github.com/", "ssh://git@github.com/", "git@github.com:"):
        value = value.removeprefix(prefix)
    return _lower(value.removesuffix(".git").removesuffix("/"))


def repo_from_urlish(text: str) -> str:
    match = _URLISH.search(text)
    return normalize_repo(match.group(1)) if match else ""


def compound_shell_command(source: str) -> bool:
    """`compound_shell_command_p`: an unquoted `;`, `&`, `|` or newline outside
    a 2>&1 redirect, except one pipe into plain output filters."""
    def output_filters(rest):
        if any(char in rest for char in ";&<>$`\\\n(){}"):
            return False
        for segment in rest.split("|"):
            try:
                words = shlex.split(segment)
            except ValueError:
                return False
            if not words or words[0] not in _OUTPUT_FILTERS:
                return False
        return True

    quote = None
    escaped = False
    for index, char in enumerate(source):
        if escaped:
            escaped = False
            continue
        if char == "\\":
            escaped = True
            continue
        if quote is not None:
            if char == quote:
                quote = None
            continue
        if char in ("\"", "'", "`"):
            quote = char
            continue
        if char == "&" and source[index - 1:index] == ">":
            continue
        if char == "|" and source[index + 1:index + 2] not in ("|", "&"):
            head = source[:index].rstrip()
            if head.endswith("2>&1"):
                head = head[:-4]
            return not ("&" not in head and output_filters(source[index + 1:]))
        if char in (";", "|", "&", "\n"):
            return True
    return False


def broker_secret_pipeline_repo(source: str) -> str:
    """The lowercase OWNER/REPO of exactly `BROKER read REF | gh secret set
    NAME -R OWNER/REPO`, the sanctioned pipeline of context/secrets.md, or ""."""
    if any(char in source for char in ";&<>()$`\\\n{}#*?[]~!") or source.count("|") != 1:
        return ""
    first, second = source.split("|")
    try:
        reader = shlex.split(first)
        writer = shlex.split(second)
    except ValueError:
        return ""

    def read_args_ok(rest, allow_account):
        while rest:
            if rest[0] in _NO_NEWLINE:
                rest = rest[1:]
            elif allow_account and rest[0] == "--account" and len(rest) >= 2:
                rest = rest[2:]
            elif allow_account and rest[0].startswith("--account="):
                rest = rest[1:]
            else:
                break
        return len(rest) == 1 and bool(_REF.fullmatch(rest[0]))

    if reader[:1] == ["op-automations"]:
        rest = reader[1:]
        if rest[:1] and rest[0] in _ACCOUNTS:
            rest = rest[1:]
        reader_ok = rest[:1] == ["read"] and read_args_ok(rest[1:], allow_account=False)
    elif reader[:1] == ["op-desktop"]:
        rest = reader[1:]
        reader_ok = rest[:1] == ["read"] and read_args_ok(rest[1:], allow_account=True)
    else:
        reader_ok = False
    if not reader_ok or writer[:3] != ["gh", "secret", "set"]:
        return ""
    names, repos, index, args = [], [], 0, writer[3:]
    while index < len(args):
        word = args[index]
        if word in ("-R", "--repo"):
            if index + 1 >= len(args):
                return ""
            repos.append(args[index + 1])
            index += 2
            continue
        if word.startswith("--repo="):
            repos.append(word[len("--repo="):])
        elif word.startswith("-"):
            return ""
        else:
            names.append(word)
        index += 1
    if len(names) != 1 or not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", names[0]):
        return ""
    if len(repos) != 1 or not _PIPELINE_REPO.fullmatch(repos[0]):
        return ""
    return repos[0].lower()


def _authorization_status(command: str) -> int:
    """The exit status of `github-operation-authorization check`: 0 for a
    committed scoped grant, 1 for none, 2 for an invalid grant. An exception
    its main() did not catch was a traceback, exit 1. In-process it reads
    GH_HOST from the engine's os.environ, the environment the subprocess got."""
    tool = classifiers.load("github-operation-authorization")
    try:
        return 0 if tool.authorized(command) else 1
    except (ValueError, TypeError, KeyError, OSError, subprocess.SubprocessError):
        return 2
    except Exception:  # noqa: BLE001 - an uncaught exception exited 1
        return 1


def _op_policy_allows(command: str) -> bool:
    """`lib-op-policy.py | grep -q '"decision": "allow"'`; a classifier crash
    exited nonzero, which pipefail turned into no match."""
    try:
        return classifiers.load("op-policy").decide(command).get("decision") == "allow"
    except Exception:  # noqa: BLE001 - any traceback was a nonzero exit
        return False


def _git_show(directory: str, spec: str, env: dict) -> bytes:
    return subprocess.run(["git", "-C", directory, "show", spec], env=env,
                          stdout=subprocess.PIPE, stderr=subprocess.DEVNULL).stdout


def _allowlist_patterns(env: dict) -> list[str]:
    """The awk pass over the committed allowlist: first field, comment and
    whitespace removed, `.git` dropped, lowercased; blank and comment lines skipped."""
    patterns = []
    for line in _git_show(str(DOTFILES), "HEAD:" + _ALLOWLIST_PATH, env).decode("utf-8", "surrogateescape").split("\n"):
        if re.match(f"[{re.escape(BASH_SPACE)}]*($|#)", line):
            continue
        fields = re.split(r"[ \t]+", line.strip(" \t"))
        entry = re.sub(r"#.*", "", fields[0], count=1)
        entry = re.sub(f"[{re.escape(BASH_SPACE)}]", "", entry)
        patterns.append(_lower(re.sub(r"\.git\Z", "", entry)))
    return patterns


def _writer_missing(env: dict) -> set[str]:
    text = _git_show(str(DOTFILES), "HEAD:" + _WRITER_MISSING_PATH, env).decode("utf-8", "surrogateescape")
    return {_lower(line.split("#", 1)[0].strip()) for line in text.split("\n") if line.split("#", 1)[0].strip()}


def push_destinations(command: str, current_branch) -> list[str] | None:
    """The branch names a plain `git ... push ...` command updates, or None when
    it is not one (unparsable, a force, --all, a tag or other non-branch ref).
    CURRENT_BRANCH() names the checked-out branch, or None."""
    try:
        words = shlex.split(command)
    except ValueError:
        return None
    if "push" not in words:
        return None
    words = words[words.index("push") + 1:]
    positionals, skip = [], False
    for word in words:
        if skip:
            skip = False
        elif word in _PUSH_VALUE_OPTIONS:
            skip = True
        elif word.split("=", 1)[0] in _PUSH_REFUSED_OPTIONS or word.startswith("--force-with-lease"):
            return None
        elif word.startswith("-"):
            if not word.startswith("--") and len(word) > 2 and set(word[1:]) & {"f", "d"}:
                return None
        else:
            positionals.append(word)
    refspecs = positionals[1:]
    if not refspecs:
        branch = current_branch()
        return [branch] if branch else None
    out = []
    for spec in refspecs:
        if spec.startswith("+") or spec.startswith(":"):
            return None
        dest = spec.split(":", 1)[-1]
        if dest == "HEAD":
            dest = current_branch()
            if not dest:
                return None
        if dest.startswith("refs/"):
            if not dest.startswith("refs/heads/"):
                return None
            dest = dest.removeprefix("refs/heads/")
        out.append(dest)
    return out


def _jq_documents(text: str) -> list:
    """The JSON texts jq reads from TEXT, up to the first parse error."""
    decoder = json.JSONDecoder()
    documents, position = [], 0
    while True:
        while position < len(text) and text[position] in " \t\n\r":
            position += 1
        if position >= len(text):
            return documents
        try:
            document, position = decoder.raw_decode(text, position)
        except ValueError:
            return documents
        documents.append(document)


def _jq_iterate(value) -> list:
    """`.[]?`: the values of an array or object, nothing for anything else."""
    if isinstance(value, list):
        return value
    if isinstance(value, dict):
        return list(value.values())
    return []


def _jq_field(value, key: str):
    """`.KEY` of an object or null; anything else is a jq error, which ends
    the output for that document."""
    if isinstance(value, dict):
        return value.get(key)
    if value is None:
        return None
    raise ValueError(key)


def _declared_repos(env: dict) -> list[str]:
    """`jq -r '.projects[]?.repos[]? // empty' | tr` over the committed registry."""
    directory = os.path.join(env["HOME"], "repos/epoch/automations-dashboard")
    if not os.path.isdir(directory):
        return []
    text = _git_show(directory, "HEAD:" + _DECLARED_REPOS_PATH, env).decode("utf-8", "replace")
    output = ""
    for document in _jq_documents(text):
        try:
            for project in _jq_iterate(_jq_field(document, "projects")):
                for value in _jq_iterate(_jq_field(project, "repos")):
                    if value is not None and value is not False:
                        output += jq_raw(value) + "\n"
        except ValueError:
            continue
    return _lower(output).split("\n")[:-1]


def _sorted_unique(lines: list[str]) -> list[str]:
    """`sort -u` under the process's LC_COLLATE; only identical lines merged."""
    return sorted(set(lines), key=lambda line: (functools.cmp_to_key(locale.strcoll)(line), line))


@contextlib.contextmanager
def _working_directory(path: str):
    previous = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(previous)


# --- compound commands (2026-10-05) ----------------------------------------------
#
# A compound command with a write used to be denied outright, which blocked the
# ordinary `cd ~/repos/x && git fetch -q && git push` and even text that only
# quoted a write (`printf '%s' '… git push' | kill-ring-put`). A compound command
# inside the small grammar of shell_segments is now judged one and-or list
# element at a time (_Guard.compound):
#
# - Each element that runs a write is judged alone by the whole hook, in the
#   directory it runs in, so `git -C`, `gh -R`, GH_REPO and URL targets stay
#   authoritative; all writes must resolve to one repository.
# - That directory starts as the command's own context and changes only at a
#   literal `cd DIR` element. Its scope is the rest of the `&&` chain: after
#   any other connector (`;`, newline, `||`, `&`), after a `cd` that cannot be
#   resolved, and after an element whose command word may change the
#   directory (anything outside _CWD_PRESERVING, such as zoxide's `z`), the
#   directory is unknown until the next resolvable `cd`, and a write there is
#   denied. A later `cd` replaces the binding.
# - An element whose every pipeline stage is a text-only command
#   (_TEXT_ONLY) with no file redirection runs no write, whatever it quotes.
#
# Anything the grammar does not model returns None and keeps the whole-command
# denial: command or process substitution outside single quotes, backticks,
# subshells and groups, heredocs, comments, `$'…'`, line continuations,
# unbalanced quotes, empty pipeline stages, a `cd` inside a pipeline, and the
# commands in _STATEFUL or an assignment-only element, which could redirect a
# later write through the environment or the directory stack.

@dataclass
class Segment:
    """One element of a top-level and-or list: its pipeline stages and the
    connector after it ("&&", "||", ";" for `;` or a newline, "&", or None)."""
    stages: list[str]
    connector: str | None

    @property
    def text(self) -> str:
        return " | ".join(self.stages)


_STATEFUL = {"pushd", "popd", "export", "declare", "typeset", "local", "readonly", "unset", "source", ".", "eval",
             "builtin", "command", "exec", "alias", "unalias", "shopt", "enable", "hash", "trap", "read", "mapfile",
             "readarray", "let", "function", "getopts", "setopt", "unsetopt", "emulate", "autoload", "zmodload",
             "chdir", "dirs", "set", "env"}
_TEXT_ONLY = {"printf", "echo", "pbcopy", "kill-ring-put"} | _OUTPUT_FILTERS
_CWD_PRESERVING = _TEXT_ONLY | {"git", "gh", "true", "false", "ls", "pwd", "test", "[", "sleep", "date", "make", "jq",
                                "sed", "awk", "rg", "find", "sort", "uniq", "diff", "tr", "cut", "tee", "mkdir", "touch"}
_ASSIGNMENT = re.compile(r"[A-Za-z_][A-Za-z0-9_]*\+?=")
# Redirections that write no file: fd duplication and /dev/null.
_HARMLESS_REDIRECT = re.compile(r"[0-9]*>&[0-9]+-?|&>/dev/null|[0-9]*>>?/dev/null")
_GLOB_OR_EXPANSION = set("$`*?[~")


def shell_segments(command: str) -> list[Segment] | None:
    """COMMAND split at its unquoted `&&`, `||`, `;`, `&` and newlines, each
    element at its pipes (`|`, `|&`); None outside the grammar described above."""
    segments: list[Segment] = []
    stages: list[str] = []
    current: list[str] = []
    quote = None
    index, end = 0, len(command)

    def close_stage() -> bool:
        stage = "".join(current).strip(BASH_SPACE)
        current.clear()
        if not stage:
            return False
        stages.append(stage)
        return True

    def close_segment(connector) -> bool:
        if not "".join(current).strip(BASH_SPACE):
            current.clear()
            if stages:
                return False  # `a |` followed by a connector
            # An empty element is a blank line or a trailing `;` or `&`.
            return not segments or segments[-1].connector in (";", "&")
        close_stage()
        segments.append(Segment(list(stages), connector))
        stages.clear()
        return True

    while index < end:
        char = command[index]
        following = command[index + 1:index + 2]
        if quote == "'":
            current.append(char)
            quote = None if char == "'" else quote
            index += 1
            continue
        if quote == '"':
            if char == "\\":
                current.append(command[index:index + 2])
                index += 2
                continue
            if char == "`" or (char == "$" and following == "("):
                return None
            current.append(char)
            quote = None if char == '"' else quote
            index += 1
            continue
        if char == "\\":
            if following in ("", "\n"):
                return None
            current.append(command[index:index + 2])
            index += 2
            continue
        if char in ("'", '"'):
            if char == "'" and current and current[-1] == "$":
                return None  # $'…'
            quote = char
            current.append(char)
        elif char == "{" and current and current[-1] == "$":
            close = command.find("}", index)
            if close < 0 or any(c in command[index:close] for c in "`'\"(\\$"):
                return None
            current.append(command[index:close + 1])
            index = close + 1
            continue
        elif char in "`(){}":
            return None
        elif char == "$" and following == "(":
            return None
        elif char in "<>" and (following == "(" or (char == "<" and following == "<")):
            return None  # process substitution, heredoc, here-string
        elif char == "#" and (not current or current[-1] in BASH_SPACE):
            return None
        elif char == "|":
            if following == "|":
                if not close_segment("||"):
                    return None
                index += 2
                continue
            if not close_stage():
                return None
            index += 2 if following == "&" else 1
            continue
        elif char == "&":
            if following == "&":
                if not close_segment("&&"):
                    return None
                index += 2
                continue
            if (current and current[-1] in "<>") or following == ">":
                current.append(char)  # 2>&1, >&2, &>file
            elif not close_segment("&"):
                return None
        elif char in ";\n":
            if char == ";" and following in (";", "&"):
                return None  # case-statement terminators
            if not close_segment(";"):
                return None
        else:
            current.append(char)
        index += 1
    if quote is not None or not close_segment(None):
        return None
    return segments


def _command_words(stage: str) -> list[str] | None:
    """STAGE's words from its command word on; None when it cannot be split or
    only assigns variables."""
    try:
        words = shlex.split(stage)
    except ValueError:
        return None
    while words and _ASSIGNMENT.match(words[0]):
        words = words[1:]
    return words or None


def _unquoted(stage: str) -> str:
    """STAGE with its quoted spans and escaped characters blanked."""
    return re.sub(r"""'[^']*'|"(\\.|[^"\\])*"|\\.""", " ", stage)


def _text_only(segment: Segment) -> bool:
    """Every stage is a text-only command that writes no file, so the element
    runs no write whatever its arguments quote. shell_segments already
    rejected substitutions, and no stage is an interpreter, because every
    stage is in _TEXT_ONLY."""
    for stage in segment.stages:
        words = _command_words(stage)
        if words is None or words[0] not in _TEXT_ONLY:
            return False
        if words[0] == "printf" and any(word.startswith("-v") for word in words[1:]):
            return False
        if re.search("[<>]", _HARMLESS_REDIRECT.sub(" ", _unquoted(stage))):
            return False
    return True


def _stateful(segment: Segment) -> bool:
    for stage in segment.stages:
        words = _command_words(stage)
        if words is None or words[0] in _STATEFUL or (words[0] == "cd" and len(segment.stages) > 1):
            return True
    return False


# --- hidden commands (2026-10-05) ---------------------------------------------------
#
# The write patterns need a separator before `git` or `gh`, so a write inside a
# substitution or a string run as code passed unchecked: `"$(git push)"`,
# `` `git push` ``, `eval "git push"`, `printf 'git push' | sh`; and
# `sh -c "cd x && git push"` missed its `cd`. _Hidden finds the code a command
# runs besides its own words:
#
# - the bodies of command substitutions, backticks and process substitutions
#   (outside single quotes, including those in unquoted-delimiter heredocs);
# - the string after `sh|bash|zsh|… -c`, and the arguments after it;
# - the arguments of eval, and the command xargs runs with its input appended;
# - the text a shell, `source /dev/stdin` or xargs reads: a here-string, a
#   heredoc, or the literal arguments of the earlier stages of its pipeline;
# - the literal output of a substitution used as code (`bash -c "$(printf …)"`).
#
# Each piece is judged as a whole command by the whole hook, in the directory
# the command starts in (_Guard.check_hidden). Mentions used as data (grep
# patterns, commit messages, `printf … | kill-ring-put`) are not code here.
# Text the parser cannot follow, `env -C`, `env -S` and `sudo -D` raise
# _Unparsed: then a write pattern anywhere in the command, with quotes and
# brackets ignored, denies. Not covered: scripts in files, other interpreters
# (python, node, perl), and code built from variables.

class _Unparsed(Exception):
    """Shell text _Hidden cannot follow."""


_SHELLS = {"sh", "bash", "zsh", "dash", "ksh", "mksh", "fish"}
_STDIN_PATHS = {"/dev/stdin", "-", "/dev/fd/0"}
_RESERVED = {"!", "if", "then", "else", "elif", "do", "while", "until", "time", "{", "}"}
# Wrappers that run their operand, and their options that take an argument.
_PREFIXES = {"command": "", "builtin": "", "exec": "a", "nohup": "", "nice": "n", "caffeinate": "", "stdbuf": "",
             "timeout": "sk", "sudo": "ughpCTUrt", "env": "uP"}
_REDIRECT = re.compile(r"([0-9]*)(<<<|<<-|<<|>>|>&|<&|&>>|&>|>\||<>|<|>)(.*)", re.DOTALL)
_HEREDOC_WORD = re.compile(r"__guard_heredoc_([0-9]+)__")
_SUBST_WORD = re.compile(r"__guard_subst_([0-9]+)__")
_HIDDEN_DEPTH = 8


def _ansi_c(text: str) -> str:
    """The value of a `$'…'` body."""
    try:
        return text.encode("latin-1", "backslashreplace").decode("unicode_escape")
    except UnicodeDecodeError as error:
        raise _Unparsed from error


class _Scanner:
    """COMMAND's top-level substitution and heredoc bodies, and its text
    (outer) with each substitution replaced by the word __guard_subst_N__, each
    heredoc operand by __guard_heredoc_N__, `$'…'` by its single-quoted value
    and `${…}` by a word.

    The OUT argument collects the top-level text; it is None inside a
    substitution body, whose own substitutions are found when that body is
    judged as a command of its own."""

    def __init__(self, command: str):
        self.text = command
        self.substitutions: list[str] = []
        self.heredocs: dict[int, str] = {}
        self.pending: list[tuple[str, bool, bool, int | None]] = []
        self.arithmetic = 0
        out: list[str] = []
        self.code(0, None, out)
        if self.pending:
            raise _Unparsed
        self.outer = "".join(out)

    def code(self, i: int, closer: str | None, out: list[str] | None) -> int:
        text, end, depth = self.text, len(self.text), 0
        while i < end:
            char, following = text[i], text[i + 1:i + 2]
            if char == "\\":
                self.emit(out, text[i:i + 2])
                i += 2
            elif char == "'":
                close = text.find("'", i + 1)
                if close < 0:
                    raise _Unparsed
                self.emit(out, text[i:close + 1])
                i = close + 1
            elif char == "$" and following == "'":
                close = i + 2
                while close < end and text[close] != "'":
                    close += 2 if text[close] == "\\" else 1
                if close >= end:
                    raise _Unparsed
                self.emit(out, shlex.quote(_ansi_c(text[i + 2:close])))
                i = close + 1
            elif char == '"':
                self.emit(out, char)
                i = self.expansions(i + 1, out, '"')
            elif char == "`":
                i = self.backtick(i, out)
            elif char in "$<>" and following == "(":
                i = self.substitution(i, out)
            elif char == "$" and following == "{":
                i = self.parameter(i + 2, out)
            elif text.startswith("<<<", i):
                self.emit(out, "<<<")
                i += 3
            elif char == "<" and following == "<" and not self.arithmetic:
                i = self.heredoc_operator(i + 2, out)
            elif char == "\n" and self.pending:
                self.emit(out, char)
                i = self.heredoc_bodies(i + 1, out is not None)
            elif char == "#" and (i == 0 or text[i - 1] in BASH_SPACE + ";&|()"):
                close = text.find("\n", i)
                i = end if close < 0 else close
            else:
                if char == "(":
                    depth += 1
                elif char == ")":
                    if depth == 0 and closer == ")":
                        return i
                    depth = max(depth - 1, 0)  # a stray `)` at top level: a case pattern
                self.emit(out, char)
                i += 1
        if closer is not None:
            raise _Unparsed
        return i

    @staticmethod
    def emit(out: list[str] | None, text: str):
        if out is not None:
            out.append(text)

    def record(self, out: list[str] | None, body: str):
        if out is not None:
            out.append(f" __guard_subst_{len(self.substitutions)}__ ")
            self.substitutions.append(body)

    def expansions(self, i: int, out: list[str] | None, terminator: str | None, end: int | None = None) -> int:
        """Inside double quotes (TERMINATOR '"') or a heredoc body (up to END):
        backslashes, backticks, `$(` and `${` stay active."""
        text, end = self.text, len(self.text) if end is None else end
        while i < end:
            char, following = text[i], text[i + 1:i + 2]
            if char == "\\":
                self.emit(out, text[i:i + 2])
                i += 2
            elif char == terminator:
                self.emit(out, char)
                return i + 1
            elif char == "`":
                i = self.backtick(i, out)
            elif char == "$" and following == "(":
                i = self.substitution(i, out)
            elif char == "$" and following == "{":
                i = self.parameter(i + 2, out)
            else:
                self.emit(out, char)
                i += 1
        if terminator is not None:
            raise _Unparsed
        return i

    def backtick(self, i: int, out: list[str] | None) -> int:
        close = i + 1
        while close < len(self.text) and self.text[close] != "`":
            close += 2 if self.text[close] == "\\" else 1
        if close >= len(self.text):
            raise _Unparsed
        self.record(out, re.sub(r"\\([\\`$])", r"\1", self.text[i + 1:close]))
        return close + 1

    def substitution(self, i: int, out: list[str] | None) -> int:
        arithmetic = self.text[i:i + 3] == "$(("
        self.arithmetic += arithmetic
        close = self.code(i + 2, ")", None)
        self.arithmetic -= arithmetic
        self.record(out, self.text[i + 2:close])
        return close + 1

    def parameter(self, i: int, out: list[str] | None) -> int:
        """`${…}` up to its `}`; substitutions inside it still run."""
        scratch = [] if out is not None else None
        text, depth = self.text, 0
        while i < len(text):
            char, following = text[i], text[i + 1:i + 2]
            if char == "\\":
                i += 2
            elif char == '"':
                i = self.expansions(i + 1, scratch, '"')
            elif char == "`":
                i = self.backtick(i, scratch)
            elif char == "$" and following == "(":
                i = self.substitution(i, scratch)
            elif char == "$" and following == "{":
                depth += 1
                i += 2
            elif char == "}":
                if depth == 0:
                    if out is not None:
                        out.extend(word for word in scratch if word.startswith(" __guard_subst_"))
                        out.append(" __guard_var__ ")
                    return i + 1
                depth -= 1
                i += 1
            else:
                i += 1
        raise _Unparsed

    def heredoc_operator(self, i: int, out: list[str] | None) -> int:
        text = self.text
        strip = text[i:i + 1] == "-"
        i += strip
        while i < len(text) and text[i] in " \t":
            i += 1
        word, quoted = [], False
        while i < len(text) and text[i] not in BASH_SPACE + ";&|<>()":
            char = text[i]
            if char in "'\"":
                close = text.find(char, i + 1)
                if close < 0:
                    raise _Unparsed
                word.append(text[i + 1:close])
                quoted, i = True, close + 1
            elif char == "\\":
                word.append(text[i + 1:i + 2])
                quoted, i = True, i + 2
            else:
                word.append(char)
                i += 1
        delimiter = "".join(word)
        if not delimiter:
            raise _Unparsed
        index = None
        if out is not None:
            index = len(self.heredocs)
            self.heredocs[index] = ""
            out.append(f" <<__guard_heredoc_{index}__ ")
        self.pending.append((delimiter, strip, not quoted, index))
        return i

    def heredoc_bodies(self, i: int, recording: bool) -> int:
        text = self.text
        for delimiter, strip, expands, index in self.pending:
            start = i
            while True:
                newline = text.find("\n", i)
                line = text[i:len(text) if newline < 0 else newline]
                if (line.lstrip("\t") if strip else line) == delimiter:
                    body_end, i = i, (len(text) if newline < 0 else newline + 1)
                    break
                if newline < 0:
                    body_end = i = len(text)  # bash reads to the end, with a warning
                    break
                i = newline + 1
            if index is not None:
                self.heredocs[index] = text[start:body_end]
            if expands and recording:
                self.expansions(start, [], None, body_end)
        self.pending.clear()
        return i


def _pipelines(outer: str) -> list[list[str]]:
    """OUTER split into pipelines of stages at its unquoted control operators,
    parentheses and standalone braces."""
    pipelines, stages, current = [], [], []
    quote, i = None, 0

    def close_stage():
        stage = "".join(current).strip(BASH_SPACE)
        current.clear()
        if stage:
            stages.append(stage)

    def close_pipeline():
        close_stage()
        if stages:
            pipelines.append(list(stages))
        stages.clear()

    while i < len(outer):
        char, following, previous = outer[i], outer[i + 1:i + 2], outer[i - 1:i]
        if char == "\\" and quote != "'":
            current.append(outer[i:i + 2])
            i += 2
            continue
        if quote:
            quote = None if char == quote else quote
            current.append(char)
        elif char in "'\"":
            quote = char
            current.append(char)
        elif char == "|" and following != "|":
            close_stage()
            i += 2 if following == "&" else 1
            continue
        elif char in "|&" and following == char:
            close_pipeline()
            i += 2
            continue
        elif char == "&" and (previous in ("<", ">") or following == ">"):
            current.append(char)
        elif char in ";\n()&" or (char in "{}" and (not previous or previous in BASH_SPACE + ";&|")
                                   and following in ("", *BASH_SPACE, ";")):
            close_pipeline()
        else:
            current.append(char)
        i += 1
    close_pipeline()
    return pipelines


def _strip_prefixes(words: list[str]) -> list[str]:
    """WORDS from the command a stage runs, past assignments, reserved words
    and wrappers that run their operand; _Unparsed for a wrapper that changes
    directory or splits a string into a command."""
    i = 0
    while i < len(words):
        word = words[i]
        name = os.path.basename(word)
        if _ASSIGNMENT.match(word) or word in _RESERVED:
            i += 1
            continue
        if name not in _PREFIXES:
            break
        if name == "command" and words[i + 1:i + 2] in (["-v"], ["-V"]):
            return []
        i += 1
        while i < len(words):
            option = words[i]
            if name in ("env", "sudo") and option.startswith(("-C", "-D", "-S", "--chdir", "--split-string")):
                raise _Unparsed
            if name == "env" and _ASSIGNMENT.match(option):
                i += 1
            elif option == "--":
                i += 1
                break
            elif option.startswith("--"):
                i += 1
            elif option.startswith("-") and len(option) > 1:
                i += 2 if len(option) == 2 and option[1] in _PREFIXES[name] else 1
            else:
                break
        if name == "timeout":
            i += 1  # the duration
    return words[i:]


class _Hidden:
    """The code hidden in COMMAND (see above): codes, a list of shell texts."""

    def __init__(self, command: str):
        self.scanner = _Scanner(command)
        self.codes: list[str] = list(self.scanner.substitutions)
        self.output_parts: list[str] = []
        for pipeline in _pipelines(self.scanner.outer):
            produced: list[str] = []
            for index, stage in enumerate(pipeline):
                words, fed = self.stage(stage)
                stdin = fed if fed is not None else ("\n".join(produced) if index else None)
                if words:
                    self.analyze(words, stdin)
                produced.append(self.literal(words, stage))
                if fed:
                    produced.append(fed)
            self.output_parts.extend(produced)

    @property
    def output(self) -> str:
        """The literal text COMMAND prints, as far as its arguments and fed
        text tell (scanned as code wherever the command's output is run)."""
        return "\n".join(self.output_parts)

    def stage(self, stage: str) -> tuple[list[str], str | None]:
        """STAGE's command words without redirections, and the text a
        here-string or heredoc feeds it."""
        try:
            raw = shlex.split(stage)
        except ValueError as error:
            raise _Unparsed from error
        words, fed, i = [], None, 0
        while i < len(raw):
            match = _REDIRECT.fullmatch(raw[i])
            if not match:
                words.append(raw[i])
                i += 1
                continue
            target = match.group(3)
            if not target and i + 1 < len(raw):
                target, i = raw[i + 1], i + 1
            i += 1
            if match.group(2) == "<<<":
                fed = self.expand(target)
            elif match.group(2) in ("<<", "<<-") and (heredoc := _HEREDOC_WORD.fullmatch(target)):
                fed = self.scanner.heredocs.get(int(heredoc.group(1)), "")
        return _strip_prefixes(words), fed

    def expand(self, word: str) -> str:
        """WORD with each substitution replaced by its literal output."""
        return _SUBST_WORD.sub(lambda match: _Hidden(self.scanner.substitutions[int(match.group(1))]).output, word)

    def literal(self, words: list[str], stage: str) -> str:
        arguments = [self.expand(word) for word in words[1:] if word not in ("-e", "-n", "-E")]
        return " ".join(arguments).replace("\\n", "\n")

    def analyze(self, words: list[str], stdin: str | None):
        name = os.path.basename(words[0])
        if name in _SHELLS:
            i, command_string, reads_stdin = 1, False, False
            while i < len(words):
                word = words[i]
                if word == "--":
                    i += 1
                    break
                if word in ("-o", "+o", "-O", "+O", "--rcfile", "--init-file"):
                    i += 2
                elif word.startswith("--"):
                    i += 1
                elif word[:1] in "-+" and len(word) > 1:
                    command_string |= word[0] == "-" and "c" in word[1:]
                    reads_stdin |= "s" in word[1:]
                    i += 1
                else:
                    break
            operands = words[i:]
            if command_string:
                self.codes.extend(self.expand(word) for word in operands)
            elif operands and not reads_stdin:
                self.script(operands[0], stdin)
            elif stdin:
                self.codes.append(stdin)
        elif name == "eval":
            self.codes.append(self.expand(" ".join(words[1:])))
        elif name in ("source", "."):
            if len(words) > 1:
                self.script(words[1], stdin)
        elif name == "xargs":
            self.xargs(words[1:], stdin or "")

    def script(self, operand: str, stdin: str | None):
        """A shell or `source` reading OPERAND as its script; a file's
        contents are unknown."""
        if operand in _STDIN_PATHS:
            if stdin:
                self.codes.append(stdin)
        elif _SUBST_WORD.search(operand):
            self.codes.append(self.expand(operand))

    def xargs(self, words: list[str], stdin: str):
        i, replace = 0, None
        while i < len(words) and words[i].startswith("-") and words[i] != "-":
            word = words[i]
            i += 1
            if word == "--":
                break
            if word.startswith("--replace"):
                replace = word.partition("=")[2] or "{}"
            elif word.startswith("-i"):
                replace = word[2:] or "{}"
            elif not word.startswith("--") and word[1] in "IJLnPsEdaRS":
                argument = word[2:]
                if not argument and i < len(words):
                    argument, i = words[i], i + 1
                if word[1] in "IJ":
                    replace = argument
        command, arguments = words[i:] or ["echo"], " ".join(stdin.split())
        if replace:
            self.codes.append(shlex.join(word.replace(replace, arguments) for word in command))
        else:
            self.codes.append(f"{shlex.join(command)} {arguments}".rstrip())


def hidden_commands(command: str) -> list[str]:
    """The shell texts COMMAND runs as code besides its own words; raises
    _Unparsed when it cannot tell."""
    return [code for code in _Hidden(command).codes if code.strip(BASH_SPACE)]


class _Guard:
    """One run of the hook over one command."""

    def __init__(self, call: Call, tool: str, command: str, segment: bool = False, depth: int = 0):
        self.call = call
        # A segment is one element of a compound command, judged by
        # compound(); the allowed targets of its writes collect in targets.
        self.segment = segment
        # depth > 0: code hidden in an enclosing command (check_hidden).
        self.depth = depth
        self.targets: set[str] = set()
        self.runtime = call.runtime
        self.tool = tool
        self.command = command
        self.env = call.env
        scan = mask_git_commit_messages(mask_heredoc_bodies(command))
        self.scan = command_substitution(ere.sed(scan, _SHELL_C_QUOTE))

    def fail(self, message: str, status: int):
        raise HookFailure(message, status=status if self.runtime == CLAUDE else 2)

    def deny(self, label: str, detail: str):
        raise _Exit(Result.deny("BLOCKED: " + label + ".\n\n" + detail + _GRANT_ADVICE))

    # --- targets -------------------------------------------------------------

    def repo_from_gh_repo_flag(self) -> str:
        match = _REPO_FLAG.search(self.command)
        return normalize_repo(match.group(4)) if match else repo_from_urlish(self.command)

    def repo_from_gh_repo_env(self) -> str:
        match = _GH_REPO_ENV.search(command_substitution(ere.sed(self.command, _GH_REPO_QUOTES)))
        return normalize_repo(match.group(2)) if match else ""

    def repo_from_gh_repo_positional(self) -> str:
        match = _REPO_POSITIONAL.search(self.command)
        return normalize_repo(match.group(3)) if match else ""

    def repo_from_gh_api_endpoint(self) -> str:
        match = _API_ENDPOINT.search(self.command)
        return normalize_repo(match.group(2) + "/" + match.group(3)) if match else ""

    def _git_text(self, args: list[str], cwd: str | None = None) -> str:
        result = subprocess.run(["git", *args], cwd=cwd, env=self.env,
                                stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        return command_substitution(result.stdout.decode("utf-8", "surrogateescape"))

    def repo_from_local_git(self) -> str:
        """lib-repo-root.sh, then the origin, upstream or any other GitHub
        remote of that repository."""
        try:
            context = repo.repo_root(self.command, self.call.raw, self.runtime, self.env)
            in_repository, cwd = bool(context.root), context.cwd
        except HookFailure:
            # `cd "$REPO_ROOT"` failed inside `$(...)`, where errexit is off:
            # the remotes were read from the hook's own directory.
            in_repository, cwd = True, bash_pwd(self.env)
        if not in_repository:
            return ""
        for remote in ("origin", "upstream"):
            url = self._git_text(["remote", "get-url", remote], cwd)
            if url and (found := repo_from_urlish(url)):
                return found
        listing = subprocess.run(["git", "remote", "-v"], cwd=cwd, env=self.env,
                                 stdout=subprocess.PIPE, stderr=subprocess.DEVNULL).stdout
        records = listing.decode("utf-8", "surrogateescape").split("\n")
        if listing.endswith(b"\n") or not listing:
            records.pop()
        urls = [(re.split(r"[ \t]+", record.strip(" \t")) + [""])[1] for record in records]
        for url in _sorted_unique(urls):
            if found := repo_from_urlish(url):
                return found
        return ""

    def repo_from_git_dir(self, directory: str) -> str:
        for remote in ("origin", "upstream"):
            url = self._git_text(["-C", directory, "remote", "get-url", remote])
            if url and (found := repo_from_urlish(url)):
                return found
        return ""

    def target_repo_for_gh(self) -> str:
        return self.repo_from_gh_repo_flag() or self.repo_from_gh_repo_env() or self.repo_from_local_git()

    def target_repo_for_api(self) -> str:
        return self.repo_from_gh_api_endpoint() or self.target_repo_for_gh()

    def resolved_run_dir(self) -> tuple[int, str]:
        """The single literal directory the command runs in: (0, path), (1, "")
        when a directory change cannot be resolved without evaluating the
        shell, (2, "") when it changes no directory."""
        changes = len(ere.grep_only(_DIRECTORY_CHANGE, self.command, echo=True))
        match = _DASH_C.search(self.command)
        dash_c = match.group(3) if match else ""
        if changes == 0 and not dash_c:
            return 2, ""
        if changes > 1:
            return 1, ""
        if dash_c:
            target = dash_c
        elif match := _CD_REST.search(self.command):
            target = match.group(3).split("&&", 1)[0].split(";", 1)[0].split("|", 1)[0]
            target = target.replace('"', "").replace("'", "").replace("\\ ", " ").rstrip(BASH_SPACE)
        else:
            return 1, ""
        home_value = self.env["HOME"]
        target = target.replace("${HOME}", home_value).replace("$HOME", home_value)
        if target.startswith("~"):
            target = home_value + target[1:]
        if any(char in target for char in "$`*?") or not os.path.isdir(target):
            return 1, ""
        return 0, target

    # --- allowlist -------------------------------------------------------------

    @functools.cached_property
    def allowlist(self) -> list[str]:
        return _allowlist_patterns(self.env)

    def repo_allowed(self, target: str) -> bool:
        target = normalize_repo(target)
        for pattern in self.allowlist:
            if pattern.endswith("/*"):
                if target.startswith(pattern[:-1]):
                    return True
            elif target == pattern:
                return True
        return target in _declared_repos(self.env)

    def deny_if_compound(self):
        if compound_shell_command(self.command):
            self.deny("compound GitHub command has no segment-local repository target",
                      "Run each GitHub write in a separate tool call so the guard can bind it to exactly one repository.")

    def require_allowed_repo(self, action: str, target: str):
        self.deny_if_compound()
        self.require_allowed_segment_repo(action, target)

    def require_allowed_segment_repo(self, action: str, target: str):
        if not target:
            self.deny(f"{action} has no unambiguous repository target", _AMBIGUOUS_ADVICE[self.runtime])
        if not self.repo_allowed(target):
            self.deny(f"{action} targets non-allowlisted repo {target}", _NOT_ALLOWLISTED_ADVICE)
        self.targets.add(normalize_repo(target))

    # --- compound commands -------------------------------------------------------

    def writes(self) -> bool:
        """Whether the command runs any GitHub write that run() gates, in its
        own words or in code hidden in them."""
        if self.direct_writes():
            return True
        if self.depth >= _HIDDEN_DEPTH:
            return True
        try:
            codes = hidden_commands(self.command)
        except _Unparsed:
            return self.unparsed_writes()
        return any(self.child(code, segment=True).writes() for code in codes)

    def unparsed_writes(self) -> bool:
        """A write pattern anywhere in the command, quotes and brackets ignored."""
        relaxed = re.sub(r"[\"'`(){}$<>\\]", " ", self.command)
        return _Guard(self.call, self.tool, relaxed, segment=True, depth=_HIDDEN_DEPTH).direct_writes()

    def child(self, code: str, segment: bool = False) -> "_Guard":
        return _Guard(self.call, self.tool, code, segment=segment, depth=self.depth + 1)

    def direct_writes(self) -> bool:
        """Whether the command's own words run a GitHub write that run() gates."""
        scan = self.scan
        if ere.grep(_GIT_PUSH, scan):
            return True
        patterns = (_GH_PR, _GH_ISSUE, _GH_SECRET, _GH_WORKFLOW, _GH_RUN, _GH_RELEASE, _GH_REPO, _GH_LABEL, _GH_GIST,
                    _GH_REPOSITORY, _GH_REPOSITORY_MORE, _GH_UNSCOPED)
        return any(ere.grep(pattern, scan, echo=True) for pattern in patterns) or self.gh_api_write()

    def context_dir(self) -> str | None:
        """The absolute directory the command starts in, found as
        repo.repo_root finds it, or None."""
        directory = self.env.get("REPO_CONTEXT_DIR", "") or repo._payload_context_dir(self.call.raw)
        if self.runtime == CODEX:
            parent = codex_payload.parent_exec_workdir(self.call.raw, self.command)
            directory = command_substitution(parent or "") or directory
        directory = directory or bash_pwd(self.env)
        return os.path.abspath(directory) if os.path.isdir(directory) else None

    def cd_target(self, stage: str, words: list[str], here: str | None) -> str | None:
        """The directory a literal `cd DIR` element lands in from HERE (None:
        unknown), or None when it cannot be resolved without the shell."""
        if len(words) != 2 or _ASSIGNMENT.match(stage):
            return None  # `cd`, `cd -P DIR`, `cd A B`, `CDPATH=x cd DIR`
        target, raw = words[1], stage[2:].lstrip(BASH_SPACE)
        if target == raw and (raw == "~" or raw.startswith("~/")) and not _GLOB_OR_EXPANSION & set(raw[1:]):
            target = self.env.get("HOME", "") + raw[1:]  # an unquoted ~ is HOME
        elif not target or target.startswith("-") or _GLOB_OR_EXPANSION & set(target):
            return None
        if not target.startswith("/"):
            # CDPATH could send a bare name elsewhere.
            if here is None or (self.env.get("CDPATH") and not target.startswith(("./", "../"))):
                return None
            target = os.path.join(here, target)
        target = os.path.normpath(target)
        return target if os.path.isdir(target) else None

    def judge_segment(self, text: str, directory: str, hidden: bool = False) -> tuple[Result, set[str]]:
        """The whole hook's decision for TEXT run in DIRECTORY, and the
        repositories its writes target. HIDDEN: TEXT is code hidden in this
        command, judged as a whole command (its own compound elements bound
        to their `cd`) but never matched against a scoped grant."""
        payload = {"tool_name": self.call.tool_name, "tool_input": {"command": text}, "cwd": directory}
        env = {key: value for key, value in self.env.items() if key != "REPO_CONTEXT_DIR"}
        env.update(PWD=directory, OLDPWD=bash_pwd(self.env))
        with _working_directory(directory):
            guard = _Guard(Call.from_payload(self.runtime, payload, env), self.tool, text, segment=not hidden,
                           depth=self.depth + hidden)
            try:
                result = guard.run()
            except _Exit as done:
                result = done.result
        return result, guard.targets

    def compound(self) -> Result | None:
        """The decision for a compound command judged element by element (see
        shell_segments), or None to keep the whole-command rule."""
        segments = shell_segments(self.command)
        if segments is None or any(_stateful(segment) for segment in segments):
            return None
        start = self.context_dir()
        here, targets = start, set()
        for segment in segments:
            words = _command_words(segment.stages[0])
            if len(segment.stages) == 1 and words[0] == "cd":
                here = self.cd_target(segment.stages[0], words, here)
            elif not _text_only(segment):
                text = segment.text
                if _Guard(self.call, self.tool, text, segment=True).writes():
                    if here is None:
                        self.deny("GitHub write target directory cannot be resolved",
                                  "An earlier `cd` in this command is not a literal existing directory, or its "
                                  "`&&` chain ended (`;`, a newline, `||`, `&`) or ran a command that may change "
                                  "directory. Bind the write with `cd DIR && …` in one `&&` chain, or run it in a "
                                  "separate tool call.")
                    result, found = self.judge_segment(text, here)
                    if result.decision == "deny":
                        return result
                    targets |= found
                if any((stage_words := _command_words(stage)) is None or stage_words[0] not in _CWD_PRESERVING
                       for stage in segment.stages):
                    here = None
            if segment.connector not in ("&&", None) and here != start:
                here = None
        if len(targets) > 1:
            self.deny("compound GitHub command writes to more than one repository",
                      "Targets: " + ", ".join(sorted(targets)) + ". Run each repository's writes in a separate tool "
                      "call.")
        return Result.allow()

    # --- the hook --------------------------------------------------------------

    def nested_exec(self) -> Result:
        """Each literal nested exec_command of a functions.exec program, run
        through the whole hook as `functions.exec_command` in its workdir.
        The shell read the contexts through a process substitution, so a
        parser failure still classifies the contexts found before it."""
        try:
            contexts = codex_payload.nested_exec_contexts(self.command)
        except codex_payload.PartialOutput as error:
            contexts = error.values
        for context in contexts:
            if context.get("ambiguous") is True:
                self.deny("functions.exec contains an ambiguous nested exec_command",
                          "Name cmd and workdir as literal object fields so the GitHub write target can be checked "
                          "without evaluating JavaScript.")
            nested_command = command_substitution(jq_text(context.get("cmd")))
            if not nested_command:
                self.deny("functions.exec contains an empty nested exec_command",
                          "The GitHub write guard cannot classify an empty command.")
            workdir = command_substitution(jq_text(context.get("workdir")))
            payload = {"tool_name": "functions.exec_command", "tool_input": {"cmd": nested_command}}
            if workdir:
                if not os.path.isdir(workdir):
                    self.deny("functions.exec nested workdir cannot be resolved",
                              "The literal workdir does not name an existing directory.")
                result = self._decide_in(workdir, payload)
            else:
                result = decide(Call.from_payload(CODEX, payload, self.env))
            if result.decision == "deny":
                return result
        return Result.allow()

    def _decide_in(self, workdir: str, payload: dict) -> Result:
        """`cd -- WORKDIR && … | "$0"`."""
        landed = codex_payload._bash_cd(workdir, self.env)
        if landed is None:
            raise HookFailure(f"cd {workdir}: cannot enter")
        directory, printed = landed
        if printed:
            # `cd` printed the directory into the captured result, which the
            # hook then emitted as its response: never a valid decision.
            raise HookFailure(f"cd {workdir} printed its target")
        env = dict(self.env, OLDPWD=bash_pwd(self.env), PWD=directory)
        with _working_directory(directory):
            return decide(Call.from_payload(CODEX, payload, env))

    def run(self) -> Result:
        if self.runtime == CODEX and self.tool == "functions.exec":
            return self.nested_exec()
        # A grant authorizes one exact command, so it is matched against the
        # whole command, never against one element of a compound command.
        status = 1 if self.segment or self.depth else _authorization_status(self.command)
        if status == 0:
            return Result.allow()
        if status != 1:
            self.deny("scoped GitHub authorization check failed", "Repair or remove the invalid grant; do not bypass the guard.")
        if "HOME" not in self.env:
            # DECLARED_REPOS_REPO="$HOME/…" is an unbound variable under `set -u`: exit 1.
            self.fail("HOME is unset", 1)

        pipeline_repo = broker_secret_pipeline_repo(self.command)
        if pipeline_repo and _op_policy_allows(self.command):
            self.require_allowed_segment_repo("gh secret/variable write operation", pipeline_repo)
            return Result.allow()

        if not self.segment and compound_shell_command(self.command):
            result = self.compound()
            if result is not None:
                return result
        command, scan = self.command, self.scan
        if ere.grep(_GIT_PUSH, scan):
            # The dry-run shortcut reads the whole command, so a compound
            # command could pair a dry run with a real push.
            self.deny_if_compound()
            if ere.grep(_DRY_RUN, command, echo=True):
                return Result.allow()
            target = repo_from_urlish(command)
            if not target:
                run_status, run_dir = self.resolved_run_dir()
                if run_status == 1:
                    self.deny("git push target directory cannot be resolved",
                              "The command changes directory and does not name a repository, so this guard cannot "
                              "tell which repo the push goes to without evaluating the shell. Name the target "
                              "explicitly, e.g. git push https://github.com/OWNER/REPO.git HEAD:BRANCH.")
                target = self.repo_from_local_git() if run_status == 2 else self.repo_from_git_dir(run_dir)
            self.require_allowed_repo("git push", target)
            if EPOCH_PUSH_ENFORCED and normalize_repo(target).startswith(_EPOCH_OWNER):
                self.check_epoch_push(normalize_repo(target))

        for pattern, action in ((_GH_PR, "gh pr write operation"), (_GH_ISSUE, "gh issue write operation")):
            if ere.grep(pattern, scan, echo=True):
                self.require_allowed_repo(action, self.target_repo_for_gh())
        if ere.grep(_GH_SECRET, scan, echo=True):
            if ere.grep(_NOT_REPO_SCOPED, command, echo=True):
                self.deny("organization/environment/app GitHub secret or variable mutation",
                          "This operation is not repo-scoped, so the repo allowlist cannot authorize it.")
            self.require_allowed_repo("gh secret/variable write operation", self.target_repo_for_gh())
        for pattern, action in ((_GH_WORKFLOW, "gh workflow write operation"), (_GH_RUN, "gh run write operation"),
                                (_GH_RELEASE, "gh release write operation")):
            if ere.grep(pattern, scan, echo=True):
                self.require_allowed_repo(action, self.target_repo_for_gh())
        if ere.grep(_GH_REPO, scan, echo=True):
            target = self.repo_from_gh_repo_positional()
            if not target and ere.grep(_GH_REPO_CREATE, scan, echo=True):
                self.deny("gh repo create without an explicit OWNER/REPO target",
                          "A repo creation does not act on the current directory's repository, so that repository's "
                          "allowlist entry cannot authorize it. Name the target as OWNER/REPO.")
            if not target:
                target = self.target_repo_for_gh()
                if not target:
                    # `[ -n "$repo" ] || repo=$(target_repo_for_gh)` failed as
                    # the last command of an and-or list, so `set -e` exited.
                    self.fail("gh repo write operation: no target repository", 1)
            self.require_allowed_repo("gh repo write operation", target)
        if ere.grep(_GH_LABEL, scan, echo=True):
            self.require_allowed_repo("gh label/milestone write operation", self.target_repo_for_gh())
        if ere.grep(_GH_GIST, scan, echo=True):
            self.deny("gh gist write operation", "Gists are not repo-scoped, so the repo allowlist cannot authorize them.")
        for pattern in (_GH_REPOSITORY, _GH_REPOSITORY_MORE):
            if ere.grep(pattern, scan, echo=True):
                self.require_allowed_repo("gh repository write operation", self.target_repo_for_gh())
        if ere.grep(_GH_UNSCOPED, scan, echo=True):
            self.deny("non-repository-scoped gh write operation",
                      "This operation has no single existing repository target that the repo allowlist can authorize.")
        if self.gh_api_write():
            self.require_allowed_repo("gh api write operation", self.target_repo_for_api())
        self.check_hidden()
        return Result.allow()

    def push_directory(self) -> str | None:
        status, directory = self.resolved_run_dir()
        if status == 0:
            return directory
        if status == 2:
            try:
                return repo.repo_root(self.command, self.call.raw, self.runtime, self.env).cwd
            except HookFailure:
                return None
        return None

    def check_epoch_push(self, target: str):
        """Allow an agent push to epoch-research only for named non-default
        branches of a repo the Writer app is not installed on."""
        directory = self.push_directory()

        def git_out(*args: str) -> str:
            if directory is None:
                return ""
            return self._git_text(list(args), directory).strip()

        branches = push_destinations(self.command, lambda: git_out("symbolic-ref", "--quiet", "--short", "HEAD"))
        defaults = {"main", "master"}
        if remote_head := git_out("symbolic-ref", "--quiet", "--short", "refs/remotes/origin/HEAD"):
            defaults.add(remote_head.removeprefix("origin/"))
        if branches and not set(branches) & defaults and target in _writer_missing(self.env):
            return
        raise _Exit(Result.deny(f"BLOCKED: agent git push to {target}.\n\n" + _EPOCH_PUSH_ADVICE))

    def check_hidden(self):
        """Judge the code hidden in the command (see _Hidden) as whole commands
        run in the directory the command starts in. A compound command whose
        elements were not judged one by one cannot bind them to a directory."""
        try:
            codes = hidden_commands(self.command)
        except _Unparsed:
            if self.unparsed_writes():
                self.deny("GitHub write in shell text the guard cannot parse",
                          "The command hides a git/gh write in quoting, substitution or a wrapper (env -C, env -S, "
                          "sudo -D) that this guard cannot follow. Run the write as a plain command.")
            return
        hidden = [code for code in codes if self.child(code, segment=True).writes()]
        if not hidden:
            return
        if self.depth >= _HIDDEN_DEPTH:
            self.deny("GitHub write nested too deeply", "Run the write as a plain command.")
        self.deny_if_compound()
        directory = self.context_dir()
        if directory is None:
            self.deny("GitHub write target directory cannot be resolved",
                      "The command's starting directory does not exist, so the target of the write it runs as "
                      "hidden code cannot be checked.")
        for code in hidden:
            result, found = self.judge_segment(code, directory, hidden=True)
            if result.decision == "deny":
                raise _Exit(result)
            self.targets |= found
        if len(self.targets) > 1:
            self.deny("GitHub command writes to more than one repository",
                      "Targets: " + ", ".join(sorted(self.targets)) + ". Run each repository's writes in a separate "
                      "tool call.")

    def gh_api_write(self) -> bool:
        command = self.command
        if not ere.grep(_GH_API, self.scan, echo=True):
            return False
        if ere.grep(_WRITE_METHOD, command, echo=True) or ere.grep(_WRITE_WORD, command, echo=True):
            return True
        if ere.grep(_GRAPHQL, command, echo=True) and ere.grep(_MUTATION, command, echo=True):
            return True
        if ere.grep(_FIELD, command, echo=True):
            if ere.grep(_GET_METHOD, command, echo=True):
                return False
            return not (ere.grep(_GRAPHQL, command, echo=True) and not ere.grep(_MUTATION, command, echo=True))
        return False


def decide(call: Call) -> Result:
    tool = tool_name(call)
    if tool not in (("Bash",) if call.runtime == CLAUDE else CODEX_SHELL_TOOLS):
        return Result.allow()
    command = call.shell_command()
    if not command:
        return Result.allow()
    try:
        return _Guard(call, tool, command).run()
    except _Exit as done:
        return done.result
