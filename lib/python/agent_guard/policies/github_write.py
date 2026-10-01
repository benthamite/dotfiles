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
_REF = re.compile(r"op://[^\s]+")
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


class _Guard:
    """One run of the hook over one command."""

    def __init__(self, call: Call, tool: str, command: str):
        self.call = call
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
        status = _authorization_status(self.command)
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
        return Result.allow()

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
