"""Sensitive-file reads: block-sensitive-read.sh (both runtimes) and the
check_sensitive_read copy inlined in claude/hooks/pretooluse-bash.sh.

The hooks deny tool calls whose arguments name files likely to hold secrets.
Claude's copy classifies Read paths with `case` globs and Grep scopes by
directory; Codex's classifies Read paths with the same regexes it applies to
commands, in a different label order. Both are kept as they were.

The regexes are the hooks' `grep -E` patterns, matched through agent_guard.ere
as /usr/bin/grep matched them.
"""

from __future__ import annotations

import os
import re

from ..codex_payload import command_substitution, jq_alternative, jq_raw, jq_text
from ..core import CLAUDE, CODEX, Call, HookFailure, Result
from ..ere import ere, grep, sed
from ..shell_text import mask_git_commit_messages, mask_heredoc_bodies


def tool_name(call: Call) -> str:
    """`$(jq -r '.tool_name // empty')` (and `codex_tool_name`)."""
    return command_substitution(jq_text(call.raw.get("tool_name")))


def claude_field(call: Call, key: str) -> str:
    """`$(jq -r '.tool_input.KEY // empty')`."""
    return command_substitution(jq_text(call.tool_input.get(key)))


def codex_field(call: Call, key: str) -> str:
    """`$(codex_tool_input_field "$INPUT" KEY)`."""
    return command_substitution(jq_text(call.tool_input.get(key)))


def command(call: Call) -> str:
    """The Bash command as the hook's `$(...)` captured it."""
    return call.shell_command()


def home(call: Call) -> str:
    """`$HOME`; unset, it was an unbound-variable error under `set -u`."""
    value = call.env.get("HOME")
    if value is None:
        raise HookFailure("HOME is unset")
    return value


def expand_tilde(path: str, home_dir: str) -> str:
    """`${PATH/#\\~/$HOME}`: a leading `~` (only the character) becomes $HOME."""
    return home_dir + path[1:] if path.startswith("~") else path


def _glob(*parts: str) -> re.Pattern:
    """A `case` pattern: `*` in PARTS is a wildcard, everything else literal."""
    return re.compile("".join(".*" if part == "*" else re.escape(part) for part in parts), re.S)


def _case(text: str, *patterns: re.Pattern) -> bool:
    return any(p.fullmatch(text) for p in patterns)


# --- reasons -------------------------------------------------------------------

_ADVICE = {
    CLAUDE: ("This could expose secret values in the conversation context. Use a safe alternative: "
             "op-automations/op-desktop/git-crypt for auth-managed secrets; ls/stat/file/wc -l/-c or git "
             "check-ignore/git ls-files for metadata; grep -l/-c/-q/-L for filename/count/quiet matches. Loading "
             "environment files into a subprocess is allowed only when the command does not print or inspect the "
             "loaded secrets."),
    CODEX: ("This could expose secret values in the conversation context. Use a safe alternative: "
              "op-automations/op-desktop/git-crypt/security for auth-managed secrets; ls/stat/file/wc or git "
              "check-ignore/git ls-files for metadata; grep/rg with -l, -c, -q, or -L for filename/count/quiet "
              "checks; or a purpose-built helper that returns only non-secret metadata."),
}
ENV_LOADER_CONTEXT = {
    CLAUDE: ("Sensitive environment file loading was allowed for this Bash command. Do not print, inspect, "
             "summarize, or quote loaded environment values; use them only as process environment for the "
             "requested command."),
    CODEX: ("Sensitive environment file loading was allowed for this shell command. Do not print, inspect, "
              "summarize, or quote loaded environment values; use them only as process environment for the "
              "requested command."),
}
_HEREDOC_DETAIL = ("a program fed to an interpreter through a heredoc names this file and the guard cannot prove "
                   "the program does not read it; write the program to a file and run that, or drop the mention")
_COMPOUND = {CLAUDE: "compound Bash command", CODEX: "compound shell command"}
_CONTENTS = {CLAUDE: "Bash command appears", CODEX: "command appears"}


def _deny(runtime: str, label: str, detail: str) -> Result:
    return Result.deny(f"BLOCKED: sensitive read: {label} — {detail}.\n\n{_ADVICE[runtime]}")


# --- classification of command text ----------------------------------------------

_ENV_OP_TEMPLATE = (
    (r"\.env(\.[A-Za-z0-9_-]+)*\.example$", "ENV_EXAMPLE_TEMPLATE", ""),
    (r"\.env(\.[A-Za-z0-9_-]+)*\.example([^A-Za-z0-9_.])", r"ENV_EXAMPLE_TEMPLATE\2", "g"),
    (r"\.env\.op$", "ENV_OP_TEMPLATE", ""),
    (r"\.env\.op([^A-Za-z0-9_.])", r"ENV_OP_TEMPLATE\1", "g"),
)

_SSH_KEY = ere(r"(^|[ /=])\.ssh/id_[A-Za-z0-9_]+\b")
_SSH_PUB = ere(r"\.ssh/id_[A-Za-z0-9_]+\.pub\b")
_LABEL_TESTS = {
    "shell secrets file": ere(r"\.zshenv-secrets\b"),
    "MCP credential config": ere(r"(^|[[:space:]/])\.mcp\.json\b|(^|[[:space:]/])mcp\.json\b"),
    # A quote or parenthesis in front of the name (`open('.env')`) names the file too.
    "environment secrets file": ere(r"""(^|[[:space:]/:=("'])\.env([.[:space:]"';&|)]|$)|(^|[[:space:]/:=("'])\.envrc\b"""),
    "SSH private key": None,
    "GPG keyring": ere(r"\.gnupg/"),
    "OAuth tokens": ere(r"(^|[[:space:]])?/?[^[:space:]]*\.config/[^/[:space:]]+/tokens\.json\b"),
    "Gmail MCP credentials": ere(r"(^|[[:space:]])?/?[^[:space:]]*\.gmail-mcp-epoch/credentials/"),
    "1Password read cache": ere(r"op-automations/cache\b"),
    "OAuth client secret": ere(r"""(^|[[:space:]])?/?[^[:space:]]*\.config/[^/[:space:]]+/(secret\.json|client_secret[^"[:space:]]*\.json)\b"""),
    "credential JSON": ere(r"(^|[[:space:]/])(credentials\.json|service-account[^/[:space:]]*\.json|tokens\.json)\b"),
}
_LABEL_ORDER = {
    CLAUDE: ("shell secrets file", "MCP credential config", "environment secrets file", "SSH private key",
             "GPG keyring", "OAuth tokens", "Gmail MCP credentials", "1Password read cache", "OAuth client secret",
             "credential JSON"),
    CODEX: ("MCP credential config", "shell secrets file", "SSH private key", "GPG keyring", "OAuth tokens",
              "Gmail MCP credentials", "1Password read cache", "OAuth client secret", "credential JSON",
              "environment secrets file"),
}


def label_for_text(runtime: str, text: str) -> str:
    """sensitive_label_for_command (Claude) / sensitive_label_for_text (Codex):
    the first sensitive-file label TEXT names, or ""."""
    text = command_substitution(sed(mask_git_commit_messages(text), *_ENV_OP_TEMPLATE))
    for label in _LABEL_ORDER[runtime]:
        if label == "SSH private key":
            if grep(_SSH_KEY, text, echo=True) and not grep(_SSH_PUB, text, echo=True):
                return label
        elif grep(_LABEL_TESTS[label], text, echo=True):
            return label
    return ""


# --- allowances ---------------------------------------------------------------------

_ENV_SOURCE = ere(r"""(^|[[:space:]&;|(])(\.|source)[[:space:]]+[^&;|[:space:]]*\.env([.[:space:]"';&|)]|$)"""
                  r"""|(^|[[:space:]&;|(])(\.|source)[[:space:]]+[^&;|[:space:]]*\.envrc\b""")
_ENV_DUMPERS = (
    # These commands commonly dump the whole environment or shell state.
    ere(r"(^|[[:space:]&;|])(env|printenv|declare|typeset)([[:space:]&;|]|$)"),
    ere(r"(^|[[:space:]&;|])export[[:space:]]+-p([[:space:]&;|]|$)"),
    ere(r"(^|[[:space:]&;|])set[[:space:]]*([;&|)]|$)"),
    # Sourcing is safe; printing or parsing the environment file contents is not.
    ere(r"""\b(cat|sed|awk|perl|python[0-9.]*|ruby|node|head|tail|less|more|grep|rg|ripgrep)\b[^&;|]*\.env([.[:space:]"';&|)]|$)"""),
    ere(r"\b(cat|sed|awk|perl|python[0-9.]*|ruby|node|head|tail|less|more|grep|rg|ripgrep)\b[^&;|]*\.envrc\b"),
)


def is_safe_env_loader(command: str) -> bool:
    return grep(_ENV_SOURCE, command, echo=True) and not any(grep(p, command, echo=True) for p in _ENV_DUMPERS)


def is_safe_shell_export_classifier(command: str, home_dir: str) -> bool:
    """The security-audit skill's exact classify-shell-exports.py invocation."""
    target = f"{home_dir}/My Drive/dotfiles/shell/.zshenv-secrets"
    for python in ("python3", "/usr/bin/python3", "/opt/homebrew/bin/python3"):
        for tool in (".claude", ".codex"):
            helper = f"{home_dir}/My Drive/dotfiles/macos/{tool}/skills/security-audit/scripts/classify-shell-exports.py"
            if command in (f'{python} "{helper}" "{target}"', f"{python} '{helper}' '{target}'"):
                return True
    return False


_NO_MASKING = ere(r"OP_RUN_NO_MASKING")
_TERMINAL_REDIRECT = ere(r">[[:space:]]*(/dev/(std(out|err)|fd/|tty)|&|-([[:space:]]|$))")
_FILE_REDIRECT = (r"[[:space:]]*[12]?&?>>?[[:space:]]*[^[:space:];&|<>()]+", "", "g")
_DQ = r'"[^"`;&|<>()]*"'
_SQ = r"'[^']*'"
_WORD = r"[^[:space:];&|<>()]"
_VALUE = f"({_DQ}|{_SQ}|{_WORD}*)"
_PATH = f"({_DQ}|{_SQ}|{_WORD}+)"
_OP_RUN = ere(rf"^[[:space:]]*(cd[[:space:]]+{_PATH}[[:space:]]*&&[[:space:]]*)?"
              rf"([A-Za-z_][A-Za-z0-9_]*={_VALUE}[[:space:]]+)*([^;&|[:space:]]*/)?op-automations[[:space:]]+run"
              rf"[[:space:]]+(--env-file(=|[[:space:]]+){_PATH}[[:space:]]+)+--[[:space:]]+{_WORD}+"
              r"([[:space:]]+[^;&|<>()`]*)?$")
_OP_ASSIGNMENT = ere(r"(^|[[:space:]])OP_[A-Za-z0-9_]*=")
_OP_RUN_SHELL = ere(r"--[[:space:]]+([^[:space:]]*/)?(env|printenv|set|export|declare|typeset|bash|sh|zsh|dash|ksh|eval)"
                    r"([[:space:]]|$)")
_SUBSTITUTION = ere(r"\$\(|`")


def is_safe_op_run_loader(command: str) -> bool:
    """`op-automations run --env-file F -- <program>` loads F into the
    program's environment, as `source .env` does, with 1Password masking its
    output. Allowed when the only composition is an optional leading `cd DIR
    &&`, leading VAR=value assignments (none OP_*) and file redirects, and the
    program is not an environment dumper or a shell."""
    if grep(_NO_MASKING, command) or grep(_TERMINAL_REDIRECT, command):
        return False
    stripped = command_substitution(sed(command, _FILE_REDIRECT))
    return (grep(_OP_RUN, stripped)
            and not any(grep(p, stripped) for p in (_OP_ASSIGNMENT, _OP_RUN_SHELL, _SUBSTITUTION)))


_COMPOSITION = ere(r"[;&|<>`]|[$][(]")


def has_shell_composition(command: str) -> bool:
    """A safe command prefix is not enough: `ls sensitive; cat sensitive` must
    not inherit the first command's allowance."""
    return grep(_COMPOSITION, command) or "\n" in command


_METADATA_COMMANDS = tuple(ere(p) for p in (
    # Auth-aware tools manage secrets safely; op-automations and op-desktop exec the real `op`.
    r"^[[:space:]]*(op[[:space:]]|op-automations[[:space:]]|op-desktop[[:space:]]|git-crypt[[:space:]]|security[[:space:]])",
    # Git ignore/index metadata; `-C DIR` only chooses the repository.
    r"^[[:space:]]*git[[:space:]]+(-C[[:space:]]+[^[:space:];&|<>()]+[[:space:]]+)?(check-ignore|ls-files)([[:space:]]|$)",
    r"^[[:space:]]*(ls|stat|file|basename|dirname|realpath)([[:space:]]|$)",
    r"^[[:space:]]*wc([[:space:]]+-[lcwmL]+)*([[:space:]]|$)",
    # grep/rg/ripgrep with a filename/count/quiet output flag.
    r"^[[:space:]]*(grep|rg|ripgrep)[[:space:]]+(-[a-zA-Z]*[lcqL][a-zA-Z]*[[:space:]]"
    r"|--(files-with-matches|files-without-match|count|quiet|silent)[[:space:]])",
    r"^[[:space:]]*(\[\[?|test)[[:space:]]+-[fedrwxs][[:space:]]",
))


def _check_command(runtime: str, command: str, home_dir: str) -> tuple[str, str, str]:
    """The Bash branch: (label, detail, "") for a denial, ("", "", context) otherwise."""
    # Heredoc bodies fed to a data sink are data; a body fed to an interpreter
    # stays in the scan because it may read the file.
    label = label_for_text(runtime, mask_heredoc_bodies(command))
    if not label:
        return "", "", ""
    if not label_for_text(runtime, mask_heredoc_bodies(command, "all")):
        return label, _HEREDOC_DETAIL, ""
    if label == "shell secrets file" and is_safe_shell_export_classifier(command, home_dir):
        return "", "", ""
    if label == "environment secrets file" and (is_safe_env_loader(command) or is_safe_op_run_loader(command)):
        return "", "", ENV_LOADER_CONTEXT[runtime]
    if has_shell_composition(command):
        return label, f"{_COMPOUND[runtime]} cannot use a metadata-command allowance safely", ""
    if any(grep(p, command, echo=True) for p in _METADATA_COMMANDS):
        return "", "", ""
    return label, f"{_CONTENTS[runtime]} to read or process the contents of {label}", ""


# --- Claude Read and Grep -------------------------------------------------------------

def _claude_read_label(path: str, home_dir: str) -> str:
    """Labels for an expanded Read path, in the hook's `case` order."""
    if _case(path, _glob("*", "/.mcp.json"), _glob("*", "/mcp.json")):
        return "MCP credential config"
    if _case(path, _glob("*", ".zshenv-secrets")):
        return "shell secrets file"
    # `.env.op` is the 1Password reference template (op:// references only).
    if not _case(path, _glob("*", "/.env.op"), _glob("*", "/.env", "*", ".example")) and \
            _case(path, _glob("*", "/.env"), _glob("*", "/.env.", "*"), _glob("*", "/.envrc")):
        return "environment secrets file"
    for prefix in (home_dir, "/Users/pablostafforini"):
        if _case(path, _glob(prefix + "/.ssh/id_", "*")):
            return "SSH private key"
    for prefix in (home_dir, "/Users/pablostafforini"):
        if _case(path, _glob(prefix + "/.gnupg/", "*")):
            return "GPG keyring"
    for prefix in (home_dir, "/Users/pablostafforini"):
        if _case(path, _glob(prefix + "/.config/", "*", "/tokens.json")):
            return "OAuth tokens"
    for prefix in (home_dir, "/Users/pablostafforini"):
        if _case(path, _glob(prefix + "/.gmail-mcp-epoch/credentials/", "*")):
            return "Gmail MCP credentials"
    if _case(path, _glob("*", "/op-automations/cache"), _glob("*", "/op-automations/cache/", "*")):
        return "1Password read cache"
    for prefix in (home_dir, "/Users/pablostafforini"):
        if _case(path, _glob(prefix + "/.config/", "*", "/secret.json"),
                 _glob(prefix + "/.config/", "*", "/client_secret", "*", ".json")):
            return "OAuth client secret"
    return ""


def _scope_label(path: str, home_dir: str) -> str:
    """grep_scope_label: why a Grep content search under PATH may hit secrets."""
    def under(*dirs: str) -> bool:
        return any(_case(path, _glob(d), _glob(d + "/", "*")) for d in dirs)

    if path in (home_dir, home_dir + "/", "/"):
        return "broad directory scope containing user secrets"
    for name, label in ((".ssh", "SSH directory"), (".gnupg", "GPG keyring")):
        if under(f"{home_dir}/{name}", f"/Users/pablostafforini/{name}"):
            return label
    for prefix in (home_dir, "/Users/pablostafforini"):
        if path == f"{prefix}/.zsh_history" or under(f"{prefix}/.zsh_sessions"):
            return "shell history"
    if under(f"{home_dir}/.gmail-mcp-epoch/credentials", "/Users/pablostafforini/.gmail-mcp-epoch/credentials"):
        return "Gmail MCP credentials"
    if _case(path, _glob("*", "/op-automations/cache"), _glob("*", "/op-automations/cache/", "*"),
             _glob("*", "/op-automations"), _glob("*", "/.local/state"), _glob("*", "/.local")):
        return "1Password read cache"
    if _case(path, _glob(".env.op"), _glob("*", "/.env.op"), _glob(".env", "*", ".example"),
             _glob("*", "/.env", "*", ".example")):
        return ""
    if _case(path, _glob(".env"), _glob("*", "/.env"), _glob(".env.", "*"), _glob("*", "/.env.", "*"),
             _glob(".envrc"), _glob("*", "/.envrc")):
        return "environment secrets file"
    if _case(path, _glob(".mcp.json"), _glob("*", "/.mcp.json"), _glob("mcp.json"), _glob("*", "/mcp.json")):
        return "MCP credential config"
    if _case(path, _glob("*", ".zshenv-secrets")):
        return "shell secrets file"
    if _case(path, _glob("*", "/.config/", "*", "/tokens.json"), _glob("*", "/.config/", "*", "/secret.json"),
             _glob("*", "/.config/", "*", "/client_secret", "*", ".json")):
        return "OAuth credentials"
    if _case(path, _glob("*", "/credentials.json"), _glob("*", "/service-account", "*", ".json"),
             _glob("*", "/tokens.json")):
        return "credential JSON"
    return ""


_SENSITIVE_GLOB_PARTS = (".env", ".envrc", ".mcp.json", "mcp.json", ".zshenv-secrets", ".zsh_history",
                         ".zsh_sessions", ".gnupg", "id_", "credentials.json", "service-account", "tokens.json",
                         "secret.json", "client_secret")


def _bash_pwd(call: Call) -> str:
    """`$PWD` as bash set it at startup: the inherited PWD when it names the
    current directory, else the physical path."""
    pwd = call.env.get("PWD", "")
    try:
        if pwd.startswith("/") and os.path.samefile(pwd, "."):
            return pwd
    except OSError:
        pass
    return os.getcwd()


def _claude_grep(call: Call, home_dir: str) -> Result:
    fields = call.tool_input
    # Filename/count-only modes do not return matched file contents.
    mode = command_substitution(jq_raw(jq_alternative(fields.get("output_mode"), "files_with_matches")))
    if mode in ("files_with_matches", "count"):
        return Result.allow()
    grep_path = (claude_field(call, "path") or command_substitution(jq_text(call.raw.get("cwd")))
                 or _bash_pwd(call))
    pattern_glob = claude_field(call, "glob")
    label = _scope_label(expand_tilde(grep_path, home_dir), home_dir)
    if label:
        return _deny(CLAUDE, label, f"Grep content output could return matching lines from {grep_path}")
    if any(part in pattern_glob for part in _SENSITIVE_GLOB_PARTS):
        return _deny(CLAUDE, "sensitive file glob",
                     f"Grep content output with glob {pattern_glob} could return secret values")
    return Result.allow()


# --- entry points -------------------------------------------------------------------

def _shell_result(runtime: str, command_text: str, home_dir: str) -> Result:
    label, detail, context = _check_command(runtime, command_text, home_dir)
    return _deny(runtime, label, detail) if label else Result(context=context)


def decide(call: Call) -> Result:
    name = tool_name(call)
    if call.runtime == CLAUDE:
        if name == "Read":
            path = claude_field(call, "file_path")
            if not path:
                return Result.allow()
            home_dir = home(call)
            label = _claude_read_label(expand_tilde(path, home_dir), home_dir)
            return _deny(CLAUDE, label, path) if label else Result.allow()
        if name == "Grep":
            return _claude_grep(call, home(call))
        if name == "Bash":
            command_text = command(call)
            return _shell_result(CLAUDE, command_text, home(call)) if command_text else Result.allow()
        return Result.allow()
    if name == "Read":
        path = codex_field(call, "file_path") or codex_field(call, "path")
        if not path:
            return Result.allow()
        label = label_for_text(call.runtime, expand_tilde(path, home(call)))
        return _deny(call.runtime, label, path) if label else Result.allow()
    if name in ("Bash", "exec_command", "functions.exec", "functions.exec_command"):
        command_text = command(call)
        return _shell_result(call.runtime, command_text, home(call)) if command_text else Result.allow()
    return Result.allow()


def dispatcher_check(call: Call) -> tuple[list[str], str]:
    """check_sensitive_read in pretooluse-bash.sh: (deny reasons, allow context).

    Identical to the Claude Bash branch except that its heredoc denial omits
    the advice paragraph.
    """
    label, detail, context = _check_command(CLAUDE, command(call), home(call))
    if not label:
        return [], context
    if detail == _HEREDOC_DETAIL:
        return [f"BLOCKED: sensitive read: {label} — {detail}."], ""
    return [_deny(CLAUDE, label, detail).reason], ""
