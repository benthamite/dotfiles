"""Claude's Bash pre-check: the policies claude/hooks/pretooluse-bash.sh ran.

Order, gating and reason joining are the dispatcher's, so the combined
decision for a Bash call is unchanged: secret leak and untrusted execution on
every command, the inline checks, the GitHub-write and Elpaca/pipeline checks
behind the same command-word gates, and output wrapping last. Former
"delegated" hooks are called in-process; a policy failure is reported with the
same "failed with exit status 2" reason the dispatcher gave a crashed hook.
"""

from __future__ import annotations

from ..core import HOME, Call, HookFailure, Result
from ..ere import ere, grep
from . import (ahrefs, destructive, elpaca_rebuild, github_write, hookspath, long_pipeline, secret_leak,
               sensitive_read, untrusted_execution, walk_list)

GIT_OR_GH = ere(r"""(^|[[:space:];|&("'])("[^"]*/(git|gh)"|'[^']*/(git|gh)'|[^[:space:];|&]*/(git|gh)|git|gh)([[:space:]]|$)""")
EMACSCLIENT = ere(r"\bemacsclient\b")
ELPACA = ere(r"elpaca-rebuild|elpaca-extras-reload")
PIPELINE = (ere(r"(python3?|nohup)[^|]*setup_db\.py[^|]*(--full|--target)"),
            ere(r"(bash|sh|nohup|\./|source )[^|]*refresh_data\.sh"))

# wrap_needs_redaction: commands whose output may carry credentials.
NEEDS_REDACTION = tuple(ere(p) for p in (
    r"(^|[[:space:];|&(])(pass|op|security|git-crypt)[[:space:]]",
    r"gh[[:space:]]+auth[[:space:]]+token|gcloud[[:space:]]+auth[[:space:]]+print|aws[[:space:]]+sts[[:space:]]"
    r"|aws[[:space:]]+secretsmanager|kubectl[[:space:]]+get[[:space:]]+secret",
    r"(^|[[:space:];|&(])(env|printenv)([[:space:];|&)]|$)",
    r"(^|[[:space:];|&(])export[[:space:]]+-p([[:space:];|&)]|$)",
    r"(^|[[:space:];|&(])(declare|typeset)([[:space:];|&)]|$)",
    r"(^|[[:space:];|&(])(which|where|whence|type|functions)([[:space:];|&)]|$)",
    r"[$][{]?([(][a-zA-Z@]*[)])?(dis_)?functions",
    r"(^|[[:space:];|&(])set[[:space:]]*([;&|)]|$)",
    r"\.zshenv-secrets|\.env\b|\.envrc\b|\.netrc\b|\.npmrc\b|\.pypirc\b|credentials|tokens\.json|keychain|\.pem\b"
    r"|\.ssh/|\.gnupg/|_TOKEN|_SECRET|API_KEY|APIKEY|PASSWORD",
))
HEREDOC = ere(r"""<<-?[[:space:]]*[A-Za-z_'"]""")
BACKGROUND = ere(r"&[[:space:]]*$")
REDACTOR = str(HOME / "My Drive/dotfiles/claude/hooks/redact-secrets.sh")


def _delegate(name: str, policy, call: Call, reasons: list[str]) -> None:
    try:
        result = policy.decide(call)
    except HookFailure as failure:
        reasons.append(f"BLOCKED: delegated guard {name} failed with exit status {failure.status}; "
                       "its policy check did not complete.")
        return
    if result.decision == "deny":
        reasons.append(result.reason or f"BLOCKED: delegated guard {name} denied the command without a reason.")


def wrapped(command: str) -> str | None:
    """check_wrap: route credential-prone output through redact-secrets.sh."""
    if "redact-secrets.sh" in command:
        return None
    if not any(grep(p, command) for p in NEEDS_REDACTION):
        return None
    if grep(HEREDOC, command) or grep(BACKGROUND, command):
        return None
    # zsh (the agents' shell here) has no PIPESTATUS, so the shell hook's
    # `exit "${PIPESTATUS[0]}"` exited 0 and reported every failing wrapped
    # command as a success. zsh's 1-based `pipestatus` first, then bash's.
    return '{ ' + command + '; } 2>&1 | "' + REDACTOR + '"; exit "${pipestatus[1]:-${PIPESTATUS[0]}}"'


def decide(call: Call) -> Result:
    command = call.shell_command()
    if call.tool_name != "Bash" or not command:
        return Result.allow()
    reasons: list[str] = []
    _delegate("block-secret-leak.sh", secret_leak, call, reasons)
    _delegate("block-untrusted-execution.sh", untrusted_execution, call, reasons)
    reasons += ahrefs.dispatcher_check(call)
    if grep(GIT_OR_GH, command, echo=True):
        _delegate("block-github-write-command.sh", github_write, call, reasons)
    sensitive_reasons, context = sensitive_read.dispatcher_check(call)
    reasons += sensitive_reasons
    reasons += destructive.dispatcher_check(call)
    reasons += walk_list.dispatcher_check(call)
    reasons += hookspath.dispatcher_check(call)
    if grep(EMACSCLIENT, command, echo=True) and grep(ELPACA, command, echo=True):
        _delegate("block-elpaca-rebuild-uncommitted.sh", elpaca_rebuild, call, reasons)
    if any(grep(p, command, echo=True) for p in PIPELINE):
        _delegate("block-long-pipeline-run-unchecked.sh", long_pipeline, call, reasons)
    new_command = wrapped(command)
    if reasons:
        return Result.deny("\n\n".join(reasons))
    updated = None
    if new_command is not None:
        updated = dict(call.tool_input)
        updated["command"] = new_command
    return Result(decision="allow", updated_input=updated, context=context)
