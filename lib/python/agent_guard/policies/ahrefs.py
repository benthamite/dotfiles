"""Raw Ahrefs API calls: block-unguarded-ahrefs-api.sh (both runtimes) and
check_ahrefs in claude/hooks/pretooluse-bash.sh.

Even read-only Ahrefs calls spend a monthly unit budget shared across Epoch
automations, so paid requests must go through `ahrefs-api-guard`, which checks
the free quota endpoint first. That endpoint may be called directly.
"""

from __future__ import annotations

from ..core import CLAUDE, CODEX_SHELL_TOOLS, Call, Result
from ..ere import ere, grep
from ..shell_text import mask_git_commit_messages, mask_heredoc_bodies
from .sensitive_read import command, tool_name

REASON = ("BLOCKED: raw Ahrefs API call detected. Ahrefs API units are shared across Epoch automations. Use "
          "`ahrefs-api-guard request ...` so the free quota probe runs and blocks the call if remaining units are "
          "below the required reserve.")
_HOST = ere(r"api\.ahrefs\.com")
_GUARD = ere(r"(^|[[:space:]/])ahrefs-api-guard([[:space:]]|$)")
_QUOTA = ere(r"subscription-info/limits-and-usage")
_PAID = ere(r"site-explorer|site-audit|keywords-explorer|rank-tracker|web-analytics|brand-radar|gsc"
                   r"|serp-overview|batch-analysis|public-crawler|management")


def _raw_call(command_text: str) -> bool:
    """A command naming the API host outside a commit message or a sink-fed
    heredoc (prose), other than through the guard or the free quota probe."""
    if not grep(_HOST, mask_git_commit_messages(mask_heredoc_bodies(command_text)), echo=True):
        return False
    if grep(_GUARD, command_text, echo=True):
        return False
    return not (grep(_QUOTA, command_text, echo=True) and not grep(_PAID, command_text, echo=True))


def decide(call: Call) -> Result:
    name = tool_name(call)
    if name not in (("Bash",) if call.runtime == CLAUDE else CODEX_SHELL_TOOLS):
        return Result.allow()
    command_text = command(call)
    return Result.deny(REASON) if command_text and _raw_call(command_text) else Result.allow()


def dispatcher_check(call: Call) -> list[str]:
    """check_ahrefs in pretooluse-bash.sh: the Claude hook verbatim."""
    return [REASON] if _raw_call(command(call)) else []
