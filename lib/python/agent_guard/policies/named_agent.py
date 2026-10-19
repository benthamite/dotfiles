"""Agent spawned with a `name`: Claude only.

Claude Code turns an Agent call that carries `name` into an agent-team
teammate. A teammate does not end when its task is done: it stays listed as
an idle background agent until the lead stops it, and the Stop hook
block-done-over-running-tasks.py cannot see it (an idle teammate is reported
as running, so teammates are excluded there). An Agent call without `name`
is a plain subagent: it is listed as completed when it returns and needs no
cleanup, and SendMessage can still reach it by its agentId.

On 2026-10-18 a tangodb session launched 28 named case workers; all finished,
and all 28 stayed listed as background agents until the owner asked why.
Every Agent call with a non-empty `name` is therefore denied, with advice to
drop the name.
"""

from __future__ import annotations

from ..core import CLAUDE, Call, Result

REASON = ("BLOCKED: do not pass `name` to the Agent tool. A named agent becomes a teammate that stays listed "
          "as an idle background agent after it finishes, until it is stopped by hand. Launch it without `name`: "
          "an unnamed subagent is listed as completed when it returns, and SendMessage can still reach it by "
          "the agentId in the spawn result.")


def decide(call: Call) -> Result:
    if call.runtime != CLAUDE or call.tool_name != "Agent":
        return Result.allow()
    name = call.tool_input.get("name")
    if isinstance(name, str) and name.strip():
        return Result.deny(REASON)
    return Result.allow()
