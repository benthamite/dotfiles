"""block-retired-secrets-cli.py: deny new references to the retired secrets CLI.

The policy already lives in lib/python/retired_secrets_cli.py (shared by both
runtimes); this adapts it to the engine and runs it in-process.
"""

from __future__ import annotations

import retired_secrets_cli

from ..core import Call, HookFailure, Result


def decide(call: Call) -> Result:
    try:
        reason = retired_secrets_cli.decide(call.raw)
    except Exception as error:  # noqa: BLE001 - the hook denied when it could not decide
        raise HookFailure("retired secrets-CLI check failed") from error
    return Result.deny(reason) if reason else Result.allow()
