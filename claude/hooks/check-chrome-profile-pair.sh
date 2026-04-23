#!/bin/bash
# SessionStart hook: placeholder (no-op).
#
# The original check warned when the account's .claude.json lacked
# `chromeExtension.pairedDeviceId`, assuming that field was required to
# drive the Chrome extension. It isn't. Claude Code's Chrome integration
# routes through `wss://bridge.claudeusercontent.com`, which matches a
# running CLI session to an extension instance by shared Anthropic
# identity; no per-CLI pair record is needed. Confirmed empirically on
# 2026-04-23: a personal CLI session with `chromeExtension: null` drove
# the Default-profile extension successfully (navigate + read_page),
# and the field stayed null after the run.
#
# The field still shows up in some accounts' .claude.json (e.g. epoch's
# `"Epoch"` device) as a leftover from an older CLI flow, but nothing
# current reads it. The hook was firing false positives on every
# personal/tlon session start, so the body is gone.
#
# Left as a placeholder in case Anthropic reintroduces a pair state
# worth surfacing. For the account ↔ Chrome-profile mapping see
# `claude/README.org` § "Chrome integration and multi-account".

set -euo pipefail

# Drain stdin so Claude Code doesn't block on a broken pipe.
cat >/dev/null || true

exit 0
