#!/usr/bin/env bash
# PreToolUse hook: block commits that change only one side of the
# Claude Code <-> Codex configuration port.

set -euo pipefail

"$HOME/My Drive/dotfiles/bin/ai-config-sync" guard-commit
