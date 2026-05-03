#!/usr/bin/env bash
# PostToolUse hook: remind the agent to update the Codex counterpart after
# Claude configuration files change.

set -euo pipefail

"$HOME/My Drive/dotfiles/bin/ai-config-sync" remind-claude
