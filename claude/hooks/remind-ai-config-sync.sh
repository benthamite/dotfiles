#!/usr/bin/env bash
# PostToolUse hook: remind the agent to update the Codex counterpart after
# Claude configuration files change.

set -euo pipefail

"$HOME/repos/dotfiles/bin/ai-config-sync" remind-claude
