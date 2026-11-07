#!/usr/bin/env bash
# PostToolUse hook: remind the agent to update the Claude counterpart after
# Codex configuration files change.

set -euo pipefail

"$HOME/repos/dotfiles/bin/ai-config-sync" remind-codex
