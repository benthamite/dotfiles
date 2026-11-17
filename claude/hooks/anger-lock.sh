#!/bin/bash
# UserPromptSubmit: lock every agent session for a while when Pablo writes in
# anger (swearing, exasperation, abuse). The logic, classifier and lock are in
# lib/python/anger_lock.py; the lock file is shared by Claude and Codex.
PY="$HOME/.pyenv/versions/3.11.9/bin/python3"
exec "$PY" -S -E -X pycache_prefix="$HOME/.cache/agent-guard/pycache" \
  "$HOME/repos/dotfiles/lib/python/anger_lock.py" claude
