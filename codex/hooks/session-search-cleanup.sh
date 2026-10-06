#!/bin/bash
# SessionEnd: terminate the search processes (ugrep, bfs, rg, find, fd, grep)
# an ended Codex session left running, identified by the thread id in their
# environment (CODEX_THREAD_ID), never by name alone. The logic and its safety
# rules are in lib/python/session_search_cleanup.py.
# Registered in codex/hooks.json; Codex runs it only after the hook is trusted (/hooks).
#
# Cleanup must never hold up or fail the session: the module reports errors on
# stderr and exits 0, and a missing interpreter only loses the cleanup.
PY="$HOME/.pyenv/versions/3.11.9/bin/python3"
exec "$PY" -S -E -X pycache_prefix="$HOME/.cache/agent-guard/pycache" \
  "$HOME/My Drive/dotfiles/lib/python/session_search_cleanup.py" codex
