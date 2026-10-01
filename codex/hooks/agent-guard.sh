#!/bin/bash
# PreToolUse entry for every Codex guard: one Python process running
# lib/python/agent_guard (docs/plans/2026-10-01-guard-engine.md).
#
# Codex treats any exit other than 2 as non-blocking, so every way the
# engine can fail must end in exit 2: a missing interpreter, a missing or
# broken library (an import error exits 1), a crash or a signal. The engine's
# main() returns only 0 or 2, so any other status is mapped to a denial.
#
# -S skips site-packages: the engine is stdlib-only, and an editable install's
# .pth finder there costs ~140 ms per start. -E ignores PYTHON* variables.
# Bytecode is cached outside Drive (no __pycache__ under the sync root),
# which saves ~30 ms of source compilation per call.
PY="$HOME/.pyenv/versions/3.11.9/bin/python3"
"$PY" -S -E -X pycache_prefix="$HOME/.cache/agent-guard/pycache" \
  -c 'import sys; sys.path.insert(0, sys.argv[1]); from agent_guard.engine import main; sys.exit(main(sys.argv[2]))' \
  "$HOME/My Drive/dotfiles/lib/python" codex
status=$?
[ "$status" -eq 0 ] && exit 0
printf '%s\n' "Security hook failed; tool execution denied." >&2
exit 2
