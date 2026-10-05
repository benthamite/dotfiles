#!/bin/bash
# PreToolUse entry for every Claude guard: one Python process running
# lib/python/agent_guard (docs/plans/2026-10-01-guard-engine.md).
#
# Claude Code treats any exit other than 2 as non-blocking, so every way the
# engine can fail must end in exit 2: a missing interpreter, a missing or
# broken library (an import error exits 1), a crash or a signal. The engine's
# main() returns only 0 or 2, so any other status is mapped to a denial.
#
# -S skips site-packages: the engine is stdlib-only, and an editable install's
# .pth finder there costs ~140 ms per start. -E ignores PYTHON* variables.
# Bytecode is cached outside Drive (no __pycache__ under the sync root),
# which saves ~30 ms of source compilation per call.
#
# One exception: a crash on a read-only tool (READ_ONLY_TOOLS) is allowed with
# a loud warning, so an agent can still read the guard to diagnose it
# (2026-10-01/02, when every tool was denied). The engine handles crashes it
# catches (agent_guard.engine.crash_warning) and exits 2 for everything it
# denies on purpose; this script handles the rest (no interpreter, an import
# error, a signal), reading the tool name from a single top-level-looking
# "tool_name" key. Anything less certain denies.
READ_ONLY_TOOLS='Read|Grep|Glob'
PY="$HOME/.pyenv/versions/3.11.9/bin/python3"
payload=$(cat)
printf '%s' "$payload" | "$PY" -S -E -X pycache_prefix="$HOME/.cache/agent-guard/pycache" \
  -c 'import sys; sys.path.insert(0, sys.argv[1]); from agent_guard.engine import main; sys.exit(main(sys.argv[2]))' \
  "$HOME/My Drive/dotfiles/lib/python" claude
status=$?
[ "$status" -eq 0 ] && exit 0
if [ "$status" -ne 2 ] && [ -n "$READ_ONLY_TOOLS" ]; then
  keys=$(printf '%s' "$payload" | /usr/bin/grep -oE '"tool_name"[[:space:]]*:' | /usr/bin/wc -l | /usr/bin/tr -d ' ')
  tool=$(printf '%s' "$payload" | /usr/bin/grep -oE '"tool_name"[[:space:]]*:[[:space:]]*"[A-Za-z]+"' | /usr/bin/sed -E 's/.*"([A-Za-z]+)"$/\1/')
  if [ "$keys" = 1 ] && [[ "$tool" =~ ^($READ_ONLY_TOOLS)$ ]]; then
    message="WARNING: agent-guard crashed (engine exit status $status). $tool was allowed WITHOUT its guard checks (sensitive-read, walk-list); Bash, Edit, Write and other tools stay blocked until the guard is fixed."
    printf '%s\n' "$message" >&2
    printf '{"systemMessage": "%s", "hookSpecificOutput": {"hookEventName": "PreToolUse", "additionalContext": "%s"}}\n' "$message" "$message"
    exit 0
  fi
fi
printf '%s\n' "Security hook failed; tool execution denied." >&2
exit 2
