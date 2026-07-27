#!/bin/bash
# Forward Claude Code session-state events to Emacs via claude-code-event-hook.
#
# Usage: notify-emacs-state.sh activity|blocked
#
#   activity  the session is working. Wired to high-frequency events
#             (PreToolUse, MessageDisplay, UserPromptSubmit), so it is
#             debounced: only one emacsclient call per DEBOUNCE_SECONDS per
#             session. Emacs only needs one activity event per turn, since
#             later ones are no-ops once the session is already busy. A turn
#             starting within the window is reported up to that many seconds
#             late, then corrects itself.
#
#   blocked   the session is waiting on the user and will not proceed alone.
#             Wired to SessionStart (a fresh session awaits its first prompt)
#             and StopFailure (the turn died, so nothing more is coming).
#
# Intended to run through fire-and-forget.sh so the CLI never blocks on
# emacsclient. Claude Code sets CLAUDE_BUFFER_NAME when claude-code.el
# launches the session; without it there is no buffer to address.
mode=${1:-}
buf=${CLAUDE_BUFFER_NAME:-}
[[ -n "$mode" && -n "$buf" ]] || exit 0

DEBOUNCE_SECONDS=10

if [[ "$mode" == "activity" ]]; then
  dir="${TMPDIR:-/tmp}/claude-agent-activity"
  mkdir -p "$dir" 2>/dev/null || exit 0
  stamp="$dir/$(printf '%s' "$buf" | shasum | cut -d' ' -f1)"
  if [[ -f "$stamp" ]]; then
    now=$(date +%s)
    then=$(stat -f %m "$stamp" 2>/dev/null || echo 0)
    (( now - then < DEBOUNCE_SECONDS )) && exit 0
  fi
  touch "$stamp" 2>/dev/null
fi

# Escape backslashes and double quotes so the value is safe inside an Elisp
# string; session buffer names contain directory paths.
esc=${buf//\\/\\\\}
esc=${esc//\"/\\\"}
emacsclient -e "(claude-code-handle-hook '${mode} \"${esc}\")" >/dev/null 2>&1 || true
