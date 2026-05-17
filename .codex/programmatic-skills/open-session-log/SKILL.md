---
name: open-session-log
description: Open the current Claude Code or Codex session log in Emacs via agent-log. Programmatic-only helper for explicit log-opening automation; not loaded in ordinary interactive sessions.
---

# Open current session log in Emacs

Open the current agent session's conversation log in Emacs using the `agent-log`
package. Use the branch for the current runtime. Do not open the latest session
as a fallback; concurrent sessions can make that wrong.

## Claude Code

1. Find the session ID by walking up the process tree from `$PPID` to find the
   Claude Code PID with a matching file in `~/.claude/sessions/`:

   ```bash
   pid=$PPID
   while [ "$pid" -gt 1 ] && [ ! -f "$HOME/.claude/sessions/${pid}.json" ]; do
     pid=$(ps -o ppid= -p "$pid" 2>/dev/null | tr -d ' ')
   done
   cat "$HOME/.claude/sessions/${pid}.json" 2>/dev/null || echo "NOT FOUND"
   ```

2. Extract the `sessionId` field from the JSON output.
3. Open it in Emacs:

   ```bash
   emacsclient --eval '(agent-log-open-session "SESSION_ID")'
   ```

If the session file cannot be found, report that and suggest running
`M-x agent-log-open-current-session` from the Claude Code buffer in Emacs.

## Codex

1. Emit a unique marker in its own shell command, then keep the printed value:

   ```bash
   marker="codex-open-session-log-$(date +%s)-$$-$RANDOM"
   printf '%s\n' "$marker"
   ```

2. Search recent Codex session logs for that exact marker:

   ```bash
   marker='PASTE_MARKER_HERE'
   file=$(
     find "$HOME/.codex/sessions" -type f -name '*.jsonl' -mmin -60 -print0 |
       xargs -0 rg -l --fixed-strings "$marker" 2>/dev/null |
       head -n 1
   )
   if [ -z "$file" ]; then
     echo "NOT FOUND"
   else
     python3 -c 'import json, sys; handle = open(sys.argv[1], encoding="utf-8"); print(json.loads(handle.readline()).get("payload", {}).get("id") or "NOT FOUND")' "$file"
   fi
   ```

3. Open the resulting session ID in Emacs:

   ```bash
   emacsclient --eval '(agent-log-open-session "SESSION_ID")'
   ```

If marker lookup fails, report that and suggest running
`M-x agent-log-open-current-session` from the Codex buffer in Emacs.

## Reporting

If `emacsclient` exits successfully, say the log was opened in Emacs. If it
errors, report the error instead of claiming success.
