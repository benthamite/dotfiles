---
name: open-session-log
description: Open the current Claude Code session's log in Emacs via agent-log. Use when the user says "open session log", "view log", "show log", "open my log", "view this session", or asks to inspect the current Claude Code thread.
user-invocable: true
allowed-tools: Bash
---

# Open current session log in Emacs

Open this Claude Code session's conversation log in Emacs using the `agent-log` package.

When triggered, do the work; do not merely describe what the skill does. This skill accepts no arguments.

## Steps

1. **Find the session ID**: Run a shell command that walks up the process tree from `$PPID` to find the Claude Code PID (the one with a matching file in `~/.claude/sessions/`), then prints the session JSON:

   ```bash
   pid=$PPID
   while [ "$pid" -gt 1 ] && [ ! -f "$HOME/.claude/sessions/${pid}.json" ]; do
     pid=$(ps -o ppid= -p "$pid" 2>/dev/null | tr -d ' ')
   done
   cat "$HOME/.claude/sessions/${pid}.json" 2>/dev/null || echo "NOT FOUND"
   ```

2. **Extract the session ID** from the `sessionId` field in the JSON output.

3. **Open in Emacs** via emacsclient:

   ```bash
   emacsclient --eval '(agent-log-open-session "SESSION_ID")'
   ```

   Replace `SESSION_ID` with the actual session ID from step 2.

4. If `emacsclient` exits successfully, tell the user the log has been opened in Emacs. If it errors, report the error instead of claiming success.

Do not open the latest session as a fallback; concurrent Claude Code sessions can make that wrong. If the session file cannot be found, tell the user and suggest running `M-x agent-log-open-current-session` from the Claude Code buffer in Emacs.
