---
name: open-session-log
description: Open the current Claude Code session's log in Emacs via claude-log. Use when the user says "open session log", "view log", "show log", "open my log", or "view this session".
user-invocable: true
allowed-tools: Bash
---

# Open current session log in Emacs

Open this Claude Code session's conversation log in Emacs using the `claude-log` package.

When triggered, follow these steps exactly. Do NOT just describe what the skill does.

## Steps

1. **Find the session ID**: Run a bash command that walks up the process tree from `$PPID` to find the Claude Code PID (the one with a matching file in `~/.claude/sessions/`), then prints the session JSON:

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
   emacsclient --eval '(claude-log-open-session "SESSION_ID")'
   ```

   Replace `SESSION_ID` with the actual session ID from step 2.

4. Tell the user the log has been opened in Emacs.

If the session file cannot be found, tell the user and suggest running `M-x claude-log-open-latest` in Emacs as a fallback.

$ARGUMENTS
