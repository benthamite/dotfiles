---
name: open-session-log
description: Open the current Codex session's log in Emacs via agent-log. Use when the user says "open session log", "view log", "show log", "open my log", "view this session", or asks to inspect the current Codex thread.
---

# Open current Codex session log in Emacs

Open this Codex session's conversation log in Emacs using the `agent-log` package.

When triggered, do the work; do not merely describe what the skill does. This skill accepts no arguments.

## Steps

1. **Emit a unique marker** in its own shell command, then keep the printed value for the next step:

   ```bash
   marker="codex-open-session-log-$(date +%s)-$$-$RANDOM"
   printf '%s\n' "$marker"
   ```

2. **Find the session ID** by searching recent Codex session logs for that marker. Replace `PASTE_MARKER_HERE` with the exact marker printed in step 1:

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

3. **Open in Emacs** via emacsclient:

   ```bash
   emacsclient --eval '(agent-log-open-session "SESSION_ID")'
   ```

   Replace `SESSION_ID` with the actual session ID from step 2.

4. If `emacsclient` exits successfully, tell the user the log has been opened in Emacs. If it errors, report the error instead of claiming success.

Do not open the latest session as a fallback; concurrent Codex agents can make that wrong. If the marker lookup fails, tell the user and suggest running `M-x agent-log-open-current-session` from the Codex buffer in Emacs.
