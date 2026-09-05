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
   Claude Code PID with a matching file in the active Claude configuration
   root (`CLAUDE_CONFIG_DIR`, or `~/.claude` when unset):

   ```bash
   session_root="${CLAUDE_CONFIG_DIR:-$HOME/.claude}/sessions"
   session_pid=$PPID
   while [ -n "$session_pid" ] && [ "$session_pid" -gt 1 ] && [ ! -f "$session_root/${session_pid}.json" ]; do
     session_pid=$(ps -o ppid= -p "$session_pid" 2>/dev/null | tr -d ' ')
   done
   if [ -n "$session_pid" ] && [ -f "$session_root/${session_pid}.json" ]; then
     jq -er '.sessionId // .session_id // empty' "$session_root/${session_pid}.json"
   else
     echo "NOT FOUND"
   fi
   ```

2. Keep the non-empty session ID returned by the command.
3. Resolve and open its transcript using the shared procedure below.

If the process metadata is unavailable, inspect the live Emacs agent buffers
for an exact runtime identity. Call `agent-log-open-current-session` in that
buffer only when the identity matches; a matching project directory alone is
insufficient. Report an unresolved identity if neither source establishes it.

## Codex

1. Read `CODEX_THREAD_ID` from the current tool environment. When present,
   use that identity and proceed to transcript resolution; no marker search
   is needed.
2. If the thread ID is absent, inspect the live Emacs agent buffer metadata for
   an exact process/session match. Do not choose a buffer by project alone.
3. Only if neither source supplies the identity, emit a unique marker in its
   own shell command, then keep the printed value:

   ```bash
   marker="codex-open-session-log-$(date +%s)-$$-$RANDOM"
   printf '%s\n' "$marker"
   ```

4. Search the active Codex root (`CODEX_HOME`, or `~/.codex` when unset),
   including `sessions/` and `archived_sessions/`, for the exact emitted
   marker. Parse matching JSONL records and require the marker in the tool's
   output, not merely a quoted command. Read the owning `session_meta` ID;
   accept only one distinct session ID. If writes are delayed, retry briefly
   with a bounded wait. Never take the first file from multiple matches.
5. Resolve and open its transcript using the shared procedure below.

If lookup fails or remains ambiguous, report the unresolved identity.

## Resolve and open the transcript

Resolve the established session ID to a transcript in the runtime's active
configuration root. Prefer the exact transcript path from process or buffer
metadata when available. Otherwise, search that root's Claude `projects/` or
Codex `sessions/` and `archived_sessions/` paths. Validate candidate contents
against the established session ID; do not rely on a filename alone.

Open the verified absolute path with `agent-log-open-file`. This avoids an
Emacs account selection resolving an ID under a different configuration root.
Quote the path as an Elisp string when constructing the expression:

```bash
emacsclient --eval '(agent-log-open-file "/absolute/path/to/transcript.jsonl")'
```

## Reporting

After opening, inspect the rendered buffer's `agent-log--session-id` and
`agent-log--source-file` and confirm they identify the requested session.
Report success only after that check; an IPC exit status alone does not prove
the right conversation was opened.
If the displayed identity differs, recheck the exact transcript and retry
once via `agent-log-open-file`; report any remaining mismatch without claiming
success.
