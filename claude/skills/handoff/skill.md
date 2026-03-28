---
name: handoff
description: Save next-session instructions to a handoff file for seamless session transitions. Use when the user says "handoff", "continue next session", "save next steps", or at the end of a session when there are clear follow-up tasks.
---

# Handoff

Write a concrete, actionable prompt for the next session based on the
current conversation. Save it to `~/.claude/handoff.md`.

## Steps

1. **Summarize what needs to happen next.** Review the conversation for:
   - Explicitly agreed next steps
   - Unfinished tasks
   - Pending findings or issues to address
   - Design decisions to implement

2. **Write a prompt** that a fresh Claude session can execute immediately.
   The prompt should:
   - Start with "Continue from previous session (DATE)."
   - List tasks in priority order
   - Include specific file paths, command names, and commit hashes
   - Be self-contained — the next session reads CLAUDE.md and the
     session log, but shouldn't need to reconstruct context from
     a conversation it doesn't have access to

3. **Save to `~/.claude/handoff.md`**, overwriting any previous handoff.

4. **Print the contents** so the user can review before closing.

5. **Tell the user** they can start the next session with:
   ```
   claude -p "$(cat ~/.claude/handoff.md)"
   ```
