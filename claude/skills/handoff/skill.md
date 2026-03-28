---
name: handoff
description: Save next-session instructions to a handoff file for seamless session transitions. Use when the user says "handoff", "continue next session", "save next steps", or at the end of a session when there are clear follow-up tasks.
---

# Handoff

Write a concrete, actionable prompt for the next session and save it
to `~/.claude/handoff.md`.

## Determining the prompt

There are two paths:

### Path 1: User specifies the prompt

If the user told you what the next session should do (via arguments or
in conversation), write a prompt that **does that thing**. You may
rephrase for clarity, but the prompt must be a faithful representation
of the user's intent — if they asked for a skill invocation, the
prompt must invoke that skill; if they asked for a task, the prompt
must perform that task. Do not replace actions with background context.
No confirmation needed.

### Path 2: Inference

If there was no explicit discussion of next steps, review the
conversation history and infer the most important follow-up tasks.
Then **present the proposed prompt to the user for confirmation**
using `AskUserQuestion` before writing the file. The user may edit,
reorder, or reject items.

When drafting an inferred prompt:
- Start with "Continue from previous session (DATE)."
- List tasks in priority order
- Include specific file paths, command names, and commit hashes
- Be self-contained — the next session reads CLAUDE.md and the
  session log, but shouldn't need to reconstruct context from a
  conversation it doesn't have access to

## Steps

1. Determine whether path 1 or path 2 applies.
2. If path 1, write the user's prompt verbatim.
   If path 2, draft a prompt, then confirm with the user.
3. Save to `/tmp/claude-code-handoff.md`, overwriting any previous handoff.
4. Print the contents so the user can review.
5. Tell the user to run `M-x claude-code-extras-handoff` to close
   this session and start a new one with the prompt auto-submitted.
