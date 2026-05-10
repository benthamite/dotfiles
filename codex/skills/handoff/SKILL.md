---
name: handoff
description: Save next-session instructions to `/tmp/codex-handoff.md` for Codex session transitions. Use when the user says "handoff", "continue next session", "save next steps", "prepare a resume prompt", or when a session has clear follow-up tasks that should resume in a fresh Codex session.
---

# Handoff

Write a concrete, actionable prompt for the next session and save it
to `/tmp/codex-handoff.md`. The Emacs command
`ai-agent-codex-handoff` reads this file, closes the current Codex
session, and starts a new one with the handoff prompt.

Do not use this skill for Codex App Browser Use handoffs; use
`codex-app-browser` for that workflow. If the user explicitly names a
project-specific closeout skill, use that skill instead.

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
**Output the full proposed prompt as a markdown code block** so the
user can read it, then ask for explicit yes/no confirmation before
saving. If a structured user-input tool is available, use it only for
short option labels; never put the prompt text inside labels,
annotations, or option descriptions. Always print the prompt as regular
text output before asking for confirmation. The user may ask you to
edit, reorder, or reject items before you save the file.

When drafting an inferred prompt:
- Start with "Continue from previous session (DATE)." Use the exact
  current date when it is available.
- List tasks in priority order
- Include specific file paths, command names, commit hashes, and
  verification state
- Be self-contained. The next session reads `AGENTS.md` and any
  project instructions or session logs it chooses to inspect, but
  should not need to reconstruct context from a conversation it cannot
  access.

## Steps

1. Determine whether path 1 or path 2 applies.
2. If path 1, write the user's requested next-session prompt directly,
   rephrasing only for clarity.
   If path 2, draft a prompt, then confirm with the user.
3. Save to `/tmp/codex-handoff.md`, overwriting any previous handoff.
   Use the available file-writing mechanism. If it refuses to overwrite
   the existing handoff, remove that specific temp file first with
   `rm -f /tmp/codex-handoff.md`, then write the new prompt.
4. Re-read `/tmp/codex-handoff.md` and verify that it matches the
   intended prompt.
5. Print the contents so the user can review.
6. Tell the user to run `! emacsclient -e '(ai-agent-codex-handoff)'`
   to close this session and start a new one with the prompt auto-submitted.
   Do NOT run this command yourself — only the user should trigger it.
