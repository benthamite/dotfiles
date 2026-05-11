---
name: session-learning-capture
description: Review the current session when a Stop/session-end hook asks for lesson capture, then write candidate reusable lessons to the central dotfiles .agent-learnings/inbox without implementing them. Use when a hook prompt says to capture session lessons, review current-session learnings, or populate the agent-learning inbox.
---

# Session Learning Capture

Use this skill as the automatic first stage of the agent-learning workflow. It
reviews the current session and writes candidate lessons to
`~/My Drive/dotfiles/.agent-learnings/inbox/`. It does not decide whether
lessons should become durable instructions, hooks, skills, docs, or code.

The separate `session-retro` skill owns the manual second stage: interviewing
the user about inbox items and implementing accepted changes.

## Scope

Run from any working directory. If the hook prompt contains `transcript_path`,
read that transcript when available. If the transcript is unavailable, use the
current conversation context and say that the capture was context-only.

Do not write a file when there are no useful lessons. A useful lesson is a
reusable observation about agent behavior, missing automation, unclear skill
triggers, brittle verification, recurring user correction, hook/tool friction,
or documentation drift. Ordinary task summaries, one-off facts, and completed
implementation details are not lessons.

## Output Location

Write one Markdown file per captured session in the central dotfiles inbox:

```text
~/My Drive/dotfiles/.agent-learnings/inbox/YYYY-MM-DD-TOOL-SESSIONID.md
```

Use `codex`, `claude`, or `agent` for `TOOL` based on the hook prompt or
transcript path. Shorten `SESSIONID` to a readable prefix when needed. The files
are ignored local working material; do not commit them unless the user
explicitly asks.

## Record Format

Use this format for each inbox file:

```markdown
# Session learning candidates: YYYY-MM-DD TOOL SESSIONID

Source transcript: PATH-OR-UNAVAILABLE
Working directory: PATH
Captured: YYYY-MM-DD

## Candidate 1: Short title

**Summary:** What should be improved or remembered.

**Why it matters:** The failure, friction, or repeated correction this would
prevent.

**Possible target:** One of `instruction`, `skill`, `hook`, `script`, `docs`,
`test`, `decision`, or `unknown`.

**Evidence:** Brief reference to the session moment, command, error, or user
correction. Do not quote secrets or sensitive content.

**Risk / uncertainty:** Anything that should make the later `session-retro`
review cautious.
```

Add multiple candidates only when they are distinct actionable ideas.

## Workflow

1. Identify the session metadata from the hook prompt: `session_id`,
   `transcript_path`, `cwd`, and tool if present.
2. Read the transcript if available. Keep the review targeted; do not summarize
   every turn.
3. Extract only reusable candidate lessons. Prefer fewer, sharper candidates
   over broad self-criticism.
4. Redact secrets, credentials, private tokens, personal data, and raw command
   output that is not needed as evidence.
5. If there are no useful lessons, do not create an inbox file. Report that no
   lesson candidates were captured.
6. If there are useful lessons, create the inbox Markdown file. Do not modify
   `AGENTS.md`, `CLAUDE.md`, skills, hooks, READMEs, decisions, or code.
7. Report the inbox path and the candidate titles. Tell the user that
   `session-retro` processes the inbox later.

## Safety

Never implement, promote, archive, or delete learning candidates in this skill.
Never create externally visible actions. This skill only creates local inbox
records for later user review.
