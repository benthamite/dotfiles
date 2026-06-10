---
name: session-learning-capture
description: Review the current session when a Stop/session-end hook asks for lesson capture, then write candidate reusable lessons and promotion metadata to the central dotfiles .agent-learnings/inbox without implementing them. Use when a hook prompt says to capture session lessons, review current-session learnings, populate the agent-learning inbox, or preserve reusable agent self-improvement ideas.
---

# Session Learning Capture

Use this skill as the automatic first stage of the agent-learning workflow. It
reviews the current session and writes candidate lessons to
`~/My Drive/dotfiles/.agent-learnings/inbox/`. It does not decide whether
lessons should become durable instructions, hooks, skills, docs, or code.

The separate `session-retro` skill owns the manual second stage: interviewing
the user about inbox items and implementing accepted changes.

This skill borrows the useful parts of Hermes-style self-improvement while
keeping this repository's inbox-first safety boundary. Captured files are
pending proposals, not durable memory. They should preserve provenance, the
likely target artifact, and enough curation metadata for a later review pass to
merge duplicates, reject stale advice, or turn a high-confidence lesson into a
staged patch.

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

**Origin / trigger:** Hook, manual request, background review, or other source;
include why this candidate surfaced.

**Loaded or relevant skills:** `skill-a`, `skill-b`, or `none observed`.

**Summary:** What should be improved or remembered.

**Why it matters:** The failure, friction, or repeated correction this would
prevent.

**Value:** NN/100

**Implementation safety:** NN/100

**Proposed action:** One of `remember`, `patch-skill`, `create-skill`,
`add-reference`, `hook`, `script`, `docs`, `test`, `decision`, or `unknown`.

**Target artifact:** Path, skill name, hook name, document, or `unknown`.

**Autonomy level:** One of `propose-only`, `staged-diff-ok`, or
`interview-required`.

**Curation hints:** Duplicate, merge, class-level umbrella, stale-risk, or
other notes for the later review pass.

**Evidence:** Brief reference to the session moment, command, error, or user
correction. Do not quote secrets or sensitive content.

**Risk / uncertainty:** Anything that should make the later `session-retro`
review cautious.

**Proposed patch:** Optional inert fenced diff or concise edit sketch. Include
only when the change is narrow and high-confidence; do not apply it.
```

Set `Value` to an integer from 0 to 100 estimating the importance of later
implementing this candidate. Use the full range: 90-100 for changes that
prevent severe or recurring failures across many sessions, 70-89 for high-value
workflow or safety improvements, 40-69 for useful but narrower improvements,
10-39 for minor polish or one-off friction, and 0-9 for candidates that barely
clear the usefulness bar. Add multiple candidates only when they are distinct
actionable ideas.

Set `Implementation safety` to an integer from 0 to 100 estimating how safe the
candidate would be for later `session-retro` automation, not how valuable it is.
Use 90-100 only for local, tracked, clerical edits with an unambiguous owner,
clear verification, no behavior or policy change, and no external effects;
70-89 for low-blast-radius local changes that still require ordinary review;
40-69 for meaningful behavior, workflow, or test changes; 10-39 for policy,
tooling, delegation, prioritization, default-behavior, or approval-boundary
changes; and 0-9 for external actions, secrets/authentication, destructive
operations, network/service mutations, unclear ownership, or missing context.
This score is only a routing hint for `session-retro`; it never authorizes this
skill to apply a change.

## Workflow

1. Identify the session metadata from the hook prompt: `session_id`,
   `transcript_path`, `cwd`, and tool if present.
2. Read the transcript if available. Keep the review targeted; do not summarize
   every turn.
3. Extract only reusable candidate lessons. Prefer fewer, sharper candidates
   over broad self-criticism.
4. For each candidate, classify the proposed action, target artifact, autonomy
   level, and curation hints. Prefer improving an existing loaded or relevant
   skill over creating a new skill when the lesson fits an existing workflow.
5. Redact secrets, credentials, private tokens, personal data, and raw command
   output that is not needed as evidence.
6. If there are no useful lessons, do not create an inbox file. Report that no
   lesson candidates were captured.
7. If there are useful lessons, create the inbox Markdown file. Do not modify
   `AGENTS.md`, `CLAUDE.md`, skills, hooks, READMEs, decisions, or code.
8. Add a `Proposed patch` section only as inert text inside the inbox file when
   the change is narrow, high-confidence, and aimed at a repository-visible
   text artifact. Use `interview-required` when the right change depends on the
   user's preference or on missing context.
9. Report the inbox path and the candidate titles with their `Value`,
   `Implementation safety`, `Proposed action`, and `Autonomy level`. Tell the
   user that `session-retro` processes the inbox later.

## Safety

Never implement, promote, archive, or delete learning candidates in this skill.
Never create externally visible actions. This skill only creates local inbox
records for later user review.

A proposed patch in an inbox file is only text. Do not apply it, stage it, or
edit the target artifact from this skill. Do not encode transient tool failures
or one-session environment quirks as durable rules; capture the reusable pattern
or fix only when it is likely to recur.
