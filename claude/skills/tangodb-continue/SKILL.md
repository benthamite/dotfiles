---
name: tangodb-continue
description: Tangodb session-start and resume workflow. Use when the user explicitly invokes $tangodb-continue, asks to start or resume a tangodb cleanup working session, or asks to run the tangodb resume workflow. Do not use for ending a tangodb session, generic handoffs, or non-tangodb continue prompts. Reads the project guide, master data-cleanup plan, progress file, and required slice docs; resumes the exact next step from docs/data-cleanup-progress.md, then keeps chaining safe read-only work until a hard stop, approval gate, or real blocker is reached.
---

# Tangodb continue

## Overview

Resume tangodb work from the repository handoff instead of chat history. The
progress file is the operational source of truth for the next step.

This skill starts or resumes work. It does not close out a session or replace
the generic handoff workflow.

## When not to use

- Do not use this skill to end a tangodb session; use `$tangodb-closeout`.
- Do not use this skill for generic handoffs or non-tangodb `continue` prompts.
- Do not use this skill to revive historical architecture-redesign work unless
  the current user request explicitly asks for it.
- Do not infer a broad cleanup task when `docs/data-cleanup-progress.md` has no
  exact next step. Stop and report the missing handoff instead.

## Workflow

1. Confirm the working directory is the tangodb repo. If not, check likely local
   repo roots such as `~/My Drive/repos/tangodb`, then ask for the path only if
   it cannot be found.
2. Read the applicable project guide files: `AGENTS.md` and `CLAUDE.md` if they
   exist. At minimum, read the current tool's guide (`AGENTS.md` for Codex,
   `CLAUDE.md` for Claude) before doing task work; if it is missing, say so
   briefly and continue with the available repository guidance.
3. Read `docs/data-cleanup-plan.md`, including the "Lessons from the
   architecture-redesign detour" section.
4. Read `docs/data-cleanup-progress.md`.
5. Read every file listed under the progress file's required first reads before
   doing task work.
6. Resume the exact next step listed in `docs/data-cleanup-progress.md`.
7. After completing one safe read-only slice, update the in-session working
   plan, then keep going to the next safe read-only step implied by the progress
   file and current findings. Save progress-file edits for closeout unless the
   current task explicitly requires updating it.
8. Stop only when a progress-file hard stop rule applies, an explicit approval
   gate is reached, externally visible action would be required, or a real
   blocker prevents further useful work.

## Priority Rules

- Treat `docs/data-cleanup-progress.md` as authoritative for immediate resume
  work. Do not infer the next step from chat history when the progress file
  says otherwise.
- If the project guide files, progress file, and chat history disagree, follow
  the current user request first, then the progress file for the handoff, then
  the applicable project guide. Mention the discrepancy briefly before acting
  if it changes what would otherwise be done.
- Do not start broad cleanup work just because the master plan is broad. Use the
  current active slice and exact next step from the progress file.
- Do not treat "finished the first listed read-only slice" as a reason to end
  the session. Continue through successive safe read-only slices until blocked.
- Keep architecture-redesign docs historical unless the current user explicitly
  asks for architecture-redesign work.
- When a next step is not clearly safe and read-only, stop before database
  writes, generated-data overwrites, or externally visible actions and ask for
  approval.

## Verification

- Before task work, confirm all required first reads from
  `docs/data-cleanup-progress.md` were read.
- For each completed slice, run the verification named by the progress file,
  slice plan, or current findings. If no executable check applies, re-read the
  produced docs or focused outputs and inspect the relevant diff.
- Before the final response, report the last completed slice, verification run,
  stop reason, and whether `$tangodb-closeout` updated the progress file.

## Closeout

When ending a data-cleanup session, use `$tangodb-closeout` if available. If it
is unavailable, manually update `docs/data-cleanup-progress.md` with completed
work, current state, exact next step, verification, blockers, and stop
conditions; then run `git diff --check` and mention the progress-file update in
the final answer.
