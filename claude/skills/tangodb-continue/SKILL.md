---
name: tangodb-continue
description: Tangodb session-start and resume workflow. Use when the user explicitly invokes $tangodb-continue, asks to start a tangodb working session, or asks to run the tangodb resume workflow. Reads the project guide, master data-cleanup plan, progress file, and required slice docs; resumes the exact next step from docs/data-cleanup-progress.md, then keeps chaining safe read-only work until a hard stop, approval gate, or real blocker is reached.
---

# Tangodb continue

## Overview

Resume tangodb work from the repository handoff instead of chat history. The
progress file is the operational source of truth for the next step.

## Workflow

1. Confirm the working directory is the tangodb repo. If not, locate it or ask
   for the path.
2. Read `CLAUDE.md`.
3. Read `docs/data-cleanup-plan.md`, including the "Lessons from the
   architecture-redesign detour" section.
4. Read `docs/data-cleanup-progress.md`.
5. Read every file listed under the progress file's required first reads before
   doing task work.
6. Resume the exact next step listed in `docs/data-cleanup-progress.md`.
7. After completing one safe read-only slice, update the working plan and keep
   going to the next safe read-only step implied by the progress file and
   current findings.
8. Stop only when a progress-file hard stop rule applies, an explicit approval
   gate is reached, externally visible action would be required, or a real
   blocker prevents further useful work.

## Priority Rules

- Treat `docs/data-cleanup-progress.md` as authoritative for immediate resume
  work. Do not infer the next step from chat history when the progress file
  says otherwise.
- If `CLAUDE.md`, the progress file, and chat history disagree, follow the
  current user request first, then the progress file for the handoff, then
  `CLAUDE.md`. Mention the discrepancy briefly before acting if it changes what
  would otherwise be done.
- Do not start broad cleanup work just because the master plan is broad. Use the
  current active slice and exact next step from the progress file.
- Do not treat "finished the first listed read-only slice" as a reason to end
  the session. Continue through successive safe read-only slices until blocked.
- Keep architecture-redesign docs historical unless the current user explicitly
  asks for architecture-redesign work.

## Closeout

When ending a data-cleanup session, use `$tangodb-closeout` if available. If it
is unavailable, manually update `docs/data-cleanup-progress.md` with completed
work, current state, exact next step, verification, blockers, and stop
conditions; then run `git diff --check` and mention the progress-file update in
the final answer.
