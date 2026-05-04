---
name: tangodb-closeout
description: End-of-session closeout workflow for the tangodb repository. Use when the user says to use $tangodb-closeout, close out, wrap up, update the progress file, prepare a handoff, or make the next "continue" session resume reliably. Updates docs/data-cleanup-progress.md with completed work, current state, exact next step, verification, blockers, and stop conditions.
---

# Tangodb closeout

## Overview

Leave the tangodb repository in a state where a later Claude Code or Codex
session can resume from the user saying only `continue`.

This skill does not replace judgment. It forces the handoff to be written to
the repository instead of relying on chat history.

## Workflow

1. Confirm the working directory is the tangodb repo. If not, locate it or ask
   for the path.
2. Read `AGENTS.md`, `CLAUDE.md`, `docs/data-cleanup-plan.md`, and
   `docs/data-cleanup-progress.md` if it exists.
3. Inspect the current work with `git status --short` and focused diffs for
   files changed during the session.
4. Update or create `docs/data-cleanup-progress.md`.
5. Run `git diff --check`.
6. Final response must say:
   - `docs/data-cleanup-progress.md` was updated;
   - what the next `continue` session should do;
   - what verification ran or why it could not run.

## Progress File Requirements

Keep `docs/data-cleanup-progress.md` operational, not narrative. It must answer
what the next session should do without reading chat history.

Include these sections:

- Active plan
- Active slice
- Last completed
- Current state
- Exact next step for `continue`
- Required first reads
- Hard stop rules
- Verification last run
- Open blockers or risks
- Closeout history

The exact next step must be concrete enough to execute. Prefer:

```text
Build docs/recording-contributor-failure-census.md from
backend/data/reconciliation/recording_contributor_projections.jsonl. Start by
counting unresolved-name, pending-group, and fallback-text rows; then group
visible causes and trace representative examples.
```

Avoid vague instructions like:

```text
Continue cleanup work.
```

## Update Rules

- Preserve user or other-agent changes. Do not revert unrelated work.
- If the session changed code or generated data, record the verification that
  exercised that path.
- If verification was not possible, record that explicitly.
- If no task work was completed, still update the progress file with the
  unchanged next step and the reason work did not progress.
- Keep historical details short. Link to other docs instead of copying long
  explanations.
- Keep database-write gates and explicit user-approval requirements visible.

## Tangodb Defaults

For current data-cleanup work:

- Active plan: `docs/data-cleanup-plan.md`
- Progress file: `docs/data-cleanup-progress.md`
- Current slice plan: `docs/recording-contributor-cleanup-plan.md`
- Current audit: `docs/recording-contributor-worktree-audit.md`

The old architecture-redesign docs are historical unless the user explicitly
asks for architecture-redesign work.
