# Unintended Behavior Root-Cause Rule

## Goal

Make diagnosis and root-cause repair the default response to every unintended
behavior an agent encounters, while keeping fallback policy as a separate
instruction.

## Changes

In both global instruction files:

- Replace the existing combined root-cause and fallback bullet with these two
  bullets, in this order:

  > - Treat every unintended behavior you encounter, in any context, as a prompt to diagnose and fix the underlying issue. Correct the observed behavior as a consequence of fixing that issue, never as a direct fix target.
  > - Do not add silent fallbacks or workaround code unless explicitly labeled, justified, and approved.

- Make no other wording or ordering changes.

The paired targets are:

- `claude/CLAUDE.md`
- `codex/AGENTS.md`

Update the corresponding documentation in the same implementation commit:

- In `claude/README.org`, replace the existing combined root-cause and fallback
  summary with these two bullets:

  > - Treat every unintended behavior agents encounter, in any context, as a prompt to diagnose and fix the underlying issue; correct the observed behavior as a consequence rather than targeting it directly.
  > - Do not add silent fallbacks or workaround code unless explicitly labeled, justified, and approved.

- In the `codex/README.org` Instructions section, add this sentence after the
  description of global instruction normalization:

  > They also require agents to diagnose and fix the underlying issue behind every unintended behavior they encounter, so the observed behavior is corrected as a consequence rather than targeted directly, and separately prohibit silent fallbacks or workarounds unless explicitly labeled, justified, and approved.

The repo-root project brief and reasoning-task templates are outside scope.
Existing safety, authorization, and higher-priority constraints remain
unchanged.

## Verification

- Re-read both files and confirm the approved wording appears exactly once in
  each.
- Confirm both README summaries accurately describe the new rules.
- Run `bin/ai-config-sync audit`.
- Inspect the final diff and working-tree status, staging and committing only
  the two instruction files and their two required README updates.
