# Skill audit evaluations

Run routing cases in fresh sessions. A positive case must announce and read the
skill before task actions. A negative case must not read or announce it.

## Explicit routing

Should load:

- `/skill-audit code-audit`
- `$skill-audit diagnose --accept`

Must not load automatically:

- `Use the skill-audit skill to review end-to-end and verify.`
- `Run skill-audit on autonomous skill invocations from the last 90 days.`
- `Review this SKILL.md for bad triggers.`
- `Make code-audit's description less broad.`
- `Audit my Codex configuration for conflicting instructions.`
- `Why did code-audit run on that prompt?`
- `How many times has each skill been used?`
- `Review this repository for bugs, architecture, or prose issues.`

## Behavior

- A normal targeted audit reads the requested copies and resources, reports
  evidence-backed findings, and leaves the worktree unchanged.
- An accepted targeted audit changes only high-confidence scoped files,
  synchronizes intended pairs, runs focused checks, and commits only its change.
- A historical fixture run reports its window and coverage; reconciles explicit,
  autonomous, and ambiguous totals; deduplicates repeated evidence in a turn;
  excludes injected catalogs and post-cutoff reads; and produces identical
  output when repeated on the same input.
