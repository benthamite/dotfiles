---
name: record-decisions
description: Extract and record architectural/algorithmic decisions from the current session. Use at end of session (called automatically by /update-log) or on demand when a significant decision is made.
user-invocable: true
allowed-tools: Read, Write, Edit, Bash, Glob, Grep
argument-hint: "[optional: specific decision to record]"
---

# Record decisions

Review the current session and create any new decision records in the project's `decisions/` directory.

**Prerequisite**: This skill requires a `decisions/` directory in the project root. If it doesn't exist, do nothing and exit — the `/update-log` skill handles offering to create it during first-run setup.

## What counts as a decision

A decision is worth recording when **alternatives were explicitly considered and one was chosen over others for non-obvious reasons.** The goal is to prevent future sessions from re-proposing rejected alternatives.

Do NOT record:
- Routine code changes or bug fixes (unless they involve choosing between fix strategies)
- Decisions that are obvious from reading the code
- Every small implementation choice

DO record:
- Algorithmic choices where a different approach was tried and failed
- Architectural decisions where multiple valid approaches existed
- Design trade-offs with known consequences
- Anything the user explicitly asked to be recorded

## Format

Each entry follows this template (keep it concise — 5-15 lines):

```markdown
## NNN: Title (YYYY-MM-DD)

**Decision:** What we decided, in one or two sentences.

**Rejected:**
- **Alternative A:** Why it was rejected. Include specific evidence (numbers, error descriptions, etc.).
- **Alternative B:** Why it was rejected.

**Files:** Key files affected (optional, only if useful for future reference).
```

## Steps

1. **List `decisions/` directory** to find the current highest entry number.

2. **Review the session conversation** for moments where:
   - Multiple approaches were discussed and one was chosen
   - An approach was tried and abandoned
   - The user or Claude explicitly said "we should record this"
   - A previously rejected approach was accidentally re-proposed (this means the original rejection wasn't recorded)

3. **For each new decision found**, create a new file `decisions/NNN.md` with the next sequential number. Use the template above. Be specific about WHY alternatives were rejected — vague reasons like "didn't work" are useless. Include specific evidence where available.

4. **Update `decisions-summary.md`** to match. This file contains a compact one-line-per-decision table that is auto-loaded into context. For each new or modified decision, add or update the corresponding row. The format is: `| NNN | Topic | One-line decision | Status |` where Status is one of: Final, Tentative, Re-evaluate.

5. **Do not duplicate existing entries.** If a decision is already recorded, skip it. If an existing entry needs updating (e.g., new evidence, status change), edit the existing file in `decisions/NNN.md` and update the corresponding row in `decisions-summary.md`.

6. **If no new decisions were made this session**, do nothing. Not every session produces decisions.

$ARGUMENTS
