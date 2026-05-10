---
name: record-decisions
description: Extract and record architectural, algorithmic, or design decisions from the current session. Use at the end of a session (usually via /update-log), when the user asks to record/capture an ADR or decision, or when a significant trade-off was made.
---

# Record decisions

Review the current session and create or update concise decision records in the project's `decisions/` directory.

**Prerequisite**: This skill requires a `decisions/` directory in the project root. If it does not exist, stop without creating it; `/update-log` handles offering to create it during first-run setup. If `decisions/` exists but `decisions-summary.md` is missing, create the summary file with the standard header before recording entries.

If `$ARGUMENTS` names a specific decision, file, or subsystem, use it to focus the review. Still skip recording if the material does not meet the criteria below.

## What counts as a decision

A decision is worth recording when **alternatives were explicitly considered and one was chosen over others for non-obvious reasons.** The goal is to prevent future sessions from re-proposing rejected alternatives.

Do NOT record:
- Routine code changes or bug fixes (unless they involve choosing between fix strategies)
- Decisions that are obvious from reading the code
- Every small implementation choice
- General session summaries, changelog entries, TODOs, or notes where no choice was made

DO record:
- Algorithmic choices where a different approach was tried and failed
- Architectural decisions where multiple valid approaches existed
- Design trade-offs with known consequences
- Anything the user explicitly asked to be recorded

Do not invent rejected alternatives. If the user explicitly asks to record a decision but no alternatives were discussed, say that in the record instead of fabricating evidence.

## Format

Each entry follows this template (keep it concise: 5-15 lines):

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

2. **Read existing decision context** before writing:
   - Scan `decisions-summary.md` when present.
   - Read any existing `decisions/NNN.md` entry that appears to cover the same subsystem or question.
   - Use the next zero-padded number after existing `decisions/[0-9][0-9][0-9].md` files.

3. **Review the session conversation** for moments where:
   - Multiple approaches were discussed and one was chosen
   - An approach was tried and abandoned
   - The user or agent explicitly said "we should record this"
   - A previously rejected approach was accidentally re-proposed (this means the original rejection wasn't recorded)

4. **For each new decision found**, create a new file `decisions/NNN.md` with the next sequential number. Use the current local session date in `YYYY-MM-DD` format. Be specific about why alternatives were rejected; vague reasons like "didn't work" are useless. Include exact evidence where available, such as error messages, benchmark numbers, failed commands, or user constraints.

5. **Update `decisions-summary.md`** to match. This file contains a compact one-line-per-decision table that is auto-loaded into context. For each new or modified decision, add or update the corresponding row. The format is: `| NNN | Topic | One-line decision | Status |` where Status is one of:
   - `Final`: the choice is settled unless new facts appear.
   - `Tentative`: the choice is provisional or awaiting validation.
   - `Re-evaluate`: the record names a condition or date for revisiting the choice.

6. **Do not duplicate existing entries.** If a decision is already recorded, skip it. If an existing entry needs updating (for example, new evidence or a status change), edit the existing file in `decisions/NNN.md` and update the corresponding row in `decisions-summary.md`.

7. **If no new decisions were made this session**, leave files unchanged. Not every session produces decisions.

## Verification and report

Before finishing, re-read every created or edited decision file and `decisions-summary.md` to confirm numbering, dates, summary rows, and status values match. Report which files were created or updated. If nothing qualified, report the skip reason (`no decisions/ directory`, `no qualifying decision`, or `already recorded`).
