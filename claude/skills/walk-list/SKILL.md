---
name: walk-list
description: Process items from a list file strictly one at a time. The input file is MOVED into a protected store that only walk.py can read; a PreToolUse hook blocks every other tool from accessing the store. Structural enforcement — you cannot see item N+1 until you've recorded a decision for item N. Use any time a list of items must be processed individually without batching (reconciliation matches, ambiguous records, per-row verifications). Invoke via `/walk-list start <file>` then `/walk-list next <decision>` after each item.
user-invocable: true
allowed-tools: Bash, Read
argument-hint: "start <input-file> | next <decision-text> | status <input-file> | restore <input-file> | abort <input-file> | show-decisions <input-file>"
---

# walk-list: strict one-at-a-time processing (structural)

## Mechanism

`walk.py start <input-file>`:
1. Moves `<input-file>` into `~/.claude/walk-list-data/<uuid>/source.json`.
2. Replaces `<input-file>` with a short stub explaining that the file is locked.
3. Prints exactly ONE item (the first).

A PreToolUse hook (`block-walk-list-access.sh`) blocks every tool (Read, Grep, Glob, Edit, Write, NotebookEdit, Bash) from touching `~/.claude/walk-list-data/` except for `python .../walk.py` invocations. The hook's block is non-overridable from inside this session — the only way around it is for the user to edit `~/.claude/settings.json` and remove the hook.

`walk.py next <input-file> <decision>`:
1. Records `<decision>` against the current item in the session state.
2. Advances the cursor by exactly one.
3. Prints the next item (or signals completion).

`walk.py restore <input-file>` (run at end of walk):
1. Copies the decisions out to `<input-file>.walk-decisions.json`.
2. Moves the source file back to its original location.
3. Removes the session from the protected store.

`walk.py abort <input-file>` — same as restore but discards decisions.

## Invocation

```
/walk-list start <input-file>
/walk-list next <decision-text>
/walk-list status <input-file>
/walk-list show-decisions <input-file>
/walk-list restore <input-file>
/walk-list abort <input-file>
```

All commands shell out to `python ~/.claude/skills/walk-list/walk.py <subcmd> <args>`.

## Input file formats

Auto-detected by `walk.py`:

1. **JSON array**: `[ {...}, {...}, ... ]` — one item per array element.
2. **JSON Lines**: one JSON object per line.
3. **Plain text**: one string per line.

Filter to a dedicated input file before walking — pointing the walk at a full upstream dataset (e.g., the entire `low_confidence_matches.json`) means you'd lose access to that file for the duration of the walk. Create a scratch subset first.

## Decision text

The decision argument is free-form. Suggested format: a verdict token plus a short reason, e.g. `confirmed→TI-JuancRodri b.1895-1928 matches Canaro credits`. Full text is stored in `<input-file>.walk-decisions.json` after `restore`.

## Uncertainty

If you don't have enough evidence to decide the current item, delegate to a background agent (one agent per case). While the agent runs, do NOT call `/walk-list next`. Wait for the verdict, then pass it as the decision.

## Resume semantics

If the session ends mid-walk, `/walk-list start <input-file>` detects the stub and resumes from the saved cursor. `status` and `show-decisions` remain available throughout.

## Output

After `restore`, decisions are in `<input-file>.walk-decisions.json` with schema:

```json
{
  "cursor": 203,
  "decisions": [
    {"index": 0, "item": <original-item>, "decision": "<text>"},
    ...
  ],
  "original_path": "<input-file-path>"
}
```

Merging decisions into any persistent store (e.g., a project's `person_reviews.json`) is a separate step you do yourself after reviewing the decisions file.
