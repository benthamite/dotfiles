---
name: walk-list
description: Process items from a list file strictly one at a time. Structurally prevents batching — the skill only shows one item per invocation and refuses to advance until a decision for the current item is recorded. Use any time you have N > 1 items that each require individual judgment (reconciliation matches, ambiguous records, per-row verifications). Invoke with `/walk-list start <file>` to begin and `/walk-list next <decision>` after processing each item.
user-invocable: true
allowed-tools: Bash, Read
argument-hint: "start <input-file> | next <decision-text> | status <input-file> | abort <input-file> | show-decisions <input-file>"
---

# Walk-list: strict one-at-a-time processing

## Why this exists

You (Claude) have a documented tendency to batch items that the domain conventions require you to process individually. This skill makes batching structurally difficult: the script only reveals one item per invocation and refuses to advance without a recorded decision. Follow the rules below; don't route around them.

## When to use

Use this skill any time you have a file containing `N > 1` items where each item requires individual judgment and cannot safely be batched. Typical cases:

- Reconciliation matches requiring per-row review (the canonical case for the tangodb project).
- Ambiguous records requiring external research per row.
- Any list where "classify all at once" would produce a non-negligible false-positive/false-negative rate.

If there's any doubt about whether items need individual review, prefer this skill.

## Commands

All commands shell out to `walk.py` in this skill directory.

```bash
python "$CLAUDE_PLUGIN_ROOT/walk.py" start <input-file>
python "$CLAUDE_PLUGIN_ROOT/walk.py" next <input-file> <decision-text>
python "$CLAUDE_PLUGIN_ROOT/walk.py" status <input-file>
python "$CLAUDE_PLUGIN_ROOT/walk.py" abort <input-file>
python "$CLAUDE_PLUGIN_ROOT/walk.py" show-decisions <input-file>
```

If `$CLAUDE_PLUGIN_ROOT` is unset, the walk script lives at `~/.claude/skills/walk-list/walk.py`.

### start

```
/walk-list start <input-file>
```

Initializes or resumes a walk over the items in `<input-file>`. Prints the first unprocessed item (resumes from the state file if one exists). Creates `<input-file>.walk-state.json` alongside the input on first use.

### next

```
/walk-list next <decision-text>
```

Records `<decision-text>` as the decision for the current item, advances the cursor by exactly one step, and prints the next item. Required — the script rejects empty decisions. The input file path is read from the most recent state file.

The decision text is free-form — write whatever encodes your verdict plus any notes. A good minimum is the raw decision (e.g., `confirmed→TI-JuancRodri`, `rejected`, `skipped-needs-external-research`) plus a one-line reason. The full text is stored in the state file.

### status

```
/walk-list status <input-file>
```

Prints total / processed / remaining counts and the most recent decision.

### abort

```
/walk-list abort <input-file>
```

Deletes the state file (discards all recorded decisions). Use only if you need to restart cleanly; the deletion is irreversible.

### show-decisions

```
/walk-list show-decisions <input-file>
```

Prints all recorded decisions so far, one per line, in the form `[N] <decision>`. Useful for reviewing your own reasoning mid-walk without re-scanning the state file.

## Input file formats

The script auto-detects:

1. **JSON array**: `[ {...}, {...}, ... ]` — one item per array element.
2. **JSON Lines**: one JSON object per line — each line is an item.
3. **Plain text**: one string per line — each line is an item.

If you're filtering a subset of items for the walk (e.g., "the 203 pending matches from low_confidence_matches.json"), write the filtered list to a dedicated input file first. Don't point the walk at a full dataset and trust yourself not to peek.

## Hard rules — read carefully

These rules exist because you have historically failed at this.

1. **Do NOT read the full input file directly.** Use only what the walk script prints. If you `Read` or `cat` the input file, you'll see multiple items at once and your batching instincts will take over. The skill's one guarantee — one item per invocation — only works if you respect it.

2. **One decision per `next` invocation.** Never pass multiple items' worth of reasoning in a single `next` call. Never try to say "items 5–12 are all the same kind, here's one decision for all of them." If you catch yourself doing this, stop and back out; issue one `next` per item.

3. **No scripted loops over the items.** Never write a Python loop, a for-loop, or any batch pattern that calls `/walk-list next` N times in a row programmatically. Each `next` is a separate judgment made by you after you process exactly one item.

4. **Uncertain triggers delegation, not a best-guess.** If you don't have enough evidence to make a decision for the current item, invoke a background agent (one agent per case, per domain convention) to research it. While the agent runs, do NOT advance the walk — wait for the agent's verdict, then pass its verdict as your decision.

5. **If you realize you peeked at multiple items, stop and acknowledge.** Don't rationalize past decisions you made based on unauthorized multi-item viewing. Abort the walk (`/walk-list abort`), re-start cleanly, and process each item without peeking.

## Resume semantics

State is persisted after every `next`. If the session ends mid-walk, re-invoking `/walk-list start <input-file>` resumes at the next unprocessed item — you don't lose decisions already recorded. A `status` call shows where you are.

## Output

After the walk completes, decisions are in `<input-file>.walk-state.json`. Schema:

```json
{
  "cursor": 203,
  "decisions": [
    {"index": 0, "item": <original-item>, "decision": "<your-decision-text>"},
    ...
  ]
}
```

Post-processing (e.g., merging decisions into a persistent store like `person_reviews.json`) is a separate task you do yourself once the walk is done. The skill does not auto-merge — you review the state file first, confirm the decisions are what you intended, then apply them.
