---
name: walk-list
description: Process items from a list file strictly one at a time (sequential) or N-at-a-time (parallel subagent pool). The input file is MOVED into a protected store that only walk.py can read; a PreToolUse hook blocks every other tool from accessing the store. In pool mode, items are dispatched to concurrent subagents (each handling one item) with a fixed-size in-flight cap — you cannot dispatch past the cap, and slots only free up when record is called. Use any time a list must be processed individually without batching.
user-invocable: true
allowed-tools: Bash, Read, Agent
argument-hint: "start <file> [--max-concurrent N] | next <decision> | dispatch <file> | record <file> <token> <decision> | pool-status <file> | status <file> | show-decisions <file> | release-stale <file> <sec> | restore <file> | abort <file>"
---

# walk-list: strict 1-at-a-time or N-at-a-time processing (structural)

## Mechanism

`walk.py start <input-file> [--max-concurrent N]`:
1. Moves `<input-file>` into `~/.claude/walk-list-data/<uuid>/source.json`.
2. Replaces `<input-file>` with a locked-file stub.
3. Initializes the state file with `max_concurrent` (default 1).
4. In sequential mode (max=1) also prints the first item.

A PreToolUse hook (`block-walk-list-access.sh`) blocks every tool (Read, Grep, Glob, Edit, Write, NotebookEdit, Bash) from touching `~/.claude/walk-list-data/` except `python .../walk.py` invocations.

The state file is read-modify-written under an exclusive `flock` so concurrent subagent `record` calls cannot clobber each other.

## Two modes

### Sequential (max_concurrent = 1, default)

```
/walk-list start /tmp/items.json
# walk.py prints item 1
# you investigate it
/walk-list next /tmp/items.json "<decision>"
# walk.py records and prints item 2
# ... repeat
/walk-list restore /tmp/items.json
```

### Pool (max_concurrent = N)

```
/walk-list start /tmp/items.json --max-concurrent 10
# main-session orchestration loop:
loop:
  status = walk.py pool-status /tmp/items.json    # -> {available_slots, remaining_to_claim, ...}
  while available_slots > 0 and remaining_to_claim > 0:
    out = walk.py dispatch /tmp/items.json         # -> CLAIM_TOKEN: <token> + item JSON
    parse token T and item I from out
    spawn background Agent(item=I, claim_token=T, file="/tmp/items.json") with prompt that includes:
      "When you have your verdict, run:
         python ~/.claude/skills/walk-list/walk.py record /tmp/items.json <T> '<your-verdict>'
       Then return a one-line summary."
  wait for any Agent completion notification
  # (the completed agent already called walk.py record, freeing its slot)
  continue loop
# when done == total, finalize:
walk.py restore /tmp/items.json
```

Key properties of the pool mode:
- The in-flight cap is enforced by `dispatch` (it errors out when the pool is full). You can't dispatch past the cap even if you try.
- Each subagent records its own verdict via `walk.py record`, freeing the slot. The main session dispatches more as notifications arrive.
- The hook blocks the subagent from reading the protected store — it only sees the single item handed to it in its prompt.
- Concurrent `record` calls are safe (flock on state file).

## Commands

| Command | Purpose |
|---|---|
| `start <file> [--max-concurrent N]` | Lock the input, init state, (in seq mode) print item 1. Resumes if a stub is already at the path. |
| `next <file> <decision>` | Sequential: record decision for current item, print next. Refuses if anything is in flight. |
| `dispatch <file>` | Pool: atomically claim next item, return `CLAIM_TOKEN` + item. Errors if pool full or nothing to dispatch. |
| `record <file> <token> <decision>` | Pool: record a dispatched claim, free its slot. Concurrent-safe. |
| `pool-status <file>` | JSON: `{total, done, in_flight, max_concurrent, available_slots, remaining_to_claim, ...}`. |
| `status <file>` | Human-readable status + list of in-flight tokens and their ages. |
| `show-decisions <file>` | Print all recorded decisions, sorted by original index. |
| `release-stale <file> <age-seconds>` | Free in-flight claims older than N seconds; re-queue them for re-dispatch. |
| `set-max-concurrent <file> <n>` | Change the cap mid-walk. |
| `restore <file>` | End walk: move source back to original path, save decisions, tear down. |
| `abort <file>` | Same as restore but discards decisions. |

## Failure modes and mitigations

**Subagent crashes / times out / doesn't call record.**
The slot stays claimed. Use `walk.py release-stale <file> <age-seconds>` to re-queue stale claims. Recommended: run this with a 600s threshold every few minutes if you see slots stuck.

**Subagent returns a malformed verdict.**
`walk.py record` accepts any non-empty string, so the verdict is stored as-is. Post-processing (e.g., mapping to person_reviews.json) can filter or flag malformed entries. If you want the agent to retry, `release-stale` the specific claim and it will re-dispatch.

**Main session forgets to dispatch more after a completion.**
No corruption — just idle. `pool-status` always shows available_slots and remaining_to_claim; use it to resume.

**Agent pool actually has N agents stuck.**
`walk.py release-stale <file> 0` releases everything in-flight; then re-dispatch. Nothing is lost.

## Suggested subagent prompt (pool mode)

When spawning a background Agent for a claim, use a prompt like:

```
Investigate this reconciliation match and decide. Return one of:
  VERDICT: confirmed→<hub-code> <short reason>
  VERDICT: rejected <short reason>
  VERDICT: escalate <reason, needs user review>

ITEM:
<item JSON>

When you have your verdict, run:
  python ~/.claude/skills/walk-list/walk.py record <file> <CLAIM_TOKEN> "<verdict-text>"

Then return a one-line summary mirroring the verdict. Do NOT try to read
the full input file — the protected-store hook will block you. Use only
the item above and external sources (hub files, web, etc.).
```

## Output

After `restore`, decisions are at `<input-file>.walk-decisions.json`:

```json
{
  "cursor": 203,
  "decisions": [
    {"index": 0, "item": <item>, "decision": "<text>", "claim_token": "...", "dispatched_at": "...", "recorded_at": "..."},
    ...
  ],
  "original_path": "<input-file>"
}
```

Merging decisions into any persistent store (e.g., `person_reviews.json` for tangodb) is a separate step you do yourself after reviewing the decisions file.
