---
name: overnight-todos
description: Batch-process org-roam TODOs autonomously. Acts on TODOs that can be completed without user input and records blockers for the rest, sorted by ease of unblock. Use when the user says "overnight todos", "run overnight", "process my todos", "todo batch", "act on my todos", or schedules a nightly run.
---

# overnight-todos

## What this does

Queries the org-roam database for actionable TODOs, classifies each, and:

- **Actionable autonomously** → dispatches a subagent that does the work end-to-end.
- **Blocked on user input** → records the blocker with an *ease-of-unblock* score (1=easiest).

Writes a report to `~/.claude/skills/overnight-todos/runs/YYYY-MM-DD-HHMM.md` and opens it in Emacs.

## Default arguments

| Flag | Default | Meaning |
|---|---|---|
| `--mode` | `dry-run` | `dry-run` classifies only; `act` actually does work. **Always start with `dry-run` on a new corpus.** |
| `--max-tasks` | `25` | Hard cap on subagent dispatches per run (cost guard). |
| `--time-budget` | `60` | Minutes after which no new subagents are dispatched. |
| `--max-concurrent` | `5` | Subagent pool size. |
| `--dir` | (none) | Restrict to org-roam files under DIR (forwarded as `(:dir DIR)`). |
| `--tag` | (none) | Restrict to nodes tagged TAG (forwarded as `(:tag . TAG)`). |

## Step 1: Dump TODOs

```bash
TODO_FILE=$(mktemp -t overnight-todos-XXXXXX.json)
emacsclient -e "(org-roam-extras-dump-actionable-todos \"$TODO_FILE\")"
# Optionally with filter-spec:
#   "(org-roam-extras-dump-actionable-todos \"$TODO_FILE\" '(:dir \"/path/\"))"
#   "(org-roam-extras-dump-actionable-todos \"$TODO_FILE\" '(:tag . \"work\"))"
```

Each JSON record has `id`, `file`, `title`, `priority` (string or null), `todo`, `effort` (e.g., `"30m"` or null), `tags`, `olp`.

## Step 2: Triage from titles (no body reads, no subagents)

Run the classifier script:

```bash
REPORT=~/.claude/skills/overnight-todos/runs/$(date +%Y-%m-%d-%H%M)-$MODE.md
CLASSIFICATIONS=$(mktemp -t overnight-todos-cls-XXXXXX.json)
python ~/.claude/skills/overnight-todos/triage.py \
    --input "$TODO_FILE" \
    --output "$REPORT" \
    --mode "$MODE" \
    --max-tasks "$MAX_TASKS" \
    --classifications-out "$CLASSIFICATIONS"
```

`triage.py` is the single source of truth for the title-pattern heuristics. It splits records into three buckets:

- **blocked** — title matches a user-bound pattern (ease 1–5) or a scope-demotion pattern (verb plus too-large object, e.g. "Translate X /book/"). Recorded straight to the report; no subagent.
- **candidate** — title matches an autonomous-action pattern (read / find / fix / update / etc.). Would be dispatched in act mode.
- **investigate** — ambiguous title, or `:project:` tag. Would be dispatched in act mode; the subagent's first step is to read the heading body and decide act-vs-block.

Tune patterns by editing `triage.py` — keep the SKILL body high-level.

### Ease scale (blockers)

| Ease | Meaning |
|---|---|
| 1 | Missing fact / direct input you can give in seconds |
| 2 | Yes-no decision |
| 3 | Outbound voice / framing for communication |
| 4 | Personal cognitive work |
| 5 | Deep strategic context only you have |

## Step 3: Rank candidates

`triage.py` already sorts candidates by `priority + difficulty` ascending (lower = higher dispatch priority). `priority` defaults to 5 if unset; `difficulty` is 1–5 inferred from `effort` (`"15m"` → 1, `"30m"` → 2, `"1:00"` → 3, `>"2:00"` → 4, none → title-keyword heuristic).

## Step 4: Dispatch subagents via walk-list

```bash
python ~/.claude/skills/walk-list/walk.py start "$TODO_FILE" --max-concurrent 5
```

Then main loop:

```
elapsed = 0
dispatched = 0
loop:
  status = walk.py pool-status $TODO_FILE   # {available_slots, remaining_to_claim, ...}
  if dispatched >= max_tasks or elapsed >= time_budget: break
  while status.available_slots > 0 and status.remaining_to_claim > 0:
    out = walk.py dispatch $TODO_FILE        # CLAIM_TOKEN: <T> + item JSON
    spawn background Agent with the per-TODO prompt below
    dispatched += 1
  wait for any agent completion notification
  refresh elapsed
```

**Per-TODO subagent prompt** (substitute `{item}`, `{token}`, `{walk_py}`, `{file}`):

```
You are processing one org-roam TODO autonomously. Do the work end-to-end if you can complete it without asking Pablo, or report a blocker.

TODO:
{item}

How to read the heading body for context:
- `emacsclient -e '(org-id-goto "{id}")'` jumps Emacs to it (you do not need to "see" Emacs)
- Or read the file directly and grep for the heading by ID

Rules:
1. NEVER take externally visible actions (open PR, send email, post Slack, modify shared infra, push commits) — those are always blockers, ease=2, with a draft as the recommended next step.
2. NEVER ask Pablo a question. If you would need to ask, classify as blocked.
3. Trust internal code; do not add tests or features beyond what the TODO asks for.
4. If you make file changes, mark the TODO state DONE on success: `emacsclient -e '(org-extras-mark-done-by-id "{id}")'` (only if that function exists; otherwise leave the state alone and note it in the verdict).
5. Time budget for this single TODO: 15 minutes. If you exceed it, return BLOCKED with ease=4.

Return ONE verdict line, then call walk.py record:

COMPLETED: <one-line summary> | files_changed=[<paths or none>] | refs=[<links or none>]
FAILED: <attempted action> | reason=<short error>
BLOCKED: <what's missing> | ease=<1-5> | suggested_next_step=<what Pablo could do>

When done:
  python {walk_py} record {file} {token} '<verdict line>'
Then return a one-line summary mirroring the verdict.
```

## Step 5: Aggregate and report

When the loop exits (budget exhausted or list drained):

```bash
python ~/.claude/skills/walk-list/walk.py release-stale "$TODO_FILE" 0   # reclaim any stuck
python ~/.claude/skills/walk-list/walk.py restore "$TODO_FILE"           # writes decisions
```

Read `${TODO_FILE}.walk-decisions.json`. Combine with the blockers recorded in Step 2 (already in `$REPORT`). Append the act-mode results — completed/failed/blocked-after-investigation/deferred — to the existing report. Skip this step for dry-run with `--max-tasks 0`: the Step 2 report is already complete.

Append the per-run summary using this shape:

```markdown
# Overnight TODO run — <date> <HH:MM>

## Summary
- N total TODOs in dump
- N obvious blockers (triaged from titles)
- N dispatched to subagents
- N completed
- N failed
- N still blocked (after subagent investigation)
- N deferred (budget hit, queue remainder)

## Completed
Each: `[priority] title — file` + one-line action + refs.

## Failed
Each: title, file, attempted action, error.

## Blocked — ranked by ease (1=easiest first), then by org priority
Each: title, file, blocker, ease, suggested next step.

## Deferred (run budget reached)
Just title + file + priority. These will be picked up next run.

## Cost note
Subagents dispatched: N. Approx tokens: <if available>.
```

After writing:

```bash
emacsclient "$REPORT"
```

## Safety rails (enforce in orchestrator AND subagents)

- **No externally visible actions without explicit user okay.** PRs, emails, Slack, Asana, shared-infra modifications → always blocked with a draft.
- **No `git push`, no `gh pr create`, no `git clone`** of unsolicited repos.
- **No destructive operations** (force-push, hard reset, recursive delete). Use `trash` not `rm`.
- **No interactive Emacs commands** that block the server (see elisp-conventions safety notes). The subagent uses `org-id-goto` only to position cursor; it reads via files/emacsclient with extractive forms.
- If a TODO appears to require any of the above, mark it blocked with the proposed action in the suggested next step.

## Cron / scheduled invocation

Wrap with `/schedule` (uses email notifications per global rules):

```
/schedule add nightly "/overnight-todos --mode act --max-tasks 15 --time-budget 45"
```

Or run manually:

```
/overnight-todos --mode dry-run            # safe classification pass
/overnight-todos --mode act --max-tasks 5  # small initial trial
```

## When NOT to use this

- Single TODO you want to act on now → just act on it directly.
- TODOs that have a scheduled or deadline date → they are intentionally excluded from the dump; deal with them via your agenda.
- Anything time-sensitive within the run window → the time budget can defer it. Run interactively instead.
