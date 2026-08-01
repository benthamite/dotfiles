---
name: orchestrate-agent-review
description: Use when coordinating two live Emacs agent sessions across planning, independent review, revision, and implementation, including Claude/Fable and Codex role assignments or reversals.
---

# orchestrate-agent-review

## Overview

Run one complete Superpowers-style handoff across two live `agent.el`
sessions: Agent 1 plans, Agent 2 reviews, Agent 1 revises until approval, and
Agent 1 implements. The skill keeps roles, monitoring, session state, and Emacs
minibuffer prompts under explicit control.

## Role contract

- **Agent 1 plans, revises, and implements.** Planning and implementation are
  one role bundle even though independent review happens between them.
- **Agent 2 independently reviews** Agent 1's committed plan or revision and
  returns `IMPLEMENTATION-READY` or `NOT READY`.
- Agent 1 defaults to Claude/Fable and Agent 2 defaults to Codex.
- A user-requested reversal swaps the entire role bundle: the new Agent 1 owns
  planning, revision, and implementation; the new Agent 2 owns independent
  review. Never reverse only the planning phase.
- Plan tasks remain internal execution boundaries for tests and commits. The
  orchestrated handoff is plan/review/revision followed by implementation of
  the approved plan; do not create a new agent cycle for every task unless the
  user explicitly requests that granularity.

Use the helper script for deterministic Emacs/session operations:

```bash
python "$SKILL_DIR/scripts/orchestrate_agent_review.py" --help
```

## Operating rules

- While orchestration is active, do not send a final response unless the workflow reached a terminal state or a real blocker requires user input. Use commentary updates instead.
- Keep prompts narrow after the first plan: “address only these blockers,” “do not reopen settled issues,” and “commit once.”
- Continue past the normal 2–3 review passes only when remaining feedback is a concrete implementation blocker. Stop when feedback becomes preference churn, scope expansion, or the blocker count stops shrinking across two consecutive reviewer passes.
- Preserve unrelated worktree changes. If the repo is dirty for unrelated reasons, report it and avoid staging or committing those files.
- Never use the interactive `agent-start-new-session` path for unattended runs when an instance-name prompt is possible. Start sessions with explicit instance names.

## Step 1: Establish the run

Identify:

- repo root
- improvement area or spec/plan path
- Agent 1 backend/session, usually Claude/Fable
- Agent 2 backend/session, usually Codex
- expected first actor
- pass limit or convergence policy

If the user did not provide enough information and no reasonable default exists, ask one focused question. Otherwise infer from the current repo and live sessions.

Inspect live sessions:

```bash
python "$SKILL_DIR/scripts/orchestrate_agent_review.py" buffers
```

Default helper output is concise and bounded. Live Emacs status is transferred
through one-shot temp files while the evaluated Emacs form returns `nil`, so
structured status data does not travel through the `emacsclient --eval` return
channel. The status contract contains buffer name, state, and directory; it
does not include buffer text.

Create a durable run file outside the repo or under an ignored state directory. The helper can create or update a JSON state file, but the supervising agent remains responsible for interpreting it:

```json
{
  "repo": "/path/to/repo",
  "area": "durable task ledger",
  "planner_buffer": "*claude:...*",
  "reviewer_buffer": "*codex:...*",
  "planner_transcript": "/path/to/claude.jsonl",
  "reviewer_transcript": "/path/to/codex.jsonl",
  "latest_planner_commit": null,
  "expected_actor": "planner",
  "status": "planning"
}
```

## Step 2: Start fresh sessions when needed

When a new session is required, avoid commands that ask for an instance name. Use explicit instance names:

```elisp
(let ((default-directory "/path/to/repo/"))
  (agent-start-session
   (agent-session-create
    :backend 'codex
    :account (agent-account-resolve 'codex t)
    :directory default-directory
    :instance "improvement-5-codex")))
```

For Claude/Fable, use `:backend 'claude-code` and an instance such as `"improvement-5-claude"`.

## Step 3: Submit prompts from temp files

Write each prompt to a `chmod 600` temp file, submit it, then delete it. Use the helper to avoid Elisp string escaping errors:

```bash
python "$SKILL_DIR/scripts/orchestrate_agent_review.py" submit \
  --buffer '*claude:/path/:improvement-5-claude*' \
  --backend claude \
  --prompt-file /tmp/prompt.txt
```

Use `--backend codex` for Codex reviewer buffers.

## Step 4: Monitor without ending the turn

Use Python-based polling, not shell `sleep`, because the reviewed agents may run broad process probes such as `pkill -f "sleep 20"` that can kill sleep-based monitor commands.

For a one-shot status check:

```bash
python "$SKILL_DIR/scripts/orchestrate_agent_review.py" status \
  --repo /path/to/repo \
  --planner-buffer '*claude:...*' \
  --reviewer-buffer '*codex:...*' \
  --planner-transcript /path/to/claude.jsonl \
  --reviewer-transcript /path/to/codex.jsonl
```

This prints a short human-readable status from repo, live buffer state, and
transcript evidence. Add `--json` only when another program will consume the
output.

For a polling loop:

```bash
python "$SKILL_DIR/scripts/orchestrate_agent_review.py" watch \
  --repo /path/to/repo \
  --planner-buffer '*claude:...*' \
  --reviewer-buffer '*codex:...*' \
  --planner-transcript /path/to/claude.jsonl \
  --reviewer-transcript /path/to/codex.jsonl \
  --interval 20
```

This prints one concise line when state changes. If it produces no output, the
state has not changed. A verdict label appears only for the designated reviewer
transcript, only on assistant output, and only when the first nonblank line is
exactly `IMPLEMENTATION-READY` or `NOT READY`. Verdict words in prompts and
progress discussion are not terminal signals.

Send concise commentary updates when state changes or every 60 seconds during long work.

## Step 5: Advance the loop

Use this policy:

1. Planner produces or revises a plan/spec and commits once.
2. Submit the resulting commit to the reviewer.
3. Reviewer answers `IMPLEMENTATION-READY` or `NOT READY`.
4. If `IMPLEMENTATION-READY`, mark the plan approved, then hand control back
   to Agent 1 to implement the approved plan.
5. If `NOT READY`, extract only remaining blockers and submit them to the planner.
6. Repeat until convergence or a stop condition.

Reviewer prompt shape:

```text
Please perform a narrow implementation-readiness review of <area> commit <hash>.

Context:
- Previous accepted/rejected commits: ...
- Your last review found <N> blockers.
- The planner has now committed <hash>.

Task: Check only whether <hash> resolves the previous blockers and whether it introduced any serious contradiction directly caused by those fixes. Do not reopen settled issues or request style/preference changes.

Answer exactly one of:
- IMPLEMENTATION-READY
- NOT READY, followed by only remaining blockers and minimal required changes.
```

Planner revision prompt shape:

```text
The reviewer returned NOT READY with <N> remaining blockers. Please make one minimal revision addressing only these blockers. Do not broaden scope or reopen settled decisions. Commit once and report the hash plus a concise mapping from blockers to changes.
```

## Step 6: Hand the approved plan back for implementation

After Agent 2 returns `IMPLEMENTATION-READY`, submit the accepted plan and
commit to Agent 1. Approval completes the review loop, not the work.

Agent 1 implements the whole approved plan using its task boundaries for the
specified tests and commits. Use the implementation workflow required by the
plan; when the plan leaves that choice open, use `superpowers:executing-plans`.
Do not transfer implementation to Agent 2 merely because Agent 2 performed the
review.

Monitor Agent 1 through the plan's final verification. Return to Agent 2 only
if the user or plan explicitly requires a post-implementation review; that is a
new review phase, not an implicit change in the role contract.

## Stop conditions

Stop and report a blocker when:

- a session is awaiting input but the expected prompt cannot be submitted
- the worktree has overlapping uncommitted changes not produced by the active actor
- the reviewer repeats the same blocker without useful narrowing across two passes
- feedback becomes style-only or scope expansion
- an external permission, destructive action, or user-only credential is needed

If the remaining blocker count keeps shrinking and the issues are concrete executable failures, continue within reason even past the nominal pass limit.

## Final report

When complete, report:

- final status per area
- accepted plan commit hash
- implementation commit or commit range
- number of planner/reviewer passes
- Agent 1 and Agent 2 assignments
- repo branch and ahead/behind state
- whether the working tree is clean
- any automation friction observed
