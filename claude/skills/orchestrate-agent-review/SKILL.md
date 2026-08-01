---
name: orchestrate-agent-review
description: Coordinate a two-agent planner/reviewer loop across live Emacs agent sessions. Use when the user asks to orchestrate Claude/Fable and Codex, run paired model review, converge a plan through back-and-forth review, monitor agent sessions until completion, or make a multi-agent review process unattended.
---

# orchestrate-agent-review

## Overview

Run a supervised loop where one live `agent.el` session creates or revises work and another live `agent.el` session reviews it. The skill exists to prevent the failure mode where the supervisor stops monitoring, loses session state, or lets an Emacs minibuffer prompt block unattended progress.

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
- planner backend/session, usually Claude/Fable
- reviewer backend/session, usually Codex
- expected first actor
- pass limit or convergence policy

If the user did not provide enough information and no reasonable default exists, ask one focused question. Otherwise infer from the current repo and live sessions.

Inspect live sessions:

```bash
python "$SKILL_DIR/scripts/orchestrate_agent_review.py" buffers
```

Default helper output is for user-facing monitoring: concise text, no raw JSON,
and no base64 buffer tails. `--json` is only for debugging or machine parsing
and still omits buffer tails by default. Use `--include-tail` only for private
local debugging, never for commentary or prompts sent to another agent. The
helper suppresses Emacs message logging during its internal evals; agents using
this skill must not inspect `*Messages*` as part of the workflow.

Do not replace the helper with ad-hoc `emacsclient --eval` probes that return
buffer lists, raw JSON, base64 tails, or arbitrary buffer substrings. Raw
Emacs return values are captured in live agent transcripts and can also create
noisy Emacs messages. If the helper lacks a needed status view, extend the
helper first; do not improvise status checks in live agent buffers.

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

This prints a short human-readable status. Add `--json` only when another
program will consume the output; JSON omits buffer tails unless
`--include-tail` is also supplied.

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
state has not changed. Do not use `watch --json` for user-facing monitoring,
and never use `watch --json --include-tail` in a live agent session.

Send concise commentary updates when state changes or every 60 seconds during long work.
Do not ask worker agents to verify orchestration noise, inspect Emacs messages,
or reason about supervisor internals. Noise prevention belongs in this helper
and in the supervising agent's command choices.

## Step 5: Advance the loop

Use this policy:

1. Planner produces or revises a plan/spec and commits once.
2. Submit the resulting commit to the reviewer.
3. Reviewer answers `IMPLEMENTATION-READY` or `NOT READY`.
4. If `IMPLEMENTATION-READY`, mark the area complete.
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
- accepted commit hash
- number of planner/reviewer passes
- repo branch and ahead/behind state
- whether the working tree is clean
- any automation friction observed
