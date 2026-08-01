---
name: orchestrate-agent-review
description: Use when coordinating two live Emacs agent sessions through a one-way spec, spec review, plan, plan review, and implementation workflow, including Claude/Fable and Codex role assignments or reversals.
---

# orchestrate-agent-review

## Overview

Run one complete Superpowers-style workflow across two live `agent.el`
sessions: Agent 1 creates each artifact and carries review feedback into the
next stage; Agent 2 independently reviews the spec and plan once each. The
skill keeps roles, monitoring, session state, and Emacs minibuffer prompts
under explicit control.

## Role contract

- **Agent 1 creates the spec.**
- **Agent 2 reviews the spec once.**
- **Agent 1 creates the plan, incorporating the spec-review feedback.**
- **Agent 2 reviews the plan once.**
- **Agent 1 implements the plan, incorporating the plan-review feedback.**
- Treat both reviews as one-way handoffs. Do not send the artifact back for
  another review pass. Agent 1 adjudicates the feedback and proceeds to the
  next stage.
- Agent 1 defaults to Claude/Fable and Agent 2 defaults to Codex.
- A user-requested reversal swaps the entire role bundle: the new Agent 1 owns
  spec creation, plan creation, and implementation; the new Agent 2 owns both
  independent reviews. Never reverse only one stage.
- Plan tasks remain internal execution boundaries for tests and commits. The
  orchestrated unit is the whole stage; do not create a new inter-model review
  cycle for every task unless the user explicitly requests that granularity.

Use the helper script for deterministic Emacs/session operations:

```bash
python "$SKILL_DIR/scripts/orchestrate_agent_review.py" --help
```

## Operating rules

- While orchestration is active, do not send a final response unless the workflow reached a terminal state or a real blocker requires user input. Use commentary updates instead.
- Treat review findings as input to Agent 1's next stage, not as a request to
  revise and resubmit the current artifact.
- Require Agent 1 to address valid findings in the next artifact or in the
  implementation. If Agent 1 rejects a finding, it records a concise reason
  while continuing; it does not ask Agent 2 to adjudicate the rejection.
- Perform exactly two inter-model review handoffs: one after the spec and one
  after the plan. Do not add implementation review unless the user explicitly
  requests a separate review phase.
- Preserve unrelated worktree changes. If the repo is dirty for unrelated reasons, report it and avoid staging or committing those files.
- Never use the interactive `agent-start-new-session` path for unattended runs when an instance-name prompt is possible. Start sessions with explicit instance names.

## Step 1: Establish the run

Identify:

- repo root
- improvement area and spec/plan paths
- Agent 1 backend/session, usually Claude/Fable
- Agent 2 backend/session, usually Codex
- expected first actor
- current workflow checkpoint

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
  "agent1_buffer": "*claude:...*",
  "agent2_buffer": "*codex:...*",
  "agent1_transcript": "/path/to/claude.jsonl",
  "agent2_transcript": "/path/to/codex.jsonl",
  "spec_commit": null,
  "plan_commit": null,
  "expected_actor": "agent1",
  "status": "spec-writing"
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
state has not changed. The helper's planner/reviewer option names map to Agent
1 and Agent 2 for monitoring purposes only. Do not use a reviewer verdict as a
permission gate: completion of the review response advances the workflow.
If a read-only poll loses the Emacs server connection, the watcher reports the
error and retries once after the normal interval. A second consecutive failure
exits. Submission commands are never retried automatically.

Send concise commentary updates when state changes or every 60 seconds during long work.

## Step 5: Create and review the spec

Have Agent 1 create the spec using the repository's required design workflow.
Commit the spec once, then submit that commit to Agent 2 for one independent
review.

Spec-review prompt shape:

```text
Independently review the spec at <path/commit>. Identify concrete omissions,
contradictions, feasibility risks, and unclear acceptance criteria that should
affect the implementation plan. Return prioritized findings with reasons. This
is the only spec-review pass; Agent 1 will use the findings when creating the
plan, so do not request a revised spec or another review round.
```

## Step 6: Create and review the plan

Submit the spec and Agent 2's complete review to Agent 1. Have Agent 1 create
the implementation plan while adjudicating every finding. Valid feedback must
change the plan; rejected feedback must receive a concise recorded reason.
Proceed directly to the plan rather than revising and resubmitting the spec.

Use the repository's required planning workflow; when unspecified, use
`superpowers:writing-plans`. Commit the plan once, then submit it to Agent 2 for
one independent implementation-readiness review.

Plan-review prompt shape:

```text
Independently review the implementation plan at <path/commit> against the spec.
Identify concrete correctness gaps, missing verification, sequencing problems,
and scope contradictions that Agent 1 should account for during implementation.
Return prioritized findings with reasons. This is the only plan-review pass;
Agent 1 will use the findings while implementing, so do not request a revised
plan or another review round.
```

## Step 7: Implement with the plan review

Submit the plan and Agent 2's complete plan review to Agent 1. Agent 1
implements the plan while adjudicating every finding. Valid feedback must
change the implementation or its verification; rejected feedback must receive
a concise recorded reason. Do not revise and resubmit the plan first.

Use the implementation workflow required by the plan; when unspecified, use
`superpowers:executing-plans`. Agent 1 implements the whole stage using the
plan's task boundaries for tests and commits. Do not transfer implementation
to Agent 2 merely because Agent 2 performed the reviews.

Monitor Agent 1 through the plan's final verification. The workflow ends after
verified implementation. A user-requested post-implementation review is a
separate phase, not an implicit third handoff.

## Stop conditions

Stop and report a blocker when:

- a session is awaiting input but the expected prompt cannot be submitted
- the worktree has overlapping uncommitted changes not produced by the active actor
- review feedback exposes a missing user decision that prevents the next stage
- an external permission, destructive action, or user-only credential is needed

## Final report

When complete, report:

- final status per area
- spec commit hash and spec-review handoff
- plan commit hash and plan-review handoff
- implementation commit or commit range
- Agent 1 and Agent 2 assignments
- repo branch and ahead/behind state
- whether the working tree is clean
- any automation friction observed
