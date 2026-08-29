---
name: orchestrate-review
description: Use when coordinating two live Emacs agent sessions for a staged implementation that needs independent artifact review, fixed author/reviewer roles, unattended completion, or role reversal.
---

# orchestrate-review

## Overview

Run one complete Superpowers-style stage across two live `agent.el` sessions.
Agent 1 owns every authoring and implementation phase; Agent 2 independently
reviews the specification and plan once each. The helper enforces phase order,
fixed roles, one initial whole-stage implementation handoff, and targeted
whole-stage steering only after a genuine incomplete return.

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
- Plan tasks belong exclusively to Agent 1's internal execution. They are not
  orchestration phases, progress units, handoff points, or acceptance gates.
- In a multi-stage engineering project, delegate top-level stages sequentially,
  one stage per fresh Agent 1 session. Do not reuse that implementation session
  for the next stage. Internal plan tasks stay inside their stage's single
  session and never receive separate orchestration delegations. `init-run`
  rejects a nonempty Agent 1 transcript for a new, non-adopted stage; sequencing
  the stage runs remains the orchestrator's responsibility.

## STAGE ATOMICITY — HARD RULE

The entire stage is the smallest orchestration unit. After the plan review,
send one initial implementation handoff to Agent 1 and wait for the complete
stage. Agent 1 owns implementation and stage-final verification.
Agent 1 must never end its turn to wait: the injected implementation
contract tells it that an unfinished background job, subagent, or reviewer is
not a reason to return, and that it must wait inside the turn with bounded
polling (under the 10-minute command limit, re-armed as needed). An ended turn
is a stop; the orchestrator's only remedy is one targeted steer.
Violating the letter of these rules violates the workflow:

- Never report progress as `Task N`; report only the stage and current phase.
- Never inspect or steer Agent 1's internal tasks, subagents, task transcripts,
  task commits, or per-task processes.
- Never send task-specific corrections, liveness checks, generic continuation
  prompts, or independent verification instructions.
- Never run independent acceptance gates at internal task boundaries. Agent 1
  owns implementation checks until the complete stage returns.
- Never treat a task commit or batch boundary as permission to prompt Agent 1.
- Send exactly one initial implementation handoff. While Agent 1 is active,
  never send a continuation, reminder, marker-repair, model-switch, or recovery
  prompt.
- Intermediate narration and tool activity are not returns. Only an
  authoritative transition to awaiting input permits return handling.
- When Agent 1 genuinely returns early, read the bounded final return, diagnose
  the specific reason for the stop, and send one targeted steering message that
  directs Agent 1 back to the whole-stage outcome.
- Never send a generic continuation prompt. Never repeat a steering message.
  A targeted message names the actual obstacle and resolves it using authority
  and evidence already available from the stage.
- Agent 1 owns implementation and stage-final verification. The orchestrator
  does not inspect the work, run acceptance tests, compare outputs, or add an
  implementation-review pass.

Internal decomposition is allowed; external task-level orchestration is not.
The absence of another Agent 2 review does not make task-level supervision
acceptable.

Use the helper script for deterministic Emacs/session operations:

```bash
python "$SKILL_DIR/scripts/orchestrate_review.py" --help
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
- Use the guarded run file for every submission. Do not call the underlying
  Emacs submit functions directly.
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

Identify the two top-level buffer names and transcript paths from the visible
Emacs sessions before creating the run. The helper deliberately has no global
buffer-list command: once implementation exists, global enumeration could leak
internal task/subagent names and states. After initialization, every status
probe resolves only the two fixed top-level actors from the run file.

Live Emacs status is transferred through one-shot mode-`0600` temp files while
the evaluated Emacs form returns `nil`, so structured status data does not
travel through the `emacsclient --eval` return channel. The status contract
contains buffer name, state, and directory; it does not include buffer text.

Create a guarded mode-`0600` run file outside the repo or under an ignored
state directory:

```bash
python "$SKILL_DIR/scripts/orchestrate_review.py" init-run \
  --run-file /tmp/improvement-5-run.json \
  --repo /path/to/repo \
  --stage 5 \
  --agent1-buffer '*claude:...*' --agent1-backend claude-code \
  --agent1-transcript /path/to/claude.jsonl \
  --agent2-buffer '*codex:...*' --agent2-backend codex \
  --agent2-transcript /path/to/codex.jsonl
```

The file fixes the complete role bundles and permits exactly these phases in
order: `spec`, `spec-review`, `plan`, `plan-review`, `implementation`.

To adopt a run whose two reviews already occurred, use
`--adopt-implementation`, `--spec-commit`, `--plan-commit`, and
`--reviews-complete`. Adoption starts at the one whole-stage implementation
handoff; it does not import task state.

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

A newly initialized Claude session can still report `unknown` before its first
lifecycle event. The guarded first submission reconciles it as waiting only
when the Claude process is live, its configured transcript has no history, and
the per-process status file names that exact transcript. Do not manually mark
other unknown sessions as waiting.

## Step 3: Submit guarded phases

Write phase context to a mode-`0600` temp file, submit it through the run, then
delete it:

```bash
python "$SKILL_DIR/scripts/orchestrate_review.py" submit \
  --run-file /tmp/improvement-5-run.json \
  --phase spec \
  --prompt-file /tmp/prompt.txt
```

The helper selects the actor from the phase and rejects out-of-order,
duplicate, wrong-role, busy-actor, and post-implementation arbitrary
submissions. Before delivery it records the fixed transcript byte boundary;
only after a busy transition or transcript growth acknowledges delivery does
it record the phase as active. If the initial submit call returns without that
acknowledgement, the helper may retry only the submit keystroke when the exact
phase marker proves that the original prompt remains in the fixed actor's
composer. For Codex app-server sessions it reads that composer through
`codex-prompt-input` instead of applying terminal prompt syntax. It never
retransmits the prompt. It does not enable the next handoff
merely because the prompt was delivered. Every phase prompt ends with a fixed
completion marker contract, and the implementation prompt also contains a
non-overridable whole-stage contract. Implementation is stricter: only
transcript growth independently acknowledges delivery, and neither the initial
submission nor a CLI recovery command retries Return.

After the fixed top-level actor is awaiting input, record the return:

```bash
python "$SKILL_DIR/scripts/orchestrate_review.py" finish-phase \
  --run-file /tmp/improvement-5-run.json \
  --phase spec
```

`finish-phase` reads only bytes appended to that fixed actor's configured
top-level transcript after the current submission and requires the exact
completion marker. It rejects stale or missing evidence and premature or
mismatched phases. Every actor, including implementation, must be authoritatively
awaiting input; transcript text never overrides a busy lifecycle state. Only
then does the next handoff become available.

The helper persists a pending record before every external submission. If
delivery fails ambiguously, all further actions stop until the operator uses
`reconcile-submission --delivered` or `--not-delivered` based on concrete
session evidence. When the exact current phase marker is still present in the
composer, use `retry-delivery --run-file <run>`; it rechecks the transcript and
session state, sends only Return, and refuses to paste the prompt again. Never
retry or retransmit an ambiguous prompt automatically. Neither path sends
Return for implementation. Reconciliation activates implementation only when
the guarded transcript actually grew; every other ambiguous implementation
outcome freezes the run permanently.

If a non-implementation actor's process exits after accepting the phase prompt
but before returning any assistant output, start a fresh fixed-role session and
use `restart-phase --run-file <run> --prompt-file <same-context>`. The helper
requires authoritative waiting state, proves that the failed transcript has no
assistant output after the guarded boundary, submits the same whole phase once
to the fresh Claude or Codex session, and atomically rebinds the run to its new
transcript. It
refuses recovery after any reviewer or author output, so this is process-loss
recovery rather than another review pass. Fresh app-server sessions are treated
as waiting from the backend's authoritative inactive-turn state even when the
cached event state is still `unknown`.

## Step 4: Monitor without ending the turn

Use Python-based polling, not shell `sleep`, because the reviewed agents may run broad process probes such as `pkill -f "sleep 20"` that can kill sleep-based monitor commands.

For a one-shot status check:

```bash
python "$SKILL_DIR/scripts/orchestrate_review.py" status \
  --run-file /tmp/improvement-5-run.json
```

This prints a short human-readable status from repo, live buffer state, and
transcript evidence. Add `--json` only when another program will consume the
output.

For a polling loop:

```bash
python "$SKILL_DIR/scripts/orchestrate_review.py" watch \
  --run-file /tmp/improvement-5-run.json \
  --interval 20
```

This prints one concise line when state changes. If it produces no output, the
state has not changed. All repository, buffer, and transcript sources come from
the guarded run; callers cannot substitute a task buffer or transcript. Do not
use a reviewer verdict as a permission gate: a returned review with its fixed
completion marker advances the workflow.
If a read-only poll loses the Emacs server connection, the watcher reports the
error and retries once after the normal interval. A second consecutive failure
exits. Prompt content is never retransmitted automatically.

During specification and planning, use the bounded transcript evidence needed
to pass artifacts between agents. During implementation, the helper disables
transcript and repository monitoring and exposes only the run's stage/phase
plus Agent 1's fixed top-level session state. The only exception is the marker
validator inside `finish-phase`; it never overrides a busy lifecycle state or
prints transcript content. Do not bypass it to read internal task/subagent
output or inspect per-task repository/process state.

If Agent 1 is awaiting input and `finish-phase --phase implementation` rejects
the return because its final marker is missing, run `stage-return --run-file
<run>`. The command prints only the latest bounded assistant return and records
its digest. It is unavailable while Agent 1 is busy and refuses a completed
marker.

Classify the stated reason. If progress truly requires a user-only credential,
identity check, irreversible action, spending decision, destructive action, or
underdetermined product choice, report that blocker. Otherwise write a specific
diagnosis to a mode-`0600` prompt file and steer the same fixed Agent 1:

```bash
python "$SKILL_DIR/scripts/orchestrate_review.py" steer-stage \
  --run-file /tmp/improvement-5-run.json \
  --prompt-file /tmp/improvement-5-steering.txt
```

The diagnosis states why Agent 1 stopped, supplies the missing evidence or
authority already available, and returns ownership of the complete stage to
Agent 1. Use exactly these three populated lines so the helper can distinguish
specific steering from a generic continuation:

```text
Obstacle: <the concrete reason Agent 1 returned>
Resolution: <the evidence or authority that resolves it>
Whole-stage direction: <how Agent 1 resumes ownership of the complete stage>
```

It never names an internal task sequence. The helper rejects generic,
repeated, busy-session, ambiguous-delivery, and unrecorded-return steering.

Send concise commentary only when the stage phase changes or the guarded run
reaches a terminal state. Do not emit periodic task-level heartbeats. If Agent
1 is busy, leave it alone.

## Step 5: Create and review the spec

Have Agent 1 create the spec using the repository's required design workflow.
When Agent 1 returns the complete spec, run `finish-phase --phase spec`.
Commit the spec once, then submit that commit to Agent 2 for one independent
review. When Agent 2 returns, run `finish-phase --phase spec-review` before
submitting the plan phase to Agent 1.

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
one independent implementation-readiness review. Record each whole-phase
return with `finish-phase --phase plan` and then
`finish-phase --phase plan-review`; never advance merely because the prior
prompt was accepted by Emacs.

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
plan's internal task boundaries for its own tests and commits. The orchestrator
does not observe or manage those boundaries. Do not transfer implementation to
Agent 2 merely because Agent 2 performed the reviews.

Wait for Agent 1 to return the complete implementation and its final
verification evidence.
The final top-level response must end with the helper-injected exact stage
completion marker. Once Agent 1 is awaiting input, run:

```bash
python "$SKILL_DIR/scripts/orchestrate_review.py" finish-phase \
  --run-file /tmp/improvement-5-run.json \
  --phase implementation
```

This command rejects a premature return without the stage marker. Save Agent
1's reported stage-final verification evidence to a mode-`0600` evidence file
and close the run:

```bash
python "$SKILL_DIR/scripts/orchestrate_review.py" complete-stage \
  --run-file /tmp/improvement-5-run.json \
  --evidence-file /tmp/improvement-5-acceptance.txt
```

`complete-stage` refuses an active implementation and records the supplied
verification-evidence digest before marking the stage complete. It does not
perform verification itself.

A user-requested post-implementation review is separate, not an implicit third
handoff.

## Red flags

Stop before acting if you are about to say or do any of these:

- “Task 7 is running”
- “Tasks 1–6 are committed”
- “I will inspect the current task's process or transcript”
- “I will send a focused correction for this task”
- “I will rerun the full gate before the stage is complete”
- “Continue.”

All indicate that internal decomposition or generic prompting has leaked into
orchestration. Return to the stage/phase view. A genuine incomplete return may
receive one specific whole-stage steering message; an internal task may not.

## Common rationalizations

| Rationalization | Required response |
|---|---|
| “I am not adding another review, so task supervision is harmless.” | Task supervision itself violates stage atomicity. |
| “A ten-minute pause justifies inspecting Task N.” | Busy means wait; elapsed time is not a return. |
| “The user needs a detailed status.” | Report the stage and phase, not Agent 1's internal decomposition. |
| “A suspicious test command needs immediate correction.” | Agent 1 owns corrections and verification until the stage returns. |
| “Continue is harmless after a return.” | Diagnose the actual obstacle and send a novel whole-stage steer, or send nothing. |

## Stop conditions

Stop and report a blocker when:

- a session is awaiting input but the expected prompt cannot be submitted
- the worktree has overlapping uncommitted changes not produced by the active actor
- review feedback exposes a missing user decision that prevents the next stage
- an external permission, destructive action, or user-only credential is needed
- Agent 1 returns with a blocker that genuinely requires the user's identity,
  credential, irreversible authority, spending, destructive action, or an
  underdetermined product choice

## Session cleanup — HARD RULE

Stage sessions are disposable. Kill both fixed actor sessions as soon as the
run reaches a terminal state (`complete-stage`, a frozen run, or an abandoned
stage) and before starting the next stage's fresh sessions:

```elisp
(dolist (b '("*claude:...*" "*codex:...*"))
  (when (get-buffer b) (agent--force-kill-buffer (get-buffer b))))
```

Never leave finished, superseded, or primed-but-unused agent sessions open;
the user finds stray sessions distracting. Also kill any session you started
for priming or discovery that did not become a run actor. Report the kills in
the final report.

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
- confirmation that both stage sessions were killed
