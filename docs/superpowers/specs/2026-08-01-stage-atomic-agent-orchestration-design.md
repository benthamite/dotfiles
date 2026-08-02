# Stage-Atomic Agent Orchestration

## Problem

`orchestrate-agent-review` says that a whole stage is the orchestration unit,
but its helper accepts arbitrary prompts at any time. Under time pressure,
agents therefore turn a reviewed plan's internal tasks into external status,
recovery, and verification checkpoints. The Stage 2 parity run reproduced this
failure: the orchestrator supervised individual tasks, sent task-specific
follow-ups, and repeated broad gates before the stage was complete.

Baseline pressure tests against the current skill reproduced the same behavior
three times. Agents reported `Task N`, inspected task commits and processes,
and proposed task-specific continuation prompts despite the existing
whole-stage sentence. This is a behavioral lapse with a structural remedy:
the prose is correct, but the helper does not enforce it.

## Invariant

One run processes one complete stage through exactly five handoffs:

1. Agent 1 creates the stage specification.
2. Agent 2 reviews the specification once.
3. Agent 1 creates the stage plan.
4. Agent 2 reviews the plan once.
5. Agent 1 implements the entire stage.

Plan tasks belong exclusively to Agent 1's internal execution. The
orchestrator must not expose them as progress units, inspect or steer them,
run acceptance gates at their boundaries, or ask the user about them. Agent 2
never receives implementation work unless the user explicitly starts a
separate post-implementation review.

## Enforced run state

The helper will own a mode-`0600` JSON run file containing the stage, fixed
Agent 1 and Agent 2 role bundles, current phase, and submission history. A new
`init-run` command creates it. The guarded `submit` command will:

- accept only the expected phase;
- route authoring/implementation phases to Agent 1 and review phases to Agent
  2;
- mark one phase active without enabling the next handoff;
- wrap the implementation handoff in a non-overridable stage-completion
  contract; and
- reject every additional arbitrary submission after implementation starts.

Every phase prompt requires a fixed completion marker. Before delivery, the
helper requires the fixed destination actor to be awaiting input and records
the configured top-level transcript's byte boundary. `finish-phase` checks that
the actor has returned and accepts only a matching marker appended after that
boundary before enabling the next handoff. This separates prompt delivery from
phase completion and prevents stale evidence or back-to-back submission of
unfinished phases.

Before contacting Emacs, the helper persists a pending-submission record. A
crash or ambiguous transport failure therefore blocks retries until
`reconcile-submission` records whether delivery occurred, preventing accidental
duplicate external submissions.

An existing run may be adopted at implementation only when the caller records
both the specification and plan commits and explicitly declares the two review
handoffs complete.

## Recovery and monitoring

Implementation recovery uses `resume-stage`, which accepts no prompt text. It
may run only while implementation is active and Agent 1 is awaiting input. It
sends a fixed instruction to complete all remaining work and stage-final
verification without stopping at internal task boundaries. Task-specific
recovery is therefore unavailable through the helper.

`run-status`, `status`, and `watch` expose only the stage and phase. During
implementation, the supervising agent may use these phase-level signals but
must not inspect Agent 1's internal subagents, task transcripts, task commits,
or per-task processes. A busy Agent 1 is left alone. An awaiting Agent 1 may
receive only the fixed `resume-stage` instruction.

The helper has no global buffer-list command. Actor buffers and transcripts are
required when the run is created and all later monitoring resolves only those
fixed top-level sessions, so task/subagent buffers cannot be substituted.

## Verification boundary

Agent 1 owns implementation checks while the stage is active. The orchestrator
runs one independent acceptance pass only after Agent 1 returns the complete
stage. User updates name the stage and phase, never an internal task. The stage
is marked complete only after `finish-phase` verifies the stage-complete marker
and `complete-stage` records a digest of explicit stage-wide acceptance
evidence.

## Acceptance criteria

- Out-of-order, duplicate, wrong-role, and post-implementation arbitrary
  submissions fail before contacting Emacs.
- The implementation prompt always contains the fixed whole-stage contract.
- Recovery accepts no arbitrary prompt and fails while Agent 1 is busy.
- An active phase cannot advance until the fixed actor returns with its exact
  completion marker.
- Ambiguous external delivery blocks retry until explicit reconciliation.
- Paired Claude and Codex skill/helper copies remain byte-identical.
- Pressure tests that previously produced task-level orchestration instead
  preserve stage-level reporting and recovery.
- Existing unrelated dotfiles changes remain untouched.
