# Stage-Atomic Agent Orchestration

**Status:** Approved in conversation on 2026-08-03.

## Goal

Coordinate one complete engineering stage through the fixed workflow:

1. Agent 1 creates the specification.
2. Agent 2 reviews the specification once.
3. Agent 1 creates the plan using that feedback.
4. Agent 2 reviews the plan once.
5. Agent 1 implements and verifies the complete stage using that feedback.

Agent 1 defaults to Claude and Agent 2 defaults to Codex. Reviews are one-way;
Agent 1 adjudicates the feedback and advances. In a multi-stage project, stages
run sequentially in fresh Agent 1 sessions. Internal plan tasks remain inside
their stage session and never become orchestration units.

## Implementation supervision

The orchestrator coordinates; it does not implement, inspect, review, or verify
Agent 1's work.

- While Agent 1 is active, send nothing.
- Intermediate narration and tool activity do not constitute a return.
- Agent 1 owns the complete implementation and its stage-final verification.
- A genuine incomplete return is read only after the fixed top-level session is
  authoritatively awaiting input.
- For a recoverable technical stop, the orchestrator diagnoses the stated
  obstacle and sends one novel, targeted whole-stage steering message.
- Generic continuation, repeated steering, task-level correction, and prompts
  sent while Agent 1 is active are forbidden.
- User involvement is limited to credentials, identity checks, irreversible or
  destructive actions, spending, and product choices that existing evidence
  cannot determine.

Agent 1's complete return includes its verification evidence and exact stage
marker. The orchestrator records that evidence and closes the stage without an
independent acceptance or implementation-review pass.

## Minimal helper support

Keep the existing version-2 helper. `stage-return` records a genuine incomplete
return. `steer-stage` accepts a mode-`0600` prompt file only after such a return,
rejects generic or repeated text and busy sessions, and returns the run to
`implementation-active`. No new daemon, schema migration, receipt protocol,
reviewer recovery loop, or backend-recovery framework is part of this change.
