# Adaptive Agent Orchestration Design

**Status:** Approved in conversation on 2026-08-03.

## Goal

Run a complete implementation stage unattended while keeping the fixed Agent 1
moving toward verified stage acceptance. When Agent 1 genuinely returns before
completion, the supervisor diagnoses the reason and applies a targeted recovery
instead of either repeating a generic continuation or terminating mechanically.

The design is backend-neutral. Claude is the default Agent 1 today, but every
workflow rule applies equally to Codex or another supported implementation
backend.

## Failure being corrected

The previous controller combined three errors:

1. Backend-specific submission bypassed `agent.el` lifecycle events, so an
   active turn could appear idle.
2. Intermediate assistant narration and `tool_use` records were interpreted as
   completed top-level returns.
3. Recovery had only two forms: mechanically repeat a generic continuation or
   terminally freeze the run.

The first form produced repeated prompts and no new information. The second
prevented repeated prompts but abandoned unattended completion. Both optimized
the mechanism instead of the stage outcome.

## North Star and invariants

The controller maximizes verified stage-level progress per intervention.

- Stage acceptance, not turn count or a completion marker, defines success.
- Internal plan tasks remain private to Agent 1. The supervisor observes only
  authoritative top-level turn boundaries, bounded final returns, stage-level
  repository state, and stage acceptance evidence.
- No periodic prompt, liveness message, task correction, or generic “continue”
  message is permitted while Agent 1 remains active.
- A genuine incomplete return triggers semantic diagnosis and a targeted next
  action.
- The same intervention can never be sent twice for the same observed state.
- User involvement is reserved for unavailable credentials or identity checks,
  irreversible external actions, spending, destructive actions, or a product
  choice that available evidence cannot determine.

## Authoritative turn detection

Every backend adapter exposes a normalized terminal event:

```text
TerminalEvent {
  actor
  backend
  session_identity
  turn_identity
  transcript_offset
  terminal_reason
  final_text_sha256
}
```

A return exists only when both conditions hold:

1. The fixed top-level session is authoritatively idle.
2. A fresh backend terminal event exists after the current turn boundary.

Each fixed actor records the authoritative session identity discovered from its
live Emacs buffer. For Claude transcripts, a terminal record must be a
non-sidechain assistant record whose session identity matches the fixed actor
and whose `message.stop_reason == "end_turn"`; `tool_use`, assistant narration,
tool results, subagent notifications, and nested-agent `end_turn` records are
not terminal. For Codex transcripts, the adapter reads only the fixed actor's
bound rollout and uses the backend's top-level task-complete/turn-complete event
rather than ordinary assistant output. The normalized event carries a monotonic
identity so stale terminal records cannot be reused.

## Return evaluation

After a genuine return, the controller records a stage-level progress snapshot:
repository HEAD, working-tree digest, acceptance-evidence digest when present,
terminal-event identity, and bounded final-return digest. It then chooses one
of four outcomes:

1. **Accepted completion.** Run the stored independent stage-acceptance contract
   after every genuine Agent 1 return. Passing acceptance completes the stage
   regardless of marker or claim; failing acceptance becomes concrete recovery
   evidence.
2. **Recoverable implementation stop.** Technical premise failure, failed test
   or capture, incomplete work, omitted work, recoverable tool failure, or a
   spec detail that parity evidence determines. Agent 2 diagnoses the stop and
   returns a targeted steering artifact.
3. **Backend recovery.** Process loss, context transport failure, rate limit, or
   model availability problem. A backend adapter restores the fixed Agent 1
   role and state under the configured cost and safety policy, then the semantic
   controller resumes from preserved work.
4. **User-only blocker.** The exact blocker requires the user's identity,
   credentials, irreversible authority, spending decision, destructive action,
   or genuinely underdetermined product judgment. Only this outcome returns to
   the user.

## Independent diagnosis and steering

Agent 2 is the independent diagnostic reviewer on genuine incomplete returns.
It receives only:

- the approved stage goal and acceptance contract;
- Agent 1's bounded final return;
- the current stage-level progress snapshot;
- failed stage-acceptance evidence, when available;
- prior diagnosis and intervention digests for this blocker.

Agent 2 returns a structured artifact:

```text
Diagnosis {
  classification: recoverable | backend-recovery | user-only | complete
  blocker_kind
  blocker_key
  reason
  evidence
  strategy_family
  strategy
  steering_prompt
}
```

A recoverable steering prompt names the actual obstacle, grants any authority
already implied by the stage goal, supplies concrete failed acceptance evidence,
and directs Agent 1 back toward the whole-stage outcome. It never prescribes an
internal task sequence and never asks for a ceremonial marker repair.

The controller validates the diagnosis schema and its evidence before contact
with Agent 1. `blocker_kind` and `strategy_family` come from fixed enumerations;
they are control data, not free-form aliases. A `user-only` diagnosis is valid
only for `credential`, `identity-check`, `irreversible-external-action`,
`spending`, `destructive-action`, or `underdetermined-product-choice`, with the
kind-specific evidence fields and exact required user action present. A
diagnosis cannot broaden external authority, change fixed roles, weaken
acceptance, or classify an ordinary technical failure as user-only.

## Anti-loop control

Every return and intervention receives a semantic fingerprint derived from:

- terminal-reason class and final-return digest;
- repository/progress snapshot;
- failed acceptance digest;
- diagnosis strategy;
- steering prompt digest.

The controller enforces these rules:

- Never send an intervention whose fingerprint already appears in the run.
- If the same normalized blocker key recurs after one strategy, request a
  root-cause diagnosis from Agent 2 with the prior result attached; require a
  different enumerated strategy family. Rewording a prompt does not create a
  new strategy.
- Two genuine returns without repository or acceptance progress force
  root-cause diagnosis rather than another ordinary continuation.
- A strategy that cannot produce a distinct next action becomes `user-only`
  only when the blocker satisfies the narrow user-only definition. Otherwise
  Agent 2 must identify another technical strategy.

There is no arbitrary total-turn cap. The bounded unit is repeated strategy,
not useful progress: long stages may require several genuine turns, but cycles
cannot repeat the same response to the same state.

## Stored policies and unattended driver

The run owns the policies needed to continue without a human caller:

- an acceptance contract containing shell-free command argument arrays,
  per-command deadlines, required output markers, and clean-worktree policy;
- a backend recovery policy permitting deterministic in-place restart of a dead
  fixed session with its recorded session identity and account, plus bounded
  retry/backoff for transient backend availability; the policy also contains an
  ordered nonempty `reviewer_profiles` list whose entries fix backend, account,
  model, and optional reasoning configuration, plus
  `fresh_session_on_exhaustion`;
- the narrow user-only blocker schema;
- polling and diagnostic timeouts.

Acceptance commands run with `subprocess` argument arrays and `shell=False` in
the fixed repository. Mode-`0600` logs retain full output; run state stores only
bounded excerpts and digests for diagnosis.

`supervise` is the durable driver. It repeatedly loads the locked run and
applies exactly one valid transition: wait on an active actor, observe a genuine
return, run acceptance, request or finish diagnosis, apply novel steering, or
recover a dead backend. Every transition is persisted before the next external
action, so restarting `supervise` resumes the same state without duplicate
contact. It exits only at `complete` or a validated `user-blocked` state.

Every diagnosis or steering submission carries a deterministic action identity
derived from the run, transition, actor, and evidence digests. Before external
contact the run stores a prepared receipt with that identity, prompt digest,
transcript boundary, and actor. After contact, delivery is reconciled from the
fixed top-level transcript's matching user-message identity, a later terminal
event after the boundary, or the transport's action ledger. Dispatch itself is
one synchronous Emacs operation, `agent-submit-once`: in the fixed session it
checks a buffer-local action ledger and transcript/composer identity, submits
only an unseen action, and records the action before returning to the supervisor.
Reinvoking the operation after a supervisor crash therefore either performs the
original send or reports the prior send; it never pastes or submits the prompt
twice. An exact still-pending composer permits only submission of that same
prepared action. A live Emacs process is the transaction boundary. If Emacs
itself dies during dispatch, backend recovery reconciles the transcript and
session identity before restoring the action ledger. This protocol applies
identically to Agent 1 steering and Agent 2 diagnosis.

Agent 2 failure is also state, not a dead end. A genuine reviewer return with
malformed JSON, invalid schema, or unsupported evidence records a bounded
diagnostic-failure artifact. The next request cites the exact validation error
and uses a distinct enumerated reviewer-recovery strategy (`format-repair`,
`evidence-repair`, or `classification-repair`). Repetition of the same failure
forces a different strategy family. An active reviewer is left alone; a dead or
unavailable reviewer goes through backend recovery. None of these failures may
be converted into a user-only blocker unless the underlying stage evidence
independently satisfies the narrow user-only schema.

If one live reviewer context exhausts every applicable recovery strategy, the
stored recovery policy rebinds the fixed Agent 2 role to a fresh reviewer
session or configured reviewer profile. The new reviewer receives the stage
evidence and the complete prior diagnostic-failure ledger, but none of Agent
1's hidden working context. Recovery history is scoped by reviewer identity;
rebinding opens a new recovery epoch without erasing earlier failures. Repeated
epoch exhaustion selects a different configured profile or a fresh context with
the accumulated failure evidence, so the supervisor never repeats an identical
diagnostic action against identical state.

Reviewer rebinding uses one backend-neutral synchronous Emacs operation,
`agent-rebind-reviewer-once`. Given a deterministic recovery action identity and
the next stored profile, it returns an existing descriptor from a global Emacs
action ledger or creates one new top-level `agent.el` session and records its
buffer, backend, session identity, transcript, and account before returning.
The supervisor validates that descriptor against the selected profile, then
atomically replaces only Agent 2's run binding and increments the recovery
epoch. A supervisor crash between session creation and run-state persistence
therefore reuses the same descriptor. Profiles rotate in stored order; after
the list is exhausted, the policy creates a fresh session from the next profile
in the same order while retaining the accumulated failure ledger. Rebinding
never signals or repurposes the prior session and never changes to an account or
model absent from the stored policy.

Dead-session recovery invokes the backend-neutral `agent.el` restart path only
after proving the fixed buffer has no live process, no captured prompt, a known
session identity, and an unchanged account. It resumes that identity in place;
it never signals a live process. Unsupported recovery evidence goes through
diagnosis rather than an improvised backend command.

## State machine and public operations

Run schema version 3 stores terminal events, progress snapshots, diagnoses,
interventions, and their fingerprints. Implementation states are:

```text
implementation-active
implementation-returned
diagnosis-active
diagnosis-returned
diagnosis-recovery
steering-ready
acceptance-active
backend-recovery
user-blocked
complete
```

Public operations are evidence-bearing transitions, not generic recovery:

- `observe-return` records a fresh authoritative terminal event.
- `request-diagnosis` sends one bounded diagnostic review to Agent 2.
- `finish-diagnosis` records and validates Agent 2's structured artifact.
- `recover-diagnosis` records invalid reviewer output and prepares one
  materially revised diagnostic request.
- `steer` sends the validated, novel steering artifact to Agent 1.
- `record-acceptance` completes the stage or records concrete failed evidence.
- `recover-backend` invokes a backend adapter under the configured policy.
- `supervise` owns the complete unattended transition loop and can resume after
  its own process interruption.

No public `resume-stage`, arbitrary prompt, or marker-repair command exists.
Every Agent 1 contact after the initial handoff requires a fresh terminal event,
a validated diagnosis, and a novel intervention fingerprint.

An active version-2 implementation run can migrate in place: its initial
submission becomes turn 1 and remains active. Migration requires explicit
mode-`0600` acceptance and recovery policy files and performs read-only Emacs
queries to bind each fixed actor's authoritative session identity. It sends no
agent message and refuses identity mismatch. Version-1 looping runs stay
invalid.

## Verification

Regression tests cover the full control loop:

- intermediate assistant/tool-use output never creates a return;
- a fresh backend end-turn plus idle state creates exactly one return;
- recoverable returns require independent diagnosis before steering;
- busy sessions cannot be steered;
- identical intervention fingerprints are rejected;
- repeated blockers force a different diagnostic strategy;
- completion acceptance prevents further Agent 1 contact;
- missing markers do not trigger repair when acceptance passes;
- ambiguous delivery cannot duplicate a turn;
- Claude and Codex adapters normalize terminal events to the same interface;
- nested-agent terminal events cannot match the fixed top-level identity;
- paraphrased prompts in the same strategy family cannot evade loop control;
- unsupported user-only classifications are rejected;
- acceptance commands execute automatically after every genuine return;
- restarting the supervisor cannot duplicate the last external action;
- prepared diagnosis and steering receipts reconcile across every dispatch
  crash boundary without repasting a prompt;
- dead fixed sessions recover through stored policy without signaling live ones;
- version-2 active runs migrate without contacting either agent.

Final verification includes live scratch sessions for both Claude and Codex.
Each backend test exercises lifecycle-aware submission, intermediate output that
does not count as a return, a genuine correlated top-level return, targeted
steering, and supervisor restart at a prepared/dispatched receipt boundary. The
scratch sessions use isolated temporary repositories and are cleaned up without
signaling unrelated Emacs sessions.

The paired Claude and Codex skill copies remain byte-identical, their helper
scripts remain byte-identical, and an independent review searches specifically
for mechanical retry, false-return, duplicate-contact, and infinite-loop paths.
