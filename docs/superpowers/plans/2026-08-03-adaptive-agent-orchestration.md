# Adaptive Agent Orchestration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace one-pass termination with a backend-neutral semantic control loop that detects genuine Agent 1 returns, obtains independent diagnosis, and sends novel targeted steering until stage acceptance.

**Architecture:** Keep the existing paired Python helper as the deterministic state machine and transport boundary. Add normalized backend terminal events, version-3 run state, Agent 2 diagnosis artifacts, intervention fingerprints, stage-level progress snapshots, and evidence-bearing steering/acceptance transitions. Preserve one-way spec and plan review while allowing implementation recovery only after a genuine top-level return.

**Tech Stack:** Python 3 standard library, `unittest`, Emacs `agent.el`, Claude/Codex JSONL transcripts, Markdown skill contracts.

---

### Task 1: Version-3 state and migration

**Files:**
- Modify: `tests/test_orchestrate_agent_review.py`
- Modify: `codex/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`
- Modify: `claude/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`

- [ ] **Step 1: Write failing state-schema tests**

Add tests asserting that a new run contains `implementation_turns`,
`terminal_events`, `progress_snapshots`, `diagnoses`, `interventions`,
`acceptance_attempts`, `current_return`, and `stalled_returns`; version-2 active
runs migrate without agent contact; version-1 runs remain unsupported. New runs
also store authoritative actor session identities plus a validated acceptance
contract and backend recovery policy supplied as mode-`0600` JSON files at
initialization.

```python
def test_version_2_active_run_migrates_without_contacting_agents(self):
    state = self.version_2_active_state()
    self.run_file.write_text(json.dumps(state), encoding="utf-8")
    with mock.patch.object(
        orchestrator, "actor_session_identity", side_effect=("agent-1", "agent-2")
    ), mock.patch.object(orchestrator, "submit_to_agent") as submit:
        orchestrator.migrate_run(self.migrate_args())
    migrated = orchestrator.load_run(self.run_file)
    self.assertEqual(migrated["version"], 3)
    self.assertEqual(len(migrated["implementation_turns"]), 1)
    submit.assert_not_called()
```

- [ ] **Step 2: Run the focused suite and verify RED**

Run: `python3 -m unittest tests.test_orchestrate_agent_review`

Expected: failures for run version 3 fields and missing `migrate_run`.

- [ ] **Step 3: Implement schema version 3 and migration**

Set `RUN_VERSION = 3`; replace the terminal `implementation-stopped` model with
the approved implementation states; initialize and validate the new evidence
lists; add an atomic `migrate-run` command that accepts only a coherent
version-2 active implementation and records its initial implementation
submission as turn 1 without agent contact. Require `--acceptance-policy` and
`--recovery-policy` mode-`0600` JSON files. Query each fixed Emacs buffer
read-only for its authoritative session identity and reject missing or
mismatched identities. Parse acceptance commands as argument arrays with
deadlines and required markers; parse recovery policy as restart/backoff limits.
Require a nonempty ordered `reviewer_profiles` list with fixed backend, account,
model, and optional reasoning configuration, plus
`fresh_session_on_exhaustion: true`. For migration, require the first profile to
match the current Agent 2 binding. Reject shell strings and unknown policy keys.

```python
IMPLEMENTATION_STATUSES = {
    "implementation-active", "implementation-returned", "diagnosis-active",
    "diagnosis-returned", "diagnosis-recovery", "steering-ready",
    "acceptance-active", "backend-recovery",
    "user-blocked", "complete",
}
```

- [ ] **Step 4: Run the suite and verify GREEN**

Run: `python3 -m unittest tests.test_orchestrate_agent_review`

Expected: all state and migration tests pass.

- [ ] **Step 5: Synchronize paired helper and commit**

Run byte comparison for both scripts, then commit only the paired scripts and
tests with message `orchestration: add adaptive run state`.

### Task 2: Authoritative backend terminal events

**Files:**
- Modify: `tests/test_orchestrate_agent_review.py`
- Modify: `codex/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`
- Modify: `claude/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`

- [ ] **Step 1: Write failing adapter tests**

Create transcript fixtures proving that Claude `tool_use` and narration do not
produce a terminal event, a matching non-sidechain Claude `end_turn` does, a
subagent or mismatched-session `end_turn` does not, Codex `response_item` does
not, and the fixed rollout's Codex `event_msg/task_complete` does. Add a command
test requiring both an idle fixed session and a fresh terminal event.

```python
def test_claude_tool_use_is_not_a_terminal_event(self):
    self.write_claude_record(stop_reason="tool_use", text="still working")
    self.assertIsNone(
        orchestrator.backend_terminal_event(self.agent1_actor(), 0)
    )
```

- [ ] **Step 2: Run and verify RED**

Run: `python3 -m unittest tests.test_orchestrate_agent_review`

Expected: missing `backend_terminal_event` and `observe_return` failures.

- [ ] **Step 3: Implement normalized terminal adapters**

Record each actor's live session identity at run initialization. Read transcript
bytes after the current turn boundary and emit a normalized
event only for a backend terminal record. Include backend, actor, record
identity, fixed session identity, terminal reason, ending offset, final text
digest, and bounded final text. Add `observe-return`; reject busy state, missing
events, sidechain/nested events, session mismatch, stale event identities, and
reused offsets. Record a stage-level progress snapshot and set
`implementation-returned`.

- [ ] **Step 4: Remove transcript-message return inference**

Delete `stage_return`, `_latest_implementation_return`, and every path that
treats the latest ordinary assistant text as a return. Keep transcript content
unavailable through generic monitoring during active implementation.

- [ ] **Step 5: Run and verify GREEN, synchronize, commit**

Run the full orchestration suite, compare paired scripts byte-for-byte, and
commit with message `orchestration: detect authoritative agent returns`.

### Task 3: Independent diagnosis and targeted steering

**Files:**
- Modify: `tests/test_orchestrate_agent_review.py`
- Modify: `codex/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`
- Modify: `claude/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`

- [ ] **Step 1: Write failing diagnosis tests**

Cover `request-diagnosis`, structured diagnosis parsing, classification
validation, busy Agent 2 rejection, steering without diagnosis rejection,
backend-neutral routing by fixed run roles, and rejection of a `user-only`
classification without an allowed blocker kind and kind-specific evidence. Add
malformed JSON, invalid schema, reviewer timeout, and genuine early reviewer
return cases; each enters an evidence-bearing diagnostic recovery rather than
stranding `diagnosis-active` or involving the user.

```python
def test_recoverable_return_requires_agent2_diagnosis_before_steering(self):
    self.observe_agent1_return("A capture premise was false")
    with self.assertRaisesRegex(SystemExit, "validated diagnosis"):
        orchestrator.steer(SimpleNamespace(run_file=str(self.run_file)))
```

- [ ] **Step 2: Run and verify RED**

Run: `python3 -m unittest tests.test_orchestrate_agent_review`

Expected: missing diagnosis and steering transitions.

- [ ] **Step 3: Implement diagnosis request and completion**

Build a bounded diagnostic prompt from the stage contract, Agent 1 final
return, progress snapshot, failed acceptance evidence, and prior intervention
digests. Submit it to fixed Agent 2 through lifecycle-aware dispatch. Require a
terminal Agent 2 event and parse one JSON object with exactly
`classification`, `blocker_kind`, `blocker_key`, `reason`, `evidence`,
`strategy_family`, `strategy`, and `steering_prompt`. Validate blocker and
strategy fields against fixed enumerations. Reject external-authority expansion,
unsupported classifications, and user-only diagnoses lacking the exact required
user action plus kind-specific evidence. Record a genuine reviewer return before
validation. Malformed or invalid output becomes a bounded diagnostic-failure
artifact and `diagnosis-recovery`, never an uncaught terminal rejection.

- [ ] **Step 4: Implement reviewer-failure recovery**

For malformed JSON, invalid evidence, or invalid classification, build one
revised request containing the exact validation failure and select a distinct
enumerated reviewer-recovery strategy: `format-repair`, `evidence-repair`, or
`classification-repair`. Reject reuse of the same strategy for the same failure
fingerprint. Leave an active reviewer alone; route a dead or unavailable
reviewer through the stored backend recovery policy. Forbid promotion to
`user-only` unless stage evidence independently meets the narrow schema. When a
live reviewer context exhausts all applicable recovery strategies, rebind the
fixed Agent 2 role to a fresh reviewer session or configured reviewer profile
under the stored recovery policy. Retain every failure artifact, increment the
reviewer recovery epoch, and supply the accumulated failure ledger to the new
context without exposing Agent 1's hidden working context. Implement
`agent-rebind-reviewer-once` as a synchronous, backend-neutral Emacs operation:
key its global action ledger by deterministic recovery identity; return the
prior descriptor on retry; otherwise create one fresh top-level `agent.el`
session from the next ordered profile and record buffer, backend, session
identity, transcript, and account before returning. Validate the descriptor,
then atomically replace only Agent 2's run binding. Rotate profiles in order and
start a fresh pass with new sessions after exhaustion; never signal or repurpose
the old reviewer session.

- [ ] **Step 5: Implement novel targeted steering**

Create an intervention fingerprint from the current return, progress snapshot,
acceptance evidence, diagnosis strategy, and steering text. Reject any existing
fingerprint. Require Agent 1 idle, send the validated steering prompt through
its configured backend, append a new implementation turn boundary, and return
to `implementation-active`.

- [ ] **Step 6: Run and verify GREEN, synchronize, commit**

Run the orchestration suite and commit with message
`orchestration: steer recoverable implementation stops`.

### Task 4: Progress-sensitive loop control, acceptance, and backend recovery

**Files:**
- Modify: `tests/test_orchestrate_agent_review.py`
- Modify: `codex/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`
- Modify: `claude/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`

- [ ] **Step 1: Write failing anti-loop and acceptance tests**

Cover identical intervention rejection, paraphrased interventions in the same
strategy family, two no-progress returns forcing root-cause diagnosis, changed
repository state resetting the stall counter, automatic acceptance completing
without marker repair, failed acceptance becoming diagnostic evidence, and
dead-session recovery through stored policy.

```python
def test_acceptance_pass_completes_without_marker_repair(self):
    self.observe_agent1_return("Implementation complete")
    orchestrator.record_acceptance(self.acceptance_args(passed=True))
    self.assertEqual(orchestrator.load_run(self.run_file)["status"], "complete")
```

- [ ] **Step 2: Run and verify RED**

Run: `python3 -m unittest tests.test_orchestrate_agent_review`

Expected: failures for stall tracking, acceptance transitions, and recovery.

- [ ] **Step 3: Implement progress and acceptance transitions**

Hash HEAD, porcelain status, acceptance evidence, and terminal identity into
progress snapshots. Increment `stalled_returns` only when two consecutive
snapshots show no repository or acceptance change. Mark the next diagnosis
request `root-cause`. Compare normalized `blocker_key` and enumerated
`strategy_family` separately from prompt text, rejecting the same family for a
repeated no-progress blocker.

- [ ] **Step 4: Implement backend-neutral session recovery**

Implement the stored acceptance runner with `subprocess`, `shell=False`, fixed
repository cwd, per-command timeout, required markers, and mode-`0600` logs.
Run it after every genuine Agent 1 return; pass completes the run, failure stores
bounded evidence for diagnosis. Implement dead-session recovery through
`agent.el`: require no live process, no captured prompt, matching session
identity/account, and configured restart allowance; resume in place without
signaling a live process. Transient backend availability uses stored bounded
backoff.

- [ ] **Step 5: Run and verify GREEN, synchronize, commit**

Run all orchestration tests and commit with message
`orchestration: prevent repeated recovery strategies`.

### Task 5: Durable unattended supervisor

**Files:**
- Modify: `tests/test_orchestrate_agent_review.py`
- Modify: `codex/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`
- Modify: `claude/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`

- [ ] **Step 1: Write failing driver tests**

Test an entire scripted transition sequence: active wait without contact;
genuine return; automatic failed acceptance; Agent 2 diagnosis; novel steer;
second return; passing acceptance; complete. Add crash-resume tests proving a
driver restarted after each persisted transition does not duplicate Agent 1 or
Agent 2 contact. Cover crashes before dispatch, after dispatch but before
delivery recording, and after transcript delivery but before state finalization.
Add the same crash matrix for a malformed Agent 2 return followed by a revised
diagnosis request. Exhaust every reviewer-recovery strategy and verify that the
fixed Agent 2 role moves to a fresh reviewer identity with all prior failure
evidence retained, then continues diagnosis without user input. Crash after the
Emacs session is created but before the new binding is persisted; restart and
verify the same action identity returns the same session descriptor rather than
creating another reviewer.

- [ ] **Step 2: Run and verify RED**

Run: `python3 -m unittest tests.test_orchestrate_agent_review`

Expected: missing receipt reconciliation, `supervise` driver, and
duplicate-action guards.

- [ ] **Step 3: Implement durable action receipts**

Add deterministic action identities and prepared submission receipts containing
actor, action kind, prompt digest, transcript boundary, and composer marker.
Add an `agent-submit-once` Emacs operation that synchronously checks a
buffer-local action ledger plus the fixed transcript/composer, submits only an
unseen identity, records it before returning, and reports `sent` or
`already-sent`. Reinvocation after a supervisor crash is the retry mechanism;
it never repastes or duplicates the prompt. Add `reconcile_action_receipt` using
the Emacs ledger, matching top-level transcript user message, or later terminal
event. If Emacs died during the operation, require backend/session recovery and
transcript reconciliation before restoring the ledger. Apply the same protocol
to Agent 2 diagnosis and Agent 1 steering.

- [ ] **Step 4: Implement one-transition supervisor step**

Add `supervisor_step(state)` that applies one state-appropriate action and
persists its receipt before external contact. Dispatch by status to idle
wait, terminal observation, acceptance, diagnosis request/completion, steering,
diagnostic-failure recovery, receipt reconciliation, or dead-backend recovery.
Return a structured action result for logging.

- [ ] **Step 5: Implement durable `supervise` loop**

Add `supervise --run-file <run> --interval <seconds>`. Reload and lock state on
every iteration, invoke one transition, emit output only when the persisted
state changes, and sleep only after a no-change result. Exit 0 at `complete`,
exit 2 at validated `user-blocked`, and resume idempotently after process
interruption.

- [ ] **Step 6: Run and verify GREEN, synchronize, commit**

Run all orchestration tests and commit with message
`orchestration: run adaptive supervision unattended`.

### Task 6: Skill contract, live verification, and documentation

**Files:**
- Modify: `codex/skills/orchestrate-agent-review/SKILL.md`
- Modify: `claude/skills/orchestrate-agent-review/SKILL.md`
- Modify: `claude/README.org`
- Modify: `tests/test_orchestrate_agent_review.py`
- Create: `tests/live_orchestrate_agent_review.py`

- [ ] **Step 1: Write failing skill pressure tests**

Replace one-pass assertions with requirements for backend-neutral Agent 1,
authoritative end-turn evidence, diagnosis before steering, no repeated
intervention, narrow user-only blockers, stage-level progress, and
automatic acceptance-driven completion plus the durable supervisor. Add
forbidden assertions for generic continuation and task-level steering.

- [ ] **Step 2: Run and verify RED**

Run: `python3 -m unittest tests.test_orchestrate_agent_review`

Expected: skill-contract assertions fail against the one-pass text.

- [ ] **Step 3: Rewrite paired skill guidance and README**

Document the semantic control loop, `supervise` command, stored acceptance and
recovery policies, stop classification, anti-loop rules, backend adapters, and
version-2 migration. Preserve one-way spec/plan review and stage-level
reporting. Remove the one-prompt and premature-return terminal rules.

- [ ] **Step 4: Validate everything**

Run:

```bash
python3 -m unittest tests.test_orchestrate_agent_review
uv run --with pyyaml python codex/skills/.system/skill-creator/scripts/quick_validate.py codex/skills/orchestrate-agent-review
uv run --with pyyaml python codex/skills/.system/skill-creator/scripts/quick_validate.py claude/skills/orchestrate-agent-review
cmp codex/skills/orchestrate-agent-review/SKILL.md claude/skills/orchestrate-agent-review/SKILL.md
cmp codex/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py claude/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py
git diff --check
```

Expected: 0 for every command.

- [ ] **Step 5: Run live Claude and Codex scratch-session verification**

Use isolated temporary repositories and newly created top-level Emacs sessions.
For each backend, submit a harmless prompt that emits intermediate output before
a genuine return, verify the supervisor ignores the intermediate record, then
exercise one targeted steer and genuine completion. Interrupt and restart the
supervisor at a prepared/dispatched receipt boundary and verify the transcript
contains the action identity exactly once. Exercise one malformed reviewer
return and verify the revised diagnostic request completes without user input.
Record command output and clean up
only the scratch sessions and temporary repositories created by this test; do
not signal unrelated Emacs sessions.

- [ ] **Step 6: Independently review failure paths**

Have a separate agent search the final diff for false-return, generic retry,
duplicate intervention, busy-session contact, user-blocker overclassification,
and backend coupling defects. Address every confirmed finding with a new
failing regression test before implementation changes.

- [ ] **Step 7: Commit and migrate the active Stage 5 run**

Commit the paired skills, README, helper scripts, and tests with message
`orchestration: supervise implementation adaptively`. Migrate the active
version-2 Stage 5 run with explicit acceptance/recovery policy files and
read-only session-identity queries, without sending Agent 1 or Agent 2 a
message, then continue stage-level supervision under version 3.
