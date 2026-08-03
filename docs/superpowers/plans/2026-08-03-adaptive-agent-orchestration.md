# Stage-Atomic Orchestration Correction Plan

**Goal:** Correct the existing orchestration skill so a genuine incomplete
implementation return receives specific whole-stage steering without exposing
internal tasks or turning the orchestrator into an implementer or verifier.

## Work

1. Update both paired skill files with the fixed spec → review → plan → review →
   implementation workflow, sequential fresh sessions for top-level stages,
   authoritative return handling, and the coordinator-only boundary.
2. Add the minimal `steer-stage` helper command. It accepts a mode-`0600`
   prompt file only after a recorded `stage-return` and only while fixed Agent
   1 is authoritatively awaiting input. It rejects generic, repeated, busy, and
   unrecorded-return steering, then returns the run to
   `implementation-active`. An ambiguous initial delivery is not a genuine
   return and remains in the existing fail-closed path.
3. Narrow the implementation prompt's user-only stop conditions so technical
   premise failures and test or capture problems remain Agent 1's responsibility.
4. Replace the former one-pass tests with focused regressions for targeted
   steering, active-session silence, stage-level reporting, paired-file parity,
   and Agent 1 verification ownership.
5. Update the Claude README, run the focused orchestration suite and skill
   validators, inspect the final diff, and commit one logical correction.

## Handoff after this correction

The original parity work can then resume one top-level stage at a time, each in
a fresh Agent 1 session. That downstream work is not part of this correction.

## Excluded

No new state-machine version, migration layer, unattended daemon, dispatch
receipt system, reviewer rotation, independent implementation review, or
orchestrator-run acceptance suite.
