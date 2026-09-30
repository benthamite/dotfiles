# End-to-End Skill Hardening Design

## Goal

Make the paired `end-to-end` skill route only real-runtime verification work,
preserve the user's requested task mode, fail closed around external effects,
and define evidence strong enough to support an end-to-end completion claim.

## Approved scope

The revision implements every finding from the 2026-07-29 skill audit:

- distinguish an unfixed regression, an already-implemented change or new
  feature, and a reproduction- or verification-only request;
- state that the skill owns live acceptance evidence and never expands the
  user's authorization to edit code, contact people, or mutate external state;
- add an external-effect and cleanup preflight before the first live action;
- define what happens after a failed decisive run;
- tighten routing around `E2E`, `live`, and `reproduce`;
- correct the boundary with `verify` and the normal completion gate;
- require supporting project checks, live revision provenance, and explicit
  repetitions or thresholds for intermittent and performance symptoms;
- distinguish scheduler-body verification from scheduler-trigger verification;
- remove the duplicate affected-user retry rule;
- add reusable trigger and behavior eval scenarios;
- add the skill to the master inventory and synchronize the Claude and Codex
  README summaries.

## RED baseline

Three independent pressure scenarios exercised the current skill before edits:

1. An already-implemented signup flow with no pre-change revision was blocked
   before the requested Chrome verification because the workflow always
   requires reproducing a failure first.
2. An urgent Slack fixture and scheduled dispatch exposed a scattered approval
   rule: the first reproduction named mutating actions before a fail-closed
   safety gate, affected-user contact appeared authorized after evidence, and
   cleanup was absent.
3. An intermittent reconnect failure showed no repetition threshold, revision
   provenance requirement, cleanup rule, or diagnosis/retry loop. The
   description also routed bare automated-E2E, deployment-status, and research
   reproduction prompts.

These failures define the minimum GREEN behavior. The new eval fixture will
preserve them alongside positive and near-miss routing cases.

## Design

### Routing and ownership

The frontmatter will start with `Use when` and describe runtime-verification
conditions only. It will exclude requests merely to explain, write, review, or
run automated E2E tests; deploy something "live"; reproduce research results;
or verify pure logic already covered by automated tests.

The body will explicitly state that the skill owns the live acceptance
criterion and evidence. Debugging and implementation remain with their owning
workflows, and the skill cannot turn a verification-only request into an edit.

### Workflow modes

After stating the success criterion and completing the safety preflight, the
agent selects one mode:

- **Unfixed regression:** reproduce safely before editing when feasible, then
  return to the authorized debugging or implementation workflow.
- **Existing change or new feature:** exercise the current acceptance path
  without requiring a nonexistent failure; record an unavailable pre-change
  baseline as a gap rather than a stop.
- **Reproduction or verification only:** collect evidence and report it without
  editing.

All modes converge on supporting checks, proof that the live surface loaded the
intended revision, the decisive live run, pass/fail evaluation, cleanup, and a
precise final evidence sentence.

### Safety and cleanup

Before the first live action, the agent classifies effects as read-only, local
and disposable, or externally visible/destructive. The skill does not authorize
external mutations or communications. The agent prefers read-only or owned test
artifacts and obtains explicit confirmation before posts, sends, writes,
dispatches, scheduled runs, or similar shared-state changes. Creation and
cleanup are covered together; cleanup that was not authorized remains untouched
and is reported.

### Evidence quality

The workflow requires applicable automated/project checks but keeps them
supporting rather than decisive. It records how the running surface was tied to
the intended source revision. Intermittent, race, and performance symptoms get
a predefined environment, run count, and threshold.

If the decisive run fails, the agent cannot push, report success, or move on.
Authorized implementation work returns to diagnosis; verification-only work
reports the failure without editing. Completion requires the exact criterion,
supporting checks, and required cleanup.

A manual scheduler dispatch verifies only the job body unless the symptom is
outside the scheduler trigger. Timing, event wiring, scheduler permissions, and
delivery require an actual scheduled invocation.

## Files

- `claude/skills/end-to-end/SKILL.md`
- `codex/skills/end-to-end/SKILL.md`
- `claude/skills/end-to-end/evals/scenarios.md`
- `codex/skills/end-to-end/evals/scenarios.md`
- `claude/README.org`
- `codex/README.org`
- `agents/README.org`

The paired bodies and eval files remain identical after normalization of
Claude-only frontmatter.

## Non-goals

- No general-purpose skill-eval runner is introduced; the repository has no
  existing convention to extend.
- The skill remains one file plus a compact eval fixture. Its surface guidance
  is still short enough that splitting references would add navigation cost.
- No unrelated `ai-config-sync` failures or untracked worktree files are
  modified.

## Acceptance criteria

- The three RED scenarios follow the intended GREEN mode and safety behavior.
- Positive runtime prompts trigger; automated-test, deployment-status,
  research-reproduction, and pure-logic near misses do not.
- The resolver loads both copies.
- The normalized skill diff is empty and paired eval files are byte-identical.
- The target files pass relevant metadata/documentation tests.
- `bin/ai-config-sync audit` reports no problem concerning `end-to-end`; any
  unrelated pre-existing failures are reported separately.
- Only the planned files are staged and committed.
