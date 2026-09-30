# Drive Workspace Migration Pilots Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prove native error-record convergence and both sides of rollback with one low-risk repository before any batch migration.

**Architecture:** `agent-skills` is the single destructive pilot because it is small, clean, remote-backed, and contains the tracked symlink that best represents the newly discovered Drive failure. The pilot moves, converges in the cloud, rolls back through the supported API/UI path, proves object identity, and then migrates a second time; all other risky workspace shapes receive read-only preflights first.

**Tech Stack:** `bin/drive-workspace`, Git, Google Drive for desktop, Google Drive v3, macOS Trash, `unittest` verification artifacts.

---

This is program plan 2 of 12. Start only after the tooling plan is committed and
its complete test suite passes. Execute from the real source paths. Stop the
entire migration program—not merely this plan—on any unexpected Drive route,
duplicate cloud object, failed rollback identity check, or retained native error
that cannot be cleared through supported Retry/Dismiss and restart behavior.

## File map

- Modify only if live discovery differs: `drive-workspaces.json` — update verified smoke-command or consumer facts; keep transaction IDs only in the external journal.
- Create outside Drive: mode-`0600` hash-chained pilot journals below `~/.local/state/drive-workspace-migration/pilots/`; every mutation attempt name contains a UTC timestamp plus UUID and is bound from `capture` stdout rather than reused.
- Move transactionally: `~/My Drive/repos/agent-skills` ↔ `~/repos/agent-skills` — one rollback drill followed by the final move.
- Move through Git: `~/My Drive/repos/dont-sleep/.worktrees/safe-local-install` → `~/repos/.worktrees/dont-sleep/safe-local-install`.
- Preserve: `~/My Drive/repos/dont-sleep` — source-only main worktree remains in Drive.
- Modify only if the worktree path is recorded: supported session metadata identified by `move-session-log` audit.
- Move through four journaled path transactions: `~/My Drive/.claude/skills/{fix-drive-errors,nosync}` and `~/My Drive/.codex/skills/{fix-drive-errors,nosync}` → Trash after global resolution passes.

### Task 1: Freeze new regression sources and capture the live baseline

**Files:**

- Inspect: `~/My Drive`, DriveFS logs/databases, `drive-workspaces.json`

- [ ] **Step 1: Prove the runtime guard is active**

Run the guard's focused tests, then invoke its read-only self-test with a fake
Drive cwd. Require denials for `npm ci`, `uv sync`, and a Drive-local
`git worktree add`, and allowance for `git status`.

- [ ] **Step 2: Characterize the real Drive pause signal**

Before touching a source path, use the supported Drive UI to pause and resume
once. Capture the complete native accessibility state, read-only queue cursors,
and new log window for both states. Require the tested provider to distinguish
paused from running without relying on the absent `UPLOAD_PAUSED` token; a
visible state plus bounded queue observation must agree. Add the exact observed
fixture to `tests.test_drive_workspace`, run the state-provider tests, and
commit that characterization before any move. If no reliable signal exists,
stop and revise the transaction design.

- [ ] **Step 3: Capture the complete symlink and native-error baseline**

Run:

```bash
install -d -m 700 "$HOME/.local/state/drive-workspace-migration/pilots"
bin/drive-workspace audit --manifest drive-workspaces.json > "$HOME/.local/state/drive-workspace-migration/pilots/baseline-audit.json"
bin/drive-workspace init-journal \
  --journal "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson" \
  --kind observation --label program-baseline
bin/drive-workspace record-native-errors \
  --journal "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson"
```

The audit is expected to exit nonzero while known links and unmoved sources
remain; capture that status without `set -e`, require its JSON to report
`traversal_complete=true`, and fail if it reports an unreadable path or partial
scan. Reconcile the filesystem paths with
the native panel categories; do not infer the native count from log lines.
Record the exact Drive root ID, `is_my_drive=1`, and zero `machine_root` rows.

- [ ] **Step 4: Confirm no active process or Emacs buffer owns either pilot**

Run the exact captures:

```bash
install -d -m 700 "$HOME/.local/state/drive-workspace-migration/pilots"
bin/drive-workspace capture agent-skills \
  --journal "$HOME/.local/state/drive-workspace-migration/pilots/agent-skills-baseline.ndjson"
bin/drive-workspace capture dont-sleep-safe-local-install \
  --journal "$HOME/.local/state/drive-workspace-migration/pilots/dont-sleep-safe-local-install-preflight.ndjson"
```

Require each stdout `journal` value to equal the supplied canonical path and
run `verify-journal` on both. Any
process cwd, active agent session, scheduler, or Emacs file-visiting buffer
below a source path defers that pilot. Never signal Emacs.

### Task 2: Preflight every representative topology without mutation

**Files:**

- Inspect: six personal workspaces and the exact three Plan 6 pilots

- [ ] **Step 1: Run captures for the six personal workspaces**

For each literal name, run `bin/drive-workspace capture NAME --journal
"$HOME/.local/state/drive-workspace-migration/preflight/NAME.ndjson"` with a
new nonexistent path, then run `verify-journal` on it. Expected outcomes:

- `agent-skills`: clean and fully remote-covered;
- `80000hours.global`: clean and fully remote-covered, with Python and Next smoke commands recorded;
- `ea.news`: aggregate parent plus two submodules; API stale Dropbox `core.worktree` recorded as repairable preimage;
- `consensus-trader`, `stafforini.com`, and `tangodb`: dirty/untracked state fully journaled; uncovered commits remain hard blockers where present.

Do not weaken a blocker or push a commit.

- [ ] **Step 2: Verify destination and filesystem assumptions**

Require all six destinations absent, `~/repos/wuzapi` unchanged, and identical
`st_dev` values for `~/My Drive` and `~/repos`. A different device ends the
rename design and requires a revised specification.

- [ ] **Step 3: Preflight the exact three later Epoch pilots directly**

Plan 6 owns the 34-entry Epoch manifest and adds those entries only when it
starts, so this plan must not require manifest membership or dynamically select
a pilot. Run the read-only filesystem, Git, remote-coverage, process, buffer,
and consumer-discovery functions directly against these exact Plan 6 pilots:

```text
~/My Drive/Epoch/projects/benchmark-candidate-digest/repo → ~/repos/epoch/benchmark-candidate-digest
~/My Drive/Epoch/projects/datacenter-automation/repo → ~/repos/epoch/datacenter-automation
~/My Drive/Epoch/projects/earnings-calls-mentions/repo → ~/repos/epoch/earnings-calls-mentions
```

Write one mode-`0600` preflight journal per path under
`~/.local/state/drive-workspace-migration/epoch-preflight/`, recording the
literal source/destination and every result. Do not call manifest-only move
commands and do not move or edit any Epoch path here. Any blocker remains a
Plan 6 blocker; it does not authorize choosing a different pilot.

### Task 3: Prove stale native-record convergence without creating a new error

**Files:**

- Inspect only: Drive record `527416`, path label `node-gyp-build-test`
- Journal outside Drive: one attempt-specific native-record observation journal

- [ ] **Step 1: Bind the native UI row to the recorded failure**

Create and bind the observation journal, then record the initial visible rows:

```bash
ATTEMPT_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(uuidgen)"
REQUESTED_JOURNAL="$HOME/.local/state/drive-workspace-migration/native-records/stale-npm-${ATTEMPT_ID}.ndjson"
INIT_OUTPUT="$(bin/drive-workspace init-journal --journal "$REQUESTED_JOURNAL" --kind observation --label stale-npm-record-convergence)"
NATIVE_JOURNAL="$(printf '%s\n' "$INIT_OUTPUT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["journal"])')"
test "$NATIVE_JOURNAL" = "$REQUESTED_JOURNAL"
bin/drive-workspace record-native-errors --journal "$NATIVE_JOURNAL"
```

Use the DriveFS log timestamp `2026-08-01T19:16:07` and record ID `527416` to
identify the already-absent `UNSUPPORTED` source. Stop if the native row cannot
be tied unambiguously to that record.

- [ ] **Step 2: Exercise only supported convergence actions**

After queues settle, use the row-specific Retry or Dismiss action if present,
then a normal restart only if needed. Require the native row to disappear. A
database-only disappearance is not evidence. If it persists, stop the program.
Run `bin/drive-workspace record-native-errors --journal "$NATIVE_JOURNAL"`
after the supported action and again after any restart so the same chained
journal proves the visible transition.

- [ ] **Step 3: Clear the other removed npm-link records by exact identity**

Bind record `527417` (`node-gyp-build`) and record `527418`
(`node-gyp-build-optional`) to their visible rows and the same incident window.
For each, require its filesystem source to be absent and apply the supported
action proved in Step 2.
Require all three removed-source rows absent after one settled restart. An
unbound, still-live, or differently classified item returns to diagnosis; do
not dismiss it by resemblance.
Append the final native inventory with `bin/drive-workspace
record-native-errors --journal "$NATIVE_JOURNAL"`; the journal must show all
three exact row keys before convergence and none afterward.

### Task 4: Move `agent-skills` and prove a live path transition

**Files:**

- Move: `~/My Drive/repos/agent-skills` → `~/repos/agent-skills`
- Journal: external mode-`0600` pilot journal

For Tasks 4–5, create a new UTC-timestamp-plus-UUID attempt path, capture once,
and bind `JOURNAL` to the canonical path returned on stdout:

```bash
ATTEMPT_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(uuidgen)"
REQUESTED_JOURNAL="$HOME/.local/state/drive-workspace-migration/pilots/agent-skills-rollback-${ATTEMPT_ID}.ndjson"
CAPTURE_OUTPUT="$(bin/drive-workspace capture agent-skills --journal "$REQUESTED_JOURNAL")"
JOURNAL="$(printf '%s\n' "$CAPTURE_OUTPUT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["journal"])')"
test "$JOURNAL" = "$REQUESTED_JOURNAL"
ROLLBACK_JOURNAL="$JOURNAL"
bin/drive-workspace verify-journal --journal "$JOURNAL"
```

- [ ] **Step 1: Record cloud identity before pausing Drive**

Run `bin/drive-workspace record-cloud agent-skills --journal "$JOURNAL"`.
Require exactly one live old
folder under the journaled My Drive parent and record its object ID.

- [ ] **Step 2: Pause Drive and prove the pause is fresh**

Pause through the supported Drive UI. Run
`bin/drive-workspace record-drive-state agent-skills --journal "$JOURNAL"`
followed by `bin/drive-workspace confirm-drive-paused agent-skills --journal
"$JOURNAL"`; require the freshly
characterized native paused state and bounded stable queue cursors. A stale log
line is not acceptable evidence.

- [ ] **Step 3: Execute the atomic rename**

Run:

```bash
bin/drive-workspace move agent-skills --journal "$JOURNAL"
bin/drive-workspace apply-consumers agent-skills --journal "$JOURNAL"
bin/drive-workspace verify-consumers agent-skills --journal "$JOURNAL"
bin/drive-workspace verify-local agent-skills --journal "$JOURNAL"
bin/drive-workspace materialize-generated-paths agent-skills --journal "$JOURNAL"
bin/drive-workspace run-smoke agent-skills --journal "$JOURNAL"
bin/drive-workspace verify-local agent-skills --journal "$JOURNAL"
```

Require the tracked `.opencode/skills -> ../skills` link still resolves, every
Git snapshot field matches, the old path is absent, and the destination is the
only writable checkout. Require the exact `codex/config.toml` trust key to name
`/Users/pablostafforini/repos/agent-skills`, preserve all unrelated config
hunks, and relocate only journaled session associations through the tested
adapter.

- [ ] **Step 4: Resume Drive and verify the cloud tree**

Resume through the supported UI, wait for queues to settle, then run
`bin/drive-workspace verify-cloud agent-skills --journal "$JOURNAL"`. Require
the journaled old folder ID in Trash, no live old path, no duplicate tree, and
no computer-backup route.

- [ ] **Step 5: Prove the native failed-create record clears**

Inspect the native error item for `.opencode/skills`. If it remains after queue
settlement, use only the path-specific Retry or Dismiss action exposed by
Drive, then perform a normal Drive restart. Record the item identity and exact
transition. If it survives, stop the full program and revise the design; do not
edit Drive databases or reconnect the account.
Run `bin/drive-workspace record-native-errors --journal "$JOURNAL"` after the
row clears so `request-rollback-drill` can validate the journaled native and
cloud convergence rather than relying on prose.

### Task 5: Exercise the after-resume rollback drill

**Files:**

- Move: `~/repos/agent-skills` → original path through the tested rollback flow
- Restore: exact journaled cloud object and parent

- [ ] **Step 1: Inject the controlled post-convergence failure marker**

Run `bin/drive-workspace request-rollback-drill agent-skills --journal
"$JOURNAL"`; do not modify repository content. This validated event is the
deliberate failure trigger.

- [ ] **Step 2: Pause Drive and restore only the journaled cloud object**

Pause Drive, then capture and confirm fresh characterized native state and
stable queue-cursor evidence before restoring anything:

```bash
bin/drive-workspace record-drive-state agent-skills --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused agent-skills --journal "$JOURNAL"
bin/drive-workspace restore-cloud agent-skills --journal "$JOURNAL"
```

Require the exact journaled object ID live again at its original parent, no
replacement ID, and no duplicate. The command must not create or upload an
object.

- [ ] **Step 3: Restore the local workspace before resuming Drive**

Run:

```bash
bin/drive-workspace rollback-consumers agent-skills --journal "$JOURNAL"
bin/drive-workspace rollback-generated-paths agent-skills --journal "$JOURNAL"
bin/drive-workspace rollback-local agent-skills --journal "$JOURNAL"
```

Require the exact consumer preimages and complete local snapshot to match.
Resume Drive only after these checks pass.

- [ ] **Step 4: Verify object identity, not merely path presence**

Require the original object ID at the original parent, no replacement ID, no
duplicate path, correct My Drive routing, matching local Git/filesystem
snapshot, and the native panel back at the recorded pre-pilot category state.
Run `bin/drive-workspace verify-rollback agent-skills --journal "$JOURNAL"
--baseline-journal
"$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson"`. Any
mismatch or missing `rollback_verified` event is a global stop.

### Task 6: Migrate `agent-skills` a second time

**Files:**

- Move finally: `~/My Drive/repos/agent-skills` → `~/repos/agent-skills`

- [ ] **Step 1: Capture a new journal**

Do not reuse the rollback-drill journal. Create and bind the final journal with
these exact commands:

```bash
ATTEMPT_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(uuidgen)"
REQUESTED_JOURNAL="$HOME/.local/state/drive-workspace-migration/pilots/agent-skills-final-${ATTEMPT_ID}.ndjson"
CAPTURE_OUTPUT="$(bin/drive-workspace capture agent-skills --journal "$REQUESTED_JOURNAL")"
JOURNAL="$(printf '%s\n' "$CAPTURE_OUTPUT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["journal"])')"
test "$JOURNAL" = "$REQUESTED_JOURNAL"
FINAL_JOURNAL="$JOURNAL"
bin/drive-workspace verify-journal --journal "$JOURNAL"
bin/drive-workspace record-cloud agent-skills --journal "$JOURNAL"
```

Require remote coverage, process/buffer checks, cloud identity, and destination
absence from that fresh capture. Pause Drive through its supported UI, then
run:

```bash
bin/drive-workspace record-drive-state agent-skills --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused agent-skills --journal "$JOURNAL"
```

- [ ] **Step 2: Move and run the manifest smoke while paused**

Run the complete second transaction explicitly:

```bash
bin/drive-workspace move agent-skills --journal "$JOURNAL"
bin/drive-workspace apply-consumers agent-skills --journal "$JOURNAL"
bin/drive-workspace verify-consumers agent-skills --journal "$JOURNAL"
bin/drive-workspace verify-local agent-skills --journal "$JOURNAL"
bin/drive-workspace materialize-generated-paths agent-skills --journal "$JOURNAL"
bin/drive-workspace run-smoke agent-skills --journal "$JOURNAL"
bin/drive-workspace verify-local agent-skills --journal "$JOURNAL"
```

- [ ] **Step 3: Run rollback-sensitive Git/link assertions, then converge**

While Drive remains paused, run from `~/repos/agent-skills`:

```bash
git status --short --branch
git fsck --no-dangling
test "$(git ls-files -s .opencode/skills | awk '{print $1}')" = 120000
test "$(readlink .opencode/skills)" = ../skills
```

Require clean status and the exact tracked link. The manifest-owned
`run-smoke` already ran `claude plugin validate .` and the two hook tests; do
not run them a second time. Resume Drive through its supported UI, wait for the
tested stable-running queue condition, and run:

```bash
bin/drive-workspace verify-cloud agent-skills --journal "$JOURNAL"
bin/drive-workspace record-native-errors --journal "$JOURNAL"
```

Record the Drive PID, perform one normal restart, wait for settlement, and run
these exact commands:

```bash
bin/drive-workspace close-rollback-window --journal "$JOURNAL" \
  --baseline-journal \
  "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson" \
  --restart-before-pid "$DRIVE_PID"
bin/drive-workspace finalize-generated-paths agent-skills --journal "$JOURNAL"
```

Require the old local path absent,
original cloud tree trashed, no duplicate, tracked internal link healthy
outside Drive, and its native error absent.

- [ ] **Step 4: Commit the exact consumer change after convergence**

Run `bin/drive-workspace stage-consumers agent-skills --journal "$JOURNAL"` to
stage only the `agent-skills` trust-key
hunk in `codex/config.toml`; require all unrelated working-tree hunks unchanged.
Commit it with `codex: trust external agent-skills workspace`. If exact hunk
staging cannot be proved, stop rather than staging the whole dirty file. Do not
push.

### Task 7: Retire all four unsafe project-local skill shadows

**Files:** the four shadow directories listed in the file map

- [ ] **Step 1: Reconfirm paired global resolution and path journals**

Require Claude and Codex to resolve the committed tracked global
`fix-drive-errors` skill in the shadow-excluded test. Use these exact
label/path/journal triples:

```text
claude-fix-drive-errors | ~/My Drive/.claude/skills/fix-drive-errors | ~/.local/state/drive-workspace-migration/skill-shadows/claude-fix-drive-errors.ndjson
claude-nosync | ~/My Drive/.claude/skills/nosync | ~/.local/state/drive-workspace-migration/skill-shadows/claude-nosync.ndjson
codex-fix-drive-errors | ~/My Drive/.codex/skills/fix-drive-errors | ~/.local/state/drive-workspace-migration/skill-shadows/codex-fix-drive-errors.ndjson
codex-nosync | ~/My Drive/.codex/skills/nosync | ~/.local/state/drive-workspace-migration/skill-shadows/codex-nosync.ndjson
```

For each literal triple, run `capture-path LABEL --journal JOURNAL --path PATH
--final-type absent --target-policy retain`, `verify-journal`, and
`record-path-cloud`. Require exact hashes and parents and no unknown content;
`cloud_preimage: null` is valid and means later cloud verification must require
continued absence.

- [ ] **Step 2: Pause, move all four to Trash, and verify resolution**

Treat the four paths as one paired rollback boundary. After characterized
pause evidence recorded by first pausing Drive through the supported UI and
then running `confirm-path-paused --journal JOURNAL` in all four journals, move
only the four literal directories to Trash, and run
`verify-path-local` for every journal before resume. Then require both
live resolvers to select the tracked global skill and `nosync` to be absent.
Run `bin/ai-config-sync audit` and the skill-policy tests.

- [ ] **Step 3: Resume and verify cloud/native state**

Resume Drive through the supported UI and wait for stable running queues.
Require every journaled pre-existing cloud object trashed; a journal with
`cloud_preimage: null` must remain absent rather than inventing a fourth object.
Require no duplicate or recreated shadow and no new native error. If any check fails, pause, restore exact cloud
IDs and local preimages from the four journals, resume, and verify the original
state before stopping.

### Task 8: Relocate the inherited `dont-sleep` worktree

**Files:**

- Move through Git: old worktree path → `~/repos/.worktrees/dont-sleep/safe-local-install`
- Preserve: `~/My Drive/repos/dont-sleep`

- [ ] **Step 1: Reconfirm the worktree is inactive and remote-covered**

Create a fresh timestamp-plus-UUID transaction before reading its OID, parse
the canonical journal path from `capture` stdout as above, and require it to
equal the requested path:

```bash
ATTEMPT_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(uuidgen)"
REQUESTED_JOURNAL="$HOME/.local/state/drive-workspace-migration/pilots/dont-sleep-safe-local-install-${ATTEMPT_ID}.ndjson"
CAPTURE_OUTPUT="$(bin/drive-workspace capture dont-sleep-safe-local-install --journal "$REQUESTED_JOURNAL")"
JOURNAL="$(printf '%s\n' "$CAPTURE_OUTPUT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["journal"])')"
test "$JOURNAL" = "$REQUESTED_JOURNAL"
bin/drive-workspace verify-journal --journal "$JOURNAL"
```

Require branch `safe-local-install`, HEAD equal to the journaled OID, no process
cwd, no Emacs buffer, and no active agent session rooted at the old worktree.
Preserve the currently dirty `README.md`, `dont-sleep.sh`, and
`tests/test-dont-sleep.sh`. At design review the main branch was ahead three and
the linked-worktree HEAD had no containing remote ref, so the move remains
blocked until both exact commits pass the remote gate. Do not push implicitly.

- [ ] **Step 2: Capture cloud identity, pause, and move through the transaction tool**

Run:

```bash
bin/drive-workspace record-cloud dont-sleep-safe-local-install --journal "$JOURNAL"
# Pause Drive through the supported UI.
bin/drive-workspace record-drive-state dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace move dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace apply-consumers dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace verify-local dont-sleep-safe-local-install --journal "$JOURNAL"
```

The manifest's `kind=linked_worktree` must invoke `git worktree move` and
repair. Do not create a compatibility link at the old path.

- [ ] **Step 3: Verify Git and session state**

Require `git worktree list --porcelain` to name only the new path, status and
HEAD to match the journal, the old path absent, and the main Drive worktree
unchanged. If resumable session metadata names the old path, relocate it with
the `move-session-log` skill and verify resume from the new path.

Run `bin/drive-workspace verify-consumers dont-sleep-safe-local-install
--journal "$JOURNAL"`, then invoke the manifest-owned test exactly once:

```bash
bin/drive-workspace materialize-generated-paths dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace run-smoke dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace verify-local dont-sleep-safe-local-install --journal "$JOURNAL"
```

Require the captured dirty state to remain byte-identical and prove the smoke
ran from `~/repos/.worktrees/dont-sleep/safe-local-install`, not the old path.

- [ ] **Step 4: Resume and close cloud/native rollback gates**

Resume through the supported UI, wait for stable running queues, and run
the exact convergence commands:

```bash
bin/drive-workspace verify-cloud dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace record-native-errors --journal "$JOURNAL"
```

Capture the Drive PID, perform one normal restart, then run:

```bash
bin/drive-workspace close-rollback-window --journal "$JOURNAL" \
  --baseline-journal \
  "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson" \
  --restart-before-pid "$DRIVE_PID"
bin/drive-workspace finalize-generated-paths dont-sleep-safe-local-install \
  --journal "$JOURNAL"
```

Require the journaled old cloud worktree folder in Trash, no duplicate or
computer-backup route, and its native path record cleared. If any check fails,
pause through the supported UI and run the exact rollback sequence:

```bash
bin/drive-workspace record-drive-state dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace restore-cloud dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace rollback-consumers dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace rollback-generated-paths dont-sleep-safe-local-install --journal "$JOURNAL"
bin/drive-workspace rollback-local dont-sleep-safe-local-install --journal "$JOURNAL"
```

Resume through the supported UI, wait for stable running queues, and run:

```bash
bin/drive-workspace verify-rollback dont-sleep-safe-local-install \
  --journal "$JOURNAL" \
  --baseline-journal \
  "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson"
```

Require the original Git metadata, dirty bytes, cloud ID, parent, and native
baseline before continuing.

### Task 9: Record the program gate

**Files:**

- Journal: external program state

- [ ] **Step 1: Re-run complete audits and one Drive restart**

Require no new symlink, no old-path recreation, no stale native pilot record,
healthy Drive registration, and no unexpected cloud creation since the pilot
timestamp.

- [ ] **Step 2: Mark batching eligibility**

Run the two validated gate commands only when the exact user-visible checks
above passed:

```bash
PROGRAM_JOURNAL="$HOME/.local/state/drive-workspace-migration/program.ndjson"
bin/drive-workspace init-journal --journal "$PROGRAM_JOURNAL" \
  --kind program --label drive-migration-program
bin/drive-workspace record-program-gate --journal "$PROGRAM_JOURNAL" \
  --gate native_record_convergence_passed --evidence-journal \
  "$NATIVE_JOURNAL"
bin/drive-workspace record-program-gate --journal "$PROGRAM_JOURNAL" \
  --gate after_resume_rollback_passed --evidence-journal \
  "$ROLLBACK_JOURNAL"
```

Later plans must validate the program journal's hash chain and refuse to run
without both events.
