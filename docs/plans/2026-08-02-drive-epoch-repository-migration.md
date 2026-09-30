# Epoch Repository Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move all 34 Epoch code repositories to `~/repos/epoch/<slug>` and cut over every live service and scheduler without moving the Drive-hosted project notes.

**Architecture:** The mixed-state schema lands first with every live path unchanged. Three one-at-a-time pilots then prove that filesystem rename, the exact per-slug registry/consumer flip, local verification, and rollback form one transaction. Remaining repositories move in small topology batches; unmoved slugs always retain their old declared paths and no consumer probes both roots.

**Tech Stack:** `bin/drive-workspace`, Git, Python/uv/pytest/unittest, Node/npm, Make, launchd, Google Drive v3.

---

This is program plan 6 of 12. Start only after the path-model plan passes and
every preparatory commit needed by the next transaction is present on an
advertised remote. No implicit push or remote-gate waiver is permitted.

## File map

- Modify: `drive-workspaces.json` — add the exact 34 Epoch code transactions, the auxiliary media linked-worktree transaction, and the structured smoke records below.
- Modify: `tests/test_drive_workspace.py` — extend the Plan 1 manifest/runner tests to the 34 code entries plus the auxiliary media worktree without changing the runner contract.
- Move: each `~/My Drive/Epoch/projects/<slug>/repo` → `~/repos/epoch/<slug>`.
- Move specially: `media-mentions/.worktrees/historical-media-backfill` → `~/repos/.worktrees/media-mentions/historical-media-backfill` before its main worktree.
- Modify during cutover: canonical launchd plists and registries from the path-model plan.
- Create during dashboard cutover: `~/My Drive/repos/launchd/agents/ai.epoch.automations-dashboard.refresh.plist`; repoint the exact live `~/Library/LaunchAgents` link only inside that transaction.
- Preserve: all `~/My Drive/Epoch/projects/<slug>` notes/metadata directories, existing dirty/untracked state, historical path aliases, and every generated-directory link target until its rollback window closes.

Exact slugs:

```text
agent-readiness ai-productivity-digest analytics-aggregation anthropic-releases
automation-watchdog automations-dashboard bench-scraper benchmark-candidate-digest
benchmark-updates candidate-screening citation-tracker datacenter-automation
earnings-calls-mentions email-triage epoch-ai-info epoch-asana epoch-find
epoch-staff-availability ga-claude-proxy gdocs-addon impact-dashboard
lever-assistant link-archiver media-mentions onboarding provider-receipts
quickbooks-claude-proxy slack-auto-responder slack-maintenance slack-preview-tools
staff-data time-tracker url-health wip-summary-poster
```

### Task 1: Extend and validate the exact Epoch manifest

**Files:**

- Modify: `drive-workspaces.json`
- Modify: `tests/test_drive_workspace.py`

- [ ] **Step 1: Add failing scope tests**

Require exactly 34 `group == "epoch"` code entries, the slug set above, unique
destinations below `~/repos/epoch`, sources ending in
`Epoch/projects/<slug>/repo`, and an expected current total of 65 directory
links across the 32 affected repositories. Require no note directory as a move
source. Require every entry to own the exact `repo-paths.json` old/new
substitution and every known repository-specific trust, instruction, skill, or
scheduler consumer; occurrence counts are mandatory. For each of the 65 links,
require an exact source-relative path, `lstat` preimage, target, and one of two
post-rename dispositions: `preserve_link` or `materialize_generated_directory`.

Require every smoke item to contain only `cwd`, `argv`, and `env`;
reject shell strings and shell metacharacters. Add runner tests proving that
each attempted item receives its own recorded exit status, one nonzero smoke
status stops later smoke records and fails the transaction. Reuse Plan 1's
single tested `finally` cleanup owner; no Epoch manifest entry may add a second
cleanup command. The task-owned temporary root
used in `env` must be outside My Drive and the moved repository.

Require one additional auxiliary entry named
`media-mentions-historical-media-backfill` with
`kind=linked_worktree`, source
`~/My Drive/Epoch/projects/media-mentions/repo/.worktrees/historical-media-backfill`,
destination `~/repos/.worktrees/media-mentions/historical-media-backfill`, owner
`~/My Drive/Epoch/projects/media-mentions/repo`, no path-map consumer, and one
read-only smoke record
`{"cwd":".","argv":["git","status","--porcelain=v2"],"env":{}}`. Keep this
auxiliary record outside the 34-code-entry count.

Extend the Plan 1 transaction tests for `materialize-generated-paths` and
`rollback-generated-paths` with Epoch entries. The former may act only on manifest entries marked
`materialize_generated_directory`: after the intact after-rename check, it
moves the link itself to recoverable Trash without following it, records its
restore identity, and retains the external target. The latter moves a generated
real replacement to Trash and restores the exact link preimage. Both commands
must reject concurrent path drift and preserve unrelated paths.

- [ ] **Step 2: Add all entries and smoke commands**

Populate the 34 code entries with their exact source, destination, Git roots,
65-link preimage subset, live consumers, and one or more structured records
from the smoke map in Task 4. Populate the auxiliary entry with its separately
declared fields and smoke record. `automations-dashboard`, `time-tracker`, and
`media-mentions` must be marked `special` so batch mode refuses them.
The auxiliary worktree transaction must close before capturing the main
`media-mentions` entry; each `move` preflight therefore sees the same
Git/worktree snapshot that its own `capture` recorded.

Reuse the Plan 1 smoke runner with a transaction-owned `transaction_tmp`
created outside Drive. Expand `{transaction_tmp}` only in manifest environment
values; reject it in `cwd` and every argument. Run all `smoke` records in order
and rely on Plan 1's `finally` block to remove the exact tool-owned temporary
root after success or failure. A cleanup failure fails the transaction but
never hides the original smoke failure.

Use the already exposed `materialize-generated-paths NAME --journal JOURNAL`
and `rollback-generated-paths NAME --journal JOURNAL` with the extended tests
from Step 1. Neither command may remove an external target. Keep the trashed link
preimage addressable until local and cloud verification close the rollback
window.

- [ ] **Step 3: Run manifest and audit tests**

Run `python3 -m unittest tests.test_drive_workspace -v` and a read-only capture
for all 34 code entries plus the auxiliary media worktree. Require no unknown
nested repository.

- [ ] **Step 4: Commit the Epoch manifest**

```bash
git add drive-workspaces.json tests/test_drive_workspace.py
git commit -m "drive: register Epoch repository migrations"
```

### Task 2: Resolve or record all hard blockers

**Files:**

- Inspect: all 34 Git roots and registered worktrees

- [ ] **Step 1: Fresh-fetch and apply remote coverage**

At design review these units were blocked and must be remeasured:

- `agent-readiness`: untracked `uv.lock`; branch `multi-lane-benchmark` lacked an upstream;
- `anthropic-releases`: local-only HEAD `54d9075`;
- `epoch-asana`: local-only HEAD `e566cf5`;
- `staff-data`: local-only HEAD `3d2ac38`;
- `media-mentions/.worktrees/historical-media-backfill`: local-only HEAD `672ebc4`;
- every repository changed by the path-model plan until its new commit is remote-covered.

Preserve untracked state; only uncovered commits block. Report exact OIDs and
stop that unit. Do not push.

- [ ] **Step 2: Record process, buffer, session, and scheduler owners**

Defer any active repository. For dashboard and Time Tracker, record launchd
labels, live PID/cwd, plist source, log paths, and next scheduled trigger.

- [ ] **Step 3: Bind the plan-2 read-only preflights to the fixed pilots**

Require plan 2's execution record to contain read-only captures for exactly
`benchmark-candidate-digest`, `datacenter-automation`, and
`earnings-calls-mentions`. These are the three fixed plan-6 pilots; there is no
dynamic fourth pilot and no manifest-membership requirement in plan 2. Repeat
their captures here if any recorded fact is stale.

### Task 3: Run three one-at-a-time Epoch pilots

**Files:**

- Move individually: `benchmark-candidate-digest`, `datacenter-automation`, `earnings-calls-mentions`

Each pilot performs two complete transactions. The first reaches local and
cloud convergence, records the verification-only failure event
`rollback_drill_requested`, and proves after-resume rollback. The second uses a
fresh journal and leaves the repository migrated. Do not commit path-map,
registry, or consumer changes during the rollback drill.

- [ ] **Step 1: Capture fresh rollback-drill journals**

In this exact order, capture `benchmark-candidate-digest`,
`datacenter-automation`, and `earnings-calls-mentions`. For each journal,
fresh-fetch and pass remote coverage, record the old cloud object ID and parent,
capture the complete local and consumer preimages, require the destination
absent, and defer on any active owner. Finish both transactions for one pilot
before beginning the next. Set `SLUG` to the current literal name, create an
attempt ID from a UTC timestamp plus `uuidgen`, and request a previously absent
path:

```bash
ATTEMPT_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(uuidgen)"
REQUESTED_JOURNAL="$HOME/.local/state/drive-workspace-migration/epoch-pilots/${SLUG}-rollback-${ATTEMPT_ID}.ndjson"
CAPTURE_OUTPUT="$(bin/drive-workspace capture "$SLUG" --journal "$REQUESTED_JOURNAL")"
JOURNAL="$(printf '%s\n' "$CAPTURE_OUTPUT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["journal"])')"
test "$JOURNAL" = "$REQUESTED_JOURNAL"
bin/drive-workspace verify-journal --journal "$JOURNAL"
bin/drive-workspace record-cloud "$SLUG" --journal "$JOURNAL"
```

- [ ] **Step 2: Rename, flip only the current slug, and verify the intact move**

For the current pilot, pause Drive, recheck the journal, and atomically rename
the repository. Before rebuilding the generated registry, apply that slug's
exact old-to-new `repo-paths.json` substitution and its manifest-owned consumer
hunks. Rebuild the registry and require the note path unchanged, the code path
exactly `~/repos/epoch/<slug>`, every other slug's path byte-identical, and the
registry checker plus pre-commit hook passing.

Run the transaction boundary with the bound pilot journal:

```bash
bin/drive-workspace record-drive-state "$SLUG" --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused "$SLUG" --journal "$JOURNAL"
bin/drive-workspace move "$SLUG" --journal "$JOURNAL"
bin/drive-workspace apply-consumers "$SLUG" --journal "$JOURNAL"
bin/drive-workspace verify-consumers "$SLUG" --journal "$JOURNAL"
bin/drive-workspace verify-local "$SLUG" --journal "$JOURNAL"
```

Before any smoke command, verify every journaled generated-directory link at
the corresponding destination-relative path with the same `lstat` type and
target. This after-rename check proves the filesystem move preserved the
captured link state; later approved runtime materialization is a separate
journaled action.

- [ ] **Step 3: Run each pilot's structured smoke records**

Invoke the generic Plan 1 machinery with the current pilot's bound journal:

```bash
bin/drive-workspace materialize-generated-paths "$SLUG" --journal "$JOURNAL"
bin/drive-workspace run-smoke "$SLUG" --journal "$JOURNAL"
bin/drive-workspace verify-local "$SLUG" --journal "$JOURNAL"
```

The records below are the exact expected manifest inputs to that single
`run-smoke` invocation; do not execute them manually a second time.

For `benchmark-candidate-digest`, run this record and require status zero:

```json
{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}
```

For `datacenter-automation`, give the runner a fresh external
`{transaction_tmp}` and
run the compile record with bytecode directed outside the repository:

```json
{"cwd":".","argv":["python3","-m","compileall","-q","."],"env":{"PYTHONPYCACHEPREFIX":"{transaction_tmp}/pycache"}}
```

Require no `__pycache__` or `.pyc` below the moved repository. Do not use
`python3 -B -m compileall`: `-B` does not suppress files explicitly written by
`compileall`.

For `earnings-calls-mentions`, give the runner a fresh external
`{transaction_tmp}`, run
this record, and require status zero:

```json
{"cwd":".","argv":["uv","run","--with","pandas","--with","openai","--with","requests","--with","beautifulsoup4","python","-m","unittest","discover","-s","tests","-v"],"env":{"UV_CACHE_DIR":"{transaction_tmp}/uv-cache"}}
```

Require dependency/cache state outside Drive and preserve every pre-existing
dirty, untracked, ignored, and linked path recorded by the journal.

- [ ] **Step 4: Resume and prove cloud convergence before requesting rollback**

Resume Drive only after all local checks pass. Wait for settlement and require
the exact journaled old cloud object ID in Trash at its original parent, no live
object at the old path, no replacement ID, no duplicate or computer-backup
route, and the expected native path-level transition. Persist this evidence
before the rollback event by running:

```bash
bin/drive-workspace verify-cloud "$SLUG" --journal "$JOURNAL"
bin/drive-workspace record-native-errors --journal "$JOURNAL"
```

- [ ] **Step 5: Inject the named verification-only failure and restore cloud**

Run `bin/drive-workspace request-rollback-drill "$SLUG" --journal
"$JOURNAL"` to
append the validated `rollback_drill_requested` event and mark the transaction
as a deliberate verification failure. The command must not modify repository,
consumer, filesystem, or cloud state. Then pause Drive, capture fresh
characterized native state and stable queue-cursor evidence, and run:

```bash
bin/drive-workspace record-drive-state "$SLUG" --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused "$SLUG" --journal "$JOURNAL"
bin/drive-workspace restore-cloud "$SLUG" --journal "$JOURNAL"
```

Require the exact original object ID live again at its original parent, no
replacement ID, and no duplicate before changing local state. The command must
only clear the journaled object's `trashed` field; it must not upload, copy,
create, delete, or select by name.

- [ ] **Step 6: Restore consumers, generated-link preimages, and the local path**

While Drive remains paused, run:

```bash
bin/drive-workspace rollback-consumers "$SLUG" --journal "$JOURNAL"
bin/drive-workspace rollback-generated-paths "$SLUG" --journal "$JOURNAL"
bin/drive-workspace rollback-local "$SLUG" --journal "$JOURNAL"
```

`rollback-generated-paths` moves any smoke-created real directory to Trash and
restores the exact captured link without touching its retained target. Require
the old map entry, generated registry, every consumer preimage, and complete
local snapshot to match before resuming Drive.

- [ ] **Step 7: Resume and close each rollback drill**

After resuming, require the original cloud object ID at the original parent, no
replacement ID or duplicate, correct My Drive routing, the matching local
Git/filesystem snapshot, and the native panel back at its recorded pre-pilot
category state. Any mismatch stops the full program. Require the validated
terminal `rollback_verified` event by running:

```bash
bin/drive-workspace verify-rollback "$SLUG" --journal "$JOURNAL" \
  --baseline-journal \
  "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson"
```

Do not
append an `after_resume_rollback_passed` event directly or use a generic journal
writer.

- [ ] **Step 8: Remigrate each pilot with a fresh journal**

For each pilot in the same order, create a new journal; do not reuse the drill
journal. Create a new timestamp-plus-UUID attempt ID, request
`$HOME/.local/state/drive-workspace-migration/epoch-pilots/${SLUG}-final-${ATTEMPT_ID}.ndjson`,
and bind `JOURNAL` to the canonical absolute path with these commands:

```bash
ATTEMPT_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(uuidgen)"
REQUESTED_JOURNAL="$HOME/.local/state/drive-workspace-migration/epoch-pilots/${SLUG}-final-${ATTEMPT_ID}.ndjson"
CAPTURE_OUTPUT="$(bin/drive-workspace capture "$SLUG" --journal "$REQUESTED_JOURNAL")"
JOURNAL="$(printf '%s\n' "$CAPTURE_OUTPUT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["journal"])')"
test "$JOURNAL" = "$REQUESTED_JOURNAL"
bin/drive-workspace verify-journal --journal "$JOURNAL"
bin/drive-workspace record-cloud "$SLUG" --journal "$JOURNAL"
```

Pause Drive through the supported UI, then repeat the exact Step 2 transaction
boundary, that slug's map/consumer flip before the registry rebuild, the
after-rename link verification, and the Step 3 structured smoke records. Resume
Drive, wait for stable running queues, and persist the evidence required for
closure:

```bash
bin/drive-workspace verify-consumers "$SLUG" --journal "$JOURNAL"
bin/drive-workspace verify-local "$SLUG" --journal "$JOURNAL"
bin/drive-workspace verify-cloud "$SLUG" --journal "$JOURNAL"
bin/drive-workspace record-native-errors --journal "$JOURNAL"
```

Do not append `rollback_drill_requested`. Leave the repository at its exact
external destination.

Only after controlled local verification and cloud convergence pass, stage and
commit the exact path-map, generated-registry, and consumer hunks owned by that
slug. Require all other 33 registry entries byte-identical. If staging or commit
verification fails, pause Drive and restore the original cloud object first.
Branch exactly once: if a plan-created commit exists, revert it with `git
revert --no-edit <oid>`, verify the journaled consumer bytes, and do not call
`rollback-consumers`; if no commit exists, run `bin/drive-workspace
rollback-consumers "$SLUG" --journal "$JOURNAL"`. Then run
`bin/drive-workspace rollback-generated-paths "$SLUG" --journal "$JOURNAL"`
and `bin/drive-workspace rollback-local "$SLUG" --journal "$JOURNAL"` and
perform the identity checks from Step 7 before beginning another pilot.

After the commit and post-commit checks pass, capture the Drive PID, perform
the required normal restart, then run:

```bash
bin/drive-workspace close-rollback-window --journal "$JOURNAL" \
  --baseline-journal \
  "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson" \
  --restart-before-pid "$DRIVE_PID"
bin/drive-workspace finalize-generated-paths "$SLUG" --journal "$JOURNAL"
```

Never finalize a target before this close.

- [ ] **Step 9: Gate batch mode**

Require all six journals complete—one after-resume rollback drill and one final
migration per pilot—registry consumers correct, all three external destinations
live, all three old paths absent, expected path-level native records cleared,
no duplicate cloud tree, and one fresh Drive restart with no recreated old repo
path.

### Task 4: Move routine Python and Node repositories in bounded batches

**Files:** remaining routine manifest entries

- [ ] **Step 1: Use this exact smoke-command map**

Every item below is a separate ordered record. Encode no `cd`, environment
assignment, semicolon, `&&`, or other shell string. The runner must execute and
persist the status of every record; all statuses must be zero.

- `agent-readiness`
  - `{"cwd":".","argv":["uv","run","--extra","dev","python","-m","pytest","-q","-p","no:cacheprovider"],"env":{}}`
- `ai-productivity-digest`
  - `{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}`
- `analytics-aggregation`
  - `{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}`
- `anthropic-releases`
  - `{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}`
- `automation-watchdog`
  - `{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}`
- `automations-dashboard`
  - `{"cwd":".","argv":["npm","ci"],"env":{}}`
  - `{"cwd":".","argv":["make","test"],"env":{}}`
  - `{"cwd":".","argv":["make","validate"],"env":{}}`
- `bench-scraper`
  - `{"cwd":".","argv":["make","test"],"env":{}}`
- `benchmark-candidate-digest`
  - `{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}`
- `benchmark-updates`
  - `{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}`
- `candidate-screening`
  - `{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}`
- `citation-tracker`
  - `{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}`
- `datacenter-automation`
  - `{"cwd":".","argv":["python3","-m","compileall","-q","."],"env":{"PYTHONPYCACHEPREFIX":"{transaction_tmp}/pycache"}}`
- `earnings-calls-mentions`
  - `{"cwd":".","argv":["uv","run","--with","pandas","--with","openai","--with","requests","--with","beautifulsoup4","python","-m","unittest","discover","-s","tests","-v"],"env":{"UV_CACHE_DIR":"{transaction_tmp}/uv-cache"}}`
- `email-triage`
  - `{"cwd":".","argv":["python3","-m","pytest","-q","-p","no:cacheprovider"],"env":{}}`
- `epoch-ai-info`
  - `{"cwd":".","argv":["npm","ci"],"env":{}}`
  - `{"cwd":".","argv":["npm","run","verify"],"env":{}}`
- `epoch-asana`
  - `{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}`
  - `{"cwd":"worker","argv":["npm","ci"],"env":{}}`
  - `{"cwd":"worker","argv":["npm","test"],"env":{}}`
- `epoch-find`
  - `{"cwd":".","argv":["npm","ci"],"env":{}}`
  - `{"cwd":".","argv":["npm","run","verify"],"env":{}}`
- `epoch-staff-availability`
  - `{"cwd":".","argv":["npm","ci"],"env":{}}`
  - `{"cwd":".","argv":["npm","run","verify"],"env":{}}`
- `ga-claude-proxy`
  - `{"cwd":".","argv":["npm","ci"],"env":{}}`
  - `{"cwd":".","argv":["npm","run","verify"],"env":{}}`
- `gdocs-addon`
  - `{"cwd":".","argv":["npm","ci"],"env":{}}`
  - `{"cwd":".","argv":["npm","test"],"env":{}}`
- `impact-dashboard`
  - `{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}`
- `lever-assistant`
  - `{"cwd":".","argv":["npm","ci"],"env":{}}`
  - `{"cwd":".","argv":["npm","test"],"env":{}}`
  - `{"cwd":".","argv":["npm","run","build"],"env":{}}`
- `link-archiver`
  - `{"cwd":".","argv":["python3","-m","pytest","tests/","-q","-p","no:cacheprovider"],"env":{}}`
- `media-mentions`
  - `{"cwd":".","argv":["python3","-m","unittest","discover","-s","tests","-v"],"env":{}}`
- `onboarding`
  - `{"cwd":".","argv":["npm","ci"],"env":{}}`
  - `{"cwd":".","argv":["npm","run","verify"],"env":{}}`
- `provider-receipts`
  - `{"cwd":".","argv":["python3","-m","pytest","-q","-p","no:cacheprovider"],"env":{}}`
- `quickbooks-claude-proxy`
  - `{"cwd":".","argv":["npm","ci"],"env":{}}`
  - `{"cwd":".","argv":["npm","run","verify"],"env":{}}`
- `slack-auto-responder`
  - `{"cwd":".","argv":["uv","run","--with","requests","--with","beautifulsoup4","python","-m","unittest","discover","-s","tests","-v"],"env":{"UV_CACHE_DIR":"{transaction_tmp}/uv-cache"}}`
- `slack-maintenance`
  - `{"cwd":".","argv":["uv","run","pytest","-q","-p","no:cacheprovider"],"env":{}}`
- `slack-preview-tools`
  - `{"cwd":".","argv":["python3","-m","pytest","-q","-p","no:cacheprovider"],"env":{}}`
- `staff-data`
  - `{"cwd":".","argv":["python3","-m","pytest",".","-p","no:cacheprovider"],"env":{}}`
- `time-tracker`
  - `{"cwd":".","argv":["make","test"],"env":{}}`
  - `{"cwd":".","argv":["make","update-current"],"env":{}}`
- `url-health`
  - `{"cwd":".","argv":["uv","run","--with","pytest","python","-m","pytest","-q","-p","no:cacheprovider"],"env":{}}`
- `wip-summary-poster`
  - `{"cwd":".","argv":["python3","-m","unittest","-v","test_post.py"],"env":{}}`

Before a smoke command that replaces a generated-directory root, run
`materialize-generated-paths` for the manifest-owned paths. This is mandatory
for every existing `node_modules` link before `npm ci` and for any `.next`,
`.open-next/server-functions/default/node_modules`, `build`, or `dist` link
whose recorded owner command removes its root. Keep the old external targets
through rollback. After smoke, require the replacement to be a functional real
directory outside Drive and record the transition from the captured link; do
not count it as a lost repository-state item.

- [ ] **Step 2: Move routine Python batches**

After the three pilots, process these in groups of at most four, with one
shared-registry rollback boundary per group:

```bash
BATCH_NAMES=(ai-productivity-digest analytics-aggregation automation-watchdog bench-scraper)
BATCH_NAMES=(benchmark-updates candidate-screening citation-tracker email-triage)
BATCH_NAMES=(impact-dashboard link-archiver provider-receipts slack-auto-responder)
BATCH_NAMES=(slack-maintenance slack-preview-tools url-health wip-summary-poster)
```

Use exactly one assignment line per batch, in order, and keep the following
commands in the same executor shell until that batch is closed or rolled back.
Every repository receives its own unambiguous journal and smoke evidence:

```bash
umask 077
declare -A BATCH_JOURNALS=()
for NAME in "${BATCH_NAMES[@]}"; do
  ATTEMPT_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(uuidgen)"
  REQUESTED_JOURNAL="$HOME/.local/state/drive-workspace-migration/epoch-batches/${NAME}-${ATTEMPT_ID}.ndjson"
  CAPTURE_OUTPUT="$(bin/drive-workspace capture "$NAME" --journal "$REQUESTED_JOURNAL")"
  JOURNAL="$(printf '%s\n' "$CAPTURE_OUTPUT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["journal"])')"
  test "$JOURNAL" = "$REQUESTED_JOURNAL"
  BATCH_JOURNALS["$NAME"]="$JOURNAL"
  bin/drive-workspace verify-journal --journal "$JOURNAL"
  bin/drive-workspace record-cloud "$NAME" --journal "$JOURNAL"
done
```

Pause Drive once only after every capture succeeds. For each selected unit,
bind its journal from the array and run:

```bash
for NAME in "${BATCH_NAMES[@]}"; do
  JOURNAL="${BATCH_JOURNALS[$NAME]}"
  test -n "$JOURNAL"
  bin/drive-workspace record-drive-state "$NAME" --journal "$JOURNAL"
  bin/drive-workspace confirm-drive-paused "$NAME" --journal "$JOURNAL"
  bin/drive-workspace move "$NAME" --journal "$JOURNAL"
  bin/drive-workspace apply-consumers "$NAME" --journal "$JOURNAL"
  bin/drive-workspace verify-consumers "$NAME" --journal "$JOURNAL"
  bin/drive-workspace verify-local "$NAME" --journal "$JOURNAL"
  bin/drive-workspace materialize-generated-paths "$NAME" --journal "$JOURNAL"
  bin/drive-workspace run-smoke "$NAME" --journal "$JOURNAL"
  bin/drive-workspace verify-local "$NAME" --journal "$JOURNAL"
done
```

Apply only the selected units' exact `repo-paths.json` and manifest consumers
before rebuilding the registry, require unmoved entries unchanged, and verify
every captured generated-directory link immediately after rename. Stage only
the exact manifest-owned hunks, but do not commit them yet.

A failure before resume runs this exact sequence for every renamed unit in
reverse order while Drive remains paused:

```bash
for ((INDEX=${#BATCH_NAMES[@]}-1; INDEX>=0; INDEX--)); do
  NAME="${BATCH_NAMES[$INDEX]}"
  JOURNAL="${BATCH_JOURNALS[$NAME]}"
  bin/drive-workspace rollback-consumers "$NAME" --journal "$JOURNAL"
  bin/drive-workspace rollback-generated-paths "$NAME" --journal "$JOURNAL"
  bin/drive-workspace rollback-local "$NAME" --journal "$JOURNAL"
done
```

After local success, resume and require every original cloud object still live
at its recorded parent.

After local success, resume, wait for stable running queues, and persist all
closure evidence for every unit:

```bash
for NAME in "${BATCH_NAMES[@]}"; do
  JOURNAL="${BATCH_JOURNALS[$NAME]}"
  bin/drive-workspace verify-consumers "$NAME" --journal "$JOURNAL"
  bin/drive-workspace verify-local "$NAME" --journal "$JOURNAL"
  bin/drive-workspace verify-cloud "$NAME" --journal "$JOURNAL"
  bin/drive-workspace record-native-errors --journal "$JOURNAL"
done
```

Any after-resume failure pauses Drive again and runs:

```bash
for NAME in "${BATCH_NAMES[@]}"; do
  JOURNAL="${BATCH_JOURNALS[$NAME]}"
  bin/drive-workspace record-drive-state "$NAME" --journal "$JOURNAL"
  bin/drive-workspace confirm-drive-paused "$NAME" --journal "$JOURNAL"
  bin/drive-workspace restore-cloud "$NAME" --journal "$JOURNAL"
done
```

Require every original ID and parent with no replacement or duplicate, then
run the exact reverse rollback block above for every unit before resuming. Wait
for stable running queues and run:

```bash
for NAME in "${BATCH_NAMES[@]}"; do
  JOURNAL="${BATCH_JOURNALS[$NAME]}"
  bin/drive-workspace verify-rollback "$NAME" --journal "$JOURNAL" \
    --baseline-journal \
    "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson"
done
```

Require the complete batch preimage. Only after all local, cloud, and native
checks pass may the executor create the batch's one shared
path-map/generated-registry commit and any separate manifest-owned
consumer-repository commits.

- [ ] **Step 3: Move Node/worker batches**

Process at most three at a time:

```bash
BATCH_NAMES=(epoch-ai-info epoch-find epoch-staff-availability)
BATCH_NAMES=(ga-claude-proxy gdocs-addon lever-assistant)
BATCH_NAMES=(onboarding quickbooks-claude-proxy)
```

Use exactly one assignment line per batch, in order, and execute the complete
capture, pause, move, consumer, smoke, convergence, rollback, and closure
protocol from Step 2. Run `npm ci` only after the owner repository is outside
Drive.

- [ ] **Step 4: Move resolved special blockers**

Move `agent-readiness`, `anthropic-releases`, `epoch-asana`, and `staff-data`
as one final batch by setting
`BATCH_NAMES=(agent-readiness anthropic-releases epoch-asana staff-data)` and
executing the complete Step 2 protocol. Begin only after fresh remote coverage.
Preserve every recorded untracked/dirty path.
When `agent-readiness` moves, verify its paired instruction files resolve the
new exact registry entry. Flip `ai-productivity-digest` and `email-triage`
Codex trust keys only inside those repositories' respective move transactions,
preserving unrelated `codex/config.toml` hunks.

- [ ] **Step 5: Verify and close each batch commit boundary**

After creating the deferred commits, rerun the registry checker, exact consumer
audit, and every recorded smoke status check. If a commit or post-commit check
fails, pause Drive and run the exact per-unit `record-drive-state`,
`confirm-drive-paused`, and `restore-cloud` loop from Step 2, requiring each
original object ID and parent with no replacement or duplicate. Then revert
only the plan-created commits in reverse creation order with
`git revert --no-edit <oid>` (never reset or rewrite history), verify the
journaled consumer bytes, and run this reverse local rollback without
`rollback-consumers`:

```bash
for ((INDEX=${#BATCH_NAMES[@]}-1; INDEX>=0; INDEX--)); do
  NAME="${BATCH_NAMES[$INDEX]}"
  JOURNAL="${BATCH_JOURNALS[$NAME]}"
  bin/drive-workspace rollback-generated-paths "$NAME" --journal "$JOURNAL"
  bin/drive-workspace rollback-local "$NAME" --journal "$JOURNAL"
done
```

Resume and run the exact per-unit `verify-rollback` loop from Step 2. Require
the complete local preimage, and record all revert OIDs and restored object IDs
before another batch begins.

When the post-commit checks pass, capture one pre-restart Drive PID for the
batch, perform one normal restart, and run:

```bash
for NAME in "${BATCH_NAMES[@]}"; do
  JOURNAL="${BATCH_JOURNALS[$NAME]}"
  bin/drive-workspace close-rollback-window --journal "$JOURNAL" \
    --baseline-journal \
    "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson" \
    --restart-before-pid "$DRIVE_PID"
  bin/drive-workspace finalize-generated-paths "$NAME" --journal "$JOURNAL"
done
```

A close or finalization failure stops before the next batch and never weakens
target retention.

### Task 5: Move `media-mentions` with its linked worktree

**Files:**

- Move worktree first to `~/repos/.worktrees/media-mentions/historical-media-backfill`
- Move main repository to `~/repos/epoch/media-mentions`

- [ ] **Step 1: Require both HEADs remote-covered and capture the worktree**

No `--force` or implicit push. Record both `data/chrome_profile` link targets.
Require the main and linked-worktree HEADs remote-covered, then create the
auxiliary journal:

```bash
WORKTREE_NAME=media-mentions-historical-media-backfill
ATTEMPT_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(uuidgen)"
REQUESTED_JOURNAL="$HOME/.local/state/drive-workspace-migration/epoch-special/${WORKTREE_NAME}-${ATTEMPT_ID}.ndjson"
CAPTURE_OUTPUT="$(bin/drive-workspace capture "$WORKTREE_NAME" --journal "$REQUESTED_JOURNAL")"
WORKTREE_JOURNAL="$(printf '%s\n' "$CAPTURE_OUTPUT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["journal"])')"
test "$WORKTREE_JOURNAL" = "$REQUESTED_JOURNAL"
bin/drive-workspace verify-journal --journal "$WORKTREE_JOURNAL"
bin/drive-workspace record-cloud "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
```

- [ ] **Step 2: Complete the linked-worktree transaction first**

Pause Drive through the supported UI and run:

```bash
bin/drive-workspace record-drive-state "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace confirm-drive-paused "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace move "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace apply-consumers "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace verify-consumers "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace verify-local "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace materialize-generated-paths "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace run-smoke "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace verify-local "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
```

Require `git worktree list --porcelain`, the linked HEAD/status, and both Git
metadata directions to name only the external worktree. Resume Drive, wait for
stable queues, then run:

```bash
bin/drive-workspace verify-cloud "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace record-native-errors --journal "$WORKTREE_JOURNAL"
```

Capture the Drive PID, perform the required restart, and run:

```bash
bin/drive-workspace close-rollback-window --journal "$WORKTREE_JOURNAL" \
  --baseline-journal \
  "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson" \
  --restart-before-pid "$DRIVE_PID"
bin/drive-workspace finalize-generated-paths "$WORKTREE_NAME" \
  --journal "$WORKTREE_JOURNAL"
```

On failure before close, pause Drive and run:

```bash
bin/drive-workspace record-drive-state "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace confirm-drive-paused "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace restore-cloud "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace rollback-consumers "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace rollback-generated-paths "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
bin/drive-workspace rollback-local "$WORKTREE_NAME" --journal "$WORKTREE_JOURNAL"
```

Resume Drive, wait for stable queues, and run:

```bash
bin/drive-workspace verify-rollback "$WORKTREE_NAME" \
  --journal "$WORKTREE_JOURNAL" \
  --baseline-journal \
  "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson"
```

Do not capture the main repository until the worktree transaction has either
closed or fully rolled back.

- [ ] **Step 3: Capture and move the now-stable main repository**

After Step 2 closes, capture the main repository's new stable worktree
registration:

```bash
NAME=media-mentions
ATTEMPT_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(uuidgen)"
REQUESTED_JOURNAL="$HOME/.local/state/drive-workspace-migration/epoch-special/${NAME}-${ATTEMPT_ID}.ndjson"
CAPTURE_OUTPUT="$(bin/drive-workspace capture "$NAME" --journal "$REQUESTED_JOURNAL")"
JOURNAL="$(printf '%s\n' "$CAPTURE_OUTPUT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["journal"])')"
test "$JOURNAL" = "$REQUESTED_JOURNAL"
bin/drive-workspace verify-journal --journal "$JOURNAL"
bin/drive-workspace record-cloud "$NAME" --journal "$JOURNAL"
```

Pause Drive through the supported UI and run:

```bash
bin/drive-workspace record-drive-state media-mentions --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused media-mentions --journal "$JOURNAL"
bin/drive-workspace move media-mentions --journal "$JOURNAL"
bin/drive-workspace apply-consumers media-mentions --journal "$JOURNAL"
bin/drive-workspace verify-consumers media-mentions --journal "$JOURNAL"
bin/drive-workspace verify-local media-mentions --journal "$JOURNAL"
bin/drive-workspace materialize-generated-paths media-mentions --journal "$JOURNAL"
bin/drive-workspace run-smoke media-mentions --journal "$JOURNAL"
bin/drive-workspace verify-local media-mentions --journal "$JOURNAL"
```

The manifest runner owns the main unittest suite; do not run it manually a
second time. Flip the `media-mentions` map/consumer hunks before rebuilding the
registry, verify all captured links immediately after rename, and require the
external linked worktree still registered with its exact HEAD/status.

- [ ] **Step 4: Converge, commit, close, or roll back the main transaction**

Keep the shared configuration hunks uncommitted. Resume Drive, wait for stable
running queues, and run:

```bash
bin/drive-workspace verify-consumers media-mentions --journal "$JOURNAL"
bin/drive-workspace verify-local media-mentions --journal "$JOURNAL"
bin/drive-workspace verify-cloud media-mentions --journal "$JOURNAL"
bin/drive-workspace record-native-errors --journal "$JOURNAL"
```

Require the old main path absent and cloud/native convergence, then create the
exact deferred shared configuration commit and rerun the worktree, smoke,
registry, and consumer checks. Capture the Drive PID, perform the required
restart, and run:

```bash
bin/drive-workspace close-rollback-window --journal "$JOURNAL" \
  --baseline-journal \
  "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson" \
  --restart-before-pid "$DRIVE_PID"
bin/drive-workspace finalize-generated-paths media-mentions --journal "$JOURNAL"
```

An after-resume failure before close pauses Drive and runs:

```bash
bin/drive-workspace record-drive-state media-mentions --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused media-mentions --journal "$JOURNAL"
bin/drive-workspace restore-cloud media-mentions --journal "$JOURNAL"
```

Verify the original cloud ID and parent. Before commit, run
`bin/drive-workspace rollback-consumers media-mentions --journal "$JOURNAL"`;
after commit, revert only the exact plan-created commit and do not run
`rollback-consumers`. Then run
`bin/drive-workspace rollback-generated-paths media-mentions --journal
"$JOURNAL"` and `bin/drive-workspace rollback-local media-mentions --journal
"$JOURNAL"`, resume, and run `bin/drive-workspace verify-rollback
media-mentions --journal "$JOURNAL" --baseline-journal
"$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson"`. Require
the main preimage and the already-closed external linked worktree to remain
exact.

### Task 6: Cut over Time Tracker

**Files:** time-tracker repository, two canonical launchd plists, live jobs

- [ ] **Step 1: Stop only the two Time Tracker jobs**

Obtain the user's explicit confirmation required by the launchd repository;
without it, defer this transaction. Then boot out
`ai.epoch.time-tracker.server` and `ai.epoch.time-tracker.updater`. Record prior
state; do not signal Emacs. After both jobs are absent, create and bind the
transaction:

```bash
NAME=time-tracker
ATTEMPT_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(uuidgen)"
REQUESTED_JOURNAL="$HOME/.local/state/drive-workspace-migration/epoch-special/${NAME}-${ATTEMPT_ID}.ndjson"
CAPTURE_OUTPUT="$(bin/drive-workspace capture "$NAME" --journal "$REQUESTED_JOURNAL")"
JOURNAL="$(printf '%s\n' "$CAPTURE_OUTPUT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["journal"])')"
test "$JOURNAL" = "$REQUESTED_JOURNAL"
bin/drive-workspace verify-journal --journal "$JOURNAL"
bin/drive-workspace record-cloud "$NAME" --journal "$JOURNAL"
```

- [ ] **Step 2: Move the repository and install canonical plists**

With both jobs booted out, pause Drive, run the standard rename, apply the exact
Time Tracker map/consumer flip, and rebuild the registry in that order. Verify
every captured generated-directory link at its destination-relative path.
Run the exact transaction and generic smoke phase before starting the new jobs:

```bash
bin/drive-workspace record-drive-state time-tracker --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused time-tracker --journal "$JOURNAL"
bin/drive-workspace move time-tracker --journal "$JOURNAL"
bin/drive-workspace apply-consumers time-tracker --journal "$JOURNAL"
bin/drive-workspace verify-consumers time-tracker --journal "$JOURNAL"
bin/drive-workspace verify-local time-tracker --journal "$JOURNAL"
bin/drive-workspace materialize-generated-paths time-tracker --journal "$JOURNAL"
bin/drive-workspace run-smoke time-tracker --journal "$JOURNAL"
bin/drive-workspace verify-local time-tracker --journal "$JOURNAL"
```

Update the two central launchd regular plists to the external root using
`bin/render-epoch-job-paths.py`, but do not commit them. Verify the loaded
`~/Library/LaunchAgents` links still resolve to those canonical files, then
bootstrap and enable both jobs. Keep the map, generated registry, consumer, and
plist changes uncommitted through controlled verification.

- [ ] **Step 3: Verify controlled and scheduled surfaces**

Require `launchctl print`, HTTP success at
`http://127.0.0.1:8788/reports/`, the successful controlled `make
update-current` status already recorded by `run-smoke`, and the
next actual updater trigger within its 300-second interval. Require logs to show
the new path and no old-path recreation. Resume Drive and require the exact old
cloud object in Trash, no duplicate or replacement, and the expected native
transition. Persist all closure evidence:

```bash
bin/drive-workspace verify-consumers time-tracker --journal "$JOURNAL"
bin/drive-workspace verify-local time-tracker --journal "$JOURNAL"
bin/drive-workspace verify-cloud time-tracker --journal "$JOURNAL"
bin/drive-workspace record-native-errors --journal "$JOURNAL"
```

On any failure before commit, boot out the two new jobs, pause Drive, and run:

```bash
bin/drive-workspace record-drive-state time-tracker --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused time-tracker --journal "$JOURNAL"
bin/drive-workspace restore-cloud time-tracker --journal "$JOURNAL"
bin/drive-workspace rollback-consumers time-tracker --journal "$JOURNAL"
bin/drive-workspace rollback-generated-paths time-tracker --journal "$JOURNAL"
bin/drive-workspace rollback-local time-tracker --journal "$JOURNAL"
```

Verify the original object ID and parent, restore the old plist preimages, then
bootstrap the two old jobs. Resume, wait for stable queues, and run
`bin/drive-workspace verify-rollback time-tracker --journal "$JOURNAL"
--baseline-journal
"$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson"`. Require
the old map, registry, plist bytes, live paths, local snapshot, and cloud
identity all match.

- [ ] **Step 4: Commit only after controlled and scheduled verification**

After Steps 2–3 and cloud/native convergence pass, commit the exact Epoch
map/generated-registry hunks and the launchd plist hunks as separate
single-purpose commits. Rerun the registry checker, launchd audit,
`launchctl print`, and one controlled report update. If commit or post-commit
verification fails, boot out the new jobs, pause Drive, restore the original
cloud object with the exact first three commands from the pre-commit rollback,
revert only the plan-created commits in reverse order, verify the journaled
consumer bytes without calling `rollback-consumers`, then run
`bin/drive-workspace rollback-generated-paths time-tracker --journal
"$JOURNAL"` and `bin/drive-workspace rollback-local time-tracker --journal
"$JOURNAL"`. Bootstrap the old jobs, resume, and run the exact
`verify-rollback` command above to prove the original object ID, parent, job
definitions, and local snapshot.

After post-commit verification passes, capture the Drive PID, perform the
required restart, and run:

```bash
bin/drive-workspace close-rollback-window --journal "$JOURNAL" \
  --baseline-journal \
  "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson" \
  --restart-before-pid "$DRIVE_PID"
bin/drive-workspace finalize-generated-paths time-tracker --journal "$JOURNAL"
```

Do not start another transaction before both commands succeed.

### Task 7: Cut over Automations Dashboard

**Files:** dashboard repository, refresh plist/job, generated catalog

- [ ] **Step 1: Boot out only the dashboard refresh job**

Obtain the user's explicit confirmation required by the launchd repository;
without it, defer this transaction. Record label, schedule, logs, and prior
state, then boot out only the dashboard refresh job. After the job is absent,
create and bind the transaction:

```bash
NAME=automations-dashboard
ATTEMPT_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(uuidgen)"
REQUESTED_JOURNAL="$HOME/.local/state/drive-workspace-migration/epoch-special/${NAME}-${ATTEMPT_ID}.ndjson"
CAPTURE_OUTPUT="$(bin/drive-workspace capture "$NAME" --journal "$REQUESTED_JOURNAL")"
JOURNAL="$(printf '%s\n' "$CAPTURE_OUTPUT" | python3 -c 'import json,sys; print(json.load(sys.stdin)["journal"])')"
test "$JOURNAL" = "$REQUESTED_JOURNAL"
bin/drive-workspace verify-journal --journal "$JOURNAL"
bin/drive-workspace record-cloud "$NAME" --journal "$JOURNAL"
```

- [ ] **Step 2: Move, rebuild, and bootstrap**

With the job booted out, run the standard rename and exact dashboard map flip.
Apply the map and manifest consumer hunks before rebuilding the registry, then
verify every captured generated-directory link at its destination-relative
path. Run the dashboard's exact generic smoke phase and require each of its
three statuses separately:

```bash
bin/drive-workspace record-drive-state automations-dashboard --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused automations-dashboard --journal "$JOURNAL"
bin/drive-workspace move automations-dashboard --journal "$JOURNAL"
bin/drive-workspace apply-consumers automations-dashboard --journal "$JOURNAL"
bin/drive-workspace verify-consumers automations-dashboard --journal "$JOURNAL"
bin/drive-workspace verify-local automations-dashboard --journal "$JOURNAL"
bin/drive-workspace materialize-generated-paths automations-dashboard --journal "$JOURNAL"
bin/drive-workspace run-smoke automations-dashboard --journal "$JOURNAL"
bin/drive-workspace verify-local automations-dashboard --journal "$JOURNAL"
```

Render the external-root plist into the launchd repository as
`agents/ai.epoch.automations-dashboard.refresh.plist` without committing it.
Update `repos/launchd/registry/jobs.json` so the job owner is the exact external
dashboard path and its note names the central regular plist. Replace only the
exact live `~/Library/LaunchAgents` link so it targets that central file, then
bootstrap it. Flip both GitHub write guards' configured dashboard source only
now and run their full tests. Keep the new plist, jobs registry, guard, path-map,
generated-registry, and consumer changes uncommitted through controlled
verification.

- [ ] **Step 3: Separate local scheduler verification from external writes**

Verify `launchctl print`, plist path, and a controlled dry-run that cannot
commit, push, or deploy. Resume Drive and require the exact old dashboard cloud
object in Trash, no replacement or duplicate, correct routing, and the expected
native transition. Persist all closure evidence:

```bash
bin/drive-workspace verify-consumers automations-dashboard --journal "$JOURNAL"
bin/drive-workspace verify-local automations-dashboard --journal "$JOURNAL"
bin/drive-workspace verify-cloud automations-dashboard --journal "$JOURNAL"
bin/drive-workspace record-native-errors --journal "$JOURNAL"
```

On any failure before commit, boot out the new job, pause Drive, and run:

```bash
bin/drive-workspace record-drive-state automations-dashboard --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused automations-dashboard --journal "$JOURNAL"
bin/drive-workspace restore-cloud automations-dashboard --journal "$JOURNAL"
bin/drive-workspace rollback-consumers automations-dashboard --journal "$JOURNAL"
bin/drive-workspace rollback-generated-paths automations-dashboard --journal "$JOURNAL"
bin/drive-workspace rollback-local automations-dashboard --journal "$JOURNAL"
```

Verify the original object ID and parent. Restore the old live link, move the
newly created central plist to Trash, bootstrap the old job, resume, wait for
stable queues, and run `bin/drive-workspace verify-rollback
automations-dashboard --journal "$JOURNAL" --baseline-journal
"$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson"`. Require
the old plist, repository path, local snapshot, cloud identity, and guard
behavior all match.

- [ ] **Step 4: Commit verified configuration, then observe the authorized trigger**

Only after the controlled dry-run and cloud/native convergence pass, commit the
exact Epoch shared-config, launchd plist/registry, and dotfiles guard hunks as
separate single-purpose commits. Rerun the registry checker, launchd audit,
guard suites, `launchctl print`, and the non-writing dry-run.

The next real 04:00 trigger may perform externally visible writes; observing it
is an explicit authorization boundary. Leave this plan pending until authorized
and observed rather than weakening the acceptance criterion. If commit,
post-commit verification, or the authorized trigger fails because of the path
cutover, boot out the new job, pause Drive, restore the original cloud object,
revert only the plan-created commits in reverse order, restore the old live
link, verify all journaled consumer bytes without calling
`rollback-consumers`, run `bin/drive-workspace rollback-generated-paths
automations-dashboard --journal "$JOURNAL"` and `bin/drive-workspace
rollback-local automations-dashboard --journal "$JOURNAL"`, bootstrap the old
job, resume, and run the exact `verify-rollback` command above. Prove the
original object ID, parent, job definition, guard behavior, and local snapshot.
Record any external side effect from the authorized trigger; do not attempt a
separate external undo without its own authorization.

After the authorized trigger and every post-commit check pass, capture the
Drive PID, perform the required restart, and run:

```bash
bin/drive-workspace close-rollback-window --journal "$JOURNAL" \
  --baseline-journal \
  "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson" \
  --restart-before-pid "$DRIVE_PID"
bin/drive-workspace finalize-generated-paths automations-dashboard \
  --journal "$JOURNAL"
```

### Task 8: Close the Epoch migration phase

- [ ] **Step 1: Verify exact local scope**

Require all 34 external code destinations, the declared external
`media-mentions-historical-media-backfill` worktree, all 34 old `repo` paths
absent, its old linked-worktree path absent, every notes directory present, and
no unexpected external repository. For each of the
65 originally recorded Drive link paths, require `lstat` to report absent at
its old Drive location. At the corresponding external repository-relative path,
require either the byte-identical preserved link and target or the explicitly
journaled real generated directory created by its owner command. Run the named
runtime check for every materialized path and require every retained old target
still present until that unit's rollback window closes; do not require the
post-smoke tree to contain 65 symlink objects.
After the per-unit rollback window is formally closed, require the tested
`finalize-generated-paths` event for every obsolete target of a materialized
generated directory; preserved link targets remain canonical. No worker moves
one manually.

- [ ] **Step 2: Verify registry and live consumers**

Rebuild the registry, run checker/pre-commit tests, dashboard/Time Tracker
tests, launchd audit, GitHub guard tests, and paired skill audit. Require no live
old-root fallback.

- [ ] **Step 3: Verify cloud/native convergence**

Require every journaled old cloud tree in Trash, no duplicate or computer
backup, all path-level native records cleared, and one settled Drive restart.
Do not claim overall zero errors; residual and regular-file repair plans remain.
