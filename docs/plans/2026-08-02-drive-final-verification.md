# Drive Migration Final Verification Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prove that the completed migration survives real workflows and two fresh Google Drive restarts with zero user-visible errors, no symlink under My Drive, and no lost or duplicated cloud content.

**Architecture:** Treat every earlier plan journal as evidence for one bounded transaction, then independently remeasure the filesystem, Git state, workflow behavior, Drive registration, cloud object routing, and native error panel. An append-only external acceptance journal records the two restart cycles; no passing component check substitutes for the final user-visible zero state.

**Tech Stack:** `drive-workspace`, Git, Python, Google Drive v3, DriveFS SQLite read-only checks, macOS launchd, Chrome control, Emacs batch tests, shell audits.

---

This is program plan 12 of 12. Start only after plans 1 through 11 have closed
their exact path-level gates. This plan does not repair an unexpected result:
it records the failing path, returns ownership to the plan that introduced or
owns it, and restarts final verification only after that root cause is fixed.
Before reading Google credentials or making Drive API calls, read
`claude/context/google-services.md` and `claude/context/secrets.md`. Never
print tokens or private document identifiers.

## File map

- Append outside Drive: `~/.local/state/drive-workspace-migration/final-verification.jsonl` — mode `0600`, one fsynced record per check and restart cycle.
- Inspect only: every external program, workspace, path, and category journal required by plans 1 through 11; `drive-workspaces.json`; all live roots and external destinations; DriveFS registration and queue databases; native Drive panel; and Google Drive API metadata.
- Modify tracked or project artifacts: nothing. The only new Drive objects
  permitted are the separately authorized Task 5 marker and directory, both of
  which must close through their exact-ID cleanup transaction. If
  documentation, generated output, or an audit test is
  stale, record the mismatch, return it to the earlier plan that owns the
  artifact, and restart final verification only after that plan closes again.
  Do not repair drift or weaken an acceptance check here.

### Task 1: Prove every earlier transaction is closed

**Files:**

- Inspect: `~/.local/state/drive-workspace-migration/`
- Inspect: all paths named by plans 1 through 11
- Append: `final-verification.jsonl`

- [ ] **Step 1: Create the external acceptance journal safely**

Create the state directory if absent, set it to mode `0700`, create the journal
with `umask 077`, and refuse to continue if the resolved journal path is under
`~/My Drive` or is a symlink. Each record must contain UTC timestamp, check
name, command or UI surface, exit status, and SHA-256 of any captured evidence.
Do not put OAuth responses, document IDs, file contents, or untracked-file
names in the journal.

- [ ] **Step 2: Enumerate the expected journals and closures**

For every moved workspace and repaired path, and for each of the four
regular-file categories in plans 8 through 11, require a matching preimage,
mutation, local verification, cloud verification, and native path-transition
event. Require the stale-record pilot and after-resume rollback drill to show
their supported resolution paths. Reject a missing, duplicated, out-of-order,
or un-fsynced transaction record.

- [ ] **Step 3: Re-run the manifest audit**

Run:

```bash
cd "/Users/pablostafforini/My Drive/dotfiles"
bin/drive-workspace audit --manifest drive-workspaces.json
```

Require all approved moved sources absent, all destinations present, the
existing `~/repos/wuzapi` checkout untouched, and the retained source-only
Drive roots still present. Require each in-place and residual path to match its
declared final representation. An expected clean final audit must exit zero;
do not interpret a partial report as success.

- [ ] **Step 4: Search for stale old-root references**

Search tracked configuration, launchd plists, active user agents, hooks,
scheduler definitions, Emacs path variables, Claude/Codex skills, shell
helpers, and repository-local scripts for every moved source path. Classify
historical prose separately. Require zero executable reference to an old root
and zero active process whose current working directory is an old root.

### Task 2: Prove the complete filesystem invariant

**Files:**

- Inspect: `/Users/pablostafforini/My Drive`
- Append: `final-verification.jsonl`

- [ ] **Step 1: Run the independent symlink traversal**

Run the standard audit and an independent traversal:

```bash
cd "/Users/pablostafforini/My Drive/dotfiles"
bin/drive-workspace audit --manifest drive-workspaces.json
find "/Users/pablostafforini/My Drive" -type l -print
```

Capture standard output and standard error separately. Require both streams
from `find` to be empty and its exit status to be zero. Any unreadable path or
single symlink fails the program, even when the native Drive panel is empty.

- [ ] **Step 2: Exercise the regression guards**

In disposable temporary repositories, run every guarded dependency,
environment, build, browser-profile, worktree, and legacy `nosync` command
from a simulated Drive path and an external path. Require the Drive form to
fail before mutation with an actionable migration message and the external
form to reach the fake executor. Run:

```bash
cd "/Users/pablostafforini/My Drive/dotfiles"
python3 -m unittest \
  tests.test_drive_workspace \
  tests.test_drive_runtime_guard \
  tests.test_ai_config_sync_audit \
  tests.test_claude_bash_pretooluse -v
bin/ai-config-sync audit
python3 bin/docs-audit audit --root .
```

Require all tests and audits to pass without creating a symlink under My
Drive. Run the independent traversal again afterward.

### Task 3: Exercise real personal and Epoch workflows

**Files:**

- Inspect: `drive-workspaces.json` smoke commands and each moved repository
- Inspect: affected launchd jobs, schedulers, hooks, and Emacs integrations
- Append: `final-verification.jsonl`

- [ ] **Step 1: Re-run all personal workspace smoke commands**

Run every manifest smoke command from its external destination. At minimum,
require:

- `agent-skills`: `claude plugin validate .`,
  `bash hooks/session-start-test.sh`, and
  `bash hooks/simplify-ignore-test.sh`;
- `80000hours.global`: `uv sync --extra dev`, its pytest suite,
  `uv run eighty-k-global report`, frontend `npm ci`, typecheck, and build;
- `dont-sleep`: `./tests/test-dont-sleep.sh` from both registered worktrees;
- `ea.news`: superproject and both submodules clean at the journaled commits,
  followed by their declared API/front-end smoke commands; and
- `stafforini.com`, `consensus-trader`, and `tangodb`: their manifest test,
  build, browser, or data-boundary smokes without modifying preserved data.

After each workspace, require Git state to match its post-migration snapshot
apart from documented generated state outside Drive. Do not push, discard a
stash, or stage an untracked file.

- [ ] **Step 2: Create and remove one fresh external linked worktree**

Use the supported `newtask` or repository worktree helper with a disposable
branch name. Require the worktree to appear below `~/repos/.worktrees`, open
correctly through the supported editor/session path, and remain absent from My
Drive. Remove this worktree and branch through Git's supported commands after
verification, then require `git worktree list --porcelain` and `git worktree
prune --dry-run` to show no stale metadata.

- [ ] **Step 3: Compare a fresh Epoch registry without touching the live file**

Create a mode-`0700` temporary directory outside My Drive. From the Epoch root,
run:

```bash
VERIFY_PARENT="${TMPDIR:-/tmp}"
VERIFY_PARENT_RESOLVED="$(realpath "$VERIFY_PARENT")"
case "$VERIFY_PARENT_RESOLVED" in
  "$HOME/My Drive"|"$HOME/My Drive"/*) exit 1 ;;
esac
VERIFY_DIR="$(mktemp -d "$VERIFY_PARENT_RESOLVED/drive-final-registry.XXXXXX")"
cleanup_verify_dir() {
  if [ -e "$VERIFY_DIR" ]; then trash "$VERIFY_DIR" || return 1; fi
  test ! -e "$VERIFY_DIR"
}
trap cleanup_verify_dir EXIT
trap 'cleanup_verify_dir || exit 1; trap - EXIT; exit 129' HUP
trap 'cleanup_verify_dir || exit 1; trap - EXIT; exit 130' INT
trap 'cleanup_verify_dir || exit 1; trap - EXIT; exit 143' TERM
test ! -L "$VERIFY_DIR"
VERIFY_DIR_RESOLVED="$(realpath "$VERIFY_DIR")"
case "$VERIFY_DIR_RESOLVED" in
  "$HOME/My Drive"|"$HOME/My Drive"/*) exit 1 ;;
esac
chmod 700 "$VERIFY_DIR"
git status --porcelain=v2 > "$VERIFY_DIR/status-before"
DOTFILES_ROOT="$HOME/My Drive/dotfiles"
"$DOTFILES_ROOT/bin/drive-workspace" audit \
  --manifest "$DOTFILES_ROOT/drive-workspaces.json" \
  > "$VERIFY_DIR/workspaces-before.json"
python3 -m unittest discover -s projects/shared/tests -v
python3 projects/shared/scripts/build_project_registry.py \
  --projects-root "$HOME/My Drive/Epoch/projects" \
  --repo-path-map projects/shared/repo-paths.json \
  --output "$VERIFY_DIR/project-registry.json"
python3 projects/shared/scripts/check_project_repos.py
cmp "$VERIFY_DIR/project-registry.json" \
  projects/shared/project-registry.json
git status --porcelain=v2 > "$VERIFY_DIR/status-after"
cmp "$VERIFY_DIR/status-before" "$VERIFY_DIR/status-after"
```

Require the temporary registry to contain all 34 active slugs below
`~/repos/epoch`. Require the workspace audit to contain those 34 code records
plus the separately named
`media-mentions-historical-media-backfill` linked-worktree record at
`~/repos/.worktrees/media-mentions/historical-media-backfill`. Hash the
comparison evidence but retain `"$VERIFY_DIR"` until
the end of this step. If either byte comparison fails, do not copy the
temporary output over the tracked registry: record the mismatch, move the
temporary directory to Trash, and return it to plan 5.

Run every repository-specific smoke command declared by the migration plan.
Require the staff-data-owned identity workflow to reference no ignored or
absent checkout under the old Epoch root. After all smoke commands, write a
fresh status snapshot to `"$VERIFY_DIR/status-after-smokes"` and require it to
be byte-identical to `status-before`:

```bash
git status --porcelain=v2 > "$VERIFY_DIR/status-after-smokes"
cmp "$VERIFY_DIR/status-before" "$VERIFY_DIR/status-after-smokes"
"$DOTFILES_ROOT/bin/drive-workspace" audit \
  --manifest "$DOTFILES_ROOT/drive-workspaces.json" \
  > "$VERIFY_DIR/workspaces-after.json"
cmp "$VERIFY_DIR/workspaces-before.json" "$VERIFY_DIR/workspaces-after.json"
cleanup_verify_dir || exit 1
trap - EXIT HUP INT TERM
```

These comparisons make final verification fail if it silently regenerates or
modifies a tracked artifact. Require both audit snapshots to contain exactly
the 34 external Epoch code destinations and the one declared auxiliary
worktree record, with each journaled HEAD, index/worktree status,
linked-worktree registration, and relevant submodule state. Compare
each repository before and after its own smoke as well as the complete JSON
snapshots; only an exact generated-state exception already declared in that
repository's migration journal may differ. Any other drift returns to the
owning plan.

- [ ] **Step 4: Verify live path consumers**

Run controlled invocations of the time tracker, dashboard, registry hooks,
session-log relocation, worktree cleanup, reasoning-task sync, Emacs Dired
root commands, Karabiner generation, publishing scripts, Google Search Console
helpers, Tango reconciliation, and every affected launchd job. Inspect the
loaded launchd configuration, not merely the source plist.

Before any controlled or scheduled invocation, inventory whether it can send
Slack or email, write Google services, commit, push, deploy, publish, or mutate
another shared system. Use a documented dry-run/read-only mode when it proves
the contract. Obtain explicit user authorization separately for every
invocation or natural trigger that can have an external effect; plan approval
alone is not that authorization. This includes the dashboard controlled run
and natural 04:00 trigger. Until each required authorization and observation is
complete, record the check as pending and do not claim final completion. Never
signal or restart an active Emacs session.

### Task 4: Exercise repaired content and file representations

**Files:**

- Inspect: all in-place and residual repair paths
- Inspect: 36 Epoch `.url` files and 12 preserved personal `.gdoc` files
- Inspect: Rubric/Uqbar bridges, Uqbar wrappers, and Enchant runtime
- Append: `final-verification.jsonl`

- [ ] **Step 1: Recreate every formerly link-producing workflow**

Exercise the supported install, build, test, package, browser, mail-sync,
Wikipedia, proofread, course-environment, and cache-producing commands named
by plans 4 and 7. Require their generated state to remain external, disabled,
or a regular in-Drive object exactly as designed. Re-run the complete symlink
audit after all recreation paths.

- [ ] **Step 2: Verify the two local HTML report vendor trees independently**

Hash the 13 substantive files in both restored Bootstrap distributions and
require byte identity. Compare the four `Icon\r` files and every other
difference with the explicit local HTML report source decision recorded by plan 7;
do not assume their disposition. Load the real consumer for each tree and
require Bootstrap assets to return successfully; do not infer one tree's
success from the other.

- [ ] **Step 3: Verify all document links through the real browser**

For each of the 36 Epoch `.url` files, compare its case-preserved document ID
with the original migration journal, open it through LaunchServices, and use
the configured Epoch Chrome profile to require the intended title with no
account chooser or permission error. Compare current permissions with the
pre-migration hash. Require all 12 personal-account `.gdoc` files to remain
present and unchanged. Do not print or journal document IDs.

- [ ] **Step 4: Verify bridges, wrappers, and Enchant**

Run the Rubric and Uqbar project-memory checks with file tools disabled, execute
both Uqbar wrapper tests and real launcher help paths, and run the Enchant
installer plus dictionary lookup tests. Require both `CLAUDE.md` bridges and
both Uqbar wrappers to be regular files with their specified modes;
`~/.config/enchant` must be a real directory containing exactly the 19 direct
external runtime links; none of the removed forwarding links may exist under
My Drive.

### Task 5: Prove cloud identity and absence of duplicates

**Files:**

- Inspect read-only: earlier Google Drive API metadata and local DriveFS databases
- Mutate only with separate authorization: the two exact generated marker IDs from Step 3
- Append: `final-verification.jsonl`
- Create temporarily outside Drive: `~/.local/state/drive-workspace-migration/marker-round-trips/<timestamp>.json` — mode `0600`, exact generated names, root ID, discovered marker IDs, authorization record, and cleanup state.

- [ ] **Step 1: Verify Drive registration and database health**

Locate the active DriveFS account from the running application rather than a
hard-coded database path. Open copied SQLite snapshots read-only, run
`PRAGMA quick_check`, and require the registered root to retain
`is_my_drive=1`, exact root ID from the private migration journal, and zero
`machine_root` rows. Record counts and hashes only. Do not edit a Drive
database, reconnect the account, or infer health from the version number.

- [ ] **Step 2: Verify every cloud transaction**

Through the Google Drive API, require each moved old repository tree to be in
Trash with its journaled ID and parent, each repaired regular object to have
the expected parent and content hash, and no same-name or journal-associated
duplicate below My Drive or a computer-backup root. Require all migration
destinations to be absent from Drive. Resolve full parent chains; do not accept
a name-only match.

- [ ] **Step 3: Verify an authorized, journaled regular-file round trip**

This step creates and trashes two cloud objects. Obtain explicit user
authorization for this round trip separately from approval of this plan and
from every authorization in Task 3 Step 4. Record only the authorization time
and scope in the private marker journal; do not copy conversational content
into it.

Before creating anything, enumerate every marker journal whose state is not
`closed`. Do not start a new round trip while one exists. Obtain explicit user
authorization to finish that interrupted cleanup. For every already recorded
file or directory ID, query that exact ID; if it is live, move only that ID to
Trash, then require the same ID to report `trashed=true`. If creation occurred
but an interruption prevented an ID from being recorded, resolve the journaled
unique name only below its journaled exact parent. Require exactly one match,
append its ID to the mode-`0600` external journal and fsync immediately, and
only then clean it up by ID. Multiple matches leave the transaction pending
and stop final verification. After queues settle, zero exact-parent matches is
the authoritative terminal state `never-created/absent` for that intended
object. Require the exact local paths absent and mark the recovered transaction
`closed` only when each intended object is either an exact recorded ID with
`trashed=true` or an fsynced `never-created/absent` result.

For a new round trip, create and fsync the mode-`0600` external journal before
the local mutation. Record a cryptographically unique directory name, a unique
marker filename, their exact intended local paths, the journaled My Drive root
ID, state `prepared`, and empty ID fields. Refuse any pre-existing local or
exact-parent cloud match. Create the dedicated directory and one regular marker
file below My Drive; do not use a symlink or overwrite a path. As soon as the
API first reports the directory below the root, append its exact ID and parent
to the external journal and fsync before waiting for the file. As soon as the
API reports the file below that exact directory ID, append its exact ID and
parent and fsync before accepting the route check. Require both full parent
chains to end at the journaled root ID.

Run creation and verification inside a controller whose idempotent `finally`
path tests lexical existence and processes the marker file first and its
directory second. It moves a local path to Trash only when that path exists;
an already absent or never-created path is success, not an exception. It then
queries each journaled cloud ID independently.
For each ID still live, use `files.update(trashed=true)` on that ID only; never
select a cleanup target by name once its ID is recorded. The `INT`, `TERM`, and
ordinary-error paths must enter this same cleanup. Verify every recorded ID
reports `trashed=true`; for an intended object with no recorded ID, wait for
settled queues and require exact-parent absence before recording
`never-created/absent`. Require no same-name live object under the recorded
parents, append state `closed`, and fsync before returning success. If the
cleanup itself is interrupted or fails, leave state `cleanup-pending`; the
re-entry procedure above owns it, and final verification remains incomplete.

- [ ] **Step 4: Verify queues and stale records**

Require active queues to settle normally and all principal database quick
checks to pass. Match the native panel against current filesystem paths and
the stale-record pilot journal. No removed source may remain as a failed-create
record. Use only Drive's supported retry or dismissal controls if a record is
path-matched and stale; any unclassified record returns to diagnosis.

### Task 6: Run two complete fresh-restart acceptance cycles

**Files:**

- Inspect: native Google Drive error panel, post-restart logs, DriveFS state,
  API metadata, and filesystem
- Append: `final-verification.jsonl`

- [ ] **Step 1: Capture the pre-restart acceptance baseline**

Record UTC time, Drive version, current native error inventory by path and
category, queue state, registration checks, API duplicate check, and complete
symlink-audit result. Require zero native errors before starting the cycle.

- [ ] **Step 2: Perform restart cycle one**

Gracefully quit and relaunch Google Drive through its supported application
controls. Wait for the application, queues, and native panel to settle. Then
repeat the complete symlink audit, registration quick checks, API route and
duplicate checks, stale-path search, and native-panel inspection. Require zero
user-visible errors and no post-restart create failure for a migrated path.

- [ ] **Step 3: Perform restart cycle two independently**

Take a new UTC baseline, gracefully restart Drive again, wait for full
settlement, and repeat every check from cycle one. Do not reuse a cached API
response, database copy, panel screenshot, or log window. Require zero
user-visible errors again.

- [ ] **Step 4: Seal and review the evidence**

Independently recalculate every referenced evidence hash. Require one passing
record for every task above, no pending scheduled trigger, no skipped
workspace, and no unexplained native record. Move temporary evidence created
by this plan to Trash after its hash is recorded. Only then fsync the final
journal, set it to mode `0400`, and retain it as the acceptance record.

### Task 7: Report completion without overstating evidence

**Files:**

- Inspect: final journal only

- [ ] **Step 1: Produce the final summary from recorded results**

Report the exact user-visible native count after each restart, symlink count,
number of moved and retained repository roots, number of `.gdoc` replacements
and preserved pointers, cloud duplicate count, root ID result, and scheduled
workflow status. Mention any workflow intentionally excluded by scope.

- [ ] **Step 2: Apply the completion gate**

Call the original Drive-error task complete only when both fresh restart cycles
show zero native errors and every direct acceptance check above passes. If any
check is pending or failed, name the owning path and plan; do not say that the
Drive errors are fixed, resolved, working, or done.
