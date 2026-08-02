---
name: fix-drive-errors
description: Triage Google Drive sync errors through the tracked fail-closed drive-workspace tooling; snapshot the native error panel, run the read-only audit first, classify each error, and route every repair through journaled transactions. Use when Drive reports sync errors, when an in-Drive symlink is suspected, or when content under ~/My Drive fails to sync. Never creates symlinks, never disconnects the account, and never edits Drive's internal databases.
---

# Fix Google Drive sync errors

Audit-first triage of Google Drive sync errors. Every observation is recorded
in a hash-chained journal, every repair runs through a `bin/drive-workspace`
transaction, and nothing mutates Drive state by hand. All commands run from
the dotfiles checkout; journals live under the tooling state root
(`~/.local/state/drive-workspace-migration`).

## Invariant

**No filesystem symlink may exist anywhere under ~/My Drive.** Google Drive
cannot represent symlinks, so every in-Drive symlink is an error: either a
leftover of the retired externalization workflow, which moved directories
outside Drive and left links behind, or new damage. A symlink found under
`~/My Drive` is never adopted, recreated, or repointed; it is repaired through
a journaled path-repair transaction that ends with the path absent or a real
directory or file.

## Hard limits

Do not perform these, and do not suggest them, even if the Drive UI or a
support article proposes them:

1. **Never disconnect or reconnect the Google Drive account**, and never
   remove or re-add the sync root. A past reconnection attempt re-registered
   the folder as a computer backup and re-uploaded thousands of files into a
   wrong cloud folder.
2. **Never edit Google Drive's internal databases** (`mirror_sqlite.db`,
   `metadata_sqlite_db`, `root_preference_sqlite.db`, or anything else under
   `~/Library/Application Support/Google/DriveFS`). Reading them for
   diagnosis is fine; writing to them is not, and neither is deleting or
   renaming the DriveFS cache.
3. **Never create a filesystem symlink under `~/My Drive`**, and never move a
   directory out of Drive by hand to silence an error. Repository moves go
   through the manifest transactions below; single-path repairs go through
   the path-repair transactions.
4. **Never trust the Drive UI or a network meter for sync direction or
   progress.** The tooling's gates require provider evidence (native panel
   rows, cloud object lookups, queue cursors); use them instead of
   inference.

## Workflow

### Step 1: Snapshot the native error categories

Before changing anything, record what Drive itself reports:

```bash
bin/drive-workspace native-preflight
bin/drive-workspace init-journal \
  --journal ~/.local/state/drive-workspace-migration/observations/$(date +%Y%m%d)-native-errors.jsonl \
  --kind observation --label "native error snapshot"
bin/drive-workspace record-native-errors \
  --journal ~/.local/state/drive-workspace-migration/observations/$(date +%Y%m%d)-native-errors.jsonl
```

If the preflight reports a permission denial, stop: that is a hard blocker,
not something to work around. The snapshot is the baseline that the final
verification compares against.

### Step 2: Run the read-only audit first

```bash
bin/drive-workspace audit --manifest drive-workspaces.json
```

The audit is strictly read-only. It scans the whole Drive root for symlinks,
records journal-grade filesystem and Git state for each configured workspace,
and exits nonzero if any symlink remains under `~/My Drive` or any blocker is
found. Run it before any repair and again after every transaction.

### Step 3: Classify every error

Map each native error row and each audit finding into exactly one class:

- **Repository errors** — errors inside a workspace configured in
  `drive-workspaces.json`. The repair is the workspace's manifest plan: the
  whole repository moves out of Drive through the journaled transaction
  pipeline (`capture`, `record-drive-state`, `confirm-drive-paused`, `move`,
  consumer application, local/cloud verification). Never repair pieces of a
  configured workspace individually.
- **Residual errors** — leftovers of the retired externalization workflow:
  in-Drive symlinks, ` (1)`-suffixed re-download duplicates beside them, and
  orphaned cloud copies of externalized directories. Each residual path is
  repaired through the journaled path-repair pipeline (`capture-path`,
  `confirm-path-paused`, `verify-path-local`, `record-path-cloud`,
  `verify-path-cloud`, `finalize-path`), which captures the exact preimage
  and keeps every mutation recoverable.
- **`.gdoc` errors** — Google-native document stubs (`.gdoc`, `.gsheet`,
  `.gslides`) whose content lives only in the cloud. Verify the cloud object
  through the tooling's cloud reader before touching the stub; a stub is
  never deleted or edited on the strength of local state alone.
- **File-representation errors** — content Drive cannot represent: dependency
  directories, bytecode and cache trees, virtual environments, illegal
  filenames. Generated state belongs outside `~/My Drive` entirely; the
  paired runtime guards deny creating it there, and existing instances are
  removed through path-repair transactions with a disposable target policy —
  not hidden behind links.

### Step 4: Stop on unknown or ambiguous content

Any error or path that does not provably belong to one class — unrecognized
directories, possible user data, conflicting evidence about which side is
current — stops the run. Report the path, the evidence, and the candidate
classes, and wait for a decision. Never guess, never delete ambiguous
content, and never move it out of Drive to make an error disappear.

### Step 5: Verify the filesystem and the native panel after a restart

After the transactions complete, quit and relaunch the Google Drive client
normally (no account changes), then verify both surfaces:

1. **Filesystem**: re-run `bin/drive-workspace audit --manifest
   drive-workspaces.json` and require the expected state — no symlink under
   `~/My Drive` and no new blocker.
2. **Native panel**: re-run `bin/drive-workspace record-native-errors` into
   the observation journal and compare against the Step 1 snapshot.

Only both together support a claim that an error is fixed. If the native
panel still shows errors, reclassify from Step 3 rather than declaring
success from the filesystem state alone.
