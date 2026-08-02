# Drive-Compatible Workspaces Design

## Status

Revised design pending final user review. This document does not authorize a
migration by itself.

This specification replaces the earlier 56-error local-link design. Google
Drive for desktop 129.0.1.0 now exposes directory-symlink failures in its
native error panel, so an in-Drive symlink is not a successful externalization.
The four implementation plans derived from the earlier design are superseded
until they are rewritten from this specification.

## Goal

Reduce Google Drive for desktop's native error count to zero without losing
repository state, breaking developer or scheduled workflows, sharing Epoch
documents with the personal account, duplicating writable sources, or
re-registering the Drive account.

The durable filesystem invariant is:

> No filesystem symlink may exist anywhere under `~/My Drive`.

Active code that requires dependencies, builds, virtual environments, caches,
browser profiles, or linked worktrees belongs outside Drive. Content that
remains in Drive must use regular Drive-compatible files and directories.

## Evidence that invalidated the earlier design

After the previous Drive restart, the native panel briefly reported 56 errors.
Once Drive completed its replay, the panel reported 193. The current-run
`UploadCreateMergeQueueItem` failures reconciled exactly:

- 134 `PARTIAL_RESULTS` failures, one for each resolving directory symlink
  under My Drive;
- 36 `INVALID_GOOGLE_DOCUMENT` failures from Epoch-account `.gdoc` files;
- 20 persistent `UNSUPPORTED` failures from tracked file symlinks; and
- three temporary npm `.bin` symlink failures created while building a skill.

The three npm links were later removed, but Drive retained their failed-create
records. The 134 directory links were visible in the native paginated error
dialog as generic upload failures. Therefore the earlier claim that directory
links were harmless log-only events was false.

The directory-link inventory was:

| Area | Count |
|---|---:|
| Epoch | 70 |
| Personal repositories | 55 |
| Dotfiles | 3 |
| Courses | 3 |
| Apps | 1 |
| Health | 1 |
| Home mirror | 1 |

Of the 134 links, 131 pointed into `~/.drive-nosync`, two pointed into an
external npm cache, and one was the intentional tracked
`agent-skills/.opencode/skills -> ../skills` link. The exact one-to-one match
establishes that leaving a healthy symlink in Drive does not solve the error.

Drive itself was healthy during diagnosis: the root remained a normal My Drive
mirror with `is_my_drive=1`, the expected root document ID, and no
`machine_root` row; its principal databases passed SQLite quick checks; and its
queues were active rather than paused or wedged.

## Decisions and hard constraints

- Canonical external repository roots are `~/repos` for personal repositories
  and `~/repos/epoch` for Epoch repositories.
- Linked worktrees live under `~/repos/.worktrees`, including dotfiles
  worktrees.
- The existing real `~/repos/wuzapi` checkout remains untouched.
- Git remotes are sufficient backup for repository history. The migration does
  not create periodic Drive snapshots or duplicate writable checkouts.
- Every repository selected for a move must have a reachable remote. After a
  fresh fetch, every commit reachable from the primary `HEAD`, every registered
  linked-worktree `HEAD` including detached heads, local branches, local tags,
  `refs/stash`, and checked-out submodule heads must also be reachable from an
  advertised remote ref. This gate has no implicit waiver and nothing is pushed
  without separate explicit authorization.
- The existing working tree is moved intact. Staged, unstaged, untracked,
  ignored, executable, symlink, submodule, and linked-worktree state must not be
  reconstructed from a fresh clone.
- Verified old repository trees leave Drive. No compatibility symlink or
  writable Drive-side mirror remains.
- Dotfiles remains the Drive-side source of truth, but it becomes a strict
  source-only exception: its runtime dependencies, generated state, and
  worktrees live outside Drive.
- Of the original 127 personal repository roots, 119 remain in Drive: the ten
  repaired repositories and 109 repositories with no current directory-link
  failure. They remain source-only trees. The ten repairs may use only their
  defined guarded external runtimes; activating one of the 109 unaffected trees
  for dependency installation, building, state-creating tests, or linked
  worktree use requires migrating it first. The already-existing `dont-sleep`
  linked worktree is the sole named exception: its main tree remains
  source-only and its active worktree moves to the external worktree root.
- No live runtime command searches both old and new roots or silently falls
  back to an old path. Stale paths fail explicitly; the migration audit may
  inspect both roots to prove convergence.
- Drive is never disconnected, reconnected, or re-registered. Sync direction
  and cloud routing are verified through the Drive API rather than inferred
  from the UI or network traffic.
- Cloud and filesystem removal uses recoverable Trash operations. Ambiguous or
  user-authored content is never classified as disposable from its basename or
  ignore status alone.
- Repository publication remains separate from local migration. No push, pull
  request, issue, or other externally visible GitHub action is implicit.

## Target layout

```text
~/repos/<name>                    moved personal repositories
~/repos/epoch/<project>           all Epoch repositories
~/repos/.worktrees/<repo>/<name>  all linked worktrees

~/My Drive/Epoch/...              documents, notes, and project metadata
~/My Drive/dotfiles               source-only dotfiles repository
~/My Drive/...                    other synchronized documents and data
```

Epoch's `projects/<name>/repo` directories disappear. Project notes and
metadata stay at their existing Drive paths, while the project registry and
live automation configuration point to `~/repos/epoch/<name>`.

Repositories outside Drive may use their existing tracked symlinks and create
`node_modules`, virtual environments, builds, caches, and browser profiles
normally. Existing large external targets such as Consensus Trader data and
Stafforini PDF collections remain in place initially; their absolute symlinks
become Drive-safe because the owning repository is outside the mirror.

Two explicit conceptual roots own live path construction:

```text
REPOS_ROOT=$HOME/repos
EPOCH_REPOS_ROOT=$HOME/repos/epoch
```

Live configuration either constructs paths from those roots or stores an exact
new path. Historical logs and documents are not mass-rewritten merely because
they mention an old location.

## Repository migration scope

### Epoch repositories

Move all 34 `Epoch/projects/<name>/repo` working trees to
`~/repos/epoch/<name>`. Thirty-two of them currently account for 65 directory
links; moving all 34 establishes one predictable Epoch repository boundary.

### Personal workspace trees moved

Move these six workspace trees, which account for 43 directory links. They
contain eight repository roots because `ea.news` is a superproject with two
submodules. The separate inherited `dont-sleep` linked worktree is not included
in either count:

| Current workspace | Destination | Links | Reason |
|---|---|---:|---|
| `~/My Drive/repos/80000hours.global` | `~/repos/80000hours.global` | 6 | Active Python and Next runtimes repeatedly create `.venv`, `node_modules`, `.next`, and caches. |
| `~/My Drive/repos/agent-skills` | `~/repos/agent-skills` | 1 | The repository intentionally tracks `.opencode/skills -> ../skills`; moving preserves its single-source semantics. |
| `~/My Drive/repos/consensus-trader` | `~/repos/consensus-trader` | 8 | It tracks a virtual environment, caches, 122 GB of live data, and 129 MB of results behind pervasive relative paths. |
| `~/My Drive/repos/ea.news` | `~/repos/ea.news` | 2 | The frontend requires `node_modules` and `.next`. Moving only that submodule would break the superproject, so the transaction preserves the parent, `ea.news-api`, `ea.news-front`, and their relative layout. |
| `~/My Drive/repos/stafforini.com` | `~/repos/stafforini.com` | 7 | Build and deploy workflows use dependencies, persistent `public` output, 48 GB of PDFs, and 5.4 GB of thumbnails. |
| `~/My Drive/repos/tangodb` | `~/repos/tangodb` | 19 | The active Python/Svelte monorepo has dependencies, framework output, persistent browser state, and tracked cache links. |

The `ea.news` correction is required by live Git state. The parent tracks
`ea.news-front` and `ea.news-api` as submodules, enables recursive submodule
operations, and stores both Git directories outside Drive. Moving only the
frontend would leave its required gitlink path absent and allow a later
`git submodule update` to recreate a second checkout in Drive. The whole 26 MB
workspace therefore moves intact; the migration repairs both submodule
`core.worktree` values, including the API's stale Dropbox path, while preserving
the frontend's current branch, commit, and pre-existing parent gitlink
difference.

Implementation must remeasure repository state. At design time, only
`80000hours.global` and `agent-skills` were clean and caught up with their
recorded upstreams. Consensus Trader, EA News, and TangoDB had local-only
commits; Consensus Trader, Stafforini, and TangoDB also had dirty or untracked
state. Moving preserves that state, but the exact remote-commit coverage rule
must be satisfied before each migration.

### Personal repositories repaired in place

These ten repositories account for the remaining 12 personal directory links:

| Repository | Links | Repair |
|---|---:|---|
| `80k-website-old` | 2 | Remove inactive `node_modules` and `.next`; migrate the repository before any future build. |
| `add-to-repo` | 1 | Restore `dist` as its three regular tracked GitHub Action artifacts; the current untracked link masks tracked deletions. |
| `archive/polymarket-bot` | 1 | Remove disposable egg metadata. |
| `archive/polymarket-bot-2` | 1 | Remove egg metadata while preserving the untracked DuckDB file. |
| `archive/polymarket-traders` | 1 | Remove the small generated `build` output; migrate before renewed development. |
| `gmail-maildir-sync` | 1 | Remove `__pycache__` and retain the existing bytecode-disable policy. |
| `launchd` | 1 | Disable or externally redirect pytest caching without changing its operational repository path. |
| `pass-utils` | 1 | Remove egg metadata and use non-editable installation in an external environment. |
| `rubric-visualizer` | 2 | Remove Python and pytest caches; separately migrate its file-symlink instruction bridge. |
| `wikipedia-deletion-analysis` | 1 | Set a project-specific external uv environment and update the scheduled collector path. |

An in-place repair is complete only after exercising the operation that could
recreate the path. Every retained build-capable repository must have a
fail-closed technical guard on its supported install and build entrypoints when
they run under My Drive. Documentation reinforces the guard but does not
replace it. If a reliable guard cannot be implemented, the repository must move
before the old operation is used again.

## Residual non-repository directory links

Repository migration and repair address 120 of the 134 directory links. The
remaining 14 have explicit dispositions:

| Paths | Disposition |
|---|---|
| `Epoch/.cache`, `Epoch/.pytest_cache`, `Epoch/projects/.cache`, and the two caches under `Epoch/projects/shared/scripts` | Remove the links; disable Python bytecode and pytest caches or direct them to an external project-specific location. |
| `dotfiles/.pytest_cache` and `dotfiles/claude/bin/__pycache__` | Remove the links and prevent recreation through the same Python policy. |
| `dotfiles/claude/skills/proofread/node_modules` | Install a purpose-built proofread runtime outside Drive and make both skill copies invoke it without an in-Drive link. |
| One course `__pycache__` and two course virtual environments | Remove the cache link; recreate or preserve each environment at a named external path and update its launch instructions. |
| `home/.pytest_cache` | Remove the link and prevent recreation. |
| The two Promethease Bootstrap `dist` trees under `Apps` and `Health` | Byte-compare the external contents, then restore them as regular Drive directories; they are vendor assets without a proven reinstall source. |

Cloud copies are reconciled path by path. Disposable caches may be moved to
cloud Trash only after their classification is proven. Promethease assets are
restored as regular content rather than deleted.

## Remaining file-representation migrations

The four earlier categories remain necessary after directory work, but their
old baselines and sequencing are invalid:

- Convert the 36 Epoch-account `.gdoc` files to regular `.url` files handled by
  the fail-closed `epoch-doc:` application. Preserve the permission audit,
  Epoch Chrome-profile routing, pilot, and cloud-parent verification.
- Replace the 16 Enchant forwarding links in dotfiles with a real
  `~/.config/enchant` runtime directory whose external links point directly to
  the unchanged regular canonical dictionary files under
  `~/My Drive/repos/<language>/dict`. The language repositories are not added
  to the personal migration scope.
- Replace Rubric Visualizer's `CLAUDE.md` link with the exact regular
  `@AGENTS.md` import bridge and make parity/update tooling bridge-aware.
- Replace Uqbar's `CLAUDE.md` link with the same import bridge and its
  `build.py` and `launch.py` links with tested regular executable wrappers.

The live inherited worktree
`repos/dont-sleep/.worktrees/safe-local-install` also moves to
`~/repos/.worktrees/dont-sleep/safe-local-install` through Git's supported
worktree mechanism after its owning session, processes, and buffers are
inactive. This relocates a worktree, not the `dont-sleep` main repository, and
does not expand the six-workspace migration scope.

All file-category counts are rebaselined only after directory links reach zero.

## Migration components

### Migration manifest and tool

A small tested tool owns repeated migration mechanics. It provides:

- a read-only audit that records blockers without mutation;
- an explicit old-to-new path manifest;
- a pre-move journal containing Git, filesystem, worktree, submodule, remote,
  consumer, process, and cloud identifiers;
- same-filesystem move, verification, and rollback operations;
- exact configured smoke commands per repository; and
- an append-only completion record for each pilot and batch.

The tool fails closed on a destination collision, different filesystem,
unreachable remote, failure of the exact remote-commit coverage rule, active
process or buffer, unknown nested repository, linked-worktree inconsistency,
uninitialized or dirty submodule that was not recorded, stale consumer, or
verification failure. It never supplies a `--force` path around these checks.

### Live path migration

The migration inventory covers live configuration and consumers, including:

- Epoch's project registry and automation hooks;
- Codex trusted-project entries and Claude/Codex project instructions;
- shell aliases and environment configuration;
- Emacs configuration and package paths;
- launchd jobs, schedulers, service definitions, and working directories;
- repository-local relative dependencies and sibling-repository references;
- scripts and skills with absolute repository paths; and
- resumable agent-session path associations, updated through their supported
  relocation mechanism rather than blanket log rewriting.

Mixed old and new paths during migration are explicit in the manifest. No
runtime resolver probes both locations.

### Regression prevention

- `fix-drive-errors` is rewritten so every in-Drive symlink is an error, not a
  healthy terminal state.
- `nosync` stops creating symlinks and is retired or redirected to explicit
  repository migration or tool-specific configuration.
- Worktree skills and helpers always allocate under `~/repos/.worktrees`.
- Agent instructions and repository-local guards refuse supported dependency
  installation, virtual-environment creation, unsupported builds, or worktree
  creation in a Drive-hosted repository. A repository that cannot enforce the
  relevant operation is migrated instead of being left source-only.
- A reusable audit reports any new symlink under My Drive, stale old-root
  reference, or source-only repository that has acquired generated runtime
  state.
- Reactivating one of the 109 unaffected source-only personal repositories
  starts with migration; it never starts by running the old symlink
  externalizer.

The inherited `dont-sleep/.worktrees/safe-local-install` state is not modified
while its session, process, or buffers are active. It is deferred or moved only
through a verified Git worktree operation, with its Git metadata and live
consumers checked before and after relocation.

## Per-repository transaction

### 1. Preflight while Drive runs

1. Confirm that the destination is absent and both roots have the same device
   ID, making the move an atomic filesystem rename rather than a copy.
2. Record branch, `HEAD`, all refs, upstream divergence, remotes, index,
   staged and unstaged diffs, untracked and ignored files, executable modes,
   symlink targets, submodules, nested repositories, and linked worktrees.
3. Fetch the configured remote, verify its advertised refs, and apply the exact
   remote-commit coverage rule from this specification. Stop on any uncovered
   commit. Do not push implicitly.
4. Inventory every live consumer of the old path.
5. Query running process working directories, active agent sessions, jobs, and
   Emacs file-visiting buffers. Defer an active repository.
6. Record the relevant cloud folder ID and parent chain before local removal.

### 2. Pause Drive and move

1. Pause Drive through its supported UI.
2. Recheck that no preflight state changed.
3. Rename the existing working tree to its exact destination without
   dereferencing symlinks or copying content.
4. Repair linked-worktree metadata and submodule `core.worktree` values through
   Git's supported commands and configuration interfaces.
5. If the move or immediate repository check fails, rename the tree back and
   restore configuration before Drive resumes.

### 3. Update consumers and verify locally

1. Apply only the manifest's live path changes.
2. Compare the complete pre/post Git and filesystem evidence.
3. Run repository-specific tests, builds, editor/CLI checks, and representative
   real commands from the new path.
4. Verify every submodule, linked worktree, sibling dependency, hook, service,
   and session association.
5. Search live configuration for stale old-path references and confirm that
   the old directory has not been recreated.

### 4. Resume Drive and verify cloud convergence

1. Resume Drive only after all local checks pass.
2. Use the Drive API to confirm the recorded old cloud tree enters Trash and
   that no replacement or duplicate tree appears under My Drive or a computer
   backup.
3. Wait for Drive's queues and native error list to settle; record the actual
   category change rather than assuming a decrement.
4. For a removed path whose failed-create record remains, exercise only Drive's
   supported retry or dismissal flow and a normal application restart. Record
   the path-level transition. Never edit Drive's databases or re-register the
   account.
5. Stop the batch on any unexpected cloud creation, route, duplicate, or error.

Pilots run one repository at a time. Later batches may share one pause/resume
window only after the after-resume rollback drill passes. A batch that changes
shared registry or configuration files is one rollback boundary. Within it,
each repository still has its own manifest, exact configuration preimage and
hunks, and verification record.

## In-place repair and residual transaction

The ten personal repairs and 14 residual paths use a separate transaction; a
repository-move journal is not sufficient:

1. Record the link's exact `lstat` data and target, the external target's
   content manifest, modes and hashes, repository state where applicable, live
   consumers, and any existing cloud object and parent IDs.
2. Prove the intended classification. Disposable cache or build state requires
   a named recreation source. User data and vendor assets are preserved by
   default. If the two Promethease copies differ, preserve both and stop for an
   explicit source choice.
3. Prepare and verify replacement configuration or regular content before
   touching the old path. For `add-to-repo/dist`, recheck all three files
   against `HEAD`; any mismatch preserves both versions and stops for an
   explicit source choice or verified rebuild. Retain the external target until
   the restored action passes a real invocation.
4. Pause Drive, recheck the preimage, move only the link itself to Trash without
   following it, and atomically install the verified regular replacement when
   one is required. Never remove the external target in the same transaction.
5. Exercise the exact install, build, cache, environment, vendor-asset, or
   scheduled workflow that could recreate or consume the path. Require the
   intended regular representation and a clean symlink audit.
6. Resume Drive and verify the path's cloud parent, representation, content,
   error-record transition, and absence of a duplicate. On failure, pause Drive
   and restore the recorded local and cloud preimage from Trash before any next
   path.
7. After the defined rollback window, move a verified obsolete external target
   to Trash. Retain a target only when the manifest names it as the canonical
   external runtime or data source. This prevents unowned duplicate vendor or
   writable content from becoming permanent.

No repair batch combines paths that share a configuration file unless that
configuration change has one atomic batch rollback boundary.

## Native error-record convergence gate

The three removed npm links proved that Drive can retain a native failed-create
record after its filesystem source disappears. Before bulk migration, a pilot
must establish the supported path from a removed link to a cleared native
record: queue settlement, any path-specific Retry or Dismiss action exposed by
Drive, and a normal restart. The test records the native item identity and its
observed transition; database disappearance alone is not evidence.

If a stale item survives all supported actions, bulk work stops and the design
is revised. Reconnecting the account and editing Drive's internal databases are
not fallback options.

## Migration sequence

1. **Freeze regression sources.** Prevent new in-Drive worktrees and symlink
   externalizations; wait for or safely relocate the inherited `dont-sleep`
   worktree; refresh the complete filesystem and native error baseline.
2. **Prove native error convergence.** Use one removed-link pilot to establish
   how its native failed-create record clears through supported Drive behavior.
3. **Pilot the migration tool and rollback.** Cover a simple personal
   repository, a Node/Next repository, a Python repository, an Epoch registry
   entry, and a repository with linked worktrees, submodules, or large external
   data. One low-risk pilot must complete a deliberately exercised
   after-resume rollback and a second successful migration before batching is
   allowed.
4. **Move the six proposed personal workspace trees.** Start with clean,
   remote-backed low-risk pilots; migrate stateful repositories only after
   their local-only history and dirty state pass the exact backup gate. Treat
   `ea.news` as the single aggregate submodule transaction defined above.
5. **Repair the ten in-place personal repositories.** Exercise each
   recreation path and preserve every named exception.
6. **Move all 34 Epoch repositories in small batches.** Update the registry,
   hooks, automation, services, and scheduled jobs alongside each batch.
7. **Resolve the 14 non-repository directory links.** Use the explicit
   disposition table; do not apply a basename-only cleanup.
8. **Reach zero directory symlinks and rebaseline.** Restart and settle Drive
   before beginning the file-representation work.
9. **Execute rewritten file-category plans.** Migrate Epoch document pointers,
   Enchant runtime links, and Rubric/Uqbar file representations against the
   new baseline.
10. **Run durability verification.** Repeat state-creating and recreation
    workflows and complete two fresh Drive restarts that both settle at zero
    before claiming success.

## Scheduled and live workflow verification

Repository verification includes every configured user-visible or runtime
surface, not merely unit tests. Affected launchd jobs, schedulers, services,
hooks, editor integrations, and automation commands receive:

1. configuration inspection showing the new path is loaded;
2. a successful controlled invocation; and
3. observation of the next actual scheduler-triggered execution when schedule
   wiring is part of the contract.

A long-interval scheduled job remains pending rather than being declared safe
from a manual invocation alone. No active Emacs session is signaled or
restarted during this work.

## Rollback

Before Drive resumes, rollback renames the repository to its original path,
restores manifest-owned configuration, repairs worktrees, and reruns the local
baseline.

After Drive resumes, the correct order for reconciling the journaled local tree
with the original cloud object is not assumed. A low-risk pilot must inject a
failure after cloud convergence, exercise the supported restore path, and prove
through the Drive API that the restored local tree maps to the journaled object
ID and parent without creating a duplicate. If this drill fails, all later
moves stop and the design is revised. It never reconnects the account or
creates an unverified replacement cloud tree.

In-place and residual rollback follows its own recorded preimage: pause Drive,
remove only the failed replacement through Trash, restore the original local
representation and cloud object, and prove content hashes, object identity, and
parentage before resuming. External targets are retained until the replacement
has passed its real workflow and cloud verification.

No rollback deletes external data targets, ambiguous outputs, untracked user
state, or local-only commits. Category-specific Git commits keep code/config
rollback independent from filesystem and cloud rollback.

## Acceptance criteria

The migration is complete only when all of the following hold:

- `find ~/My\ Drive -type l` exits successfully with empty standard output and
  standard error;
- every moved workspace and nested repository's pre/post manifest matches and
  its configured live workflows pass from the new path;
- every repaired repository passes the operation that could recreate its old
  link without producing a new symlink or persistent Drive failure;
- every residual path matches its approved disposition and content manifest,
  and its real consumer or recreation workflow passes;
- the after-resume rollback pilot restored the journaled cloud object without a
  duplicate, followed by a second successful migration of that pilot;
- every affected scheduled workflow has passed its required live trigger;
- Drive registration still has `is_my_drive=1`, the exact journaled My Drive
  root ID, and zero `machine_root` rows;
- the Drive API shows expected old repository trees in Trash, no unexpected
  cloud creations, and no computer-backup or duplicate tree;
- the native Drive panel shows zero errors after queues settle;
- two post-migration fresh Drive restarts both settle at zero;
- targeted logs contain no persistent create failure for a migrated path;
- the stale-record pilot demonstrated the supported native-record transition,
  and no removed source survives as a stale native error;
- `npm ci`, `uv sync`, pytest, representative builds, browser tooling, and a
  fresh linked worktree complete in their approved external locations without
  recreating an old Drive path; and
- the current zero state survives a final complete symlink and stale-path
  audit.

Passing tests, a clean local invariant, or a transient zero immediately after
restart is not sufficient evidence.

## Documentation and plan consequences

The rewritten implementation plans must separate at least these bounded
projects:

1. migration tooling, path registry, and regression guards;
2. representative repository pilots;
3. personal workspace moves;
4. personal in-place repairs;
5. Epoch repository migration and scheduled-workflow verification;
6. residual non-repository directory repairs;
7. Epoch document links;
8. Claude instruction bridges;
9. Uqbar wrappers;
10. Enchant runtime migration; and
11. final Drive/cloud durability verification.

The former four plans must not be executed by editing their numeric gates in
place. Their valid technical material should be carried into new plans whose
prerequisites and acceptance criteria derive from this specification.
