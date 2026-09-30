# Personal Workspace Moves Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move the six approved personal workspace transactions outside Drive while preserving every Git and filesystem state and leaving the other 119 repository roots in place.

**Architecture:** The pilot plan has already completed `agent-skills`; this plan moves the remaining five one at a time through the journaled rename transaction. Clean and fully remote-covered workspaces move first, stateful workspaces remain blocked until the exact remote gate passes, and each workspace receives its own local, cloud, workflow, and rollback boundary.

**Tech Stack:** `bin/drive-workspace`, Git, npm/Next/Svelte, uv/pytest, Hugo, Google Drive v3, macOS Trash.

---

This is program plan 3 of 12. Require both pilot journal events before starting.
No step pushes a commit, creates a compatibility link, clones a repository, or
copies a working tree. Treat `ea.news` as one workspace containing the parent
and both submodules. Every rename is immediately followed by
`apply-consumers`, `verify-consumers`, and `verify-local`; rollback restores
consumer preimages before the old path. Drive resumes before cloud/native
convergence; only after `close-rollback-window` succeeds does
`stage-consumers` stage reviewed manifest hunks for each logical consumer
commit. No task commits before Drive resumes.

For every task below, `NAME` is the literal manifest name and `JOURNAL` is a
new nonexistent path
`$HOME/.local/state/drive-workspace-migration/personal/NAME.ndjson`. The
standard local transaction means these exact operations, in this order:

```bash
bin/drive-workspace capture "$NAME" --journal "$JOURNAL"
bin/drive-workspace verify-journal --journal "$JOURNAL"
bin/drive-workspace record-cloud "$NAME" --journal "$JOURNAL"
# Pause Drive through the supported UI.
bin/drive-workspace record-drive-state "$NAME" --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused "$NAME" --journal "$JOURNAL"
bin/drive-workspace move "$NAME" --journal "$JOURNAL"
bin/drive-workspace apply-consumers "$NAME" --journal "$JOURNAL"
bin/drive-workspace verify-consumers "$NAME" --journal "$JOURNAL"
bin/drive-workspace verify-local "$NAME" --journal "$JOURNAL"
```

Keep Drive paused while running each repository's local workflow checks. For
each workspace, run the manifest-owned smoke phase exactly once:

```bash
bin/drive-workspace materialize-generated-paths "$NAME" --journal "$JOURNAL"
bin/drive-workspace run-smoke "$NAME" --journal "$JOURNAL"
bin/drive-workspace verify-local "$NAME" --journal "$JOURNAL"
```

The first command changes only links explicitly classified
`materialize_generated_directory`; persistent data/runtime links remain
intact. On rollback, run `rollback-generated-paths` before
`rollback-consumers` and `rollback-local`. Keep Drive paused throughout. The
standard convergence then means:

```bash
# Resume Drive through the supported UI and wait for stable running queues.
bin/drive-workspace verify-cloud "$NAME" --journal "$JOURNAL"
bin/drive-workspace record-native-errors --journal "$JOURNAL"
# Capture DRIVE_PID, perform one normal restart, and wait for settlement.
bin/drive-workspace close-rollback-window --journal "$JOURNAL" \
  --baseline-journal "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson" \
  --restart-before-pid "$DRIVE_PID"
bin/drive-workspace finalize-generated-paths "$NAME" --journal "$JOURNAL"
```

Before resume, a failure runs `rollback-generated-paths`,
`rollback-consumers`, then `rollback-local` while Drive remains paused. After
resume but before the rollback window closes, a failure pauses Drive afresh,
runs `restore-cloud`, `rollback-generated-paths`, `rollback-consumers`, and
`rollback-local` in that order, resumes Drive, and verifies the journaled
original cloud ID, native baseline, local snapshot, and link preimages. There
is no compatibility link or name-based cloud recovery.

## File map

- Already moved by prerequisite: `~/repos/agent-skills`.
- Move: `~/My Drive/repos/80000hours.global` → `~/repos/80000hours.global`.
- Move: `~/My Drive/repos/stafforini.com` → `~/repos/stafforini.com`.
- Move: `~/My Drive/repos/ea.news` → `~/repos/ea.news`.
- Move: `~/My Drive/repos/consensus-trader` → `~/repos/consensus-trader`.
- Move: `~/My Drive/repos/tangodb` → `~/repos/tangodb`.
- Modify: `codex/config.toml` — move only trust entries for these exact workspaces; preserve unrelated concurrent changes.
- Modify: `shell/.zshenv`, `bin/annas-mcp` — Consensus Trader papers path.
- Modify: `karabiner/generate-layouts.py` — Stafforini site image output path.
- Modify: `emacs/extras/paths.el`, `emacs/extras/test/paths-test.el`, `emacs/extras/dired-extras.el`, focused Dired tests, `emacs/extras/doc/paths.org`, and generated `emacs/extras/doc/paths.texi` — add an active external repository root without hiding retained source-only Drive repositories.
- Modify: `stafforini.com/{PUBLISHING.md,scripts/export-notes.el,scripts/sa-lp-refresh.sh,.claude/skills/gsc-indexing-triage/{SKILL.md,references/stafforini-com.md},.codex/skills/gsc-indexing-triage/{SKILL.md,references/stafforini-com.md}}`; create executable `stafforini.com/scripts/render-verify-external`.
- Modify: `~/My Drive/repos/launchd/agents/com.stafforini.{download-pdfs,sa-lp-refresh}.plist` and their exact loaded `~/Library/LaunchAgents` registrations after explicit authorization.
- Modify: `tangodb/{.claude/skills/tangodb-continue/SKILL.md,.codex/skills/tangodb-continue/SKILL.md,backend/scripts/reconciliation/duration_walk_prompt.py}`.
- Modify: `claude/hooks/regenerate-coverage-map.sh` — Rubric remains in Drive, so no change expected; audit only.
- Modify only the exact consumers named in this file map and task file lists; relocate captured active-session associations through the tested adapter. Any additional executable old-root match stops for a reviewed manifest amendment rather than an opportunistic edit.
- Preserve: `~/repos/wuzapi`, all 119 Drive-side repository roots, Consensus data/results targets, Stafforini PDF/thumbnail targets, and Tango browser/data targets.

### Task 1: Freeze and remeasure the five remaining workspaces

**Files:**

- Inspect: five sources, five destinations, external target trees, manifest consumers

- [ ] **Step 1: Capture fresh journals**

For each of the five literal names, create a fresh preflight journal at
`$HOME/.local/state/drive-workspace-migration/personal-preflight/NAME.ndjson`
and run `capture NAME --journal "$PREFLIGHT_JOURNAL"` followed by
`verify-journal --journal "$PREFLIGHT_JOURNAL"`; never infer or discover a
journal filename. These are read-only preflights and are not reused by the
later move. Compare with the
design-time state. Require
the complete staged, unstaged, untracked, ignored, executable, symlink,
submodule, nested-repository, worktree, and remote evidence. If state changed,
update only the journal and smoke-command facts; do not normalize it.

- [ ] **Step 2: Apply the remote gate without publication**

Require every protected commit to be reachable from an advertised remote ref.
At design time, `ea.news-front`, Consensus Trader, and TangoDB had local-only
commits, so expect those workspaces to stop until separately authorized
publication has made the gate true. Record exact uncovered OIDs; do not push or
waive them.

- [ ] **Step 3: Audit all live consumers**

Search the canonical configuration roots for each literal old path. Classify
matches as live configuration, repository-local relative reference, resumable
session association, or historical prose/log. Require every executable match
to equal a reviewed manifest consumer; an additional match stops for a plan
amendment. Put session associations in the journal. Historical material is not
rewritten.

- [ ] **Step 4: Add the split Emacs repository-root model**

Using the `elisp-conventions` skill, add
`paths-dir-active-personal-repos` with default `~/repos` while keeping
`paths-dir-personal-repos` pointed at `~/My Drive/repos`. Update active-project
and worktree commands to use the new external root; keep source browsing and
Dired access to all 119 retained Drive roots. Add ERT coverage for both values,
update the Org manual, regenerate the Texinfo output through the documented
command, and run the focused paths and Dired test suites. Do not reload or
signal the active Emacs session.

### Task 2: Move `80000hours.global`

**Files:**

- Move: `~/My Drive/repos/80000hours.global` → `~/repos/80000hours.global`
- Modify: exact trust/session consumers recorded in its journal

Set `NAME=80000hours.global` and
`JOURNAL="$HOME/.local/state/drive-workspace-migration/personal/80000hours.global.ndjson"`
for every standard-transaction command in this task.

- [ ] **Step 1: Run the complete preflight and pause gate**

Require clean Git state, full remote coverage, absent destination, matching
device ID, no active process/buffer/session, exact cloud ID, and fresh
characterized native paused-state plus stable queue-cursor evidence by running
the standard local transaction through `confirm-drive-paused`.

- [ ] **Step 2: Rename and update consumers**

Run the remaining standard local commands: `move`, `apply-consumers`,
`verify-consumers`, and `verify-local`, all with the literal name and journal.
Do not dereference `.venv`, `frontend/node_modules`, `.next`, or cache links.

- [ ] **Step 3: Exercise both runtime stacks from the new root**

Run the standard manifest-owned smoke phase. Require its uv, pytest, report,
frontend install, typecheck, and build records all to pass, no path below the
old Drive root to reappear, and
any generated replacement to live under the new workspace or its named
external runtime target.
If `npm ci` replaced a moved `node_modules` link, retain its journaled external
target through the rollback window; only the tested
`finalize-generated-paths` command may move it after closure.

- [ ] **Step 4: Resume and verify Drive/cloud convergence**

Run the standard convergence. Require the journaled old cloud folder in Trash, no duplicate or computer
backup, clearance of every native record tied to the moved source path with no
new destination-path record, and one normal restart without a recreated old
path. Require all unrelated baseline records to remain unchanged.

- [ ] **Step 5: Commit consumers after convergence**

Run `bin/drive-workspace stage-consumers 80000hours.global --journal "$JOURNAL"`, verify only
manifest-owned hunks are staged, and commit each repository-local or dotfiles
consumer as its own single-purpose commit. Preserve unrelated changes and do
not push.

### Task 3: Move `stafforini.com`

**Files:**

- Move: `~/My Drive/repos/stafforini.com` → `~/repos/stafforini.com`
- Modify: `codex/config.toml`, `karabiner/generate-layouts.py`, both `add-to-emacs-packages` skills, Stafforini publishing/GSC paths, and the two canonical launchd plists named in the file map

Set `NAME=stafforini.com` and
`JOURNAL="$HOME/.local/state/drive-workspace-migration/personal/stafforini.com.ndjson"`
for every standard-transaction command in this task.

- [ ] **Step 1: Preserve the untracked script and external asset targets**

Before `capture`, create `scripts/render-verify-external`, require
`RENDER_ROOT` to resolve outside My Drive, invoke `hugo --minify --config
hugo.toml,hugo.deploy.toml --destination "$RENDER_ROOT" --noBuildLock --quiet`,
then run `python3 scripts/verify-site.py --dir "$RENDER_ROOT"`. Reject a
missing, symlinked, or Drive-resolving render root before invoking Hugo. Set and
test executable mode `100755`, test both the rejection and external-render
paths, and commit only this helper as a separate preparatory commit. Do not
start `capture` until the normal remote-coverage gate proves that exact helper
commit reachable from an advertised remote ref; this plan does not push it.

Record the hash of `scripts/audit-bib-abstracts.py` and the exact targets and
manifests for `static/pdfs`, `static/pdf-thumbnails`, `public`, and
`node_modules`. The PDFs and thumbnails remain canonical external data; never
copy or delete them during this move. Include `refs/stash` in the remote
coverage set and stop if any stash commit is not reachable from an advertised
remote ref; do not drop, apply, or rewrite the stash.

Before starting the move, obtain the user's explicit authorization to boot out,
repoint, bootstrap, controlled-invoke, and observe the next triggers of
`com.stafforini.download-pdfs` and `com.stafforini.sa-lp-refresh`. If it is not
granted, defer this repository. Complete `capture`, `verify-journal`,
`record-cloud`, process/buffer/session checks, destination/device checks, and
remote coverage first. Immediately before the pause/mutation boundary, record
the bytes and modes of these exact loaded plists and the output of both
`launchctl print` commands in the transaction's private preimage directory:

```bash
uid="$(id -u)"
launchctl print "gui/$uid/com.stafforini.download-pdfs"
launchctl print "gui/$uid/com.stafforini.sa-lp-refresh"
launchctl bootout "gui/$uid" "$HOME/Library/LaunchAgents/com.stafforini.download-pdfs.plist"
launchctl bootout "gui/$uid" "$HOME/Library/LaunchAgents/com.stafforini.sa-lp-refresh.plist"
```

Verify both labels are absent before pausing Drive. If any later pre-resume
step aborts, restore the exact two loaded-plist preimages, then run:

```bash
launchctl bootstrap "gui/$uid" "$HOME/Library/LaunchAgents/com.stafforini.download-pdfs.plist"
launchctl bootstrap "gui/$uid" "$HOME/Library/LaunchAgents/com.stafforini.sa-lp-refresh.plist"
launchctl print "gui/$uid/com.stafforini.download-pdfs"
launchctl print "gui/$uid/com.stafforini.sa-lp-refresh"
```

Do not leave either originally loaded job unloaded on an aborted transaction.

- [ ] **Step 2: Move and update path consumers**

Do not recapture the journal created in Step 1. Pause Drive and run only the
remaining local phase:

```bash
bin/drive-workspace record-drive-state stafforini.com --journal "$JOURNAL"
bin/drive-workspace confirm-drive-paused stafforini.com --journal "$JOURNAL"
bin/drive-workspace move stafforini.com --journal "$JOURNAL"
bin/drive-workspace apply-consumers stafforini.com --journal "$JOURNAL"
bin/drive-workspace verify-consumers stafforini.com --journal "$JOURNAL"
bin/drive-workspace verify-local stafforini.com --journal "$JOURNAL"
```

Change the Hugo
base path in both paired `add-to-emacs-packages` skills to
`~/repos/stafforini.com/`, keep bodies paired, and move only the exact Codex
trust entry. Preserve unrelated `codex/config.toml` edits.
Update the two source plists and their exact `~/Library/LaunchAgents` copies
through journaled consumer substitutions, verify `plutil -lint` on all four,
then bootstrap the two new loaded plists with the exact `launchctl bootstrap`
commands above. Require `launchctl print` to show the new external working
directory/program path before the controlled invocations.

- [ ] **Step 3: Run the real site workflow**

Run the standard manifest-owned smoke phase and require its install, test,
external render/verification, and three exact data-directory assertions to
pass. The already committed helper from Step 1 is the smoke entry point. The
tested runner supplies `RENDER_ROOT={transaction_tmp}/stafforini-render` and
owns its cleanup. Do not run the live `npm run build`, which writes through the
preserved `public` link. Require the untracked script hash unchanged and all
large external links still resolving. If `npm` replaces `node_modules`, keep
the new runtime and let only `finalize-generated-paths` move the
now-unreferenced old target after cloud verification and rollback-window close.

Until both
loaded definitions, controlled invocations, and next actual triggers pass from
the new path, leave this transaction pending. Do not infer scheduler safety
from the source plist alone.

- [ ] **Step 4: Resume and verify cloud/native state**

Run standard convergence through `record-native-errors`, but do not run
`close-rollback-window` until both controlled invocations and the next actual
triggers have passed from the new path. Then perform the required normal Drive
restart and run the exact `close-rollback-window` command. A job failure before
that close uses the after-resume rollback sequence and restores the original
loaded plist preimages. After the close succeeds, run
`bin/drive-workspace finalize-generated-paths stafforini.com --journal
"$JOURNAL"`; never finalize an old generated target before that event.

- [ ] **Step 5: Commit Stafforini consumers after convergence**

After local, cloud, native, loaded-job, controlled-invocation, next-trigger,
and rollback-window checks pass, run `bin/drive-workspace stage-consumers stafforini.com --journal
"$JOURNAL"`. Commit the Stafforini-local consumers, paired dotfiles skills,
Karabiner path, launchd source plists, and trust entry as separate logical
commits. Preserve unrelated changes and do not push.

### Task 4: Move the aggregate `ea.news` workspace

**Files:**

- Move: `~/My Drive/repos/ea.news` → `~/repos/ea.news`
- Preserve external Git directory: `~/git-dirs/ea.news`
- Modify: submodule `core.worktree` values through Git configuration

Set `NAME=ea.news` and
`JOURNAL="$HOME/.local/state/drive-workspace-migration/personal/ea.news.ndjson"`
for every standard-transaction command in this task.

- [ ] **Step 1: Require remote coverage for all three repository roots**

Record parent HEAD, API HEAD `ddec50f…` or its freshly remeasured successor,
frontend branch `separate-inactive-sites`, frontend HEAD, and the parent's
pre-existing frontend gitlink difference. Stop if any current commit is not
remote-covered.

- [ ] **Step 2: Move the whole workspace atomically**

Run the complete standard local transaction. Its single `move ea.news` renames
only the parent directory and leaves `~/git-dirs/ea.news` in place. The tested
move implementation updates parent, API, and frontend worktree registrations
to the three new paths with Git config/worktree repair; this must also remove
the API's stale Dropbox `core.worktree` value.

- [ ] **Step 3: Verify recursive Git behavior before building**

Run:

```bash
git -C "$HOME/repos/ea.news" status --short --branch
git -C "$HOME/repos/ea.news" submodule status --recursive
git -C "$HOME/repos/ea.news/ea.news-api" status --short --branch
git -C "$HOME/repos/ea.news/ea.news-front" status --short --branch
```

Require the API clean at its captured HEAD, the frontend clean at its captured
HEAD, and the parent to show only the captured frontend gitlink difference.

- [ ] **Step 4: Exercise the frontend workflow**

Run the standard manifest-owned smoke phase, which contains separate API test,
frontend install, lint, build, and per-Git-root status records. Require every
status separately, no second checkout, and no old Drive path. Then run the standard
convergence. If `npm ci` replaced the moved
`node_modules` link, retain the old generated target until
`close-rollback-window`; then let `finalize-generated-paths` prove no live link
references it and move only that target to Trash.
Run `bin/drive-workspace stage-consumers ea.news --journal "$JOURNAL"` after
cloud/native convergence, then commit any exact Git-metadata or consumer
configuration change immediately;
do not stage the captured parent gitlink difference.

### Task 5: Move `consensus-trader`

**Files:**

- Move: `~/My Drive/repos/consensus-trader` → `~/repos/consensus-trader`
- Modify: `shell/.zshenv`, `bin/annas-mcp`, `codex/config.toml`, paired repository skills or scripts containing the old absolute root
- Preserve external targets: `data` and `results`

Set `NAME=consensus-trader` and
`JOURNAL="$HOME/.local/state/drive-workspace-migration/personal/consensus-trader.ndjson"`
for every standard-transaction command in this task.

- [ ] **Step 1: Require the exact backup and dirty-state gates**

Require all captured commits remote-covered and preserve both untracked log
files. Record hashes/manifests for `data`, `results`, `.venv`, and every tracked
mode-`120000` cache link. Never untrack or dereference them.

- [ ] **Step 2: Move and update consumers**

Run the standard local-only transaction from `capture` through the first
`verify-local`; do not run smoke or convergence yet. Change `bin/annas-mcp` to
`$HOME/repos/consensus-trader/papers` and update exact trust/skill paths. Run
the dotfiles wrapper test or a guarded dry-run that proves the resolved path
without downloading content.

- [ ] **Step 3: Exercise the Python/data contract**

Run the standard manifest-owned smoke phase; its transaction-owned temporary
root and uv cache are cleaned in `finally` by the tested runner. Then require
`data` and `results` to be directories and compare `git status --porcelain=v2
--branch` with the journal. Require the two captured untracked logs and all pre-existing tracked state to
match. Run the standard convergence, then run `bin/drive-workspace
stage-consumers consensus-trader --journal "$JOURNAL"` and commit
the exact `shell/.zshenv`, `bin/annas-mcp`, trust, and paired consumer changes
as single-purpose commits.

### Task 6: Move `tangodb`

**Files:**

- Move: `~/My Drive/repos/tangodb` → `~/repos/tangodb`
- Modify: `codex/config.toml`, paired `tangodb-continue` skills, `backend/scripts/reconciliation/duration_walk_prompt.py`, and every audited live browser/data consumer
- Preserve: browser state and all captured tracked/ignored links

Set `NAME=tangodb` and
`JOURNAL="$HOME/.local/state/drive-workspace-migration/personal/tangodb.ndjson"`
for every standard-transaction command in this task.

- [ ] **Step 1: Require remote coverage and record deleted tracked files**

The four pre-existing tracked Tango text deletions and every untracked/ignored
path must appear identically in the journal. Stop until all 43 design-time
local-only commits, or their freshly remeasured successors, pass remote
coverage.

- [ ] **Step 2: Move and verify the monorepo**

Run the standard local-only transaction from `capture` through the first
`verify-local`; do not run smoke or convergence yet. Update exact consumers,
and require all tracked mode-`120000` browser/cache links to resolve outside
Drive from the new owner.

- [ ] **Step 3: Exercise backend, admin, and web surfaces**

Run the standard manifest-owned smoke phase and require every backend, admin,
and web status separately. Require ports 8000, 5173, and 5174 initially free,
then run:

```bash
bin/drive-workspace smoke-start tangodb --service backend --journal "$JOURNAL"
bin/drive-workspace smoke-start tangodb --service web --journal "$JOURNAL"
bin/drive-workspace smoke-start tangodb --service admin --journal "$JOURNAL"
bin/drive-workspace smoke-check tangodb --service backend --journal "$JOURNAL"
bin/drive-workspace smoke-check tangodb --service web --journal "$JOURNAL"
bin/drive-workspace smoke-check tangodb --service admin --journal "$JOURNAL"
bin/drive-workspace smoke-stop tangodb --service admin --journal "$JOURNAL"
bin/drive-workspace smoke-stop tangodb --service web --journal "$JOURNAL"
bin/drive-workspace smoke-stop tangodb --service backend --journal "$JOURNAL"
```

Install an exit trap before the first start that invokes `smoke-stop` in reverse
order for every service whose start event exists. Require the backend response
to contain exactly the manifest-required top-level keys, both Svelte responses
to have HTTP 200 and contain `<html`, all three process groups to be empty, and
all three ports to be bindable after cleanup.
Require persistent browser state intact and no old Drive path recreation.
For each `npm ci` replacement, retain its journaled old generated target until
the rollback window closes, then use only `finalize-generated-paths` to prove it
is unreferenced and move that target to Trash; never finalize the browser/data
targets named `preserve`.
Run the standard convergence, then run `bin/drive-workspace stage-consumers
tangodb --journal "$JOURNAL"` and commit
the paired `tangodb-continue` skills, `duration_walk_prompt.py`, trust, and any
other exact live manifest consumer as separate logical commits.

### Task 7: Close the personal migration phase

**Files:**

- Inspect: all six destinations, all old sources, 119 remaining Drive repository roots

- [ ] **Step 1: Verify exact scope**

Require the six destination workspace names, eight nested repository roots,
and no other moved personal root. Require `~/repos/wuzapi` unchanged and the
119 retained roots still present.

- [ ] **Step 2: Verify live consumers and configuration parity**

Search live config for all six old roots; require no live occurrence. Run
`bin/ai-config-sync audit`, focused wrapper/skill tests, Emacs paths/Dired ERT
tests in batch mode, Karabiner's generator check, and the full migration audit.
Require `paths-dir-active-personal-repos` to resolve to `~/repos` and
`paths-dir-personal-repos` to continue resolving to `~/My Drive/repos`.
Historical logs and documents may still contain old paths.

- [ ] **Step 3: Verify the per-transaction commits**

Require every logical consumer/config change to have been committed after its
workspace resumed and its cloud/native rollback window closed, and before the
next workspace transaction began. Check staged state is empty except for
journaled pre-existing changes. Do not push any commit as part of this plan.
