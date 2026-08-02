# Personal In-Place Drive Repairs Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove the remaining 12 directory symlinks from ten retained personal repositories and prevent every supported recreation path from writing runtime state into Drive.

**Architecture:** Each repository receives an exact preimage journal and its own pause/resume boundary. Disposable generated state is removed through Trash, tracked content is restored from verified Git bytes, supported build/install entrypoints fail closed or route runtime state to a named external environment, and every repair is exercised before its obsolete external target is finalized.

**Tech Stack:** Git, npm lifecycle scripts, Python/setuptools/uv/pytest, launchd, Bash, macOS Trash, `bin/drive-workspace`.

---

This is program plan 4 of 12. Start after the personal workspace-move phase has
closed. Do not move any of these ten repository roots. Process one repository
at a time with `drive-workspace capture-path`, `confirm-path-paused`,
`verify-path-local`, `record-path-cloud`, and `verify-path-cloud`. Keep each
external target until `rollback-path-local` is no longer needed.

## File map

- Create: `drive-path-repairs.json` — exact declarative inventory and journal/finalization policy for all 12 path repairs.
- Create: `bin/drive-path-repair` and `tests/test_drive_path_repair.py` — manifest-driven orchestration over the fail-closed `drive-workspace` path commands.
- Modify: `~/My Drive/repos/80k-website-old/package.json`; create `scripts/refuse-drive-runtime.mjs`.
- Restore: `~/My Drive/repos/add-to-repo/dist/{index.js,index.js.map,licenses.txt}` from verified `HEAD`; preserve `action.yml`.
- Modify: archived Polymarket install/build documentation and guarded entrypoint files identified below.
- Modify: `~/My Drive/repos/launchd/pytest.ini` (create) and its tests/docs.
- Preserve: `~/My Drive/repos/archive/polymarket-bot-2/pmbot_cache.duckdb`.
- Modify: `~/My Drive/repos/pass-utils/README.org` and guarded install wrapper.
- Modify: `~/My Drive/repos/rubric-visualizer/AGENTS.md`, `README.md`; separately leave `CLAUDE.md` for program plan 9.
- Modify: `bin/gmail-maildir-sync` — set bytecode prevention explicitly for noninteractive use.
- Create: `~/My Drive/repos/wikipedia-deletion-analysis/scripts/uv-external`; modify its README and launchd plist.
- Modify: `~/My Drive/repos/launchd/agents/com.benthamite.wikipedia-deletion-analysis.plist` to match the repository source plist.
- Remove through Trash after preimage capture: the exact 12 symlink paths named in the design.

### Task 1: Add the shared repair preflight and recreation checklist

**Files:**

- Create: `drive-path-repairs.json`
- Create: `bin/drive-path-repair`
- Create: `tests/test_drive_path_repair.py`
- Inspect: all ten repositories and their external targets

- [ ] **Step 1: Create and test the exact repair manifest and driver**

Write the failing manifest/driver test first. It must assert these exact 12
records, with no unknown key, duplicate label/path/journal, or path outside
`~/My Drive`; every journal expands below the external state root and every
target policy is `disposable`:

| Label | Path | Journal | Final type | Replacement |
| --- | --- | --- | --- | --- |
| `80k-old-node-modules` | `~/My Drive/repos/80k-website-old/node_modules` | `path-repairs/80k-old-node-modules/journal.ndjson` | `absent` | none |
| `80k-old-next` | `~/My Drive/repos/80k-website-old/.next` | `path-repairs/80k-old-next/journal.ndjson` | `absent` | none |
| `add-to-repo-dist` | `~/My Drive/repos/add-to-repo/dist` | `path-repairs/add-to-repo-dist/journal.ndjson` | `directory` | Git `HEAD`: `dist/index.js`, `dist/index.js.map`, `dist/licenses.txt` |
| `polymarket-bot-egg-info` | `~/My Drive/repos/archive/polymarket-bot/polymarket_bot.egg-info` | `path-repairs/polymarket-bot-egg-info/journal.ndjson` | `absent` | none |
| `polymarket-bot-2-egg-info` | `~/My Drive/repos/archive/polymarket-bot-2/src/pmbot.egg-info` | `path-repairs/polymarket-bot-2-egg-info/journal.ndjson` | `absent` | none |
| `polymarket-traders-build` | `~/My Drive/repos/archive/polymarket-traders/build` | `path-repairs/polymarket-traders-build/journal.ndjson` | `absent` | none |
| `gmail-maildir-sync-pycache` | `~/My Drive/repos/gmail-maildir-sync/gmail_maildir_sync/__pycache__` | `path-repairs/gmail-maildir-sync-pycache/journal.ndjson` | `absent` | none |
| `launchd-pytest-cache` | `~/My Drive/repos/launchd/.pytest_cache` | `path-repairs/launchd-pytest-cache/journal.ndjson` | `absent` | none |
| `pass-utils-egg-info` | `~/My Drive/repos/pass-utils/pass_utils.egg-info` | `path-repairs/pass-utils-egg-info/journal.ndjson` | `absent` | none |
| `rubric-pytest-cache` | `~/My Drive/repos/rubric-visualizer/.pytest_cache` | `path-repairs/rubric-pytest-cache/journal.ndjson` | `absent` | none |
| `rubric-pycache` | `~/My Drive/repos/rubric-visualizer/__pycache__` | `path-repairs/rubric-pycache/journal.ndjson` | `absent` | none |
| `wikipedia-venv` | `~/My Drive/repos/wikipedia-deletion-analysis/.venv` | `path-repairs/wikipedia-venv/journal.ndjson` | `absent` | none |

Run `python3 -m unittest tests.test_drive_path_repair -v`; expected: FAIL
because the manifest and driver do not exist. Then create the JSON with exactly
the table values and implement the driver subcommands described below; do not
add an interactive or name-discovery mode.

The journal column is relative to
`~/.local/state/drive-workspace-migration/`; the driver expands it and never
discovers a name. `prepare LABEL` executes `capture-path LABEL --journal
ABSOLUTE_JOURNAL --path EXACT_PATH --final-type TYPE --target-policy
disposable`, adding the three exact `--replacement` Git paths only for
`add-to-repo-dist`, then executes `record-path-cloud`. It records `lstat`, link
text, external target manifest/hashes, Git mode/status, live consumers, and
cloud identity. Require the repository link counts `2,1,1,1,1,1,1,1,2,1` in
the design order. A mismatch blocks before code changes. Never combine two
preimages under one label.

Test the driver with a spy `drive-workspace` and assert the exact argv for all
12 labels and all lifecycle commands. Set `bin/drive-path-repair` mode `100755`
and require `stat -f '%Lp'` and Git mode `100755`. Re-run
`python3 -m unittest tests.test_drive_path_repair -v`; expected: PASS before
using the driver on a live path.

- [ ] **Step 2: Capture all 12 preimages before code changes and classify targets**

Run `bin/drive-path-repair prepare LABEL --manifest drive-path-repairs.json`
once for each of the 12 labels in table order, followed by `verify-journal` on
the exact journal. This is the only `prepare` call for each label; all must
succeed before editing any repository. Mark caches, egg metadata, old `build`, `.next`, and `node_modules` as
disposable only after verifying their regeneration source. Mark the DuckDB file
as user state. Mark `add-to-repo/dist` as tracked release content. Do not Trash
any target during preflight.

- [ ] **Step 3: Use one exact transaction protocol for every label**

For a task's literal `LABEL`, run:

```bash
bin/drive-path-repair verify-prepared "$LABEL" --manifest drive-path-repairs.json
# Pause Drive through the supported UI.
bin/drive-path-repair pause-confirm "$LABEL" --manifest drive-path-repairs.json
bin/drive-path-repair mutate "$LABEL" --manifest drive-path-repairs.json
bin/drive-path-repair verify-local "$LABEL" --manifest drive-path-repairs.json
# Resume Drive through the supported UI and wait for stable running queues.
bin/drive-path-repair converge "$LABEL" --manifest drive-path-repairs.json \
  --baseline-journal "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson"
# Capture DRIVE_PID, perform one normal restart, and wait for settlement.
bin/drive-path-repair close "$LABEL" --manifest drive-path-repairs.json \
  --baseline-journal "$HOME/.local/state/drive-workspace-migration/program-baseline.ndjson" \
  --restart-before-pid "$DRIVE_PID"
bin/drive-path-repair finalize "$LABEL" --manifest drive-path-repairs.json
```

`verify-prepared` requires the exact Step 2 journal and rejects any path,
target, path-specific Git mode/status, consumer, or cloud drift; it never
refreshes a preimage. It reports repository-wide status changes and permits
only the task's reviewed guard/document files listed under **Files**; an
unlisted or path-overlapping change blocks.
`mutate` revalidates the hash-chained journal, moves only the exact symlink to
Trash, and leaves its target untouched. For `add-to-repo-dist` only, it then
runs the exact three-path `git restore --source=HEAD --worktree -- ...` argv.
`converge` calls `verify-path-cloud` and `record-native-errors`; `close` calls
`close-rollback-window`; `finalize` calls `finalize-path`. Before resume, any
failure runs `rollback-path-local` while Drive remains paused. After resume but
before `close`, any failure pauses Drive afresh, runs `confirm-path-paused`,
`restore-path-cloud`, and `rollback-path-local`, resumes Drive, and requires the
original path type/link text, cloud ID or journaled absence, parent, and native
baseline. After `close`, rollback is deliberately unavailable and only the
verified unreferenced disposable target/private preimage may be finalized.

### Task 2: Guard and clean `80k-website-old`

**Files:**

- Create: `scripts/refuse-drive-runtime.mjs`
- Modify: `package.json`
- Remove through Trash: `node_modules`, `.next` symlinks

- [ ] **Step 1: Add a failing lifecycle-guard test**

Create a Node test that runs the guard with a fake cwd below `My Drive` and
outside it. Require exit 78 plus the external-migration message under Drive and
exit zero outside.

- [ ] **Step 2: Implement the guard and package lifecycle hooks**

Use `fs.realpathSync(process.cwd())` and reject any cwd equal to or below
`path.join(os.homedir(), "My Drive")`. Wire the script to `preinstall`,
`predev`, `prebuild`, `prestart`, and `prelint` without changing the existing
commands.

- [ ] **Step 3: Verify refusal before removing links**

Run the Node test and `npm run build` from the live Drive root. Require the
explicit refusal before npm can touch either link.

- [ ] **Step 4: Run the repair transaction**

Run the Task 1 protocol separately for `80k-old-node-modules` and
`80k-old-next`. After each `mutate` and `verify-local`, but before resuming
Drive, run `npm run build` from the live Drive root and require exit `78` with
the guard message. After the second mutation, additionally require
`test ! -e node_modules && test ! -L node_modules && test ! -e .next && test !
-L .next`. Only then resume and continue `converge`, restart, `close`, and
`finalize` for that label. Thus the supported post-removal build proves neither
path can be recreated; the pre-removal test alone is insufficient.

- [ ] **Step 5: Commit the repository guard**

Commit only `package.json`, the guard, and its test with
`chore: refuse runtime builds inside Drive`.

### Task 3: Restore `add-to-repo/dist` as tracked regular files

**Files:**

- Restore: `dist/index.js`, `dist/index.js.map`, `dist/licenses.txt`
- Preserve: `action.yml`

- [ ] **Step 1: Recompare the external files with `HEAD`**

Require byte equality and modes for all three files. Compare against a
temporary `git archive HEAD dist` extraction rather than following the link.
Any mismatch preserves
both copies and stops for an explicit source choice or verified rebuild.
Use this complete harness before the first fallible archive operation:

```bash
ORIGINAL_CWD="$PWD"
REPO="$HOME/My Drive/repos/add-to-repo"
ARCHIVE_STAGE="$(mktemp -d "${TMPDIR:-/tmp}/add-to-repo-compare.XXXXXX")"
cleanup_archive_stage() {
  cd "$ORIGINAL_CWD" || return 1
  if [ -e "$ARCHIVE_STAGE" ]; then trash "$ARCHIVE_STAGE" || return 1; fi
  test ! -e "$ARCHIVE_STAGE"
}
trap cleanup_archive_stage EXIT
trap 'cleanup_archive_stage || exit 1; trap - EXIT; exit 129' HUP
trap 'cleanup_archive_stage || exit 1; trap - EXIT; exit 130' INT
trap 'cleanup_archive_stage || exit 1; trap - EXIT; exit 143' TERM
chmod 700 "$ARCHIVE_STAGE"
git -C "$REPO" archive --format=tar \
  --output="$ARCHIVE_STAGE/source.tar" HEAD dist
tar -xf "$ARCHIVE_STAGE/source.tar" -C "$ARCHIVE_STAGE"
trash "$ARCHIVE_STAGE/source.tar"
cmp "$ARCHIVE_STAGE/dist/index.js" "$REPO/dist/index.js"
cmp "$ARCHIVE_STAGE/dist/index.js.map" "$REPO/dist/index.js.map"
cmp "$ARCHIVE_STAGE/dist/licenses.txt" "$REPO/dist/licenses.txt"
test "$(stat -f '%Lp' "$ARCHIVE_STAGE/dist/index.js")" = \
  "$(stat -f '%Lp' "$REPO/dist/index.js")"
test "$(stat -f '%Lp' "$ARCHIVE_STAGE/dist/index.js.map")" = \
  "$(stat -f '%Lp' "$REPO/dist/index.js.map")"
test "$(stat -f '%Lp' "$ARCHIVE_STAGE/dist/licenses.txt")" = \
  "$(stat -f '%Lp' "$REPO/dist/licenses.txt")"
cleanup_archive_stage || exit 1
trap - EXIT HUP INT TERM
```

Use a fresh stage and the same cleanup pattern in Step 3; never reuse a trashed
stage.

- [ ] **Step 2: Replace the link while Drive is paused**

Run `verify-prepared add-to-repo-dist` and `pause-confirm add-to-repo-dist` from the
Task 1 protocol, then run `mutate add-to-repo-dist`. That command moves only
the untracked `dist` symlink to Trash and executes:

```bash
git restore --source=HEAD --worktree -- dist/index.js dist/index.js.map dist/licenses.txt
```

Require three regular tracked files and no other status change.
Run `verify-local add-to-repo-dist` before any resume.

- [ ] **Step 3: Verify the bundled action from an external staged tree**

Require `test ! -L dist`, `git diff --exit-code -- dist`, and
`node --check dist/index.js` in the live source. Create no clone or worktree.
Use an immutable archive plus a task-owned external temporary directory,
without a pipeline whose first failure could be masked:

```bash
ORIGINAL_CWD="$PWD"
ADD_TO_REPO_STAGE="$(mktemp -d "${TMPDIR:-/tmp}/add-to-repo-verify.XXXXXX")"
cleanup_add_to_repo_stage() {
  cd "$ORIGINAL_CWD" || return 1
  if [ -e "$ADD_TO_REPO_STAGE" ]; then trash "$ADD_TO_REPO_STAGE" || return 1; fi
  test ! -e "$ADD_TO_REPO_STAGE"
}
trap cleanup_add_to_repo_stage EXIT
trap 'cleanup_add_to_repo_stage || exit 1; trap - EXIT; exit 129' HUP
trap 'cleanup_add_to_repo_stage || exit 1; trap - EXIT; exit 130' INT
trap 'cleanup_add_to_repo_stage || exit 1; trap - EXIT; exit 143' TERM
chmod 700 "$ADD_TO_REPO_STAGE"
git -C "$HOME/My Drive/repos/add-to-repo" archive --format=tar \
  --output="$ADD_TO_REPO_STAGE/source.tar" HEAD
tar -xf "$ADD_TO_REPO_STAGE/source.tar" -C "$ADD_TO_REPO_STAGE"
trash "$ADD_TO_REPO_STAGE/source.tar"
cd "$ADD_TO_REPO_STAGE"
npm ci
npm test
npx tsc --noEmit
npm run build:package
cmp dist/index.js "$HOME/My Drive/repos/add-to-repo/dist/index.js"
cmp dist/index.js.map "$HOME/My Drive/repos/add-to-repo/dist/index.js.map"
cmp dist/licenses.txt "$HOME/My Drive/repos/add-to-repo/dist/licenses.txt"
cleanup_add_to_repo_stage || exit 1
trap - EXIT HUP INT TERM
```

The archive contains no `.git`, cannot update the live index, and is not a
linked worktree. The idempotent handler restores the original cwd before
moving the exact task-created stage to Trash and clears its traps only after
absence is verified. A genuinely
live GitHub Action invocation changes a project and is not authorized by this
plan. Keep the external target until local checks and cloud convergence pass,
then resume Drive and finish the `add-to-repo-dist` protocol through
`converge`, the required restart, `close`, and `finalize`.

- [ ] **Step 4: Commit only the restored representation if Git records a change**

Do not rebuild or publish the action in this plan.

### Task 4: Repair the three archived Polymarket repositories

**Files:**

- Modify: `archive/polymarket-bot/setup.py`, `README.md`
- Modify: `archive/polymarket-bot-2/pyproject.toml`, `README.md`
- Modify: `archive/polymarket-traders/package.json`
- Create in each active build surface: a Drive-runtime refusal test/script
- Remove through Trash: two egg-info links and one `build` link

- [ ] **Step 1: Add fail-closed supported entrypoints**

For `polymarket-bot`, remove the README's editable-install path and add a
source-only guard that states renewed development requires migration. Build a
wheel from a private staged copy outside Drive and install it non-editably into
`~/.local/share/python-envs/polymarket-bot`; verify
`polymarket_bot.__file__` is below that environment and run
`polymarket-analyze-trader --help`.

For `polymarket-bot-2`, treat the broken pyenv registration as its own
reversible external-environment transaction. Require these exact existing
members and prove `python -m pip show -f pmbot`/the distribution `RECORD` owns
them before mutation:

```text
~/.pyenv/versions/3.11.9/lib/python3.11/site-packages/__editable__.pmbot-0.1.0.pth
~/.pyenv/versions/3.11.9/lib/python3.11/site-packages/pmbot-0.1.0.dist-info/
~/.pyenv/versions/3.11.9/bin/pmbot
```

Create mode-`0700`
`~/.local/state/drive-workspace-migration/path-repairs/pmbot-pyenv-registration/`,
save `pip show -f`, the `RECORD`, `stat` output, and a `tar -cpf
preimage.tar` made relative to `~/.pyenv/versions/3.11.9`, then save the
archive SHA-256 and fsync it. Run only
`~/.pyenv/versions/3.11.9/bin/python -m pip uninstall --yes pmbot`; require all
three members absent and `python -I -c 'import pmbot'` to fail with
`ModuleNotFoundError`. Until `polymarket-bot-2-egg-info` closes its rollback
window, any failure restores only those validated archive members with `tar
-xpf` relative to the same pyenv root, rechecks the SHA-256/modes, and proves
the original import/console state. After closure, append a finalization record
and move only this task-created archive to Trash.

For Polymarket Traders, add the same Node guard as `80k-website-old` and wire
exact hooks `preinstall`, `pretemplatify:matic`, `precodegen`, `predeploy`, and
`prestart`. The repository has no `build` script, so do not claim to guard one.

- [ ] **Step 2: Test the guards before cleanup**

Require Drive-local editable/build operations to refuse. Verify the removed
`pmbot` registration no longer points at Dropbox and `import pmbot` fails with
the expected absent-package error.

- [ ] **Step 3: Remove only the generated links transactionally**

Preserve `pmbot_cache.duckdb` byte-for-byte. Run the Task 1 transaction
protocol separately for `polymarket-bot-egg-info`,
`polymarket-bot-2-egg-info`, and `polymarket-traders-build`. After each
mutation, rerun its exact supported guarded entrypoints before resume and
require no link recreation. For Traders, run `npm install`, `npm run
templatify:matic`, `npm run codegen`, `npm run deploy`, and `npm start` from the
Drive root and require each to stop in its matching pre-hook before invoking
Graph or Docker. Close and finalize one label before preparing the next.

- [ ] **Step 4: Commit each archive independently**

Use one commit per repository; do not combine their histories.

### Task 5: Repair Gmail sync, launchd, and pass-utils

**Files:**

- Remove: `gmail-maildir-sync/gmail_maildir_sync/__pycache__` symlink
- Create: `launchd/pytest.ini`
- Remove: `launchd/.pytest_cache` symlink
- Create: `pass-utils/scripts/build-and-install-external`
- Modify: `pass-utils/README.org`
- Remove: `pass-utils/pass_utils.egg-info` symlink

- [ ] **Step 1: Verify the bytecode and pytest policies**

Add `PYTHONDONTWRITEBYTECODE=1` explicitly inside the dotfiles
`bin/gmail-maildir-sync` wrapper so noninteractive execution cannot depend on
the login shell. Require the variable in the resulting child process. Add this exact
`launchd/pytest.ini`:

```ini
[pytest]
addopts = -p no:cacheprovider
```

Run the launchd suite and require no `.pytest_cache` recreation.

- [ ] **Step 2: Add and test the Pass Utils external installer**

The wrapper must create a temporary staged source outside Drive, build a wheel
there, create or reuse the named environment
`~/.local/share/python-envs/pass-utils`, install the wheel non-editably with
that environment's `python -m pip`, and run:

```bash
~/.local/share/python-envs/pass-utils/bin/python -c \
  'import pass_utils, pass_utils.core; print(pass_utils.__file__)'
```

Require both module paths below that environment's `site-packages`, never
Dropbox or My Drive. There is no console entry point; do not invent a
`--help` smoke.

- [ ] **Step 3: Remove the three links through separate transactions**

Run the Task 1 protocol separately for `gmail-maildir-sync-pycache`,
`launchd-pytest-cache`, and `pass-utils-egg-info`. Between each mutation and
resume: for Gmail sync, run its focused tests and one wrapper dry-run without
network mail mutation; for launchd, run its full test suite; for Pass Utils,
run the exact import command above from the external installation. Require the
label path to remain absent, then resume, converge, close, and finalize before
preparing the next label.

- [ ] **Step 4: Commit each repository separately**

Preserve the untracked `gmail-maildir-sync/uv.lock`; do not stage it unless it
was already part of the repository's intended change.

### Task 6: Repair Rubric Visualizer caches

**Files:**

- Modify: `AGENTS.md`, `README.md`
- Remove through Trash: `.pytest_cache`, `__pycache__` symlinks
- Preserve for later plan: `CLAUDE.md` symlink

- [ ] **Step 1: Make the supported Python commands bytecode-free**

Update the documented and agent-supported commands to:

```bash
python3 -B tests/test_coverage_map.py
python3 -B coverage_map.py --example
```

Run both and prove neither cache path changes. The global runtime guard must
continue to reject an unmanaged pytest invocation that could recreate
`.pytest_cache`.

- [ ] **Step 2: Remove both cache links transactionally**

Run the Task 1 protocol separately for `rubric-pytest-cache` and
`rubric-pycache`. Between each mutation and resume, exercise both commands from
Step 1 and require both repaired paths that have been processed so far absent,
the instruction symlink untouched, and no cache recreation. Close and finalize
the first label before preparing the second; native errors may decrease only
by the corresponding cache records.

- [ ] **Step 3: Commit the pytest policy**

Do not edit `CLAUDE.md` in this plan.

### Task 7: Give Wikipedia Deletion Analysis a named external uv environment

**Files:**

- Create: `scripts/uv-external`
- Modify: `README.md`
- Modify: `launchd/com.benthamite.wikipedia-deletion-analysis.plist`
- Modify: `~/My Drive/repos/launchd/agents/com.benthamite.wikipedia-deletion-analysis.plist`
- Remove through Trash: `.venv` symlink

- [ ] **Step 1: Add the wrapper and tests**

Create mode-`100755` `scripts/uv-external`:

```bash
#!/bin/sh
set -eu
export UV_PROJECT_ENVIRONMENT="${XDG_DATA_HOME:-$HOME/.local/share}/uv/environments/wikipedia-deletion-analysis"
export PYTHONDONTWRITEBYTECODE=1
exec "${UV_BIN:-uv}" "$@"
```

Test with a fake `uv` that records the exact environment and arguments.

- [ ] **Step 2: Update both launchd plist copies**

Replace direct `uv run` calls with the absolute repository wrapper path while
keeping the Drive source cwd. Keep the two plist command bodies equivalent and
preserve their intentionally different log destinations only if reinspection
confirms that difference is still required.

- [ ] **Step 3: Remove `.venv` and recreate externally**

Run the Task 1 protocol for `wikipedia-venv`. Between `mutate` and resume, run
`scripts/uv-external sync`; require the named external environment to exist and
`test ! -e .venv && test ! -L .venv`. Continue through converge, the required
restart, close, and finalize only after the scheduled checks below pass.

- [ ] **Step 4: Verify manual and scheduled execution**

Run a controlled collector invocation through the wrapper, reload only the
specific launchd job without signaling Emacs, and observe the next actual
hourly trigger. Before either reload or controlled invocation, obtain the
user's explicit confirmation required by the launchd repository; plan approval
is not sufficient. Require successful logs and no old path recreation, and
leave the plan pending until authorization and the actual trigger complete.

- [ ] **Step 5: Commit repository and launchd changes separately**

Do not combine the two repositories in one commit.

### Task 8: Close the in-place phase

**Files:**

- Inspect: all ten repositories and 12 repaired paths

- [ ] **Step 1: Exercise every recreation path one final time**

Run the exact install/build/test/scheduled commands above. Require zero new
symlink and the intended regular-or-absent representation at every path.

- [ ] **Step 2: Verify Git and cloud preimages**

Require all preserved dirty/untracked state unchanged, no duplicate cloud
object, and a validated `rollback_window_closed` plus
`private_preimage_finalized` event in each of the 12 hash-chained journals.
Require every disposable external target moved to Trash only by `finalize-path`
after that close event; retained named runtime/data sources must be byte-identical
and explicitly recorded as retained.

- [ ] **Step 3: Record 12 resolved directory-link entries**

The phase gate is the actual native category transition plus a complete
filesystem audit, not the planned count alone.
