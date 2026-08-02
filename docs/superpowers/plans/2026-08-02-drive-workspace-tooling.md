# Drive Workspace Migration Tooling Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the fail-closed manifest, audit, journal, move, rollback, cloud-verification, and regression guards required before any repository leaves Google Drive.

**Architecture:** One standard-library Python command owns deterministic workspace state and writes append-only journals outside Drive. Paired Claude/Codex guards reject state-creating commands in Drive-hosted repositories, while the rewritten `fix-drive-errors` skill audits and routes repairs without ever leaving an in-Drive symlink.

**Tech Stack:** Python 3.11 standard library, Bash, Git, Google Drive v3,
macOS System Events Accessibility through `/usr/bin/osascript -l JavaScript`,
`/usr/sbin/screencapture`, `unittest`, Claude/Codex hooks, JSON Lines.

---

This is program plan 1 of 12. It changes tooling only; it must not move a live
repository, pause or restart Drive, mutate a cloud object, or remove an existing
symlink. Read `claude/context/google-services.md` and
`claude/context/secrets.md` before implementing the OAuth-backed cloud reader.

## File map

- Create: `drive-workspaces.json` — canonical roots, six personal workspace transactions, the inherited worktree, and per-workspace smoke commands.
- Create: `bin/drive-workspace` — audit, capture, pause verification, atomic move, local rollback, verification, and cloud-record commands.
- Create: `bin/drive-workspace-native.jxa` — the only native Drive UI provider; enumerate the menu/error panel through Accessibility and return JSON.
- Create: `lib/drive_runtime_command.py` — conservative POSIX command segmentation and effective-cwd/target resolution shared by both runtime guards.
- Create: `tests/test_drive_workspace.py` — disposable repositories, remotes, worktrees, journals, and mocked Drive API coverage.
- Create: `claude/hooks/block-drive-runtime-command.sh` — Claude fail-closed runtime/worktree guard.
- Create: `codex/hooks/block-drive-runtime-command.sh` — byte-equivalent Codex guard.
- Modify: `claude/hooks/pretooluse-bash.sh` — dispatch guarded commands.
- Modify: `codex/hooks.json` — register the Codex guard.
- Create: `tests/test_drive_runtime_guard.py` — parity and command-boundary tests.
- Create: `claude/skills/fix-drive-errors/SKILL.md` — tracked audit-and-route workflow.
- Create: `codex/skills/fix-drive-errors/SKILL.md` — paired tracked workflow.
- Modify: `claude/README.org`, `codex/README.org`, `codex/skills/README.org`, `agents/README.org`, `bin/README.org`, `claude/hooks/README.org`, `codex/hooks/README.org` — ownership and command contracts.
- Modify: `claude/CLAUDE.md`, `codex/AGENTS.md` — split source-only Drive roots from active external workspaces and declare `~/repos/.worktrees`.
- Modify: `tests/test_ai_config_sync_audit.py` — paired skill/guard coverage.
- Modify: `ai-config-sync.json` — register paired skill and guard ownership.
- Create: `claude/skills/move-session-log/scripts/move_session_log.py` and tests — give Claude the same supported relocation adapter already present in Codex.
- Modify: `shell/.zshrc` — retire `_gdrive_relocate_node_modules`, guard `mkvenv`, and make `newtask` use the external worktree root.
- Modify: `emacs/config.org` — make Trajectory worktree creation use the external root, then tangle through the profile-aware command.
- Modify: `bin/cr-review-worktree`, `claude/bin/cr-worktree-gc.sh`, `bin/sync-reasoning-tasks-worktrees`, `claude/hooks/sync-reasoning-tasks-worktree.sh` — remove Drive-local worktree defaults.
- Inspect only in this plan: paired shadows under `~/My Drive/.claude/skills/{fix-drive-errors,nosync}` and `~/My Drive/.codex/skills/{fix-drive-errors,nosync}` — retirement belongs to the mutation pilot.

### Task 1: Define and validate the migration manifest

**Files:**

- Create: `drive-workspaces.json`
- Create: `tests/test_drive_workspace.py`
- Create: `bin/drive-workspace`

- [ ] **Step 1: Write the failing manifest tests**

Create `tests/test_drive_workspace.py` with imports, a source loader matching
the repository's existing script tests, and these assertions:

```python
class ManifestTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module = load_script("drive_workspace", ROOT / "bin/drive-workspace")
        cls.manifest = cls.module.load_manifest(ROOT / "drive-workspaces.json")

    def test_roots_are_exact_and_external(self):
        self.assertEqual("~/My Drive", self.manifest.drive_root)
        self.assertEqual("~/repos", self.manifest.repos_root)
        self.assertEqual("~/repos/epoch", self.manifest.epoch_repos_root)
        self.assertEqual("~/repos/.worktrees", self.manifest.worktrees_root)

    def test_personal_scope_is_six_workspace_transactions(self):
        personal = [w for w in self.manifest.workspaces if w.group == "personal"]
        self.assertEqual(
            {
                "80000hours.global", "agent-skills", "consensus-trader",
                "ea.news", "stafforini.com", "tangodb",
            },
            {w.name for w in personal},
        )
        self.assertEqual(43, sum(w.expected_symlinks for w in personal))

    def test_ea_news_is_one_aggregate_transaction(self):
        item = self.manifest.by_name("ea.news")
        self.assertEqual("~/My Drive/repos/ea.news", item.source)
        self.assertEqual("~/repos/ea.news", item.destination)
        self.assertEqual((".", "ea.news-api", "ea.news-front"), item.git_roots)

    def test_manifest_loading_is_structural_not_live_git_validation(self):
        item = self.manifest.by_name("ea.news")
        self.assertEqual("workspace", item.kind)
        self.assertIn("ea.news-api", item.git_roots)

    def test_smokes_have_only_cwd_argv_and_env(self):
        for workspace in self.manifest.workspaces:
            for smoke in workspace.smoke:
                self.assertEqual({"cwd", "argv", "env"}, set(smoke.raw))
                self.assertIsInstance(smoke.argv, tuple)

    def test_agent_skills_has_exact_trust_consumer(self):
        item = self.manifest.by_name("agent-skills")
        self.assertIn(
            "codex-agent-skills-trust",
            {consumer.name for consumer in item.consumers},
        )

    def test_wuzapi_is_never_a_manifest_target(self):
        serialized = json.dumps(self.manifest.raw)
        self.assertNotIn("wuzapi", serialized)
```

- [ ] **Step 2: Run the tests and require the missing-script failure**

Run: `python3 -m unittest tests.test_drive_workspace.ManifestTests -v`

Expected: import failure because `bin/drive-workspace` does not exist.

- [ ] **Step 3: Add the manifest and strict data model**

Create `drive-workspaces.json` with `version: 1`, the four exact roots above,
`state_root: ~/.local/state/drive-workspace-migration`, and these six exact
personal entries:

| Name | Source | Destination | Git roots | Links | Smoke commands |
| --- | --- | --- | --- | ---: | --- |
| `80000hours.global` | `~/My Drive/repos/80000hours.global` | `~/repos/80000hours.global` | `.` | 6 | cwd `.`: `uv sync --extra dev`, `uv run pytest -q`, `uv run eighty-k-global report`; cwd `frontend`: `npm ci`, `npm run typecheck`, `npm run build`; empty env |
| `agent-skills` | `~/My Drive/repos/agent-skills` | `~/repos/agent-skills` | `.` | 1 | cwd `.`: `claude plugin validate .`, `bash hooks/session-start-test.sh`, `bash hooks/simplify-ignore-test.sh`; empty env |
| `consensus-trader` | `~/My Drive/repos/consensus-trader` | `~/repos/consensus-trader` | `.` | 8 | cwd `.`: `uv run python -m pytest -q`; env `UV_CACHE_DIR={transaction_tmp}/uv-cache` |
| `ea.news` | `~/My Drive/repos/ea.news` | `~/repos/ea.news` | `.`, `ea.news-api`, `ea.news-front` | 2 | cwd each Git root: `git status --short --branch`; cwd `ea.news-api`: `python3 -B -m unittest libs.test_summary_builder -v`; cwd `ea.news-front`: `npm ci`, `npm run lint`, `npm run build`; empty env |
| `stafforini.com` | `~/My Drive/repos/stafforini.com` | `~/repos/stafforini.com` | `.` | 7 | cwd `.`: `npm ci`, `npm test`, `scripts/render-verify-external`, plus `/usr/bin/test -d` for `static/pdfs`, `static/pdf-thumbnails`, `public`; render env `RENDER_ROOT={transaction_tmp}/stafforini-render` |
| `tangodb` | `~/My Drive/repos/tangodb` | `~/repos/tangodb` | `.` | 19 | cwd `backend`: `pyenv exec python -m pytest -q -p no:cacheprovider`, env `PYTHONDONTWRITEBYTECODE=1`; cwd `admin` and `web`: `npm ci`, `npm run check`, `npm test`, `npm run build`; empty env |

The table uses semicolons only to separate records. Encode every smoke item as
exactly `{"cwd": RELATIVE_PATH, "argv": [ARG, ...], "env": {NAME: VALUE}}`;
never encode `cd`, an environment assignment, `&&`, or another shell string.
Expand each table count into exact `links` records containing source-relative
path, captured `lstat` type/mode, link text, target policy, and one of
`preserve_link` or `materialize_generated_directory`. Require exactly 43 link
records across the six personal workspaces. Persistent data, browser state,
tracked source links, and named external runtimes use `preserve_link`;
`node_modules`, `.next`, and another generated root that its declared smoke
command removes use `materialize_generated_directory`. Reject a count-only,
unclassified, basename-only, or overlapping record.
The manifest must equal this exact 43-record disposition list; `link` means
expected post-smoke type `symlink`, and `directory` means a journaled
link-to-real-directory transition:

```text
80000hours.global | frontend/node_modules | materialize_generated_directory | disposable | directory
80000hours.global | frontend/.next | materialize_generated_directory | disposable | directory
80000hours.global | .pytest_cache | preserve_link | retain | link
80000hours.global | .venv | preserve_link | retain | link
80000hours.global | src/eighty_k_global/__pycache__ | preserve_link | retain | link
80000hours.global | src/eighty_k_global/api/__pycache__ | preserve_link | retain | link
agent-skills | .opencode/skills | preserve_link | retain | link
consensus-trader | .pytest_cache | preserve_link | retain | link
consensus-trader | consensus_trader/__pycache__ | preserve_link | retain | link
consensus-trader | tests/__pycache__ | preserve_link | retain | link
consensus-trader | consensus_trader.egg-info | preserve_link | retain | link
consensus-trader | results | preserve_link | retain | link
consensus-trader | .venv | preserve_link | retain | link
consensus-trader | scripts/__pycache__ | preserve_link | retain | link
consensus-trader | data | preserve_link | retain | link
ea.news | ea.news-front/node_modules | materialize_generated_directory | disposable | directory
ea.news | ea.news-front/.next | materialize_generated_directory | disposable | directory
stafforini.com | .pytest_cache | preserve_link | retain | link
stafforini.com | node_modules | materialize_generated_directory | disposable | directory
stafforini.com | tests/__pycache__ | preserve_link | retain | link
stafforini.com | public | preserve_link | retain | link
stafforini.com | static/pdfs | preserve_link | retain | link
stafforini.com | static/pdf-thumbnails | preserve_link | retain | link
stafforini.com | scripts/__pycache__ | preserve_link | retain | link
tangodb | web/node_modules | materialize_generated_directory | disposable | directory
tangodb | web/.svelte-kit | materialize_generated_directory | disposable | directory
tangodb | admin/node_modules | materialize_generated_directory | disposable | directory
tangodb | admin/.svelte-kit | materialize_generated_directory | disposable | directory
tangodb | backend/app/models/__pycache__ | preserve_link | retain | link
tangodb | backend/app/__pycache__ | preserve_link | retain | link
tangodb | backend/app/schemas/__pycache__ | preserve_link | retain | link
tangodb | backend/app/api/__pycache__ | preserve_link | retain | link
tangodb | backend/.pytest_cache | preserve_link | retain | link
tangodb | backend/tests/__pycache__ | preserve_link | retain | link
tangodb | backend/scripts/reconciliation/__pycache__ | preserve_link | retain | link
tangodb | backend/scripts/reconciliation/source_loaders/__pycache__ | preserve_link | retain | link
tangodb | backend/scripts/integration/__pycache__ | preserve_link | retain | link
tangodb | backend/scripts/musicbrainz_dump/__pycache__ | preserve_link | retain | link
tangodb | backend/scripts/utils/__pycache__ | preserve_link | retain | link
tangodb | backend/scripts/__pycache__ | preserve_link | retain | link
tangodb | backend/scripts/archived/__pycache__ | preserve_link | retain | link
tangodb | backend/.browser-data-todotango | preserve_link | retain | link
tangodb | scripts/__pycache__ | preserve_link | retain | link
```

Tests compare workspace, relative path, disposition, target policy, and
expected post-smoke type as an exact ordered set. The schema also supports
post-smoke type `absent` for a later manifest whose guarded owner intentionally
does not recreate a materialized cache; it never infers this from a missing
path.
`{transaction_tmp}` is the one permitted environment placeholder. Before a
smoke run, create a mode-`0700` directory with
`tempfile.mkdtemp(prefix="drive-workspace-", dir=os.environ.get("TMPDIR", "/tmp"))`,
expand the token only inside environment values, and remove that exact
tool-created directory with `shutil.rmtree()` in `finally` after confirming it
is outside Drive. No manifest may name a shared `/tmp/uv-cache`.
Preserve named external data/runtime targets in `preserve`. Add exact
`consumers` records containing `name`, `kind`, `path`, literal `old` and `new`
values, expected occurrence count, and optional session adapter. At minimum,
`agent-skills` owns the exact `codex/config.toml` trust-table key and any
captured resumable session association. Add this separate worktree entry:

For `tangodb`, also add `runtime_services` records for: backend cwd `backend`,
argv `pyenv exec uvicorn app.main:app --reload --host 127.0.0.1 --port 8000`,
port `8000`, and HTTP JSON keys `people,labels,works,recordings,albums,tracks,year_range,top_genres,top_artists`;
web cwd `web`, argv `npm run dev -- --host 127.0.0.1 --port 5173`, port `5173`,
HTTP status `200` and content marker `<html`; admin cwd `admin`, argv
`npm run dev -- --host 127.0.0.1 --port 5174`, port `5174`, HTTP status `200`
and content marker `<html`.

```json
{
  "name": "dont-sleep-safe-local-install",
  "group": "worktree",
  "source": "~/My Drive/repos/dont-sleep/.worktrees/safe-local-install",
  "destination": "~/repos/.worktrees/dont-sleep/safe-local-install",
  "owner": "~/My Drive/repos/dont-sleep",
  "branch": "safe-local-install",
  "kind": "linked_worktree",
  "expected_symlinks": 0,
  "consumers": [],
  "smoke": [
    {
      "cwd": ".",
      "argv": ["./tests/test-dont-sleep.sh"],
      "env": {}
    }
  ]
}
```

Implement immutable `Manifest` and `Workspace` dataclasses. `load_manifest()`
must reject unknown keys, duplicate names or destinations, a source outside My
Drive, a destination inside My Drive, a non-absolute expanded path, and any
entry containing `wuzapi`. Path expansion happens only after validation; the
raw `~` form remains available for stable journals. Loading is structural and
must not call Git or require the source to exist. Runtime capture separately
inspects each Git root; for a stale `.git` pointer or `core.worktree`, it opens
the recorded Git directory directly, journals the invalid value, and identifies
the supported repair needed after the move.
Create `bin/drive-workspace` with mode `100755`; the manifest test must assert
`stat.S_IMODE(path.stat().st_mode) == 0o755` before invoking it directly.

- [ ] **Step 4: Run the manifest tests**

Run: `python3 -m unittest tests.test_drive_workspace.ManifestTests -v`

Expected: all manifest tests pass.

- [ ] **Step 5: Commit the manifest slice**

```bash
git add drive-workspaces.json bin/drive-workspace tests/test_drive_workspace.py
git commit -m "drive: define workspace migration manifest"
```

### Task 2: Implement read-only filesystem and Git auditing

**Files:**

- Modify: `bin/drive-workspace`
- Modify: `tests/test_drive_workspace.py`

- [ ] **Step 1: Add failing audit tests**

Use temporary directories and repositories to cover these exact results:

```python
def test_scan_symlinks_reports_every_link_without_following_it(self):
    target = self.root / "outside"; target.mkdir()
    link = self.drive / "repo" / "node_modules"
    link.parent.mkdir(parents=True); link.symlink_to(target)
    self.assertEqual([str(link)], self.module.scan_symlinks(self.drive))

def test_snapshot_records_dirty_untracked_ignored_modes_and_links(self):
    snapshot = self.module.snapshot_git(self.repo)
    self.assertIn("head", snapshot)
    self.assertIn("refs", snapshot)
    self.assertIn("status_porcelain_v2", snapshot)
    self.assertIn("untracked", snapshot)
    self.assertIn("ignored", snapshot)
    self.assertIn("symlinks", snapshot)
    self.assertIn("worktrees", snapshot)
    self.assertIn("submodules", snapshot)
```

Also test that an unreadable directory produces an audit error rather than an
empty success result, and that `audit` exits nonzero for any symlink under the
Drive root. Run an unchanged audit twice and require byte-identical JSON:
sorted paths/keys, no observation timestamp, PID, temporary ref namespace, or
other volatile field. Transaction journals may add their own timestamps around
this deterministic payload.

- [ ] **Step 2: Run the focused tests and require failure**

Run: `python3 -m unittest tests.test_drive_workspace.AuditTests -v`

Expected: failures for undefined `scan_symlinks()` and `snapshot_git()`.

- [ ] **Step 3: Implement the audit functions**

Use `os.scandir()` plus `entry.is_symlink()` so traversal never follows a
link. Implement Git reads with `subprocess.run(..., shell=False, check=True)`
and NUL-delimited output where Git supports it. The snapshot must include:

```python
{
    "head": git(repo, "rev-parse", "HEAD"),
    "branch": git(repo, "branch", "--show-current"),
    "refs": git(repo, "for-each-ref", "--format=%(refname)%00%(objectname)"),
    "status_porcelain_v2": git(repo, "status", "--porcelain=v2", "-z"),
    "untracked": git(repo, "ls-files", "--others", "--exclude-standard", "-z"),
    "ignored": git(repo, "ls-files", "--others", "--ignored", "--exclude-standard", "-z"),
    "worktrees": git(repo, "worktree", "list", "--porcelain"),
    "submodules": git(repo, "submodule", "status", "--recursive"),
}
```

Add filesystem device ID, file modes, link targets, and SHA-256 hashes of
regular files to the journal representation. Hashing errors are blockers.
Serialize standalone `audit` output canonically with sorted records and keys;
keep capture time and other volatile observation metadata only in the external
journal event wrapper so final verification can byte-compare two live audits.

- [ ] **Step 4: Run the audit tests**

Run: `python3 -m unittest tests.test_drive_workspace.AuditTests -v`

Expected: all audit tests pass.

- [ ] **Step 5: Commit the audit slice**

```bash
git add bin/drive-workspace tests/test_drive_workspace.py
git commit -m "drive: audit workspace state without mutation"
```

### Task 3: Enforce exact remote commit coverage

**Files:**

- Modify: `bin/drive-workspace`
- Modify: `tests/test_drive_workspace.py`

- [ ] **Step 1: Add disposable-remote coverage tests**

Build bare remotes with `git init --bare`, never network clones. Cover a
published branch, an unpublished local branch, a stash commit, a detached
linked-worktree head, a checked-out submodule head, an advertised non-head ref,
and an annotated tag. Snapshot every pre-existing local ref before the audit;
assert the unpublished commit OIDs appear in the blocker list and every
pre-existing ref is byte-identical afterward. Require the run-scoped audit
namespace to be absent after both success and failure.

- [ ] **Step 2: Run the tests and require failure**

Run: `python3 -m unittest tests.test_drive_workspace.RemoteCoverageTests -v`

Expected: failure because `remote_coverage()` is undefined.

- [ ] **Step 3: Implement the coverage gate**

For each Git root, run `git ls-remote --refs <remote>`, create a unique
`refs/drive-workspace-audit/<run-id>/` namespace, and fetch the advertised refs
only into that namespace with explicit force refspecs and `--no-tags`. Never
use `--prune`, never update `refs/remotes/*` or `refs/tags/*`, and never fetch
an unqualified object ID. Verify that each advertised ref/OID pair resolves to
the fetched audit ref. Collect commits reachable from the primary
HEAD, every `git worktree list --porcelain` HEAD, `refs/heads/*`, `refs/tags/*`,
`refs/stash`, and recursive checked-out submodule HEADs. For each required
commit, require `git merge-base --is-ancestor <required> <audit-tip>` for
at least one advertised tip. A failed fetch, missing object, or uncovered
commit is a blocker. In a `finally` path, delete only the exact run namespace
with `git update-ref --stdin`, verify all pre-existing refs match their snapshot,
and leave fetched unreachable objects for normal Git maintenance. The command
has no push path and no waiver option.

- [ ] **Step 4: Run the coverage tests**

Run: `python3 -m unittest tests.test_drive_workspace.RemoteCoverageTests -v`

Expected: all coverage tests pass.

- [ ] **Step 5: Commit the coverage slice**

```bash
git add bin/drive-workspace tests/test_drive_workspace.py
git commit -m "drive: require remote coverage before workspace moves"
```

### Task 4: Add journaled atomic move and local rollback

**Files:**

- Modify: `bin/drive-workspace`
- Modify: `tests/test_drive_workspace.py`

- [ ] **Step 1: Add failing transaction tests**

Test that `capture --journal PATH` creates a mode-`0700` transaction directory
and mode-`0600` JSON Lines journal under the external state root and prints the
canonical journal path as one JSON object on stdout;
`move` rejects a destination collision, different `st_dev`, changed source
snapshot, uncovered commit, and missing fresh pause evidence; successful move
uses `os.rename`; `rollback-local` restores the original path and exact
snapshot; a `kind=linked_worktree` entry uses `git worktree move` and can move
back; exact consumer patch staging preserves unrelated dirty hunks and rejects
overlap; and no subcommand accepts `--force`. Add corruption tests for a
truncated last line, duplicate or skipped sequence, wrong `previous_hash`,
wrong `event_hash`, two concurrent appenders, and a journal or parent with
broader permissions. Every mutating command must reject all six cases before
inspecting or changing the source path.
Test that `materialize-generated-paths` acts only on manifest links classified
`materialize_generated_directory`, moves only the link itself to recoverable
Trash after the intact post-rename check, retains the external target, and
journals its restore identity. Test that `rollback-generated-paths` moves only
a generated real replacement to Trash and restores the exact captured link.
Test that `run-smoke` uses each record's exact cwd/argv/env with `shell=False`,
records every status, stops on the first nonzero command, and always cleans its
task-owned external temporary directory in `finally` without hiding the
original failure. All three commands must reject concurrent path drift.
Test `finalize-generated-paths` as the only cleanup owner for old disposable
targets: it requires `rollback_window_closed`, exact journal hashes, and no
live symlink reference anywhere in the manifest; it moves only the journaled
target to Trash. It no-ops for retained targets and already-finalized entries
and refuses a changed, shared, or ambiguous target.
Test that `verify-rollback` refuses every local, consumer, generated-link,
cloud-ID/parent, duplicate, queue, or native-baseline mismatch and appends the
terminal event only after the complete after-resume state matches.

- [ ] **Step 2: Run the transaction tests and require failure**

Run: `python3 -m unittest tests.test_drive_workspace.TransactionTests -v`

Expected: failures for missing transaction commands.

- [ ] **Step 3: Implement the transaction commands**

Expose only these mutating forms:

```text
drive-workspace capture NAME --journal JOURNAL
drive-workspace init-journal --journal JOURNAL --kind observation|program --label LABEL
drive-workspace verify-journal --journal JOURNAL
drive-workspace record-drive-state NAME --journal JOURNAL
drive-workspace confirm-drive-paused NAME --journal JOURNAL
drive-workspace move NAME --journal JOURNAL
drive-workspace apply-consumers NAME --journal JOURNAL
drive-workspace verify-consumers NAME --journal JOURNAL
drive-workspace stage-consumers NAME --journal JOURNAL
drive-workspace verify-local NAME --journal JOURNAL
drive-workspace materialize-generated-paths NAME --journal JOURNAL
drive-workspace run-smoke NAME --journal JOURNAL
drive-workspace rollback-generated-paths NAME --journal JOURNAL
drive-workspace finalize-generated-paths NAME --journal JOURNAL
drive-workspace rollback-consumers NAME --journal JOURNAL
drive-workspace rollback-local NAME --journal JOURNAL
drive-workspace verify-rollback NAME --journal JOURNAL --baseline-journal BASELINE
drive-workspace request-rollback-drill NAME --journal JOURNAL
drive-workspace record-program-gate --journal PROGRAM_JOURNAL --gate native_record_convergence_passed|after_resume_rollback_passed --evidence-journal JOURNAL
```

`capture` and `init-journal` reject an existing journal and require `JOURNAL` to resolve below
`~/.local/state/drive-workspace-migration`, creates its parent as mode `0700`,
and writes a mode-`0600` JSON Lines journal. The first line is a `header` event
with schema version, transaction UUID, workspace name, expanded source and
destination, UTC time, `sequence: 0`, `previous_hash: null`, and `event_hash`.
Every later event has the same transaction UUID, a sequence exactly one higher,
the prior line's SHA-256 as `previous_hash`, and `event_hash` equal to SHA-256
of the canonical UTF-8 JSON object with `event_hash` omitted (`sort_keys=True`,
compact separators, one trailing newline). Append under an exclusive
`fcntl.flock`, open with `O_APPEND|O_WRONLY`, perform one complete `os.write`,
call `os.fsync`, fsync the parent after creation, and release the lock only
after rereading and validating the chain. A partial write, extra bytes, changed
permissions, unknown event, or chain mismatch permanently blocks mutation; the
tool never truncates or repairs a journal. `init-journal` uses the same header
with no workspace source/destination and permits only observation events for
`kind=observation` or validated gate events for `kind=program`. Success stdout is exactly
`{"journal":"/absolute/path"}` and all diagnostics go to stderr.

`capture` records all audit and exact consumer preimages. `record-drive-state`
captures the visible native Drive state plus read-only queue cursors without
assuming a log token. `confirm-drive-paused` requires a fresh post-capture
native UI state of paused and no outbound queue advancement during the bounded
observation defined by the characterized provider.

For `kind=workspace`, `move` uses `os.rename`; for
`kind=linked_worktree`, it runs `git worktree move` through an argument vector.
Both re-run preflight, require matching device IDs, repair worktree and
submodule metadata, and fsync the event. `apply-consumers` requires every
journaled literal and occurrence count to match, atomically applies only those
old/new substitutions, and invokes the named session adapter. Verification
requires no executable old value and exact new counts. Rollback restores
consumer preimages before the local path and refuses concurrent drift. On any
immediate verification error, the tool runs these tested rollback operations
before returning nonzero.
`stage-consumers` emits and checks an exact patch for only manifest-owned
hunks, applies it to the owning Git index with `git apply --cached`, and
refuses an overlapping dirty hunk or any additional staged path. It never
stages an entire dirty file.
Immediately after rename, `verify-local` requires every captured link intact at
its destination-relative path. `materialize-generated-paths` may then move only
the manifest-classified generated links to recoverable Trash; it never follows
a link or removes its external target. `run-smoke` executes the manifest's
structured records in order, gives each transaction its own mode-`0700`
temporary root outside Drive, expands `{transaction_tmp}` only in environment
values, records every result, and removes that exact tool-created root in
`finally`. A later `verify-local` accepts only the journaled transition from a
classified link to its exact declared post-smoke type (`directory` or
`absent`). `rollback-generated-paths` restores all such link preimages before
`rollback-local`. `finalize-generated-paths` runs only after
`rollback_window_closed`, revalidates each disposable target and every live
link reference, moves only the exact now-unreferenced target to Trash, and
appends `generated_target_finalized`; it never touches a retained target. No
count-only or unclassified filesystem change is accepted.
After Drive resumes from a rollback, `verify-rollback` requires the exact old
local snapshot, all consumer and generated-link preimages, the original cloud
object ID and parent live with no replacement/duplicate, stable running queues,
and the native rows equal to the supplied baseline. Only then does it append
`rollback_verified`; no other command or generic writer may append that event.
`request-rollback-drill` appends the sole accepted deliberate-failure event
after validating successful local, cloud, and native convergence.
`record-program-gate` accepts only the two enumerated gate names, verifies the
referenced transaction's complete hash chain and corresponding native or
rollback evidence itself, and then appends to a separately chained mode-`0600`
program journal. There is no generic `append-event` command.

- [ ] **Step 4: Run the transaction tests**

Run: `python3 -m unittest tests.test_drive_workspace.TransactionTests -v`

Expected: all transaction tests pass.

- [ ] **Step 5: Commit the transaction slice**

```bash
git add bin/drive-workspace tests/test_drive_workspace.py
git commit -m "drive: add journaled atomic workspace moves"
```

### Task 5: Add process, Emacs-buffer, consumer, and cloud gates

**Files:**

- Modify: `bin/drive-workspace`
- Create: `bin/drive-workspace-native.jxa`
- Modify: `tests/test_drive_workspace.py`

- [ ] **Step 1: Add failing gate tests**

Mock `lsof`, `emacsclient`, and `urllib.request.urlopen`. Require blockers for a
process whose cwd is below the source, an Emacs file-visiting buffer below the
source, a manifest consumer still containing the old path after local update,
an old cloud folder with the wrong parent, an untrashed old tree after resume,
and any duplicate live destination tree. Add a rollback test proving cloud
restore issues only a Drive `files.update` for the journaled object ID with
`trashed=false`, never creates an object, refuses a changed parent or a live
duplicate, and appends the returned ID and parent before local rollback.
Mock a paginated native Drive accessibility provider. Require
`record-native-errors` to enumerate every visible row with stable row key,
displayed path, displayed category/text, page position, UTC capture time, and
screenshot hash; reject an unreadable row, pagination loop, or duplicate key.
Log/database matches are optional annotations and can never create or clear a
native row. Add state-provider tests for paused, running, ambiguous UI, and a
queue cursor that advances while the UI claims to be paused.
Test `/usr/bin/osascript -l JavaScript bin/drive-workspace-native.jxa preflight`
fixtures for Drive absent, System Events Accessibility denied (`-25211`),
Screen Recording denied or a blank capture, an ambiguous Drive window, and
full permission. Every denied or ambiguous case exits nonzero before a journal
or Drive object changes.

- [ ] **Step 2: Run the tests and require failure**

Run: `python3 -m unittest tests.test_drive_workspace.LiveGateTests -v`

Expected: failures for missing live gates.

- [ ] **Step 3: Implement the gates**

Use `lsof -Fn -a -d cwd` without shell parsing. Query Emacs only with
`emacsclient -e` and never signal it. Read the personal gdoc OAuth file only at
runtime, refresh the token without printing it, and expose:

```text
drive-workspace record-cloud NAME --journal JOURNAL
drive-workspace verify-cloud NAME --journal JOURNAL
drive-workspace restore-cloud NAME --journal JOURNAL
drive-workspace native-preflight
drive-workspace record-native-errors --journal JOURNAL
drive-workspace close-rollback-window --journal JOURNAL --baseline-journal BASELINE --restart-before-pid PID
drive-workspace smoke-start NAME --service SERVICE --journal JOURNAL
drive-workspace smoke-check NAME --service SERVICE --journal JOURNAL
drive-workspace smoke-stop NAME --service SERVICE --journal JOURNAL
```

The cloud record stores the old object ID and parent chain. Verification
requires that exact ID to be trashed, no live object occupies the old path, no
destination or computer-backup duplicate exists, `is_my_drive=1`, the journaled
root ID matches, and `machine_root` count is zero. API ambiguity is a blocker.

`native-preflight` runs exactly
`/usr/bin/osascript -l JavaScript bin/drive-workspace-native.jxa preflight`.
The JXA script addresses the `Google Drive` process through System Events,
requires `UI elements enabled`, finds exactly one visible menu/status window,
reads the `AXRole`, `AXIdentifier`, `AXTitle`, `AXDescription`, `AXValue`,
`AXPosition`, and `AXSize` attributes used by the committed fixture, and exits
`77` on Accessibility denial or missing attributes. It obtains the window's
`AXWindowNumber`, captures only that window to a mode-`0600` tool-owned PNG
with `/usr/sbin/screencapture -x -l WINDOW_ID PATH`, rejects an empty or
single-color image as missing Screen Recording permission, hashes it, and
deletes the PNG in `finally`. `record-native-errors` invokes only this tested
provider with `list-errors --page-token TOKEN`, follows the returned next-page
token, and stores the schema above. It never derives a user-visible item from
logs or SQLite. Neither command opens System Settings or prompts for
permission; denial is a hard preflight blocker.

`close-rollback-window` validates the journal and requires all of: exactly one
successful `run-smoke` after materialization, a later successful
`verify-local`, successful `verify-consumers`, `verify-cloud`, no matching
native error compared with the baseline journal, stable running queue cursors,
no live process group recorded by `smoke-start`, and a current Drive process
ID different from `PID` captured immediately before the one required normal
restart. A workspace with no consumer records still needs the explicit
successful no-op `verify-consumers` event. It appends `rollback_window_closed`
only after re-running those reads.
Until that event exists, no tool may delete a private preimage or classify an
external target as finalizable. `restore-cloud` requires fresh
characterized pause evidence, the exact journaled
object currently in Trash at its original parent, no live object at the old
path, and the destination still present outside Drive. It clears only that
object's `trashed` field, refetches the same ID and parent, fsyncs the response
metadata to the journal, and exits. It has no upload, copy, create, delete, or
name-based selection path. The caller must then run `rollback-local` before
resuming Drive.

`smoke-start` first binds a loopback socket to the service port; any existing
listener is a blocker. It opens a mode-`0600` log below the external state root,
starts the structured argv with `subprocess.Popen(..., shell=False,
start_new_session=True)`, and journals PID, process-group ID, port, and log.
`smoke-check` requires that exact process group to own the listener and checks
the manifest's exact status, content marker, or JSON key set. `smoke-stop`
sends SIGTERM to only the journaled process group, waits ten seconds, uses
SIGKILL only on that same group if necessary, and then requires the port to be
bindable and `ps -Ao pgid=,pid=` to contain no member of the recorded group.
Tests use child-spawning fixture servers and prove both normal and forced
cleanup; a pre-existing listener can never satisfy a smoke.

- [ ] **Step 4: Run all tooling tests**

Run: `python3 -m unittest tests.test_drive_workspace -v`

Expected: all tests pass with no network access.

- [ ] **Step 5: Commit the live-gate slice**

```bash
git add bin/drive-workspace bin/drive-workspace-native.jxa tests/test_drive_workspace.py
git commit -m "drive: verify live consumers and cloud identity"
```

### Task 6: Add journaled in-place path repair and rollback

**Files:**

- Modify: `bin/drive-workspace`
- Modify: `tests/test_drive_workspace.py`

- [ ] **Step 1: Add failing path-repair tests**

Use temporary Drive roots to cover a link whose target is a disposable
directory, a link whose target contains user data, a regular-file preimage with
a sibling replacement, a regular-file replacement, and a regular-directory
preimage/replacement. Require `capture-path` to record the source with `lstat` without
following links, the link text or exact private regular-file/directory preimage, target
manifest and hashes, Git mode/status, cloud identity if one exists, intended
final type, and zero or more exact replacement paths.
Require blockers for an unknown current object, changed target, path outside
the journaled root, nonempty unjournaled replacement, unreadable content, or
missing fresh pause evidence.

Test that local rollback moves only the journaled replacement to Trash and
recreates the exact recorded link text or regular-file/directory bytes and modes; it must
never remove or rewrite an external target. Test that cloud verification
rejects a wrong parent, wrong type/hash, duplicate, computer-backup route, or
unexpected cloud creation. Test that cloud rollback clears `trashed` only on
the exact preimage object IDs and refuses a changed parent or live duplicate.
Cover a path with no cloud preimage and require `cloud_preimage: null`, not a
fabricated object ID.

- [ ] **Step 2: Run the focused tests and require failure**

Run:

```bash
python3 -m unittest tests.test_drive_workspace.PathRepairTests -v
```

Expected: failures for missing path-repair commands.

- [ ] **Step 3: Implement the fail-closed repair commands**

Expose:

```text
drive-workspace capture-path LABEL --journal JOURNAL --path PATH [--replacement PATH ...] --final-type absent|file|directory --target-policy disposable|retain
drive-workspace confirm-path-paused --journal JOURNAL
drive-workspace verify-path-local --journal JOURNAL [--manifest MANIFEST]
drive-workspace rollback-path-local --journal JOURNAL
drive-workspace record-path-cloud --journal JOURNAL
drive-workspace verify-path-cloud --journal JOURNAL [--manifest MANIFEST]
drive-workspace restore-path-cloud --journal JOURNAL
drive-workspace finalize-path --journal JOURNAL
```

`capture-path` creates a mode-`0700` external transaction directory containing
a mode-`0600` fsynced journal and private preimage assets; it records whether
the preimage is tracked. `confirm-path-paused` requires the freshly
characterized native paused state and stable bounded queue cursors.
`verify-path-local` requires the declared final type,
exact Git mode/status, and optional NUL-safe SHA-256 manifest. It never accepts
a symlink as a file or directory. `rollback-path-local` runs only while paused,
refuses a changed same-path or sibling replacement, moves that replacement to
Trash, and atomically restores the exact recorded link text or private regular
preimage bytes and modes while leaving any link target untouched. Recursive
preimages use NUL-safe relative paths and reject sockets, devices, FIFOs, hard
links, or any entry type not explicitly covered by the tests.

`record-path-cloud` snapshots the exact parent chain and any pre-existing
object IDs before mutation. `verify-path-cloud` requires the final absent or
regular representation at that exact parent, expected content hashes when the
API exposes them, no duplicate, and no computer-backup route. Ambiguous name
matches, Google-native files without a journaled ID, and unsupported hash
coverage are blockers rather than skipped checks.
`restore-path-cloud` requires Drive paused, operates only on the journaled
preimage IDs through `files.update(trashed=false)`, refetches their original
parents, and has no create, copy, upload, rename, or name-selection path.
An absent preimage is represented as `cloud_preimage: null`; it is valid only
when capture found no matching object, and verification then requires absence
rather than a trashed ID. `finalize-path` requires a validated
`rollback_window_closed` event. For `target_policy=disposable` it moves only
the exact journaled symlink target to Trash after rechecking its device,
manifest, hashes, and that no live symlink references it; for
`target_policy=retain` it leaves the target untouched. It then removes only
tool-created private preimage assets and appends their hashes and
`private_preimage_finalized`, retaining the journal permanently. There is no
time-based or implicit rollback-window expiry.

- [ ] **Step 4: Run all path and workspace transaction tests**

Run:

```bash
python3 -m unittest \
  tests.test_drive_workspace.PathRepairTests \
  tests.test_drive_workspace.TransactionTests \
  tests.test_drive_workspace.LiveGateTests -v
```

- [ ] **Step 5: Commit the repair transaction**

```bash
git add bin/drive-workspace tests/test_drive_workspace.py
git commit -m "drive: add journaled in-place repair rollback"
```

### Task 7: Block state-creating commands inside Drive

**Files:**

- Create: `claude/hooks/block-drive-runtime-command.sh`
- Create: `codex/hooks/block-drive-runtime-command.sh`
- Create: `lib/drive_runtime_command.py`
- Create: `tests/test_drive_runtime_guard.py`
- Modify: `claude/hooks/pretooluse-bash.sh`
- Modify: `codex/hooks.json`

- [ ] **Step 1: Add failing parity and behavior tests**

Test byte parity of the two guard files. Feed hook JSON with cwd under and
outside My Drive. Require denial whenever the effective project/cwd or explicit
target is under Drive for: `npm ci|install`, `npm run build|dev|test`,
`npm --prefix`, `uv sync`, project-bound `uv run`, `uv --project`,
`python -m venv`, every pip install form, Python without `-B` or
`PYTHONDONTWRITEBYTECODE=1`, pytest without both bytecode and cache prevention,
and `git worktree add` targeting Drive. Cover `git -C`, `cd ... &&`, subshells,
quoted paths, and environment prefixes. Require allowance only for proven
read-only commands and equivalent state-creating commands whose effective
project and targets are all external.
Unit-test `lib.drive_runtime_command.parse_command()` directly with every
chain, subshell, environment prefix, `cd`, `git -C`, `npm --prefix`, and
redirection form above. Unterminated quotes, here-documents, command/process
substitution, shell functions, and an unsupported token must produce
`indeterminate`, never a partially parsed command.

- [ ] **Step 2: Run the tests and require failure**

Run: `python3 -m unittest tests.test_drive_runtime_guard -v`

Expected: failure because the guards do not exist.

- [ ] **Step 3: Implement and register the paired guard**

The guard reads JSON from stdin, extracts cwd and command without `eval`, masks
quoted prose using the established destructive-command helper, and returns the
native deny response with this exact reason:

```text
Active dependency, build, cache, or worktree state is not allowed under ~/My Drive; migrate the repository or run the workflow from its approved external workspace.
```

Register it in both hook systems. Do not add an approval/escalation response.
Resolve the parser as
`$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)/lib/drive_runtime_command.py`
from either hook. Invoke `python3 -I "$PARSER" --cwd CWD --command COMMAND`
and consume its JSON array of execution segments. Implement the module with
`shlex.shlex(posix=True, punctuation_chars=";&|()")`; it tracks `cd` and
subshell cwd scopes, strips only syntactically valid environment prefixes, and
resolves `git -C`, `npm --prefix`, `uv --project`, worktree destinations, and
explicit path arguments. Reject redirections, here-documents, expansions, and
grammar the module does not model as `indeterminate`. This new module is the
single parser; do not depend on a pre-existing generic parser or duplicate
parsing in Bash. A command whose target cannot be determined safely is denied with
the same reason; there is no silent allow fallback.

Set mode `100755` on `bin/drive-workspace`,
`bin/drive-workspace-native.jxa`, and both guard files; assert those four modes
with `stat -f '%Lp'` and assert Git records mode `100755`. A mode mismatch is a
failing test, not a manual post-install fix.

- [ ] **Step 4: Run focused and parity tests**

Run:

```bash
python3 -m unittest tests.test_drive_runtime_guard -v
python3 -m unittest tests.test_claude_bash_pretooluse -v
```

Expected: all tests pass.

- [ ] **Step 5: Commit the guard slice**

```bash
git add claude/hooks/block-drive-runtime-command.sh codex/hooks/block-drive-runtime-command.sh lib/drive_runtime_command.py claude/hooks/pretooluse-bash.sh codex/hooks.json tests/test_drive_runtime_guard.py
git commit -m "agents: block Drive-hosted runtime state"
```

- [ ] **Step 6: Remove the existing symlink-producing helpers**

Delete `_gdrive_relocate_node_modules` and its hook from `shell/.zshrc`; it is
the root cause of repeated Drive symlink recreation. Add a Drive-root refusal
to `mkvenv`. Change `newtask`, both Trajectory worktree functions, and every
listed worktree helper to construct
`~/repos/.worktrees/<repository>/<branch>`. Add focused shell/ERT/Python tests,
tangle `emacs/config.org` with
`emacsclient -e '(init-build-profile (file-name-directory user-init-file))'`,
and require no helper to contain a Drive-local `.worktrees` destination.

- [ ] **Step 7: Add the missing Claude session relocation adapter**

Port the tested Codex script interface to the paired Claude skill so both
support:

```bash
python3 scripts/move_session_log.py --dry-run --rename OLD NEW
python3 scripts/move_session_log.py --rename OLD NEW
```

Run both adapters on temporary session trees and require byte-equivalent path
mapping semantics before the migration tool may call them.

- [ ] **Step 8: Commit helper and session-adapter changes**

Run all focused shell, ERT, Python, session-adapter, sync, and documentation
tests, then commit exactly `shell/.zshrc`, `emacs/config.org`, the four
worktree helpers, the Claude adapter/tests,
`ai-config-sync.json`, and any paired ownership docs with
`drive: route worktrees and sessions outside Drive`. Preserve unrelated files.

### Task 8: Replace the unsafe skills with a tracked audit workflow

**Files:**

- Create: `claude/skills/fix-drive-errors/SKILL.md`
- Create: `codex/skills/fix-drive-errors/SKILL.md`
- Modify: `tests/test_ai_config_sync_audit.py`
- Modify: `claude/README.org`, `codex/README.org`, `codex/skills/README.org`, `agents/README.org`

- [ ] **Step 1: Add failing skill-policy tests**

Require paired bodies, the invariant `No filesystem symlink may exist anywhere
under ~/My Drive`, invocation of `bin/drive-workspace audit`, prohibition of
disconnect/reconnect and internal database edits, and absence of instructions
that create a symlink or call `nosync`.

- [ ] **Step 2: Run the tests and require failure**

Run: `python3 -m unittest tests.test_ai_config_sync_audit -v`

Expected: missing tracked skill failures.

- [ ] **Step 3: Write the paired skill**

The complete workflow must: snapshot native categories; run the read-only
audit first; classify repository, residual, `.gdoc`, and file-representation
errors; route repository moves through the manifest plans; stop on unknown or
ambiguous content; and verify both the filesystem and native panel after a
restart. It must state that every in-Drive symlink is an error. Do not recreate
the `nosync` skill.

- [ ] **Step 4: Run sync and documentation audits**

Run:

```bash
bin/ai-config-sync audit
python3 -m unittest tests.test_ai_config_sync_audit -v
python3 bin/docs-audit audit --root .
```

Expected: all commands exit zero.

- [ ] **Step 5: Commit the tracked skill**

```bash
git add claude/skills/fix-drive-errors codex/skills/fix-drive-errors tests/test_ai_config_sync_audit.py ai-config-sync.json claude/README.org codex/README.org codex/skills/README.org agents/README.org
git commit -m "agents: replace Drive symlink externalization workflow"
```

- [ ] **Step 6: Prove global resolution and journal all four shadows**

Prove both Claude and Codex global resolution select the committed tracked
`fix-drive-errors` skill when the project-local shadows are excluded in a
disposable resolver test. Capture hashes and cloud identities for these four
inspected directories, but do not mutate them in this tooling-only plan:

```text
~/My Drive/.claude/skills/fix-drive-errors
~/My Drive/.claude/skills/nosync
~/My Drive/.codex/skills/fix-drive-errors
~/My Drive/.codex/skills/nosync
```

Store the four path-journal references for the pilot plan. Require
`bin/ai-config-sync audit` to pass with the shadows present and to have a tested
expected state after all four are retired together. The pilot performs the
recoverable Trash and cloud mutation.

### Task 9: Document roots and verify the tooling baseline

**Files:**

- Modify: `claude/CLAUDE.md`, `codex/AGENTS.md`
- Modify: `bin/README.org`, `claude/hooks/README.org`, `codex/hooks/README.org`

- [ ] **Step 1: Update the root contracts**

Replace the single `Project repos: ~/My Drive/repos/` statement with:

```text
- Active personal repositories: `~/repos/`; active Epoch repositories: `~/repos/epoch/`.
- Source-only personal repositories may remain under `~/My Drive/repos/`; do not create dependencies, builds, caches, virtual environments, or worktrees there.
- All linked worktrees: `~/repos/.worktrees/<repo>/<name>`.
```

Document `drive-workspace` and both guards in their command indexes.

- [ ] **Step 2: Run the full verification set**

Run:

```bash
python3 -m unittest tests.test_drive_workspace tests.test_drive_runtime_guard tests.test_ai_config_sync_audit tests.test_claude_bash_pretooluse -v
bin/ai-config-sync audit
python3 bin/docs-audit audit --root .
git diff --check
```

Expected: all commands exit zero.

- [ ] **Step 3: Commit the documentation slice**

```bash
git add claude/CLAUDE.md codex/AGENTS.md bin/README.org claude/hooks/README.org codex/hooks/README.org
git commit -m "drive: document external workspace boundaries"
```

- [ ] **Step 4: Prove this plan made no live migration**

Run `bin/drive-workspace audit --manifest drive-workspaces.json` and record the
current blockers. Require every configured source still exists, every
configured destination is absent, and no cloud or Drive mutation journal event
exists. This expected nonzero audit is the handoff to the pilot plan, not a
failure of the tooling implementation.
