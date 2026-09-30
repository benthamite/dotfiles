# Drive Workspace Tooling — Follow-ups and Pilot Baseline

Durable record of the post-merge obligations, recommended fixes, pilot
handoff baseline, and deferred polish identified during the final
whole-branch review of `drive-workspace-tooling` (2026-08-02). The
branch's working progress ledger (`.superpowers/sdd/progress.md`) is
uncommitted and dies with the worktree; this document travels with the
merge.

## Post-merge REQUIRED actions

1. **Run the live profile-aware tangle.** `emacs/config.org` changed on
   this branch; regenerate the init files in the running Emacs:

   ```
   emacsclient -e '(init-build-profile (file-name-directory user-init-file))'
   ```

2. **Keep the merge-to-shadow-retirement window short.** The canonical
   `ai-config-sync` audit reports `Duplicate skill name: fix-drive-errors`
   until the pilot retires the four legacy shadow skills. This is a
   deliberate forcing function (test-pinned), not a defect — but every day
   the window stays open, the canonical audit stays red.

## Follow-up fixes recommended

- **ai-config-sync worktree classifier bug.** The classifier compares
  `same_path` where it should compare `same_git_repository`
  (`bin/ai-config-sync:~2516`), producing an unsatisfiable block for
  worktree commits that touch the global instruction pair. A reviewed fix
  exists in outline; the primitive `same_git_repository` is already
  defined at `bin/ai-config-sync:174`.
- **Canonical `:ID:`-drawer / generated-inventory interaction.** The
  org-roam ID sweep and the generated inventory file fight over `:ID:`
  drawers. Owner decision needed: either exempt generated files from the
  org-roam ID sweep or make the generator ID-preserving. It did not
  reproduce at Task 8 time, but it recurs whenever the sweep runs.

## Pilot plan handoff baseline

- **Audit exit status.** `drive-workspace audit` exits 1 on the ea.news
  submodule: `core.worktree` in
  `~/git-dirs/ea.news/modules/ea.news-api/config` (line 8) points at a
  defunct Dropbox path. Fix that config first.
- **ea.news-front** `core.worktree` hardcodes the Drive path; update it at
  migration time.
- **Unassessed workspaces.** The other six workspaces' git-state
  cleanliness is UNASSESSED — the audit fails fast on ea.news before
  reaching them. Expect serial discovery as each blocker is cleared.
- **Symlink inventory.** 43 in-Drive symlinks enumerated, matching the
  manifest path-for-path.
- **Shadow hash journals** live in
  `~/.local/state/drive-workspace-migration/shadow-skills/*.jsonl`. They
  contain hashes only — cloud identities were deferred to pilot re-capture
  via `record-path-cloud`. Note: the pilot plan names
  `skill-shadows/*.ndjson`; these are the same four objects under a
  different directory name — do not treat the discrepancy as drift.

## Pilot cautions from review

- **T6 finalize reference-scan scope** is the captured path's parent
  directory only; a same-target symlink elsewhere would be missed.
  Damage is bounded: targets are disposable-only and recoverable from the
  transaction Trash.
- **T6 basename-only cloud identity sweep** will hard-block on common
  cache names (e.g. `node_modules`); expect friction.
- **T5 baseline-journal kind is unrestricted** — observation journals can
  be passed as baselines. A one-line kind check is a good post-merge
  patch.
- **T5 JXA exit-77** on attribute-poor auxiliary windows may need pilot
  tuning.
- **pytest guard rule:** in-Drive pytest without `-B` (or
  `PYTHONDONTWRITEBYTECODE` in the command's own environment prefix)
  denies. Expected; run test suites from the external workspaces.

## Deferred polish

- T3: `finally` block can mask an in-flight `AuditError`.
- T4: success-stdout exactness is asserted on a subset;
  record-program-gate lacks a corruption-matrix test; one `os.rmdir` is
  unwrapped.
- T5: OAuth token file mode is unchecked.
- T6: trash mapping is journaled only on success.
- T7: quoted-program-text delegation residual (documented boundary).
- T8: substring invariant test, raw `PermissionError` wrap, mutation-test
  scope.
- pretooluse dispatcher: keyword-gate regex duplicated in three places —
  single-source it or cross-comment the copies.
