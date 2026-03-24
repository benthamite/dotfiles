---
name: fix-drive-errors
description: Scan Google Drive for directories causing sync errors and bulk-apply the .nosync trick, then restart Google Drive. Use when Google Drive shows sync errors or periodically to keep the error list clean.
---

# Fix Google Drive sync errors

Scan `~/My Drive/` for directories that cause sync errors (node_modules, build output, caches, large data dirs, etc.), apply the `.nosync` trick to new offenders, purge stale entries from the DriveFS mirror database, repair broken symlinks, and restart Google Drive.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

### Execution steps

#### Step 1: Diagnose current error state from the mirror database

This is the most important step. Applying the `.nosync` trick renames a directory and creates a symlink, but Drive's mirror database (`mirror_sqlite.db`) retains all previously tracked descendants. These stale entries are the primary cause of recurring errors — Drive keeps trying to sync files that no longer exist at their tracked paths.

1. **Find the DriveFS account ID(s)**:
   ```bash
   ls ~/Library/Application\ Support/Google/DriveFS/ | grep -E '^[0-9]+$'
   ```

2. **For each account**, count stale items inside `.nosync` directories using a recursive CTE:
   ```sql
   WITH RECURSIVE nosync_tree(local_stable_id, root_id) AS (
     SELECT local_stable_id, local_stable_id FROM mirror_item WHERE local_filename LIKE '%.nosync'
     UNION ALL
     SELECT m.local_stable_id, n.root_id
     FROM mirror_item m
     JOIN nosync_tree n ON m.parent_local_stable_id = n.local_stable_id
   )
   SELECT count(*) FROM nosync_tree;
   ```

3. **Break down by `.nosync` root** to show what's contributing most:
   ```sql
   WITH RECURSIVE nosync_tree(local_stable_id, root_id) AS (
     SELECT local_stable_id, local_stable_id FROM mirror_item WHERE local_filename LIKE '%.nosync'
     UNION ALL
     SELECT m.local_stable_id, n.root_id
     FROM mirror_item m
     JOIN nosync_tree n ON m.parent_local_stable_id = n.local_stable_id
   )
   SELECT r.local_filename, p.local_filename as parent, count(*) as descendant_count
   FROM nosync_tree t
   JOIN mirror_item r ON r.local_stable_id = t.root_id
   JOIN mirror_item p ON p.local_stable_id = r.parent_local_stable_id
   GROUP BY t.root_id
   ORDER BY descendant_count DESC;
   ```

4. **Check queued_uploads** for stuck items inside `.nosync` directories:
   ```sql
   SELECT m1.local_filename, m2.local_filename as parent, m3.local_filename as grandparent, q.size
   FROM queued_uploads q
   JOIN mirror_item m1 ON m1.local_stable_id = q.local_stable_id
   LEFT JOIN mirror_item m2 ON m2.local_stable_id = m1.parent_local_stable_id
   LEFT JOIN mirror_item m3 ON m3.local_stable_id = m2.parent_local_stable_id
   ORDER BY q.size DESC;
   ```

Report the total stale item count and the top contributors. This count is the primary error metric.

#### Step 2: Scan for new problematic directories

Search `~/My Drive/` for directories that commonly cause Google Drive sync errors. Run all of these searches in parallel:

1. **node_modules** (npm/yarn dependencies):
   ```
   find ~/My\ Drive -maxdepth 6 -type d -name "node_modules" 2>/dev/null
   ```
   Exclude any that are already symlinks (i.e., already `.nosync`'d). Also exclude nested `node_modules` inside an already-`.nosync`'d parent (e.g. `node_modules.nosync/foo/node_modules` is already covered).

2. **Build output directories**:
   ```
   find ~/My\ Drive -maxdepth 5 -type d \( -name ".next" -o -name "out" -o -name "dist" -o -name "build" -o -name ".nuxt" -o -name ".svelte-kit" -o -name ".parcel-cache" -o -name ".turbo" -o -name ".cache" \) 2>/dev/null
   ```
   Filter these: only include directories that are **generated** (i.e., listed in a `.gitignore`, or clearly build artifacts). Directories named `build` or `dist` that contain source code (not generated) should be skipped. Use your judgement and check the parent project's `.gitignore` when uncertain. Exclude dirs inside an already-`.nosync`'d ancestor.

3. **Python caches and virtual environments**:
   ```
   find ~/My\ Drive -maxdepth 6 -type d \( -name "__pycache__" -o -name ".pytest_cache" -o -name "venv" -o -name ".venv" -o -name ".mypy_cache" -o -name ".ruff_cache" \) 2>/dev/null
   ```

For all searches: exclude directories that are already symlinks (`[ -L "$d" ]`) and directories inside a `.nosync` ancestor.

#### Step 3: Repair broken symlinks from previous runs

Check every `.nosync` directory for symlink integrity:

```bash
find ~/My\ Drive -maxdepth 5 -name "*.nosync" -type d 2>/dev/null | while read nosync; do
  original="${nosync%.nosync}"
  if [ -d "$original" ] && [ ! -L "$original" ]; then
    echo "REPLACED: $original (real dir recreated alongside .nosync)"
  elif [ ! -e "$original" ]; then
    echo "MISSING: $original (symlink gone)"
  fi
done
```

- **REPLACED**: a package manager or build tool recreated the directory as a real dir. Remove the real dir (`trash` it), then recreate the symlink.
- **MISSING**: the symlink was deleted. Recreate it: `ln -s <basename>.nosync <original>`.

#### Step 4: Apply `.nosync` trick to new directories

If Step 2 found new directories, apply the trick to each:

1. `mv <dir> <dir>.nosync`
2. `ln -s <basename>.nosync <dir>`
3. Verify the symlink resolves correctly.

Process directories from largest to smallest to maximize impact quickly.

#### Step 5: Update .gitignore files

For each affected git repository (from Steps 3–4), update its `.gitignore`:

1. Read the repo's `.gitignore`.
2. For each `.nosync`'d directory, ensure both the original name and the `.nosync` variant are ignored. For example, if `node_modules/` is already listed, add `node_modules.nosync/` right below it.
3. Commit the `.gitignore` change: `chore: apply .nosync trick to <dirnames> for Google Drive compatibility`.

Batch multiple `.nosync` entries per repo into a single commit.

#### Step 6: Purge stale mirror database entries

This is what actually clears the error count. The `.nosync` trick prevents new files from being tracked, but Drive's mirror database retains all previously tracked descendants. These must be deleted.

1. **Stop Google Drive** (required — the DB is locked while Drive runs):
   ```bash
   killall "Google Drive"
   while pgrep -x "Google Drive" > /dev/null; do sleep 1; done
   ```

2. **Back up the mirror database**:
   ```bash
   cp ~/Library/Application\ Support/Google/DriveFS/<account-id>/mirror_sqlite.db \
      ~/Library/Application\ Support/Google/DriveFS/<account-id>/mirror_sqlite.db.bak
   ```

3. **Delete stale entries** — all descendants of `.nosync` directories, plus the `.nosync` directory entries themselves:
   ```sql
   -- Collect all local_stable_ids to purge
   WITH RECURSIVE nosync_tree(local_stable_id) AS (
     SELECT local_stable_id FROM mirror_item WHERE local_filename LIKE '%.nosync'
     UNION ALL
     SELECT m.local_stable_id
     FROM mirror_item m
     JOIN nosync_tree n ON m.parent_local_stable_id = n.local_stable_id
   )
   DELETE FROM mirror_item WHERE local_stable_id IN (SELECT local_stable_id FROM nosync_tree);
   ```

4. **Clear queued_uploads** for purged items:
   ```sql
   DELETE FROM queued_uploads WHERE local_stable_id NOT IN (SELECT local_stable_id FROM mirror_item);
   ```

5. **Verify** the purge worked:
   ```sql
   SELECT count(*) FROM mirror_item WHERE local_filename LIKE '%.nosync';
   -- Should be 0
   ```

#### Step 7: Restart Google Drive

Relaunch after the DB purge:

```bash
open -a "Google Drive"
sleep 5 && pgrep -x "Google Drive" > /dev/null && echo "Google Drive restarted successfully"
```

#### Step 8: Report

Summarize:
- **Stale entries purged** from the mirror database (the main fix).
- **New directories** `.nosync`'d and total size removed from sync (if any).
- **Broken symlinks** repaired (if any).
- **`.gitignore` files** updated and committed (if any).
- Confirmation that Google Drive was restarted.

Remind the user that:
- Package installs (`npm install`, `pip install`) or builds (`next build`, `hugo`) may recreate directories as real directories instead of following the symlink. If that happens, re-run `/fix-drive-errors`.
- The `/nosync` skill can be used to fix individual directories on the spot.
