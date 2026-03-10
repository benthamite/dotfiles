---
name: fix-drive-errors
description: Scan Google Drive for directories causing sync errors and bulk-apply the .nosync trick, then restart Google Drive. Use when Google Drive shows sync errors or periodically to keep the error list clean.
---

# Fix Google Drive sync errors

Scan `~/My Drive/` for directories that cause sync errors (node_modules, build output, caches, large data dirs, etc.), apply the `.nosync` trick to all of them, and restart Google Drive so it refreshes its error list.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

### Execution steps

#### Step 1: Scan for problematic directories

Search `~/My Drive/` for directories that commonly cause Google Drive sync errors. Run all of these searches in parallel:

1. **node_modules** (npm/yarn dependencies):
   ```
   find ~/My\ Drive -maxdepth 6 -type d -name "node_modules" 2>/dev/null
   ```
   Exclude any that are already symlinks (i.e., already `.nosync`'d).

2. **Build output directories**:
   ```
   find ~/My\ Drive -maxdepth 5 -type d \( -name ".next" -o -name "out" -o -name "dist" -o -name "build" -o -name ".nuxt" -o -name ".svelte-kit" -o -name ".parcel-cache" -o -name ".turbo" -o -name ".cache" \) 2>/dev/null
   ```
   Filter these: only include directories that are **generated** (i.e., listed in a `.gitignore`, or clearly build artifacts). Directories named `build` or `dist` that contain source code (not generated) should be skipped. Use your judgement and check the parent project's `.gitignore` when uncertain.

3. **Python caches and virtual environments**:
   ```
   find ~/My\ Drive -maxdepth 6 -type d \( -name "__pycache__" -o -name ".pytest_cache" -o -name "venv" -o -name ".venv" -o -name ".mypy_cache" -o -name ".ruff_cache" \) 2>/dev/null
   ```

4. **Large data directories**: check the Google Drive logs for actively failing uploads/downloads:
   ```
   grep "Content upload of\|Content download of" ~/Library/Application\ Support/Google/DriveFS/Logs/drive_fs*.txt 2>/dev/null | \
     sed -n 's/.*filename="\([^"]*\)".*/\1/p' | sort | uniq -c | sort -rn | head -30
   ```
   If specific filenames appear hundreds or thousands of times, trace them back to their parent directories using the DriveFS mirror database:
   ```
   sqlite3 ~/Library/Application\ Support/Google/DriveFS/<account-id>/mirror_sqlite.db
   ```
   Use recursive CTEs to reconstruct full paths from `local_stable_id` → `parent_local_stable_id` chains in the `mirror_item` table.

For all searches: exclude directories that are already symlinks (`[ -L "$d" ]`) — those are already `.nosync`'d.

#### Step 2: Present findings and apply

Show the user a summary table of all directories found, grouped by type (node_modules, build output, caches, data), with sizes.

Then apply the `.nosync` trick to **each** directory:

1. `mv <dir> <dir>.nosync`
2. `ln -s <basename>.nosync <dir>`
3. Verify the symlink resolves correctly.

Process directories from largest to smallest to maximize impact quickly.

#### Step 3: Update .gitignore files

For each affected git repository, update its `.gitignore`:

1. Read the repo's `.gitignore`.
2. For each `.nosync`'d directory, ensure both the original name and the `.nosync` variant are ignored. For example, if `node_modules/` is already listed, add `node_modules.nosync/` right below it.
3. Commit the `.gitignore` change: `chore: apply .nosync trick to <dirnames> for Google Drive compatibility`.

Batch multiple `.nosync` entries per repo into a single commit.

#### Step 4: Restart Google Drive

Quit Google Drive, wait for it to fully exit, then relaunch:

```bash
killall "Google Drive"
```

Then poll until the process is gone (do NOT just sleep a fixed amount):

```bash
while pgrep -x "Google Drive" > /dev/null; do sleep 1; done
```

Once it has fully exited, relaunch:

```bash
open -a "Google Drive"
```

Wait a few seconds, then confirm the process is running:

```bash
sleep 5 && pgrep -x "Google Drive" > /dev/null && echo "Google Drive restarted successfully"
```

#### Step 5: Report

Summarize:
- How many directories were `.nosync`'d and total size removed from sync.
- Which `.gitignore` files were updated and committed.
- Confirmation that Google Drive was restarted.

Remind the user that:
- Package installs (`npm install`, `pip install`) or builds (`next build`, `hugo`) may recreate directories as real directories instead of following the symlink. If that happens, re-run `/fix-drive-errors`.
- The `/nosync` skill can be used to fix individual directories on the spot.
