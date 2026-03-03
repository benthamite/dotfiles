---
name: nosync
description: Apply the .nosync trick to a directory so Google Drive stops syncing it. Renames the directory with a .nosync suffix and creates a symlink at the original path. Use when Google Drive complains about symlinks or other problematic files inside a directory (e.g. node_modules/.bin).
argument-hint: <directory-path>
---

# Google Drive .nosync fix

Rename a directory to have a `.nosync` suffix (which Google Drive skips) and create a symlink at the original path so tooling continues to work transparently.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

### Execution steps

#### Step 1: Validate the target

The user must provide a directory path as `$ARGUMENTS`. If no argument is provided, ask: "Which directory should I apply the .nosync fix to? (e.g. `./node_modules`)"

Resolve the path to an absolute path. Verify:
1. The path exists and is a directory (not already a symlink).
2. The path is inside a Google Drive-synced location (i.e. under `~/My Drive/` or `~/Library/CloudStorage/GoogleDrive-*/My Drive/`).
3. A `.nosync` version does not already exist at the same level.

If any check fails, explain the issue and stop.

#### Step 2: Apply the fix

1. Rename the directory by appending `.nosync`:
   ```
   mv <dir> <dir>.nosync
   ```
2. Create a relative symlink at the original path:
   ```
   ln -s <basename>.nosync <dir>
   ```
3. Verify the symlink resolves correctly by listing a few entries inside it.

#### Step 3: Update .gitignore (if applicable)

If the directory is inside a git repository:
1. Read the `.gitignore` file (at the repo root).
2. If the original directory name is already ignored (e.g. `node_modules/`), add a corresponding entry for the `.nosync` variant (e.g. `node_modules.nosync/`) right below it.
3. If neither is ignored, add both entries.

#### Step 4: Commit

Commit the `.gitignore` change with the message: `chore: apply .nosync trick to <dirname> for Google Drive compatibility`.

#### Step 5: Report

Tell the user what was done and remind them that if they ever run a fresh install (e.g. `npm install`) that recreates the directory as a real directory instead of following the symlink, they may need to re-run this skill.
