---
name: nosync
description: Move a directory outside Google Drive's sync root so Drive stops syncing it, leaving a symlink in its place. Use when Google Drive complains about symlinks or other problematic files inside a directory (e.g. node_modules/.bin).
argument-hint: <directory-path>
model: sonnet
---

# Move directory outside Google Drive sync

Move a directory from inside `~/My Drive/` to `~/.drive-nosync/` (outside Drive's sync root) and leave a symlink in its place. Drive skips symlinks, so the directory becomes invisible to sync while tooling continues to work transparently.

**Important**: the `.nosync` suffix is an iCloud convention. Google Drive does **not** honor it. Do not use the `.nosync` rename trick — it does not work for Google Drive.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

### Execution steps

#### Step 1: Validate the target

The user must provide a directory path as `$ARGUMENTS`. If no argument is provided, ask: "Which directory should I move outside Drive? (e.g. `./node_modules`)"

Resolve the path to an absolute path. Verify:
1. The path exists and is a directory (not already a symlink).
2. The path is inside a Google Drive-synced location (i.e. under `~/My Drive/`).

If any check fails, explain the issue and stop.

#### Step 2: Move outside Drive

The external storage root is `~/.drive-nosync/`. The directory structure mirrors `~/My Drive/`:

```bash
DRIVE_ROOT="$HOME/My Drive"
NOSYNC_ROOT="$HOME/.drive-nosync"

rel_path="${dir#$DRIVE_ROOT/}"
ext_dir="$NOSYNC_ROOT/$(dirname "$rel_path")/$(basename "$dir")"

mkdir -p "$(dirname "$ext_dir")"
mv "$dir" "$ext_dir"
ln -s "$ext_dir" "$dir"
```

Verify the symlink resolves correctly by listing a few entries inside it.

#### Step 3: Update .gitignore (if applicable)

If the directory is inside a git repository:
1. Read the `.gitignore` file (at the repo root).
2. Ensure the directory name is ignored (e.g. `node_modules/`).
3. If there are stale `.nosync` entries from the old approach, remove them.

#### Step 4: Commit

Commit the `.gitignore` change (if any) with the message: `chore: move <dirname> outside Google Drive sync root`.

#### Step 5: Report

Tell the user what was done and remind them that if they ever run a fresh install (e.g. `npm install`) that recreates the directory as a real directory instead of following the symlink, they may need to re-run this skill.
