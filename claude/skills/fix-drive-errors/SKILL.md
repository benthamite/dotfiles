---
name: fix-drive-errors
description: Scan Google Drive for directories causing sync errors, move problematic directories outside Drive, and restart Google Drive. Use when Google Drive shows sync errors or periodically to keep the error list clean.
---

# Fix Google Drive sync errors

Scan `~/My Drive/` for directories that cause sync errors (node_modules, build output, caches, large data dirs, etc.), move them outside Drive's sync root to `~/.drive-nosync/`, repair broken symlinks, and restart Google Drive.

## Background

Google Drive cannot sync symlinks, and struggles with certain directory contents (node_modules `.bin/` symlinks, Python bytecode, etc.). The **only** reliable fix is to move the problematic directory entirely outside `~/My Drive/` and leave a symlink in its place. The symlink is invisible to Drive (it skips symlinks), and the real data lives outside Drive's sync root.

**Important**: the `.nosync` suffix is an iCloud convention. Google Drive does **not** honor it. A directory named `foo.nosync` inside `~/My Drive/` is synced like any other directory. Do not use the `.nosync` rename trick — it does not work for Google Drive.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

### Execution steps

#### Step 1: Scan for problematic directories

Search `~/My Drive/` for directories that commonly cause Google Drive sync errors. Run all of these searches in parallel:

1. **node_modules** (npm/yarn dependencies):
   ```
   find ~/My\ Drive -maxdepth 6 -type d -name "node_modules" 2>/dev/null
   ```

2. **Build output directories**:
   ```
   find ~/My\ Drive -maxdepth 5 -type d \( -name ".next" -o -name "out" -o -name "dist" -o -name "build" -o -name ".nuxt" -o -name ".svelte-kit" -o -name ".parcel-cache" -o -name ".turbo" -o -name ".cache" \) 2>/dev/null
   ```
   Filter these: only include directories that are **generated** (i.e., listed in a `.gitignore`, or clearly build artifacts). Directories named `build` or `dist` that contain source code (not generated) should be skipped. Use your judgement and check the parent project's `.gitignore` when uncertain.

3. **Python caches and virtual environments**:
   ```
   find ~/My\ Drive -maxdepth 6 -type d \( -name "__pycache__" -o -name ".pytest_cache" -o -name "venv" -o -name ".venv" -o -name ".mypy_cache" -o -name ".ruff_cache" \) 2>/dev/null
   ```

4. **Legacy `.nosync` directories** still inside Drive:
   ```
   find ~/My\ Drive -maxdepth 6 -name "*.nosync" -type d 2>/dev/null
   ```
   These are left over from the old (broken) `.nosync` rename trick and must be migrated out.

For all searches: exclude directories that are already symlinks (`[ -L "$d" ]`) — those are already handled.

#### Step 2: Check for broken symlinks

Verify that all symlinks pointing to `~/.drive-nosync/` are intact:

```bash
find ~/My\ Drive -maxdepth 6 -type l 2>/dev/null | while read link; do
  target=$(readlink "$link")
  if [[ "$target" == */.drive-nosync/* ]] && [ ! -e "$link" ]; then
    echo "BROKEN: $link -> $target"
  fi
done
```

Also check for directories that were recreated as real dirs by a package manager, replacing a symlink:

```bash
# For each external dir, check if the corresponding Drive path is a real dir instead of a symlink
find ~/.drive-nosync -mindepth 1 -maxdepth 8 -type d -not -path "*/.drive-nosync/*/*" 2>/dev/null
```

A simpler approach: just check known locations where tools recreate directories.

#### Step 3: Move problematic directories outside Drive

The external storage root is `~/.drive-nosync/`. The directory structure mirrors `~/My Drive/`:

```
~/My Drive/repos/foo/node_modules  →  ~/.drive-nosync/repos/foo/node_modules
```

For each directory found in Step 1 (and broken symlinks from Step 2):

```bash
DRIVE_ROOT="$HOME/My Drive"
NOSYNC_ROOT="$HOME/.drive-nosync"

# For each problematic dir:
rel_path="${dir#$DRIVE_ROOT/}"          # e.g. repos/foo/node_modules
ext_dir="$NOSYNC_ROOT/$(dirname "$rel_path")/$(basename "$dir")"

mkdir -p "$(dirname "$ext_dir")"
mv "$dir" "$ext_dir"                    # Same filesystem = instant rename
ln -s "$ext_dir" "$dir"                 # Leave symlink in Drive
```

For **legacy `.nosync` directories**, also update the existing symlink:
1. Move `dir.nosync` to `~/.drive-nosync/.../dir`
2. Remove old symlink (`dir -> dir.nosync`)
3. Create new symlink (`dir -> ~/.drive-nosync/.../dir`)

Process directories from largest to smallest.

#### Step 4: Update .gitignore files

For each affected git repository, ensure the original directory names are gitignored (they should already be). No `.nosync` entries are needed.

If there are stale `.nosync` gitignore entries from the old approach, remove them.

#### Step 5: Restart Google Drive

```bash
killall "Google Drive"
while pgrep -x "Google Drive" > /dev/null; do sleep 1; done
open -a "Google Drive"
sleep 5 && pgrep -x "Google Drive" > /dev/null && echo "Google Drive restarted successfully"
```

#### Step 6: Report

Summarize:
- **Directories moved** outside Drive and total size removed from sync.
- **Broken symlinks** repaired (if any).
- **`.gitignore` files** updated (if any).
- Confirmation that Google Drive was restarted.

Remind the user that:
- Package installs (`npm install`, `pip install`) or builds (`next build`, `hugo`) may recreate directories as real directories instead of following the symlink. If that happens, re-run `/fix-drive-errors`.
- The `/nosync` skill can be used to fix individual directories on the spot.
