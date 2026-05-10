---
name: fix-drive-errors
description: Triage Google Drive sync errors by finding generated dependency/build/cache directories, moving confirmed offenders out of Drive, repairing symlinks, and restarting Drive. For one named directory use nosync.
---

# Fix Google Drive sync errors

Scan `~/My Drive/` for directories that cause sync errors (`node_modules`, build output, caches, virtual environments, clearly generated large data dirs), move confirmed offenders outside Drive's sync root to `~/.drive-nosync/`, repair symlink fallout, and restart Google Drive after changes.

## Background

Google Drive cannot sync symlinks, and struggles with certain directory contents (node_modules `.bin/` symlinks, Python bytecode, etc.). The **only** reliable fix is to move the problematic directory entirely outside `~/My Drive/` and leave a symlink in its place. The symlink is invisible to Drive (it skips symlinks), and the real data lives outside Drive's sync root.

**Important**: the `.nosync` suffix is an iCloud convention. Google Drive does **not** honor it. A directory named `foo.nosync` inside `~/My Drive/` is synced like any other directory. Do not use the `.nosync` rename trick; it does not work for Google Drive.

## When this skill is invoked

When triggered, follow the execution steps below. Do not just describe what the skill does.

Proceed without asking for routine generated directories that are clearly safe to move. Stop and ask before moving directories that may contain source or user data, merging with an existing `~/.drive-nosync/` target, or deleting anything other than replacing a known stale symlink.

Do not use this skill for one explicitly named directory; use `nosync` for that narrower workflow. Do not use it for Google Docs/content sync problems that are not caused by local generated directories.

### Execution steps

#### Step 1: Scan for problematic directories

Set common roots first:

```bash
DRIVE_ROOT="$HOME/My Drive"
NOSYNC_ROOT="$HOME/.drive-nosync"
mkdir -p "$NOSYNC_ROOT"
```

Search `~/My Drive/` for directories that commonly cause Google Drive sync errors. Run all of these searches in parallel, then review the candidates before moving anything:

1. **node_modules** (npm/yarn dependencies):
   ```bash
   find "$DRIVE_ROOT" -maxdepth 6 -type d -name "node_modules" -print 2>/dev/null
   ```

2. **Build output directories**:
   ```bash
   find "$DRIVE_ROOT" -maxdepth 5 -type d \( -name ".next" -o -name "out" -o -name "dist" -o -name "build" -o -name ".nuxt" -o -name ".svelte-kit" -o -name ".parcel-cache" -o -name ".turbo" -o -name ".cache" \) -print 2>/dev/null
   ```
   Filter these: only include directories that are **generated** (listed in `.gitignore`, created by a build tool, or otherwise clearly disposable). Directories named `build`, `dist`, `out`, or `data` that contain source/user data must be skipped unless the repository proves they are generated.

3. **Python caches and virtual environments**:
   ```bash
   find "$DRIVE_ROOT" -maxdepth 6 -type d \( -name "__pycache__" -o -name ".pytest_cache" -o -name "venv" -o -name ".venv" -o -name ".mypy_cache" -o -name ".ruff_cache" -o -name ".tox" -o -name ".nox" \) -print 2>/dev/null
   ```

4. **Legacy `.nosync` directories** still inside Drive:
   ```bash
   find "$DRIVE_ROOT" -maxdepth 6 -name "*.nosync" -type d -print 2>/dev/null
   ```
   These are left over from the old (broken) `.nosync` rename trick and must be migrated out.

For any command whose output is piped into a loop, use `-print0` plus `while IFS= read -r -d '' dir` so paths with spaces and backslashes are handled correctly. `find -type d` already skips symlinked directories.

#### Step 2: Check for broken symlinks

Verify that all symlinks pointing to `~/.drive-nosync/` are intact:

```bash
find "$DRIVE_ROOT" -maxdepth 6 -type l -print0 2>/dev/null |
while IFS= read -r -d '' link; do
  target=$(readlink "$link")
  case "$target" in
    "$NOSYNC_ROOT"/*|*/.drive-nosync/*)
      if [ ! -e "$link" ]; then
        printf 'BROKEN\t%s -> %s\n' "$link" "$target"
      fi
      ;;
  esac
done
```

Also check for directories that were recreated as real dirs by a package manager, replacing a symlink:

```bash
find "$NOSYNC_ROOT" -maxdepth 8 -type d \( -name "node_modules" -o -name ".next" -o -name "out" -o -name "dist" -o -name "build" -o -name ".nuxt" -o -name ".svelte-kit" -o -name ".parcel-cache" -o -name ".turbo" -o -name ".cache" -o -name "__pycache__" -o -name ".pytest_cache" -o -name "venv" -o -name ".venv" -o -name ".mypy_cache" -o -name ".ruff_cache" -o -name ".tox" -o -name ".nox" \) -print0 2>/dev/null |
while IFS= read -r -d '' ext_dir; do
  rel_path="${ext_dir#$NOSYNC_ROOT/}"
  drive_dir="$DRIVE_ROOT/$rel_path"
  if [ -d "$drive_dir" ] && [ ! -L "$drive_dir" ]; then
    printf 'RECREATED\t%s has external copy %s\n' "$drive_dir" "$ext_dir"
  fi
done
```

Broken symlinks are not automatically repairable if the external target is gone. Report them unless there is a newly recreated real directory at the Drive path that can be moved after resolving the conflict.

#### Step 3: Move problematic directories outside Drive

The external storage root is `~/.drive-nosync/`. The directory structure mirrors `~/My Drive/`:

```
~/My Drive/repos/foo/node_modules  ->  ~/.drive-nosync/repos/foo/node_modules
```

Process confirmed candidates from largest to smallest so the biggest sync wins happen first. For each directory found in Step 1, skip paths that are already symlinks or no longer exist, and refuse to overwrite an existing external target:

```bash
rel_path="${dir#$DRIVE_ROOT/}"          # example: repos/foo/node_modules
ext_dir="$NOSYNC_ROOT/$(dirname "$rel_path")/$(basename "$dir")"

if [ -L "$dir" ] || [ ! -d "$dir" ]; then
  printf 'SKIP\t%s is not a real directory\n' "$dir"
  continue
fi

if [ -e "$ext_dir" ] || [ -L "$ext_dir" ]; then
  printf 'CONFLICT\t%s already exists; inspect before merging %s\n' "$ext_dir" "$dir"
  continue
fi

mkdir -p "$(dirname "$ext_dir")"
mv "$dir" "$ext_dir"
ln -s "$ext_dir" "$dir"
du -sh "$ext_dir"
```

For **legacy `.nosync` directories**, also update the existing symlink:
1. Move `dir.nosync` to `~/.drive-nosync/.../dir`
2. Move the old symlink (`dir -> dir.nosync`) to trash or aside with a timestamped `.stale` suffix
3. Create new symlink (`dir -> ~/.drive-nosync/.../dir`)

Do not merge two real directories or delete the stale in-Drive copy without explicit confirmation.

#### Step 4: Rewrite broken relative symlinks

After each successful move, rewrite broken relative symlinks inside the moved tree whose original targets still exist. This mirrors the `nosync` skill and prevents relative `..` links from silently breaking after the tree moves from `~/My Drive/` to `~/.drive-nosync/`.

```bash
python3 - "$dir" "$ext_dir" <<'PY'
import os
import sys
from pathlib import Path

src, dst = sys.argv[1], sys.argv[2]
fixed = []
remaining = []

for link in Path(dst).rglob("*"):
    if not link.is_symlink():
        continue
    if link.exists():
        continue
    target = os.readlink(link)
    if os.path.isabs(target):
        remaining.append((str(link), target))
        continue
    orig_link = src + str(link)[len(dst):]
    abs_target = os.path.normpath(os.path.join(os.path.dirname(orig_link), target))
    if os.path.exists(abs_target):
        link.unlink()
        link.symlink_to(abs_target)
        fixed.append((str(link), abs_target))
    else:
        remaining.append((str(link), target))

for link, target in fixed:
    print(f"rewrote {link} -> {target}")
for link, target in remaining:
    print(f"still broken {link} -> {target}")
PY
```

Report any remaining broken symlinks as unresolved rather than hiding them.

#### Step 5: Update .gitignore files

For each affected git repository, ensure the original directory names are gitignored (they should already be). No `.nosync` entries are needed.

If there are stale `.nosync` gitignore entries from the old approach, remove them.

If you edit a `.gitignore`, commit only that `.gitignore` hunk in the affected repository with a scoped message such as `chore: ignore generated Google Drive nosync directory`. If the repository has unrelated dirty changes and safe partial staging is not practical, report the uncommitted `.gitignore` edit instead of staging unrelated work.

#### Step 6: Restart Google Drive

Restart Google Drive if any directory was moved, symlink was repaired, or `.gitignore` was updated:

```bash
if pgrep -x "Google Drive" >/dev/null; then
  killall "Google Drive"
  while pgrep -x "Google Drive" >/dev/null; do sleep 1; done
fi
open -a "Google Drive"
sleep 5
if pgrep -x "Google Drive" >/dev/null; then
  echo "Google Drive restarted successfully"
else
  echo "Google Drive did not appear to start; report this as unresolved"
fi
```

If the scan made no changes, do not restart Drive unless the user explicitly asked for a restart.

#### Step 7: Verify and report

Before reporting completion:
- Re-run the relevant scans and confirm moved candidates are symlinks, not recreated real directories.
- Verify each new symlink resolves with `[ -L "$dir" ] && [ -e "$dir" ]`.
- Confirm Google Drive is running if it was restarted.
- Check `git status --short` in any repository where `.gitignore` was touched and make sure unrelated changes were not staged.

Summarize:
- **Directories moved** outside Drive and total size removed from sync.
- **Broken symlinks** repaired (if any).
- **`.gitignore` files** updated (if any).
- Confirmation that Google Drive was restarted, or that no restart was needed.
- **Unresolved issues**: conflicts, ambiguous directories, missing external targets, remaining broken symlinks, or failed restart.

Remind the user that:
- Package installs (`npm install`, `pip install`) or builds (`next build`, `hugo`) may recreate directories as real directories instead of following the symlink. If that happens, re-run `/fix-drive-errors`.
- The `/nosync` skill can be used to fix individual directories on the spot.
