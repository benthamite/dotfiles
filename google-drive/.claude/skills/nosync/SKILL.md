---
name: nosync
description: Move one known generated dependency/build/cache directory out of Google Drive sync and leave a symlink. Use when the user names a specific directory; for broad Drive error scans use fix-drive-errors.
argument-hint: <directory-path>
model: sonnet
---

# Move directory outside Google Drive sync

Move a directory from inside `~/My Drive/` to `~/.drive-nosync/` (outside Drive's sync root), leave a symlink in its place, **and remove the copy that remains in the cloud**. Drive skips symlinks, so the directory becomes invisible to sync while tooling continues to work transparently.

**Important**: the `.nosync` suffix is an iCloud convention. Google Drive does **not** honor it. Do not use the `.nosync` rename trick — it does not work for Google Drive.

**The cloud copy must go too** (Step 2.7). Because Drive skips the symlink, a surviving cloud copy has no local counterpart, and on the next full re-merge Drive downloads it back beside the symlink as `node_modules (1)`. Externalizing locally without clearing the cloud copy leaves that trap armed.

**Never** disconnect/reconnect the Drive account or re-register the sync root as part of this workflow. See the "Hard limits" section of `fix-drive-errors` for the documented reason.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

Use this skill only for one explicitly named directory. For scanning `~/My Drive/` for multiple problematic generated directories, use `fix-drive-errors` instead.

### Execution steps

#### Step 1: Validate the target

The user must provide a directory path as `$ARGUMENTS`. If no argument is provided, ask: "Which directory should I move outside Drive? (e.g. `./node_modules`)"

Resolve the path to an absolute path. Verify:
1. The path exists and is a directory (not already a symlink).
2. The path is inside a Google Drive-synced location (i.e. under `~/My Drive/`).
3. The mirrored target path under `~/.drive-nosync/` does not already exist. Stop and ask before merging, overwriting, or deleting anything.

If any check fails, explain the issue and stop.

#### Step 2: Move outside Drive

The external storage root is `~/.drive-nosync/`. The directory structure mirrors `~/My Drive/`:

```bash
DRIVE_ROOT="$HOME/My Drive"
NOSYNC_ROOT="$HOME/.drive-nosync"

rel_path="${dir#$DRIVE_ROOT/}"
ext_dir="$NOSYNC_ROOT/$(dirname "$rel_path")/$(basename "$dir")"

if [ -L "$dir" ] || [ ! -d "$dir" ]; then
  printf 'ERROR\t%s is not a real directory\n' "$dir"
  exit 1
fi

if [ -e "$ext_dir" ] || [ -L "$ext_dir" ]; then
  printf 'CONFLICT\t%s already exists; inspect before merging %s\n' "$ext_dir" "$dir"
  exit 1
fi

mkdir -p "$(dirname "$ext_dir")"
mv "$dir" "$ext_dir"
ln -s "$ext_dir" "$dir"
```

Verify the symlink resolves correctly with `[ -L "$dir" ] && [ -e "$dir" ]`, then list a few entries inside it.

#### Step 2.5: Rewrite broken relative symlinks

Relative symlinks inside the moved tree that pointed *outside* the moved directory will break, because their `..` chain now climbs out of `~/.drive-nosync/` instead of `~/My Drive/`. Walk the moved tree, find each broken relative symlink, compute what it used to point to (by resolving its target string from the symlink's *original* pre-move location), and rewrite to that absolute path if it exists. Pure string normalization — do not use `realpath`/`Path.resolve`, since they follow the new symlink back into nosync.

```bash
python3 - "$dir" "$ext_dir" <<'PY'
import os, sys
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
for l, t in fixed:
    print(f"rewrote {l} -> {t}")
for l, t in remaining:
    print(f"still broken {l} -> {t}")
PY
```

Report the rewrites to the user; if any symlinks remain broken after this pass (e.g. their original target was already missing), list them so the user can decide what to do.

#### Step 2.7: Remove the cloud copy

If the directory was previously synced, a copy still exists in the cloud at the same path. Leave it and Drive will re-download it beside the symlink as `<name> (1)` on the next full re-merge.

Resolve the cloud folder at the corresponding path (walk parent→child from `root`, matching on name + `'<parent>' in parents` + `trashed = false`) and move it to Drive's Trash (`PATCH {"trashed": true}` — recoverable for 30 days). Use the personal-account Drive credentials described in `context/google-services.md`.

Only do this once the local externalization has succeeded and the symlink resolves. The real data is safe in `~/.drive-nosync/`, outside Drive's view. Report the size reclaimed, or say plainly that no cloud copy existed.

#### Step 3: Update .gitignore (if applicable)

If the directory is inside a git repository:
1. Read the `.gitignore` file (at the repo root).
2. Ensure the directory name is ignored, using the **bare name** (`node_modules`, not `node_modules/`). A trailing slash matches only a directory, so it stops matching once the path becomes a symlink and git reports it as newly untracked. Confirm with `git check-ignore -q <name>` instead of assuming, and fix an existing trailing-slash entry for this path.
3. If there are stale `.nosync` entries from the old approach, remove them.
4. Stage only the `.gitignore` hunk. If unrelated dirty changes make safe staging impractical, report the uncommitted `.gitignore` edit instead of staging unrelated work.

#### Step 4: Commit

Commit the `.gitignore` change (if any) with the message: `chore: move <dirname> outside Google Drive sync root`.

#### Step 5: Report

Tell the user what was done and remind them that if they ever run a fresh install (e.g. `npm install`) that recreates the directory as a real directory instead of following the symlink, they may need to re-run this skill.
