---
name: migrate-profile
description: Consolidate Claude Code project data after elpaca profile changes. Use when the user says "/migrate-profile", changes Emacs or elpaca profiles, or wants Claude sessions, memory, trust entries, history, cwd paths, or the active profile symlink moved to a new profile.
argument-hint: "[new-profile]"
---

# Profile migration

Consolidate Claude Code project data (sessions, memory, subagents, tool results) across elpaca profile changes by scanning `~/.claude/projects/` for all directories matching the elpaca pattern and grouping them by package name. No explicit package list is needed.

> **Related skills.** The per-package logic here is essentially the same operation as `move-session-log --rename` (whole-project rename mode), applied in bulk. This skill stays separate because it adds (a) merge semantics — multiple sources can flow into one target — and (b) profile-path rewrites in `~/.claude.json` keyed on the elpaca profile name rather than a single old/new pair. For a single project rename outside an elpaca-profile bump, use the `move-session-log` skill directly (and `rename-project` for full project renames).

Treat this as destructive-adjacent: it retargets a profile symlink and eventually trashes old Claude project directories. Always produce the dry run first and get explicit confirmation before changing the symlink, copying data, rewriting JSONL files, or deleting sources.

## Determine new profile

- If `$ARGUMENTS` names a profile (e.g., `8.0.0-dev`), use that.
- If `$ARGUMENTS` is empty, auto-detect:

```bash
emacsclient -e 'init-current-profile' | tr -d '"'
```

- If `emacsclient` is unavailable, ask the user.

## Path encoding

Claude Code encodes project paths as directory names under `~/.claude/projects/` by replacing every `/` with `-`. The leading `/` becomes a leading `-`. Dots are also replaced with `-`.

Example: `/Users/pablostafforini/.config/emacs-profiles/8.0.0-dev/elpaca/sources/elfeed-ai` becomes `-Users-pablostafforini--config-emacs-profiles-8-0-0-dev-elpaca-sources-elfeed-ai`.

## Update the active-profile symlink

During the dry run, report whether the stable symlink `~/.config/emacs-profiles/active` already points at the new profile. After confirmation, retarget it so that org `#+INCLUDE:` directives (and anything else using the stable path) resolve to the new profile:

```bash
ln -sfn "$HOME/.config/emacs-profiles/$NEW_PROFILE" "$HOME/.config/emacs-profiles/active"
```

## Discover and group projects

Scan `~/.claude/projects/` for all directories matching the elpaca pattern. Extract the package name by splitting on `-elpaca-repos-` or `-elpaca-sources-` (these are unambiguous delimiters — they correspond to the literal directory structure `elpaca/repos/` or `elpaca/sources/`).

```bash
CLAUDE_PROJECTS=~/.claude/projects
NEW_PROFILE_ENCODED=$(echo "$NEW_PROFILE" | tr './' '-')

declare -A targets sources_map

for dir in "$CLAUDE_PROJECTS"/-Users-pablostafforini--config-emacs-profiles-*-elpaca-*; do
  [ -d "$dir" ] || continue
  base=$(basename "$dir")

  # Extract package name by splitting on the elpaca subdir delimiter
  if [[ "$base" == *-elpaca-repos-* ]]; then
    pkg="${base##*-elpaca-repos-}"
  elif [[ "$base" == *-elpaca-sources-* ]]; then
    pkg="${base##*-elpaca-sources-}"
  else
    continue
  fi

  # Classify as target or source based on whether it contains the new profile
  if [[ "$base" == *"-${NEW_PROFILE_ENCODED}-elpaca-"* ]]; then
    targets[$pkg]="$base"
  else
    sources_map[$pkg]+="$base "
  fi
done
```

## Find or create target directories

For each package that has sources but no existing target in `~/.claude/projects/`, check whether the package exists on disk under the new profile and construct the target path:

```bash
for pkg in "${!sources_map[@]}"; do
  if [[ -z "${targets[$pkg]}" ]]; then
    for subdir in sources repos; do
      if [ -d "/Users/pablostafforini/.config/emacs-profiles/$NEW_PROFILE/elpaca/$subdir/$pkg" ]; then
        targets[$pkg]="-Users-pablostafforini--config-emacs-profiles-${NEW_PROFILE_ENCODED}-elpaca-${subdir}-${pkg}"
        break
      fi
    done
  fi
done
```

If the package does not exist under the new profile at all, **ask the user whether the package was renamed**. Packages are sometimes renamed across profiles (e.g., `claude-log` -> `agent-log`, `infovore` -> `elfeed-ai`). If the user confirms a rename, verify the new package name exists under the new profile and keep both names in the migration record:

- `source_pkg`: the old package name extracted from the source directory.
- `target_pkg`: the package name under the new profile.
- `target_encoded_dir`: the Claude project directory name for the target.
- `new_path`: the actual target filesystem path under the new profile.

Use `source_pkg` for matching old `history.jsonl` and session `cwd` values, and `new_path` for the replacement. If no rename applies, skip the package and note this in the summary.

## Migration (non-destructive merge)

For each package with both source(s) and a target:

### 1. Create the target directory if needed

```bash
mkdir -p "$CLAUDE_PROJECTS/${targets[$pkg]}"
```

### 2. Copy session files

Copy session `.jsonl` files and session subdirectories that do not already exist in the target:

```bash
for SOURCE_ENCODED in ${sources_map[$pkg]}; do
  # Copy top-level .jsonl files that don't exist in target
  for f in "$CLAUDE_PROJECTS/$SOURCE_ENCODED"/*.jsonl; do
    [ -f "$f" ] || continue
    base=$(basename "$f")
    if [ ! -f "$CLAUDE_PROJECTS/${targets[$pkg]}/$base" ]; then
      cp -p "$f" "$CLAUDE_PROJECTS/${targets[$pkg]}/$base"
    fi
  done

  # Copy session subdirectories that don't exist in target
  for d in "$CLAUDE_PROJECTS/$SOURCE_ENCODED"/*/; do
    [ -d "$d" ] || continue
    base=$(basename "$d")
    [ "$base" = "memory" ] && continue  # handle memory separately
    if [ ! -d "$CLAUDE_PROJECTS/${targets[$pkg]}/$base" ]; then
      cp -Rp "$d" "$CLAUDE_PROJECTS/${targets[$pkg]}/$base"
    fi
  done
done
```

### 3. Copy memory

If any source has a `memory/` subdirectory, copy its files to the target's `memory/` directory without overwriting existing files:

```bash
for SOURCE_ENCODED in ${sources_map[$pkg]}; do
  if [ -d "$CLAUDE_PROJECTS/$SOURCE_ENCODED/memory" ]; then
    mkdir -p "$CLAUDE_PROJECTS/${targets[$pkg]}/memory"
    for f in "$CLAUDE_PROJECTS/$SOURCE_ENCODED/memory/"*; do
      [ -f "$f" ] || continue
      base=$(basename "$f")
      if [ ! -f "$CLAUDE_PROJECTS/${targets[$pkg]}/memory/$base" ]; then
        cp -p "$f" "$CLAUDE_PROJECTS/${targets[$pkg]}/memory/$base"
      fi
    done
  fi
done
```

### 4. Migrate project trust entries

Project authorization (trust dialog acceptance, onboarding state, tool permissions, MCP config) is stored in `~/.claude.json` under a `projects` object, keyed by the **actual filesystem path** (e.g., `/Users/pablostafforini/.config/emacs-profiles/8.0.0-dev/elpaca/sources/gdocs`). This is separate from session data. Without migrating these entries, Claude Code will prompt the user to re-authorize every project.

For each migrated package, create a corresponding new-profile entry (if one doesn't already exist). Match exact elpaca project paths by package instead of doing a broad string replacement on a single `OLD_PROFILE`; a migration may include sources from several old profiles, and renamed packages need to match the old package name but write the new target path.

```python
import copy
import json
import os
import re

# Build this from the dry-run migration records: source package name ->
# actual target path under the new profile.
# For renamed packages, the key is the old package name and the value contains
# the new package name, e.g. ".../elpaca/sources/agent-log".
migrations = {}

with open(os.path.expanduser("~/.claude.json"), "r") as f:
    data = json.load(f)

projects = data.get("projects", {})
project_pattern = re.compile(
    r"^/Users/pablostafforini/\.config/emacs-profiles/[^/]+/elpaca/(?:repos|sources)/([^/]+)$"
)

for old_path in list(projects.keys()):
    match = project_pattern.match(old_path)
    if not match:
        continue
    new_path = migrations.get(match.group(1))
    if new_path and old_path != new_path and new_path not in projects:
        projects[new_path] = copy.deepcopy(projects[old_path])

data["projects"] = projects
with open(os.path.expanduser("~/.claude.json"), "w") as f:
    json.dump(data, f, indent=2)
```

### 5. Rewrite project paths in history.jsonl

Session entries in `~/.claude/history.jsonl` record the original `project` path (e.g., `…/7.1.30-target/elpaca/repos/annas-archive`). After migration, these must point to the new profile path so that `agent-log-resume-session` opens sessions in the correct directory.

For each migrated package, update every `history.jsonl` entry whose `project` matches any old-profile elpaca path for the source package:

```python
import json
import os
import re

history = os.path.expanduser("~/.claude/history.jsonl")
# new_path: the actual filesystem path under the new profile, e.g.
# /Users/pablostafforini/.config/emacs-profiles/8.2.0-dev/elpaca/sources/annas-archive
# source_pkg: the old package name extracted from the source directory

pattern = re.compile(
    r".*/emacs-profiles/[^/]+/elpaca/(repos|sources)/" + re.escape(source_pkg) + r"$"
)

lines = []
with open(history) as f:
    for line in f:
        entry = json.loads(line)
        if pattern.match(entry.get("project", "")) and entry["project"] != new_path:
            entry["project"] = new_path
        lines.append(json.dumps(entry, ensure_ascii=False))

with open(history, "w") as f:
    f.write("\n".join(lines) + "\n")
```

### 6. Rewrite cwd in session .jsonl files

Each message inside a session `.jsonl` file records a `cwd` field with the original working directory. Update these in all target session files, including files that were already present before the copy:

```python
import glob
import json
import os
import re

# source_pkg: the old package name extracted from the source directory.
# target_encoded_dir: the Claude project directory name from the migration record.
target_dir = os.path.join(CLAUDE_PROJECTS, target_encoded_dir)
pattern = re.compile(
    r".*/emacs-profiles/[^/]+/elpaca/(repos|sources)/" + re.escape(source_pkg) + r"$"
)

for jsonl in glob.glob(os.path.join(target_dir, "*.jsonl")):
    lines = []
    changed = False
    with open(jsonl) as f:
        for line in f:
            entry = json.loads(line)
            if pattern.match(entry.get("cwd", "")) and entry["cwd"] != new_path:
                entry["cwd"] = new_path
                changed = True
            lines.append(json.dumps(entry, ensure_ascii=False))
    if changed:
        with open(jsonl, "w") as f:
            f.write("\n".join(lines) + "\n")
```

### 7. Trash source directories

After all copying and JSON/JSONL rewrites for a source directory have succeeded, move it to the trash:

```bash
for SOURCE_ENCODED in ${sources_map[$pkg]}; do
  trash "$CLAUDE_PROJECTS/$SOURCE_ENCODED"
done
```

This ensures old profile directories don't accumulate and makes it clear which packages have already been migrated.

## Execution

1. **Dry run first**: before copying anything, present a summary table showing:
   - Package name
   - Source directories (count and profile names)
   - Target directory (existing / will create / package missing from new profile)
   - Sessions to copy (count)
   - Memory files to copy (count)
   - Trust entry in `~/.claude.json` (yes/no)
   - history.jsonl entries to rewrite (count)
   - Active profile symlink state (already current / will retarget)
   - Status: "will migrate", "already migrated" (all files exist in target), "skipped" (no source or no target)

2. **Ask for confirmation** before proceeding with the symlink retarget, actual copy, rewrites, and deletion.

3. **Execute the migration**: retarget the symlink, create any missing targets, copy data without overwriting, rewrite trust/history/session paths, then trash sources whose migration succeeded.

4. **Post-migration summary**: report whether the active symlink was retargeted, how many sessions and memory files were copied for each package, how many source directories were trashed, how many trust entries were migrated in `~/.claude.json`, how many `history.jsonl` and session `.jsonl` path entries were rewritten, and any packages that were skipped.

## Important notes

- Existing data in the target is never overwritten — only new files are copied.
- Source directories are moved to the trash only after their data is copied to the target and path rewrites have succeeded.
- All elpaca project directories are discovered automatically — no package list is needed.
