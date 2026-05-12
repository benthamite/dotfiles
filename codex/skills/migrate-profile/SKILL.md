---
name: migrate-profile
description: Run post-Emacs-profile-switch housekeeping after elpaca profile changes. Use when the user says "/migrate-profile", changes Emacs or elpaca profiles, switches to a new profile, or wants Claude/Codex sessions, memory, trust entries, history, cwd paths, profile symlinks/caches, or new-profile repos checked after a profile bump.
argument-hint: "[old-profile] to [new-profile]"
---

# Post-profile-switch migration

Run the housekeeping needed immediately after switching to a new Emacs/elpaca profile. This includes profile-state checks, remote/local repo drift checks, per-package agent directory migration, and Claude Code/Codex state migration. Claude data needs project-directory merging under `~/.claude/projects/`; Codex data needs in-place structured path rewrites across `~/.codex` session, history, index, and archive JSONL files. Package-local `.claude/` and `.codex/` directories need non-destructive copying from old-profile package directories to their new-profile package directories. No explicit package list is needed for Claude project directories.

> **Related skills.** The per-package logic here is essentially the same operation as `move-session-log --rename` (whole-project rename mode), applied in bulk. This skill stays separate because it adds (a) merge semantics — multiple sources can flow into one target — and (b) profile-path rewrites in `~/.claude.json` keyed on the elpaca profile name rather than a single old/new pair. For a single project rename outside an elpaca-profile bump, use the `move-session-log` skill directly (and `rename-project` for full project renames).

Treat this as destructive-adjacent: it retargets a profile symlink, may pull profile repos after confirmation, rewrites agent state files, and eventually trashes old Claude project directories. Always produce the dry run first and get explicit confirmation before changing symlinks, pulling repos, copying data, rewriting JSON/JSONL/log files, or deleting sources.

## Scope

Migrate both stores unless the user explicitly asks for only one:

- **Profile state**: `~/.config/emacs-profiles/.current-profile` and `~/.config/emacs-profiles/active`.
- **New-profile repos**: git repositories under `~/.config/emacs-profiles/<new-profile>/elpaca/sources/` and `repos/`.
- **Package-local agent directories**: `.claude/` and `.codex/` directories inside old-profile elpaca package directories, copied into the corresponding new-profile package directories.
- **Claude Code**: `~/.claude/projects/`, `~/.claude.json`, `~/.claude/history.jsonl`, and copied session `.jsonl` files.
- **Codex**: `~/.codex/history.jsonl`, `~/.codex/session_index.jsonl`, `~/.codex/sessions/**/*.jsonl`, and `~/.codex/archived_sessions/**/*.jsonl`.

Codex currently stores sessions by date/rollout ID rather than by encoded project directory. Do not look for Codex equivalents of `~/.claude/projects`; instead, count and rewrite structured `cwd` and `project` fields whose value is an exact elpaca package path under the old profile.

## Determine profiles

- If `$ARGUMENTS` uses `OLD to NEW` or `OLD -> NEW` (e.g., `8.2.0-dev to 8.3.0-dev`), migrate only from `OLD_PROFILE` to `NEW_PROFILE`.
- If `$ARGUMENTS` names one profile (e.g., `8.3.0-dev`), treat that as `NEW_PROFILE` and discover all older elpaca profile sources.
- If `$ARGUMENTS` is empty, auto-detect `NEW_PROFILE`:

```bash
emacsclient -e 'init-current-profile' | tr -d '"'
```

- If `emacsclient` is unavailable and no `NEW_PROFILE` was given, ask the user.
- When `OLD_PROFILE` is explicit, dry-run and migrate only matching Claude/Codex references for that old profile. Do not migrate data from other old profiles in the same run.

## Path encoding

Claude Code encodes project paths as directory names under `~/.claude/projects/` by replacing every `/` with `-`. The leading `/` becomes a leading `-`. Dots are also replaced with `-`.

Example: `/Users/pablostafforini/.config/emacs-profiles/8.0.0-dev/elpaca/sources/elfeed-ai` becomes `-Users-pablostafforini--config-emacs-profiles-8-0-0-dev-elpaca-sources-elfeed-ai`.

Codex stores normal filesystem paths inside JSON/JSONL content, so Codex migration rewrites structured `cwd` and `project` fields from old package paths to new package paths:

```text
/Users/pablostafforini/.config/emacs-profiles/<old-profile>/elpaca/
/Users/pablostafforini/.config/emacs-profiles/<new-profile>/elpaca/
```

Do not perform raw string replacement across Codex transcript text, command output, function-call arguments, or logs. Preserve historical prompts and outputs.

## Profile state checks

There are two profile markers with different meanings:

- `~/.config/emacs-profiles/.current-profile` is the real active-profile cache written by Emacs at startup from `init-current-profile`. Git hooks use this cache to avoid `emacsclient` deadlocks.
- `~/.config/emacs-profiles/active` is a stable/legacy symlink used by any path that intentionally wants a profile-independent filesystem location.

During the dry run, report:

- `NEW_PROFILE` from arguments or `emacsclient -e 'init-current-profile'`
- `.current-profile` contents and whether it matches `NEW_PROFILE`
- `active` symlink target and whether it points at `NEW_PROFILE`

If `.current-profile` does not match `NEW_PROFILE`, stop before mutating anything unless the user explicitly confirms that Emacs has not yet been restarted into the new profile or that the mismatch is expected.

After confirmation, retarget `active` only if it exists or if the dry run found paths that use it. Use:

```bash
ln -sfn "$HOME/.config/emacs-profiles/$NEW_PROFILE" "$HOME/.config/emacs-profiles/active"
```

Do not rewrite `.current-profile` by hand. It is generated by Emacs startup; if it is wrong, ask the user to start/restart Emacs in the intended profile or explicitly confirm continuing despite the mismatch.

## New-profile repo drift check

After a profile switch, the new profile may contain stale clones if work continued in the previous profile and was pushed before the user started using the new profile. Check every git repository under:

```text
~/.config/emacs-profiles/<new-profile>/elpaca/sources/
~/.config/emacs-profiles/<new-profile>/elpaca/repos/
```

For each repo:

1. Record dirty worktree state with `git status --porcelain`.
2. Find the upstream with `git rev-parse --abbrev-ref --symbolic-full-name @{u}`. If no upstream exists, report `no upstream` and do not fetch/pull it automatically.
3. Run `git fetch --prune` for repos with upstreams.
4. Compare local vs upstream with `git rev-list --left-right --count HEAD...@{u}`.

Dry-run report columns:

- package/repo name
- path
- dirty? (`yes`/`no`)
- upstream or `none`
- local-only commits
- remote-only commits
- status: `current`, `needs pull`, `local ahead`, `diverged`, `dirty`, or `no upstream`

After confirmation, pull only repos that are clean, have an upstream, have remote-only commits, and have no local-only commits:

```bash
git -C "$repo" pull --ff-only
```

Do not pull dirty repos or diverged repos. Report them as manual follow-up items.

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

## Package-local agent directory migration

Some package repositories contain agent-local state under `.claude/` or `.codex/`, such as:

```text
/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev/elpaca/sources/tlon/.claude
/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev/elpaca/sources/tlon/.codex
```

For each migration record, check the old package path for `.claude/` and `.codex/`. If either exists, copy it to the verified `new_path` for that package without overwriting existing target files. Treat this as part of the same per-package migration record as the Claude project directory and Codex structured-path rewrite.

Dry-run reporting:

- Count source `.claude/` and `.codex/` directories found under old-profile package paths.
- For each package, report whether the target `.claude/` or `.codex/` directory already exists, will be created, will receive only missing files, or is skipped because the package has no verified new-profile target.
- Count files that would be copied and files that would be skipped because the target already has the same relative path.

Execution rules:

- Create `new_path/.claude/` or `new_path/.codex/` only when the corresponding source directory exists.
- Copy files and subdirectories recursively while preserving metadata.
- Do not overwrite existing target files. If both source and target contain the same relative file path, leave the target file untouched and count it as skipped.
- Do not rewrite contents inside package-local `.claude/` or `.codex/` files unless a later explicit section says to; this step is a non-destructive directory merge only.
- Do not trash the old package directory after copying these local agent directories. The profile cleanup only trashes old encoded directories under `~/.claude/projects/`.

Example shell shape:

```bash
for agent_dir in .claude .codex; do
  src="$old_path/$agent_dir"
  dst="$new_path/$agent_dir"
  [ -d "$src" ] || continue
  mkdir -p "$dst"
  (cd "$src" && find . -type d -exec mkdir -p "$dst/{}" \;)
  (cd "$src" && find . -type f -print | while read -r rel; do
    if [ ! -e "$dst/$rel" ]; then
      cp -p "$src/$rel" "$dst/$rel"
    fi
  done)
done
```

## Claude migration (non-destructive merge)

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

## Codex migration (structured in-place rewrite)

Codex migration is not a directory merge. Scan the Codex store for JSONL rows containing structured `cwd` or `project` values under the old profile, then rewrite only those structured fields.

### 1. Discover affected Codex files

```bash
CODEX_HOME="${CODEX_HOME:-$HOME/.codex}"
OLD_PROFILE_PATH="/Users/pablostafforini/.config/emacs-profiles/$OLD_PROFILE"

rg -l "\"(cwd|project)\":\"$OLD_PROFILE_PATH/elpaca/" \
  "$CODEX_HOME/history.jsonl" \
  "$CODEX_HOME/session_index.jsonl" \
  "$CODEX_HOME/sessions" \
  "$CODEX_HOME/archived_sessions" 2>/dev/null
```

During the dry run, report:

- affected Codex file count by area (`history`, `session_index`, active sessions, archived sessions, logs)
- number of structured `cwd` and `project` fields to rewrite
- whether any matching file is not valid JSONL when it has a `.jsonl` suffix

### 2. Rewrite Codex files

Rewrite `.jsonl` files line by line as JSON. For each object, recursively rewrite only keys named `cwd` or `project` whose value exactly matches an old-profile elpaca package path. Use the same migration records as Claude:

- `source_pkg` matches the old path package name.
- `new_path` is the verified target path under the new profile.
- Renamed packages use `source_pkg` for matching and `new_path` for replacement.

```python
import json
import re
from pathlib import Path

project_pattern = re.compile(
    r"^/Users/pablostafforini/\.config/emacs-profiles/"
    + re.escape(OLD_PROFILE)
    + r"/elpaca/(?:repos|sources)/([^/]+)$"
)

def rewrite_structured_paths(obj):
    count = 0
    if isinstance(obj, dict):
        for key, value in obj.items():
            if key in {"cwd", "project"} and isinstance(value, str):
                match = project_pattern.match(value)
                if match:
                    new_path = migrations.get(match.group(1))
                    if new_path and value != new_path:
                        obj[key] = new_path
                        count += 1
            else:
                count += rewrite_structured_paths(value)
    elif isinstance(obj, list):
        for item in obj:
            count += rewrite_structured_paths(item)
    return count
```

Do not trash Codex files. Codex history is a single store; this migration only updates structured metadata paths.

## Execution

1. **Dry run first**: before copying anything, present a Claude summary table showing:
   - Profile state: `NEW_PROFILE`, `.current-profile`, `active` symlink target, and whether each matches
   - Repo drift: counts and table of new-profile repos that are dirty, have no upstream, need pull, are ahead, or diverged
   - Package name
   - Source directories (count and profile names)
   - Target directory (existing / will create / package missing from new profile)
   - Sessions to copy (count)
   - Memory files to copy (count)
   - Package-local `.claude/` and `.codex/` directories to merge, with files to copy and existing target files to skip
   - Trust entry in `~/.claude.json` (yes/no)
   - history.jsonl entries to rewrite (count)
   - Active profile symlink state (already current / will retarget)
   - Status: "will migrate", "already migrated" (all files exist in target), "skipped" (no source or no target)

   Also present a Codex summary showing affected JSONL file counts by area, structured `cwd`/`project` fields to rewrite, JSONL parse problems, and whether the Codex step is "will rewrite" or "already current".

2. **Ask for confirmation** before proceeding with symlink retarget, repo pulls, actual copy, rewrites, and deletion.

3. **Execute the migration**: retarget the `active` symlink when appropriate, pull clean fast-forwardable new-profile repos after confirmation, merge package-local `.claude/` and `.codex/` directories without overwriting, create any missing Claude targets, copy Claude data without overwriting, rewrite Claude trust/history/session paths, rewrite Codex history/session/archive structured paths, then trash Claude sources whose migration succeeded.

4. **Post-migration summary**: report `.current-profile` state, whether the `active` symlink was retargeted, which repos were pulled or need manual attention, how many package-local `.claude/` and `.codex/` files were copied or skipped, how many Claude sessions and memory files were copied for each package, how many Claude source directories were trashed, how many trust entries were migrated in `~/.claude.json`, how many Claude `history.jsonl` and session `.jsonl` path entries were rewritten, how many Codex files and structured fields were rewritten, and any packages or Codex files that were skipped.

## Important notes

- Existing Claude data in the target is never overwritten — only new files are copied.
- Existing package-local `.claude/` and `.codex/` files in the new profile are never overwritten — only missing files are copied.
- Claude source directories are moved to the trash only after their data is copied to the target and path rewrites have succeeded.
- Codex JSONL files are rewritten in place and never trashed.
- Repo pulls are fast-forward only and only for clean repos.
- All elpaca project directories are discovered automatically — no package list is needed.
