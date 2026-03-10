---
name: migrate-profile
description: Consolidate Claude Code project data across elpaca profile changes. Use `/migrate-profile [new-profile]` to migrate.
argument-hint: "[new-profile]"
---

# Profile migration

Consolidate Claude Code project data (sessions, memory, subagents, tool results) across elpaca profile changes by scanning `~/.claude/projects/` for all directories matching the elpaca pattern and grouping them by package name. No explicit package list is needed.

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

## Package renames

Some packages were renamed between profiles. Known renames (old → new):

- `infovore` → `elfeed-ai`
- `tango-wiki` → `tangodb`

After grouping, merge any sources under old names into the new name's source list:

```bash
# For each known rename: old_name → new_name
for old_name in infovore tango-wiki; do
  case "$old_name" in
    infovore)    new_name="elfeed-ai" ;;
    tango-wiki)  new_name="tangodb" ;;
  esac
  if [[ -n "${sources_map[$old_name]}" ]]; then
    sources_map[$new_name]+="${sources_map[$old_name]}"
    unset "sources_map[$old_name]"
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

If the package does not exist under the new profile at all, skip it and note this in the summary.

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
      cp "$f" "$CLAUDE_PROJECTS/${targets[$pkg]}/$base"
    fi
  done

  # Copy session subdirectories that don't exist in target
  for d in "$CLAUDE_PROJECTS/$SOURCE_ENCODED"/*/; do
    [ -d "$d" ] || continue
    base=$(basename "$d")
    [ "$base" = "memory" ] && continue  # handle memory separately
    if [ ! -d "$CLAUDE_PROJECTS/${targets[$pkg]}/$base" ]; then
      cp -R "$d" "$CLAUDE_PROJECTS/${targets[$pkg]}/$base"
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
        cp "$f" "$CLAUDE_PROJECTS/${targets[$pkg]}/memory/$base"
      fi
    done
  fi
done
```

### 4. Delete source directories

After successfully copying all data from a source directory, delete it:

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
   - Status: "will migrate", "already migrated" (all files exist in target), "skipped" (no source or no target)

2. **Ask for confirmation** before proceeding with the actual copy and deletion.

3. **Execute the migration** (copy then delete sources) using the commands above.

4. **Post-migration summary**: report how many sessions and memory files were copied for each package, how many source directories were deleted, and any packages that were skipped.

## Important notes

- Existing data in the target is never overwritten — only new files are copied.
- Source directories are deleted after their data is copied to the target.
- All elpaca project directories are discovered automatically — no package list is needed.
