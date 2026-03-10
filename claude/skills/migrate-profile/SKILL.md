---
name: migrate-profile
description: Migrate Claude Code project data (sessions, memory) from an old Emacs elpaca profile to the current one. Use `/migrate-profile <old-profile>` to migrate.
---

# Profile migration

Migrate Claude Code project-specific data (sessions, memory, subagents, tool results) from an old Emacs elpaca profile to the current one, for each package listed in the user's packages file.

## Determine profiles

### Old profile

- If `$ARGUMENTS` names a profile (e.g., `7.1.30-target`), use that.
- If `$ARGUMENTS` is empty, ask the user which old profile to migrate from.

### New profile

Detect the current elpaca profile automatically:

```bash
emacsclient -e 'init-current-profile' | tr -d '"'
```

If `emacsclient` is unavailable, ask the user.

## Determine package list

Read package names from both the listed and unlisted sections of:

```
~/My Drive/notes/pablos-miscellany/my-emacs-packages.org
```

Parse with:

```bash
grep -E '^\*{2,3} =' ~/My\ Drive/notes/pablos-miscellany/my-emacs-packages.org | sed 's/^[* ]* =//;s/=$//'
```

This produces names like `anki-noter`, `stafforini.el`, etc. Strip any `.el` suffix to get the directory name (e.g., `stafforini.el` -> `stafforini`).

## Package renames

Some packages were renamed between profiles. When searching for source data in the old profile, also check old names. Known renames (old → new):

- `infovore` → `elfeed-ai`

When a renamed package is found under its old name, migrate its data into the new package's target directory (merging with any data already there from the new name).

## Path encoding

Claude Code encodes project paths as directory names under `~/.claude/projects/` by replacing every `/` with `-`. The leading `/` becomes a leading `-`.

Example: `/Users/pablostafforini/.config/emacs-profiles/8.0.0-dev/elpaca/sources/elfeed-ai` becomes `-Users-pablostafforini--config-emacs-profiles-8-0-0-dev-elpaca-sources-elfeed-ai`.

Note that dots in the profile name are also replaced: `7.1.30-target` becomes `7-1-30-target`, `8.0.0-dev` becomes `8-0-0-dev`.

## Find source project directories

For each package, search `~/.claude/projects/` for directories matching the old profile. Packages may live under either `elpaca/repos/` or `elpaca/sources/` — check both:

```bash
OLD_PROFILE_ENCODED=$(echo "$OLD_PROFILE" | tr './' '-')
CLAUDE_PROJECTS=~/.claude/projects

for pkg in $PACKAGES; do
  for subdir in repos sources; do
    encoded="-Users-pablostafforini--config-emacs-profiles-${OLD_PROFILE_ENCODED}-elpaca-${subdir}-${pkg}"
    if [ -d "$CLAUDE_PROJECTS/$encoded" ]; then
      echo "FOUND: $encoded"
    fi
  done
done
```

## Find target project directories

For each package that has source data, determine where it lives in the new profile. Check the actual filesystem:

```bash
NEW_PROFILE_ENCODED=$(echo "$NEW_PROFILE" | tr './' '-')

for subdir in sources repos; do
  if [ -d "/Users/pablostafforini/.config/emacs-profiles/$NEW_PROFILE/elpaca/$subdir/$pkg" ]; then
    TARGET_ENCODED="-Users-pablostafforini--config-emacs-profiles-${NEW_PROFILE_ENCODED}-elpaca-${subdir}-${pkg}"
    break
  fi
done
```

If the package does not exist under the new profile at all, skip it and note this in the summary.

## Migration

For each package with both a source and target:

### 1. Create the target directory if needed

```bash
mkdir -p "$CLAUDE_PROJECTS/$TARGET_ENCODED"
```

### 2. Copy session files (non-destructive merge)

Copy session `.jsonl` files and session subdirectories that do not already exist in the target:

```bash
# Copy top-level .jsonl files that don't exist in target
for f in "$CLAUDE_PROJECTS/$SOURCE_ENCODED"/*.jsonl; do
  [ -f "$f" ] || continue
  base=$(basename "$f")
  if [ ! -f "$CLAUDE_PROJECTS/$TARGET_ENCODED/$base" ]; then
    cp "$f" "$CLAUDE_PROJECTS/$TARGET_ENCODED/$base"
  fi
done

# Copy session subdirectories that don't exist in target
for d in "$CLAUDE_PROJECTS/$SOURCE_ENCODED"/*/; do
  [ -d "$d" ] || continue
  base=$(basename "$d")
  [ "$base" = "memory" ] && continue  # handle memory separately
  if [ ! -d "$CLAUDE_PROJECTS/$TARGET_ENCODED/$base" ]; then
    cp -R "$d" "$CLAUDE_PROJECTS/$TARGET_ENCODED/$base"
  fi
done
```

### 3. Copy memory (non-destructive merge)

If the source has a `memory/` subdirectory, copy its files to the target's `memory/` directory without overwriting existing files:

```bash
if [ -d "$CLAUDE_PROJECTS/$SOURCE_ENCODED/memory" ]; then
  mkdir -p "$CLAUDE_PROJECTS/$TARGET_ENCODED/memory"
  for f in "$CLAUDE_PROJECTS/$SOURCE_ENCODED/memory/"*; do
    [ -f "$f" ] || continue
    base=$(basename "$f")
    if [ ! -f "$CLAUDE_PROJECTS/$TARGET_ENCODED/memory/$base" ]; then
      cp "$f" "$CLAUDE_PROJECTS/$TARGET_ENCODED/memory/$base"
    fi
  done
fi
```

## Execution

1. **Dry run first**: before copying anything, present a summary table showing:
   - Package name
   - Source directory (found / not found)
   - Target directory (found / not found / package missing from new profile)
   - Sessions to copy (count)
   - Memory files to copy (count)
   - Status: "will migrate", "already migrated" (all files exist in target), "skipped" (no source or no target)

2. **Ask for confirmation** before proceeding with the actual copy.

3. **Execute the copy** using the bash commands above.

4. **Post-migration summary**: report how many sessions and memory files were copied for each package, and any packages that were skipped.

## Important notes

- This is a **non-destructive merge**: existing data in the target is never overwritten.
- Source data is **not deleted** after migration. The user can clean up old profile data manually if desired.
- Packages that exist in the old profile's Claude Code data but are NOT in the packages org file are intentionally ignored by this skill. If any are detected, mention them at the end of the summary so the user can decide whether to migrate them separately.
