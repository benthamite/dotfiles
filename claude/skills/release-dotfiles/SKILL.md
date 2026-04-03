---
name: release-dotfiles
description: Release a new version of the dotfiles repo after verifying all package repos are clean. Use `/release-dotfiles` to cut a release.
---

# Dotfiles release

Cut a new release for the dotfiles repo (benthamite/dotfiles) on GitHub. Before releasing, verify that every repo in the elpaca sources directory has a clean working tree.

## Elpaca profile resolution

Resolve the active elpaca profile dynamically — never hard-code a profile name:

```bash
PROFILE=$(emacsclient -e 'init-current-profile' | tr -d '"')
ELPACA_SOURCES=~/.config/emacs-profiles/$PROFILE/elpaca/sources
```

## Mode selection

Parse `$ARGUMENTS`:

- `--accept` → skip the confirmation gate (step 7) and proceed directly to execution. All other steps still run normally.

---

## Step 0: Verify dotfiles repo

Confirm we are inside the dotfiles git repo:

```bash
git remote get-url origin
```

The remote must match `benthamite/dotfiles`. If not, stop and tell the user to navigate to the dotfiles repo.

Do NOT clone the repo — the user must already be in it.

---

## Step 1: Check all elpaca source repos are clean and pushed

Iterate over every subdirectory in `$ELPACA_SOURCES` that is a git repository. For each, check for uncommitted changes **and** unpushed commits. The goal is to ensure no local changes are lost when switching to a new elpaca profile.

### Cleanliness check

For each git repo in `$ELPACA_SOURCES`:

```bash
git -C "$ELPACA_SOURCES/<dir>" status --porcelain
```

If the output is non-empty, the repo is **dirty**.

```bash
unpushed=$(git -C "$ELPACA_SOURCES/<dir>" log --oneline @{u}..HEAD 2>/dev/null)
rc=$?
```

If `rc` is non-zero, the branch has **no upstream configured** — warn but do not abort. If `rc` is zero and the output is non-empty, the repo has **unpushed commits**.

### Auto-push unpushed repos

For repos that are **clean but have unpushed commits** (i.e., not dirty), push them automatically without asking. Report any push failures (e.g., permission denied on forks).

### Report dirty repos

Only report repos that are **dirty**. Present a table of problems:

```
| Package          | Status                   |
|------------------|--------------------------|
| bib              | dirty                    |
| gdocs            | dirty + 51 unpushed      |
| emacs-slack      | dirty + no upstream      |
```

### Pre-clean: delete artifacts

Before reporting dirty repos, automatically clean up untracked build artifacts:

- **`.elc` files** (byte-compiled Elisp): delete them outright. They are always regenerable and should never be committed.

After deleting artifacts, re-check whether the repo is still dirty. If the only untracked files were artifacts, the repo is now clean.

### Offer to commit dirty repos

If any repos are still dirty after pre-cleaning, **ask the user** whether they want you to commit all changes in all dirty repos. If the user accepts:

1. For each dirty repo, stage all changes (`git add -A`) and create a focused commit with a descriptive message based on the changes.
2. After committing, push each repo.
3. Report any push failures.
4. Then ask the user whether to proceed with the release.

If the user declines, abort the release.

---

## Step 2: Check dotfiles working tree

```bash
git status --porcelain
```

If the output is non-empty, **refuse to proceed**. Tell the user to commit or stash changes first.

---

## Step 3: Sync with remote

```bash
git fetch origin
git status -sb
```

If the local branch is behind the remote, warn the user and ask whether to pull before proceeding.

---

## Step 4: Fetch latest tag and list unreleased commits

```bash
LATEST_TAG=$(git describe --tags --abbrev=0 2>/dev/null)
git log "$LATEST_TAG"..HEAD --oneline
```

If there are **zero** commits since the last tag, stop — there is nothing to release.

---

## Step 5: Classify commits and suggest bump

The dotfiles uses a `MAJOR.MINOR.PATCH` version scheme (e.g., `7.1.29`). There is no `Version:` header — the version lives only in the git tag.

Read each commit since the last tag. Classify into:

- **Breaking** — backwards-incompatible changes (e.g., major restructuring, dropped support) → bump MAJOR
- **Feature** — new functionality (new packages, new extras, new skills) → bump MINOR
- **Fix** — bug fixes, config corrections → bump PATCH
- **Docs/chore/other** — no bump on their own

Suggest the appropriate semver bump based on the highest-priority category present. Present the suggestion but let the user override.

---

## Step 6: Draft release notes

Group commits by category and draft release notes in this format:

```markdown
## What's changed

### Features
- Description of feature (sha[:7])

### Fixes
- Description of fix (sha[:7])

### Other
- Description (sha[:7])
```

Write clear, user-facing descriptions — do not just copy commit messages verbatim. Rewrite terse or unclear messages into readable prose.

If there are many commits (e.g., 100+), summarize by theme rather than listing every commit individually. Group related changes together and highlight the most significant ones.

---

## Step 7: Pre-release summary

Present a summary:

- Current version (from tag)
- New version (proposed bump)
- Number of commits included
- Elpaca sources cleanliness status (all clean)
- Release notes draft

**Wait for explicit user confirmation before proceeding** (unless `--accept` was passed). Do not take any action until the user says yes. If the user wants changes to the version or notes, revise and re-present.

---

## Step 8: Write lockfile and commit

After the user confirms the version and notes:

1. **Write lockfile**:

   ```bash
   emacsclient -e '(elpaca-extras-write-lock-file-excluding init-master-lockfile-path)'
   ```

   This writes the current elpaca lockfile to `emacs/lockfile.el` in the dotfiles repo.

2. **Commit the lockfile**:

   If the lockfile changed (`git status --porcelain emacs/lockfile.el` is non-empty), stage and commit it:

   ```bash
   git add emacs/lockfile.el
   git commit -m "NEW_VERSION"
   ```

   If the lockfile did **not** change, create an empty commit:

   ```bash
   git commit --allow-empty -m "NEW_VERSION"
   ```

   The commit message is just the version number (e.g., `7.2.0`).

3. **Stop and wait.** Tell the user the lockfile commit is ready and ask them to test the new profile. Explain that once they confirm the profile builds without errors, the release will proceed. If there are problems, the user can make further changes and amend the commit before releasing.

---

## Step 9: Release gate

**Wait for explicit user confirmation** that the profile tested successfully. Do not proceed until the user explicitly says to continue.

If the user made additional dotfiles changes after step 8 (e.g., fixing issues found during testing), check whether those changes have been committed. If uncommitted changes exist, offer to amend the release commit to include them before proceeding.

---

## Step 10: Tag, push, and create release

Only after the user confirms:

1. **Tag**:

   ```bash
   git tag NEW_VERSION
   ```

2. **Push tag**:

   ```bash
   git push origin HEAD --follow-tags
   ```

3. **Create GitHub release**:

   ```bash
   gh release create NEW_VERSION --title "NEW_VERSION" --notes "RELEASE_NOTES"
   ```

   Use a heredoc for the notes to preserve formatting.

---

## Step 11: Post-release verification

After the release is created:

```bash
gh release view NEW_VERSION --repo benthamite/dotfiles
gh api repos/benthamite/dotfiles/tags --jq '.[0].name'
```

Confirm the tag and release are live. Report success or any errors.

---

## Error handling

- **No tags exist**: treat the entire history as unreleased. Use the root commit as the base for comparison.
- **API rate limiting**: if `gh api` returns 403/429, report it and suggest the user wait or authenticate with a higher-rate token.
- **Emacs not running**: if `emacsclient` fails, stop and tell the user to start Emacs first (needed to resolve the elpaca profile).
- **Tag format**: dotfiles tags have no `v` prefix (e.g., `7.1.29`). Preserve this convention.
