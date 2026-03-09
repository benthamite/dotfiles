---
name: release-dotfiles
description: Release a new version of the dotfiles repo after verifying all package repos are clean. Use `/release-dotfiles` to cut a release.
---

# Dotfiles release

Cut a new release for the dotfiles repo (benthamite/dotfiles) on GitHub. Before releasing, verify that every personal package repo has a clean working tree in its local elpaca clone.

## Packages source

Read **all packages** (listed + unlisted) from:

```
~/My Drive/notes/pablos-miscellany/my-emacs-packages.org
```

Parse with:

```bash
grep -E '^\*{2,3} =' ~/My\ Drive/notes/pablos-miscellany/my-emacs-packages.org | sed 's/^\*\{2,3\} =//;s/=$//'
```

## Elpaca profile resolution

Resolve the active elpaca profile dynamically — never hard-code a profile name:

```bash
PROFILE=$(emacsclient -e 'init-current-profile' | tr -d '"')
ELPACA_REPOS=~/.config/emacs-profiles/$PROFILE/elpaca/repos
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

## Step 1: Check all package repos are clean

For each package parsed from the org file, check its local elpaca clone for a clean working tree.

### Finding the elpaca directory

For each package name, look up its directory in `$ELPACA_REPOS`:

1. Try `$ELPACA_REPOS/<name>` (exact match).
2. If not found, try stripping the `.el` suffix: `$ELPACA_REPOS/<name-without-.el>` (handles `stafforini.el` → `stafforini`, `tangodb.el` → `tangodb`).
3. If still not found, **warn** but do not abort — the package may not be installed in the current profile.

### Cleanliness check

For each found directory:

```bash
git -C "$ELPACA_REPOS/<dir>" status --porcelain
```

If the output is non-empty, the repo is **dirty**.

### Output

Present a summary table:

```
| Package          | Status |
|------------------|--------|
| annas-archive    | clean  |
| bib              | dirty  |
| ledger-prices    | skipped (not in profile) |
```

If **any** repo is dirty, **abort the release**. List the dirty repos and tell the user to commit or stash changes in each before retrying.

If all repos are clean (or skipped), proceed to step 2.

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

## Step 7: Confirmation gate

Present a full summary:

- Current version (from tag)
- New version (proposed bump)
- Number of commits included
- Package cleanliness status (all clean / N skipped)
- Release notes draft
- Actions that will be taken: tag, push, create GitHub release

**Wait for explicit user confirmation before proceeding** (unless `--accept` was passed, in which case skip straight to step 8). Do not take any public/irreversible action until the user says yes. If the user wants changes, revise and re-present.

---

## Step 8: Execute the release

Only after confirmation:

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

## Step 9: Post-release verification

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
