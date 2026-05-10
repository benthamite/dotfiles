---
name: release
description: Release one of the user's Emacs packages or audit package release readiness. Use when the user says `/release <package>`, asks to bump/tag/create a GitHub release, wants release notes for a benthamite Emacs package, or runs `/release --audit`; do not use for dotfiles releases.
---

# Package release

Cut a new release for an Emacs package on GitHub (benthamite/), or audit all packages for release readiness.

Use `release-dotfiles` instead for the `dotfiles` repo itself. Do not use this skill for README generation, package registration, or generic release-note drafting unless the user is releasing one of these Emacs packages.

## Packages

Do NOT hard-code a package list. Instead, read the **listed packages** (level-2 `=name=` headings, excluding the "Unlisted packages" section) from:

```
~/My Drive/notes/pablos-miscellany/my-emacs-packages.org
```

Parse with:

```bash
awk '
  /^\*\* Unlisted packages([[:space:]]|$)/ { stop = 1 }
  stop { next }
  /^\*\* =[^=]+=$/ {
    name = $0
    sub(/^\*\* =/, "", name)
    sub(/=$/, "", name)
    print name
  }
' "$HOME/My Drive/notes/pablos-miscellany/my-emacs-packages.org"
```

## Semver normalization rules

When comparing versions, normalize to 3-part semver (`MAJOR.MINOR.PATCH`):

- Strip a leading `v` from tags for comparison.
- If a tag or header has only 2 parts (e.g., `0.1`), treat it as `0.1.0` for comparison purposes.
- A tag `0.1` and a header `0.1.0` are considered a **match** after normalization.
- Compare numeric components, not strings (`0.10.0` is greater than `0.2.0`).

When writing a new version (header or tag), **match the format the repo already uses**. If the most recent tag is 2-part (e.g., `0.3`), create a 2-part tag. If 3-part (e.g., `0.1.0`), create a 3-part tag. The header format should likewise match the repo's convention.

## Mode selection

Parse `$ARGUMENTS` to determine the mode:

- `--audit` → audit mode
- Anything else (a package name, or empty if inside a package repo) → release mode

Strip recognized flags such as `--audit` and `--accept` before interpreting a release-mode package name. If any unrecognized flag or more than one non-flag argument remains, stop and ask for clarification rather than guessing the package.

If `--accept` is present in `$ARGUMENTS`, skip the confirmation gate (step 9) and proceed directly to executing the release. All other steps (including presenting the summary and release notes) still run normally — `--accept` only removes the wait for user confirmation.

---

## Audit mode (`/release --audit`)

Scan all listed packages **in parallel** where the current agent/tooling supports it, using `gh api` exclusively — do NOT clone any repos. Do not create tags, releases, PRs, issues, or other externally visible changes in audit mode.

### For each package, run these in a single subagent:

1. **Fetch the latest tag**:

   ```bash
   gh api repos/benthamite/PACKAGE/tags --jq '.[0].name'
   ```

   If this returns nothing, record the latest tag as `(none)`, skip the compare API, and treat the full commit history as unreleased.

2. **Count commits since that tag**:

   ```bash
   gh api repos/benthamite/PACKAGE/compare/TAG...HEAD --jq '.ahead_by'
   ```

   Also fetch a one-line summary of each commit:

   ```bash
   gh api repos/benthamite/PACKAGE/compare/TAG...HEAD --jq '.commits[] | .commit.message' | head -20
   ```

   For a repo with no tags, use commits endpoint pagination instead:

   ```bash
   gh api repos/benthamite/PACKAGE/commits --paginate --jq '.[].sha' | wc -l
   gh api repos/benthamite/PACKAGE/commits --per-page 20 --jq '.[].commit.message'
   ```

3. **Read the `Version:` header** from the main `.el` file:

   ```bash
   gh api repos/benthamite/PACKAGE/contents/PACKAGE.el --jq '.content' | base64 -d | grep -m1 '^;; Version:'
   ```

4. **Normalize and compare**: apply the semver normalization rules above to both the tag and the header version. Flag any mismatch. If there is no tag, report `n/a` for match status and include the package with release-ready results if it has a version header.

### Output

Present a summary table:

```
| Package           | Latest tag | Header version | Match | Unreleased | Summary                     |
|-------------------|------------|----------------|-------|------------|-----------------------------|
| annas-archive     | 0.1.0      | 0.1.0          | yes   | 3          | Add search, fix pagination… |
```

Then group results into four sections:

1. **Version mismatches (header behind tag)** — header version is lower than tag after normalization. These are genuinely broken and need manual resolution.
2. **Ready to release (pre-bumped)** — header version is ahead of tag. The header was already bumped in anticipation of the next release. These can be released using the header version directly (the release skill handles this automatically). Note: even if the compare API reports 0 unreleased commits, the header bump itself indicates intent to release — include these here, not in "up to date".
3. **Ready to release** — versions match, unreleased commit count > 0, or the package has no tags but does have a `Version:` header.
4. **Up to date** — versions match, unreleased commit count = 0.

---

## Release mode (`/release [package-name]`)

### 1. Determine the package

- If `$ARGUMENTS` names a package, use that.
- If `$ARGUMENTS` is empty, infer from the current working directory (look for a `.el` file matching the repo name).
- If neither works, ask the user.
- Ignore recognized flags such as `--accept` when determining the package.

### 2. Verify local repo

Confirm we are inside the package's local git repo:

```bash
git remote get-url origin
```

The remote must match `benthamite/PACKAGE`. If not, stop and tell the user.

Do NOT clone the repo — the user must already be in it.

### 3. Check working tree

```bash
git status --porcelain
```

If the output is non-empty, **refuse to proceed**. Tell the user to commit or stash changes first.

### 4. Sync with remote

```bash
git fetch origin
git status -sb
```

If the local branch is behind the remote, warn the user and ask whether to pull before proceeding.

### 5. Fetch latest tag and list unreleased commits

```bash
LATEST_TAG=$(git describe --tags --abbrev=0 2>/dev/null)
if [ -n "$LATEST_TAG" ]; then
  git log "$LATEST_TAG"..HEAD --oneline
else
  git log --oneline
fi
```

Record the number of commits shown. Do not stop yet when the count is zero, because a pre-bumped `Version:` header can still indicate that the intended release tag has not been created.

### 6. Check version header vs tag

Read the `Version:` header from the main `.el` file. Normalize both to 3-part semver and compare:

- **No tag exists**: use the header version as the initial release version, skip step 7, and skip the version-bump commit in step 10 unless the user explicitly asks for a different initial version.
- **Header == tag**: normal flow. Proceed to step 7 to classify commits and suggest a bump.
- **Header > tag** (pre-bumped): the header was already bumped in anticipation of this release. Use the header version as the release version. **Skip step 7** (the user already chose the version). Proceed to step 8 to draft release notes.
- **Header < tag**: this is genuinely broken — the header is behind the tag. **Stop and flag the issue.** Do not proceed. Offer to update the header to match the tag.

If the header matches the tag and there are **zero** commits since the tag, stop — there is nothing to release.

### 7. Classify commits and suggest bump

*(Skip this step if the header was pre-bumped in step 6.)*

Read each commit since the last tag. Classify into:

- **Breaking** — backwards-incompatible changes → bump MAJOR
- **Feature** — new functionality → bump MINOR
- **Fix** — bug fixes → bump PATCH
- **Docs/chore/other** — no bump on their own

Suggest the appropriate semver bump based on the highest-priority category present. If `--accept` was passed, use this suggested bump automatically; otherwise present the suggestion and let the user override in step 9.

### 8. Draft release notes

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

### 9. Confirmation gate

Present a full summary:

- Package name
- Current version (from tag)
- New version (proposed bump, or pre-bumped header version)
- Whether the header was pre-bumped (if so, note that the header update and version-bump commit will be skipped)
- Release notes draft
- Actions that will be taken: update header + commit (unless pre-bumped), tag, push, create GitHub release

**Wait for explicit user confirmation before proceeding** (unless `--accept` was passed, in which case skip straight to step 10). Do not take any public/irreversible action until the user says yes. If the user wants changes, revise and re-present.

### 10. Execute the release

Only after confirmation, or after step 9 has been skipped by `--accept`:

1. **Update the `Version:` header** in the main `.el` file to the new version (skip if pre-bumped):

   ```bash
   # Use apply_patch in Codex, or the Edit tool in Claude Code. Do not rewrite with sed/perl.
   ```

2. **Commit** (skip if pre-bumped — the header is already at the right version):

   ```bash
   git add PACKAGE.el
   git commit -m "Bump version to NEW_VERSION"
   ```

3. **Tag**:

   ```bash
   git tag NEW_VERSION
   ```

4. **Push commit and tag**:

   ```bash
   git push origin HEAD --follow-tags
   ```

5. **Create GitHub release**:

   ```bash
   gh release create NEW_VERSION --title "NEW_VERSION" --notes "RELEASE_NOTES"
   ```

   Use a heredoc for the notes to preserve formatting.

### 11. Post-release verification

After the release is created:

```bash
gh release view NEW_VERSION --repo benthamite/PACKAGE
gh api repos/benthamite/PACKAGE/tags --jq '.[0].name'
```

Confirm the tag and release are live. Report success or any errors.

---

## Error handling

- **No tags exist**: treat the entire history as unreleased. In audit mode, report latest tag as `(none)` and match as `n/a`; in release mode, use the `Version:` header as the initial release version and skip the version-bump commit unless the user asks for a different initial version.
- **API rate limiting**: if `gh api` returns 403/429, report it and suggest the user wait or authenticate with a higher-rate token.
- **Tag format variations**: tags may or may not have a `v` prefix. Normalize by stripping `v` when comparing, but preserve the existing convention when creating new tags (match whatever the repo already uses).
- **Multiple `.el` files**: use the one whose name matches the package/repo name.
