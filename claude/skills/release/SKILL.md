---
name: release
description: Release an Emacs package or audit all packages for release readiness. Use `/release <package>` to cut a release, or `/release --audit` to scan all packages.
---

# Package release

Cut a new release for an Emacs package on GitHub (benthamite/), or audit all packages for release readiness.

## Packages

```
annas-archive bib claude-log gptel-plus kelly mullvad org-indent-pixel pangram pdf-tools-pages stafforini.el
```

## Semver normalization rules

All versions MUST be 3-part semver (`MAJOR.MINOR.PATCH`).

- If a tag or header has only 2 parts (e.g., `0.1`), treat it as `0.1.0`.
- When writing a new version (header or tag), always use 3 parts.
- A tag `0.1` and a header `0.1.0` are considered a **match** after normalization.

## Mode selection

Parse `$ARGUMENTS` to determine the mode:

- `--audit` → audit mode
- Anything else (a package name, or empty if inside a package repo) → release mode

---

## Audit mode (`/release --audit`)

Scan all 10 packages **in parallel** using `gh api` exclusively — do NOT clone any repos.

### For each package, run these in a single subagent:

1. **Fetch the latest tag**:

   ```bash
   gh api repos/benthamite/PACKAGE/tags --jq '.[0].name'
   ```

2. **Count commits since that tag**:

   ```bash
   gh api repos/benthamite/PACKAGE/compare/TAG...HEAD --jq '.ahead_by'
   ```

   Also fetch a one-line summary of each commit:

   ```bash
   gh api repos/benthamite/PACKAGE/compare/TAG...HEAD --jq '.commits[] | .commit.message' | head -20
   ```

3. **Read the `Version:` header** from the main `.el` file:

   ```bash
   gh api repos/benthamite/PACKAGE/contents/PACKAGE.el --jq '.content' | base64 -d | grep -m1 '^;; Version:'
   ```

   For `stafforini.el`, the file is `stafforini.el`.

4. **Normalize and compare**: apply the semver normalization rules above to both the tag and the header version. Flag any mismatch.

### Output

Present a summary table:

```
| Package           | Latest tag | Header version | Match | Unreleased | Summary                     |
|-------------------|------------|----------------|-------|------------|-----------------------------|
| annas-archive     | 0.1.0      | 0.1.0          | yes   | 3          | Add search, fix pagination… |
```

Then group results into three sections:

1. **Version mismatches** — tag and header disagree (even after normalization). These need manual resolution before releasing.
2. **Ready to release** — versions match, unreleased commit count > 0.
3. **Up to date** — versions match, unreleased commit count = 0.

---

## Release mode (`/release [package-name]`)

### 1. Determine the package

- If `$ARGUMENTS` names a package, use that.
- If `$ARGUMENTS` is empty, infer from the current working directory (look for a `.el` file matching the repo name).
- If neither works, ask the user.

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
git log "$LATEST_TAG"..HEAD --oneline
```

If there are **zero** commits since the last tag, stop — there is nothing to release.

### 6. Check version header vs tag

Read the `Version:` header from the main `.el` file. Normalize both to 3-part semver. If they do not match:

- Report the mismatch clearly.
- **Do not proceed with the release.** Tell the user the header and tag must agree on the current version before bumping to a new one. Offer to fix the header to match the tag if appropriate.

### 7. Classify commits and suggest bump

Read each commit since the last tag. Classify into:

- **Breaking** — backwards-incompatible changes → bump MAJOR
- **Feature** — new functionality → bump MINOR
- **Fix** — bug fixes → bump PATCH
- **Docs/chore/other** — no bump on their own

Suggest the appropriate semver bump based on the highest-priority category present. Present the suggestion but let the user override.

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
- New version (proposed bump)
- Release notes draft
- Actions that will be taken: update header, commit, tag, push, create GitHub release

**Wait for explicit user confirmation before proceeding.** Do not take any public/irreversible action until the user says yes. If the user wants changes, revise and re-present.

### 10. Execute the release

Only after confirmation:

1. **Update the `Version:` header** in the main `.el` file to the new version:

   ```bash
   # Use the Edit tool — do not use sed
   ```

2. **Commit**:

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

- **No tags exist**: treat the entire history as unreleased. Use the root commit as the base for comparison.
- **API rate limiting**: if `gh api` returns 403/429, report it and suggest the user wait or authenticate with a higher-rate token.
- **Tag format variations**: tags may or may not have a `v` prefix. Normalize by stripping `v` when comparing, but preserve the existing convention when creating new tags (match whatever the repo already uses).
- **Multiple `.el` files**: use the one whose name matches the package/repo name.
