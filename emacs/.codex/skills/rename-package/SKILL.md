---
name: rename-package
description: Rename an Emacs package end to end, especially elpaca-managed standalone packages. Use when the user asks to rename a package, local files, feature/symbol prefixes, GitHub repository, elpaca source directory, or references in dotfiles/system configuration.
---

# Rename Emacs Package

Rename an Emacs package across its source repo, GitHub repository, active
elpaca profile, dotfiles references, live Emacs build, and local documentation.

This workflow is for a real package rename, not a casual prose cleanup. Prefer
exact package/API references over broad natural-language replacements.

## Safety boundaries

- Check `git status --short --branch` in every repo before editing. Preserve
  unrelated dirty or staged changes, and stage only the rename files you own.
- Do not rename old Emacs profile references unless the user asks for that.
  Previous profiles may correctly refer to the old package name.
- Renaming a GitHub repository is externally visible. Do it only when the user
  explicitly asks to rename the repository, or ask for confirmation first.
- Use Trash for stale generated build directories. Do not destructively remove
  elpaca builds or source directories.
- Avoid broad replacements for ordinary words such as "agent", "agents",
  "subagents", `AGENTS.md`, or project docs named `agents/`. Rename package
  identifiers, feature names, repository paths, and config references.

## 1. Identify the package

Resolve the active profile and source path instead of guessing:

```bash
emacsclient -e 'init-current-profile'
```

For standalone packages, the canonical source is:

```text
~/.config/emacs-profiles/<profile>/elpaca/sources/<old-name>/
```

Inspect:

```bash
git -C "$SOURCE" status --short --branch
git -C "$SOURCE" remote -v
gh repo view OWNER/OLD --json nameWithOwner,url,defaultBranchRef
gh repo view OWNER/NEW --json nameWithOwner,url,defaultBranchRef
git -C "$SOURCE" ls-files
```

If `OWNER/NEW` already exists, stop and ask how to proceed.

## 2. Plan exact replacements

Build a replacement map before editing:

- Package/file names: `old.el` -> `new.el`, `old-sub.el` -> `new-sub.el`.
- Feature names: `(provide 'old)` and `(require 'old)` -> `new`.
- Public symbols and custom groups: `old-*` -> `new-*`.
- Environment variables or generated filenames if they encode the package
  name, for example `OLD_STATUS_DIR` -> `NEW_STATUS_DIR`.
- Repository URLs: `OWNER/old` -> `OWNER/new`.
- Active-profile paths: `.../sources/old` -> `.../sources/new`.

Be conservative with natural language. For example, when renaming package
`agents` to `agent`, change `agents-codex-handoff` but not unrelated text about
`subagents` or files named `AGENTS.md`.

## 3. Rename package source

In the package repo:

```bash
git mv old.el new.el
git mv old-sub.el new-sub.el
git mv old.texi new.texi
git mv test/old-test.el test/new-test.el
```

Then perform scoped textual replacements over tracked files. Re-read the diff
and search for stale identifiers:

```bash
rg -n --hidden -S 'old|OLD|Owner/old|OWNER/OLD' . -g '!**/.git/**'
git diff --stat
git diff -- new.el
```

If a replacement touched unrelated prose or docs, fix it before testing.

## 4. Test from source

Because shell rewrites can bypass live elpaca rebuild hooks, first run tests
directly against the renamed source and existing dependency builds:

```bash
emacs --batch \
  --eval '(dolist (dir (file-expand-wildcards "/PROFILE/elpaca/builds/*/")) (add-to-list (quote load-path) dir))' \
  --eval '(push "/PROFILE/elpaca/sources/new" load-path)' \
  --eval '(push "/PROFILE/elpaca/sources/new/test" load-path)' \
  --eval '(require (quote new-test))' \
  --eval '(ert-run-tests-batch-and-exit)'
```

For packages with multiple test files, require each renamed test feature. Fix
failures at the renamed API, not by adding compatibility aliases unless the
user explicitly wants a transition layer.

Commit the package source rename:

```bash
git add -A
git commit -m "new: rename package"
```

## 5. Rename GitHub and local source directory

Only after the package commit exists:

```bash
gh repo rename NEW --repo OWNER/OLD --yes
git -C "$SOURCE" remote set-url origin https://github.com/OWNER/NEW.git
mv "$SOURCE" "$ELPACA/sources/NEW"
git -C "$ELPACA/sources/NEW" push origin main
```

Record the pushed SHA:

```bash
git -C "$ELPACA/sources/NEW" rev-parse HEAD
```

If pushing includes commits that were already ahead of origin before your
rename, mention that explicitly in the final report.

## 6. Update dotfiles and system references

Search outside old profile trees unless the user requested historical updates:

```bash
rg -n --hidden -S 'old|OLD|OWNER/old|sources/old' \
  "$HOME/My Drive/dotfiles" \
  -g '!**/.git/**'
```

Update only real package references, usually:

- `emacs/config.org` use-package form, requires, keybindings, custom vars.
- `emacs/lockfile.el` package name, id, repo, and pushed commit SHA.
- Extras or modeline integrations that declare or call package functions.
- Package-specific README/skill references in Claude/Codex docs.
- `ai-config-sync.json` notes if instruction, skill, hook, or wrapper
  registration text changed.
- Current-profile project trust entries such as `sources/old` -> `sources/new`.

Do not update references in older `~/.config/emacs-profiles/<old-profile>/...`
unless requested. It is usually correct for old profiles to keep old package
paths.

When editing `codex/config.toml`, separate runtime trust churn from the package
rename. Commit only the relevant `sources/new` entry, and preserve unrelated
local trust entries as unstaged changes.

## 7. Rebuild and verify active Emacs

Tangle dotfiles config through the profile-aware command:

```bash
emacsclient -e '(init-build-profile (file-name-directory user-init-file))'
```

Ask elpaca to build and load the renamed package:

```elisp
(progn
  (elpaca (new :host github :repo "OWNER/new")
    (require 'new)
    (require 'new-sub))
  (elpaca-wait))
```

Use `emacsclient -e` for the expression above, adjusting subfeatures.

Move the stale generated old build to Trash after the new build loads:

```bash
trash ~/.config/emacs-profiles/<profile>/elpaca/builds/old
```

Verify:

```bash
~/My\ Drive/dotfiles/claude/bin/batch-test.sh new
~/My\ Drive/dotfiles/claude/bin/batch-test.sh changed-extra
bin/ai-config-sync audit
emacsclient -e '(list (locate-library "new") (locate-library "old") (fboundp (quote new-menu)))'
```

Expected result: `new` resolves, old generated package no longer resolves, and
the relevant commands/features are available.

## 8. Commit dotfiles references

Commit only the rename-owned dotfiles files:

```bash
git add ai-config-sync.json emacs/config.org emacs/lockfile.el ...
git commit -m "new: update package references"
```

If the dotfiles post-commit hook syncs the elpaca dotfiles mirror, confirm the
mirror no longer contains stale package references except intentionally ignored
old-profile entries.

## 9. Post-push CI

After pushing the package repo, check GitHub Actions:

```bash
ci-after-push --no-push --commit "$(git -C "$ELPACA/sources/NEW" rev-parse HEAD)"
gh run list --repo OWNER/NEW --commit SHA --event push --limit 10
```

If no run appears, report that no push workflow was triggered. Do not leave a
polling helper running indefinitely.

## Final report

Include:

- Final package name, repository URL, local source path, and pushed package SHA.
- Dotfiles commit SHA if committed.
- Verification commands and results.
- CI result or explicit note that no workflow run appeared.
- Any intentionally preserved dirty changes or ignored old-profile references.
