---
name: dotfiles-context
description: Dotfiles and Emacs package routing context. Use when working in ~/My Drive/dotfiles, editing emacs/config.org or emacs/extras, choosing between the dotfiles repo and an elpaca source clone, or applying dotfiles documentation and commit conventions.
---

# Purpose

Use this context to keep dotfiles work in the right clone and verification path. It answers five questions: where to edit files, how to resolve the active elpaca profile, what to do after editing `emacs/config.org`, how to keep upstream package PRs active in Elpaca until merge, and which documentation and commit rules apply.

If changing Emacs Lisp code, also use `elisp-conventions`; that skill owns style, batch testing, rebuild/reload rules, and live Emacs verification.

# Edit locations

All elpaca-managed packages, whether authored by the user or third-party, are cloned to `~/.config/emacs-profiles/<profile>/elpaca/sources/<package>/`.

- For every package except `dotfiles`, the elpaca source clone is the canonical working copy. Edit it directly and commit there; push only when the task calls for it. There is no separate Drive-side master.
- The `dotfiles` package is the single exception. Its canonical source is `~/My Drive/dotfiles/`, and the elpaca clone at `elpaca/sources/dotfiles/` is a read-only mirror. Do not edit the mirror; a PreToolUse hook (`block-elpaca-dotfiles-edit.sh`) blocks direct edits there.

After a `dotfiles` commit, the local git `post-commit` and `post-rewrite` hooks sync the elpaca mirror and trigger `elpaca-rebuild` plus `elpaca-extras-reload` for changed extras packages. Pushing is a separate sharing step, not what makes the local elpaca clone current.

If you need to rebuild manually after the relevant working tree changes are committed, use:

```bash
emacsclient -e '(progn (elpaca-rebuild (quote dotfiles) t) (elpaca-wait) (elpaca-extras-reload (quote dotfiles)))'
```

# Elpaca profile

The current elpaca profile name is stored in the Elisp variable `init-current-profile`. Query it with:

```bash
emacsclient -e 'init-current-profile'
```

Always use this to resolve the active profile path (`~/.config/emacs-profiles/<profile>/elpaca/`) rather than hardcoding a profile name, since it changes over time.

# Workflow

1. Identify the layout before editing.
   - Files under `~/My Drive/dotfiles/` are canonical dotfiles files.
   - Files under `~/My Drive/dotfiles/emacs/extras/` are dotfiles extras; edit the canonical dotfiles file and use `elisp-conventions` for batch testing, commit-time sync, and live Emacs verification.
   - Standalone Emacs packages live in the active profile's `elpaca/sources/<package>/` clone; query the active profile instead of guessing the path.
2. If editing `emacs/config.org`, tangle it with the profile-aware command below.
3. Update directly required documentation.
   - Significant changes under `claude/` require `claude/README.org`.
   - Codex integration changes under `codex/` require `codex/README.org`.
   - Emacs extras package changes should follow the documentation rules in `elisp-conventions`.
4. Commit each logical change with the format below, staging only files that belong to that change.

# Upstream package PRs

When you create or update an upstream PR for a non-`dotfiles` Elpaca package,
make the local profile use that PR branch as part of the normal PR creation
workflow. Do this after the PR URL exists and before you report the PR as done.

1. Query or record the PR metadata:

   ```bash
   gh pr view --json url,state,merged,baseRepository,baseRefName,headRepository,headRefName
   ```

2. If the PR is open, edit `~/My Drive/dotfiles/emacs/config.org` and update
   the package's Elpaca recipe to point at the PR head repo and head branch.
3. Add the exact lifecycle marker comment, preferably on the temporary
   `:branch` line:

   ```elisp
   ; awaiting PR merge: https://github.com/OWNER/REPO/pull/NUMBER
   ```

4. Tangle with the profile-aware command below.
5. Review `git diff -- emacs/config.org` and commit the pin separately:

   ```bash
   git add emacs/config.org
   git commit -m "emacs: pin PACKAGE to pr branch"
   ```

If the PR is merged, do not pin; use the upstream/base recipe. If the PR is
closed but unmerged, stop and ask the user before changing `config.org`.

# Tangling `config.org`

After editing `emacs/config.org`, always tangle it to the current profile using:

```bash
emacsclient -e '(init-build-profile (file-name-directory user-init-file))'
```

Do NOT use `org-babel-tangle-file` directly — it doesn't know about the profile system and may write to the wrong location.

# Version control

- Commit message format: `<scope>: <description>`, where `<scope>` is a short identifier for the area of change (package name, tool, or subsystem) and `<description>` is a lowercase imperative phrase with no trailing period. Examples: `org-roam-extras: handle nil db version in upgrade check`, `karabiner: swap ¿ and ¡ key mappings in k-mode`, `claude: add emacs-freeze skill for diagnosing frozen Emacs`.

# Verification

Before calling the task done, verify the workflow-specific effect:

- For `emacs/config.org`, confirm the profile-aware tangle command completed.
- For Emacs Lisp changes, follow `elisp-conventions` verification rather than only checking that the file saved.
- For paired Claude/Codex configuration or skill changes, run `bin/ai-config-sync audit`.
- Inspect `git status --short` so unrelated concurrent edits are not staged or committed.
