---
name: dotfiles-context
description: Dotfiles worktree structure, elpaca clone rules, batch-test entrypoints, and commit-message conventions. Use when working in ~/My Drive/dotfiles or related Emacs files.
---

# Where external Emacs packages live

All elpaca-managed packages — whether authored by the user or third-party — are
cloned to `~/.config/emacs-profiles/<profile>/elpaca/sources/<package>/`. For
every package *except* `dotfiles`, that clone is the canonical working copy:
edit it directly, commit there, push upstream. There is no separate Drive-side
master.

The `dotfiles` package is the single exception: its canonical source is
`~/My Drive/dotfiles/`, and the elpaca clone at
`~/.config/emacs-profiles/<profile>/elpaca/sources/dotfiles/` is a read-only
mirror that syncs via git commits. Do **not** commit or edit that mirror.

If you need to rebuild the installed package after editing dotfiles Emacs Lisp,
use:

```bash
emacsclient -e '(progn (elpaca-rebuild (quote dotfiles) t) (elpaca-wait) (elpaca-extras-reload (quote dotfiles)))'
```

Never use `load-file` as a substitute for `elpaca-rebuild`.

# Elpaca profile

Resolve the active profile dynamically:

```bash
emacsclient -e 'init-current-profile'
```

Use that value to derive `~/.config/emacs-profiles/<profile>/elpaca/` instead
of hardcoding a profile name.

# Tangling `emacs/config.org`

After editing `emacs/config.org`, tangle it with:

```bash
emacsclient -e '(init-build-profile (file-name-directory user-init-file))'
```

Do not use `org-babel-tangle-file` directly.

# Testing and verification

See `$elisp-conventions` for the batch-testing recipe. After changing Emacs
behavior, prefer a targeted `emacsclient -e` verification of the actual user
path before declaring the work done.

# Version control

- Commit message format: `<scope>: <description>`.
- If you change files under `claude/`, update `claude/README.org`.
- If you change Codex integration files such as `AGENTS.md`, `.codex/`, or
  `codex/`, update `codex/README.org`.
