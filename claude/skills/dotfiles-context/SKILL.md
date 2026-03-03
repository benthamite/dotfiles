---
name: dotfiles-context
description: Dotfiles worktree structure and extras documentation instructions. Use when working with dotfiles or Emacs extras doc files.
user-invocable: false
---

# Dotfiles repository

The canonical location for all dotfiles, including Elisp extras, is **Google Drive/dotfiles** (`~/My Drive/dotfiles/`). All edits and commits should be made here.

Elpaca keeps a separate clone at `~/.config/emacs-profiles/<profile>/elpaca/repos/dotfiles/` for loading Elisp, but do **not** commit there. The elpaca clone syncs from the Google Drive repo automatically.

# Elpaca profile

The current elpaca profile name is stored in the Elisp variable `init-current-profile`. Query it with:

```bash
emacsclient -e 'init-current-profile'
```

Always use this to resolve the active profile path (`~/.config/emacs-profiles/<profile>/elpaca/`) rather than hardcoding a profile name, since it changes over time.

# Making changes

If you make a change to an extras Elisp file, please also update the corresponding org file in the `doc` subdirectory. For example, if you modify `emacs/extras/claude-code-extras.el`, also update `emacs/extras/doc/claude-code-extras.org`.

# Testing Emacs extras

To test loading Emacs extras in batch mode with all dependencies:

```bash
ELPACA=/Users/pablostafforini/.config/emacs-profiles/$(emacsclient -e 'init-current-profile' | tr -d '"')/elpaca
emacs --batch \
  --eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))" \
  --eval "(push \"$PWD/emacs/extras\" load-path)" \
  --eval "(require 'YOUR-PACKAGE)" \
  --eval "(message \"Result: %S\" (YOUR-TEST-EXPRESSION-HERE))" \
  2>&1
```

**How the load-path is set up:**

1. All elpaca build directories are prepended to `load-path` (overriding system packages like `transient` with elpaca's versions).
2. `$PWD/emacs/extras` is pushed to the very front, so extras under test always load from the working tree source `.el` files, not stale `.elc` from elpaca builds.

**Do not use `load-prefer-newer`** — it causes version mismatches between elpaca dependencies (e.g. magit's `.elc` expecting a transient API that differs from transient's `.el` source). The load-path ordering above already ensures the correct files are loaded.

For interactive testing against the running Emacs session, use `emacsclient -e '(EXPRESSION)'`.

# Version control

- Commit message format: `<scope>: <description>`, where `<scope>` is a short identifier for the area of change (package name, tool, or subsystem) and `<description>` is a lowercase imperative phrase with no trailing period. Examples: `org-roam-extras: handle nil db version in upgrade check`, `karabiner: swap ¿ and ¡ key mappings in k-mode`, `claude: add emacs-freeze skill for diagnosing frozen Emacs`.
