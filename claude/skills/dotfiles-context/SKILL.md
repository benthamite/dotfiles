---
name: dotfiles-context
description: Dotfiles worktree structure and documentation instructions for 'extras-' modules. Use when working with dotfiles or Emacs files.
user-invocable: false
---

# Dotfiles repository

Elpaca keeps a separate clone at `~/.config/emacs-profiles/<profile>/elpaca/sources/dotfiles/` for loading Elisp, but do **not** commit there. The elpaca clone syncs from the Google Drive repo automatically.

# Elpaca profile

The current elpaca profile name is stored in the Elisp variable `init-current-profile`. Query it with:

```bash
emacsclient -e 'init-current-profile'
```

Always use this to resolve the active profile path (`~/.config/emacs-profiles/<profile>/elpaca/`) rather than hardcoding a profile name, since it changes over time.

# Tangling config.org

After editing `emacs/config.org`, always tangle it to the current profile using:

```bash
emacsclient -e '(init-build-profile (file-name-directory user-init-file))'
```

Do NOT use `org-babel-tangle-file` directly — it doesn't know about the profile system and may write to the wrong location.

# Testing Emacs extras

To test loading Emacs extras in batch mode with all dependencies:

```bash
ELPACA=/Users/pablostafforini/.config/emacs-profiles/$(emacsclient -e 'init-current-profile' | tr -d '"')/elpaca
# Remove stale .elc so the source .el is loaded (load-prefer-newer is off)
rm -f "$PWD/emacs/extras/YOUR-PACKAGE.elc"
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

**Stale `.elc` files** — Elpaca does not byte-compile in the source directory, but other processes (manual `byte-compile-file`, prior Claude sessions) occasionally leave `.elc` files in `emacs/extras/`. Since `load-prefer-newer` is off, a co-located `.elc` silently shadows the `.el` source. The `rm -f` step above prevents this. These files are gitignored.

For interactive testing against the running Emacs session, use `emacsclient -e '(EXPRESSION)'`.

# Version control

- Commit message format: `<scope>: <description>`, where `<scope>` is a short identifier for the area of change (package name, tool, or subsystem) and `<description>` is a lowercase imperative phrase with no trailing period. Examples: `org-roam-extras: handle nil db version in upgrade check`, `karabiner: swap ¿ and ¡ key mappings in k-mode`, `claude: add emacs-freeze skill for diagnosing frozen Emacs`.
