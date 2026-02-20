---
name: dotfiles-context
description: Dotfiles worktree structure and Emacs extras testing instructions. Use when working with dotfiles, Emacs extras, elpaca repos, or testing Elisp packages.
user-invocable: false
---

# Dotfiles worktree structure

The dotfiles repo has two working trees on the same machine:

- **Dropbox/dotfiles** (`~/Library/CloudStorage/Dropbox/dotfiles/`): the primary location for non-Elisp files, including `emacs/config.org`.
- **Elpaca repos** (`~/.config/emacs-profiles/<profile>/elpaca/repos/dotfiles/`): the location Emacs loads Elisp from, including the `emacs/extras/*.el` files.

Both point to the same Git repo, so changes only need to be committed once in the appropriate working tree. Edit each file in its canonical location: `config.org` in Dropbox/dotfiles, Elisp extras in the elpaca repos directory.

# Elpaca profile

The current elpaca profile name is stored in the Elisp variable `init-current-profile`. Query it with:

```bash
emacsclient -e 'init-current-profile'
```

Always use this to resolve the active profile path (`~/.config/emacs-profiles/<profile>/elpaca/`) rather than hardcoding a profile name, since it changes over time.

# Testing Emacs extras

To test loading Emacs extras in batch mode with all dependencies:

```bash
ELPACA=/Users/pablostafforini/.config/emacs-profiles/$(emacsclient -e 'init-current-profile' | tr -d '"')/elpaca
emacs --batch \
  -L "$PWD/emacs/extras" \
  -L "$ELPACA/builds/claude-code" \
  -L "$ELPACA/builds/doom-modeline" \
  -L "$ELPACA/builds/compat" \
  -L "$ELPACA/builds/nerd-icons" \
  -L "$ELPACA/builds/shrink-path" \
  -L "$ELPACA/builds/paths" \
  -L "$ELPACA/builds/eat" \
  -L "$ELPACA/builds/inheritenv" \
  -L "$ELPACA/builds/dash" \
  -L "$ELPACA/builds/s" \
  -L "$ELPACA/builds/f" \
  -L "$ELPACA/builds/transient" \
  -L "$ELPACA/builds/cond-let" \
  --eval "(setq load-prefer-newer t)" \
  --eval "(require 'claude-code-extras)" \
  --eval "(require 'doom-modeline-extras)" \
  --eval "(message \"Result: %S\" (YOUR-TEST-EXPRESSION-HERE))" \
  2>&1
```

**Important:** `$PWD/emacs/extras` loads the extras from the current working tree (where you edit files). `$ELPACA/builds/*` loads pre-built dependencies from a (possibly different) elpaca profile. Do not use `$ELPACA/repos/dotfiles/emacs/extras` for the extras under test—that may point to a stale copy in another profile.

The `load-prefer-newer t` setting is important because elpaca may have outdated `.elc` files that don't reflect recent source changes.

If a dependency is missing, find it with `ls $ELPACA/builds/ | grep NAME` and add another `-L` flag.

For interactive testing against the running Emacs session, use `emacsclient -e '(EXPRESSION)'`.
