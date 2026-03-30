---
name: dotfiles-context
description: Dotfiles worktree structure and documentation instructions for 'extras-' modules. Use when working with dotfiles or Emacs files.
user-invocable: false
---

# Dotfiles repository

Elpaca keeps a separate clone at `~/.config/emacs-profiles/<profile>/elpaca/sources/dotfiles/` for loading Elisp, but do **not** commit there. The elpaca clone syncs from the Google Drive repo via git commits.

**A PreToolUse hook (`block-elpaca-dotfiles-edit.sh`) blocks direct edits to `elpaca/sources/dotfiles/`.** Always edit the canonical files in `~/My Drive/dotfiles/` instead. After editing, commit and push in the dotfiles repo so the elpaca clone picks up the change on next sync. Only then will `elpaca-rebuild` (triggered by the `load-elisp-after-edit.sh` PostToolUse hook) compile the new code.

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

See the `elisp-conventions` skill for the batch testing recipe, load-path setup, and stale `.elc` handling. For interactive testing, use `emacsclient -e '(EXPRESSION)'`.

# Version control

- Commit message format: `<scope>: <description>`, where `<scope>` is a short identifier for the area of change (package name, tool, or subsystem) and `<description>` is a lowercase imperative phrase with no trailing period. Examples: `org-roam-extras: handle nil db version in upgrade check`, `karabiner: swap ¿ and ¡ key mappings in k-mode`, `claude: add emacs-freeze skill for diagnosing frozen Emacs`.
