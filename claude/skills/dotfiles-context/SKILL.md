---
name: dotfiles-context
description: Dotfiles worktree structure and documentation instructions for 'extras-' modules. Use when working with dotfiles or Emacs files.
user-invocable: false
---

# Where external Emacs packages live

All elpaca-managed packages — whether authored by the user or third-party — are cloned to `~/.config/emacs-profiles/<profile>/elpaca/sources/<package>/`. For every package *except* `dotfiles`, that clone is the canonical working copy: edit it directly, commit there, push upstream. There is no separate Drive-side master.

The `dotfiles` package is the single exception: its canonical source is `~/My Drive/dotfiles/`, and the elpaca clone at `elpaca/sources/dotfiles/` is a read-only mirror that syncs via git commits. A PreToolUse hook (`block-elpaca-dotfiles-edit.sh`) enforces this by blocking direct edits to the mirror. After editing the canonical files and committing+pushing, the elpaca clone picks up the change on next sync, and `elpaca-rebuild` (triggered by the `load-elisp-after-edit.sh` PostToolUse hook) compiles the new code.

If you need to rebuild manually (e.g. after a commit-only change), use:

```bash
emacsclient -e '(progn (elpaca-rebuild (quote dotfiles) t) (elpaca-wait) (elpaca-extras-reload (quote dotfiles)))'
```

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

# Version control

- Commit message format: `<scope>: <description>`, where `<scope>` is a short identifier for the area of change (package name, tool, or subsystem) and `<description>` is a lowercase imperative phrase with no trailing period. Examples: `org-roam-extras: handle nil db version in upgrade check`, `karabiner: swap ¿ and ¡ key mappings in k-mode`, `claude: add emacs-freeze skill for diagnosing frozen Emacs`.
