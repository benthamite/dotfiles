---
name: dotfiles-context
description: Dotfiles worktree structure and extras documentation instructions. Use when working with dotfiles or Emacs extras doc files.
user-invocable: false
---

# Dotfiles worktree structure

The dotfiles repo has two working trees on the same machine:

- **Google Drive/dotfiles** (`~/My Drive/dotfiles/`): the primary location for non-Elisp files, including `emacs/config.org`.
- **Elpaca repos** (`~/.config/emacs-profiles/<profile>/elpaca/repos/dotfiles/`): the location Emacs loads Elisp from, including the `emacs/extras/*.el` files.

Both point to the same Git repo, so changes only need to be committed once in the appropriate working tree. Edit each file in its canonical location: `config.org` in Google Drive/dotfiles, Elisp extras in the elpaca repos directory.

# Making changes

If you make a change to an extras Elisp file, please also update the corresponding org file in the `doc` subdirectory. For example, if you modify `emacs/extras/claude-code-extras.el`, also update `emacs/extras/doc/claude-code-extras.org`.
