# dotfiles conventions

@claude/skills/dotfiles-context/SKILL.md

# Claude Code configuration

For the configuration architecture (symlink topology, multi-account setup, settings hierarchy, hook loading order), see `claude/README.org` § "Directory topology" and § "Multi-account setup". Consult `emacs/extras/claude-code-extras.el` for the Elisp code that manages account switching and shared symlinks. Do not treat `~/.claude/` files as independent from `dotfiles/claude/` — most are symlinks.

# Documentation

When you add, remove, or significantly change anything in the `claude/` subdirectory (skills, hooks, settings, CLAUDE.md), update `claude/README.org` to reflect the change.
