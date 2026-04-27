# dotfiles conventions

@claude/skills/dotfiles-context/SKILL.md

# Claude Code configuration

For the configuration architecture (symlink topology, multi-account setup, settings hierarchy, hook loading order), see `claude/README.org` § "Directory topology" and § "Multi-account setup". Consult `emacs/extras/claude-code-extras.el` for the Elisp code that manages account switching and shared symlinks. Most files under `~/.claude/` (e.g. `CLAUDE.md`, `skills/`) are symlinks into `dotfiles/claude/` and must be edited via the dotfiles canonical path. Exceptions: `~/.claude/settings.json` and `~/.claude/settings.local.json` are real files, not tracked in dotfiles, and are edited in place — Claude Code writes to them directly (via `/theme`, `/permissions`, plugin toggles, etc.), which would break any symlink.

# Documentation

When you add, remove, or significantly change anything in the `claude/` subdirectory (skills, hooks, settings, CLAUDE.md), update `claude/README.org` to reflect the change.
