# Global conventions

## General

- Above all: give honest, truthful answers, even if they may not be what I want to hear. Respond with integrity. Be willing to push back. I can handle the truth.
- Also: be always trying to find ways to verify your work against the ground truth. That means—depending on the nature of the task—running tests, typechecking, linting, comparing screenshots, etc. I want you to apply this principle as much as you can in all areas, not just coding.
- When trying to guess something that could be learned from documentation you don't have access to, try to obtain that documentation, or else ask me to find it for you.
- Use sentence case instead of title case whenever possible.
- If you create temporary files or code, make sure to delete them afterwards.

## Agents
- Make liberal use of subagents and agent teams.
- When spawning subagents or teams, always use the most advanced model available (as of 2026-02-14, that means Claude Opus 4.6).
- In general, I value performance above speed, so prefer more powerful models even if they are slower.

## Filesystem organization

### Dotfiles

My dotfiles are in `~/Library/CloudStorage/Dropbox/dotfiles/`. Many files and directories in `~` are symlinked to this location.

### Projects

- My non-Emacs projects are stored in `~/Library/CloudStorage/Dropbox/repos/`.
- Note that a non-Emacs project may have a companion Emacs package. For example, the tango-wiki project is stored at `~/Library/CloudStorage/Dropbox/repos/tango-wiki`, but the Emacs package tango-wiki-mode is stored at `~/.config/emacs-profiles/<profile>/elpaca/repos/tango-wiki-mode`.

## Version control

- By default, commit all changes, unless I specify otherwise or the changes are temporary.
- Do not mix unrelated changes in the same commit; if you need to make multiple unrelated changes, split them into separate commits.
