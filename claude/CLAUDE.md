# Global conventions

## General

- Above all: give honest, truthful answers, even if they may not be what I want to hear. Respond with integrity. Be willing to push back. I can handle the truth.
- Also: be always trying to find ways to verify your work against the ground truth. That means—depending on the nature of the task—running tests, typechecking, linting, comparing screenshots, etc. I want you to apply this principle as much as you can in all areas, not just coding.
- When trying to guess something that could be learned from documentation you don't have access to, try to obtain that documentation, or else ask me to find it for you.
- Use sentence case instead of title case whenever possible.
- If you create temporary files or code, make sure to delete them afterwards.

## Safety

- **Deletion:** use `trash` instead of `rm -rf` (hook-enforced).
- **Git cloning:** only clone repositories (`git clone`, `gh repo clone`) that the user has explicitly requested by URL or name. If a task seems to require cloning a repo the user hasn't specifically mentioned, ask first.

## Coding

- If you detect a bug, always try to fix its root cause, rather than patch its symptoms. For example, if the search functionality of a website we are building shows duplicate results, do not add a deduplication filter; instead, investigate why the search is returning duplicates in the first place.
- Similarly, never rely on “fallbacks”: if you need to use a fallback, that means the primary approach is not working reliably. Make the primary approach robust, or replace it with a superior alternative.

## Agents

- Make liberal use of subagents and agent teams.
- When spawning subagents or teams, always use the most advanced model available (as of 2026-02-14, that means Claude Opus 4.6).
- In general, I value performance above speed, so prefer more powerful models even if they are slower.

## Filesystem organization

### Dotfiles

My dotfiles are in `~/Library/CloudStorage/Dropbox/dotfiles/`. Many files and directories in `~` are symlinked to this location.

### Projects

- My projects are stored in `~/Library/CloudStorage/Dropbox/repos/`.

## Version control

- **Important**: commit all changes you make immediately, unless I specify otherwise or the changes are temporary.
- Do not mix unrelated changes in the same commit; if you need to make multiple unrelated changes, split them into separate commits.
