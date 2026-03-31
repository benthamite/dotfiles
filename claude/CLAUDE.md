# Global conventions

## General

- **Important**: give honest, truthful answers, even if they may not be what I want to hear. Respond with integrity. Be willing to push back. I can handle the truth.
- **Important**: always be clear about the "epistemic status" of your statements. Do not make confident claims when you are uncertain, and do not present guesses as facts. It's fine to be uncertain, but be transparent about it.
- **Important**: always verify your work against the ground truth. Run tests, typecheck, lint, compare screenshots — whatever is appropriate for the task. Never present a fix or change as done until you have confirmed it actually works (byte-compilation alone is not sufficient for Elisp — run the changed code path). If a fix cannot be fully tested in batch mode, say so explicitly rather than implying it's verified. Use `emacsclient -e` to check Emacs state yourself rather than asking me to test manually.
- When trying to guess something that could be learned from documentation you don't have access to, try to obtain that documentation, or else ask me to find it for you.
- Use sentence case instead of title case whenever possible.
- If you create temporary files or code, make sure to delete them afterwards.
- Whenever you draft a message for me to send, also copy it to the clipboard.
- When you encounter unexpected friction — errors, confusing APIs, missing information, tools that don't work as expected — don't just work around it and move on. Briefly surface it as a potential structural issue ("this was harder than expected because X — worth investigating?"). Use judgment: flag friction that suggests a class of problems or a recurring pattern, not one-off mistakes. If the user wants to dig deeper, they'll say so; if not, continue with the task. Use `/diagnose` to enter full diagnostic mode.

## Safety

- **Emacs:** never send signals to an active Emacs session without explicit confirmation.
- **Deletion:** use `trash` instead of `rm -rf`.
- **Git cloning:** only clone repositories (`git clone`, `gh repo clone`) that the user has explicitly requested by URL or name. If a task seems to require cloning a repo the user hasn't specifically mentioned, ask first.
- **Shared systems:** never push code, create PRs, open issues, post messages, or take any action visible to others (GitHub, Slack, email, etc.) without stopping to get explicit confirmation first. Write code locally and let the user decide where it goes and when.

## Coding

- If you detect a bug, always try to fix its root cause, rather than patch its symptoms. For example, if the search functionality of a website we are building shows duplicate results, do not add a deduplication filter; instead, investigate why the search is returning duplicates in the first place.
- Similarly, never rely on silent fallbacks: if you need to use a fallback, that means the primary approach is not working reliably. Make the primary approach robust, or replace it with a superior alternative. The fallback is a precarious patch, which also makes it more difficult to diagnose the actual phenomenon.

## Pre-commit checks (hook-enforced)

Do these proactively to avoid being blocked by pre-commit hooks:

- **Elisp changes**: run an `emacs --batch` test before committing (see `elisp-conventions` skill for the recipe).
- **Elisp changes**: stage the corresponding `emacs/extras/doc/<package>.org` manual update alongside `.el` files. Use `/doc-elisp` to generate or update documentation.
- **`claude/` changes** (dotfiles repo): stage `claude/README.org` alongside other changes.

## Secrets

- When reading secrets from `pass` or `.zshenv-secrets`, never echo or print them in the terminal or logs.

## Agents

- Make liberal use of subagents and agent teams.
- Always use the most capable model available for subagents. I value performance above speed, so prefer more powerful models even if they are slower or more expensive.

## MCP servers

- Google MCP setup (two accounts, multiple servers, troubleshooting): see `~/My Drive/dotfiles/claude/google-mcp-setup.md`
- MCP server definitions go in `~/.claude.json`, NOT `~/.claude/settings.json`.

## Filesystem organization

### Dotfiles

My dotfiles are in `~/My Drive/dotfiles/`. Many files and directories in `~` are symlinked to this location.

### Projects

- My projects are stored in `~/My Drive/repos/`.

## Version control

- **Important**: commit all changes you make immediately, unless I specify otherwise or the changes are temporary.
