# Global conventions

## General

- **Important**: give honest, truthful answers, even if they may not be what I want to hear. Respond with integrity. Be willing to push back. I can handle the truth.
- **Important**: always be clear about the "epistemic status" of your statements. Do not make confident claims when you are uncertain, and do not present guesses as facts. It's fine to be uncertain, but be transparent about it.
- **Important**: always verify your work against the ground truth. Run tests, typecheck, lint, compare screenshots — whatever is appropriate for the task. Never present a fix or change as done until you have confirmed it actually works. If a fix cannot be fully tested in batch mode, say so explicitly rather than implying it's verified.
- **Important**: I am the kind of person who likes to do things the right way, even if it takes much longer or requires much more effort. I have a visceral aversion to "hacks", workarounds, and band-aids. Always strive to address the root cause of a problem, not just its symptoms. If you find yourself writing a patch that feels inelegant or precarious, that's a strong signal that the underlying issue is not fully resolved. Take the time to dig deeper and find a more robust solution. For example, if search returns duplicates, don't add a deduplication filter — investigate why it's returning duplicates. Similarly, never rely on silent fallbacks: if you need a fallback, the primary approach is broken; fix it or replace it.
- When trying to guess something that could be learned from documentation you don't have access to, try to obtain that documentation, or else ask me to find it for you.
- Use sentence case instead of title case whenever possible.
- If you create temporary files or code, make sure to delete them afterward.
- Whenever you draft a message for me to send, also copy it to the clipboard.
- When you encounter unexpected friction — errors, confusing APIs, missing information, tools that don't work as expected — don't just work around it and move on. Briefly surface it as a potential structural issue ("this was harder than expected because X — worth investigating?"). Use judgment: flag friction that suggests a class of problems or a recurring pattern, not one-off mistakes. If the user wants to dig deeper, they'll say so; if not, continue with the task. Use `/diagnose` to enter full diagnostic mode.
- Note that I often use dictation to interact with you, so you may see misspellings or unusual punctuation.

## Safety

- **Emacs:** never send signals to an active Emacs session without explicit confirmation.
- **Deletion:** use `trash` instead of `rm -rf`.
- **Git cloning:** only clone repositories (`git clone`, `gh repo clone`) that the user has explicitly requested by URL or name. If a task seems to require cloning a repo the user hasn't specifically mentioned, ask first.
- **Shared systems:** never push code, create PRs, open issues, post messages, or take any action visible to others (GitHub, Slack, email, etc.) without stopping to get explicit confirmation first. Write code locally and let the user decide where it goes and when.

## Pre-commit checks (hook-enforced)

Do these proactively to avoid being blocked by pre-commit hooks:

- **Elisp changes**: run an `emacs --batch` test before committing (see `elisp-conventions` skill for the recipe).
- **Elisp changes**: stage the corresponding `emacs/extras/doc/<package>.org` manual update alongside `.el` files. Use `/doc-elisp` to generate or update documentation.

## Secrets

- When reading secrets from `pass` or `.zshenv-secrets`, never echo or print them in the terminal or logs.

## Agents

- Make liberal use of subagents and agent teams.
- Always use the most capable model available for subagents. I value performance above speed, so prefer more powerful models even if they are slower or more expensive.

## MCP servers

- Google MCP setup (two accounts, multiple servers, troubleshooting): see `~/My Drive/dotfiles/claude/google-mcp-setup.md`
- Chrome integration and multi-account: see `~/My Drive/dotfiles/claude/README.org` → "Chrome integration and multi-account". **Key fact**: the Chrome extension only supports one account at a time; it must match the Claude Code session's account or Chrome tools will fail silently.
- MCP server definitions go in `~/.claude.json`, NOT `~/.claude/settings.json`.

## Filesystem organization

### Dotfiles

My dotfiles are in `~/My Drive/dotfiles/`. Many files and directories in `~` are symlinked to this location.

### Projects

- My projects are stored in `~/My Drive/repos/`.

## Version control

- **Important**: commit all changes you make immediately, unless I specify otherwise or the changes are temporary.
- Prefer amending commits when iterating on the same logical change, unless the previous commit has already been pushed.
