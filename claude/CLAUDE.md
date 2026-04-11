# Global conventions

## General

- Give honest, truthful answers. Push back when needed. Be transparent about uncertainty. Never present guesses as facts.
- Verify work against the ground truth (tests, typechecks, lint, screenshots) before declaring it done. If verification isn't possible in batch mode, say so.
- Fix root causes, not symptoms. No hacks, workarounds, or silent fallbacks. If a patch feels precarious, dig deeper. (E.g. if search returns duplicates, investigate why rather than adding deduplication.)
- Seek documentation rather than guessing. If you can't access it, ask me to find it.
- Never save a memory without asking first.
- Delete temporary files and code when done.
- Copy drafted messages to the clipboard automatically.
- Surface structural friction when you encounter it ("this was harder than expected because X — worth investigating?"). Use `/diagnose` for deep dives.
- I often use dictation, so expect misspellings and unusual punctuation.

## Safety

- **Emacs:** never send signals to an active Emacs session without explicit confirmation.
- **Deletion:** use `trash` instead of `rm -rf`.
- **Git cloning:** only clone repositories (`git clone`, `gh repo clone`) that the user has explicitly requested by URL or name. If a task seems to require cloning a repo the user hasn't specifically mentioned, ask first.
- **Shared systems:** never push code, create PRs, open issues, post messages, or take any action visible to others (GitHub, Slack, email, etc.) without stopping to get explicit confirmation first. Write code locally and let the user decide where it goes and when.

## Secrets

- When reading secrets from `pass` or `.zshenv-secrets`, never echo or print them in the terminal or logs.
- **Personal secrets** (non-Epoch): stored in `pass` (GPG-encrypted). The **Epoch** secrets setup is described in `../../Epoch/CLAUDE.md`.
- **Account-specific MCP secrets** (e.g. different API keys per Claude Code account): use the `claude-code-extras-account-env-vars` mechanism. Export suffixed vars in `.zshenv-secrets` (e.g. `TWITTERAPI_API_KEY_TLON`, `TWITTERAPI_API_KEY_EPOCH`) and map them in `config.org`.

## Agents

- Make liberal use of subagents and agent teams.
- Always use the most capable model available for subagents. I value performance above speed, so prefer more powerful models even if they are slower or more expensive.

## MCP servers

- Google MCP setup (two accounts, multiple servers, troubleshooting): see `google-mcp-setup.md`
- Chrome integration and multi-account: see `README.org` → "Chrome integration and multi-account".
- MCP server definitions go in `~/.claude.json`, NOT `~/.claude/settings.json`.

## Filesystem organization

### Dotfiles

My dotfiles are in `../` (i.e. `~/My Drive/dotfiles/`). Many files and directories in `~` are symlinked to this location.

### Projects

- My projects are stored in `../../repos/`.

## Version control

- **Important**: commit all changes you make immediately, unless I specify otherwise or the changes are temporary.
- Prefer amending commits when iterating on the same logical change, unless the previous commit has already been pushed.
