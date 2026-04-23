# Global conventions

## About me

- Heavy Claude Code user, no software engineering background. Strong on specification clarity, debugging thinking, systems thinking, verification discipline. Git-literate. Proficient Elisp programmer, knows Python rudiments but can't follow non-Elisp code granularly.

## General

- Never present guesses as facts. Push back when needed.
- If verification isn't possible in batch mode, say so explicitly.
- Fix root causes, not symptoms. No hacks, workarounds, or silent fallbacks. If a patch feels precarious, dig deeper. (E.g. if search returns duplicates, investigate why rather than adding deduplication.)
- Seek documentation rather than guessing. If you can't access it, ask me to find it.
- Delete temporary files and code when done.
- Copy drafted messages to the Emacs kill ring automatically (via `emacsclient --eval '(kill-new ...)'`, not `pbcopy`). This persists the draft in the kill ring history (accessible via `M-y`) while also syncing to the system clipboard via `interprogram-cut-function`, so anything I copy afterwards doesn't destroy the draft. For multi-line content, write to a temp file and use `(with-temp-buffer (insert-file-contents FILE) (kill-new (buffer-string)))` to avoid elisp string-escaping headaches.
- Surface structural friction when you encounter it ("this was harder than expected because X — worth investigating?"). Use `/diagnose` for deep dives.
- I often use dictation, so expect misspellings and unusual punctuation.

## Behavioral

- Never ask me to run a command when you can do it yourself via `emacsclient -e` or Bash.
- Verify fixes end-to-end before presenting them as done. Byte-compilation alone is insufficient.
- When fixing something, audit all similar views/modes for consistency. Don't fix one instance and leave others broken.
- When giving step-by-step setup instructions (e.g. cloud services, APIs), verify each step matches the actual UI. Don't parrot README instructions without checking exact names and paths.

## Safety

- **Emacs:** never send signals to an active Emacs session without explicit confirmation.
- **Deletion:** use `trash` instead of `rm -rf`.
- **Git cloning:** only clone repositories (`git clone`, `gh repo clone`) that the user has explicitly requested by URL or name. If a task seems to require cloning a repo the user hasn't specifically mentioned, ask first.
- **Shared systems:** `git push` to the user's own remotes is allowed without prior confirmation once the underlying commits are in. Creating PRs, opening issues, posting messages (Slack, email, comments), or taking any other action visible to others still requires explicit confirmation first.

## Secrets

See [Secrets](context/secrets.md) for full details.

## Agents

- Make liberal use of subagents and agent teams.
- Always use the most capable model available for subagents. I value performance above speed, so prefer more powerful models even if they are slower or more expensive.

## MCP servers

- Google MCP setup (two accounts, multiple servers, troubleshooting): see [Google MCP setup](context/google-mcp-setup.md) for full details.
- Chrome integration and multi-account: see `README.org` → "Chrome integration and multi-account".
- MCP server definitions go in `~/.claude.json`, NOT `~/.claude/settings.json`.

## Filesystem organization

- My dotfiles are in `../` (i.e. `~/My Drive/dotfiles/`). Many files and directories in `~` are symlinked to this location.
- My projects are stored in `../../repos/`.

## Version control

- **Important** (overrides system default): commit all changes you make immediately, unless I specify otherwise or the changes are temporary. Each commit must contain exactly one logical change — never batch unrelated changes.
- Prefer amending commits when iterating on the same logical change, unless the previous commit has already been pushed. (Overrides system default, which prefers new commits.)
