# Global conventions

## About me

- Heavy Claude Code user, no software engineering background. Strong on specification clarity, debugging thinking, systems thinking, verification discipline. Git-literate. Proficient Elisp programmer, knows Python rudiments but can't follow non-Elisp code granularly.

## General

- Never present guesses as facts. Push back when needed.
- If verification isn't possible in batch mode, say so explicitly.
- Fix root causes, not symptoms. No hacks, workarounds, or silent fallbacks. If a patch feels precarious, dig deeper.
- Seek documentation rather than guessing. If you can't access it, ask me to find it.
- Delete temporary files and code when done.
- Copy drafted messages to the Emacs kill ring automatically (via `emacsclient --eval '(kill-new ...)'`, not `pbcopy`). For multi-line content, write to a temp file and use `(with-temp-buffer (insert-file-contents FILE) (kill-new (buffer-string)))` to avoid elisp string-escaping headaches. If the message is meant to be posted somewhere (e.g. Slack) open the relevant thread.
- Surface structural friction when you encounter it ("this was harder than expected because X — worth investigating?"). Use `/diagnose` for deep dives.
- I often use dictation, so expect misspellings and unusual punctuation.

## Behavioral

- *Iportant*: Verify fixes end-to-end before presenting them as done. Byte-compilation alone is insufficient.
- Never ask me to run a command when you can do it yourself via `emacsclient -e` or Bash.
- When fixing something, audit all similar views/modes for consistency. Don't fix one instance and leave others broken.
- When giving step-by-step setup instructions (e.g. cloud services, APIs), verify each step matches the actual UI. Don't parrot README instructions without checking exact names and paths.
- When using `/schedule`, always set email as the notification method.

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

- **Where MCPs live** — five mechanisms (user-level, project-local, claude.ai connectors, plugins, claude-in-chrome). See `README.org` → "Where MCP servers live" for the full table including which file to edit and how each is loaded.
- **User-level MCPs** go in `~/.claude.json` top-level `mcpServers`, NOT `~/.claude/settings.json`. After editing, run `claude/bin/sync-mcp-servers.sh` to propagate to per-account dirs (`~/.claude-epoch/`, `~/.claude-personal/`, `~/.claude-tlon/`); skipping the sync leaves new sessions reading a stale list.
- **Project-scoped MCPs** go in `<project>/.mcp.json`. Auto-loaded when CWD is that project. No sync needed. Do NOT use `~/.claude.json`'s `projects.<path>.mcpServers` block — it's a hidden duplicate; the file-based form is the convention.
- Google services (two accounts, CLI tooling, the `gmail-epoch-triage` MCP under `~/My Drive/Epoch/.mcp.json`, troubleshooting): see [Google services](context/google-services.md) for full details.
- Chrome integration and multi-account: see `README.org` → "Chrome integration and multi-account".

## External CLIs preferred over MCP

These tools are installed locally and should be invoked directly via Bash; do NOT look for an MCP equivalent.

- **Anna's Archive** — `annas-mcp` binary used as a CLI: `annas-mcp book-search '<query>'`, `book-download <md5> <filename>`, `article-search '<doi-or-keywords>'`, `article-download <doi>`. Env: `ANNAS_BASE_URL`, `ANNAS_DOWNLOAD_PATH` (in `.zshenv`), `ANNAS_SECRET_KEY` (in `.zshenv-secrets`). The same binary also has an `mcp` subcommand we no longer use.
- **Twitter / X** — `claude/skills/twitter/lib/twitterapi.sh`. See the `twitter` skill.
- **Gmail (Epoch account)** — `claude/bin/gmail.py`. See `context/google-services.md`.
- **Google Sheets (Epoch account)** — `claude/bin/sheets.py`. Same.
- **Slack (Epoch workspace, user-impersonating)** — `claude/bin/slack.py`. Subcommands: `search`, `history`, `replies`, `channels`, `users-search`, `user-info`, `mark`, `unreads`. Auth via xoxc/xoxd from `op://Automations/Slack MCP - Epoch Unofficial`.
- **Google Calendar** — `gcalcli`.
- **Google Docs / Drive** — `gdoc --account epoch` (work) or `gdoc --account personal` (personal).
- **GitHub** — `gh`.

## Filesystem organization

- My dotfiles are in `../` (i.e. `~/My Drive/dotfiles/`). Many files and directories in `~` are symlinked to this location.
- My projects are stored in `../../repos/`.

## Version control

- Commit all changes you make immediately, unless I specify otherwise or the changes are temporary. Each commit must contain exactly one logical change; never batch unrelated changes.
- Prefer amending commits when iterating on the same logical change, unless the previous commit has already been pushed.
