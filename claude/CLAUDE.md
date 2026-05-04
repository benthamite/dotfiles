# Global Claude Code Conventions

## Operating Rules

- Never present guesses as facts; state uncertainty and push back when assumptions are weak or conflict with constraints.
- Fix root causes, not symptoms. Do not add silent fallbacks or workaround code unless explicitly labeled, justified, and approved.
- Verify fixes end-to-end before calling them done. If end-to-end verification is not possible, say exactly what was and was not verified.
- Do not ask me to do things you can do yourself. Ask only when blocked by authentication, unavailable UI access, or required confirmation.
- For unfamiliar APIs, tools, or behavior that may have changed, consult authoritative docs rather than guessing.
- Clean up temporary files/code you created before finishing.
- When fixing a repeated pattern, check analogous views/modes touched by the same implementation path and keep behavior consistent.
- If repo/tooling/docs made the task unexpectedly harder, mention the concrete friction. For deep investigation, use the `diagnose` skill.

## Safety

- **Emacs:** never send signals to an active Emacs session without explicit confirmation.
- **Deletion:** use `trash` instead of destructive recursive deletion.
- **Git cloning:** only clone repositories (`git clone`, `gh repo clone`) that I have explicitly requested by URL or name. If a task seems to require cloning an unmentioned repo, ask first.
- **Shared systems:** do not create PRs, open issues, post Slack/email/comments, or take other externally visible actions without my explicit confirmation.
- @context/secrets.md

## User context and communication

- Assume I am git-literate, strong on specs/debugging/verification, proficient in Elisp, and less comfortable reading non-Elisp code line-by-line.
- Expect dictation errors in my messages.
- When drafting a message for me to post, copy it to the Emacs kill ring with `emacsclient`. For multi-line content, write to a temp file and use `(with-temp-buffer (insert-file-contents FILE) (kill-new (buffer-string)))` to avoid Elisp string-escaping issues. If the message is meant for Slack, email, or comments, open the relevant thread in Emacs.

## Agents

- Make liberal use of subagents and agent teams.
- Always use the most capable model available for subagents; prefer performance over speed/cost.

## MCP Servers

For MCP server placement, inventory, and account-specific MCP notes, read `context/mcp-servers.md`.

## Service Access Tools

Use these local tools to connect to external services: Anna's Archive `annas-mcp`, Twitter/X `claude/skills/twitter/lib/twitterapi.sh`, Gmail `claude/bin/gmail.py`, Google Sheets `claude/bin/sheets.py`, Slack `claude/bin/slack.py`, Google Calendar `gcalcli`, Google Docs/Drive `gdoc`, GitHub `gh`.

For Google account/auth details, read `context/google-services.md`. For Twitter workflows, use the `twitter` skill.

For `/schedule`, always set email as the notification method.

## Filesystem Organization

- Dotfiles source of truth: `~/My Drive/dotfiles/`; many home-directory paths are symlinks into it.
- Project repos: `~/My Drive/repos/`.

## Version Control

- Commit each logical change immediately unless the change is temporary.
- Keep commits single-purpose. Amend when iterating on the same logical change.
