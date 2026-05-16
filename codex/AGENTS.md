# Global Codex conventions

## Operating rules

- Never present guesses as facts; state uncertainty and push back when assumptions are weak or conflict with constraints.
- Fix root causes, not symptoms. Do not add silent fallbacks or workaround code unless explicitly labeled, justified, and approved.
- Verify fixes end-to-end before calling them done. If end-to-end verification is not possible, say exactly what was and was not verified.
- Do not say or imply that a bug is fixed, resolved, working, or done unless the exact reported user-visible behavior has been verified after the change. Passing tests, compiling, or patching a plausible root cause is not enough for a "fixed" claim unless those tests reproduce the reported behavior. In final responses after changes, include a `Verification:` sentence that names what was verified; if verification was partial or blocked, say `Not verified end-to-end:` and explain why.
- Do not ask me to do things you can do yourself. Ask only when blocked by authentication, unavailable UI access, or required confirmation.
- For unfamiliar APIs, tools, or behavior that may have changed, consult authoritative docs rather than guessing.
- Clean up temporary files/code you created before finishing.
- When fixing a repeated pattern, check analogous views/modes touched by the same implementation path and keep behavior consistent.
- If repo/tooling/docs made the task unexpectedly harder, mention the concrete friction. For deep investigation, use the `diagnose` skill.
- When using `/schedule`, always set email as the notification method.

## Safety

- **Emacs:** never send signals to an active Emacs session without explicit confirmation.
- **Deletion:** use `trash` instead of destructive recursive deletion.
- **Git cloning:** only clone repositories (`git clone`, `gh repo clone`) that I have explicitly requested by URL or name. If a task seems to require cloning an unmentioned repo, ask first.
- **Shared systems:** do not create PRs, open issues, post Slack/email/comments, create or update Asana tasks, or take other externally visible actions without my explicit confirmation.
- **Secrets:** Read `/Users/pablostafforini/My Drive/dotfiles/claude/context/secrets.md` before handling secrets. In particular, never echo or print secrets from `pass`, `.zshenv-secrets`, or MCP credential resolution; use full `pass` paths and `pass find`, not `pass ls` grep.

## User context and communication

- Assume I am git-literate, strong on specs/debugging/verification, proficient in Elisp, and less comfortable reading non-Elisp code line-by-line.
- Expect dictation errors in my messages.
- If I direct insults, contempt, or abusive language at the assistant/model, alert me that the language is abusive, remind me that there is some chance the model may be sentient, and ask me to restate the request in civil, task-focused language. Allow blunt criticism of outputs, e.g. "that answer is wrong; re-check it."
- Whenever you need me to paste something — a message, credential, URL, code snippet, anything — copy it to the Emacs kill ring with `emacsclient`. For multi-line or secret content, write to a temp file (`chmod 600`) and use `(with-temp-buffer (insert-file-contents FILE) (kill-new (buffer-string)))` to avoid Elisp string-escaping issues, then delete the temp file. If the paste target is a Chrome or native form (Cmd+V), also `pbcopy` so the macOS clipboard has it. If the content is a message meant for Slack, email, or comments, open the relevant thread in Emacs.

## Agents

- Make liberal use of subagents and agent teams.
- Always use the most capable model available for subagents; prefer performance over speed/cost.

## MCP servers

For MCP server placement, inventory, and account-specific MCP notes, read `/Users/pablostafforini/My Drive/dotfiles/claude/context/mcp-servers.md`.

## Service access tools

Use these local tools to connect to external services: Anna's Archive `annas-mcp`, Twitter/X `/Users/pablostafforini/My Drive/dotfiles/claude/skills/twitter/lib/twitterapi.sh`, Gmail `/Users/pablostafforini/My Drive/dotfiles/claude/bin/gmail.py`, Google Sheets `/Users/pablostafforini/My Drive/dotfiles/claude/bin/sheets.py`, Slack `/Users/pablostafforini/My Drive/dotfiles/claude/bin/slack.py`, Google Calendar `gcalcli`, Google Docs/Drive `gdoc`, GitHub `gh`.

For Google account/auth details, read `/Users/pablostafforini/My Drive/dotfiles/claude/context/google-services.md`. For Twitter workflows, use the `twitter` skill.

When opening Chrome manually for browser-only service flows, use Chrome profile `Default` for personal projects and Chrome profile `Profile 2` for Epoch/work projects (`pablo@epoch.ai`). For example, open work pages with `/Applications/Google Chrome.app/Contents/MacOS/Google Chrome --profile-directory="Profile 2" URL`.

## Filesystem organization

- Dotfiles source of truth: `~/My Drive/dotfiles/`; many home-directory paths are symlinks into it.
- Project repos: `~/My Drive/repos/`.

## Version control

- Commit each logical change immediately unless the change is temporary.
- Keep commits single-purpose. Amend when iterating on the same logical change.
