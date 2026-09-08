# Service access tools

Local tools for external services. Prefer them over ad-hoc API calls or
browser automation; each already carries the right account and auth.

| Service | Tool |
|---|---|
| Anna's Archive | `annas-mcp` |
| Gmail | `claude/bin/gmail.py` |
| Google Sheets | `claude/bin/sheets.py` |
| Slack | `claude/bin/slack.py` |
| Google Calendar | `gcalcli-epoch` (canonical Epoch OAuth grant; shared personal calendars) |
| Google Docs / Drive | `gdoc` |
| GitHub | `gh` |

Relative paths are under the dotfiles root, `~/My Drive/dotfiles/`. For
Google account and auth details, read `google-services.md` in this
directory.

## Browser-only flows

For Codex automation that depends on an existing Chrome session, use the
installed Chrome plugin through its `control-chrome` skill and browser client.
Verify the connection with a read-only tab listing before navigating. A failed
generic Playwright or guessed CDP connection does not show that Chrome control
is unavailable.

`chrome-profile-open <alias> URL` is only a launcher for a page the user wants
opened manually. It activates Chrome and can steal focus. Never use it,
AppleScript, System Events, screenshots, or coordinate clicking as a fallback
for Codex browser automation. If the Chrome plugin cannot connect after its
documented checks, fail closed and report the connection problem.

Configure launcher aliases with `chrome-profile-open --setup <alias>`.
Project wrappers may call the launcher when manual opening is the requested
action, for example `trajectory-open URL` for Trajectory/CR pages.
