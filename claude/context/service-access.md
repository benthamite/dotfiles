# Service access tools

Local tools for external services. Prefer them over ad-hoc API calls or
browser automation; each already carries the right account and auth.

| Service | Tool |
|---|---|
| Anna's Archive | `annas-mcp` |
| Gmail | `claude/bin/gmail.py` |
| Google Sheets | `claude/bin/sheets.py` |
| Slack | `claude/bin/slack.py` |
| Google Calendar | `gcalcli` |
| Google Docs / Drive | `gdoc` |
| GitHub | `gh` |

Relative paths are under the dotfiles root, `~/My Drive/dotfiles/`. For
Google account and auth details, read `google-services.md` in this
directory.

## Browser-only flows

When opening Chrome manually for a browser-only service flow, use
`chrome-profile-open <alias> URL`; configure aliases with
`chrome-profile-open --setup <alias>`. Project wrappers may call it, for
example `trajectory-open URL` for Trajectory/CR pages.
