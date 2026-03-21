# Google MCP servers setup

Pablo uses two Google accounts, which requires separate MCP servers for each.

## Accounts

| Account                      | Purpose            |
|------------------------------|--------------------|
| `pablo@epoch.ai`             | Epoch work account |
| `pablo.stafforini@gmail.com` | Personal account   |

## Servers by account

### pablo@epoch.ai

**google-workspace** (User MCP in `~/.claude.json`)
- **Package:** `google-workspace-mcp` via `uvx`
- **Covers:** Gmail, Calendar, Docs, Drive, Sheets, Slides
- **OAuth project:** `claude-code-gmail-490520` (client ID `573541886075-...`)
- **Credentials:** env vars in `~/My Drive/dotfiles/shell/.zshenv-secrets`:
  - `GOOGLE_WORKSPACE_CLIENT_ID`
  - `GOOGLE_WORKSPACE_CLIENT_SECRET`
  - `GOOGLE_WORKSPACE_REFRESH_TOKEN`
- **Known issue:** the PyPI package (v2.0.1) has a buggy entry point — `asyncio.run(mcp.run())` fails because `mcp.run()` is synchronous. The config uses `python3 -c "...mcp.run()"` to bypass this.
- **Refresh token generation:** if the token expires or scopes need updating, run the OAuth flow via `google_auth_oauthlib.flow.InstalledAppFlow` with the desired scopes (see below).

**gmail-epoch** (Local MCP, project-scoped to Epoch)
- **Package:** custom/separate Gmail MCP
- **Covers:** Gmail only
- **Config dir:** `~/.gmail-mcp-epoch/`
- **Accounts:** has credentials for both `pablo@epoch.ai` (`credentials-pablo.json`) and `email-triage@epoch.ai` (`credentials-bot.json`). The latter is a third Epoch account used only for the email triage bot.
- **Not redundant:** retained for `email-triage@epoch.ai` access, which `google-workspace` doesn't cover.

### pablo.stafforini@gmail.com

**google-docs** (User MCP in `~/.claude.json`)
- **Package:** `@a-bonus/google-docs-mcp` via `npx`
- **Covers:** Docs (and possibly Drive/Sheets — the package has broad tools)
- **OAuth client:** `831988183270-...` (different GCP project)
- **Config dir:** `~/.config/google-docs-mcp/`

**google-calendar** (User MCP in `~/.claude.json`)
- **Package:** `@cocal/google-calendar-mcp` via `npx`
- **Covers:** Calendar only
- **OAuth client:** `896560031613-...` (yet another GCP project)
- **Config dir:** `~/.config/google-calendar-mcp/`

### Built-in (claude.ai integrations)

- **claude.ai Gmail** — unclear which account; managed by Claude's own auth
- **claude.ai Google Calendar** — same as above
- **claude.ai Airtable** — Epoch Airtable

## Enabled APIs on `claude-code-gmail-490520`

The following APIs must be enabled for the `google-workspace` server to work:
- Gmail API
- Google Calendar API
- Google Drive API
- Google Docs API
- Google Sheets API

Enable at: Google Cloud Console > APIs & Services > Library (project `claude-code-gmail-490520`).

## Generating a new refresh token

If the token expires or you need to add scopes:

```bash
GOOGLE_OAUTH_CLIENT_ID="$GOOGLE_WORKSPACE_CLIENT_ID" \
GOOGLE_OAUTH_CLIENT_SECRET="$GOOGLE_WORKSPACE_CLIENT_SECRET" \
uvx --from google-workspace-mcp python3 -c "
from google_auth_oauthlib.flow import InstalledAppFlow
import os
flow = InstalledAppFlow.from_client_config({
    'installed': {
        'client_id': os.environ['GOOGLE_OAUTH_CLIENT_ID'],
        'client_secret': os.environ['GOOGLE_OAUTH_CLIENT_SECRET'],
        'redirect_uris': ['http://localhost'],
        'auth_uri': 'https://accounts.google.com/o/oauth2/auth',
        'token_uri': 'https://oauth2.googleapis.com/token',
    }
}, [
    'https://www.googleapis.com/auth/gmail.modify',
    'https://www.googleapis.com/auth/calendar',
    'https://www.googleapis.com/auth/drive',
    'https://www.googleapis.com/auth/documents',
    'https://www.googleapis.com/auth/spreadsheets',
])
creds = flow.run_local_server(port=0, open_browser=True)
print('REFRESH_TOKEN=' + creds.refresh_token)
"
```

Then update `GOOGLE_WORKSPACE_REFRESH_TOKEN` in `~/My Drive/dotfiles/shell/.zshenv-secrets`.

## Troubleshooting

- **`invalid_grant`**: refresh token is expired or revoked. Regenerate it (see above).
- **403 "caller does not have permission"**: either the relevant API isn't enabled on the GCP project, or the document isn't shared with the account the token belongs to.
- **404 "File not found"**: Google Drive returns 404 (not 403) when you lack access — check sharing settings on the document.
- **Server not appearing in `/mcp`**: MCP servers go in `~/.claude.json`, NOT `~/.claude/settings.json`. The latter is for Claude Code settings (hooks, permissions, theme).
- **`ValueError: a coroutine was expected`**: the `google-workspace-mcp` entry point bug. Use the `python3 -c` invocation that calls `mcp.run()` directly.
