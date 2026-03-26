# Google MCP servers setup

Pablo uses two Google accounts, which requires separate MCP servers for each.

## Accounts

| Account                      | Purpose            |
|------------------------------|--------------------|
| `pablo@epoch.ai`             | Epoch work account |
| `pablo.stafforini@gmail.com` | Personal account   |

## Servers by account

### work account

**google-workspace-epoch** (User MCP in `~/.claude.json`)
- **Package:** `google-workspace-mcp` via `uvx`
- **Covers:** Gmail, Calendar, Docs, Drive, Sheets, Slides
- **OAuth project:** `claude-code-gmail-490520` (client ID `573541886075-...`)
- **Credentials:** env vars in `~/My Drive/dotfiles/shell/.zshenv-secrets`:
  - `GOOGLE_WORKSPACE_CLIENT_ID`
  - `GOOGLE_WORKSPACE_CLIENT_SECRET`
  - `GOOGLE_WORKSPACE_REFRESH_TOKEN`
- **Known issue:** the PyPI package (v2.0.1) has a buggy entry point — `asyncio.run(mcp.run())` fails because `mcp.run()` is synchronous. The config uses `python3 -c "...mcp.run()"` to bypass this.
- **Refresh token generation:** if the token expires or scopes need updating, run the OAuth flow via `google_auth_oauthlib.flow.InstalledAppFlow` with the desired scopes (see below).

**gmail-epoch-triage** (project-scoped to Epoch, defined in `~/.claude.json` under `projects./Users/pablostafforini/My Drive/Epoch`)
- **Package:** `workspace-mcp` via `uvx` (same package as `google-workspace-epoch`, but configured with `--tools gmail` and different credentials)
- **Covers:** Gmail only
- **OAuth project:** same as `google-workspace-epoch` (`claude-code-gmail-490520`)
- **Credentials:** stored in `~/.gmail-mcp-epoch/credentials/` as `{email}.json` files (one per account). The directory is set via `WORKSPACE_MCP_CREDENTIALS_DIR` in the MCP env config.
- **Accounts:** `email-triage@epoch.ai` (the email triage bot account). Legacy credential files (`credentials-bot.json`, `credentials-pablo.json`) also exist in `~/.gmail-mcp-epoch/` but are not used by the MCP server.
- **GCP OAuth keys:** `~/.gmail-mcp-epoch/gcp-oauth.keys.json`
- **Not redundant:** retained for `email-triage@epoch.ai` access, which `google-workspace-epoch` doesn't cover.
- **Known issue (port conflict):** both `google-workspace-epoch` and `gmail-epoch-triage` run the same `workspace-mcp` package, which starts an OAuth callback server on port 8000. Whichever MCP process starts first claims the port; the other's built-in OAuth flow will always fail with "Invalid or expired OAuth state parameter" because the callback goes to the wrong process. **Fix:** never rely on the built-in OAuth flow for `gmail-epoch-triage`. Instead, generate tokens manually (see below) and write them to the credentials directory.

### personal account

**google-docs-personal** (User MCP in `~/.claude.json`)
- **Package:** `@a-bonus/google-docs-mcp` via `npx`
- **Covers:** Docs (and possibly Drive/Sheets — the package has broad tools)
- **OAuth client:** `831988183270-...` (different GCP project)
- **Config dir:** `~/.config/google-docs-mcp/`

**google-calendar-personal** (User MCP in `~/.claude.json`)
- **Package:** `@cocal/google-calendar-mcp` via `npx`
- **Covers:** Calendar only
- **OAuth client:** `896560031613-...` (yet another GCP project)
- **Config dir:** `~/.config/google-calendar-mcp/`

### Built-in (claude.ai integrations)

- **claude.ai Gmail** — unclear which account; managed by Claude's own auth
- **claude.ai Google Calendar** — same as above
- **claude.ai Airtable** — Epoch Airtable

## Enabled APIs on `claude-code-gmail-490520`

The following APIs must be enabled for the `google-workspace-epoch` server to work:
- Gmail API
- Google Calendar API
- Google Drive API
- Google Docs API
- Google Sheets API

Enable at: Google Cloud Console > APIs & Services > Library (project `claude-code-gmail-490520`).

## Generating a new refresh token

### google-workspace-epoch (pablo@epoch.ai)

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

### gmail-epoch-triage (email-triage@epoch.ai)

The built-in OAuth flow does not work due to the port conflict (see known issue above). Generate tokens manually using `InstalledAppFlow` on a different port, then write the result to the credentials directory.

```bash
python3 -c "
from google_auth_oauthlib.flow import InstalledAppFlow
import json
flow = InstalledAppFlow.from_client_secrets_file(
    '$HOME/.gmail-mcp-epoch/gcp-oauth.keys.json',
    scopes=[
        'https://www.googleapis.com/auth/gmail.readonly',
        'https://www.googleapis.com/auth/gmail.compose',
        'https://www.googleapis.com/auth/gmail.modify',
        'https://www.googleapis.com/auth/gmail.labels',
        'https://www.googleapis.com/auth/gmail.settings.basic',
        'https://www.googleapis.com/auth/gmail.send',
        'https://www.googleapis.com/auth/userinfo.email',
        'https://www.googleapis.com/auth/userinfo.profile',
        'openid',
    ]
)
# IMPORTANT: use a different port than 8000 to avoid the port conflict
creds = flow.run_local_server(port=9090, open_browser=True)
token_data = {
    'token': creds.token,
    'refresh_token': creds.refresh_token,
    'token_uri': creds.token_uri,
    'client_id': creds.client_id,
    'client_secret': creds.client_secret,
    'scopes': list(creds.scopes),
}
# Write in the format the workspace-mcp credential store expects
# The filename must be {email}.json
with open('$HOME/.gmail-mcp-epoch/credentials/email-triage@epoch.ai.json', 'w') as f:
    json.dump(token_data, f, indent=2)
print('Credentials saved. Sign in as email-triage@epoch.ai in the browser.')
"
```

After running, restart Claude Code so the `gmail-epoch-triage` MCP server picks up the new credentials.

## Troubleshooting

- **`invalid_grant`**: refresh token is expired or revoked. Regenerate it (see above).
- **403 "caller does not have permission"**: either the relevant API isn't enabled on the GCP project, or the document isn't shared with the account the token belongs to.
- **404 "File not found"**: Google Drive returns 404 (not 403) when you lack access; check sharing settings on the document.
- **Server not appearing in `/mcp`**: MCP servers go in `~/.claude.json`, NOT `~/.claude/settings.json`. The latter is for Claude Code settings (hooks, permissions, theme).
- **`ValueError: a coroutine was expected`**: the `google-workspace-mcp` entry point bug. Use the `python3 -c` invocation that calls `mcp.run()` directly.
- **"Invalid or expired OAuth state parameter" on `gmail-epoch-triage`**: this is the port 8000 conflict between `google-workspace-epoch` and `gmail-epoch-triage`. Do NOT retry the built-in OAuth flow; it will never work. Instead, generate tokens manually on port 9090 (see "gmail-epoch-triage" section above).
- **gmail-epoch-triage says "ACTION REQUIRED: Google Authentication Needed"**: the credential file is missing or the refresh token is expired. Regenerate using the manual flow above and write to `~/.gmail-mcp-epoch/credentials/{email}.json`. Then restart Claude Code.
