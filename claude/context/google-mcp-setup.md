# Google services setup

Pablo uses two Google accounts and accesses each via different tools.

## Accounts

| Account                      | Purpose            |
|------------------------------|--------------------|
| `pablo@epoch.ai`             | Epoch work account |
| `pablo.stafforini@gmail.com` | Personal account   |

## Tooling by service

For the **Epoch** account (`pablo@epoch.ai`), prefer CLIs over MCP. The CLIs share auth via the `GOOGLE_WORKSPACE_*` env vars (see "Auth" below).

| Service  | Tool | Notes |
|----------|------|-------|
| Docs     | `gdoc` CLI | `gdoc cat/find/edit/insert/info/share`. Always pass `--account epoch`. |
| Drive    | `gdoc` CLI | `gdoc find --account epoch --title --plain '<query>'`. |
| Gmail    | `claude/bin/gmail.py` | Subcommands: `query`, `get`, `raw`, `attachment`, `archive`, `draft`, `send`, `reply`, `send-draft`. |
| Sheets   | `claude/bin/sheets.py` | Subcommands: `read`, `write`, `append`, `clear`, `add-sheet`, `delete-sheet`, `create`, `info`. |
| Calendar | `gcalcli` CLI | `gcalcli agenda/search/add/edit/delete`, with `--calendar '<name>'` to scope. |
| Slides   | none | Rare enough to handle ad-hoc via curl + Sheets/Drive APIs if ever needed. |

For the **personal** account (`pablo.stafforini@gmail.com`):
- Docs: `mcp__google-docs-personal__*` (the only personal-account MCP still in use). See "google-docs-personal" below for the OAuth-token-expiry quirk.
- Calendar: `gcalcli` (already configured for both accounts).
- Gmail: not currently wired. Add to `gmail.py` with separate creds if needed.

## Auth (Epoch CLIs)

`gmail.py` and `sheets.py` share auth via `claude/bin/_gworkspace_auth.py`. They read these env vars (set in `~/My Drive/dotfiles/shell/.zshenv-secrets`):

- `GOOGLE_WORKSPACE_CLIENT_ID`
- `GOOGLE_WORKSPACE_CLIENT_SECRET`
- `GOOGLE_WORKSPACE_REFRESH_TOKEN`

The OAuth project is `claude-code-gmail-490520`. The wrapper exchanges the refresh token for an access token and caches it in `/tmp/gworkspace-access-token.json` until it expires.

The same refresh token also powers `gdoc` (which has its own auth flow but uses the same OAuth client) and `gmail-epoch-triage` (separate, see below).

If the token expires (`invalid_grant` errors), regenerate with the recipe under "Generating a new refresh token" below and update `GOOGLE_WORKSPACE_REFRESH_TOKEN` in `~/.zshenv-secrets`.

## Servers by account

### work account

**gmail-epoch-triage** (project-scoped to Epoch, defined in `~/.claude.json` under `projects./Users/pablostafforini/My Drive/Epoch`)
- **Package:** `workspace-mcp` via `uvx` (configured with `--tools gmail`)
- **Covers:** Gmail only, for the `email-triage@epoch.ai` bot account (NOT `pablo@epoch.ai`).
- **Credentials:** stored in `~/.gmail-mcp-epoch/credentials/` as `{email}.json` files. Directory set via `WORKSPACE_MCP_CREDENTIALS_DIR`.
- **Not redundant with `gmail.py`:** the wrapper is authenticated as `pablo@epoch.ai`; this server is the `email-triage@epoch.ai` bot.

### personal account

**google-docs-personal** (User MCP in `~/.claude.json`)
- **Package:** `@a-bonus/google-docs-mcp` via `npx`
- **Covers:** Docs (and possibly Drive/Sheets — the package has broad tools)
- **OAuth client:** `831988183270-...` (different GCP project)
- **Config dir:** `~/.config/google-docs-mcp/`
- **Known issue (token expiry):** the OAuth app is in Google "testing" mode, which means **refresh tokens expire after 7 days**. This causes recurring `invalid_grant` errors. Long-term fix is to replace it with a `google-workspace-personal` server in production mode (see "Planned" below).

### Planned: google-workspace-personal

A GCP project `claude-mcp-personal-ps` was created with APIs enabled to replace the flaky `google-docs-personal`. Remaining steps:
1. Configure OAuth consent screen in production mode (GCP Console > APIs & Services > OAuth consent screen for project `claude-mcp-personal-ps`)
2. Create OAuth client credentials (Desktop app)
3. Generate refresh token using `InstalledAppFlow` (see recipe below)
4. Either (a) add a `google-workspace-personal` MCP server entry to `~/.claude.json`, or (b) extend `gmail.py`/`sheets.py` to support a `--account personal` flag with a separate set of `GOOGLE_WORKSPACE_PERSONAL_*` env vars. Option (b) is preferable — fewer running processes, less schema overhead.
5. Remove `google-docs-personal` from `~/.claude.json`
6. Update this documentation

### Built-in (claude.ai integrations)

- **claude.ai Gmail** — managed by Claude's own auth
- **claude.ai Google Calendar** — same
- **claude.ai Airtable** — Epoch Airtable

## Enabled APIs on `claude-code-gmail-490520`

These APIs must be enabled for `gmail.py`, `sheets.py`, `gdoc`, and `gcalcli` to work:
- Gmail API
- Google Calendar API
- Google Drive API
- Google Docs API
- Google Sheets API

Enable at: Google Cloud Console > APIs & Services > Library (project `claude-code-gmail-490520`).

## Generating a new refresh token

### For the Epoch CLIs (`gmail.py`, `sheets.py`, gdoc)

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

Update `GOOGLE_WORKSPACE_REFRESH_TOKEN` in `~/My Drive/dotfiles/shell/.zshenv-secrets`. Then `rm /tmp/gworkspace-access-token.json` to flush the wrapper's token cache.

### gmail-epoch-triage (email-triage@epoch.ai)

The built-in OAuth flow does not work for this server (port-conflict-related, when both this and the old `google-workspace-epoch` ran). Generate tokens manually using `InstalledAppFlow` on a different port and write to the credentials directory:

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
creds = flow.run_local_server(port=9090, open_browser=True)
token_data = {
    'token': creds.token,
    'refresh_token': creds.refresh_token,
    'token_uri': creds.token_uri,
    'client_id': creds.client_id,
    'client_secret': creds.client_secret,
    'scopes': list(creds.scopes),
}
with open('$HOME/.gmail-mcp-epoch/credentials/email-triage@epoch.ai.json', 'w') as f:
    json.dump(token_data, f, indent=2)
print('Credentials saved. Sign in as email-triage@epoch.ai in the browser.')
"
```

After running, restart Claude Code so the MCP server picks up the new credentials.

## Troubleshooting

### `invalid_grant` from gmail.py / sheets.py

The cached access token expired or the refresh token was revoked. Try in order:

1. `rm /tmp/gworkspace-access-token.json` and re-run the command. If a stale cache was the only problem, this fixes it.
2. If still failing, regenerate the refresh token (see "Generating a new refresh token" above), update `~/.zshenv-secrets`, then `source ~/.zshenv-secrets` (or open a fresh shell).

### `invalid_grant` on google-docs-personal

The `@a-bonus/google-docs-mcp` server's OAuth app is in Google "testing" mode, so refresh tokens expire every 7 days. To fix:

1. Delete the stale token: `rm ~/.config/google-docs-mcp/token.json`
2. Re-run the MCP server to trigger the OAuth flow:
   ```bash
   GOOGLE_CLIENT_ID="$GOOGLE_DOCS_CLIENT_ID" \
   GOOGLE_CLIENT_SECRET="$GOOGLE_DOCS_CLIENT_SECRET" \
   npx -y @a-bonus/google-docs-mcp
   ```
   Open the printed auth URL, sign in with `pablo.stafforini@gmail.com`, authorize.
3. **Critical:** the running MCP server process caches the old token in memory. You **must start a brand new Claude Code session** (not resume) for it to pick up the new token.

### gmail.py reports "ERROR: missing env var"

`~/.zshenv-secrets` isn't being sourced into the active shell. Open a fresh terminal or `source ~/.zshenv-secrets`.

### gmail-epoch-triage says "ACTION REQUIRED: Google Authentication Needed"

The credential file is missing or the refresh token is expired. Regenerate using the manual flow above and write to `~/.gmail-mcp-epoch/credentials/{email}.json`. Then restart Claude Code.

### Other issues

- **403 "caller does not have permission"**: either the relevant API isn't enabled on the GCP project, or the document isn't shared with the account the token belongs to.
- **MCP server not appearing in `/mcp`**: MCP servers go in `~/.claude.json`, NOT `~/.claude/settings.json`. The latter is for Claude Code settings (hooks, permissions, theme).
