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
- Docs/Drive: `gdoc --account personal` (same CLI as Epoch, separate OAuth token under `~/.config/gdoc/accounts/personal/`).
- Gmail: `gmail.py --account personal` (same CLI, separate refresh token; see "Auth" below).
- Sheets: `sheets.py --account personal` (same CLI, same token as Gmail).
- Calendar: `gcalcli` (already configured for both accounts via shared calendars; no per-account flag).

The personal-account OAuth grants are against the same `claude-code-gmail-490520` GCP project as the Epoch one, with `pablo.stafforini@gmail.com` added as a test user on the OAuth consent screen.

## Auth

`gmail.py`, `sheets.py`, and the shared `claude/bin/_gworkspace_auth.py` helper support both accounts. The OAuth client (id and secret) is shared; only the refresh token differs per account. Env vars (set in `~/My Drive/dotfiles/shell/.zshenv-secrets`):

| Var | Account | Purpose |
|---|---|---|
| `GOOGLE_WORKSPACE_CLIENT_ID` | both | OAuth client ID (shared) |
| `GOOGLE_WORKSPACE_CLIENT_SECRET` | both | OAuth client secret (shared) |
| `GOOGLE_WORKSPACE_REFRESH_TOKEN` | epoch | refresh token authenticated as `pablo@epoch.ai` |
| `GOOGLE_WORKSPACE_REFRESH_TOKEN_PERSONAL` | personal | refresh token authenticated as `pablo.stafforini@gmail.com` |

Pick the account with `--account epoch` (default) or `--account personal` on `gmail.py`/`sheets.py`. The wrapper exchanges the refresh token for an access token and caches per-account at `/tmp/gworkspace-access-token-<account>.json`.

The Epoch refresh token also powers `gdoc --account epoch` (which has its own auth flow but uses the same OAuth client) and the `gmail-epoch-triage` MCP (separate, see below). The personal refresh token is used only by `gmail.py`/`sheets.py`; `gdoc --account personal` has its own token under `~/.config/gdoc/accounts/personal/`.

If a token expires (`invalid_grant` errors), regenerate with the recipe under "Generating a new refresh token" below and update the appropriate env var in `~/.zshenv-secrets`.

## Servers by account

### work account

**gmail-epoch-triage** (project-scoped to Epoch, defined in `~/My Drive/Epoch/.mcp.json`)
- **Package:** `workspace-mcp` via `uvx` (configured with `--tools gmail`)
- **Covers:** Gmail only, for the `email-triage@epoch.ai` bot account (NOT `pablo@epoch.ai`).
- **Credentials:** stored in `~/.gmail-mcp-epoch/credentials/` as `{email}.json` files. Directory set via `WORKSPACE_MCP_CREDENTIALS_DIR`.
- **Not redundant with `gmail.py`:** the wrapper is authenticated as `pablo@epoch.ai`; this server is the `email-triage@epoch.ai` bot.

### personal account

No dedicated MCP server. Personal-account Docs/Drive go through `gdoc --account personal` (see "Tooling by service" above). Calendar through `gcalcli`. Gmail isn't wired.

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

The recipe below works for both accounts — the only differences are which Google account you sign in as, which env var to update afterwards, and which scopes to request. It prints the refresh token to stdout; copy that into the matching env var in `~/.zshenv-secrets`. If you'd rather not have the token transit your terminal, see the "without printing" variant below.

### Recipe (both accounts)

Choose your scopes by what the token will be used for:

- Epoch (full set): `gmail.modify`, `calendar`, `drive`, `documents`, `spreadsheets`.
- Personal (just what `gmail.py` and `sheets.py` need): `gmail.modify`, `spreadsheets`.

```bash
SCOPES="https://www.googleapis.com/auth/gmail.modify https://www.googleapis.com/auth/spreadsheets"
# Add ' https://www.googleapis.com/auth/calendar https://www.googleapis.com/auth/drive https://www.googleapis.com/auth/documents' for the Epoch full set.

uv run --with google-auth-oauthlib --no-project python3 -c "
import os
from google_auth_oauthlib.flow import InstalledAppFlow
flow = InstalledAppFlow.from_client_config({'installed': {
    'client_id': os.environ['GOOGLE_WORKSPACE_CLIENT_ID'],
    'client_secret': os.environ['GOOGLE_WORKSPACE_CLIENT_SECRET'],
    'redirect_uris': ['http://localhost'],
    'auth_uri': 'https://accounts.google.com/o/oauth2/auth',
    'token_uri': 'https://oauth2.googleapis.com/token',
}}, os.environ['SCOPES'].split())
creds = flow.run_local_server(port=8080, open_browser=True)
print('REFRESH_TOKEN=' + creds.refresh_token)
"
```

When the browser opens, sign in as the account you want the token for: `pablo@epoch.ai` for `GOOGLE_WORKSPACE_REFRESH_TOKEN`, `pablo.stafforini@gmail.com` for `GOOGLE_WORKSPACE_REFRESH_TOKEN_PERSONAL`.

After printing, paste the value into the corresponding `export` line in `~/.zshenv-secrets`, then `rm /tmp/gworkspace-access-token-<account>.json` to flush the wrapper's token cache and `source ~/.zshenv-secrets` (or open a fresh shell).

### Without printing the token to the terminal

If you'd rather not have the token in scrollback, replace the final `print(...)` line with code that writes the token directly to `.zshenv-secrets`:

```python
from pathlib import Path
import sys
VAR = "GOOGLE_WORKSPACE_REFRESH_TOKEN_PERSONAL"  # or GOOGLE_WORKSPACE_REFRESH_TOKEN
secrets = Path.home() / "My Drive" / "dotfiles" / "shell" / ".zshenv-secrets"
text = secrets.read_text() if secrets.exists() else ""
new_line = f'export {VAR}="{creds.refresh_token}"\n'
lines = text.splitlines(keepends=True)
for i, line in enumerate(lines):
    if line.startswith(f"export {VAR}="):
        lines[i] = new_line
        break
else:
    if text and not text.endswith("\n"): lines.append("\n")
    lines.append(new_line)
secrets.write_text("".join(lines))
print(f"Wrote {VAR} to {secrets}", file=sys.stderr)
```

Note: the OAuth flow's `port=8080` matches the redirect URI registered for `claude-code-gmail-490520`. `port=0` (random ephemeral) returns a Google 400 because the loopback redirect URI isn't auto-registered for this client.

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

1. `rm /tmp/gworkspace-access-token-<account>.json` (`<account>` is `epoch` or `personal` — match whichever account you ran with) and re-run the command. If a stale cache was the only problem, this fixes it.
2. If still failing, regenerate the refresh token (see "Generating a new refresh token" above), update the matching env var (`GOOGLE_WORKSPACE_REFRESH_TOKEN` for epoch, `GOOGLE_WORKSPACE_REFRESH_TOKEN_PERSONAL` for personal) in `~/.zshenv-secrets`, then `source ~/.zshenv-secrets` (or open a fresh shell).

### `invalid_grant` from `gdoc --account personal`

The personal-account OAuth token was revoked or aged out. Re-auth:

```bash
gdoc auth --account personal
```

A browser opens; sign in as `pablo.stafforini@gmail.com`. The OAuth client is in "testing" mode under GCP project `claude-code-gmail-490520`, so the personal email must remain in the project's **Test users** list (GCP Console → APIs & Services → OAuth consent screen → Audience).

### gmail.py reports "ERROR: missing env var"

`~/.zshenv-secrets` isn't being sourced into the active shell. Open a fresh terminal or `source ~/.zshenv-secrets`.

### gmail-epoch-triage says "ACTION REQUIRED: Google Authentication Needed"

The credential file is missing or the refresh token is expired. Regenerate using the manual flow above and write to `~/.gmail-mcp-epoch/credentials/{email}.json`. Then restart Claude Code.

### Other issues

- **403 "caller does not have permission"**: either the relevant API isn't enabled on the GCP project, or the document isn't shared with the account the token belongs to.
- **MCP server not appearing in `/mcp`**: MCP servers go in `~/.claude.json`, NOT `~/.claude/settings.json`. The latter is for Claude Code settings (hooks, permissions, theme).
