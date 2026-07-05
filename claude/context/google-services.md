# Google services setup

Pablo uses multiple human Google accounts plus a bot account, and accesses each via different tools.

## Accounts

| Account                                  | Purpose                       |
|------------------------------------------|-------------------------------|
| `pablo@epoch.ai`                         | Epoch work account            |
| `pablo.stafforini@trajectorylabs.net`    | Trajectory Docs/Drive account |
| `pablo.stafforini@gmail.com`             | Personal account              |
| `email-triage@epoch.ai`                  | Email-triage bot account      |

## Tooling by service

For the **Epoch** account (`pablo@epoch.ai`), use the local Google tools below. They replace the former general-purpose Google Workspace MCP path for Pablo-owned agent work and share auth via the `GOOGLE_WORKSPACE_*` env vars (see "Auth" below).

| Service  | Tool | Notes |
|----------|------|-------|
| Docs     | `gdoc` CLI | `gdoc cat/find/edit/insert/info/share`. Always pass `--account epoch`. |
| Drive    | `gdoc` CLI | `gdoc find --account epoch --title --plain '<query>'`. |
| Gmail    | `claude/bin/gmail.py` | Subcommands: `query`, `get`, `raw`, `attachment`, `archive`, `draft`, `send`, `reply`, `send-draft`; `send`, `draft`, and `reply` support `--attach`. |
| Sheets   | `claude/bin/sheets.py` | Subcommands: `read`, `write`, `append`, `clear`, `add-sheet`, `delete-sheet`, `create`, `info`. |
| Calendar | `gcalcli` CLI | `gcalcli agenda/search/add/edit/delete`, with `--calendar '<name>'` to scope. |
| Slides   | none | Rare enough to handle ad-hoc via curl + Sheets/Drive APIs if ever needed. |

For the **personal** account (`pablo.stafforini@gmail.com`):
- Docs/Drive: `gdoc --account personal` (same CLI as Epoch, separate OAuth token under `~/.config/gdoc/accounts/personal/`).
- Gmail: `gmail.py --account personal` (same CLI, separate refresh token; see "Auth" below).
- Sheets: `sheets.py --account personal` (same CLI, same token as Gmail).
- Calendar: `gcalcli` (already configured for both accounts via shared calendars; no per-account flag).

The personal-account OAuth grants are against the same `claude-code-gmail-490520` GCP project as the Epoch one, with `pablo.stafforini@gmail.com` added as a test user on the OAuth consent screen.

For **Trajectory-owned Google Docs/Drive**, use the Trajectory Labs account:
`gdoc --account pablo.stafforini@trajectorylabs.net`. Do not fall back to
`--account epoch` for Trajectory Docs/Drive access; fix sharing or OAuth access
for the Trajectory account instead. This routing note is Docs/Drive-only and
does not establish Gmail, Sheets, or Calendar support for the Trajectory account.

For the **email-triage bot** account (`email-triage@epoch.ai`), use
`gmail.py --account email-triage`. This account is only for email-triage
automation maintenance, bot mailbox checks, and related debugging. Do not use it
for Pablo's Epoch inbox, Pablo's personal inbox, Mercury receipts, or one-off
human-account email work.

## Google Docs browser body edits

Google Docs edits are externally visible. Do not use this protocol as approval
to edit a shared doc; it only describes how to perform browser body edits after
the normal approval boundary has already been satisfied.

For approved Google Docs browser body edits:

- Never use `Cmd+A` in the document body during browser automation.
- After opening find with `Cmd+F` and before typing, screenshot and verify that
  focus is in the intended find field or document body target.
- Click checkboxes or table cells only at coordinates confirmed in the
  immediately preceding screenshot.
- Select table or body text only with scoped gestures, such as triple-clicking
  inside a single table cell. Before applying formatting, verify by screenshot
  that the selection is contained in the intended cell or text range.
- If any unexpected text change occurs, press `Cmd+Z` immediately and stop
  instead of continuing the edit batch.

After browser body edits, verify content integrity at the revision level: export
the last pre-session revision and the current document as `text/plain`, then
byte-compare the exports. A plain-text compare catches body content damage, not
all formatting damage; when checkbox state, strikethrough, or similar formatting
matters, also check an export format that preserves it, such as Markdown or
HTML.

## Google Docs read-only inspection caveats (observed 2026-06-11)

For read-only Google Docs inspection, treat Drive API exports as the reliable
source for document body state. `gdoc cat` and `gdoc info` can be convenient
orientation tools, but do not use their output alone for content-change claims.

- Use HTML export to locate inline comment anchors, including inline `[letter]`
  markers.
- Use Markdown export to preserve and check checkbox state (`- [ ]` / `- [x]`)
  and strikethrough (`~~`).
- For read-only inspection, resolved comments are absent from body exports, so
  the absence of a comment in an export does not prove that the comment was
  never present.
- Treat `gdoc` "edited by X" or since-last-interaction banners, and `gdoc info`
  word counts, as unreliable signals for content-change claims.
- Verify real content edits with Drive revisions plus per-revision export
  byte-compare.

This behavior was observed with installed gdoc v0.7.6 on 2026-06-11. Re-check
after `gdoc` upgrades before relying on exact subcommand limitations.

## Auth

`gmail.py`, `sheets.py`, `bin/gmail-maildir-sync`, and the shared `claude/bin/_gworkspace_auth.py` helper support the migrated secret layout. For the human accounts, the OAuth client (id and secret) is shared and only the refresh token differs per account. These variables are no longer globally exported from `.zshenv-secrets`; the wrappers still accept explicit env vars for one-off overrides, but normally resolve values from the stores below:

| Var | Account | Purpose |
|---|---|---|
| `GOOGLE_WORKSPACE_CLIENT_ID` | both | OAuth client ID (shared); stored in `pass` at `env/google-workspace-client-id` |
| `GOOGLE_WORKSPACE_CLIENT_SECRET` | both | OAuth client secret (shared); stored in `pass` at `env/google-workspace-client-secret` |
| `GOOGLE_WORKSPACE_REFRESH_TOKEN` | epoch | refresh token authenticated as `pablo@epoch.ai`; stored in 1Password at `op://Automations/Google Workspace OAuth - Pablo Epoch/credential` |
| `GOOGLE_WORKSPACE_REFRESH_TOKEN_PERSONAL` | personal | refresh token authenticated as `pablo.stafforini@gmail.com`; stored in `pass` at `env/google-workspace-refresh-token-personal` and injected by the local wrappers |

Pick the account with `--account epoch` (default), `--account personal`, or
`--account email-triage` on `gmail.py`; `sheets.py` supports the two human
accounts. The wrapper exchanges the refresh token for an access token and
caches per-account at `/tmp/gworkspace-access-token-<account>.json`.
`bin/gmail-maildir-sync` is Epoch-only and injects the Epoch Google Workspace
credentials before running the Python package used by mu4e. The email-triage
bot account uses the existing local OAuth credential file at
`~/.gmail-mcp-epoch/credentials/email-triage@epoch.ai.json`.

The Epoch refresh token also powers `gdoc --account epoch` (which has its own auth flow but uses the same OAuth client). The personal refresh token is used only by `gmail.py`/`sheets.py`; `gdoc --account personal` has its own token under `~/.config/gdoc/accounts/personal/`.

If a token expires (`invalid_grant` errors), regenerate it with `claude/bin/update-gworkspace-refresh-token --account <epoch|personal>`. The helper opens the OAuth browser flow, updates the appropriate backing store (1Password for the Epoch token, `pass` for the personal token), and prints only status metadata, never token values. The manual recipe under "Generating a new refresh token" remains available for unusual recovery cases.

## Tooling by account

### work account

Use `gmail.py --account epoch` for `pablo@epoch.ai`.

### email-triage bot account

Use `gmail.py --account email-triage` for `email-triage@epoch.ai`. This replaces the previous `gmail-epoch-triage` MCP server path for local agent workflows.

### personal account

No dedicated MCP server. Personal-account Docs/Drive go through `gdoc --account personal` (see "Tooling by service" above). Calendar through `gcalcli`. Gmail isn't wired.

### Built-in (claude.ai integrations)

- **claude.ai Gmail** — managed by Claude's own auth; not canonical for local Claude/Codex workflows
- **claude.ai Google Calendar** — same
- **claude.ai Airtable** — Epoch Airtable; use only when a task explicitly depends on the hosted connector surface

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

### Preferred helper

```bash
uv run --with google-auth-oauthlib --no-project claude/bin/update-gworkspace-refresh-token --account personal
```

Use `--account epoch` for the Epoch account. When the browser opens, sign in as the matching Google account: `pablo@epoch.ai` for Epoch or `pablo.stafforini@gmail.com` for personal.

### Manual recipe (both accounts)

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

When the browser opens, sign in as the account you want the token for: `pablo@epoch.ai` for `GOOGLE_WORKSPACE_REFRESH_TOKEN`, `pablo.stafforini@gmail.com` for `GOOGLE_WORKSPACE_REFRESH_TOKEN_PERSONAL`. The helper writes the Epoch token to 1Password at `op://Automations/Google Workspace OAuth - Pablo Epoch/credential` and the personal token to `pass` at `env/google-workspace-refresh-token-personal`.

After printing, update the corresponding backing store, then `rm /tmp/gworkspace-access-token-<account>.json` to flush the wrapper's token cache. For the personal account, use `pass insert -m env/google-workspace-refresh-token-personal`; for the Epoch account, update the `credential` field of `Google Workspace OAuth - Pablo Epoch` in the Automations 1Password vault.

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

### email-triage bot account (email-triage@epoch.ai)

The built-in OAuth flow previously used by the MCP server was port-conflict-prone. Generate tokens manually using `InstalledAppFlow` on a different port and write to the credentials directory:

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

After running, clear `/tmp/gworkspace-access-token-email-triage.json` so the CLI refreshes from the new credentials.

## Troubleshooting

### `invalid_grant` from gmail.py / sheets.py

The cached access token expired or the refresh token was revoked. Try in order:

1. `rm /tmp/gworkspace-access-token-<account>.json` (`<account>` is `epoch` or `personal` — match whichever account you ran with) and re-run the command. If a stale cache was the only problem, this fixes it.
2. If still failing, regenerate the refresh token (see "Generating a new refresh token" above), update the matching backing store (`op://Automations/Google Workspace OAuth - Pablo Epoch/credential` for epoch, `env/google-workspace-refresh-token-personal` in `pass` for personal), then clear `/tmp/gworkspace-access-token-<account>.json`.

### `invalid_grant` from `gdoc --account personal`

The personal-account OAuth token was revoked or aged out. Re-auth:

```bash
gdoc auth --account personal
```

A browser opens; sign in as `pablo.stafforini@gmail.com`. The OAuth client is in "testing" mode under GCP project `claude-code-gmail-490520`, so the personal email must remain in the project's **Test users** list (GCP Console → APIs & Services → OAuth consent screen → Audience).

### `access_denied` from `gdoc --account pablo.stafforini@trajectorylabs.net`

If OAuth testing blocks Trajectory Docs/Drive auth with `access_denied`, add
`pablo.stafforini@trajectorylabs.net` as a test user in the GCP project's OAuth
consent screen, then rerun:

```bash
gdoc auth --account pablo.stafforini@trajectorylabs.net
```

### gmail.py reports "ERROR: missing env var"

The wrapper could not resolve one of the backing stores. Check that `pass show env/google-workspace-client-id`, `pass show env/google-workspace-client-secret`, and the account-specific refresh-token store are available.

### `gmail.py --account email-triage` reports missing or expired credentials

The credential file is missing or the refresh token is expired. Regenerate using the manual flow above, write to `~/.gmail-mcp-epoch/credentials/{email}.json`, and clear `/tmp/gworkspace-access-token-email-triage.json`.

### Other issues

- **403 "caller does not have permission"**: either the relevant API isn't enabled on the GCP project, or the document isn't shared with the account the token belongs to.
- **MCP server not appearing in `/mcp`**: MCP servers go in `~/.claude.json`, NOT `~/.claude/settings.json`. The latter is for Claude Code settings (hooks, permissions, theme).
