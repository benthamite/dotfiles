---
name: store-secret
description: Create, update, or delete a secret in Epoch's 1Password Automations vault. The service account token in OP_SERVICE_ACCOUNT_TOKEN is read-only, so write operations must fall back to the desktop-app biometric auth flow. Use when the user says "store secret", "save credential", "add to 1password", "create 1password item", "add API key to vault", "new secret", or "put in automations". Also invoke whenever `op item create`, `op item edit`, or `op item delete` returns "You do not have permission to perform this action".
---

# Store, update, or delete an Epoch 1Password secret

Epoch secrets live in the **Automations** vault on the `epoch-team.1password.com` tenant. The service account token (`OP_SERVICE_ACCOUNT_TOKEN`) has `read_items` scope — it can read but never write. All write operations must go through the desktop app's CLI integration.

## Prerequisites (one-time, already set up on Pablo's machine)

- 1Password desktop app running (`pgrep -l 1Password` should show it).
- CLI integration enabled in the app: Settings → Developer → "Integrate with 1Password CLI".
- System auth (Touch ID) enabled: Settings → Security → Touch ID.

The signed-in personal account is cached in `~/.config/op/config` under `system_auth_latest_signin`. If it's populated, signin should not require interactive prompts.

## Establish a CLI session (required once per shell that will write)

Each Bash call starts a fresh shell with the service account token re-exported. To write, you need to first bypass that token **and** establish a desktop-app session:

```bash
env -u OP_SERVICE_ACCOUNT_TOKEN op signin --force
```

Empty output = success. The `op-daemon.sock` in `~/.config/op/` keeps the session alive across subsequent commands. You can verify with:

```bash
env -u OP_SERVICE_ACCOUNT_TOKEN op whoami
# Expected: URL, Email, User ID (Pablo's personal account — NOT "User Type: SERVICE_ACCOUNT")
```

If `op signin` gives an error like "1Password CLI couldn't connect to the 1Password desktop app", the CLI integration toggle in the app is off — ask the user to flip it on in Settings → Developer.

## Create an item

Pick the **category** first — the category determines the field syntax. Getting this wrong is the #1 source of errors. The supported `[fieldType]` tokens are: `text`, `concealed`, `password`, `email`, `url`, `phone`, `date`, `monthYear`, `otp`, `file`. **`[username]` is not a valid type** — it's a label baked into the LOGIN category.

### `--category=login` (web logins: username + password)

```bash
env -u OP_SERVICE_ACCOUNT_TOKEN op item create \
  --category=login \
  --title="<Service> - <scope>" \
  --vault=<vault> \
  --url="https://<service>" \
  "username=<email-or-handle>" \
  "password=<secret>"
```

`username` and `password` are **bare assignments** here, not bracketed — the category provides them as built-in fields. Adding `[text]` or `[concealed]` is wrong. Do **not** write `"username[username]=..."` (it will fail with `"username" is not a supported field type`).

### `--category="API Credential"` (API tokens, webhook URLs, etc.)

```bash
env -u OP_SERVICE_ACCOUNT_TOKEN op item create \
  --category="API Credential" \
  --title="<Service> - <scope>" \
  --vault=Automations \
  "field1[text]=<value>" \
  "field2[concealed]=<secret-value>"
```

Field type syntax: `name[text]=...` for plain fields, `name[concealed]=...` for masked/secret fields, `name[url]=...` for URLs.

### When in doubt

Run `op item create --help` and `op item template list` (then `op item template get <category>`) to see the exact field shape for any category. Don't guess.

### Handling values with special characters

Two safe patterns:

**Pipe through a bash variable** (works for most passwords; bash command substitution `$(...)` strips trailing newlines automatically):

```bash
SECRET="$(pbpaste)"
env -u OP_SERVICE_ACCOUNT_TOKEN op item create \
  --category=login --title="..." --vault=<vault> --url="..." \
  "username=<email>" \
  "password=$SECRET"
unset SECRET
```

**Temp file** (safer when the value may contain shell metacharacters like `$`, backticks, or quotes):

```bash
env -u OP_SERVICE_ACCOUNT_TOKEN op item create \
  --category="API Credential" --title="..." --vault=Automations \
  "access_key[text]=$(head -1 /tmp/keys.env)" \
  "secret_key[concealed]=$(tail -1 /tmp/keys.env)"
trash /tmp/keys.env
```

**Never** name a bash variable `$PWD` — it's the shell's current-working-directory builtin and will get clobbered.

### When the password came from the user's clipboard

`pbpaste` returns the macOS clipboard content. **Sanity-check before writing** — if length > ~100 bytes or includes newlines, it's almost certainly not the password (probably stale Emacs kill-ring content from `(kill-new ...)`, or a copied document):

```bash
printf 'len=%d nl=%d\n' "$(pbpaste | wc -c)" "$(pbpaste | grep -c '')"
```

If suspicious, ask the user to copy the password again before running `op item create`. Never display the contents to the user — even partial — to verify; trust the length+newline-count signal.

## Update / delete

```bash
env -u OP_SERVICE_ACCOUNT_TOKEN op item edit "<title>" --vault=Automations "field=<new-value>"
env -u OP_SERVICE_ACCOUNT_TOKEN op item delete "<title>" --vault=Automations
```

## Verify from the service account's perspective

Confirm the workflow will be able to read the item. The service account token is still exported, so a plain `op read` uses it:

```bash
op read "op://Automations/<item-title>/<field-name>"
```

## Naming conventions

- **Never use parentheses in item titles** — they break `op://` reference parsing.
- Format: `<Service> - <scope>` (e.g. `Wayback - SPN2`, `Anthropic - email-triage`, `Slack - Epoch Bot`).
- Field names: lowercase snake_case (`access_key`, `secret_key`, `webhook_url`), so `op://...` references match between Python env vars and the 1Password item.

## Vault rules

- **Only write to `Automations`.** Never create, edit, or delete items in shared vaults (Ops Automation, Epoch AI - All, Operations, Employee) without explicit user confirmation.
- If a task seems to require touching a shared vault, stop and ask.

## Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| `You do not have permission to perform this action` | Token set, write attempted with service account. | Prepend `env -u OP_SERVICE_ACCOUNT_TOKEN` to the command. |
| `account is not signed in` (with `env -u`) | No active desktop-app session. | Run `env -u OP_SERVICE_ACCOUNT_TOKEN op signin --force` first. |
| `1Password CLI couldn't connect to the 1Password desktop app` | App not running or CLI integration off. | Ask user to launch the app and enable Settings → Developer → "Integrate with 1Password CLI". |
| `native messaging: LostConnectionToApp` | App not running. | `open -a 1Password` and retry. |
| `assignment statement number N is not formatted correctly - "X" is not a supported field type` | You wrote `"X[X]=value"` thinking the bracket holds a label, but it's the field *type* (one of `text`, `concealed`, `password`, etc.). | For LOGIN: use bare `"username=..."` and `"password=..."`. For API Credential: use `"label[text]=..."` or `"label[concealed]=..."`. See "Create an item" above. |
| `op signin` exits 1 silently | Already signed in — exit 1 just means "did nothing because session is alive". | Verify with `op whoami` (under `env -u OP_SERVICE_ACCOUNT_TOKEN`); if it prints a user, you're good. |

## Reference: how things resolve at runtime

- In `.env.op` files, in MCP server `env` blocks, and in GitHub Actions workflows: `op://Automations/<item-title>/<field-name>`.
- Claude Code resolves `op://` references natively in MCP env blocks.
- At shell startup, `OP_SERVICE_ACCOUNT_TOKEN` is sourced by `.zshenv-secrets` from `pass epoch/1password-service-account-token`.

## Rotation (read-only scope)

To rotate the service account token:

```bash
op service-account create "Epoch Ops Automation" --vault "Automations:read_items" --raw
```

Then update the `pass` entry:

```bash
pass edit epoch/1password-service-account-token
```

Open a new shell to pick up the new token.
