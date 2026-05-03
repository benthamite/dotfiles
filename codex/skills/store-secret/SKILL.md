---
name: store-secret
description: Create, update, or delete a secret in any of Epoch's 1Password vaults. The service account token in OP_SERVICE_ACCOUNT_TOKEN is read-only, so write operations must fall back to the desktop-app biometric auth flow. Use when the user says "store secret", "save credential", "add to 1password", "create 1password item", "add API key to vault", or "new secret". Also invoke whenever `op item create`, `op item edit`, or `op item delete` returns "You do not have permission to perform this action".
---

# Store, update, or delete an Epoch 1Password secret

Epoch's secrets live across multiple vaults on the `epoch-team.1password.com` tenant. The right destination depends on what the secret is (automation key vs. shared vendor login vs. handover credential, etc.); see the `epoch-vaults` skill for the topology and selection rules. Pick the vault first, then run the commands below.

The service account token (`OP_SERVICE_ACCOUNT_TOKEN`) has `read_items` scope — it can read but never write. All write operations must go through the desktop app's CLI integration regardless of which vault.

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
  --vault=<vault> \
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
  --category="API Credential" --title="..." --vault=<vault> \
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
env -u OP_SERVICE_ACCOUNT_TOKEN op item edit "<title>" --vault=<vault> "field=<new-value>"
env -u OP_SERVICE_ACCOUNT_TOKEN op item delete "<title>" --vault=<vault>
```

## Verify the item is reachable

Read it back via the same desktop session that just wrote it:

```bash
env -u OP_SERVICE_ACCOUNT_TOKEN op read "op://<vault>/<item-title>/<field-name>"
```

If the item lives specifically in `Automations`, the read-only service account token can also read it — useful to confirm a GitHub Actions workflow will see it without falling back to biometrics:

```bash
op read "op://Automations/<item-title>/<field-name>"
```

This second form fails for items in any other vault. That's expected: only `Automations` is reachable from the service account token.

## Naming conventions

- **Never use parentheses in item titles** — they break `op://` reference parsing.
- Format: `<Service> - <scope>` (e.g. `Wayback - SPN2`, `Anthropic - email-triage`, `Slack - Epoch Bot`).
- Field names: lowercase snake_case (`access_key`, `secret_key`, `webhook_url`), so `op://...` references match between Python env vars and the 1Password item.

## Choosing the vault

This skill does not pick a vault for you, and there is no default vault. Each Epoch vault has a specific purpose (automation creds vs. shared org logins vs. handover staging vs. personal-work creds, etc.) — pick based on what the secret is.

- **Defer to the `epoch-vaults` skill** for the topology and the "where does this kind of secret belong" mapping. It's the source of truth.
- **Strong precedent rule**: if a related credential already exists in a specific vault (e.g. an existing `Pablo AWS` entry in `API transition`), put the new related credential in the same vault. Don't split a logical group across vaults without a reason.
- **`Automations` is for one specific case**: API keys consumed by automation projects (GitHub Actions / scheduled jobs) that need the read-only service account token to fetch them at runtime. Putting non-automation creds (human IAM logins, vendor account passwords, handover items) there is a category error — even if it "works."
- **Confirmation rule**: only `Automations` and `Employee` are owner-write for Pablo. For any other vault (`Operations`, `Benchmarking`, `API transition`, `Epoch AI - All`, etc.), confirm the destination with the user before writing.
- **When unsure, ask** rather than guessing. There is no safe default.

## Sharing a secret with someone else

The right pattern depends on the secret's lifecycle. Pick **before** you create any 1P item — getting this wrong creates autofill pollution that's annoying to clean up.

| Lifecycle | Pattern |
|---|---|
| **Ephemeral / one-time** (e.g., AWS first-login temp password, password-reset link) — consumed on first use | **Send directly via Slack DM.** Do not create a 1P item. The credential dies on first use; persisting it in 1P adds nothing security-wise and pollutes Pablo's autofill (any item with a matching URL will surface in the browser extension). |
| **Long-lived, recipient is an Epoch staffer with 1P access** | **Use a shared 1P vault.** Move the item into a vault both parties have ACL on (`Operations`, `Benchmarking`, `API transition`, etc., per `epoch-vaults`). No share link, no expiry to manage — vault permissions do the work. |
| **Long-lived, recipient is external or doesn't have 1P access in the Epoch workspace** | Use `op item share`, but set it up so it doesn't pollute Pablo's autofill: (a) put the item in a vault that the browser extension excludes, OR strip the URL field on the item; (b) `--view-once --emails <recipient>`; (c) delete the item once the recipient confirms access. |

### Why the autofill pollution problem exists

1Password's share feature does not have a transient/ephemeral mode — every share link is backed by an item that lives in one of your vaults. The browser extension surfaces all items in vaults Pablo has access to whenever a URL matches. So creating `Giles Hawk AWS` in `API transition` to share it means Pablo's own AWS console sign-in pages now offer Giles's creds in the autofill dropdown, forever (or until the item is deleted). This is a real footgun — don't create persistent items for ephemeral creds.

### `op item share` flags

```bash
env -u OP_SERVICE_ACCOUNT_TOKEN op item share <item> --vault=<vault> [flags]
```

- `--view-once`: link expires after one view. Mutually exclusive with `--expires-in` (combining them returns "expiration cannot be set when the share is only one view").
- `--expires-in <duration>`: `7d` default. Accepts `s`/`m`/`h`/`d`/`w`.
- `--emails a@b.com,c@d.com`: restrict to specific addresses; recipients verify by email code. Combinable with `--view-once` or `--expires-in`.
- The CLI prints the share URL to stdout. Pipe to a file or capture in a variable; do not echo to logs you'll later quote.

### Cleanup after sharing

- **Item**: delete from the vault (`op item delete <id> --vault=<v>`) once the recipient confirms access.
- **Share link**: separately revocable via desktop app (item → ⌄ next to vault name → View Sharing History → Delete Link). Deleting the item invalidates pending share links anyway.

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

- `op://` reference format: `op://<vault>/<item-title>/<field-name>`. Used in `.env.op` files, MCP server `env` blocks, and GitHub Actions workflows.
- The `OP_SERVICE_ACCOUNT_TOKEN` is scoped to `Automations` only — workflows referencing other vaults need a different auth path (e.g. desktop session, or a different service account).
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
