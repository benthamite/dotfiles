---
name: store-secret
description: Create, update, or delete a secret in Epoch's 1Password Automations vault. The service account token in OP_SERVICE_ACCOUNT_TOKEN is read-only, so write operations must fall back to the desktop-app biometric auth flow. Use when the user says "store secret", "save credential", "add to 1password", "create 1password item", "add API key to vault", "new secret", or "put in automations". Also invoke whenever `op item create`, `op item edit`, or `op item delete` returns "You do not have permission to perform this action".
---

# Store, update, or delete an Epoch 1Password secret

Epoch secrets live in the **Automations** vault on the `epoch-team.1password.com` tenant. The service account token (`OP_SERVICE_ACCOUNT_TOKEN`) has `read_items` scope — it can read but never write. All write operations must go through the desktop app with biometric auth.

## Prerequisites

- 1Password desktop app running.
- CLI integration enabled in the app: Settings → Developer → "Connect with 1Password CLI".

## Create an item

Unset the service account token so `op` falls back to the desktop-app integration, then run `op item create`. A Touch ID / system auth prompt will appear in the 1Password app — approve it.

```bash
unset OP_SERVICE_ACCOUNT_TOKEN && op item create \
  --category="API Credential" \
  --title="<Service> - <scope>" \
  --vault=Automations \
  "field1[text]=<value>" \
  "field2[concealed]=<secret-value>"
```

Field type syntax: `name[text]=...` for plain fields, `name[concealed]=...` for masked/secret fields, `name[url]=...` for URLs.

## Update / delete

Same unset-then-run pattern:

```bash
unset OP_SERVICE_ACCOUNT_TOKEN && op item edit "<title>" --vault=Automations "field=<new-value>"
unset OP_SERVICE_ACCOUNT_TOKEN && op item delete "<title>" --vault=Automations
```

## Verify from the service account's perspective

After creating, confirm the service account can read the item back — this is what GHA workflows will do:

```bash
op read "op://Automations/<item-title>/<field-name>"
```

(This works with the service account token still exported, because read is within scope.)

## Naming conventions

- **Never use parentheses in item titles** — they break `op://` reference parsing.
- Format: `<Service> - <scope>` (e.g. `Wayback - SPN2`, `Anthropic - email-triage`, `Slack - Epoch Bot`).
- Field names: lowercase snake_case (`access_key`, `secret_key`, `webhook_url`), so `op://...` references match between Python env vars and the 1Password item.

## Vault rules

- **Only write to `Automations`.** Never create, edit, or delete items in shared vaults (Ops Automation, Epoch AI - All, Operations, Employee) without explicit user confirmation.
- If a task seems to require touching a shared vault, stop and ask.

## Reference references (how things resolve at runtime)

- In `.env.op` files, in MCP server `env` blocks, and in GitHub Actions workflows: `op://Automations/<item-title>/<field-name>`.
- Claude Code resolves `op://` references natively in MCP env blocks.
- At shell startup, `OP_SERVICE_ACCOUNT_TOKEN` is sourced by `.zshenv-secrets` from `pass epoch/1password-service-account-token`.

## Rotation

To rotate the service account token:

```bash
op service-account create "Epoch Ops Automation" --vault "Automations:read_items" --raw
```

Then update the `pass` entry:

```bash
pass edit epoch/1password-service-account-token
```

Open a new shell to pick up the new token.
