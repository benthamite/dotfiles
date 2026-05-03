---
name: epoch-vaults
description: Reference for Epoch's 1Password vault topology and rules for where to read or store secrets. Use when discussing Epoch credentials, API keys, vaults, 1Password items; when running `op` commands against Epoch resources; when deciding where a new credential belongs; or when locating which vault holds a specific cred. For writes (create/edit/delete), the `store-secret` skill complements this one and handles the biometric-auth fallback.
---

# Epoch 1Password vault topology

Epoch's secrets are spread across 9 vaults Pablo has access to. The shape isn't accidental — different vaults serve different purposes, and the rotation/ownership rules differ by category.

## Vault inventory

| Vault | What lives here | Pablo's role |
|---|---|---|
| **Automations** | API keys consumed by Pablo's automation projects (GH Actions workflows). Read-only via `OP_SERVICE_ACCOUNT_TOKEN`. | Owner |
| **Ops Automation** | Mostly mirrors Automations (Anthropic project keys, OpenAI - Epoch, Slack bot, GA4, Ahrefs API, etc.) — likely the org-wide automation vault. | Read access |
| **API transition** | Ricardo's handover staging vault (9 items: Alibaba, Cohere, Deep Infra, Fireworks, Hyperbolic ×2, OpenRouter, Volcengine, X.ai). Created for the AI-access-management handover. | Inheriting ownership |
| **Operations** | Canonical home for shared org-account credentials (vendor logins, internal accounts, finance tools). Per the [SOP software subscriptions, accounts and login details](https://app.asana.com/1/1206049289330050/project/1207376717869892/task/1207971600938919) (María-owned). | Read access; write only with confirmation |
| **Benchmarking** | Benchmarking-team creds (DigitalOcean, StepFun, DeepSeek, Cerebras, Z.ai, Moonshot, Grok, Hetzner, ssh passwords for benchmark servers, etc.) | Read access; write only with benchmarking-lead confirmation |
| **Epoch AI - All** | Org-wide shared logins (Economist, NYT, WSJ, HetrixTools, etc.) — non-AI mostly. | Read access |
| **Comms and Social Media** | Social-media platform creds (likely Ricardo's comms portfolio). | Read access |
| **Edu Automations** | Edu's automation creds. | Read access |
| **Employee** | Pablo's personal vault for his own work credentials (Asana PAT, Wayback, GA4 service account key, etc.). | Owner |

## Where to put a NEW secret

The destination depends on what kind of secret it is:

| Kind of secret | Destination | Owner of rotation |
|---|---|---|
| **API key for an automation project** Pablo owns (GH Actions / scheduled job) | `Automations` | Pablo |
| **Centralized org-level provider key** (e.g., a new shared OpenAI org key, central Anthropic API key) | `Operations` (canonical) — coordinate with María | Pablo (post-handover), with consumer coordination |
| **AI provider login credential** during the Ricardo→Pablo handover | `API transition` (Ricardo's pattern) | Pablo |
| **Per-user API key** the user generated for their own work | The user's `Employee` vault | The user |
| **Benchmarking-run-scoped key** | `Benchmarking` (coordinate with benchmarking lead) | Named user; Pablo audits quarterly |
| **Shared vendor login** (non-AI: Mercury, Justworks, etc.) | `Operations` — coordinate with Ricardo | Ricardo (still owns non-AI vendor admin) |

## Hard rules

- **Never modify shared vaults** (Operations, Ops Automation, Epoch AI - All, API transition, Benchmarking, Comms and Social Media, Edu Automations) without explicit user confirmation. Reads are fine; writes need a check.
- **When setting up a new automation project that uses API tokens, persist them in 1Password immediately** — never leave them ephemeral.
- **Use `op` references in code/configs**, never inline secrets. Format: `op://Automations/<item-title>/<field-name>`.
- **Never** put secrets in plaintext `.env`, in repo files, or in chat logs.

## Reading vs. writing

- **Reads** use the read-only service account token (`OP_SERVICE_ACCOUNT_TOKEN`) — works in any shell, scoped to `Automations`. For other vaults, use `env -u OP_SERVICE_ACCOUNT_TOKEN op …` (desktop-app session, biometric).
- **Writes** require the desktop-app biometric flow → invoke the `store-secret` skill, which handles the `op signin --force`, item creation syntax, and naming conventions.

## AI provider key topology (for the AI-access-management handover specifically)

The post-handover SOP for API key rotation ([Asana](https://app.asana.com/1/1206049289330050/project/1207376717869892/task/1214426110687072)) breaks AI provider keys into 5 categories:

1. **Automation-project keys** — `Automations` + `Ops Automation`. Pablo owns rotation on quarterly cadence.
2. **Centralized org-level provider keys** — `Operations` (canonical), some overlap into `Automations`. Pablo owns rotation after coordinating with consumers.
3. **Per-user API keys** (named after the staffer, e.g., `Jaeho xAI API Key`, `Together API Tom March 2025`, `Moonshot API - Bret`) — `Operations` + `Benchmarking`. Owned by the named staffer; Pablo audits quarterly and rotates immediately on departure.
4. **Provider account login credentials** (LOGIN items: username + password for the web console) — `API transition` + parts of `Operations` / `Benchmarking`. Pablo owns; rotation = password change on suspected exposure or MFA hygiene.
5. **Personal-vault keys** — each user's `Employee` vault. Owner: the staffer. Out of scope for org-level rotation.

The canonical inventory of which provider has a credential where lives in the [LLM API providers tracker](https://docs.google.com/spreadsheets/d/1HQXRIRnvRqnyfvw-2NjKNHZcZxZ_vC7bCo4NhtAryAI/edit) — column R `Cred location (Pablo)` records the per-provider scatter.

## Pointers

- Writes / creates / deletes: invoke the `store-secret` skill.
- AI provider inventory: [LLM API providers tracker](https://docs.google.com/spreadsheets/d/1HQXRIRnvRqnyfvw-2NjKNHZcZxZ_vC7bCo4NhtAryAI/edit).
- Existing org SOP for accounts/subscriptions (María-owned): [Asana](https://app.asana.com/1/1206049289330050/project/1207376717869892/task/1207971600938919).
- API key rotation SOP (Pablo-owned, post-handover): [Asana](https://app.asana.com/1/1206049289330050/project/1207376717869892/task/1214426110687072).
