# Secrets setup

- When reading secrets from `pass`, `1password` or `.zshenv-secrets`, never echo or print them in the terminal or logs.
- When using `pass`, always use full paths (e.g. `env/home-assistant-token`, not `home-assistant-token`). Never grep `pass ls` output — it strips directory context. Use `pass find <name>` to search.
- **Tlon organization secrets**: if a Tlon-related secret already has a `pass` entry under `tlon/`, keep that entry as the single source of truth. Do not duplicate it under `env/`.
- **Personal secrets** (non-Epoch): stored in `pass` (GPG-encrypted), normally under `env/` when the consumer conceptually needs an environment variable. **Epoch-related secrets live in 1Password**, usually in the `Automations` vault behind `op://` references; do not create duplicate Epoch entries in `pass`.
- **Epoch project and automation runtime secrets**: use Epoch 1Password workflows such as `op://` references, checked-in or ignored `.env.op` templates, `op run`, or wrapper-specific `op read` calls. If code or docs offer `pass`, `.zshenv-secrets`, or ambient environment variables as a fallback for an Epoch runtime secret, treat that as suspect legacy wiring: diagnose and repair the 1Password path before using it.
- **Epoch 1Password bootstrap exception**: `OP_SERVICE_ACCOUNT_TOKEN` may live at `pass epoch/1password-service-account-token` only to enable read-only 1Password Automations access for `op read` / `op run` flows. Do not generalize this exception to other Epoch runtime secrets.
- **Personal and Tlon `pass` workflows**: the Epoch runtime rule does not change personal non-Epoch `pass` entries or Tlon organization entries under `tlon/`.
- **Classifying duplicates**: inspect the relevant `pass` entry structure directly. Entries may store the real secret in named fields such as `key`, `gptel`, or service-specific labels rather than on the first line. Do not infer identity from the path or by comparing an environment variable to the whole `pass show` output.
- **Account-specific MCP secrets**: placement rules live in `context/mcp-servers.md`; treat resolved values as secrets and never print them.
- **Private GitHub file fetch**: `gh api contents` returns `download_url` with an inline `?token=...`, which the Bash secret hook rightly blocks for `curl`. Use the base64 content path instead: `gh api repos/OWNER/REPO/contents/PATH --jq .content | base64 -D > out`. For files >1 MB, use `gh api repos/OWNER/REPO/git/blobs/SHA --jq .content | base64 -D` or `gh api --paginate`.

## Minimizing 1Password biometric prompts

Desktop-gated `op` operations (any write, and reads outside the `Automations` vault) cost the user one Touch ID approval **per shell invocation** — agent tool calls spawn fresh processes, so sessions never carry over between commands, and there is no cache the agent can extend. Treat each prompt as a real interruption of the user and design around it:

- **Inventory first, then batch.** Before touching `op`, list every desktop-gated operation the task will need (reads of personal vaults, item creates/edits, share links) and run them all in ONE shell invocation. Ten operations in one command is one prompt; ten commands is ten prompts.
- **Never `op signin --force`.** It discards any live session and forces re-auth. Use `op whoami >/dev/null 2>&1 || op signin` so an existing session is reused when one exists.
- **Prefer the promptless path when placing secrets.** Reads of `op://Automations/...` go through the read-only service-account token with no prompt ever. If an agent will need to read a secret repeatedly (runtime keys, test credentials for agent-driven verification), weigh placing it in `Automations` at creation time; reserve `Employee`/other vaults for credentials only humans consume.
- **Warn before prompting.** Tell the user a Touch ID prompt is coming before running the command, so they're at the keyboard — a timed-out prompt means a retry, which is another prompt.
- **Steady state should be silent.** Prompt bursts are acceptable during one-time provisioning; if routine operation of an automation requires recurring biometric approval, that's a design smell — restructure so runtime reads come from `Automations`.

## Main Epoch 1Password vaults

The Epoch tenant (`epoch-team.1password.com`) has several vaults; these are the three that matter most for routine work. For the full 9-vault topology and the "where does a new secret belong" rules, use the `epoch-vaults` skill; for create/edit/delete, use `store-secret`.

- **Automations** — secrets consumed by Pablo's automations (GitHub Actions / scheduled jobs), behind `op://Automations/...` references. The read-only `OP_SERVICE_ACCOUNT_TOKEN` can read this vault directly; other vaults need the desktop-app biometric session. Pablo is owner.
- **Operations** — shared org-account credentials: vendor logins, finance tools (e.g. Mercury cards), centralized org-level provider keys, and per-user provider keys. Shared/María-owned: reads are fine, writes need confirmation. (Note: this is *not* a catch-all for `ai-access-management` secrets — those are deliberately scattered across **Employee**, **Automations**, and **Benchmarking**, with only a few in Operations. Resolve any specific credential's location via the `epoch-vaults` skill or the LLM API providers tracker, not by assuming Operations.)
- **Employee** — Pablo's personal vault for his own work credentials. Pablo is owner (owner-write, like Automations).
