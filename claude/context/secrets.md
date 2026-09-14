# Secrets setup

- When reading secrets from `pass`, `1password` or `.zshenv-secrets`, never echo or print them in the terminal or logs.
- When using `pass`, always use full paths (e.g. `env/home-assistant-token`, not `home-assistant-token`). Never grep `pass ls` output — it strips directory context. Use `pass find <name>` to search.
- **Tlon organization secrets**: if a Tlon-related secret already has a `pass` entry under `tlon/`, keep that entry as the single source of truth. Do not duplicate it under `env/`.
- **Personal secrets** (non-Epoch): stored in `pass` (GPG-encrypted), normally under `env/` when the consumer conceptually needs an environment variable. **Epoch-related secrets live in 1Password**, usually in the `Automations` vault behind `op://` references; do not create duplicate Epoch entries in `pass`.
- **Epoch project and automation runtime secrets**: use Epoch 1Password workflows such as `op://` references, checked-in or ignored `.env.op` templates, `op-automations run --env-file .env.op -- <program>`, and captured `op-automations read` calls. The secret-leak guard permits a closed list of broker command shapes whose stdout carries no credential and denies every other shape (see "1Password from an agent shell" below). If code or docs offer raw `op`, `pass`, `.zshenv-secrets`, or ambient environment variables as a fallback for an Epoch runtime secret, treat that as suspect legacy wiring: diagnose and repair the broker path before using it.
- **Epoch 1Password bootstrap exception**: `OP_SERVICE_ACCOUNT_TOKEN` may live at `pass epoch/1password-service-account-token` only to enable read-only Automations access through `op-automations`. Do not generalize this exception to other Epoch runtime secrets.
- **Personal and Tlon `pass` workflows**: the Epoch runtime rule does not change personal non-Epoch `pass` entries or Tlon organization entries under `tlon/`.
- **Classifying duplicates**: inspect the relevant `pass` entry structure directly. Entries may store the real secret in named fields such as `key`, `gptel`, or service-specific labels rather than on the first line. Do not infer identity from the path or by comparing an environment variable to the whole `pass show` output.
- **Anna's Archive**: the member secret key is the Tlon `pass` entry `tlon/core/annas-archive` (first line). Its consumers are `lib/python/paper_fetch.py` (behind `bin/paper-fetch` and the stafforini.com book downloader) and `annas-archive.el` (via `auth-source-pass`); each reads it in-process, never prints it, and sends it only to `annas-archive.*` hosts. The fast-download API answering `Not a member` is an account state (lapsed paid membership), not a key problem.
- **Account-specific MCP secrets**: placement rules live in `context/mcp-servers.md`; treat resolved values as secrets and never print them.
- **Private GitHub file fetch**: `gh api contents` returns `download_url` with an inline `?token=...`, which the Bash secret hook rightly blocks for `curl`. Use the base64 content path instead: `gh api repos/OWNER/REPO/contents/PATH --jq .content | base64 -D > out`. For files >1 MB, use `gh api repos/OWNER/REPO/git/blobs/SHA --jq .content | base64 -D` or `gh api --paginate`.

## 1Password from an agent shell

Invoking 1Password through the brokers is the normal way to use a secret; what
the guard forbids is printing one. `claude/hooks/lib-op-policy.py` (paired for
Codex) classifies every broker command and allows only these shapes, denying
everything else including shapes it cannot place:

- `op-automations run --env-file F -- <program>`: injected values are masked in
  the program's output; the program may not be a shell or an environment
  dumper (`env`, `printenv`, `set`, `export`, `declare`, `typeset`), and no
  `OP_*` variable may be set on the command (`OP_RUN_NO_MASKING` disables
  masking). Because `F` is usually a `.env.op`, the sensitive-file guard also
  inspects this shape: the only composition it accepts is an optional leading
  `cd DIR &&`, leading `VAR=value` assignments (quoted values allowed) and file
  redirects; a `;`, `||`, pipe or `$(…)` anywhere else denies.
- `X=$(op-automations read REF)` used by a non-printing command in the same
  call; `read REF > file`; `read REF | pbcopy` / `gh secret set` / `wrangler
  secret put` / `ssh-add -` / `docker login --password-stdin`.
- Clipboard → 1Password goes through the audited wrapper `op-clipboard-store`
  (dotfiles `claude/bin`): shape check, `op-desktop item create|edit`, read-back,
  and only the `op://` reference on stdout. `pbpaste` itself stays denied in
  every agent-shell shape; a protected tool *name* is allowed only as a plain
  argument of a read-only text tool (`grep -rn pass docs/`, `git log -S pbpaste`),
  see `hooks/lib-inert-mentions.py`. A bare `read`,
  `read 2>/dev/null`, `read | cat`, `> /dev/stdout`, `>&2`, or `echo "$X"`
  afterwards is denied.
- `item get|list … | jq <filter>` where the filter names only metadata keys
  (`id`, `title`, `category`, `vault`, `fields`, `label`, `purpose`, `type`,
  `tags`, timestamps); `jq .`, `.fields[].value`, `to_entries` and `--fields`
  are denied. Or redirect the JSON to a regular file.
- `document get … --out-file F`, `inject … --out-file F`; `item create|edit`
  without `--format`; `item delete`, `document create|edit|delete`, `vault
  create`; `whoami`, `vault|user|group list|get`, `vault user|group list`,
  `group user list`, `account list`, `document list`, `item template
  list|get`, `--status`, `--stop`.

Denied by name: `--reveal` anywhere, `item share`, `signin` (`--raw` prints a
session token), `environment read`, `service-account create`, `connect …`,
`events-api create`, brokers inside `bash -c '…'`/`eval`, variable or
`command -v` indirection, process substitution, and raw `op` in any spelling
(Touch ID routing, next section). Reading a broker's *source* is fine: a
broker path handed to a read-only text tool (`cat bin/op-automations`,
`sed -n 1,50p ~/My\ Drive/dotfiles/bin/op-automations`) passes unless that
stage is piped into a shell or interpreter. The policy and its residual risks are in
`docs/superpowers/plans/2026-09-02-secret-guard-op-output-policy.md`; the case
table is `tests/test_op_policy.py`.

## Minimizing 1Password biometric prompts

Desktop-gated `op` operations (any write, and reads outside the `Automations` vault) cost the user one Touch ID approval **per shell invocation**. Treat each prompt as a real interruption of the user and design around it.

**Why this happens** (1Password's [app-integration security model](https://www.1password.dev/cli/biometric-security/), confirmed against the desktop app's own logs on 2026-07-28): on macOS an authorization is identified by the **controlling terminal — the tty plus its start time** — and survives 10 minutes of inactivity, 12 hours maximum, extending to every sub-shell in that terminal. It is revoked outright whenever the app locks, which `AutoLockMonitor` does on screen lock or sleep. Agent tool calls run each command in a shell with **no controlling terminal at all** (`tty` returns "not a tty"), so every invocation presents a new session identity and re-authorizes. Those repeat prompts are pure waste: the app log shows each one driving a system-unlock challenge that returns `NoNewAccountsUnlocked`, because the vault was already open.

- **Routing is automatic in zsh; scripts must still name a wrapper.** `shell/shims/op` routes each invocation by target — `op://Automations/…` and `--vault Automations` to `op-automations`, everything else to `op-desktop` — and `.zshenv` defines an `op` function that reaches it regardless of PATH. The function matters because the Claude Code and Codex Bash tools source a snapshot ending in a frozen `export PATH=…` that places `/opt/homebrew/bin` ahead of `shell/shims`; PATH order alone loses there, function definitions survive. **Child processes inherit neither**, so a script that shells out must name `op-desktop`/`op-automations` itself. A bare-CLI child process inherits the service-account context, which cannot read personal vaults, so a personal `--op-ref` from such a process produces a stray prompt and then a lookup failure. `tests/test_op_routing.py` fails on any bare-`op` caller or absolute-path invocation, so give a new caller the same routing property or a wrapper (see `op_reader_for()` in `claude/bin/ahrefs-api-guard` for the pattern). Note an ambient `OP_SERVICE_ACCOUNT_TOKEN` is deliberately *not* honoured for personal-vault requests — passing it through converts a routable request into an authorization failure.
- **Use `op-desktop` for every desktop-gated operation.** It keeps one detached broker process that owns a real pty and runs each `op` command inside that session, so they all share a single authorization: one prompt per 10-minute idle window instead of one per command. It forwards protected stdin through its mode-0600 broker socket, so piped item templates are supported. It does not extend 1Password's limits, it just stops us discarding the session between commands. **Measured 2026-07-28:** five `op` commands across four separate agent shells produced *one* biometric challenge in the 1Password log, against five before.
- **Inventory first, then batch.** List every desktop-gated operation the task will need (non-printing reads of personal vaults, item creates/edits, share links) before starting. Use `op-desktop` for each in an allowed shape; the broker keeps them in the same authorization window.
- **Never `op signin --force`.** It discards any live session and forces re-auth. `op-desktop` reuses its broker session automatically. If the broker is unavailable, stop and repair it rather than bypassing it with raw `op`.
- **Prefer the promptless path when placing secrets.** Agents run read-only `Automations` operations through `op-automations`, which injects the read-only service-account token without a Touch ID prompt; a deliberately biometric operation against another vault uses `op-desktop`. Raw `op`, `pass`, Keychain, and clipboard commands are blocked in agent shells, and broker commands must take one of the non-printing shapes above. The shell shim refuses to fall through when the broker is missing. If an automation needs to consume a secret repeatedly, weigh placing it in `Automations` at creation time; reserve `Employee`/other vaults for credentials only humans consume.
- **Warn before prompting.** Tell the user a Touch ID prompt is coming before running the command, so they're at the keyboard — a timed-out prompt means a retry, which is another prompt.
- **Steady state should be silent.** Prompt bursts are acceptable during one-time provisioning; if routine operation of an automation requires recurring biometric approval, that's a design smell — restructure so runtime reads come from `Automations`.

## Epoch 1Password vaults

Vault topology, what each vault holds, ownership, and the "where does a new
secret belong" rules are Epoch project documentation, so they live with the
Epoch workspace rather than here: use the `epoch-vaults` skill, and
`store-secret` for create/edit/delete.

They are deliberately not restated in this file. This repository is public, and
a description of an employer's vault layout, its finance and vendor credential
locations, and who owns each vault is an operational map of someone else's
organization. The routing rules above are all an agent needs to pick the right
wrapper; the placement rules belong to the workspace that owns them.
