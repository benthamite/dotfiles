# Secrets setup

- When reading secrets from 1Password or `.zshenv-secrets`, never echo or print them in the terminal or logs.
- **Where secrets live (2026-09-25 onwards)**: three 1Password accounts, one per owner. **Personal** (`my.1password.com`): `Private` for Pablo's own logins, `Shared` for family, `Automation` for secrets programs read unattended. **Tlon** (`tln-team.1password.com`): project vaults (`Core`, `Babel`, `EAN`, `RAE`, `LBDLH`, `Archive`) plus `Automation`. **Epoch** (`epoch-team.1password.com`): see below. A program reads only from an `Automation` vault (Epoch: `Automations`) through `op-automations [@personal|@tlon]`, whose read-only service-account tokens live in the login Keychain as `op-service-account/ACCOUNT-automation`; everything else needs a person. Emacs reads them with `auth-source-extras-op-get` and the `1password-automation` auth-source backend. New secrets go in the owner's account; one a program needs goes in that account's `Automation` vault as its own item.
- **Epoch project and automation runtime secrets**: use Epoch 1Password workflows such as `op://` references, checked-in or ignored `.env.op` templates, `op-automations run --env-file .env.op -- <program>`, and captured `op-automations read` calls. A `.env.op` holds `op://` references only, so the sensitive-read guard does not treat it as a secrets file (reading it, editing prose that names it and committing a message that mentions it all pass), and the secret-leak guard denies writing a literal secret into one. Tracked `.env.example`-style templates are exempt on the same grounds, and a quoted `git commit -m` message is inert text for the scan, so a commit message may name `.env` or `.env.local`. The secret-leak guard permits a closed list of broker command shapes whose stdout carries no credential and denies every other shape (see "1Password from an agent shell" below). If code or docs offer raw `op`, `.zshenv-secrets`, or ambient environment variables as a fallback for an Epoch runtime secret, treat that as suspect legacy wiring: diagnose and repair the broker path before using it.
- **Cached reads for programs on timers** (2026-09-30): the personal account (Individual/Families) allows about 1,000 service-account requests per 24 hours, failed ones included, and one LaunchAgent reading its credential every few minutes exhausted it on 2026-09-29. A job that runs more often than every hour or so reads with `op-automations [@ACCOUNT] cache read [--ttl DURATION] op://REF` (default TTL 24h; `DURATION` is seconds or `30m`/`12h`/`2d`) instead of `read`. It prints exactly what `read` would. A fresh entry is served from `${XDG_STATE_HOME:-~/.local/state}/op-automations/cache/` without touching 1Password or the Keychain; otherwise one fresh read replaces it atomically. A failed fresh read is never cached as a value and never falls back to an expired entry; it starts a 10-minute backoff for that reference in which `cache read` fails at once, so a job on a one-minute timer cannot keep spending the quota while it is exhausted. When a service rejects a cached credential (HTTP 401, `invalid_grant`), the caller runs `cache forget op://REF` (which clears the entry and its backoff) and reads again **once**; it must not loop. After rotating a cached value by hand, run `cache forget` for its reference (`update-gworkspace-refresh-token` does this itself). Current cached callers: `_gworkspace_auth.py` (so `gcalcli-epoch`, `gmail.py`, `sheets.py`), `gmail-maildir-sync`, `mbsync-parallel`/`mbsync-passcmd`, the launchd repo's Deco connectivity probe, and time-tracker's Home Assistant source. Interactive tools and one-off scripts keep using `read`.
  - **Security trade-off**: the cache holds plaintext secret values at rest, readable by any process running as Pablo. That does not widen same-user exposure much, because the same process could already run the broker and the Keychain grants its token without a prompt. What it adds: values persist on disk after the process ends (up to the TTL, or until `cache forget`), and a stale value survives rotation until its TTL expires or the caller's rejection handling forgets it. Mitigations: the directory is 0700 and files 0600 (umask 077), file names are SHA-256 hashes of account and reference so a listing reveals nothing, the directory is excluded from Time Machine and lies outside the Drive sync root, and the sensitive-read guards deny agents reading, grepping or naming it. Pablo chose this over plaintext environment variables in LaunchAgent plists (normally mode 0644 in `~/Library/LaunchAgents`, and shown by `launchctl print`). Only cache items from an `Automation` vault that a program consumes unattended; never cache a person's login.
- **Epoch 1Password bootstrap token**: the read-only Automations service-account token lives in the login Keychain as `op-service-account/epoch-automation`, where `op-automations` reads it. Do not generalize this to other Epoch runtime secrets.
- **Anna's Archive**: the member secret key is the `password` field of the item `annas-archive` in the Tlon `Automation` vault. Its consumers are `lib/python/paper_fetch.py` (behind `bin/paper-fetch` and the stafforini.com book downloader) and `annas-archive.el` (via `auth-source-extras-op-get`); each reads it in-process, never prints it, and sends it only to `annas-archive.*` hosts. The fast-download API answering `Not a member` is an account state (lapsed paid membership), not a key problem.
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
  secret put` / `ssh-add -` / `docker login --password-stdin`. The GitHub
  write guard accepts the `gh secret set` pipe only as exactly
  `BROKER read REF | gh secret set NAME -R OWNER/REPO` with a literal,
  allowlisted repo and nothing else in the command.
- `op-automations` may take its account selector first (`@personal`,
  `@tlon`, `@epoch`); the guard strips it and applies the same shapes, so
  `op-automations @personal read REF > file` passes and a bare
  `op-automations @personal read REF` is denied. Any other `@…` is denied.
- `op-automations [@ACCOUNT] cache read [--ttl D] REF` takes the same shapes
  as `read` (no `--out-file`); `cache forget REF` prints nothing and passes.
  The cache directory itself (`…/op-automations/cache`) is a sensitive path:
  reading, grepping or naming it in a command is denied.
- Clipboard → 1Password goes through the audited wrapper `op-clipboard-store` (pass `--token` for API tokens, so a terminal-wrapped copy with an inserted space is refused)
  (dotfiles `claude/bin`): shape check, `op-desktop item create|edit`, read-back,
  and only the `op://` reference on stdout. `pbpaste` itself stays denied in
  every agent-shell shape; a protected tool *name* is allowed only as a plain
  argument of a read-only text tool (`grep -rn pbpaste docs/`, `git log -S pbpaste`),
  see `hooks/lib-inert-mentions.py`. A bare `read`,
  `read 2>/dev/null`, `read | cat`, `> /dev/stdout`, `>&2`, or `echo "$X"`
  afterwards is denied.
- `item get|list … | jq <filter>` where the filter names only metadata keys
  (`id`, `title`, `category`, `vault`, `fields`, `label`, `purpose`, `type`,
  `tags`, timestamps); `jq .`, `.fields[].value`, `to_entries` and `--fields`
  are denied. Or redirect the JSON to a regular file.
- `document get … --out-file F`, `inject … --out-file F`; `item create|edit`
  without `--format`; `item delete`, `document create|edit|delete`, `vault
  create`; `whoami`, `vault|user|group list|get`, `vault user|group list`, `vault user grant|revoke`,
  `group user list`, `account list`, `document list`, `item template
  list|get`, `--status`, `--stop`.
- `op-desktop item share … --emails ADDRESSES`: a link restricted to named
  recipients opens only after they confirm a code sent to their address, so
  printing it leaks nothing. This is the standard way to hand someone a
  secret; sending the link is still a separate step Pablo approves.

Denied by name: `--reveal` anywhere, `item share` without `--emails` (the
link opens for anyone holding it), `signin` (`--raw` prints a
session token), `environment read`, `service-account create`, `connect …`,
`events-api create`, brokers inside `bash -c '…'`/`eval`, a broker named
anywhere in an interpreter program (a `python3 -` or `node` heredoc,
`python3 -c`, `perl -e`…) unless it is a recognized pathlib document edit,
since the guard cannot tell a string from a `subprocess` call; variable or
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
- **Prefer the promptless path when placing secrets.** Agents run read-only `Automations` operations through `op-automations`, which injects the read-only service-account token without a Touch ID prompt; a deliberately biometric operation against another vault uses `op-desktop`. Raw `op`, Keychain, and clipboard commands are blocked in agent shells, and broker commands must take one of the non-printing shapes above. The shell shim refuses to fall through when the broker is missing. If an automation needs to consume a secret repeatedly, weigh placing it in `Automations` at creation time; reserve `Employee`/other vaults for credentials only humans consume.
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
