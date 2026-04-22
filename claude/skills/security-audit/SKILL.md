---
name: security-audit
description: "Scan for exposed API keys and leaked credentials, audit dependency vulnerabilities, review macOS hardening settings, and evaluate Claude Code configuration risks. Use when the user wants a security review, vulnerability scan, dependency audit, secrets check, or periodic security posture assessment."
argument-hint: "[--secrets] [--deps] [--machine] [--claude] [dir]"
---

# Security audit

Audit the development environment for security risks across four domains. Run all four by default, or pass flags to audit specific domains.

If `dir` is provided, scope the `--secrets` and `--deps` domains to that directory. Otherwise, audit the current working directory plus well-known config locations (`~/.claude.json`, `~/.zshenv`, etc.).

Use subagents to explore in parallel where appropriate. Read actual files — don't guess from paths.

## Threat model

Assume eventual supply chain compromise. User-level malware can read browser cookies and steal authenticated sessions, bypassing 2FA. Therefore **isolation** is the primary defense — checks that reduce blast radius (per-process secrets, browser separation, sandboxed installs) are weighted higher than prevention-only checks.

## Domains

### `--secrets` — Secrets hygiene

Scan for exposed credentials and secrets mismanagement.

**What to check:**

- **Plaintext secrets in config files**: scan `~/.claude.json`, project `.env` files, `launchd` plists, and similar for API keys, tokens, passwords, and OAuth secrets. Match common patterns:
  - AWS: `AKIA[0-9A-Z]{16}`
  - GitHub: `gh[ps]_[A-Za-z0-9_]{36,}`, `github_pat_[A-Za-z0-9_]{22,}`
  - Generic: `(api[_-]?key|secret|token|password)\s*[=:]\s*['"][A-Za-z0-9/+=_-]{20,}`
  - Slack: `xox[bporca]-[A-Za-z0-9-]+`
  - Private keys: `-----BEGIN (RSA |EC |OPENSSH )?PRIVATE KEY-----`
  - Do **not** flag public identifiers such as Google OAuth client IDs by themselves. Only flag actual secrets (client secrets, tokens, private keys, plaintext passwords).
- **Globally exported secrets in shell rc files**: scan `~/.zshrc`, `~/.bashrc`, `~/.profile`, `~/.zshenv`, `.zshenv-secrets`, and similar for `export` statements setting secret-bearing variables. Flag two distinct issues:
  1. **Literal secrets** (severity: **critical**): API keys, tokens, passwords, and private keys hardcoded as plaintext values in the file. Even if the file is git-crypt encrypted on disk, these values are loaded into the shell environment at startup and readable by every child process via `printenv`. Remediation: move each secret into `pass` or the OS keychain and inject per-process only (see item 2).
  2. **Globally exported secrets from encrypted stores** (severity: **high**): e.g. `export FOO=$(pass ...)` or `export FOO=$(op read ...)`. The secret isn't stored in plaintext on disk, but it's still injected into the global shell environment where every child process can read it. A single rogue dependency running `printenv` gets everything. Remediation: replace global exports with per-process injection — `envchain` (macOS Keychain, injects secrets only into the prefixed command's environment), per-process `op run --`, or inline `FOO=$(pass ...) command` so secrets never enter the global environment.
- **`.envrc` files**: if direnv is in use, check that `.envrc` files fetch secrets from a keychain (`pass`, `op`, `envchain`) rather than containing plaintext values.
- **Git-crypt coverage**: verify `.gitattributes` lists all files containing secrets. Check that encrypted files are actually encrypted in the repo (not committed in plaintext before git-crypt was configured).
- **`.gitignore` coverage**: verify `.env` files are gitignored. Flag any `.env` that is both present on disk and tracked by git.
- **Shell history**: check `~/.zsh_history` for leaked secrets in command arguments (e.g. `curl -H "Authorization: Bearer sk-..."`)
- **1Password integration**: if `.env.op` files exist, verify they use `op://` references rather than plaintext values. Check whether the resolved `.env` is gitignored.
- **Pass store consistency**: if `~/.password-store/` exists, flag any env variable or config value that contains a raw secret where a `pass` reference would be appropriate.

**What NOT to check:**

- Secrets in encrypted stores (pass, 1Password, Keychain) — these are fine by definition.
- `.zshenv-secrets` when properly git-crypt encrypted — verify encryption status, don't flag the file itself.

### `--deps` — Supply chain

Audit dependencies for known vulnerabilities and supply chain risk.

**What to check:**

- **Install scripts**: check the global `~/.npmrc` and any project-level `.npmrc` for `ignore-scripts=true`. If absent, flag as **high** — postinstall scripts are the most common npm supply chain vector. When a package legitimately needs build scripts (esbuild, sharp, etc.), it should be allowlisted explicitly (pnpm supports `onlyBuiltDependencies`; note any such allowlist rather than flagging it).
- **Python install-time code**: note that Python has no equivalent to `ignore-scripts` — `setup.py` can run arbitrary code at install time. For Python packages not fully trusted, recommend running installs inside a VM (OrbStack, Docker) or via Claude Code Web. Flag any `pip install` of non-PyPI or non-mainstream packages outside an isolated environment.
- **Known vulnerabilities**: run `npm audit` (Node), `pip audit` (Python), `cargo audit` (Rust), `gh api /repos/{owner}/{repo}/dependabot/alerts` (GitHub) as applicable. Report severity, CVE or advisory ID, and whether a fix is available.
- **Unpinned dependencies**: flag `^`, `~`, `>=`, or `*` version ranges in `package.json`, unpinned entries in `requirements.txt` or `pyproject.toml`. These are how supply chain attacks propagate. Check that CI and install commands use `--frozen-lockfile` (pnpm), `npm ci` (npm), or equivalent to prevent lockfile drift.
- **Lockfile integrity**: verify lockfiles exist and are committed. Flag repos that have a manifest but no lockfile.
- **Release age policy**: check `.npmrc` for `minimum-release-age` (the canonical setting name, supported natively in npm 11.10+, pnpm 10.16+, yarn, bun, and deno). Recommend `minimum-release-age=3d` if absent — most malicious packages are caught and pulled within hours, so a 3-day delay is cheap insurance. For Python, note `uv --exclude-newer` as the equivalent.
- **Dependency freshness**: flag dependencies more than 2 major versions behind — they likely have unpatched vulnerabilities.
- **Socket.dev** (GitHub repos): for repos with dependencies hosted on GitHub, check whether Socket.dev is installed as a GitHub App (reviews PRs for suspicious package behavior: obfuscated code, network calls at install time, unexpected filesystem access). Recommend it if absent.

**What NOT to check:**

- Transitive dependency vulnerabilities with no upgrade path (note them but don't flag as directly actionable unless the repo can mitigate them another way).
- Do **not** automatically downgrade dev-only dependencies. In a development-environment audit they still run on the developer machine and can be a real supply-chain path. Lower severity only when you can explain why the vulnerable package is isolated from actual developer workflows.

### `--machine` — Machine posture

Audit macOS system security configuration.

**What to check:**

- **Software updates**: `softwareupdate -l` for pending updates. Flag any pending security update.
- **FileVault**: `fdesetup status` — must be enabled.
- **Firewall**: `/usr/libexec/ApplicationFirewall/socketfilterfw --getglobalstate` — should be enabled.
- **Gatekeeper**: `spctl --status` — must be enabled.
- **SIP**: `csrutil status` — must be enabled.
- **Lockdown Mode**: check `defaults read /var/db/SystemPolicyConfiguration/mdm MDMProfileIdentifier 2>/dev/null` or look for indicators in system settings. Lockdown Mode disables JIT compilation in Safari, blocks unknown USB accessories, and restricts message attachment types. Recommend enabling it (System Settings > Privacy & Security > Lockdown Mode). Note: web fonts may not display correctly in Safari; Chrome is unaffected.
- **Code execution isolation**: check whether OrbStack, UTM, Docker Desktop, or another VM tool is installed for sandboxing untrusted code. Per the threat model, running untrusted code (npm/pip installs, experiments) on the primary machine is the highest-risk activity. Recommend Claude Code Web for npm/pip work where possible (code executes on Anthropic's infrastructure), and a Linux VM for everything else. Flag if no isolation tooling is found.
- **Browser profile separation**: check whether Chrome has multiple profiles (inspect `~/Library/Application Support/Google/Chrome/` for `Profile *` directories) or whether multiple browsers are in use. If all browsing (dev, email, banking) happens in one profile, flag as **medium** — a cookie theft from the dev browser should not yield email/financial sessions. Recommend: one browser or profile for email/finances, another for development.
- **Google Advanced Protection**: cannot be checked programmatically, but include as a **low** recommendation if not previously acknowledged — enrollment takes 5 minutes (phone as passkey, no hardware key needed), restricts third-party app access to Gmail/Drive, hardens account recovery.
- **Password manager**: check whether a password manager is installed (`/Applications/1Password*.app`, `Bitwarden.app`, `KeePassXC.app`, or similar) and whether the corresponding browser extension is present. Flag as **high** if no password manager is found — unique passwords per service limit the blast radius of any single credential breach.
- **Hardware security keys**: note as a **low** recommendation if not already in use — YubiKeys upgrade phishing defense from phone passkeys to hardware keys.
- **SSH keys**: flag keys without passphrases (`ssh-keygen -y -P "" -f <key>` returns 0 = no passphrase). Flag `~/.ssh/authorized_keys` entries that are unfamiliar.
- **Browser extensions**: list installed extensions for Chrome and Firefox. Flag extensions that are not from major, well-known publishers (per Kim's Glasswing guidance: "be skeptical of lesser-known browser extensions").
- **Network egress**: check for an outbound firewall (Little Snitch, LuLu, or Radio Silence). Report its status. If none is installed, note the absence as a gap.
- **Login items**: `osascript -e 'tell application "System Events" to get the name of every login item'` — flag unfamiliar entries.
- **Remote access**: verify Screen Sharing and Remote Login are disabled unless intentionally enabled.

**What NOT to check:**

- Application-level security (use `/audit-mac-app` for that).
- MDM status (organizational concern, not individual).

### `--claude` — Claude Code attack surface

Audit the Claude Code configuration for security risks.

**What to check:**

- **Permission mode**: report whether `bypassPermissions` is active. If so, flag the absence of compensating controls (hooks that enforce safety). Calculate the "unguarded surface" — tools that have no PreToolUse hook and run without permission prompts.
- **MCP server credentials**: scan `~/.claude.json` for plaintext tokens and secrets in MCP server `env` blocks. Recommend moving them to env variables sourced from an encrypted store.
- **Hook coverage**: map which tool categories have PreToolUse guards and which don't. Flag gaps where a hook could prevent secret exfiltration or destructive actions.
- **CLAUDE.md safety instructions**: identify safety-relevant instructions (e.g. "never echo secrets") that are not backed by an enforced hook. These are suggestions, not controls — a prompt injection can bypass them.
- **Skill permissions**: scan skill SKILL.md files for instructions that grant broad access (e.g. "run any command", "bypass checks"). Flag skills that could be exploited via prompt injection to escalate privileges.
- **Memory exposure**: check whether memory files contain secrets, credentials, or sensitive information that could leak into future sessions.

**What NOT to check:**

- MCP server functionality (only credential storage).
- Claude Code version currency (managed by Anthropic).

## Output format

For each domain, produce:

1. **Status**: one-line summary (e.g. "3 critical, 2 high, 1 medium")
2. **Critical**: issues requiring immediate action (exposed secrets, active vulnerabilities, disabled security features)
3. **High**: issues that significantly increase attack surface (unpinned deps, unguarded tools, plaintext tokens in config, untrusted code running without isolation)
4. **Medium**: gaps worth closing but not urgently exploitable (missing lockfiles, absent egress controls, single-profile browsing)
5. **Low**: hardening recommendations (release age policies, additional hooks, hardware keys, Advanced Protection)

For each finding, include:
- Location (file path and line, or system setting)
- What's wrong (be specific — "Slack token in plaintext at ~/.claude.json line 47", not "credentials could be more secure")
- Remediation (concrete command or change)

Never echo or paste the full secret value into the report. Redact secrets by default; mention the secret type, location, and at most a short fingerprint or prefix/suffix when needed to distinguish duplicates.

## Final section

End with an **overall risk posture** paragraph and offer to fix the critical and high findings.
