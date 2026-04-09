---
name: security-audit
description: Audit development environment security posture — secrets hygiene, supply chain, machine hardening, and Claude Code attack surface. Use when the user wants to check their security setup, after reading about new threats, or periodically.
argument-hint: [--secrets] [--deps] [--machine] [--claude] [dir]
---

# Security audit

Audit the development environment for security risks across four domains. Run all four by default, or pass flags to audit specific domains.

If `dir` is provided, scope the `--secrets` and `--deps` domains to that directory. Otherwise, audit the current working directory plus well-known config locations (`~/.claude.json`, `~/.zshenv`, etc.).

Use subagents to explore in parallel where appropriate. Read actual files — don't guess from paths.

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

- **Known vulnerabilities**: run `npm audit` (Node), `pip audit` (Python), `cargo audit` (Rust), `gh api /repos/{owner}/{repo}/dependabot/alerts` (GitHub) as applicable. Report severity, CVE or advisory ID, and whether a fix is available.
- **Unpinned dependencies**: flag `^`, `~`, `>=`, or `*` version ranges in `package.json`, unpinned entries in `requirements.txt` or `pyproject.toml`. These are how supply chain attacks like axios and litellm propagate.
- **Lockfile integrity**: verify lockfiles exist and are committed. Flag repos that have a manifest but no lockfile.
- **Release age policy**: check for `min-release-age` in `.npmrc`, or equivalent protections. Recommend adding it if absent.
- **Dependency freshness**: flag dependencies more than 2 major versions behind — they likely have unpatched vulnerabilities.

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
3. **High**: issues that significantly increase attack surface (unpinned deps, unguarded tools, plaintext tokens in config)
4. **Medium**: gaps worth closing but not urgently exploitable (missing lockfiles, absent egress controls)
5. **Low**: hardening recommendations (release age policies, additional hooks)

For each finding, include:
- Location (file path and line, or system setting)
- What's wrong (be specific — "Slack token in plaintext at ~/.claude.json line 47", not "credentials could be more secure")
- Remediation (concrete command or change)

Never echo or paste the full secret value into the report. Redact secrets by default; mention the secret type, location, and at most a short fingerprint or prefix/suffix when needed to distinguish duplicates.

## Final section

End with an **overall risk posture** paragraph and offer to fix the critical and high findings.
