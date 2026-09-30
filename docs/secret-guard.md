# Secret guard: evidence and rules for changing it

Read this before changing any secret-protection hook
(`{claude,codex}/hooks/block-secret-leak.sh`, `lib-*.py`,
`redact-secrets.sh`, `redact-tool-output.py`, `block-sensitive-read.sh`,
`pretooluse-bash.sh`). The approved redesign is
[plans/2026-09-30-secret-guard-redesign.md](plans/2026-09-30-secret-guard-redesign.md).

## Rules

1. **No new per-site exemptions.** Do not add entries to `ROUTES` in
   `lib-public-url-scan.py` or other one-off allowances for a harmless command.
   `tests/test_secret_guard_scope.py` fails if the registry grows. If a public
   URL is blocked, fetch it with WebFetch or the browser instead. 63 of 76
   guard commits from 2026-08-01 to 2026-09-30 were exemptions of this
   kind; they fixed nothing and made the guard slower.
2. **Never loosen a check that maps to a real incident** (table below). The
   heuristic for secret-looking literals in commands maps to none; it is
   scheduled for removal only via the redesign's log-only trial.
3. **Every new exposure is recorded here** (a row in the table, no values),
   with the path it took. Changes to the guard must say which rows they close.
4. **Tests must stay fast.** New guard tests call classifiers in-process;
   add end-to-end hook invocations only to prove wiring.

## Incident inventory (agent exposures)

Compiled 2026-09-30 from dotfiles history, hook READMEs and session logs
(Claude logs from 2026-07-07, Codex from 2026-03-29). Path: (a) command
output, (b) file read, (c) agent typed the literal, (d) browser page or
screenshot.

| Date | Secret kind | Path | How |
|---|---|---|---|
| 05-16 | Asana tokens (Codex config) | b | a search printed the Codex config file |
| 07-21 | Slack app client/signing secrets | a | Slack create-app API response |
| 07-28 | Anthropic key | d | screenshot |
| 07-31 | Anthropic key | d | screenshot of key-reveal dialog |
| 08-09 | GitHub password, 2FA recovery codes | a | `pass show` behind a partial filter |
| 08-12 | OpenAI admin key | d | console page data |
| 08-18 | Anthropic key | d | screenshot |
| 08-19 | Asana token (Codex config) | b | file read during diagnosis |
| 08-19 | 1Password account password | d | browser check of a filled sign-in field |
| 08-19 | 1Password Secret Key, setup code | b | Emergency Kit PDF rendered |
| 08-20 | GCP service-account key | a | `op item list` showed a URL field holding the key |
| 08-23 | Home Assistant SMB password | a | `defaults read` of a Finder plist |
| 09-01 | Google refresh token | a/b | helper error wrote it to a log; log read raw |
| 09-02 | Anthropic key | d | echoed by browser tools |
| 09-10 | Slack bot token | d | screenshot |
| 09-27 | Cloudflare token | d | Chrome `find` echoed the field |
| 09-27 | Mullvad account number | a | `op-desktop item list` titles |
| 09-30 | Slack bot and user tokens | d | Slack settings page |
| 09-30 | Google OAuth client secret, refresh token | a | `which` printed a function from `.zshenv-secrets` |

Tally: page/screenshot 9, command output 7, file read 3, **typed literal 0**.

## Known gaps (2026-09-30)

- Screenshots are never masked.
- Codex masks no tool output.
- Claude does not mask Read or Grep output.
- The redactor recognises formats, not your actual values: hex Slack secrets,
  passwords in URLs and prefix-less tokens pass.
- Each Claude Bash call spends ~0.6 s in the guard (~120 subprocesses); the
  parity test file takes ~26 minutes.
