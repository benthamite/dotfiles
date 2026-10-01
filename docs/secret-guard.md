# Secret guard: evidence and rules for changing it

Read this before changing any guard or output redactor. Since 2026-10-01
every PreToolUse guard of both runtimes is one Python engine,
`lib/python/agent_guard` (one module per policy in `policies/`), entered
through `{claude,codex}/hooks/agent-guard.sh`; the output side is
`{claude,codex}/hooks/redact-tool-output.py` and `redact-secrets.sh`. The
engine replaced the shell guards decision for decision, apart from recorded
intended differences:
[plans/2026-10-01-guard-engine.md](plans/2026-10-01-guard-engine.md). The
redesign behind it is
[plans/2026-09-30-secret-guard-redesign.md](plans/2026-09-30-secret-guard-redesign.md).

## Rules

1. **No per-site exemptions.** The opaque-token heuristic (a network command
   carrying a long mixed-class string) no longer denies: where it would have
   been the only reason, `agent_guard/url_heuristic.py` appends one line
   (time, runtime, tool, label; never the value or command) to
   `~/.local/state/agent-guard/heuristic.log`. Its 42-route exemption
   registry is deleted, and `tests/test_secret_guard_scope.py` fails if one
   comes back. 63 of 76 guard commits from 2026-08-01 to 2026-09-30 were
   exemptions of this kind; they fixed nothing and made the guard slower. If a
   public URL is blocked for another reason, fetch it with WebFetch or the
   browser instead of adding an allowance.
2. **Never loosen a check that maps to a real incident** (table below). The
   opaque-token heuristic mapped to none, which is why it only logs. If the
   log ever points at a real secret, restore the denial: revert the commit
   "Guards: the opaque-token heuristic only logs" (`git log --grep`), which
   brings back the denial, the route registry and their tests, then add the exposure to the table.
3. **Every new exposure is recorded here** (a row in the table, no values),
   with the path it took. Changes to the guard must say which rows they close.
4. **Tests must stay fast.** Guard tests call the engine in-process through
   `tests/guard_harness.py`; add end-to-end runs of `agent-guard.sh` only to
   prove wiring. Run `bin/test-guards` (every guard test, ~35 s) before
   committing a guard change, `bin/test-guards --fast` while iterating, and
   `bin/test-guards --differential` after changing `agent_guard/ere.py` (the
   grep/sed emulation, compared with the real tools).
5. **A check that cannot run must deny.** Any exception in a policy denies
   (`engine.main` exits 2), and `agent-guard.sh` turns every non-zero status
   of the interpreter, including a failed import, into exit 2. Before adding an
   `except`, check what the old behaviour was and fail closed unless it was a
   deliberate fallback.

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
| 08-09 | GitHub password, 2FA recovery codes | a | password-manager CLI read behind a partial filter |
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
| 09-30 | Google OAuth client secret, refresh token | a | `which` printed a function from `.zshenv-secrets`; rotated 09-30 (old secret deleted, old token revoked); path closed by the function-body policy, 02f475152 |

Tally: page/screenshot 9, command output 7, file read 3, **typed literal 0**.

## Protection in place (2026-10-01)

- **Known values:** `bin/secret-index refresh` (Pablo, Touch ID) fingerprints
  every concealed 1Password field, `.zshenv-secrets` and known credential
  files; `op-clipboard-store` adds new secrets as it stores them. Commands and
  file writes carrying one are denied; tool output carrying one is masked.
- **Output, Claude:** every tool's result (Read, Grep, Bash, MCP, subagent tool
  calls...) is masked; fails closed.
- **Output, Codex:** shell and MCP results are masked via `decision: block`.
- **Screenshots:** dropped on pages in `agents/credential-pages.txt`, or when
  the page cannot be determined.

## Known gaps (2026-10-01)

- A background subagent's final report is not masked (no hook can rewrite
  it); its own tool calls are, so this needs a subagent that deliberately
  transforms a secret.
- Codex: chunks of still-running `exec_command` sessions and error results
  are masked only for credential-prone commands (`policies/codex_wrap`, the
  same rule as Claude's output wrapping); Codex's raw logs are not masked.
- Secrets that are new and not yet indexed, shorter than 12 characters, or
  stored somewhere the index does not read (the Keychain, browser storage)
  are recognised only by shape.
- A secret glued to the end of a word (`abcSECRET`) is not found.
- Each guarded tool call costs ~85 ms CPU in the engine (the shell guards
  used ~800 ms). Under 50 ms would need a resident process, rejected because
  it would keep enforcing old rules after a guard edit until restarted.
- The 2026-09-30 one-off Codex allow under load is unexplained but belonged to
  the shell guards' pipelines (`echo | grep -q` races, see intended difference
  15); the engine has no such pipelines.
