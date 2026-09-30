# Secret guard redesign (proposal, 2026-09-30, revised after review)

Status: approved by Pablo on 2026-09-30. Not yet implemented; start with item 0.

## Evidence

From the incident inventory (19 agent exposures, 2026-03 to 2026-09-30) and the
guard map, both produced 2026-09-30:

| Exposure path | Incidents | Current coverage |
|---|---|---|
| Browser page / screenshot | 9 (≥5 screenshots) | Page text masked in Claude only; screenshots never |
| Command output | 7 | Partial: op broker, ps, function bodies (uncommitted); redactor knows ~24 formats; Codex output unmasked |
| Agent read a file | 3 | Credential paths blocked; Read/Grep output not masked |
| Agent typed a secret into a command | **0** | Target of the opaque-token heuristic and the 42-entry public-route registry |

- 63 of 76 guard commits since August were false-positive fixes, almost all
  from the input heuristic. Only 3 of 19 incidents led to any guard change.
- Every Claude Bash call pays ~0.6 s (≈2.2 s if hooks resolve `python3`
  through the pyenv shim): ~120 child processes, 5–7 Python interpreters.
- The parity test file makes ~2,900 full hook invocations: 26 minutes.
- 15 guard tests fail on the current working tree.

## Principle

Put strictness where secrets actually leaked (output, pages, file reads);
remove only what demonstrably caught nothing, and only after logging proves it.

## Design

0. **Baseline.** Commit or drop the uncommitted guard work in the tree
   (function-body policy, redactor patterns, both `block-secret-leak.sh`), make
   the suite green, and build a fast in-process test harness (classifier
   called as functions; ~20 end-to-end hook runs per runtime; target <30 s).
   Every later step is tested against this.

1. **Mask secrets by known value, not only by shape.**
   - *Source:* a `secret-index refresh` command Pablo runs (Touch ID once)
     reads concealed fields from 1Password (Private + Automation vaults),
     `.zshenv-secrets`, and credential files under `~/.config/{gcloud,gh,
     exercism,Jackett,taiga,internetarchive,*-mcp,qBittorrent}`, `cookies.txt`.
     Hooks never call 1Password. `op-clipboard-store` and the brokers add new
     values to the index when a secret is stored, so fresh tokens enter at
     creation.
   - *Storage:* keyed hashes (HMAC; key in the Keychain), concealed fields
     only, minimum length 12. Values shorter than that (recovery codes) rely
     on items 2–4 instead.
   - *Matching:* rolling-hash prefilter per stored length, confirmed by HMAC;
     benchmark on a 1 MB output before adoption (target <20 ms).
   - Keep the prefix regexes (`ghp_`, `sk-`, `xox?-`, `AKIA`, `GOCSPX-`,
     `ya29.`, `1//`, PEM, …) for secrets not yet indexed.
   - Denying commands/writes that contain a known value is a new
     false-positive source; it only denies, never rewrites, and names the
     matched item's title in the message.

2. **Mask output on every channel, failing closed.**
   - Claude PostToolUse `updatedToolOutput` for Bash, MCP, Read, Grep, Glob,
     WebFetch, Agent results, background-task and Monitor output. One test per
     tool shape passes a planted fake secret through and asserts it comes out
     masked (a wrong shape is silently ignored by Claude Code, so this test is
     the only proof). If the redactor errors, the output is replaced by an
     error notice, not passed through. Images and PDFs from Read cannot be
     masked: Read of those under credential paths stays blocked.
   - Codex: PreToolUse `updatedInput` wraps Bash commands through the redactor
     (needed for long-running `exec_command` sessions), plus PostToolUse
     `decision: "block"` replacing results for other tools, including the
     bundled Chrome, computer-use and node_repl tools. Verify both on Codex
     0.158.0 before relying on them.

3. **Stop screenshots of credential pages.** Hooks see only a tab id, and a
   batch can navigate and screenshot in one call, so the check runs on the
   result: a PostToolUse hook on `computer`, `browser_batch`, `zoom` and
   `gif_creator` looks up the tab's current URL (via Chrome AppleScript; verify
   the extension's tab id maps to it) and strips image blocks when the URL
   matches the credential-page list (API-key, token, OAuth-settings and
   1Password pages for Anthropic, OpenAI, Slack, Cloudflare, GitHub, Google;
   new providers added to one tracked file). If the URL can't be determined,
   the image is stripped.

4. **Keep the "don't print secrets" blockers.** 1Password broker policy,
   process listings, shell function bodies, credential-file reads.

5. **Retire the opaque-token heuristic and the route registry — last, and
   only on evidence.** After items 1–3 ship, run the heuristic in log-only mode
   for 30 days (labels only, no values). Delete it and the 42 routes only if
   it logged no real secret. Rationale: it has caught no incident, and it
   cannot stop deliberate exfiltration anyway (encoding, splitting, or sending
   `$TOKEN` bypass it); real exfiltration control would need an outbound
   network allowlist, which neither design has.

6. **One fast process per runtime.** Replace the bash dispatcher and its ~120
   subprocesses with one Python hook per event, invoked by absolute
   interpreter path, shared by Claude and Codex. It absorbs the
   destructive-command, GitHub-write and untrusted-execution checks
   unchanged. Target <50 ms per call.

## Incident coverage

| Incident path | Closed by |
|---|---|
| Screenshots (≥5) | 3 |
| Page text (Chrome `find`, page data) | 2 (Claude MCP, Codex browser tools) + 1 |
| Command output (API responses, `op item list`, `which`, `defaults read`, `exercism --show`) | 1 + 2 + 4 |
| File reads (Codex config, logs, Emacs autosaves) | 1 + 2 (Read/Grep masking) |
| 2026-09-30 `which` → OAuth secret | 4 (function-body policy) + 2 |

## What becomes less strict (after item 5 only)

An agent typing into a network command a secret that is (a) not in the index —
new since the last refresh, under 12 characters, or from a source the index
doesn't cover — and (b) has no known prefix. This includes copying a freshly
revealed token from a page into `curl`, if it was never stored. No past
incident took this path.

## Order

1. Rotate the Google OAuth client secret and Epoch refresh token exposed
   2026-09-30 (Pablo).
2. Item 0 (baseline, fast tests).
3. Items 1–2 (known-value index, full output masking). No loosening.
4. Item 3 (screenshots).
5. Item 6 (single process), behaviour-preserving.
6. Item 5 (log-only heuristic for 30 days, then decide).
