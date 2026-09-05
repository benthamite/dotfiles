---
name: profile-ai-cli-performance
description: Benchmark or profile fresh Claude Code and Codex response latency, startup cost, and clean-versus-configured conditions. Use for configuration, hooks, MCP, skills, plugins or first-output comparisons; not to infer provider superiority or run paid requests while merely auditing this skill.
user-invocable: true
---

# Profile AI CLI performance

Measure the requested client conditions without turning failures, changed
identities or incomparable events into latency claims. Audit-only work uses
synthetic processes; it does not authorize a live benchmark, credential access,
new login, account switch or changes to active configuration.

## Define and preflight the experiment

1. Inspect the current native CLI help and official documentation before choosing
   flags. Record selected binary paths and versions; do not assume the first PATH
   entry is the native binary rather than a local wrapper. Check only selected
   clients. Resolve helper paths relative to this skill's installed directory.
2. Predeclare the prompt, exact expected answer, selected conditions, run count,
   timeout, randomized/alternating block order, metrics and stopping rule. Default
   to ten measured attempts plus one preflight per condition. These are real
   provider requests during a live benchmark; internal retries or agentic turns
   can add cost. Keep the count and effects within the user's request.
3. Inspect startup hooks, tools, project instructions, inherited policy and
   outbound effects before launching configured processes. A fresh directory or
   process is not a sandbox. Claude safe mode still has built-in tools and normal
   permissions; admin policy can remain active. Codex read-only controls do not
   establish isolation of every service or startup hook. Do not weaken mandatory
   guards or restart/signal an active user session to obtain a sample.
4. Establish the intended same account, provider/endpoint, service tier, model
   and effort for each client's arms using safe non-secret metadata and relevant
   configuration. Model/effort flags alone do not pin the provider or tier.
   Record unavailable evidence rather than infer identity from a matching answer.
   Do not substitute another account, key, provider, model or startup proxy when
   one condition fails. Cross-client models are not assumed equivalent.
5. Apply the secrets context before any authorized credential handling. The helper
   never reads or copies Codex auth files. Its clean Codex arm requires the caller
   already to have selected API-key mode with `CODEX_API_KEY`; it preserves that
   same key in both child environments without printing it. It does not retrieve
   a key, provision one, or switch a subscription login to API billing. Without
   that existing mode, clean Codex is unavailable; configured-only timing can
   still use the native selected login. Never copy a refreshable or externally
   managed token bundle into a disposable home: refreshing it can invalidate the
   active copy, and deletion can discard the newest credentials. Never copy it
   back over active state as cleanup. The selected file/keyring store and account
   still require metadata review; the existence of an auth file proves neither.
6. For Claude, preserve the intended account namespace: `CLAUDE_CONFIG_DIR`
   also selects its macOS Keychain entry. Do not silently drop account/provider
   selectors or strip API credentials and call the resulting login the same
   baseline. The helper refuses unsupported/confounded modes rather than changing
   accounts; safe metadata review must still cover settings-based selections.

Authentication lifecycle and isolation contracts are documented in
[Codex account auth](https://learn.chatgpt.com/docs/auth/ci-cd-auth),
[Claude authentication](https://code.claude.com/docs/en/authentication), and
[Claude CLI flags](https://code.claude.com/docs/en/cli-reference).

## Run the supported conditions

Read `scripts/profile_ai_cli_performance.py --help` through Python before use.
Supply explicit models/efforts for selected clients and the configured project
path. `--only` names conditions; retaining only one arm permits its timing, not
that client's clean-versus-configured comparison. Empty or unknown selections
are errors, not a request to run every client.

Use `--dry-run` first to inspect commands without credential reads/copies or
client subprocesses. Treat custom prompts and project paths as potentially
private; a command preview is not automatically safe for a public report.
A dry run does not validate authentication, flag support or runtime isolation.
Use a new private output path, outside public repositories and Drive, when a
retained report is needed; never overwrite a prior report or follow a symlink.

Clean work/state directories belong outside repositories and Drive. Record
exact exclusions and remaining policy, environment and working-directory
differences. `--ignore-user-config` skips Codex's user config file, not every
instruction or policy layer. CLI feature flags can vary by version; unsupported
flags make the condition unavailable, not permission to silently omit them.
A separate MCP-list command cannot prove the measured process loaded no tools.

Run one preflight per selected condition. Require the exact answer, completed
answer-bearing message, native successful terminal event, exit zero, and valid
exposed isolation evidence. A correct intermediate message followed by failure
is not success. Codex uses `turn.completed`; Claude uses a successful `result`.
See [Codex JSON events](https://learn.chatgpt.com/docs/non-interactive-mode).
Stop the affected comparison on failed preflight. Use a separately declared
`--only` run for an unaffected client; do not relabel missing conditions.

Run the declared blocks through the helper. Preserve every preflight and
measured attempt with its condition, block and failure classification. Recheck
exposed isolation evidence for every sample, not only the first process.
Claude init output can expose MCP/tool evidence. The Codex JSON timing stream
may not expose a complete runtime inventory: record this as unobserved, not a
verified empty inventory. Contradictory isolation evidence invalidates the arm;
missing evidence limits which comparisons can be established.

Use bounded responsive host waits while the helper runs. Do not cherry-pick
retries until a cohort passes. Stop on the declared authentication/quota or
other invalidating condition, retain prior attempts, and report unavailable or
inconclusive results. A process timeout is not a successful slow response.

## Metrics and interpretation

Compare process-start to the completed answer-bearing assistant message and
process-start to observed exit. The helper's total includes pipe delivery and
parsing lag, but excludes subsequent cleanup; do not call it exact kernel exit
time or pure provider latency. Keep terminal-event time, first stdout line and Claude
partial-text time distinct. Codex completed-message JSON is not token-level
TTFT; do not compare it with Claude's first text delta as though equivalent.
A missing metric stays null with its own observation count, never zero or a
substitute timestamp from an unrelated message.

Report per-condition attempts, accepted samples, failures, median and sample
p95 for each observed metric. With ten samples, p95 is an unstable descriptive
estimate, not a precise population tail. For within-client contrasts, report
matched successful blocks and paired differences; disjoint successes provide
no paired estimate. Keep the full cohort visible, including failures excluded
from a latency summary. Success-only latency can be selection-biased.

Say that the observed conditions differed by X seconds. Fresh processes do not
establish cold caches or eliminate network/load/service-tier confounds. A
clean/configured contrast changes multiple layers, including cwd and environment;
it alone cannot attribute the difference to configuration, MCP schemas or a
provider's intrinsic speed. Match relevant conditions and run a scoped ablation
before a causal claim. For `--claude-mcp-ablation`, verify MCP exclusions and
hold other inputs fixed; the flag alone does not prove the only changed factor.

Helper completion means the selected timing cohort met its acceptance checks,
not that identity, every isolation layer or a causal explanation was proved.
If account/provider/tier cannot be matched, report per-condition observations
only and mark the intended comparison unavailable. Never disguise this with
an alternate client, model or local startup-only metric.

## Cleanup and reporting

The helper must bound and reap only its owned process groups, remove owned
temporary state and preserve unrelated files. Confirm cleanup, including any
private credential artifact actually created by a supported strategy. Never
repair cleanup by overwriting active auth/config, logging in again, or killing
an unrelated process. Report a material cleanup gap explicitly.

Keep raw provider errors, transcripts and credential values out of reports;
use sanitized failure classes. Preserve selected commands, versions, declared
cohort, per-attempt outcomes and limitations in private evidence. Lead the reply
with the measured result or unavailable comparison and the few facts that
change its interpretation; do not dump every event or present a failed run as
proof that another provider is faster.
