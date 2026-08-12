---
name: profile-ai-cli-performance
description: Use when benchmarking, profiling, or comparing Claude Code and Codex response latency, startup cost, clean-versus-configured performance, hooks, MCPs, skills, plugins, instruction loading, or time to first output.
---

# Profile AI CLI performance

Measure fresh Claude Code and Codex processes without turning failures or
incomparable events into latency claims. The primary question is normally
whether configuration explains an observed speed difference, not which provider
is intrinsically faster.

## Required workflow

1. Read current CLI help before selecting flags. Record binary paths and
   versions. Use the native Claude binary, not a local wrapper.
2. Resolve the active Codex identity from `${CODEX_HOME:-$HOME/.codex}`. Copy
   only its `auth.json` to the protected temporary clean home. Never assume
   `~/.codex` is active and never print credentials.
3. Pin one model and effort per client across its clean and configured arms.
   Do not describe the Claude and Codex models as equivalent.
4. Use a fresh directory outside any repository and Drive sync root for clean
   runs. Use Claude `--safe-mode`; use a temporary `HOME` and `CODEX_HOME` plus
   Codex isolation flags. Confirm init output contains no MCP servers or hosted
   tool names where the CLI exposes this evidence.
5. Run one untimed preflight for every condition. Require exit 0 and the exact
   expected answer. If authentication, quota, syntax, or isolation fails, stop
   that comparison and report the condition unavailable. Do not substitute
   another account, provider, model, proxy metric, or configured arm. When one
   client is unavailable, `--only` keeps the other client's clean-versus-
   configured comparison. The cross-client comparison stays unavailable.
6. Predeclare the run count, prompt, timeout, condition order, and metrics.
   Default to 10 measured runs per condition. Alternate or randomize conditions
   in blocks; never run every sample of one condition first.
7. Run `scripts/profile_ai_cli_performance.py`. Preserve every attempt and
   failure classification. Clean up benchmark-owned temporary state.
8. Attribute configuration cost with an ablation only when needed. For example,
   compare normal Claude with the same configuration plus
   `--strict-mcp-config` before claiming MCP schemas caused the difference.

## Metrics and claims

Use process start to completed assistant message and process start to exit as
the cross-client metrics. Claude partial content gives a separate first-content
measurement. Codex JSON may expose only a completed message; do not label that
event TTFT or compare it with Claude's partial token.

Report median, p95, raw sample count, failures, and configured-minus-clean
difference for each client. State the exact environment and CLI versions.

Allowed: “Under these conditions, configuration added X seconds.”

Not allowed: intrinsic provider superiority, cold-cache claims from fresh
processes alone, causal MCP claims without an ablation, or successful latency
for rejected, invalid, timed-out, or noncompliant requests.

## Script

Run `python3 scripts/profile_ai_cli_performance.py --help` before use. Supply
explicit models, effort levels, and the configured project directory. Use
`--dry-run` to inspect commands without model requests.

## Common mistakes

- Wrong Codex plan: active `CODEX_HOME` was ignored.
- Degraded task: one client failed, so a local startup proxy was substituted.
- Confounded arms: configured and clean runs used different model or effort.
- False TTFT comparison: Claude partial text was compared with a completed
  Codex message.
- False cause: all configuration was removed, then MCP alone was blamed.
- Dirty cleanup: temporary auth copies or benchmark processes were left behind.
