---
name: security-audit
description: "Audit development-environment secrets, supply chains, macOS security controls, Claude/Codex agent permissions, online-account takeover exposure (breaches, Watchtower, recovery paths), and personal security practices (browsers, messaging, phone, home network, habits). Use for environment security reviews, secrets checks, dependency audits, or periodic security posture assessments; use specialist skills for app-binary assessments or application-code reviews."
---

# Security audit

Audit the development environment and explain the exposure each finding
establishes. Honor the user's natural-language scope: a dependency-only request
selects that domain without requiring a literal flag. Run all six domains only
for a comprehensive environment audit. Explicit selectors are `--secrets`,
`--deps`, `--machine`, `--agents`, `--accounts`, or `--habits`. Keep `--claude` as a compatibility selector for
the Claude-only portion of `--agents`. These are skill-request selectors, not
flags for a bundled all-domain scanner.

When reviewing or testing this skill itself, inspect its artifacts and use only
synthetic fixtures. Do not run the domains against the real machine,
credentials, histories, account state or live agent configuration. Inspect test
entry points before running them; related repository suites include live checks.

An optional directory scopes secrets and dependency checks to that directory.
Without one, use the current directory plus configuration locations relevant
only to the selected domains. Machine checks concern the current Mac and, from
Pablo's answers, the other always-on devices on his network; agent checks
cover installed agents, their active account/profile configurations, and the
current project's overrides; account checks cover the online accounts and email
addresses resolved under [accounts](references/accounts.md); the practices
interview covers Pablo's own answers. State these roots before scanning; do not silently
expand into unrelated repositories or accounts.

## Profile

`~/repos/notes/mac-hardening/security-profile.md` is the standing record of
how Pablo's setup is configured: each control and practice a run or a
remediation has established, its state, the date, and whether it was seen or
told. The dated reports beside it are history; the profile is the current
state. It is private: nothing from it goes in this skill. Another person's
Mac has its own profile in that folder, named for the target.

Read the profile's sections for the selected domains before the first check.

- Do not ask Pablo about, or recommend, anything the profile records as in
  place, accepted or not applicable. Ask only for what it lacks or marks
  unknown, and recommend only what it does not show as done.
- An entry never replaces a read the session can make without him. Read that
  state every run. When it contradicts the profile, a control has regressed:
  report the finding and correct the entry.
- An entry that rests on Pablo's word stands until something observed
  contradicts it, or the thing it describes is replaced (a new phone, router,
  carrier or account). Then ask again, about that thing only.
- Raise an accepted item again only when a fact behind the decision changed,
  and say which fact. A deferred item is raised once per run, in one line.
- Update the profile before ending any run or remediation: each new
  observation, answer and decision replaces the old entry and its date. When
  Pablo says he has done something, record it then. An answer left only in a
  report or the conversation will be asked again.
- Record kinds of place only, never a secret, code or passphrase.

If the file is missing, rebuild it from the reports in that folder before
asking anything.

## Domains

Read the selected domain references completely before running their checks:

| Domain | Required reference |
|---|---|
| Secrets and credential incidents | [secrets](references/secrets.md) |
| Dependencies and other executable supply chains | [supply chain](references/supply-chain.md) |
| macOS controls, browsers and mail clients, isolation, recovery, Wi-Fi and other devices on the network | [machine](references/machine.md) |
| Claude, Codex, MCP, plugins, and browser automation | [agents](references/agents.md) |
| Online accounts, messaging and social accounts, the second-factor phone, breach and public exposure, and recovery paths | [accounts](references/accounts.md) |
| Practices and physical security, by interview | [habits](references/habits.md) |

Before authorized remediation of an online account, also read
[remediation](references/remediation.md). It lists vendor steps with
easy-to-miss side effects: revoked connectors, a second factor left in place,
keys a password change does not rotate.

The machine, accounts and habits references carry every Essential item of the
[Personal Security Checklist](https://digital-defense.io/). When that checklist
changes, or before moving or removing a check it supplied, read
[checklist coverage](references/checklist-coverage.md).

Independent domains may use subagents. Give each the resolved scope, relevant
reference, and all safety boundaries below. Deduplicate findings that cross
domains.

## Safety boundaries

An audit is read-only unless the user authorizes remediation. Do not rotate
credentials, change settings, install audit dependencies, resolve/build project
dependencies, activate project code, or mutate accounts while auditing.
Authorized remediation remains limited to the requested targets and actions.

Preserve the owner's guard policy. Repairing enforcement of an existing rule
does not authorize changing what the rule permits. A different rule requires
an explicit decision on that proposed policy; existing authorization for the
same change suffices. In Pablo's configuration, guards must allow or deny,
never introduce interactive approval prompts.

Before credential-related work, read
`~/repos/dotfiles/claude/context/secrets.md`. Use its personal/Epoch
placement and broker rules; this skill does not redefine them. For service
access use `claude/context/service-access.md`; for MCP placement use
`claude/context/mcp-servers.md`, resolved under that dotfiles root.

Never emit matching secret values or raw config/history lines. Use an approved
local scanner or metadata-only helper that reports path, line, type, and a
keyed fingerprint when necessary. Do not open vault contents, private keys,
browser cookies/session databases, or credential files for content inspection.
The bundled shell classifier supplies value-free output for shell-file reads
permitted by the guard; other checks must stay within the existing read policy. A
metadata-only output is not permission to bypass a denied input read.
Do not source a target shell file, load an editor config, run `direnv allow`,
start an MCP server, or execute a project-supplied scanner to inspect it.

The prohibition covers secret values, not security state. Read non-secret
security-state metadata the session can already read, such as macOS privacy
grants (`scripts/tcc-grants.py`), firewall rules and launch-item registrations;
skipping such a read leaves a consequential control unexamined and protects
nothing. Never acquire access to complete a read. When the OS denies one, ask
Pablo for the equivalent System Settings view before marking it not checked.

Inspect a check's execution, output, and network effects before running it.
Capture and filter potentially sensitive stdout **and stderr** locally before
tool output reaches the transcript. Do not send secrets, config files, or
private dependency metadata to a remote scanner; verify the destination and
data scope for advisory queries. All temporary captures and sanitized artifacts
must be private (0700 directories, 0600 files), outside Drive, and cleaned up.
Do not treat redaction as permission to acquire a prohibited input. Mark
unavailable, unsafe, denied, or unsupported checks as not checked, with the
concrete reason. Do not weaken a guard to complete a check.

## Threat model and evidence

Assume a dependency or agent workflow could execute malicious code. Assess
which files, authenticated services, devices, and credentials that process can
reach, and how the owner would recover. Consider credential theft/phishing,
physical loss, persistence, and prompt injection where relevant.

OS permissions and actual sandbox/VM boundaries constrain processes. A browser
profile separates browser state; it does not protect another profile from
malware running as the same OS user. A VM application's presence does not
establish isolation. Per-process secret injection reduces inheritance but still
exposes the injected values to that process and its descendants; same-user OS
process inspection can expose them to peers too. Test that channel with the
synthetic fixture described in the agents reference when evaluating credential
isolation. Encryption at rest does not establish protection while data is unlocked.

Distinguish a configured control from an observed effective control. Use safe
synthetic fixtures to verify a guard's decisions or isolation behavior when
needed; never probe with actual credentials or destructive actions. Do not
equate installed tools, hook counts, famous publishers, or a scanner's silence
with security. Keep an unverified claim explicitly unverified.

## Report

Give a compact coverage table per selected domain: checked (with or without a
finding), not checked (reason), or not applicable (reason). Questions for
Pablo list only what the profile lacks. Identify roots,
tool versions, and reference dates only where they affect the conclusion.
Track skipped subchecks so a partial scan cannot look complete. Rank each
skipped subcheck by the severity its failure would have. A skipped subcheck
that could hide a High or Critical exposure goes in the summary beside the
findings as an unverified high-risk item, naming the next step and who takes
it; it is never only a coverage-table row or a generic deferral. A
"not checked" table row for privacy grants, for example, can hide a script
host holding Accessibility, which lets any code click through firewall prompts.
Keep machine-specific findings in the private report, never in this skill.

For each deduplicated finding include location, observed evidence, confidence,
the concrete exposure and affected workflow, severity rationale, and a scoped
remedy. Separate findings from optional hardening and unresolved questions.

- Critical: evidence of an immediately consequential exposure, such as a
  currently usable credential exposed publicly or active compromise.
- High: an established, substantial access or execution path to sensitive assets.
- Medium: a material control gap with a plausible but constrained exposure.
- Low: an established limited-impact gap. Keep optional hardening separate from
  vulnerability findings rather than assigning it a severity by default.

A scanner match is a candidate until classified; do not test a credential's
live validity without authorization. Advisory severity, local exposure,
confidence, and fix availability are separate facts. Keep relevant unfixed
transitive and dev-tool vulnerabilities visible; no upgrade path does not
remove the risk. Do not label a whole domain safe when consequential checks
were unavailable.

End with the material risk and coverage limits, plus prioritized do/skip/defer
recommendations. If remediation was authorized, perform it within that scope
and verify the reported behavior before claiming it resolved.

## Tracking and cadence

A finding that lives only in a report or session log gets lost: the 2026-10-17
accounts run left three open items in its log and nowhere else. Before ending
a run, give every finding and deferred remediation step not resolved and
verified in the session a `** TODO` heading under `* PROJECT dotfiles` in
`~/repos/notes/protean-projects.org`, which syncs to his Linear under that
project. Never put them in an inbox file (`~/repos/android/inbox-*.org`):
inboxes are for unsorted captures, not project tasks. Order them by
severity and, within accounts, by the account tiers in the accounts reference.
The heading names the account or control and the action; evidence stays in
the private report. Then compare the report's findings with the TODOs one by
one, and in the report name any finding left without a TODO on purpose, with
the reason. Then update the profile (above) and commit it with the report.

Two repeating TODOs under the same project heading carry the schedule: a quarterly accounts
check and a yearly comprehensive audit. When a run covers one of them, mark it
done so it repeats.
