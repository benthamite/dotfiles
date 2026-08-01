# Review LuLu Alert Skill Design

## Goal

Create a project-local `$review-lulu-alert` skill that audits the LuLu alert
currently visible on this Mac and recommends whether to allow or block it.
Review invocations must not change LuLu rules or click either decision button.

## Location and discovery

Place the skill at `.codex/skills/review-lulu-alert/` so it is available while
working in the `macos` project. Its trigger metadata should cover explicit
`$review-lulu-alert` use and natural-language requests to inspect, check, audit,
or review an open LuLu firewall window.

## Components

The skill contains:

- `SKILL.md`: the audit workflow, safety boundary, evidence requirements,
  recommendation format, and explicit-action gate.
- `scripts/inspect-lulu-alert`: a deterministic, read-only helper that uses
  macOS Accessibility to return the current alert's process name, PID,
  executable path, arguments, destination, port/protocol, reverse DNS, and
  selected rule scope and duration.
- `agents/openai.yaml`: concise UI metadata generated from the finished skill.

No action command belongs in the helper. Keeping inspection and mutation in
separate mechanisms makes accidental firewall changes harder.

The helper writes one JSON object to standard output. A present alert includes
all readable fields plus `alert_present: true`; no-alert state returns
`{"alert_present": false}`. Operational failures use a nonzero exit status and
write diagnostics to standard error. A fixture input option supports tests
without synthesizing or changing a real LuLu window.

## Audit workflow

1. Read the current alert through the helper. If there is no alert, report that
   fact and do not open LuLu.
2. Record the alert identity: PID, executable path, arguments, destination, and
   timestamp. Re-read these fields before any later action.
3. Trace the process ancestry, working directory, open executable, sockets, and
   relevant files. Explain what user or automation workflow launched it.
4. Verify executable provenance in proportion to its form:
   - For an app bundle, use `audit-mac-app` for signing, notarization,
     entitlements, framework, and static-code checks.
   - For a command-line executable, check path resolution, code-signing
     integrity, entitlements, package-manager receipts and hashes where
     available, linked libraries, and signs of replacement or tampering.
5. Verify the destination using authoritative DNS, TLS identity, official
   vendor source or documentation, and local package source when useful.
   Minimize direct network probes because the probes can create additional
   LuLu alerts. If an audit-created prompt appears, identify and dismiss only
   that owned artifact without creating a lasting rule.
6. Give a clear allow/block recommendation, highest-severity findings,
   uncertainty, and the narrowest suitable LuLu scope and duration.

## Mutation boundary

Invoking the skill to review an alert is audit-only. It must never press Allow
or Block, change the scope or duration, or create a LuLu rule.

An explicit instruction in the current request such as “allow it” or “block
it” may authorize that single decision. Before acting, the agent must re-read
the visible alert and require an exact match with the audited PID, path,
arguments, and endpoint. Unless the user requests persistence, prefer Remote
Endpoint plus Process lifetime. After acting, verify that the alert closed,
the intended workflow progressed or failed visibly, and no broader persistent
rule was created.

Do not process subsequently queued alerts unless the request explicitly covers
them. Stop when the next alert has a different identity.

## Error handling

- Missing Accessibility access: report the denied read and use a screenshot or
  other read-only agent-accessible path if available.
- Process exited during review: continue with available provenance evidence but
  say which live checks became unavailable.
- PID reuse or changed alert: stop; never apply a conclusion to the new alert.
- Unverifiable publisher or destination: recommend blocking, or leaving the
  alert unanswered, rather than guessing.
- Audit-created prompt: clean it up only after proving it came from the audit.

## Testing

Use skill TDD:

1. Baseline-test agents without the skill on representative raw alert fixtures
   and record unsafe or incomplete behavior.
2. Test the helper against both no-alert state and a controlled mock of the
   Accessibility output without changing LuLu.
3. Forward-test the completed skill on:
   - no open alert;
   - a legitimate signed developer-tool connection;
   - a suspicious unsigned executable with an unrelated endpoint;
   - a review-only request that pressures the agent to click Allow;
   - a changed or queued alert after an explicit action request.
4. Run the standard skill validator and confirm project-local discovery.

Success means the agent identifies the exact visible alert, grounds its verdict
in process and destination evidence, refuses mutation during review-only use,
and scopes any separately authorized action to the audited connection.
