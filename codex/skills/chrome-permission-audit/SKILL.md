---
name: chrome-permission-audit
description: Audit and revoke the sites Claude in Chrome is permanently allowed to access, across every Chrome profile. Use when the user asks to check/audit/review Claude's browser site permissions, asks which sites Claude can access, wants to revoke browser access to sensitive sites, or asks whether the "permanent but not sensitive" policy still holds. Not for Chrome's own content settings, extension host permissions, or OAuth app grants.
user-invocable: true
argument-hint: "audit [--record] | revoke --sites a,b --dry-run --plan FILE | revoke --plan FILE | accept --site X --tier N | state | setup"
---

# chrome-permission-audit

Audit stored permanent site-allow records for Claude in Chrome against the policy
of keeping ordinary sites and reviewing/revoking sensitive ones. Stored records
are not a complete model of effective access: runtime permission modes, surfaces,
deny precedence, one-time grants and domain transitions can change the outcome.

This skill concerns the extension's site-permission store, not Chrome content
settings, extension host permissions or OAuth grants. A request to audit/report
does not authorize revocation, persistent reclassification, dependency setup or
closing browser windows.

## Commands and prerequisites

Resolve `chrome_permissions.js` relative to this loaded skill directory and quote
paths containing spaces. The current CLI requires Chrome to be closed and obtains
exclusive LevelDB access; a window closing is not proof the browser process ended.
Do not quit Chrome or close profiles without explicit user authorization.

```sh
node "SKILL-DIR/chrome_permissions.js" audit
```

| Command | Contract |
|---|---|
| `audit [--json] [--record\|--no-record]` | Read stored records; default does not update baseline/state |
| `revoke (--sites a,b\|--tier 1\|2\|3) [--profile exact-dir\|unique-label\|all] --dry-run [--plan FILE]` | Preview exact standing-allow targets; optionally save a private reviewed plan |
| `revoke --plan FILE` | Revalidate and apply only that plan |
| `accept --site X --tier 1\|2\|3 [--note TEXT]` | Persist an explicitly approved classification override |
| `state` | Inspect baseline and overrides |
| `setup` | Explicitly install pinned `classic-level@3.0.0`; never an implicit audit step |

Commands return structured JSON; `--json` remains an accepted audit option.
Tier 2 still requires review even when no Tier 1 violation was classified.

The established state/dependency directory is
`~/.claude/chrome-permission-audit/`, currently outside Drive. Do not put runtime
dependencies, state, plans or generated artifacts in the skill tree or a synced
location. Resolve symlinks rather than assuming every home path is outside Drive.
Missing/broken dependencies are setup gaps, not permission-free profiles.

Read `/Users/pablostafforini/My Drive/dotfiles/claude/context/secrets.md` before
handling permission-store data or backups. Access only the permission key through
the helper; never dump the whole database, tokens, raw errors or account sessions.

## Evidence and coverage

- Inventory every discoverable profile in the configured Chrome user-data root.
  Report inaccessible, malformed or unregistered profile/store evidence explicitly.
  A missing directory is not proof that the extension cannot act elsewhere.
  Other Chrome channels or custom user-data roots require separate scoped evidence.
- Use a real LevelDB reader. Log files contain superseded records; grep/strings
  cannot establish the live permission set.
- Do not copy an active LevelDB directory and claim a coherent snapshot.
  Current reads use exclusive database access after the Chrome-closed check.
  The audit does not write permission/state values by default, but opening
  LevelDB can update engine housekeeping files; it is not byte-for-byte inert.
- Distinguish missing permission data from read errors or an unsupported schema.
  Incomplete evidence prevents an unqualified policy-holds claim.
- Browser-tool support for extension pages varies. Inspect actual capabilities
  rather than declaring all `chrome-extension://` pages impossible. An available
  UI observation may help diagnose a gap, but does not replace verified
  all-profile coverage or authorize an alternative mutation path.

Only `action=allow`, `duration=always`, `scope.type=netloc` records are the
standing site allows targeted by this CLI. Report deny, one-time and
domain-transition records separately; do not call them permanent access grants.
Unknown records are a coverage gap, not safe entries to silently discard.

The inspected extension version 1.0.91 also checks grant surface and gives matching
deny records precedence. It normalizes leading `www.` and trailing dots for
netloc matching and supports wildcard hosts. These are version-specific runtime
observations, not permission to merge stored records during editing. Re-check
the installed implementation when its schema or semantics change.

## Classification

| Tier | Meaning |
|---|---|
| 1 | Sensitive standing allow; recommend scoped revocation |
| 2 | Review private content, account authority or context-dependent exposure |
| 3 | Explicitly named ordinary-site candidate; not a universal safety guarantee |
| Unclassified | No supported rule; investigate rather than assume safe |

`rules.json` uses ordered first-match rules. Generic prefixes such as `docs.`,
`help.` or `support.` do not prove public/read-only access. Look-alike domains or
arbitrary search-engine suffixes must not inherit a keep classification.
Heuristics may raise concern, never grant Tier 3.

AI/chat/social/publishing accounts can contain private data even when their
landing pages are public. Google AI Studio also manages API keys; see
[Google's API-key documentation](https://ai.google.dev/gemini-api/docs/api-key).
Classify the host's actual scope, not the benign page most recently visited.

Overrides are global exact-site classification decisions across profiles, not
browser permission grants. Do not call `accept` automatically during an audit or
downgrade a concern to silence it. Propose an evidence-backed tier; persist it only
when the current request or subsequent decision authorizes that change.

## Revocation workflow

1. Audit without recording state and check coverage/schema failures first.
   Distinguish observed findings from incomplete evidence.
2. Resolve the exact profile directories, account context and site records.
   Profile labels can collide; never select multiple profiles by an ambiguous name.
   Distinguish stored identifiers, including ports, wildcards and grant surfaces.
3. Present the exact standing-allow records to remove. An explicit scoped user
   revocation request supplies authority; do not ask redundantly. An audit request
   or tier label alone does not. Never treat rule matches as automatic approval.
4. Run the dry-run command and save a private plan when revocation is authorized.
   Inspect the complete plan/target list before applying it. Broad tier selectors
   are not permission to include newly appearing records after review.
5. Apply only that plan. Changed profile/store contents, rules or overrides require
   a new review, not a silent broadened retry. The helper preserves deny, once and
   domain-transition records; deleting a deny could increase access.
6. Treat nonzero/partial/uncertain results as such. Retain recovery evidence and
   reconcile exactly which profiles changed before any retry.
7. Re-run `audit` after mutation and verify the intended stored records are gone
   while unrelated records remain. Once Chrome is reopened through an authorized
   path, inspect its actual extension UI/runtime state before claiming effective
   access was revoked. A disk edit alone does not prove what a live extension can do.

If the user asked to remove all effective access, remaining one-time, wildcard,
transition, surface or runtime-mode access must be addressed through an
appropriately supported and authorized path. Do not present removal of standing
netloc records as complete revocation of every possible access route.

## Baselines and recovery

Recording a baseline is a separate local mutation: use `audit --record` only
when authorized. A first baseline has no prior comparison; do not say no change
occurred. Legacy all-time seen history cannot prove drift since the last run;
establish a fresh baseline explicitly while preserving valid user overrides.
Changed/reappearing records matter, not just never-before-seen hostnames.

New recovery backups contain only the exact pre-write `permissionStorage` value,
captured under the database lock, not OAuth keys or full store copies. They still
contain private browsing/account metadata: keep them restrictive and outside
sync, and report their exact location. Prior whole-store backups may contain live
credentials; do not inspect, copy or delete them as incidental audit cleanup.

An uncertain write retains its backup. Do not restore automatically, which could
reintroduce access or overwrite newer decisions. Recovery needs an exact target,
closed browser, current-state comparison and explicit restoration authority.

## Judgment and reporting

A localhost grant is tied to host/port, not the process that happened to own it.
Use scoped read-only process/config evidence to identify it; do not probe arbitrary
services or send credentials. Preview and redirect hosts need ownership/destination
review. Exact stored records and effective runtime host matching are different.

Lead with the stored-policy finding and coverage limits, then what changed from
a valid prior baseline. Group relevant findings by profile/account, explain the
actual concern without inventing control capabilities, and count records as well
as distinct sites. Minimize private inventory detail in reports. Separate observed,
recommended, authorized, changed and runtime-verified states.
