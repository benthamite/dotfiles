---
name: chrome-permission-audit
description: Audit and revoke the sites Claude in Chrome is permanently allowed to access, across every Chrome profile. Use when the user asks to check/audit/review Claude's browser site permissions, asks which sites Claude can access, wants to revoke browser access to sensitive sites, or asks whether the "permanent but not sensitive" policy still holds. Not for Chrome's own content settings, extension host permissions, or OAuth app grants.
user-invocable: true
argument-hint: "[audit] | revoke --sites a,b,c [--profile P] [--dry-run] | revoke --tier 1 | accept --site X --tier N | state"
---

# chrome-permission-audit

Inventories every site the Claude Chrome extension may act on, classifies each by
sensitivity, and reports drift from the standing policy: **permanently allow
ordinary sites, never sensitive ones.**

## Run it

```sh
node <skill-dir>/chrome_permissions.js audit
```

First run establishes a baseline and reports no drift. Later runs also list what
is new. `classic-level` self-installs into the skill directory on first use
(already gitignored).

| Command | Purpose |
|---|---|
| `audit [--json] [--no-record]` | Inventory, classify, report violations + drift |
| `revoke --sites a,b,c [--profile P] [--dry-run]` | Remove named grants |
| `revoke --tier 1 [--profile all]` | Remove every Tier 1 grant |
| `accept --site X --tier N [--note "..."]` | Record a decision so X classifies that way from now on |
| `state` | Show baseline and recorded overrides |

## Five facts that determine correct behaviour

**Permissions are per Chrome profile, not global.** Each account keeps an
independent list. Auditing only the default profile misses most of them. The
script discovers profiles from Chrome's `Local State` and skips those where the
extension is not installed.

**Reading requires a real LevelDB reader.** The store's `.log` files retain
superseded history — one profile held 1,784 stale `netloc` records against 51
live ones. Grep or `strings` over those files reports long-revoked grants as
active. Reads therefore go through `classic-level` against a throwaway copy, so a
running Chrome cannot block them.

**Writing requires the profile to be closed.** Chrome holds an exclusive LevelDB
lock on profiles it has open. Writes go direct so the lock fails them loudly.
Closing just that profile's windows is usually enough — quitting Chrome entirely
is only needed when the target profile is the one in use. Check by trying; a
locked profile is reported as skipped, never partially written.

**The extension UI cannot be automated.** Browser tools reject
`chrome-extension://` URLs outright, so `options.html` is unreachable to an
agent. Its permission list also offers one Revoke button per entry with no bulk
action. Revocation goes through the store, or the user clicks manually.

**Backups contain live credentials.** The same store holds `accessToken` and
`refreshToken`. Backups are written mode 700 under
`~/Library/Application Support/claude-chrome-permission-backup/<timestamp>/`.
Never copy token values into a report, a log, or a scratch file, and never leave
a store copy behind. Tell the user the backup path and that it should not be
synced.

## Tiers

| Tier | Meaning | Examples |
|---|---|---|
| 1 | **Revoke** — violates the policy | admin consoles, identity, banking, payroll, cloud/IAM, API-key dashboards, registrars, e-signature, `file` |
| 2 | **Review** — defensible, real exposure | private Drive/Calendar, Slack, messaging, source code, retail with saved cards, localhost |
| 3 | **Keep** | search engines, archives, public reference |
| — | **Unclassified** | no rule matched |

Rules live in `rules.json`, ordered, first match wins.

Unmatched domains are reported as unclassified rather than assumed safe. Some
rules are deliberately broad prefix heuristics (`admin.*`, `signin.*`, `api.*`),
and **every broad rule escalates toward Tier 1 — none of them grants "keep."** A
heuristic that misfires costs a needless revocation, never a silent approval.

## Workflow

1. Run `audit`. Read the violations and unclassified sections first.
2. For unclassified entries, decide a tier and record it with `accept`. Do not
   loosen a rule to silence one site unless the rule itself is wrong.
3. Propose revocations to the user as an explicit list and get approval. Never
   revoke unprompted — some Tier 1 grants are deliberate.
4. Revoke with `--dry-run` first, then for real.
5. **Verify by re-running `audit`**, which re-reads the live stores. The revoke
   command's own output is not verification of itself.

## Judgement notes

**A `localhost` grant is scoped to the port, not the process.** Whatever binds
that port next inherits the permission. Identify what a port actually serves
before keeping it: `lsof -iTCP:PORT -sTCP:LISTEN -P -n`, then `ps -p <PID>
-o command=` for the project path, then `curl -sI` for what it serves. Plain grep
for a 4-digit port across repos is mostly noise — it collides with catalog
numbers, UUID fragments and row counts. Search launchd agents and config files.

**Ephemeral hosts age badly.** Preview deployments (`*.workers.dev`) and
tracking redirects (`*.sendgrid.net`) get granted by accident and keep standing
access to a hostname that may later belong to someone else.

**`localhost` and `127.0.0.1` are distinct entries** to the extension, as are
`example.com` and `www.example.com`. Match exactly when revoking.

**Duplicate entries are normal.** The same netloc can appear twice with different
durations (`always` plus a leftover `once`). Revoking by netloc removes all of
them; count records, not just names, when reporting.

## Reporting to the user

Lead with whether the policy holds and what changed since last run — not with a
185-row table. Group by profile, name the account, and for each violation say
what the site actually controls ("Workspace super-admin: users, passwords") not
just its category. Flag anything whose grant date suggests it was a one-off that
never got cleaned up.
