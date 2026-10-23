# Alerting for Pablo's automations

Principle (2026-10-09): liveness and failure alerts for Pablo's automations go through one service, `launchd-heartbeat`, applied the same way in every project. Do not build a project's own watchdog Worker, push topic or alert channel; extend the shared service instead.

GitHub notifications (Pablo, 2026-10-23; applies in every project: personal, Epoch, Forethought): a GitHub Actions run whose failures an alerting service covers (this service, Epoch's or ph-ps's watchdog) must not generate a GitHub notification at all. GitHub notifies whoever a run is attributed to (the pusher, the dispatcher, the last editor of a `cron:` line), so such runs are started by an App or bot identity, never by Pablo: dispatched through the App, push checks started by an App dispatcher, cron lines last changed by the App. Never suppress or filter GitHub's notifications in account settings: a notification that does arrive is a real signal, of a run nothing else covers or of an attribution leak to fix at its source. Reference instance: ph-ps (`AGENTS.md`, "Automations"; `scripts/check-automations` fails any workflow an event attributed to a person would start). Not yet compliant (TODO): The Hinge's "Daily issue" runs, attributed to Pablo (notified 2026-10-23).

Validity rule (2026-10-09): an alert states only what is true when it is sent. A source that fails to deliver keeps no findings for later; its next run checks afresh and reports only what still holds, and the failed delivery is reported by the withheld heartbeat. A check does not report a condition that the Mac being off or logged out explains (a job not yet due since load, a heartbeat that could not have been refreshed).

## The service

`~/repos/launchd/heartbeat/` (README there): a Cloudflare Worker on the personal account, independent of the Mac and of GitHub, delivering on Telegram (@PablosNotificationBot) or Gmail SMTP.

- **Monitors** live in `src/monitors.ts`, one Durable Object each. Two rules:
  - `staleAfterMs`: alert when no heartbeat has arrived for that long (Mac jobs);
  - `dailyDeadlineUtc`: alert when no heartbeat dated today has arrived by that time. It is armed from deployment and alerts again on each missed day (daily jobs, CI).
- **Routes**: `POST /heartbeat/NAME` after each success; `POST /alert/NAME` (text body) for an immediate failure alert; `GET /status`.
- **Tokens**: `HEARTBEAT_TOKEN` (Mac client, every route). A source outside the Mac gets a monitor-scoped token (`tokenSecret` in the monitor config, listed in `deploy.py` `MONITOR_TOKEN_SECRETS`, kept in the personal `Automation` vault) that covers only its own heartbeat and alert routes.
- **Verify live** with its verification mode (`deploy.py --var VERIFICATION_MODE:true`, `/verify/stale`, `/verify/reset`), then redeploy without it.

## Adding a source

1. Add the monitor (and a scoped token if the source is not the Mac), with a test, in `~/repos/launchd/heartbeat`; run `npm run verify`; deploy.
2. The source sends a heartbeat after real success, judged by its outcome, not merely by the process exiting 0. On failure it calls `/alert/NAME`.
3. Do not rely on GitHub's own failure email: it did not arrive on 2026-10-06 or 2026-10-08.

## Instances

- Compliant: the launchd audit and the personal responder (Mac); The Hinge's daily publish (`the-hinge-publish`, deadline 11:30 UTC, from its GitHub workflow). Mail sync into mu4e (`mail-sync-personal`, `mail-sync-epoch`, `mail-sync-forethought`, stale after 2 h, from `bin/mbsync-parallel`), added 2026-10-15 after the Forethought Gmail token was revoked and mail silently stopped for two days. Homebrew updates (`personal-updates`, stale after 27 h, from `bin/personal-updates`), added 2026-10-18; it also alerts when upgrades are held back for lack of a LuLu directory rule (`lulu-firewall.md`).
- Not yet compliant (TODO): The Hinge's Mac watchdog `watchdog/run.py` (emails by itself; kept as a second check that the published page loads, to be folded into the shared service or retired); other projects with their own alerting still to be swept.
