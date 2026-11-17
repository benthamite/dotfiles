# Scheduling Pablo's automations

Principle (Pablo, 2026-11-02): a job that must run on time has a second trigger independent of its scheduler, and every entry point plans from fresh state.

- **Must run on time** means a missed run costs something a later run does not recover: a publication, an email, a deadline. A job whose next run simply catches up (a sync, a cache refresh) needs only its scheduler and the alerting principle.
- **The second trigger comes from the shared service**, `launchd-heartbeat`'s supervisor (`~/repos/launchd/heartbeat`, README "Supervisor"), not from a project's own Worker or cron, as alerting does (`alerting.md`). It is off GitHub and off the Mac, and dispatches at checkpoints when the job's outcome is not yet observed and nothing is running. A second schedule on the same scheduler is not a second trigger: on 2026-11-02 GitHub created none of The Hinge's four scheduled runs, so its retries on that scheduler were lost with the first.
- **Every entry point plans from fresh state.** A run first establishes what is already done, from the outcome's own source of truth (for a publication, the published site, not the repository's record of it), and does only what remains. Two triggers then cannot do the work twice, whichever starts first. A run that cannot establish the state does not act; it alerts.
- **Identity**: a dispatched GitHub run is started by an App, never a person's token (`alerting.md`, "GitHub notifications").
- **The outcome is checked, not the start.** The supervisor reads whether the job's outcome exists (the post is published), so it also covers runs that started and failed.

Reasons and design: The Hinge's `docs/plans/2026-11-02-reliability.md` (`~/repos/the-hinge`).

## Run from pushed code

Principle (Pablo, 2026-11-13): a scheduled job that publishes, alerts or reaches other people runs the code pushed to its repository, never the checkout people and agents edit, so an edit takes effect only once it is pushed. On 2026-11-13 an agent's uncommitted edit to The Hinge's watchdog deadline, in the checkout the job ran from, took effect at once and emailed a false "no issue" alert.

- **How**: a launchd job runs under `~/repos/launchd/bin/launchd-run-from-origin` (launchd README, "Jobs That Run From Pushed Code"), which resets the job's own worktree to `origin/main` each run and does not run if it cannot. Jobs off the Mac (GitHub Actions, Workers) already run pushed code.
- **Out of scope**: personal tooling whose effects stay with Pablo's own machine and files (dotfiles scripts, syncs, local servers), where an edit is meant to take effect at once.
- **Instances**: compliant: The Hinge's watchdog (`com.stafforini.hinge-watchdog`, 2026-11-13). Partly: The Hinge's weekly audit (`com.stafforini.hinge-audit`) does its work in a fresh worktree of `origin/main`, but its runner, `audit/run.py`, runs from the checkout. To assess (TODO), whether each publishes, alerts or reaches other people, and if so move it under the wrapper: `com.stafforini.launchd-audit` (alert emails), `com.stafforini.ha-health-check` (email), `com.stafforini.protean` (replaced `com.stafforini.inbox-monitor` on 2026-11-16), `com.stafforini.finance-daily`, `com.stafforini.epoch-website-pull`, `com.stafforini.sa-lp-refresh` and `com.stafforini.vara-refresh` (stafforini.com), `com.stafforini.emacs-server-watchdog`, `com.stafforini.download-pdfs`.

## Instances

- Compliant once deployed: The Hinge's daily issue (supervisor checkpoints 09:30, 11:15, 13:30 UTC since 2026-11-14; every run reconciles with Substack before planning). Status: deployed 2026-11-02 (App "The Hinge supervisor"); production verification is the first day it dispatches a real run.
- Compliant by its own rules (Forethought joint work, governed by ph-ps's `AGENTS.md`): ph-ps automations, dispatched by the meeting-debrief Worker's cron as the ph-ps App rather than GitHub's `schedule`.
- To assess (TODO): whether each of these must run on time, and if so, bring it under the principle: `benthamite/stafforini.com` and `benthamite/benthamite` (GitHub `schedule`); The Hinge's weekly audit (`com.stafforini.hinge-audit`, launchd on the Mac); other launchd jobs in `~/repos/launchd`.
