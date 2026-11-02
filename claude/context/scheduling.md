# Scheduling Pablo's automations

Principle (Pablo, 2026-11-02): a job that must run on time has a second trigger independent of its scheduler, and every entry point plans from fresh state.

- **Must run on time** means a missed run costs something a later run does not recover: a publication, an email, a deadline. A job whose next run simply catches up (a sync, a cache refresh) needs only its scheduler and the alerting principle.
- **The second trigger comes from the shared service**, `launchd-heartbeat`'s supervisor (`~/repos/launchd/heartbeat`, README "Supervisor"), not from a project's own Worker or cron, as alerting does (`alerting.md`). It is off GitHub and off the Mac, and dispatches at checkpoints when the job's outcome is not yet observed and nothing is running. A second schedule on the same scheduler is not a second trigger: on 2026-11-02 GitHub created none of The Hinge's four scheduled runs, so its retries on that scheduler were lost with the first.
- **Every entry point plans from fresh state.** A run first establishes what is already done, from the outcome's own source of truth (for a publication, the published site, not the repository's record of it), and does only what remains. Two triggers then cannot do the work twice, whichever starts first. A run that cannot establish the state does not act; it alerts.
- **Identity**: a dispatched GitHub run is started by an App, never a person's token (`alerting.md`, "GitHub notifications").
- **The outcome is checked, not the start.** The supervisor reads whether the job's outcome exists (the post is published), so it also covers runs that started and failed.

Reasons and design: The Hinge's `docs/plans/2026-11-02-reliability.md` (`~/repos/the-hinge`).

## Instances

- Compliant once deployed: The Hinge's daily issue (supervisor checkpoints 10:45, 12:30, 14:45 UTC; every run reconciles with Substack before planning). Status: deployed 2026-11-02 (App "The Hinge supervisor"); production verification is the first day it dispatches a real run.
- Compliant by its own rules (Forethought joint work, governed by ph-ps's `AGENTS.md`): ph-ps automations, dispatched by the meeting-debrief Worker's cron as the ph-ps App rather than GitHub's `schedule`.
- To assess (TODO): whether each of these must run on time, and if so, bring it under the principle: `benthamite/stafforini.com` and `benthamite/benthamite` (GitHub `schedule`); The Hinge's weekly audit (`com.stafforini.hinge-audit`, launchd on the Mac); other launchd jobs in `~/repos/launchd`.
