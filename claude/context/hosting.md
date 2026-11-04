# Hosting Pablo's always-on automations

Principle (Pablo, 2026-11-04): an always-on service that belongs to an organization runs on that organization's own server, billed to it and reached with its own accounts, never on Pablo's Mac. A service that needs Pablo's personal credentials (his WhatsApp, his Gmail, his Slack session) stays on the Mac.

- **Why**: a service on the Mac stops when the Mac sleeps and when Pablo steps back from the organization; one on a server keeps running for whoever takes over. Personal credentials, in turn, never go on a shared server.
- **Belongs to an organization** means its inputs are the organization's own channels (a shared number, a group address, an org Slack channel) and its outputs are the organization's records. Mixed services are split along that line, not hosted whole on either side.
- **Server, not a Worker**, when the service holds a long-lived connection or needs a filesystem (a WhatsApp companion device, a Claude Code run over a git checkout). Short, event- or schedule-triggered jobs can be Workers.
- **Alerts and schedules** still follow `alerting.md` and `scheduling.md`; secrets placed on the server follow `secrets.md`.

## Instances

- In progress (approved 2026-11-04): FIAS treasury recorder (companion device on the FIAS WhatsApp Business number) and updater (scheduled Claude Code run that records the FIAS number, the FIAS treasury mailbox and `#ops-fias` in the `tesoreria` repo), on a FIAS-billed VPS. Design: `~/repos/fias/tesoreria/docs/ops-handover.md`.
- Compliant by the personal-credentials exception: the inbox monitor and `wa-archive` on Pablo's own accounts.
- To assess (TODO): other `~/repos/launchd` jobs that serve an organization rather than Pablo.
