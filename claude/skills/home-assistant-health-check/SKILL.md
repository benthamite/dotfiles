---
name: home-assistant-health-check
description: Check Pablo's Home Assistant instance for breakages, log regressions, unavailable critical devices, failed config entries, and new warnings; use for HA health checks, daily HA monitoring, "what broke in Home Assistant", or scheduled Home Assistant audits.
---

# Home Assistant Health Check

Use this skill to audit Pablo's Home Assistant instance at `http://homeassistant.local:8123` and produce a short operational report. Default stance: diagnose and report only; do not restart, reload, reconfigure, edit files, or call device-changing services unless the user explicitly asks.

## Workflow

1. Read project HA context when needed:
   - `/Users/pablostafforini/My Drive/home/AGENTS.md`
   - `/Users/pablostafforini/My Drive/home/context/ha-reference.md`
   - `/Users/pablostafforini/My Drive/home/context/ha-automations.md`
   - `/Users/pablostafforini/My Drive/home/context/ha-health-known-issues.md`
2. Run the collector from the active skill directory:
   ```bash
   python scripts/collect_ha_health.py \
     --home-root "/Users/pablostafforini/My Drive/home" \
     --known-issues "/Users/pablostafforini/My Drive/home/context/ha-health-known-issues.md"
   ```
   The script fetches `env/home-assistant-token` from `pass`, queries HA REST endpoints, redacts signed URLs in logs, writes a run snapshot under `~/.local/state/ha-health-check/`, and prints JSON.
3. Classify the JSON findings:
   - **urgent:** critical workflow unavailable, config entry failure that blocks a known automation, repeated service-call warning for an active automation, or device state that affects security/safety.
   - **actionable:** broken integration/device with clear owner and likely remediation, but no immediate critical workflow failure.
   - **known persistent:** matches `ha-health-known-issues.md`; include only if still present, grouped separately.
   - **transient/noise:** restart shutdown messages, one-off signed camera stream 404s, synthetic-test artifacts, or optional diagnostic entities unavailable while their parent device is otherwise healthy.
4. Compare against `new_*` arrays from the collector. Lead with what changed since the previous run, then current important breakages, then known persistent noise.
5. If the user asks to fix something, switch to normal systematic debugging before editing or service calls. For secrets, follow the global secrets rule before reading `pass`, 1Password, or other credential stores.

## Report Shape

Use this compact structure:

```markdown
**New**
- <new breakage or "None">

**Needs Action**
- <issue, affected workflow, likely next diagnostic step>

**Known / Still Present**
- <known persistent warning, if relevant>

**Verification**
- Collector run time, HA API status, and any live checks performed.
```

For scheduled runs, keep the email concise. Do not include raw logs unless there is a new high-priority failure.

## Local Daily Email Job

For this HA workflow, prefer the bundled local launchd job over cloud `/schedule`: cloud scheduled agents cannot reach `homeassistant.local`, local `pass`, or the local baseline file.

- Email runner: `scripts/send_ha_health_email.py`
- Launchd plist: `launchd/com.stafforini.ha-health-check.plist`
- Default cadence: daily at 08:00 local time
- Default recipient: `pablo.stafforini@gmail.com`

The email runner calls the collector locally, updates the saved baseline, classifies current findings against the known-issues file, and sends through `claude/bin/gmail.py --account personal`.

## Scheduling Prompt

Only use `/schedule` if the target workflow is reachable from Anthropic's cloud runtime. When creating a recurring `/schedule`, use email notifications and a prompt like:

```text
Run the home-assistant-health-check skill for /Users/pablostafforini/My Drive/home. Report only new or actionable Home Assistant breakages first, then known persistent issues. Do not change Home Assistant state or restart anything unless explicitly instructed.
```
