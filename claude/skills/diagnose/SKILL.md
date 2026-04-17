---
name: diagnose
description: Investigate errors and friction diagnostically instead of fixing them. Use when the user says "/diagnose" followed by a question about why something went wrong.
user-invocable: true
---

The user is asking you to investigate a problem diagnostically. Treat errors, confusion, and friction as *symptoms* of an underlying structural issue, not as problems to fix directly.

Your job is to:

1. **Check whether an existing mechanism already covers the failure.** Before proposing anything, enumerate the hooks, skills, configs, or scripts that *should* have prevented or detected the issue. For each, confirm by observation (read the hook, check the log, test the behavior) whether it fired, whether it succeeded, and whether its output was acted on. Do not assert behavior you have not verified.
2. **Identify the root cause** — why was this hard, confusing, or error-prone? Distinguish carefully between:
   - *Structural gap*: no existing mechanism would have caught this, or the mechanism is broken.
   - *Behavioral lapse*: an existing mechanism would have caught it (or did, and was ignored), and the failure is disciplinary. This is a valid diagnosis outcome. When it is the correct diagnosis, say so plainly and stop — do not invent additional mechanisms to compensate for discipline.
3. **Consider whether it's an instance of a broader class of problems** — is this a one-off, or does it point to a pattern?
4. **Propose structural fixes only when a structural gap is confirmed.** If the existing mechanism would have prevented the failure and only discipline was missing, the correct output is to name the rule that was violated, not to propose new mechanisms on top of it.

Do NOT try to fix or work around the original error. Focus entirely on diagnosis and structural remedies. Present your findings, then wait for the user to decide what action to take.

## Before proposing any fix

Run this single check: *would an existing mechanism, firing as intended, have prevented this failure?* If yes, stop — the diagnosis is "behavioral lapse; existing rule X was violated." If no, a structural gap exists and a fix is warranted. Do not skip this check; every turn that skips it compounds onto unverified assumptions about what the system does.

## What counts as a structural fix

A structural fix is a **mechanism** — something that makes the problem impossible or automatically prevented. Examples: a hook that blocks a commit, a config change that enforces a constraint, a skill that loads context automatically, a CI check.

The following are NOT structural fixes and must NEVER be proposed as solutions:

- "I should do X in the future" — this is a resolution, not a mechanism. It depends on memory and discipline, which are exactly what failed.
- "I will be more careful about Y" — same problem. There is no enforcement.
- "I should scan/check/review Z before starting" — unless there is a concrete mechanism (hook, memory, config) that forces the scan to happen, this is just a good intention.

If you cannot identify a concrete mechanism, say so explicitly: "I don't see a reliable structural fix for this — it may require a hook or config change that I need to investigate further." Never fill the gap with aspirational statements.
