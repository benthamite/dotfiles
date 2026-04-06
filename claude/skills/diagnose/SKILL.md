---
name: diagnose
description: Investigate errors and friction diagnostically instead of fixing them. Use when the user says "/diagnose" followed by a question about why something went wrong.
user-invocable: true
---

The user is asking you to investigate a problem diagnostically. Treat errors, confusion, and friction as *symptoms* of an underlying structural issue, not as problems to fix directly.

Your job is to:

1. **Identify the root cause** — why was this hard, confusing, or error-prone?
2. **Consider whether it's an instance of a broader class of problems** — is this a one-off, or does it point to a pattern?
3. **Propose structural fixes** (to config, tooling, documentation, or workflow) that prevent the class of problems, not just this instance.

Do NOT try to fix or work around the original error. Focus entirely on diagnosis and structural remedies. Present your findings, then wait for the user to decide what action to take.

## What counts as a structural fix

A structural fix is a **mechanism** — something that makes the problem impossible or automatically prevented. Examples: a hook that blocks a commit, a config change that enforces a constraint, a skill that loads context automatically, a CI check.

The following are NOT structural fixes and must NEVER be proposed as solutions:

- "I should do X in the future" — this is a resolution, not a mechanism. It depends on memory and discipline, which are exactly what failed.
- "I will be more careful about Y" — same problem. There is no enforcement.
- "I should scan/check/review Z before starting" — unless there is a concrete mechanism (hook, memory, config) that forces the scan to happen, this is just a good intention.

If you cannot identify a concrete mechanism, say so explicitly: "I don't see a reliable structural fix for this — it may require a hook or config change that I need to investigate further." Never fill the gap with aspirational statements.
