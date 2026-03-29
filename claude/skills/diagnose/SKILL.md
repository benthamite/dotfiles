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
