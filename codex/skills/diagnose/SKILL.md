---
name: diagnose
description: Diagnose a specific agent, tooling, configuration, or workflow incident before any remedy. Use for /diagnose, why-did-this-happen questions, recurring process failures, broken hooks/skills/scripts, or diagnose-and-fix requests; not for ordinary code bugs, broad configuration audits, or repository-wide AI developer-experience reviews.
---

Use this workflow to establish why a specific incident occurred before changing the failing mechanism. Treat errors, confusion, and friction as *symptoms* of an underlying issue, not as direct fix targets. For a diagnosis-only request, the deliverable is the diagnosis. For a request that also asks for a fix, diagnose first, then implement the confirmed remedy.

Do not use this skill for ordinary code debugging, planned implementation work, broad agent-configuration inventories, or repository-wide developer-experience audits. When the request includes a fix, report the diagnosis alongside the implemented remedy in the same turn. Still stop for confirmation before a remedy that is externally visible, destructive, or otherwise hard to reverse.

Your job is to:

1. **Check whether an existing mechanism already covers the failure.** Before proposing anything, enumerate the hooks, skills, configs, or scripts that *should* have prevented or detected the issue. For each, confirm by observation (read the hook, check the log, test the behavior) whether it fired, whether it succeeded, and whether its output was acted on. Do not assert behavior you have not verified.
2. **Identify the root cause** — why was this hard, confusing, or error-prone? Distinguish carefully between:
   - *Structural gap*: no existing mechanism would have caught this, or the mechanism is broken.
   - *Enforcement gap*: prose or a manual step covered the immediate failure, but the operation is repetitive, low-level, and deterministic enough to enforce. Name both the immediate rule violation and the mechanism that would prevent, constrain, or automatically detect that class of failure.
   - *Behavioral lapse*: an existing instruction or mechanism was clear, sufficient, and usable, but it was not followed, or an enforced mechanism worked and its result was ignored or bypassed. Use this category only when no practical enforcement improvement is indicated. When it is correct, say so plainly and stop — do not add another mechanism to compensate for discipline.
3. **Consider whether it's an instance of a broader class of problems** — is this a one-off, or does it point to a pattern?
4. **Propose structural fixes only for structural or enforcement gaps.** If an existing instruction or mechanism was clear, sufficient, and usable and only discipline was missing, name what was not followed instead of proposing another mechanism.
5. **Report, then stop or fix depending on the request.** Present the observed symptom, mechanisms checked, diagnosis category, evidence, broader pattern, and any structural remedy candidates. If no reliable structural fix is visible yet, say what remains to investigate. When the request was diagnosis-only, do not implement the remedy until the user asks; when the request included fixing, implement the confirmed remedy in the same turn (subject to the reversibility limits above).

While diagnosing, do NOT fix or work around the original error — the diagnosis must come from the unmodified failure. Present your findings; wait for the user to decide what action to take only when the request was diagnosis-only.

## Diagnostic communication

When a diagnosis uses overloaded local tooling labels, define the concrete artifact in plain language before relying on the label: e.g. a Markdown skill file, plugin bundle, agent-skill helper, Emacs package, command wrapper, or hook script. Do this only when ambiguity could affect the explanation; do not define ordinary unambiguous terms.

## Before proposing any fix

For agent instruction-loading or context-loading questions, inspect the actual loaded context or the mechanism that creates it before answering. Use the relevant agent's diagnostic tool when one exists; otherwise verify the concrete files, hooks, or configs that determine the loaded context.

Run this check: *would an existing mechanism, firing as intended, have prevented the immediate failure or surfaced it in time for the required action?* If no, a structural gap exists and a fix is warranted.

If yes, run one more check before stopping: *is the existing mechanism enforced, or is it only prose/instructions that require a brittle deterministic operation to be performed correctly by attention?* If a manual/prose operation can be made deterministic with a tool, hook, config, or wrapper, diagnose an enforcement gap rather than stopping at discipline. Otherwise, if the instruction or mechanism was clear, sufficient, and usable but was not followed, diagnose a behavioral lapse.

## What counts as a structural fix

A structural fix is a **mechanism** that prevents, constrains, or automatically detects the failure. Examples: a hook that blocks a commit, a config change that enforces a constraint, a deterministic resolver CLI that replaces manual path expansion, a skill that loads context automatically, or a CI check that detects the failure before completion.

The following are NOT structural fixes and must NEVER be proposed as solutions:

- "I should do X in the future" — this is a resolution, not a mechanism. It depends on memory and discipline, which are exactly what failed.
- "I will be more careful about Y" — same problem. There is no enforcement.
- "I should scan/check/review Z before starting" — unless there is a concrete mechanism (hook, memory, config) that forces the scan to happen, this is just a good intention.

If you cannot identify a concrete mechanism, say so explicitly: "I don't see a reliable structural fix for this — it may require a hook or config change that I need to investigate further." Never fill the gap with aspirational statements.
