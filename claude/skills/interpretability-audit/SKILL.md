---
name: interpretability-audit
description: Audit code for intent transparency and reader comprehension. Use for understandable, readable, self-explanatory, naming, or future-maintainer clarity reviews; not for bug/security, architecture, DX, or lint-only reviews.
argument-hint: [--accept] [dir]
argument-choices: "--accept"
---

# Interpretability audit

Review $ARGUMENTS for clarity and intent transparency. The audit asks whether a competent maintainer can understand what the code means, why it is shaped this way, and which assumptions it relies on.

## Scope boundary

- In scope: naming clarity, magic values, missing rationale, undocumented conventions, hard-to-follow phases, unclear score formulas, duplicated explanatory logic, and type-safety escapes that obscure meaning.
- Out of scope: finding defects, security issues, performance problems, broad architecture changes, missing tests, or formatting/lint-only cleanup. If those appear, mention them only when they also make intent unclear and route the main concern to the appropriate audit skill.
- Do not suggest new features, broader validation, or behavior changes unless they are necessary to make the existing intent explicit.
- Prefer clearer names and small extractions for "what" confusion, and comments for "why" or domain-context confusion.

## Workflow

1. Resolve the review scope from `$ARGUMENTS`. If the request names a whole repo, audit the main implementation paths and representative complex areas unless the user asks for exhaustive coverage.
2. Identify the project context before judging code: read local instructions, README/design notes, relevant tests, and nearby modules that explain conventions.
3. Read actual code and record line numbers. Use parallel exploration or subagents when available and useful for independent areas such as backend/frontend or package/module boundaries.
4. Classify findings by reader impact:
   - **High**: a maintainer is likely to misunderstand behavior, change the wrong thing, or miss a critical convention.
   - **Medium**: comprehension is slowed by hidden rationale, ambiguous names, or dense structure, but the confusion is localized.
   - **Low**: polish that would improve clarity but is not blocking.
5. For every finding, state the confusing surface, the inferred intent, the evidence for that inference, and the smallest concrete edit that would clarify it.
6. If `--accept` is present, apply only high-confidence, behavior-preserving clarity fixes across impact levels. Leave risky renames, public API changes, broad refactors, and domain-judgment calls as unresolved findings with reasons. After editing, run the relevant project checks, byte-compile only where applicable, inspect the diff, and commit the scoped result.

## What to look for

- **Magic values**: hardcoded numbers, strings, thresholds, or sentinels without the reason they were chosen, such as `maxsize=65536`, `score >= 0.85`, or `"Zzzzz"`.
- **Misleading names**: functions, variables, or parameters whose names do not describe what they compute or represent, such as `_overlap` for Jaccard similarity.
- **Implicit phases or structure**: long functions with logical stages that are not separated by helper extraction or short orienting comments.
- **Undocumented conventions**: import aliases, key formats, sentinel values, cache semantics, or `None`/`nil` meanings that require project lore.
- **Missing rationale comments**: code where the mechanics are clear but the reason for the approach, threshold, ordering, or exception is not.
- **Score formula obscurity**: computed values whose intended range, monotonic behavior, or weighting tradeoff is unclear.
- **Duplicated explanatory logic**: repeated switches, threshold tables, or helper snippets that make the same concept harder to update consistently.
- **Type safety escapes**: `as any`, untyped returns, union abuse, or overloaded `None`/`nil` returns that hide distinct states or error cases.

## What not to flag

- Formatting or linting issues that a formatter/linter should handle.
- Missing tests, unless the absence of executable examples is the reason a convention cannot be understood.
- Performance issues, unless the optimization makes the code hard to understand and lacks rationale.
- Error handling or validation gaps that would change behavior rather than clarify current behavior.
- Clear code that merely follows a style you would not have chosen.

## What to highlight positively

Note clear naming, effective structure, helpful comments, readable abstractions, and documentation patterns worth copying elsewhere. Positive examples make the audit more useful than a list of complaints.

## Output format

In audit-only mode, organize the report as:

1. **Overall verdict**: one paragraph assessing reader clarity and the main source of interpretability risk.
2. **Clarity issues worth addressing**: grouped by impact, with file paths, function names, line numbers, inferred intent, why the current code obscures it, and a concrete fix.
3. **Things done well**: specific patterns that should be preserved or reused.
4. **Not worth changing**: lower-priority items you intentionally left alone.
5. **Verification**: what you read or checked, and what remains unverified.

In `--accept` mode, finish with files changed, fixes applied, unresolved findings with reasons, verification performed, and the commit hash.
