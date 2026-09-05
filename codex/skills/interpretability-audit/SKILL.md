---
name: interpretability-audit
description: Audit code for intent transparency and reader comprehension. Use for understandable, readable, self-explanatory, naming, or future-maintainer clarity reviews; not for bug/security, architecture, DX, or lint-only reviews.
---

# Interpretability audit

Review $ARGUMENTS for clarity and intent transparency. The audit asks whether a competent maintainer can understand what the code means, why it is shaped this way, and which assumptions it relies on.

## Scope boundary

- In scope: naming clarity, magic values, missing rationale, undocumented conventions, hard-to-follow phases, unclear score formulas, duplicated explanatory logic, and type-safety escapes that obscure meaning.
- Out of scope: bug/security hunting, performance work, broad architecture
  changes, missing tests and formatting/lint-only cleanup. Separately flag a
  material defect encountered incidentally, but do not silently launch another
  audit or implement an out-of-scope fix.
- Clarifying intent does not authorize changing behavior. Keep new features,
  validation rules, public contracts and algorithm changes outside the clarity
  patch; explain any separate decision they require.
- Prefer clearer names and small extractions for "what" confusion, and comments for "why" or domain-context confusion.

## Workflow

1. Resolve paths, revision/worktree, requested depth and audit-versus-fix mode
   from the actual request; `$ARGUMENTS` is not the only source of authority.
   A plain-language request to apply fixes has the same effect as `--accept`.
   Otherwise remain read-only. For a general repo review, disclose which main
   paths and complex areas were sampled; an exhaustive request requires an
   inventory and coverage of every scoped item, with exclusions or blockers
   recorded rather than presenting sampling as complete.
2. Read applicable instructions, README/design notes, relevant tests and callers
   before judging local conventions. Preserve dirty/staged work. Existing prose
   may be stale: distinguish observed behavior from documented intent and
   hypotheses about historical rationale.
3. Read actual code and record stable file/symbol locations and current lines.
   Use independent exploration when available, useful and compatible with the
   requested order; keep workers inside the same authorized scope. Before any
   diagnostic command, inspect its effects: audit-only does not authorize
   formatter/snapshot rewrites, installs, network mutations or runtime reloads.
4. Classify findings by reader impact:
   - **High**: a maintainer is likely to misunderstand behavior, change the wrong thing, or miss a critical convention.
   - **Medium**: comprehension is slowed by hidden rationale, ambiguous names, or dense structure, but the confusion is localized.
   - **Low**: polish that would improve clarity but is not blocking.
5. For each finding, show the specific reading mistake or missing concept,
   evidence from code/callers/tests, and the smallest useful clarification.
   A familiar local idiom, long function or unexplained number alone is not a
   finding. Label inferred intent and unknown rationale; do not invent a reason
   for a threshold, order or sentinel and then encode it as a factual comment.
6. When fixes are authorized, apply only high-confidence behavior-preserving
   changes justified by reader impact. Do not make low-value edits to fill a
   severity tier. Before renaming or extracting, inspect callers, exports,
   reflection/string references, serialized keys, keyword arguments, evaluation
   order and side effects; an apparently private name may still be a contract.
   Leave risky changes and unknown domain judgments as findings with reasons.
7. Verify the actual changed surface: re-read the edited passage in caller
   context, confirm the misleading interpretation is removed, and check
   references/contracts. Run relevant behavior checks for executable changes;
   follow project-specific rules for compilation or live verification when
   applicable. Comments alone do not require unrelated execution. Do not claim
   behavior preservation from formatting or compilation alone. Inspect the
   final diff, commit only owned changes under project policy, and do not push
   or publish without separate authority.

## What to look for

These are investigation cues, not automatic findings; establish the actual
reader confusion and check whether local conventions already explain it.

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

Mention a clear naming, structure or documentation pattern only when preserving
it changes a useful decision. Do not manufacture praise or findings to fill a
report template. A well-supported no-findings result is valid.

## Output format

Lead with actionable findings or the no-findings verdict. For each finding,
include impact, location, the likely misunderstanding, evidence and the smallest
clarification. State coverage limits and material uncertainty. Add positive
examples or deferred minor items only when useful; keep empty sections out.

When fixes were requested, report the changes, unresolved decisions and relevant
verification limits, with the commit identity when one was created. Distinguish
source clarity review from behavioral or live verification.
