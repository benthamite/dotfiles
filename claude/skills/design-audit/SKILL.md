---
name: design-audit
description: Audit code for suboptimal architecture, unnecessary complexity, missing or wrong abstractions, duplication, and refactoring opportunities. Use when the user wants to improve code structure rather than find bugs or readability issues.
argument-hint: [--accept] [dir]
argument-choices: "--accept"
---

# Design audit

Review $ARGUMENTS for architectural quality and structural soundness. The goal is to find code that **works correctly and reads clearly** but is structured in a way that makes it harder to maintain, extend, or reason about than it needs to be.

If `--accept` is present in `$ARGUMENTS`, after completing the audit, immediately refactor **all** findings (high, medium, and low impact) without asking for confirmation. Byte-compile and run tests after applying all changes. Commit the result.

Use subagents to explore the codebase in parallel where appropriate. Read actual code — don't guess from file names.

## What to look for

### Abstraction problems

- **Premature abstraction**: generalized frameworks, configuration-driven behavior, or extension points that serve only one use case — the abstraction adds indirection without earning its keep
- **Missing abstraction**: repeated patterns (3+ occurrences) that should be extracted into a shared function, macro, or module
- **Wrong level of abstraction**: functions that mix high-level orchestration with low-level details, or modules that bundle unrelated responsibilities
- **Leaky abstraction**: callers that need to know internal details of a function or module to use it correctly (e.g., relying on side effects, required call order, internal state)

### Complexity

- **Unnecessary indirection**: wrapper functions that add nothing, delegation chains that pass through unchanged, adapter layers with no adaptation
- **Overengineering**: feature flags for features that will never be toggled, plugin systems with one plugin, strategy patterns with one strategy, configuration for values that never change
- **God functions/modules**: single units that do too many things and would benefit from decomposition
- **Deep nesting**: logic that could be flattened with early returns, guard clauses, or extraction into named helpers

### Structural issues

- **Circular dependencies**: modules that import each other, creating coupling that makes both harder to change
- **Shotgun surgery**: a single logical change requires touching many unrelated files because related logic is scattered
- **Feature envy**: code that primarily operates on another module's data rather than its own — suggests the logic belongs elsewhere
- **Dead code**: functions, branches, parameters, or configuration that are never used and serve no documentary purpose
- **Inconsistent patterns**: the same kind of task (e.g., error handling, data transformation, API calls) done differently in different places without justification

### Interface design

- **Unclear contracts**: functions whose behavior depends on implicit assumptions rather than explicit parameters (e.g., relying on global state, environment variables, or call order)
- **Boolean blindness**: functions that take multiple boolean flags, making call sites unreadable (`process(data, true, false, true)`)
- **Primitive obsession**: passing around raw strings, numbers, or lists where a named type or struct would prevent misuse and clarify intent
- **Kitchen-sink parameters**: functions with many optional parameters that serve different use cases — suggests the function should be split

### Duplication

- **Copy-paste code**: identical or near-identical logic in multiple places (not just similar-looking code — it must be genuinely the same operation)
- **Parallel hierarchies**: two structures (e.g., a set of handlers and a set of validators) that must always be updated in lockstep — suggests they should be unified
- **Reimplemented stdlib**: logic that duplicates what the language or framework already provides

## What NOT to flag

- Bugs, security issues, or error handling gaps (use `/code-audit` for that)
- Naming, comments, or readability issues (use `/interpretability-audit` for that)
- Style or formatting issues (that's what linters are for)
- Performance optimizations (unless the structure itself prevents reasonable performance)
- Missing features or tests
- Simple code that works — don't suggest abstractions for straightforward, non-duplicated logic
- Patterns that are idiomatic for the language/framework even if they seem verbose

## Output format

Organize findings into:

1. **Overall assessment**: one paragraph on the codebase's structural health — is it well-factored, or does it have systemic issues?
2. **High impact**: structural problems that actively make the codebase harder to maintain or extend, with clear refactoring paths
3. **Medium impact**: issues worth addressing when working in the area, but not urgent
4. **Low impact**: things that are slightly off but not worth a dedicated refactoring effort
5. **Good design**: specific examples of well-structured code — clean interfaces, appropriate abstractions, good module boundaries — so positive patterns can be reinforced

For each finding, include:
- File path and line number(s)
- What the structural problem is (be specific — "this 200-line function has three distinct phases that should be separate functions", not "this function is too long")
- A concrete refactoring approach
- What the improvement buys you (e.g., "makes it possible to test the validation logic independently")

At the end, offer to refactor the high-impact issues.
