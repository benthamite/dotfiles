---
name: interpretability-audit
description: Audit a codebase for intent transparency and readability. Use when the user wants to check if code is understandable, if intentions are clear, or if there are obscurities a reader would struggle with.
---

# Interpretability audit

Review $ARGUMENTS for clarity and intent transparency. The goal is NOT to find bugs, security issues, or style violations — it's to identify places where a reader (like yourself) would struggle to understand what the code is doing or why.

Use subagents to explore the codebase in parallel where appropriate (e.g., one for backend, one for frontend). Read actual code — don't guess from file names.

## What to look for

For each file or module you examine, note:

- **Magic values**: hardcoded numbers, strings, or thresholds without explanation of why they were chosen (e.g., `maxsize=65536`, `score >= 0.85`, a `"Zzzzz"` sentinel)
- **Misleading names**: functions, variables, or parameters whose names don't accurately describe what they compute or represent (e.g., a function called `_overlap` that computes Jaccard similarity)
- **Implicit phases or structure**: long functions with multiple logical stages that aren't delineated with comments or method extraction — the reader has to reverse-engineer the structure
- **Undocumented conventions**: patterns (like import aliases, sentinel values, key formats, or `None` meaning "pending") that a reader wouldn't understand without project context
- **Missing "why" comments**: code where the *what* is clear from reading it but the *reason* for the approach is not
- **Score formula obscurity**: computed values (scores, thresholds, mappings) where the formula isn't accompanied by an explanation of the intended range or behavior
- **Duplicated logic**: the same logic (switch statements, helper functions, threshold values) copy-pasted across files instead of being extracted to a shared module
- **Type safety escapes**: `as any`, untyped returns, or `None` return values that conflate different error conditions

## What NOT to flag

- Formatting or linting issues (that's what linters are for)
- Missing tests (that's a separate concern)
- Performance issues (unless they're also clarity issues)
- Suggestions to add features or error handling beyond what exists
- Code that is clear and working fine — don't suggest changes for the sake of changes

## What to highlight positively

Also note things that ARE well done — clear naming, good structure, effective documentation, elegant abstractions — so the audit is balanced and the positive patterns can serve as models for the rest of the codebase.

## Output format

Organize findings into:

1. **Overall verdict**: one paragraph assessing the codebase's clarity
2. **Clarity issues worth addressing**: grouped by priority (highest-impact first), with specific file paths, function names, and line numbers
3. **Things that are done well**: specific examples of clear, well-structured code
4. **What I would NOT bother with**: lower-priority items that aren't worth the effort unless onboarding other developers

At the end, offer to address the high-impact issues.
