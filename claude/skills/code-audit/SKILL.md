---
name: code-audit
description: Audit code for bugs, security issues, error handling gaps, and correctness problems. Use when the user wants to find actual defects rather than readability issues.
argument-hint: [--accept] [dir]
---

# Code audit

Review $ARGUMENTS for correctness, security, and robustness issues. The goal is to find **actual or potential defects** — things that could cause wrong behavior, data loss, security vulnerabilities, or silent failures.

If `--accept` is present in `$ARGUMENTS`, after completing the audit, immediately fix **all** findings (critical, bugs, fragile, and minor) without asking for confirmation. Byte-compile and run tests after applying all fixes. Commit the result.

Use subagents to explore the codebase in parallel where appropriate. Read actual code — don't guess from file names.

## What to look for

### Correctness

- **Logic errors**: off-by-one, wrong operator, inverted conditions, short-circuit evaluation that skips side effects
- **Race conditions**: shared mutable state accessed without synchronization, TOCTOU (time-of-check-to-time-of-use) bugs
- **Unhandled edge cases**: empty inputs, None/null where not expected, division by zero, integer overflow, empty collections passed to min/max
- **Silent data loss**: exceptions caught and swallowed, truncation without warning, overwrites without backup
- **Incorrect assumptions**: hardcoded values that could drift from reality, stale caches that are never invalidated, assumptions about file encoding or line endings

### Security

- **Injection**: SQL injection, command injection, XSS, template injection, path traversal
- **Authentication/authorization gaps**: missing auth checks on endpoints, privilege escalation, insecure token handling
- **Secrets exposure**: credentials in code or logs, API keys in client-side bundles, verbose error messages leaking internals
- **Input validation**: missing or insufficient validation at system boundaries (API endpoints, file uploads, URL parameters)
- **Dependency risks**: known vulnerable packages, unpinned dependencies that could silently upgrade

### Error handling

- **Bare exception catches**: `except Exception`, `catch (e)` that swallow specific errors the caller should know about
- **Missing error paths**: API calls without timeout/retry, file operations without existence checks, network calls that assume success
- **Inconsistent error signaling**: mixing None returns, exceptions, and error codes for similar failure modes within the same module
- **Resource leaks**: files, connections, or browser contexts opened but not reliably closed (missing `finally`, `with`, or `try/catch`)

### Data integrity

- **Non-atomic operations**: multi-step writes that leave corrupt state if interrupted (no transactions, no atomic file writes)
- **Missing validation on imports/loads**: JSON or CSV data loaded without schema validation, assuming fields exist
- **Idempotency violations**: operations that claim to be idempotent but produce different results on re-run
- **Encoding issues**: mixing bytes and strings, assuming UTF-8 without handling BOM or other encodings

## What NOT to flag

- Style, formatting, or naming issues (use `/interpretability-audit` for that)
- Missing features or enhancements
- Performance issues that don't affect correctness (unless they could cause timeouts or OOM)
- Test coverage gaps (mention if a critical path is untested, but don't audit test quality)
- Code that is correct and handles errors properly — don't suggest defensive code for impossible conditions

## Output format

Organize findings into:

1. **Critical**: issues that could cause data loss, security breaches, or silent corruption in production
2. **Bugs**: things that are demonstrably wrong or will fail under specific conditions
3. **Fragile**: code that works now but will break under reasonable future changes or edge cases
4. **Minor**: small issues worth fixing when nearby code is being changed

For each finding, include:
- File path and line number(s)
- What's wrong (be specific — "this crashes when X is empty", not "error handling could be improved")
- A concrete fix or approach

At the end, offer to fix the critical and bug-level issues.
