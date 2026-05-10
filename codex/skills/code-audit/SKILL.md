---
name: code-audit
description: "Audit code for actual defects: bugs, security vulnerabilities in application code, error handling gaps, and correctness problems. Use when the user asks to find bugs, review code for defects, debug risky edge cases, run a code vulnerability check, or apply confirmed fixes with --accept. Do not use for readability, architecture, test coverage, performance-only, or broad environment security reviews."
---

# Code audit

Review $ARGUMENTS for correctness, security, and robustness issues. The goal is to find **actual or potential defects** — things that could cause wrong behavior, data loss, security vulnerabilities, or silent failures.

If `--accept` is present in `$ARGUMENTS`, audit first, then fix all confirmed findings whose fix is clear and within the requested scope. Do not broaden into refactors, feature work, or speculative defensive rewrites. If a finding needs product judgment, credentials, or an externally visible action, report it as unresolved instead of guessing. Run the relevant verification for the project and commit only the accepted fixes.

## Workflow

1. **Resolve the scope**: use the explicit files, directories, diff, PR, or issue named in `$ARGUMENTS`; if none is provided, default to the current project. Read project instructions and check the working tree before editing.
2. **Identify verification**: inspect the project's docs and config for the right checks before changing code. Examples: byte-compile and ERT for Elisp, typecheck/lint/test commands for typed or compiled projects, dependency scanners when the audit includes package risk.
3. **Read real code paths**: inspect the implementation, callers, inputs, and persistence boundaries. Use available subagents or parallel searches for broad codebases, but do not infer findings from filenames alone.
4. **Confirm each finding**: trace a concrete failure mode, bad input, race, leak, or attack path. Prefer reproducible examples or precise reasoning over generic "could be safer" advice.
5. **Report or fix**: in normal mode, report findings in the format below and offer to fix critical and bug-level issues. With `--accept`, apply the confirmed in-scope fixes, verify them, and commit the result.

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
- Architecture, abstraction, duplication, or refactoring opportunities where behavior is already correct (use `/design-audit` for that)
- Missing features or enhancements
- Performance issues that don't affect correctness (unless they could cause timeouts or OOM)
- Test coverage gaps (mention if a critical path is untested, but don't audit test quality)
- Broad machine, secrets, dependency, or Claude Code posture issues outside the application code under review (use `/security-audit` for that)
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
- The concrete input, call path, data state, or threat model that makes it fail
- A concrete fix or approach

Then include:

- **Verification**: checks run, results, and any checks that were relevant but could not be run
- **No findings**: if no defects were found, say that clearly and describe the scoped files or paths reviewed plus any residual risk

At the end, offer to fix the critical and bug-level issues unless `--accept` was used. If `--accept` was used, summarize the fixes, verification, unresolved findings, and commit hash.
