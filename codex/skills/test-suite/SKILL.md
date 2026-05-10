---
name: test-suite
description: Create or expand high-value test suites for a codebase or specific module. Use when the user asks to add or write tests, improve coverage, bootstrap testing, create regression tests, or test a module/codebase; not for ordinary bug-fixing unless tests are the requested deliverable.
---

# Test suite

Create a comprehensive test suite for $ARGUMENTS (if no argument provided, default to the current project). The goal is to produce **tests that catch real bugs** - not ceremonial tests that merely confirm the code runs without crashing.

Use subagents to explore the codebase in parallel where appropriate (e.g., one for backend, one for frontend, one for shared utilities). Read actual code, understand its behavior, and write tests against that behavior.

## Scope boundaries

Use this skill when the deliverable is new or substantially improved tests. Do not use it for ordinary debugging, code review, lint cleanup, PR validation, or one-off command verification unless the user explicitly asks to add tests as part of that work. Route those requests to the neighboring debugging, audit, lint, or PR verification workflows.

Keep the work scoped to the requested project, module, or risk area. If comprehensive coverage would require a broad architecture change, live service credentials, or a new test framework that conflicts with project conventions, document that limit instead of forcing a workaround.

## Process

### 1. Reconnaissance

Before writing any tests:

- **Read project instructions**: check `AGENTS.md`, `CLAUDE.md`, README testing sections, package scripts, and CI config so the suite follows local rules.
- **Check worktree state**: note unrelated dirty files before editing and avoid overwriting user changes.
- **Identify the tech stack**: languages, frameworks, existing test runners, assertion libraries, and fixture patterns already in use. Adopt the project's existing conventions.
- **Find existing tests**: look for test directories, test files, config files (`pytest.ini`, `vitest.config.ts`, `jest.config.js`, `.mocharc.yml`, etc.). Study the patterns and style already established.
- **Identify verification commands**: find the targeted test command, full suite command, lint/typecheck commands for tests, and any required environment setup.
- **Map the codebase**: identify the modules, entry points, data flows, and external boundaries (APIs, databases, file I/O, third-party services).
- **Find the critical paths**: which code handles money, auth, data persistence, user input parsing, or state transitions? These get tested first.
- **Define coverage goals**: state which behavior, module, or risk area the new tests must cover before writing them.

If there is no testing infrastructure at all, set up the smallest idiomatic runner, config, and directory structure before writing tests. Prefer tools already standard for the language or framework; if adding dependencies, update the relevant manifest and lockfile and explain the choice.

### 2. What to test

Prioritize tests by the damage a bug would cause:

#### Critical (test these first)

- **Data integrity**: functions that write, transform, or delete data — verify they produce correct output and don't corrupt state
- **Input boundaries**: parsers, validators, API endpoints — test with valid input, invalid input, empty input, huge input, and type-confused input
- **State transitions**: anything with multiple states (auth flows, multi-step wizards, reconciliation pipelines) — test every transition and the illegal ones
- **Error paths**: what happens when the database is down, the file doesn't exist, the API returns 500, the JSON is malformed? Test that errors are handled, not swallowed

#### Important

- **Business logic**: calculations, scoring, matching, filtering, sorting — test with representative real-world data AND edge cases
- **Integrations**: test that components work together correctly (API route -> service -> database, frontend -> API client -> backend)
- **Idempotency**: operations that claim to be idempotent (imports, upserts, sync) — run them twice and verify identical results
- **Concurrency**: if the code uses threads, async, or parallel processing — test for race conditions, deadlocks, and ordering issues

#### Useful

- **Regressions**: if the codebase has a bug history (git log, issue tracker), write tests that would have caught past bugs
- **Configuration**: test that the system handles missing config, invalid config, and config with edge-case values
- **Serialization round-trips**: encode then decode, serialize then deserialize — verify nothing is lost or mangled

### 3. How to write good tests

- **Test behavior, not implementation**: test what a function does, not how it does it. Tests that break when you refactor internals are a liability.
- **One assertion per logical concept**: each test should verify one thing. If it fails, you should know exactly what broke without reading the test body.
- **Descriptive names**: test names should describe the scenario and expected outcome (e.g., `test_matching_rejects_empty_title`, not `test_matching_3`).
- **Arrange-Act-Assert**: set up state, perform the action, check the result. Keep these phases visually distinct.
- **Use real-ish data**: don't test a CSV parser with `"a,b,c"` — use data that resembles the actual inputs the code will process. Copy sanitized examples from the project's own data files when possible.
- **Test the unhappy path**: for every happy-path test, write at least one test for the corresponding failure mode.
- **Avoid mocking internals**: mock at system boundaries (network, filesystem, database), not between your own modules. Over-mocking creates tests that pass while the real code is broken.
- **No sleeping**: don't use `time.sleep()` or `setTimeout()` in tests. Use proper async waiting, test clocks, or event-based synchronization.

### 4. What NOT to do

- **Don't test framework code**: don't verify that FastAPI returns 200 for a valid route definition or that SQLAlchemy can connect to a database. Trust the framework.
- **Don't test trivial getters/setters**: a property that returns `self._name` doesn't need a test.
- **Don't write tests that always pass**: a test with no meaningful assertion, or one that catches all exceptions and passes anyway, is worse than no test — it gives false confidence.
- **Don't duplicate tests**: if two tests exercise the same code path with the same logic, keep the more descriptive one and delete the other.
- **Don't over-parametrize**: parametrized tests are powerful, but 50 cases in one parametrized test are harder to debug than 5 focused tests. Use parametrize for systematic variation (e.g., all date formats), not as a substitute for thinking about what to test.
- **Don't ignore test failures**: if a test you wrote fails, investigate. It might have found a real bug. Don't delete or skip it to make the suite green.

### 5. Handling failures and bugs

- Run new tests in isolation before running the broader suite, so failures are easy to localize.
- If a failure comes from a bad test, fixture, or test infrastructure, fix the test code.
- If a failure exposes a real product bug, keep the test as the reproduction. Fix the bug only when it is within the requested scope and the fix is narrow; otherwise report the failing command, the suspected bug, and the smallest next step.
- Do not weaken assertions, add broad skips, or introduce silent fallbacks just to make the suite pass.

## Verification

Before reporting completion:

1. Run the targeted new tests.
2. Run the relevant existing suite, or the full suite when it is practical.
3. Run project-standard lint, typecheck, formatting, or compile checks that cover the changed tests.
4. Re-read changed tests and fixtures to confirm assertions would fail for the wrong behavior, not just for crashes.

If any check cannot be run, state the exact command or check that was skipped and why.

## Output format

Organize the work into:

1. **Test infrastructure**: any setup needed (new dependencies, config files, fixtures, factories, test utilities)
2. **Tests by priority**: write critical tests first, then important, then useful — so even if time runs out, the highest-value tests exist
3. **Coverage summary**: after writing tests, summarize what is and isn't covered, and note any areas that were deliberately skipped (with reasons)

For each test file, include:
- A brief comment at the top explaining what module/functionality it covers
- Tests grouped by function or feature being tested

After writing the tests, **run them** and fix failures caused by the tests or infrastructure before reporting results. If a remaining failure exposes a real product bug that is out of scope to fix, report it as an unresolved blocker; do not call the suite complete.

At the end, report:

- Test files and infrastructure changed
- Commands run and their results
- Coverage added and important gaps left
- Bugs uncovered, whether they were fixed, and any unresolved failures or follow-up work
