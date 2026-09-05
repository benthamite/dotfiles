---
name: test-suite
description: Create or expand high-value test suites for a codebase or specific module. Use when new tests, regression tests, coverage improvements, or test infrastructure are the deliverable; not for merely running an existing suite, reviewing code, or ordinary bug-fixing without a requested test deliverable.
argument-hint: "[dir]"
---

# Test suite

Build tests for the requested project, module or risk area. If no target was
provided, use the established current project; treat supplied paths as data,
not shell commands. Prioritize behavior whose failure would matter, not test
counts or a nominal coverage percentage.

## Establish scope and the test environment

Read applicable project instructions, testing documentation, runner/config,
CI commands, and representative existing tests. Inspect the selected worktree
and preserve unrelated edits. Identify the production entry points, external
boundaries and known test conventions before choosing new infrastructure.
Delegate independent bounded areas when useful, with disjoint edit ownership
and the same privacy, environment and verification constraints.

Distinguish writing tests from merely running or assessing them. A tests-only
request does not authorize product repairs, refactoring, shared CI/service
changes or external writes. If no runner exists and test setup is within the
request, use the smallest compatible conventional runner and update its
manifest/lockfile through normal tooling. Do not silently replace an existing
framework or enable a paid/live service.

Before executing even the existing suite, inspect setup/teardown and commands
for side effects. Use owned disposable fixtures and explicit isolated homes,
config, credentials, databases and network destinations where relevant.
Never inherit a real account or production store just because a runner calls
it “test.” Mock or disable external writes by default; live integration checks
need applicable authorization and a confirmed isolated destination. Keep
dependencies, caches, builds and temporary artifacts outside Drive. Register
cleanup for owned files, processes, ports and fixtures, including failures.

Record the relevant baseline when safe and practical. Identify the command,
selected tests, source checkout/revision, interpreter/runtime and fixture
environment. Verify imports/load paths or built artifacts refer to the
intended changed source, not a stale installed copy. Missing prerequisites
or unsafe effects are explicit limits, not silently successful checks.

## Choose meaningful contracts

Derive expectations from requirements, public contracts, independently worked
examples or trusted domain rules. Read implementation to understand paths,
not to turn its current output into the expected answer. When requirements
are ambiguous, distinguish characterization of existing behavior from a
correctness claim and resolve material uncertainty before inventing assertions.

Select cases by impact and the requested scope:

- Data integrity: exact persisted state and absence of corruption or unintended
  writes, including partial failure and recovery.
- Input boundaries: valid, empty, malformed, type-confused and size-limit cases;
  keep resource-stress cases bounded to owned test capacity.
- State transitions: allowed and rejected transitions, retries, cancellation,
  errors and recovery, especially where authorization or persistence changes.
- Business logic: representative calculations, ordering, matching and boundary
  cases with independently established expected values.
- Integrations: actual wiring across owned components and framework boundaries,
  not only isolated mocks that agree with each other.
- Idempotency: repeated calls leave the promised state and side effects intact;
  identical return values alone are not proof. Include ambiguous/partial retry
  behavior when the operation claims to handle it.
- Concurrency: controlled interleavings, ordering, cancellation and bounded
  deadlock detection, rather than relying on a lucky scheduler.
- Regressions/configuration/serialization: relevant past defects, missing or
  invalid configuration, known-value encodings and information preservation.
  A round trip alone can hide matching encoder/decoder defects.

Use synthetic representative fixtures by default. Small examples are valuable
when they isolate a boundary. Do not read/copy private project data, transcripts,
credentials or customer material into tests merely for realism; sanitization
alone does not establish permission to store or publish it.

## Write discriminating tests

Follow the established test style, names and discovery rules. Keep a coherent
behavior per test, with enough assertions to verify its result, state changes
and forbidden side effects. Parameterize meaningful systematic variation with
identifiable cases; do not impose arbitrary assertion or case-count limits.

Exercise the real production entry point at the appropriate layer. Mock
unavailable or externally mutating boundaries, not the behavior being claimed
as tested. Use real isolated filesystem/database/runtime paths where the
contract depends on them, and state what mocks leave unmeasured. Framework
wiring, schema migrations and application configuration are valid test targets
even when framework internals themselves need no retesting.

Ground boundary doubles in the actual supported interface or authoritative
contract, not guessed fields or event ordering. Add a decisive unmocked
interface check when safe and relevant; a synthetic double alone cannot
establish compatibility with the installed runtime or remote service.

Prefer event/barrier synchronization, test clocks and bounded condition waits.
Give event/barrier waits finite deadlines too; a missing signal must not hang
the suite indefinitely.
Do not use fixed delays as evidence that async work finished. Testing actual
timeout or scheduler behavior may require real elapsed time; bound it, explain
the need and observe the condition that proves success or failure.

Avoid vacuous assertions, broad exception swallowing and tests that merely
duplicate implementation logic. Do not remove existing tests because they
look similar: establish their distinct contracts and preserve intentional
regressions. Any consolidation must stay within scope and preserve coverage.

## Failures and verification

1. Run the new tests in isolation and confirm they were discovered and executed.
   A zero-test run, unexpected skip/xfail, or collection error is not a pass.
2. Demonstrate that important assertions discriminate: for a regression,
   reproduce the defect before an authorized repair; otherwise use a known
   failing example or a controlled mutation in an isolated owned copy when
   practical. Do not edit or reset the user's production tree to manufacture
   red evidence. Record a gap when only static assertion review was possible.
3. Diagnose failures as test/fixture defects, product defects or environment
   limits using the baseline and evidence. Fix owned faulty tests or setup.
   Keep a valid failing reproduction; do not weaken expectations, regenerate
   snapshots from unexplained output, broadly skip, or repeatedly rerun until
   one lucky green result hides flakiness.
4. Repair product code only when implementation is also authorized and within
   scope. A tests-only request may finish with a useful failing regression and
   an explicit unresolved product defect, but never a claim that the suite is
   green or the bug fixed. Continue unaffected in-scope test work.
5. Run the relevant existing suite, and the full suite when safe and practical,
   plus applicable checks for changed tests. Reconcile new failures with the
   baseline; unexplained failures remain unresolved.
6. For tests claiming live/runtime acceptance, observe the decisive surface,
   not just a mock or unit assertion. Report the tested source and evidence
   boundaries accurately. Clean up owned disposable artifacts and processes.

Summarize the coverage added, important gaps, commands/results and any discovered
unresolved defects. Distinguish writing the requested tests from obtaining a
green suite or verifying a product repair. Include paths and setup details only
where they help the handoff; do not force a report section or boilerplate file
comment for every test.
