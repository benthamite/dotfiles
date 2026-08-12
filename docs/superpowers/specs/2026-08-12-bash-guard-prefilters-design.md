# Bash Guard Prefilters Design

## Purpose

Reduce the common-path latency of Claude's `pretooluse-bash.sh` security
dispatcher without changing its allow, deny, context, or command-rewrite
behavior.

The current dispatcher starts many `grep` processes for every Bash tool call.
Most benign commands cannot match most guard rules. A Bash substring test can
skip selected `grep` calls when the command does not contain a string that the
regex must contain.

## Considered approaches

### Rewrite all regex checks in Bash

Reject this approach. Bash and BSD `grep` do not use the same regex dialect.
In particular, existing word-boundary expressions can silently stop matching.
This creates a fail-open security risk.

### Add prefilters to every `grep`

Reject this approach. The dispatcher contains dynamic regexes, negative
matches, extraction pipelines, ordered branches, and expressions without a
useful fixed literal. A broad conversion would duplicate too much policy and
would make the proof and maintenance burden too large.

### Add a small set of proven fixed-literal prefilters

Use this approach. Add prefilters only to Boolean positive matches on the
common benign-command path where a fixed literal is a necessary condition of
the unchanged regex. The implication for each site must be:

```text
original regex matches => prefilter matches
```

The prefilter can over-match. It must never under-match. When it matches, the
existing `grep` runs unchanged and remains the final authority.

## Scope

The first implementation will select only sites that meet all of these rules:

1. The `grep` is a Boolean positive test, not an extraction or transformation.
2. The test is not negated and does not form the negated part of a compound
   condition.
3. The regex has one clear fixed literal, or a small explicit set of fixed
   literals, that every possible match must contain.
4. The test runs on the common path for an ordinary command such as
   `git status --short`.
5. Skipping the `grep` has no side effect other than avoiding its process.

The first implementation will not change dynamic secret-pattern arrays,
high-entropy extraction, allowlist checks, output-redaction rules, delegated
guard behavior, or JSON parsing. It will not combine the `jq` calls.

## Implementation shape

Use a small Bash helper whose arguments keep the prefilter and unchanged regex
at the same call site. The helper returns false without starting `grep` when
none of its fixed literals occurs. Otherwise, it passes the original text and
regex to BSD `grep` without changing flags or pattern syntax.

Call sites must remain readable enough that a reviewer can see why each fixed
literal is necessary. Do not hide unrelated alternatives inside a generic
policy table.

## Verification

Tests will be added before production changes.

- A structural test will require every optimized call site to declare its
  fixed literal next to its regex.
- A per-site invariant test will generate and mutate representative commands
  and check that no regex match is rejected by its prefilter.
- A differential test will run the current and optimized dispatchers over a
  corpus that covers benign commands, denials, allowances, multiline input,
  quoting, and command rewrites. It will compare exit status, standard error,
  and exact standard output.
- Existing security-hook tests will run unchanged.
- A benchmark will measure the full dispatcher on benign and guarded commands.
  The change will be kept only if it reduces benign-command latency without a
  material regression on guarded commands.
- The live Claude path will be exercised after the tracked tests pass, because
  standalone hook execution does not prove live hook registration.

## Failure and maintenance rules

If a candidate prefilter cannot be proved to be a necessary condition, leave
that `grep` unchanged. If the benchmark shows little benefit, keep the tests
that describe the guard behavior but do not expand the optimization.

Any later regex change at an optimized site must update or revalidate the
adjacent prefilter. The invariant and differential tests must fail if that
relationship drifts for covered inputs.
