# Skill artifact audit

Use this checklist for the targeted mode. Report only issues that can change
routing, execution, safety, verification, portability, or maintenance. Do not
report personal style preferences.

## Routing and scope

- The description states the real trigger and a useful near-miss boundary.
- Common user phrasing is present without broad keywords that attract unrelated
  tasks.
- Invocation metadata agrees with the intended automatic or explicit policy.
- The workflow is repeatable and has a clear purpose. It does not combine
  unrelated jobs or duplicate another skill without a boundary.

## Workflow and authorization

- Steps have a usable order, required inputs, and a concrete completion point.
- Deterministic repeated work lives in a script when that improves reliability.
- Conditional detail is in a routed reference rather than always loaded.
- The skill does not infer permission for writes, external actions, deletion,
  or a broader scope from an audit request.
- Tool, path, account, and secret-handling assumptions are explicit when they
  affect success.

## Verification

- Checks measure the requested behavior, not only syntax or wording.
- Complex routing has realistic should-trigger and near-miss prompts.
- Objective results use assertions, fixtures, or deterministic comparisons.
- Paired implementations are synchronized except for intentional tool metadata.
- Temporary artifacts are isolated and cleaned up.

## Finding format

Order findings by impact. For each finding, give the file and line, observed
behavior, why it matters, and the smallest concrete correction. Then state:

- strengths worth preserving;
- the smallest coherent next revision;
- the checks that would show the revision is better.

If there are no serious issues, say so and report only residual risks or
optional improvements.
