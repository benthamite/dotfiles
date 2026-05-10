---
name: post-push-ci
description: "Close the loop after agent pushes by waiting for GitHub Actions, inspecting failed logs, fixing push-triggered CI failures, and repeating until the pushed commit is green or a real blocker is identified. Use whenever an agent pushes commits or the user asks to check CI after a push."
---

# Post-push CI loop

Use this skill whenever you push code or the user asks you to check CI after a push. The goal is to avoid leaving the user to notice a red GitHub Actions run and manually paste URLs back into the agent.

Do not use this as permission to push. Only push when the current task or the user explicitly authorizes pushing. Once you have pushed, this skill applies automatically.

## Workflow

1. Record the repository, branch, and commit SHA you pushed:

   ```bash
   gh repo view --json nameWithOwner --jq .nameWithOwner
   git rev-parse --abbrev-ref HEAD
   git rev-parse HEAD
   ```

2. Run the helper from the repository you just pushed:

   ```bash
   ci-after-push
   ```

   If the push already happened before you invoked the skill, use:

   ```bash
   ci-after-push --no-push --commit "$(git rev-parse HEAD)"
   ```

3. If `ci-after-push` exits successfully, report the green runs and the pushed SHA.

4. If it exits non-zero because CI failed:
   - Treat the failed logs as the starting evidence.
   - Identify the root cause from the failed step and the relevant local code.
   - Reproduce locally when feasible, using the project’s documented commands or the CI command shown in the log.
   - Fix the root cause, not just the specific assertion or warning.
   - Run the narrow regression check and then the relevant full local check.
   - Commit the fix as a separate logical commit.
   - Push again and rerun `ci-after-push`.

5. Repeat the loop until the current pushed SHA has no failing GitHub Actions runs, or until you can state a concrete blocker such as missing credentials, unavailable GitHub access, an upstream service outage, or an environmental matrix failure that cannot be reproduced or fixed locally.

## Failure handling

- If GitHub Actions has multiple matrix jobs, inspect every failing job. Do not stop after the first failure if more failed logs are available.
- If a failure occurs only on an older runtime, check compatibility rather than assuming the latest local runtime is sufficient.
- If no run appears, verify the pushed SHA, branch, remote, and workflow trigger before concluding that CI is unavailable.
- If the helper times out, run `gh run list --commit SHA --event push` and inspect any pending or completed runs manually.
- If a failure is unrelated to the pushed change, state the evidence. Do not ignore it silently; decide whether the current task requires fixing it before calling the push complete.

## External action boundaries

- Do not create PRs, issues, review comments, Slack messages, emails, or other externally visible records as part of this loop unless the user explicitly authorizes that action.
- Do not force-push unless the user explicitly requested or approved it for the current task.
- Do not rewrite or revert user changes to get CI green. Work with the existing tree and ask only if the conflict makes progress impossible.

## Reporting

When done, include:

- Pushed repository and SHA.
- CI result and the GitHub Actions run URL(s).
- Failed job names and root causes, if any were fixed.
- Local verification commands run after the fix.
- Any checks that could not be run and the exact reason.
