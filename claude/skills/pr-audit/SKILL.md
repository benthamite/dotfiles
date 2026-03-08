---
name: pr-audit
description: Verify a PR before submitting or after revising it. Runs compilation, tests, commit review, and diff checks to catch problems before a maintainer sees them. Use before pushing a new PR or after force-pushing revisions.
---

# PR verification

Thoroughly verify the current branch's PR before it is pushed or after it has been revised. The goal is to catch every class of problem that would be embarrassing if a maintainer found it.

Run the checks below **in parallel where possible** using subagents. Do not skip any step.

## 1. Build and test

- **Byte-compile / type-check / lint**: run whatever static analysis the project uses. For Elisp: `eask compile` or `emacs --batch -L . -f batch-byte-compile`. For other languages: the project's standard lint/typecheck command.
- **Run the full test suite**: use the project's test runner (`make test`, `eask test ert`, `pytest`, etc.). Report the exact pass/fail counts.
- **Note any pre-existing warnings** vs. warnings introduced by our changes.

## 2. Commit structure review

For each commit on the branch (relative to the PR base):

- **Read the full diff** (`git show <sha>`) and verify:
  - The commit touches only what its message describes — no unrelated changes leak in.
  - Changes are not duplicated across commits.
  - No changes are missing (nothing accidentally left out of the split).
  - Docstring and comment updates are in the same commit as the code they describe.
- **Check commit messages**: they should be concise, accurate, and follow the project's conventions.
- **Verify the final state** matches intent: `git diff <original-ref>..<new-tip>` should show no unintended differences if the commits were restructured (e.g., after a split or rebase). If applicable, diff against a saved copy of the pre-restructured state.

## 3. Diff against upstream

- Identify the PR base branch and its upstream HEAD (`gh api repos/OWNER/REPO/commits/BRANCH`).
- Verify our branch is based on the current upstream HEAD — flag if local base is stale.
- Check `git log <base>..HEAD` to confirm only our intended commits are included (no unrelated commits from a stale merge base).

## 4. File-level checks

- **No temporary or debug artifacts**: no leftover `console.log`, `print`, temp files, `.orig` files, commented-out code added by us.
- **No secrets or credentials** accidentally staged.
- **File endings**: no unintended whitespace changes, no mixed line endings introduced.

## 5. CI configuration (if changed)

- If CI config was modified, verify the changes are syntactically valid (e.g., YAML lint for GitHub Actions).
- Confirm that test commands referenced in CI actually exist in the Makefile/config.
- If possible, do a dry-run of the CI command locally.

## 6. Remote CI results (after pushing)

After a push or force-push, check the CI results on the remote:

- **Poll CI status** (`gh pr checks`, `gh run view`, etc.) and wait for results if still pending.
- **Inspect failures**: for any failing job, fetch the logs (`gh run view <id> --job <job-id> --log`) and determine whether the failure is caused by our changes or is a pre-existing/infrastructure issue.
- **Report clearly**: distinguish our failures (must fix) from environmental ones (e.g., dependency resolution bugs on a specific Emacs/OS version, flaky CI infra).

## Output

Report results in this order:

1. **Build/test results**: pass/fail, warnings
2. **Commit review**: any issues found per commit
3. **Upstream alignment**: base branch status
4. **File checks**: any artifacts or issues
5. **Overall verdict**: ready to push, or list of items to fix first

If everything passes, say so clearly. If there are issues, list them all before offering to fix.
