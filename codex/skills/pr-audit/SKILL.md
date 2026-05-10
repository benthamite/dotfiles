---
name: pr-audit
description: "Verify a pull request or PR-ready branch before submitting, pushing, merging, or after revising it. Runs build/test checks, commit review, upstream alignment, file hygiene, and remote CI checks when available. Use when the user asks to validate a PR, run pre-submit checks, review before pushing, check PR readiness, or verify after rebasing or force-pushing revisions."
---

# PR verification

Thoroughly verify the current branch's PR before it is pushed or after it has been revised. The goal is to catch every class of problem that would be embarrassing if a maintainer found it.

Parallelize independent checks where the runtime supports it, and use subagents when available. Keep dependencies explicit: discover the base before reviewing commits, and check remote CI only after a branch or PR has been pushed.

If a check is not applicable or cannot be run because required tooling, auth, network access, or project metadata is unavailable, mark it as **not run** with the concrete reason. Do not silently skip it.

Do not push, force-push, create a PR, post comments, approve reviews, or otherwise change remote/shared state unless the user explicitly asked for that action.

## When not to use

- General bug, security, or architecture review of code that is not tied to PR readiness; use `code-audit`, `security-audit`, or `design-audit` instead.
- A narrow request to run one known command, unless the user also asks for PR readiness or pre-submit verification.
- Investigation of why a check failed without a full PR audit; use `diagnose` when the user wants root-cause analysis first.

## 0. Scope and base discovery

- Record the current branch, `git status --short`, remotes, and whether the branch has an open PR. Treat unrelated dirty files as out of scope unless they are part of the requested PR audit.
- Identify the base branch and current upstream HEAD. Prefer `gh pr view --json baseRefName,baseRefOid,headRefName,headRefOid,url` for an existing GitHub PR; otherwise use the branch's upstream or the repository default branch from `refs/remotes/origin/HEAD`.
- Fetch the relevant remote refs when network/auth is available so stale-base checks use current data. If fetching is unavailable, report that upstream freshness is unverified.
- Define the commit range once, usually `<base>..HEAD`, and use it consistently for commit, diff, and file-level checks.

## 1. Build and test

- Discover the project's documented checks from files such as `README`, `Makefile`, package manifests, CI config, or local project instructions.
- **Byte-compile / type-check / lint**: run the standard static analysis command. For Elisp, prefer the project command if present; otherwise use an appropriate batch compile command such as `eask compile` or `emacs --batch -L . -f batch-byte-compile`.
- **Run the full test suite**: use the project's test runner (`make test`, `eask test ert`, `pytest`, etc.). Report the exact command, pass/fail counts, and any warnings.
- Distinguish pre-existing warnings or failures from warnings or failures plausibly introduced by the PR.

## 2. Commit structure review

For each commit on the branch (relative to the PR base):

- **Read the full diff** (`git show --stat --patch --find-renames <sha>`) and verify:
  - The commit touches only what its message describes — no unrelated changes leak in.
  - Changes are not duplicated across commits.
  - No changes are missing (nothing accidentally left out of the split).
  - Docstring and comment updates are in the same commit as the code they describe.
- **Check commit messages**: they should be concise, accurate, and follow the project's conventions.
- **Verify the final state** matches intent: if commits were restructured, compare the final diff against the saved pre-rewrite state or the original ref supplied by the user.

## 3. Diff against upstream

- Verify the branch is based on the current upstream HEAD. For GitHub PRs, compare against the base ref reported by `gh pr view`; otherwise compare against the fetched upstream/default branch.
- Flag a stale base if the current upstream base is not an ancestor of `HEAD`.
- Check `git log <base>..HEAD` and, when useful, `git log --left-right --cherry-pick --oneline <base>...HEAD` to confirm only intended commits are included.

## 4. File-level checks

- **No temporary or debug artifacts**: no leftover `console.log`, `print`, temp files, `.orig` files, commented-out code added by us.
- **No secrets or credentials** accidentally staged or introduced in the PR diff. Follow local secret-handling rules and never quote credential values in the report.
- **File endings and whitespace**: run `git diff --check` for the PR range and working tree as applicable; flag unintended whitespace changes or mixed line endings.

## 5. CI configuration (if changed)

- If CI config was modified, verify the changes are syntactically valid (e.g., YAML lint for GitHub Actions).
- Confirm that test commands referenced in CI actually exist in the Makefile/config.
- If possible, do a dry-run of the CI command locally.

## 6. Remote CI results

If the branch or PR has already been pushed, check the CI results on the remote. Do not push just to make this step possible.

- **Poll CI status** (`gh pr checks`, `gh run view`, etc.) and wait for results if still pending.
- **Inspect failures**: for any failing job, fetch the logs (`gh run view <id> --job <job-id> --log`) and determine whether the failure is caused by our changes or is a pre-existing/infrastructure issue.
- **Report clearly**: distinguish our failures (must fix) from environmental ones (e.g., dependency resolution bugs on a specific Emacs/OS version, flaky CI infra).

## Output

Report results in this order:

1. **Scope**: branch, base, PR URL if any, dirty-tree notes
2. **Build/test results**: commands run, pass/fail counts, warnings
3. **Commit review**: any issues found per commit
4. **Upstream alignment**: base branch status and whether freshness was verified
5. **File checks**: artifacts, secrets, whitespace, line endings
6. **Remote CI**: status, failures inspected, or why not run
7. **Overall verdict**: ready to push/merge, or list of items to fix first

If everything passes, say so clearly. If there are issues, list them all before offering to fix. Mention every not-run check and its reason.
