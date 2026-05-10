---
name: triage-issues
description: Scan, prioritize, and batch-resolve open GitHub issues for tlon-team/tlon.el. Use when the user says "triage issues", "process issues", "fix issues batch", "resolve issues", "work through issues", or wants to systematically address open GitHub issues.
argument-hint: [batch-size]
---

# Triage and resolve tlon.el issues

Systematically scan, prioritize, and resolve open GitHub issues for `tlon-team/tlon.el`. Process them in small batches, fixing the easiest ones first.

## Configuration

- **Repository**: `tlon-team/tlon.el`
- **Codebase path**: the current working directory (should be the tlon.el repo checkout)
- **Org file**: `/Users/pablostafforini/My Drive/tlon-notes/tlon.el.org`
- **GitHub comment user**: `tlon-ai` (uses `TLON_AI_GITHUB_TOKEN` env var)
- **Default batch size**: 3 (override with `$ARGUMENTS`, e.g. `/triage-issues 5`)

## Workflow

### 1. Fetch and prioritize issues

Fetch all open issues:

```bash
gh issue list --repo tlon-team/tlon.el --state open --limit 100 --json number,title,body,labels,createdAt
```

Read the codebase to understand each issue's complexity. Prioritize by ease of resolution:

1. **Trivial**: typos, missing requires, simple config changes, obvious one-line fixes
2. **Easy**: small bug fixes, straightforward function additions, clear error messages
3. **Medium**: multi-file changes with clear scope, refactoring with tests
4. **Hard**: architectural changes, features requiring design decisions, unclear requirements
5. **Needs clarification**: issues with ambiguous descriptions or multiple possible approaches

Sort issues by priority (trivial first). Use subagents in parallel to assess complexity — read the relevant source files for each issue before ranking.

### 2. Present the batch

Present the top N issues (N = batch size) to the user. For each issue, show:

- Issue number and title
- Brief assessment of what needs to be done
- Estimated complexity (trivial/easy/medium/hard)
- Any questions or concerns

### 3. Interview (if needed)

If any issue in the batch is ambiguous, has multiple valid approaches, or needs user input, ask the user before proceeding. Use `AskUserQuestion` to gather preferences. Keep questions focused and actionable.

### 4. Fix the issues

For each issue in the batch:

1. Read all relevant source files thoroughly before making changes.
2. Write the fix. Follow the project's existing coding conventions (see the `elisp-conventions` skill if available).
3. Byte-compile modified files to check for errors: `emacs -Q --batch -L . -f batch-byte-compile <file>.el`
4. Commit each fix in a separate commit with a clear message referencing the issue number (e.g., `fix: handle empty input in tlon-get-counterpart (#181)`).

Use subagents to work on independent issues in parallel when possible.

### 5. Close issues on GitHub

For each resolved issue, close it with a comment **from the `tlon-ai` user**. Use `GH_TOKEN` (not `GITHUB_TOKEN` — `gh` CLI only respects `GH_TOKEN` for overriding stored auth):

```bash
# Post closing comment as tlon-ai
GH_TOKEN="$TLON_AI_GITHUB_TOKEN" gh issue comment <NUMBER> --repo tlon-team/tlon.el --body "Fixed in commit <SHA>. <brief description of what was done>"

# Close the issue as tlon-ai
GH_TOKEN="$TLON_AI_GITHUB_TOKEN" gh issue close <NUMBER> --repo tlon-team/tlon.el
```

**IMPORTANT**:
- Always use `GH_TOKEN=`, not `GITHUB_TOKEN=`. The `gh` CLI prioritizes its stored credentials over `GITHUB_TOKEN`, but `GH_TOKEN` takes precedence over everything.
- **Before posting any comments**, verify the token works: `GH_TOKEN="$TLON_AI_GITHUB_TOKEN" gh api user --jq '.login'` must return `tlon-ai`. Also test that writes work: post a test comment, verify `user.login` is `tlon-ai` in the response, then delete it.
- **NEVER silently fall back to the default `gh` auth (benthamite)**. If `TLON_AI_GITHUB_TOKEN` is not set or the token fails, **stop and tell the user** — do not post under the wrong account.
- The token must be a **classic PAT** (prefix `ghp_`) with `repo` scope. Fine-grained PATs have org approval issues that block writes.

### 6. Update the org file

For each resolved issue, archive its heading in `/Users/pablostafforini/My Drive/tlon-notes/tlon.el.org` using Emacs batch mode. **Do NOT manually edit the org file** — use `org-archive-subtree-default` which handles TODO state, CLOSED timestamp, ARCHIVE_TIME property, and moving the heading to the archive section automatically:

```bash
emacs --batch \
  -l org \
  --eval '(progn
            (find-file "/Users/pablostafforini/My Drive/tlon-notes/tlon.el.org")
            (goto-char (point-min))
            (when (re-search-forward "#<ISSUE-NUMBER> " nil t)
              (org-todo "DONE")
              (org-archive-subtree-default))
            (save-buffer))'
```

Replace `<ISSUE-NUMBER>` with the actual issue number. If the search fails (heading not found), skip the org update and note it in the summary.

### 7. Summary and continuation

After processing the batch, present a summary table. Include issue URLs so the user can verify. Example:

| Issue | Action | Commit | Org archived |
|-------|--------|--------|-------------|
| [#126](https://github.com/tlon-team/tlon.el/issues/126) | Closed as duplicate of #142 | — | Yes |
| [#74](https://github.com/tlon-team/tlon.el/issues/74) | Removed unused `Package-Requires` entries | `6326457d` | Yes |

Also note any issues that were skipped or need follow-up.

Then ask: "Want to proceed to the next batch?"

If the user agrees, repeat from step 1 with the remaining issues.

## Important guidelines

- Never guess at fixes — read the actual code first.
- If a fix is non-trivial or risky, discuss it with the user before applying.
- Keep commits atomic: one issue per commit.
- Do not mix unrelated changes.
- Always byte-compile after changes to catch errors.
- When in doubt about an issue's intent, ask the user rather than guessing.
