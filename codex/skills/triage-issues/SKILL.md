---
name: triage-issues
description: Triage or batch-fix GitHub issues for tlon-team/tlon.el. Use when the user says "triage issues", "process tlon.el issues", "fix issues batch", "work through open issues", or asks to systematically prioritize or resolve tlon.el GitHub issues. Do not use for unrelated repositories or generic troubleshooting.
---

# Triage and resolve tlon.el issues

Systematically scan, prioritize, and resolve open GitHub issues for `tlon-team/tlon.el`. Process them in small batches, fixing the easiest ones first.

The workflow has three modes:

- **Read-only triage**: fetch, assess, prioritize, and present issues.
- **Local fix**: edit, test, and commit fixes in the local `tlon.el` checkout.
- **External closure**: comment on or close GitHub issues as `tlon-ai` and archive the matching org heading.

Only advance from triage to local fixes or external closure when the current user request explicitly asks for that mode or the user approves it after seeing the batch.

## Configuration

- **Repository**: `tlon-team/tlon.el`
- **Codebase path**: the current working directory, after verifying it is the `tlon.el` repo checkout
- **Org file**: `/Users/pablostafforini/My Drive/tlon-notes/tlon.el.org`
- **GitHub comment user**: `tlon-ai` (uses `TLON_AI_GITHUB_TOKEN` env var; never print the token)
- **Default batch size**: 3 (override with `$ARGUMENTS`, e.g. `/triage-issues 5`)

## Workflow

### 0. Preflight and scope gate

1. Confirm the current directory is the intended checkout before reading or editing code:

   ```bash
   git rev-parse --show-toplevel
   git remote -v
   ```

   The remote must point to `tlon-team/tlon.el`. If it does not, stop and tell the user the skill must run from the `tlon.el` checkout.
2. Inspect `git status --short`. Preserve unrelated user changes; do not overwrite, stage, or commit them.
3. Parse the batch size from the first numeric argument. Use the default batch size if none is provided.
4. Determine the authorized mode from the current request:
   - If the user only asked to triage, stay in read-only triage mode and stop after presenting the batch.
   - If the user asked to fix or resolve issues, local edits and commits are allowed after presenting the selected batch.
   - If the user explicitly authorized GitHub comments, issue closure, and org archiving, external closure is allowed. Otherwise ask before doing any externally visible action.

### 1. Fetch and prioritize issues

Fetch all open issues:

```bash
gh issue list --repo tlon-team/tlon.el --state open --limit 300 --json number,title,body,labels,createdAt,url
```

If the result appears truncated, raise the limit or paginate before ranking.

Read the codebase to understand each issue's complexity. Prioritize by ease of resolution:

1. **Trivial**: typos, missing requires, simple config changes, obvious one-line fixes
2. **Easy**: small bug fixes, straightforward function additions, clear error messages
3. **Medium**: multi-file changes with clear scope, refactoring with tests
4. **Hard**: architectural changes, features requiring design decisions, unclear requirements
5. **Needs clarification**: issues with ambiguous descriptions or multiple possible approaches

Sort issues by priority (trivial first). Use subagents or agent teams in parallel when available; otherwise assess sequentially. In either case, read the relevant source files for each issue before ranking.

### 2. Present the batch

Present the top N issues (N = batch size) to the user. For each issue, show:

- Issue number and title
- Issue URL
- Brief assessment of what needs to be done
- Estimated complexity (trivial/easy/medium/hard)
- Any questions or concerns
- The proposed next mode: stop after triage, fix locally, or fix and close

### 3. Interview (if needed)

If any issue in the batch is ambiguous, has multiple valid approaches, or needs user input, ask before proceeding. Use the available question/input mechanism for the current tool, and keep questions focused and actionable.

If local fix mode was not already explicit in the user's request, ask for approval before editing files. If external closure mode was not already explicit, ask again before commenting on GitHub, closing issues, or archiving org headings.

### 4. Fix the issues

For each issue in the batch:

1. Read all relevant source files thoroughly before making changes.
2. Write the fix. Follow the project's existing coding conventions (see the `elisp-conventions` skill if available).
3. Run the narrowest meaningful project checks for the changed files. At minimum, byte-compile modified Elisp files:

   ```bash
   emacs -Q --batch -L . -f batch-byte-compile <file>.el
   ```

4. Inspect `git diff` and `git status --short` before staging so unrelated changes are not included.
5. Commit each fix in a separate commit with a clear message referencing the issue number (e.g., `fix: handle empty input in tlon-get-counterpart (#181)`).

Use subagents or agent teams for independent issues when available; otherwise work one issue at a time.

### 5. Close issues on GitHub

Only run this section when external closure mode is explicitly authorized.

For each resolved issue, close it with a comment **from the `tlon-ai` user**. Use `GH_TOKEN` (not `GITHUB_TOKEN` — `gh` CLI only respects `GH_TOKEN` for overriding stored auth):

```bash
# Confirm the token exists and belongs to tlon-ai without printing the token.
test -n "${TLON_AI_GITHUB_TOKEN:-}" || { echo "TLON_AI_GITHUB_TOKEN is not set"; exit 1; }
GH_TOKEN="$TLON_AI_GITHUB_TOKEN" gh api user --jq '.login'

# Post closing comment as tlon-ai and verify the comment author.
COMMENT_ID=$(GH_TOKEN="$TLON_AI_GITHUB_TOKEN" gh api \
  repos/tlon-team/tlon.el/issues/<NUMBER>/comments \
  -f body="Fixed in commit <SHA>. <brief description of what was done>" \
  --jq '.id')
GH_TOKEN="$TLON_AI_GITHUB_TOKEN" gh api \
  repos/tlon-team/tlon.el/issues/comments/"$COMMENT_ID" \
  --jq '.user.login'

# Close the issue as tlon-ai
GH_TOKEN="$TLON_AI_GITHUB_TOKEN" gh issue close <NUMBER> --repo tlon-team/tlon.el

# Verify the issue is closed.
GH_TOKEN="$TLON_AI_GITHUB_TOKEN" gh issue view <NUMBER> --repo tlon-team/tlon.el --json state --jq '.state'
```

**IMPORTANT**:
- Always use `GH_TOKEN=`, not `GITHUB_TOKEN=`. The `gh` CLI prioritizes its stored credentials over `GITHUB_TOKEN`, but `GH_TOKEN` takes precedence over everything.
- **Before posting any comments**, verify the token identity: `GH_TOKEN="$TLON_AI_GITHUB_TOKEN" gh api user --jq '.login'` must return `tlon-ai`.
- Do not create test comments just to verify write access. Use the first authorized closing comment as the write check, then verify its author before closing the issue.
- **NEVER silently fall back to the default `gh` auth (benthamite)**. If `TLON_AI_GITHUB_TOKEN` is not set or the token fails, **stop and tell the user** — do not post under the wrong account.
- The token must be a **classic PAT** (prefix `ghp_`) with `repo` scope. Fine-grained PATs have org approval issues that block writes.

### 6. Update the org file

Only archive an issue after the GitHub issue has been closed or the user explicitly asks to archive the local org heading without closing GitHub.

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

### 7. Verify and summarize

Verify each completed issue before the final summary:

- `git show --stat <SHA>` confirms the commit exists and is scoped to the issue.
- The relevant byte-compile, test, or check command passed, or the summary explains why it could not be run.
- If external closure was authorized, `gh issue view` shows the issue is closed and the closing comment is from `tlon-ai`.
- If org archiving was authorized, the heading was archived or the summary says the heading was not found.

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
