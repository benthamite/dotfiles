# Prepare an upstream Elisp fix and retain it locally

Use this workflow for a fix to an externally maintained Elisp package, even
when the user has not mentioned contributing upstream, or for keeping an
existing upstream PR active in an Elpaca profile. A diagnosis-only request
does not authorize edits; an explicit local-only request excludes contribution
preparation. User-owned packages follow their own development workflow.

## Prepare before requesting publication

Identify the actual upstream from package metadata, repository instructions
and existing remotes; do not assume `origin` is upstream. Resolve managed
checkouts through the parent skill. Preserve the named checkout for unmanaged
packages and do not introduce an Elpaca pin for them.

For an authorized fix, use an isolated local topic branch, starting from the
intended upstream base, and keep the patch focused enough to submit upstream.
Preserve unrelated work and avoid carrying unrelated commits into the proposed
PR. Use an existing checkout or a worktree under the established worktree root;
creating a new clone still requires explicit authorization. Complete the fix,
applicable package checks and requested behavior verification before presenting
the contribution for approval. If the original issue remains unverified or the
change addresses only a separate confirmed defect, say so in the PR draft.

Prepare a PR title/body and identify the existing recipe and temporary fork
branch needed to retain the change locally. When that requires configuration
changes, prepare a reviewable patch before asking to activate it. Do not wait
for the user to explain this workflow. If upstream contribution is unsuitable
(for example, a personal customization), state the reason and retain the
authorized local solution without preparing an inappropriate PR.

## Publication and local retention

Creating a fork, pushing and creating/updating a PR require explicit user
authorization for the intended repositories and operation. A PR review or
submission request alone does not authorize a local profile pin. Check existing
session authorization before asking again; otherwise request the missing
publication and activation approval together after the artifacts are ready.
This workflow itself grants none of those permissions.

For guarded GitHub fork creation or PR submission, follow
[scoped operation authorization](../../../../agents/github-operation-authorizations.md)
after explicit authorization. Prepare and commit only the grant for the exact
reviewed command, account and PR body, then remove it after the action completes.
The grant records authorization; it does not establish user consent by itself.
Do not edit or disable the guard, broaden its standing allowlist, or substitute
an unguarded API route. Other blocked operations remain blocked; report the
concrete operation when no supported grant applies.

Once publication and local activation are authorized, publish the reviewed
branch/PR, then apply the temporary pin below. If publication is declined or
blocked, preserve the local branch and draft, and report whether the local fix
is loaded and how it will survive package updates; do not label it fork-pinned.

## Activate and retire an Elpaca pin

1. Bind the package checkout, forge and exact requested PR identity. For GitHub,
   query the explicit PR URL, not the current branch's implicit PR:

   ```bash
   gh pr view "$PR_URL" --json url,state,mergedAt,baseRefName,headRepository,headRepositoryOwner,headRefName,headRefOid,mergeCommit
   ```

   Confirm the returned URL matches the requested repository and PR number.
   Record the head commit, not only a branch name that may move. Use
   `state == MERGED` or non-null `mergedAt` to recognize a merge. Derive
   the base owner/repository from the returned PR URL; `merged` and
   `baseRepository` are not supported CLI JSON fields. Query the base repo's
   default branch with `gh repo view "$BASE_REPO" --json nameWithOwner,defaultBranchRef`
   when deciding whether an explicit recipe branch is needed. For Codeberg or
   another forge, use its supported read-only interface and equivalent identity,
   state, head and base evidence; do not send that URL to GitHub's CLI. If the
   head repo was deleted or metadata is incomplete, do not guess its identity.

2. Inspect the working tree/index, unsaved config buffer and existing recipe,
   including any different temporary pin. If the PR is open, edit the existing
   Elpaca recipe in
   `~/My Drive/dotfiles/emacs/config.org` so it points to the PR head repository
   and branch. Preserve the invariant that one package has one install recipe:
   modify or move the existing `use-package` or `elpaca` owner instead of adding
   a duplicate dependency recipe. Preserve package ID, file/build directives and
   other unrelated recipe settings. Resolve conflicting `:ref`, `:tag`, `:pin`
   and remote recipe overrides before claiming a branch change took effect;
   the installed Git backend gives `:ref` precedence over `:branch`. Do not
   silently replace a different requested pin or discard checkout modifications.
3. Put the lifecycle marker on the temporary `:branch` line when possible.
   Use the actual forge's PR URL; this example is for GitHub:

   ```elisp
   ; awaiting PR merge: https://github.com/OWNER/REPO/pull/NUMBER
   ```

4. Recheck the PR head before activation; a moved head requires review of the new
   changes, not an assumption that the old review covers them. Complete the
   config-buffer/source/profile preflight and tangle with the profile-aware
   command in the parent `SKILL.md`. Tangling
   does not update the live Elpaca recipe or checkout. Apply the supported
   reconfiguration/update path and verify the live source before claiming the
   pin is active or writing a lockfile from the live queue. Verify the actual
   package source repository and intended reviewed revision, rebuild/reload
   completion and the requested behavior. A checkout path or recipe printout
   alone does not establish loaded-code identity. Lockfile regeneration is a
   separate requested operation, not an automatic side effect of this procedure.
5. Review and commit only the owned pin change in dotfiles, independently of the
   package checkout. Inspect both staged and unstaged hunks of `config.org`;
   `commit --only config.org` includes unrelated work in that same file. Use
   repository-approved hunk isolation, preserve the original index, and verify
   the proposed commit's exact diff. Do not commit another session's config.

If the PR is merged, remove the temporary fork pin only after verifying that
the chosen upstream/base revision contains the merged change. A merge into a
maintenance branch does not imply inclusion in the default branch. Preserve an
explicit `baseRefName` where the intended supported branch requires it; verify
containment or equivalent forge merge evidence, including squash/rebase history,
instead of demanding that the original head SHA be an ancestor. If the PR is
closed without merge, ask before changing `config.org` because the answer
determines whether the local profile should retain abandoned code.
