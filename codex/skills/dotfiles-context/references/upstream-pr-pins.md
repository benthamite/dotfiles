# Keep an upstream Elisp PR active locally

Use this procedure only when the task already includes an upstream PR for a
non-`dotfiles` Elpaca package and requires keeping its change active locally.
A PR review or submission request alone does not authorize a local profile pin.
This procedure does not authorize creating/updating a PR, pushing, cloning a
new repository or switching a live profile by itself.

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
