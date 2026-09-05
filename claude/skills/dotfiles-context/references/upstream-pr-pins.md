# Keep an upstream Elisp PR active locally

Use this procedure only when the task already includes creating or updating an
upstream PR for a non-`dotfiles` Elpaca package. It does not authorize creating
or updating a PR by itself.

1. Record the PR metadata from the package checkout:

   ```bash
   gh pr view --json url,state,mergedAt,baseRefName,headRepository,headRepositoryOwner,headRefName
   ```

   Use `state == MERGED` or non-null `mergedAt` to recognize a merge. Derive
   the base owner/repository from the returned PR URL; `merged` and
   `baseRepository` are not supported CLI JSON fields. Query the base repo's
   default branch with `gh repo view "$BASE_REPO" --json nameWithOwner,defaultBranchRef`
   when deciding whether an explicit recipe branch is needed. If the head repo
   was deleted, do not guess its identity from a missing value.

2. If the PR is open, edit the package's existing Elpaca recipe in
   `~/My Drive/dotfiles/emacs/config.org` so it points to the PR head repository
   and branch. Preserve the invariant that one package has one install recipe:
   modify or move the existing `use-package` or `elpaca` owner instead of adding
   a duplicate dependency recipe.
3. Put the lifecycle marker on the temporary `:branch` line when possible:

   ```elisp
   ; awaiting PR merge: https://github.com/OWNER/REPO/pull/NUMBER
   ```

4. Tangle with the profile-aware command in the parent `SKILL.md`. Tangling
   does not update the live Elpaca recipe or checkout. Apply the supported
   reconfiguration/update path and verify the live source before claiming the
   pin is active or writing a lockfile from the live queue.
5. Review and commit the dotfiles pin in the dotfiles repository, independent of
   the package checkout:

   ```bash
   git -C "$HOME/My Drive/dotfiles" diff -- emacs/config.org
   git -C "$HOME/My Drive/dotfiles" add -- emacs/config.org
   git -C "$HOME/My Drive/dotfiles" commit --only -m "emacs: pin PACKAGE to pr branch" -- emacs/config.org
   ```

If the PR is merged, use the upstream/base recipe and do not pin it. If the PR
is closed without merge, stop and ask before changing `config.org` because the
answer determines whether the local profile should retain abandoned code.
