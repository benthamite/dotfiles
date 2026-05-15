---
name: pin-elisp-pr
description: Update dotfiles emacs/config.org to temporarily pin an Elpaca-managed Emacs package to the fork branch for an upstream PR, marking it as awaiting PR merge.
---

# Pin Elisp PR

Use this after creating or updating an upstream PR for an Elpaca-managed Emacs
package when the local Emacs profile should use the PR branch until upstream
merges it.

Do not create or update a PR as part of this skill unless the user explicitly
authorized that external action. This skill owns only the dotfiles pin.

## Workflow

1. Identify the package and PR.
   - Prefer the current package repo name when the user does not provide a
     package name.
   - Use the provided PR URL, or query the current repo with:

     ```bash
     gh pr view --json url,state,merged,baseRepository,baseRefName,headRepository,headRefName
     ```

2. Verify the PR is the right pin target.
   - The PR should be open.
   - Record the base repo, head repo, head branch, and PR URL.
   - If the PR is merged, do not pin; use the upstream/base recipe instead.
   - If the PR is closed but unmerged, stop and ask the user before changing
     `config.org`.

3. Edit `~/My Drive/dotfiles/emacs/config.org`.
   - Locate the package's `use-package` form.
   - Update the Elpaca recipe to point at the PR head repo and head branch.
   - Add the exact lifecycle marker comment:

     ```elisp
     ; awaiting PR merge: https://github.com/OWNER/REPO/pull/NUMBER
     ```

   - Prefer attaching the marker to the temporary `:branch` line:

     ```elisp
     (use-package some-package
       :ensure (:host github
                :repo "benthamite/some-package"
                :branch "fix/something") ; awaiting PR merge: https://github.com/upstream/some-package/pull/123
       ...)
     ```

   - Preserve unrelated recipe fields and nearby comments.

4. Tangle with the profile-aware command:

   ```bash
   emacsclient -e '(init-build-profile (file-name-directory user-init-file))'
   ```

5. Review and commit only the intended dotfiles change.
   - Check that `git diff -- emacs/config.org` only changes the package
     recipe and marker.
   - Commit separately:

     ```bash
     git add emacs/config.org
     git commit -m "emacs: pin PACKAGE to pr branch"
     ```

Do not push dotfiles unless the user explicitly asks.
