---
name: new-emacs-package
description: Create a new standalone Emacs package of Pablo's from scratch, or turn existing code into one, through to a public, documented and registered package. Use for "create a new package", "make this a proper package", "turn this into a public package" or a new benthamite/<name> Emacs repository. Not for dotfiles extras (emacs/extras), renames (rename-package), releases of an existing package (release-package) or documentation alone (document-elisp-package).
argument-hint: "[package-name]"
---

# New Emacs package

This skill sequences the work. Each step's procedure belongs to the skill named
in it: load that skill at the step and follow it rather than improvising. Do not
copy their rules here.

## Decisions to collect first

Before writing files, put every decision the whole task needs in one list
(global instructions, "Ask for decisions once per task"): the name, public or
private repository, creating and pushing the GitHub repository, the `config.org`
recipe and tangle, the GitHub profile card (`add-to-emacs-packages` treats it as
separately authorized) and any live test that writes to an outside service.
Submitting to MELPA or another archive is not part of this workflow unless Pablo
asks.

## Steps

1. **Name.** Choose a name that says what the package does, then check it is
   free: no MELPA recipe (`https://raw.githubusercontent.com/melpa/melpa/master/recipes/NAME`
   returns 404) and no Emacs package of that name on GitHub.
2. **Source location.** Ask the running Emacs for `elpaca-sources-directory`
   (never guess the profile) and create the package at `SOURCES/NAME/` with
   `git init`. That directory becomes the canonical checkout once the recipe in
   step 4 exists
   (`dotfiles-context`). Keep scratch work out of the Drive tree.
3. **Layout.** Match Pablo's published packages (for example `annas-archive`,
   `elfeed-ai`, `mullvad`):
   - `NAME.el`, from the yasnippet `emacs/yasnippets/emacs-lisp-mode/package-header`,
     with `Package-Requires`, `Keywords` and `URL: https://github.com/benthamite/NAME`;
     split large packages into `NAME-*.el` libraries;
   - `COPYING.txt` (GPL-3.0), copied from a sibling package;
   - `test/NAME-test.el` and an executable `test/run-tests.sh`, so the commit
     guard has a tracked check (copy `forethought.el`'s or `elfeed-ai`'s and adapt);
   - `README.org` (step 5), `README.md` (step 5) and the exported `NAME.texi`.
4. **Publish and install the skeleton first.** The commit guard accepts Elisp
   only with test evidence from `batch-test.sh` or `elisp-ert`, and those
   resolve the package through Elpaca, which knows it only once it has a
   recipe, which needs the repository to exist. So, before committing any
   Elisp:
   1. commit `COPYING.txt` and `test/run-tests.sh` alone, on the branch `main`;
   2. with Pablo's approval, create the public repository and push that
      commit (`gh repo create benthamite/NAME --public --source . --remote origin --push`);
   3. add a `config.org` section in the matching part of the file: a heading
      with an `:ID:`, a one-line description linking the repository, and
      `(use-package NAME :ensure (:host github :repo "benthamite/NAME") :defer t)`
      (see the `org-linear` and `elfeed-ai` sections), plus its table-of-contents
      line. Follow `dotfiles-context` for editing and tangling, and commit it
      with `elisp-check-evidence file:emacs/config.org -- bin/check-config-org`
      (use `--staged` and `--snapshot` when the file holds others' edits);
   4. register it in the running Emacs without a restart:
      `(progn (elpaca (NAME :host github :repo "benthamite/NAME")) (elpaca-process-queues))`.
      Elpaca reuses the existing source directory; check that
      `bin/elpaca-package-resolve NAME` now answers.
5. **Code and tests.** Follow `elisp-conventions` throughout: never block the
   main thread on the network, test with the batch runners, commit with
   evidence. For code that writes to an outside service, give the tests a fake of
   that service; run live writes only against a sandbox account Pablo approved.
   Before calling the code done, have an independent reviewer audit it
   (`code-audit`, or a subagent given the code rather than your conclusions).
6. **Documentation.** `document-elisp-package` writes the `README.org` manual
   and its Texinfo export; then `generate-readme` derives `README.md`.
7. **Push and verify.** Push the code, and verify the package in the live
   session with `elisp-live-verify`. Turn on any behaviour that runs by itself
   (timers, modes) in `config.org` only after a live test passes.
8. **Register.** Run `add-to-emacs-packages`: the entry in
   `my-emacs-packages.org` (the website list), its documentation note, the
   repository's About fields and, if authorized, the card on the GitHub profile.
9. **Release.** Tag a first version only when Pablo asks, with `release-package`.

## Done when

The repository is public and pushed, the package loads in the live Emacs from
its Elpaca recipe, its tests pass, `README.org`, `README.md` and the Texinfo
manual agree with the code, and it appears on the website list (and, if
authorized, the profile). Report anything left undone as open, not done.
