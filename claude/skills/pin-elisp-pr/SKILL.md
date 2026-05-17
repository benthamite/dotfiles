---
name: pin-elisp-pr
description: "Pin an Emacs/Elpaca package in dotfiles to a fork, branch, or upstream PR while it awaits merge. Use when the user asks to use their fork, pin a package to a PR branch, temporarily install an Elisp package from a fork, or keep an upstream fix active in emacs/config.org. Enforces one package, one recipe: modify or move the existing package recipe instead of adding duplicate use-package or elpaca declarations."
user-invocable: true
---

# Pin Elisp PR

Pin a package by changing its existing Elpaca recipe to the PR head repo and
branch. The critical invariant is: **one package has one install recipe**.

Do not add a second declaration just because the package is also a dependency.
That is how Elpaca ends up warning that a package was "previously queued as
dependency of" another package.

## Workflow

1. Identify the package and PR/fork metadata.
   - Package symbol: `PACKAGE`
   - PR URL, if any
   - Head repository owner/name
   - Head branch
   - Base/upstream repository, for restoring after merge
2. Edit the canonical dotfiles source:
   - Use `~/My Drive/dotfiles/emacs/config.org`.
   - Do not edit the tangled `init.el` directly.
   - Do not edit the `elpaca/sources/dotfiles/` mirror.
3. Locate all existing declarations and recipes for the package before editing:

   ```bash
   rg -n "\(use-package PACKAGE\b|\(elpaca \(PACKAGE\b|:ensure \([^)]*PACKAGE\b" emacs/config.org
   ```

   Adjust `PACKAGE` to the actual symbol. Also search dependents that may queue
   the package as a dependency.
4. Apply the single-recipe rule below.
5. Tangle:

   ```bash
   emacsclient -e '(init-build-profile (file-name-directory user-init-file))'
   ```

6. Verify the source and tangled init before committing.
7. Commit the pin separately:

   ```bash
   git add emacs/config.org
   git commit -m "emacs: pin PACKAGE to pr branch"
   ```

## Single-Recipe Rule

For a package named `PACKAGE`, there must be exactly one install recipe in
`emacs/config.org`.

Allowed recipe owners:

- The package's canonical `(use-package PACKAGE ...)` form, via `:ensure (...)`.
- A single `(elpaca (PACKAGE ...))` order only when there is no canonical
  `use-package` form to own installation.

Not allowed:

- Adding an early duplicate `(use-package PACKAGE :ensure (...) :defer t)` while
  leaving the real package configuration later.
- Adding a raw `(elpaca (PACKAGE ...))` while a real `(use-package PACKAGE
  :ensure (...))` recipe still exists.
- Leaving a stale recipe on the upstream source and adding a second recipe for
  the fork.
- Creating a shared "dependency recipes" block for packages that are configured
  later.

If duplicate declarations or recipes already exist, consolidate them first.
Keep the real configuration on the canonical package form and keep only one
install recipe.

## How To Pin

If the package already has a canonical `use-package` form, put the fork recipe
there:

```elisp
(use-package PACKAGE
  :ensure (:host github
                 :repo "OWNER/REPO"
                 :branch "BRANCH") ; awaiting PR merge: https://github.com/UPSTREAM/REPO/pull/NUMBER
  ...)
```

If the package already has a custom `:ensure` recipe, replace the repository and
branch in that recipe. Do not add another recipe.

If the package has `:ensure nil`, determine why before changing it:

- If `:ensure nil` exists only because an earlier duplicate recipe was created,
  remove the earlier duplicate and put the recipe on the canonical
  `use-package` form.
- If the package truly has no installable package to fetch, stop and report that
  pinning does not apply.
- If moving the recipe to the canonical declaration would cause a dependency to
  queue the package first, prefer moving the canonical declaration earlier so it
  remains the single owner. If moving it would clearly change load behavior,
  stop and explain the tradeoff before introducing a separate `elpaca` owner.

If there is no `use-package PACKAGE` form at all, a single explicit Elpaca order
is acceptable:

```elisp
(elpaca (PACKAGE :host github
                 :repo "OWNER/REPO"
                 :branch "BRANCH")) ; awaiting PR merge: https://github.com/UPSTREAM/REPO/pull/NUMBER
```

After adding this, verify there is still no later `use-package PACKAGE` recipe
for the same package.

## Dependency Timing

When a dependent package queues `PACKAGE` before the pinned recipe is seen, do
not solve that by adding a second recipe.

Use this order of preference:

1. Move the existing canonical `use-package PACKAGE` declaration earlier, before
   the dependent is queued, while preserving its configuration.
2. Move the dependent package later, if that is simpler and behavior-preserving.
3. Use one explicit `(elpaca (PACKAGE ...))` owner only if `PACKAGE` has no
   canonical `use-package` declaration or moving declarations would change
   behavior. Leave any later config-only form with `:ensure nil` and document why
   there is still only one recipe.

## Verification

Before reporting success:

1. Confirm there is no duplicate owner:

   ```bash
   rg -n "\(use-package PACKAGE\b|\(elpaca \(PACKAGE\b|awaiting PR merge:.*PACKAGE|Recipe owner is declared near the package bootstrap|Some packages are dependencies of earlier packages" emacs/config.org
   ```

2. Confirm the active tangled init reflects the same single owner:

   ```bash
   rg -n "\(use-package PACKAGE\b|\(elpaca \(PACKAGE\b" ~/.config/emacs-profiles/$(emacsclient -e init-current-profile | tr -d '"')/init.el
   ```

3. Run an Emacs startup or batch check capable of surfacing Elpaca queue
   warnings. Search its output for:
   - `previously queued as dependency`
   - `Duplicate item ID queued`

If startup is blocked by an unrelated package failure, report that explicitly
and still state whether the duplicate-queue warning appeared.
