---
name: dotfiles-context
description: Route changes to emacs/config.org, dotfiles extras, Elpaca-managed package checkouts, and paired Claude/Codex configuration. Use when a task must choose the canonical edit location, tangle the active Emacs profile, or run paired-config checks; not for read-only or unrelated dotfiles work.
---

# Dotfiles and package routing

Use this context to choose the correct source tree and verification path. If the
task changes Emacs Lisp, also use `elisp-conventions` for coding, batch-test,
rebuild, and live-verification rules.

## Canonical sources

- `~/My Drive/dotfiles/` is the canonical dotfiles source. It is the intentional
  exception to the rule that development repositories do not live under the
  Drive sync root.
- The active profile's `elpaca/sources/dotfiles/` checkout is a read-only mirror.
  Do not edit it. A hook blocks direct edits and committed dotfiles changes sync
  into it.
- For every other Elpaca-managed package, edit its active
  `elpaca/sources/<package>/` checkout. A legacy `elpaca/repos/<package>/`
  checkout is used only when `sources` is absent.

Resolve package paths with the deterministic helper instead of guessing a
profile or using `locate-library` or `symbol-file`:

```bash
~/My\ Drive/dotfiles/bin/elpaca-package-path PACKAGE [relative-path]
```

The helper returns the canonical dotfiles root for `dotfiles` and the active
profile checkout for standalone packages. Pushing is a separate sharing step;
it is not required to update the local dotfiles mirror.

## Workflow

1. Resolve the canonical source before editing.
2. For `.el` changes, follow `elisp-conventions` after choosing the layout.
3. For `emacs/config.org`, read
   [references/config-org.md](references/config-org.md), then tangle with the
   profile-aware command below.
4. For paired Claude/Codex configuration or skill changes, update both tracked
   counterparts unless their documented tool-specific frontmatter differs.

After editing `emacs/config.org`, always tangle with:

```bash
emacsclient -e '(init-build-profile (file-name-directory user-init-file))'
```

Do not use `org-babel-tangle-file` directly. It does not know the active profile
and can write to the wrong target.

When a task creates or updates an upstream PR and must keep its Elisp change
active locally, read
[references/upstream-pr-pins.md](references/upstream-pr-pins.md). Do not load
that procedure for ordinary package edits.

If a committed package needs an explicit rebuild and an observable completion
point, use the bounded helper and target the package, not the aggregate
`dotfiles` source:

```bash
~/My\ Drive/dotfiles/claude/bin/elpaca-rebuild-wait PACKAGE
```

## Verification

- Confirm the profile-aware tangle completed after a `config.org` change.
- Follow `elisp-conventions` for every Emacs Lisp change.
- Run `~/My\ Drive/dotfiles/bin/ai-config-sync audit` after paired
  Claude/Codex changes.
- If a tracked public skill name or catalog description changed, run
  `~/My\ Drive/dotfiles/bin/docs-audit generate` and then
  `~/My\ Drive/dotfiles/bin/docs-audit audit`.
- Inspect `git status --short` before completion so unrelated concurrent edits
  remain separate.
