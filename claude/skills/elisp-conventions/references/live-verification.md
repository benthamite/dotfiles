# Live Emacs verification

Read this reference before sending any verification expression to the active
Emacs session.

## Establish loaded-code state

After the verified commit, use the bound live-verification helper:

```bash
~/My\ Drive/dotfiles/claude/bin/elisp-live-verify PACKAGE -- '(PACKAGE-CHECK)'
```

The helper rejects dirty source, waits until the named package reaches a
`finished` rebuild-and-reload state, then runs the expression in the active
session. The expression must name and exercise the package and return a
non-nil result. Its evidence is
bound to the repository, package, and commit. Do not infer completion from an
edit event, a commit return value, another package, or an unrelated
`emacsclient` call. For
buffer-local state, hooks, timers, teardown, or other session-lifecycle changes,
test fresh state and legacy or partial state, including idempotence.

For a deleted package, use:

```bash
~/My\ Drive/dotfiles/claude/bin/elisp-live-verify deleted:PACKAGE -- '(not (featurep '\''PACKAGE))'
```

This mode requires the package source to be absent at `HEAD`. It removes only
the resolved package build below `elpaca-builds-directory`, removes that build
from `load-path`, unloads the feature, and then runs the non-nil absence check.
For a standalone package, the deleted path must be exactly `PACKAGE.el` or
`lisp/PACKAGE.el`, and neither path can remain at `HEAD`. A same-named file in a
vendor or other nested directory is an ordinary package change. The helper does
not delete the shared Elpaca source mirror. A rename requires this check for the
old package and normal live verification for the new package.

## Active-session safety

- Run ERT suites in a separate batch Emacs, never through `emacsclient` in the
  active session.
- Do not print large hash tables, EIEIO objects, queue objects, or emoji data.
  Extract a specific slot, key, count, or small predicate result.
- Do not send `font-lock-ensure`, interactive commands, unbounded loops,
  character-by-character buffer scans, or any expression with unpredictable
  runtime through `emacsclient -e`.
- Use `next-single-property-change` for bounded property-region traversal, or
  move a complex synthetic reproduction into batch/ERT.
- Do not run multiple agents that edit `.el` files at the same time. Concurrent
  edit-triggered rebuilds can flood the active session. Parallel read-only work
  is safe.

## Transient menus

After adding or changing a `transient-define-prefix`, verify every suffix symbol
against code from the current change. Transient defers suffix validation until
invocation, so compiling the prefix is not sufficient.

- Check every suffix through `batch-test.sh` against canonical source before
  commit. After commit, repeat the small `interactive-form` checks through
  `elisp-live-verify`.

An `interactive-form` result of `nil` means the suffix function needs an
`interactive` specification.

## DWIM wrappers

For a wrapper around a built-in command, inspect the wrapped command's
interactive form and reproduce its boundary conditions before choosing among
`region-active-p`, `use-region-p`, and `mark-active`.
