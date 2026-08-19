# Live Emacs verification

Read this reference before sending any verification expression to the active
Emacs session.

## Establish loaded-code state

Use the bounded rebuild helper when an Elpaca package must be loaded into the
active session:

```bash
~/My\ Drive/dotfiles/claude/bin/elpaca-rebuild-wait PACKAGE
```

The helper must return success for the requested package only after its rebuild
and reload status reaches `finished`. For a standalone package, run it after the
edit. For a dotfiles extra, run it after the verified commit has synchronized
the canonical source into the Elpaca mirror. Do not infer completion from an
edit event, a commit return value, or a status check for another package.

After completion, exercise the exact changed command or runtime path. For
buffer-local state, hooks, timers, teardown, or other session-lifecycle changes,
test fresh state and legacy or partial state, including idempotence.

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

- For a dotfiles extra, check every suffix through `batch-test.sh` against the
  canonical source before commit. After commit and a successful
  `elpaca-rebuild-wait`, repeat the small `interactive-form` checks in live
  Emacs.
- For a standalone package, run `elpaca-rebuild-wait` after the edit, then check
  every suffix in live Emacs before commit.

An `interactive-form` result of `nil` means the suffix function needs an
`interactive` specification.

## DWIM wrappers

For a wrapper around a built-in command, inspect the wrapped command's
interactive form and reproduce its boundary conditions before choosing among
`region-active-p`, `use-region-p`, and `mark-active`.
