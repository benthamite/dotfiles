# Live Emacs verification

Read this reference before sending any verification expression to the active
Emacs session.

## Establish loaded-code state

After the verified commit, use the bound live-verification helper:

```bash
~/My\ Drive/dotfiles/claude/bin/elisp-live-verify PACKAGE -- '(PACKAGE-CHECK)'
```

Confirm live interaction and its effects are within the requested task. Resolve
the package and intended process/profile first; a source edit or successful
fixture is not permission to restart Emacs or change an unrelated live state.

The helper rejects dirty source, waits until the named package reaches a
`finished` rebuild-and-reload state, then runs the expression in the active
session. The expression must name and exercise the package and return a
non-nil result. Its evidence is bound to the repository, package, and commit.
Its rebuild helper validates the
actual Emacs process/start identity, profile, registered source checkout, and
package/token status before accepting completion or reusing a receipt. Old
unbound `finished` files do not prove a rebuild. Do not retry uncertain ownership
by starting competing jobs or delete a receipt to force success.
For a standalone-package label, the helper uses `elpaca-package-resolve` and
binds evidence to the resolved checkout, independently of the current directory.
The target source must remain committed and unchanged through the expression;
a concurrent HEAD change or new target Elisp invalidates the receipt. Unrelated
non-source edits are not a reason to discard or overwrite user work.
Do not infer completion from an edit event, a commit return value, another
package, or an unrelated `emacsclient` call. For buffer-local state, hooks,
timers, teardown, or other session-lifecycle changes,
test fresh state and legacy or partial state, including idempotence.

For a deleted package, run from its owning Git checkout and use the actual
registry/feature ID, which may differ from the checkout evidence label:

```bash
~/My\ Drive/dotfiles/claude/bin/elisp-live-verify deleted:PACKAGE -- '(not (featurep '\''PACKAGE))'
```

This mode requires the package source to be absent at `HEAD`. It removes only
the resolved package build below `elpaca-builds-directory`, removes that build
from `load-path`, unloads the feature, and then runs the non-nil absence check.
For a standalone package, the supported deleted path must be exactly `<id>.el`
or `lisp/<id>.el`, and neither path can remain at `HEAD`. A same-named file in a
vendor or other nested directory is an ordinary package change. The helper does
not delete the shared Elpaca source mirror. Use this mode only when the requested
change authorizes removal of that entire package and the helper resolves the
correct identity. A whole-package replacement can require old-package absence
and new-package live checks. A library or file rename inside a surviving package
must not unload or remove the entire package; check the relevant old/new features
through that package's normal workflow.

## Active-session safety

- Do not send signals to, kill, or restart an active Emacs session without the
  user's explicit confirmation. Missing connectivity is a verification gap,
  not permission to switch to an arbitrary process/profile.
- Run ERT suites in a separate batch Emacs, never through `emacsclient` in the
  active session.
- Do not print large hash tables, EIEIO objects, queue objects, or emoji data.
  Extract a specific slot, key, count, or small predicate result.
- Do not send `font-lock-ensure`, interactive commands, unbounded loops,
  character-by-character buffer scans, or any expression with unpredictable
  runtime through `emacsclient -e`.
- When acceptance requires an actual menu/buffer/interactive workflow, use
  `end-to-end` for that authorized bounded user-visible check rather than forcing
  it through an unsafe synchronous RPC. Preserve pre-existing buffers/state.
- Use `next-single-property-change` for bounded property-region traversal, or
  move a complex synthetic reproduction into batch/ERT.
- Do not run multiple agents that edit `.el` files at the same time. Concurrent
  edit-triggered rebuilds can flood the active session. Parallel read-only work
  is safe.

## Transient menus

After adding or changing a `transient-define-prefix`, inspect its actual suffix
specifications against code from the current change. Transient defers suffix
validation until invocation, so compiling the prefix is not sufficient.

- Check every suffix through `batch-test.sh` against canonical source before
  commit. After commit, repeat the small `interactive-form` checks through
  `elisp-live-verify`.

For a suffix that must be an ordinary command symbol, a nil `interactive-form`
is a defect; check its intended role before adding `interactive`. Transient also
supports structured suffix/infix specifications, so do not impose that predicate
on every element indiscriminately. Command metadata alone does not prove the
prefix opens or the suffix works: verify the affected menu path through a safe,
authorized interaction when claiming user-visible success. If that cannot be
exercised, state the specific unverified behavior.

## DWIM wrappers

For a wrapper around a built-in command, inspect the wrapped command's
interactive form and reproduce its boundary conditions before choosing among
`region-active-p`, `use-region-p`, and `mark-active`.
