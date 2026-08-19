# Elisp batch and ERT details

Read this reference when a change needs ERT, a RED-baseline check, or a
branch-local documentation deferral.

## Commands

Use the provided runners instead of assembling ad hoc `emacs --batch -L ...`
commands:

```bash
~/My\ Drive/dotfiles/claude/bin/batch-test.sh PACKAGE
~/My\ Drive/dotfiles/claude/bin/batch-test.sh PACKAGE '(message "%S" (PACKAGE-some-fn))'
~/My\ Drive/dotfiles/claude/bin/elisp-check-evidence file:RELATIVE-PATH -- PROJECT-CHECK
~/My\ Drive/dotfiles/claude/bin/elisp-check-evidence --staged file:RELATIVE-PATH -- PROJECT-CHECK
~/My\ Drive/dotfiles/claude/bin/elisp-ert PACKAGE test/PACKAGE-test.el
~/My\ Drive/dotfiles/claude/bin/elisp-ert PACKAGE test/PACKAGE-test.el TEST-NAME
```

Wrap multiple top-level expressions for `batch-test.sh PACKAGE EXPR` in
`progn`; one `--eval` argument evaluates one form. For a standalone package with
multiple libraries, `PACKAGE` is the Elpaca source-directory name, not the test
file or library basename. For example, use
`elisp-ert agent test/agent-claude-test.el`, not
`elisp-ert agent-claude test/agent-claude-test.el`.

For non-package Elisp, replace `RELATIVE-PATH` with the staged path relative to
the repository root and replace `PROJECT-CHECK` with the owning project's
actual batch, compile, or test command. The wrapper emits evidence only when
that command is a tracked executable in the repository and succeeds without a
stale-load warning. For `emacs/config.org`, use the exact
`file:emacs/config.org` label and a tracked check that tangles and validates the
affected output.

For a deleted production file, use its old repository-relative path in the
`file:RELATIVE-PATH` label and run a project check that proves the proposed tree
is valid without that file. A standalone package is fully deleted only when the
old path is exactly `PACKAGE.el` or `lisp/PACKAGE.el` and neither canonical main
path remains. Deleting a same-named file below another directory is an ordinary
package change. For a rename, establish file-labeled evidence for the old path
and normal package evidence for the new path.

Use `--staged` only when the same file also has unrelated unstaged edits that
must remain outside the commit. It materializes the Git index under a temporary
directory, runs the tracked project check there, and emits the index content
identity. Package source does not use this exception: its staged and working
bytes must match before `batch-test.sh` evidence can authorize a commit.

Do not accept the result unless its evidence is bound to the repository,
package, and source revision or equivalent content identity under review. A
stale-load warning invalidates the result even when the command otherwise exits
successfully.

## Test isolation

- When an ERT test asserts copied or killed text, or asserts that the kill ring
  did not change around kill/copy commands, bind `kill-ring` and
  `kill-ring-yank-pointer` locally. Bind `last-command` only when prior-kill
  append behavior can affect the result or the test covers append semantics.
- Do not stub `cl-defstruct` accessors or other inlinable functions with
  `cl-letf` in byte-compiled code. Compiled call sites can bypass the stub. Use
  real objects or a non-inlined interface, and force RED against unfixed source.

## Stale compiled state

After `stash`, `checkout`, or another operation that rewinds source for a RED
baseline, do not trust a target that skips compilation. Recompile through the
official compile-first target before testing. For `emacs-slack`, run full
`make test`; `make test-upstream` alone skips compilation.

If official standalone checks require Cask and Cask is unavailable, report the
official command as blocked. A targeted `emacs -Q --batch` run with source plus
active-profile dependencies is only narrow, partial evidence.

## Deliberate documentation deferral

A multi-commit feature-branch refactor can defer manual updates to milestone
boundaries with:

```bash
git config branch.<branch>.deferDocUpdates true
```

Remove the setting before merge:

```bash
git config --unset branch.<branch>.deferDocUpdates
```

This setting is ignored on `main` and `master`. It is not a routine bypass for
ordinary commits.
