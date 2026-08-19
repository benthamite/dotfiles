# Elisp batch and ERT details

Read this reference when a change needs ERT, a RED-baseline check, or a
branch-local documentation deferral.

## Commands

Use the provided runners instead of assembling ad hoc `emacs --batch -L ...`
commands:

```bash
~/My\ Drive/dotfiles/claude/bin/batch-test.sh PACKAGE
~/My\ Drive/dotfiles/claude/bin/batch-test.sh PACKAGE '(message "%S" (PACKAGE-some-fn))'
~/My\ Drive/dotfiles/claude/bin/elisp-ert PACKAGE test/PACKAGE-test.el
~/My\ Drive/dotfiles/claude/bin/elisp-ert PACKAGE test/PACKAGE-test.el TEST-NAME
```

Wrap multiple top-level expressions for `batch-test.sh PACKAGE EXPR` in
`progn`; one `--eval` argument evaluates one form. For a standalone package with
multiple libraries, `PACKAGE` is the Elpaca source-directory name, not the test
file or library basename. For example, use
`elisp-ert agent test/agent-claude-test.el`, not
`elisp-ert agent-claude test/agent-claude-test.el`.

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
