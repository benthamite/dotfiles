# Elisp batch and ERT details

Read this reference when a change needs ERT, a RED-baseline check, or a
branch-local documentation deferral.

## Commands

For registered packages, use the provided runners instead of assembling ad hoc
load paths. Use the owning project's official targets as well when required;
unmanaged packages follow their own clean batch workflow:

```bash
~/My\ Drive/dotfiles/claude/bin/batch-test.sh PACKAGE
~/My\ Drive/dotfiles/claude/bin/batch-test.sh PACKAGE '(message "%S" (PACKAGE-some-fn))'
~/My\ Drive/dotfiles/claude/bin/elisp-check-evidence file:RELATIVE-PATH -- PROJECT-CHECK
~/My\ Drive/dotfiles/claude/bin/elisp-check-evidence --staged file:RELATIVE-PATH -- PROJECT-CHECK
~/My\ Drive/dotfiles/claude/bin/elisp-ert PACKAGE test/PACKAGE-test.el
~/My\ Drive/dotfiles/claude/bin/elisp-ert PACKAGE test/PACKAGE-test.el TEST-NAME
```

Wrap multiple top-level expressions for `batch-test.sh PACKAGE EXPR` in
`progn`; one `--eval` argument evaluates one form. Resolve `PACKAGE` first with
`bin/elpaca-package-resolve`. For standalone commit
evidence, pass its `.label`, since the tracker binds evidence to the literal
command label and the commit guard expects the checkout label. The runner uses
`.id` and `.source` to load code; the checkout name, registry ID, and main library
can differ. For focused ERT, the registered ID or resolvable checkout label is
accepted, not an arbitrary sibling library basename. For example, use
`elisp-ert agent test/agent-claude-test.el`, not
`elisp-ert agent-claude test/agent-claude-test.el`.

Both runners start `emacs -Q --batch`, put canonical source and its `lisp/`
directory before dependencies, prefer `.el` even after source timestamps were
rewound, and exclude the package's own build. They do not delete `.elc` files.
Missing canonical source fails instead of silently testing a build. The simple
batch loader requires one unambiguous `<registry-id>.el` at the source root or
`lisp/`; otherwise use the project's supported check and diagnose the unsupported
layout. ERT loads the explicit test file and returns its status; it does not
emit the batch wrapper's commit receipt.

For non-package Elisp, replace `RELATIVE-PATH` with the staged path relative to
the repository root and replace `PROJECT-CHECK` with the owning project's
actual batch, compile, or test command. The wrapper emits evidence only when
that command is a tracked executable in the repository and succeeds without a
stale-load warning. For `emacs/config.org`, use the exact
`file:emacs/config.org` label and a tracked check that tangles and validates the
affected output.

For a deleted production file, use its old repository-relative path in the
`file:RELATIVE-PATH` label and run a project check that proves the proposed tree
is valid without that file. The deleted-package helper supports a standalone
removal only when the old path is exactly `<id>.el` or `lisp/<id>.el` and neither
canonical main path remains. Establish the matching registry ID, file and
feature identity; do not substitute the checkout evidence label when it differs.
This is helper eligibility, not proof that the entire package was removed.
Deleting a same-named file below another directory is an ordinary package
change. For a file rename, establish any required file-labeled evidence for the
old path and normal evidence for the surviving package/new path. Do not infer whole
package deletion from a renamed sibling library or use destructive deleted-mode
live verification on a package that still exists.

Use `--staged` only when the same file also has unrelated unstaged edits that
must remain outside the commit. It materializes the Git index under a temporary
directory outside Drive from one frozen index tree, runs the tracked project
check there, and emits its index content identity only if the live index still
matches. It materializes raw blobs without running smudge filters or applying
checkout transformations; transformed bytes cannot stand in for indexed bytes.
The command must be a regular tracked executable; snapshot symlinks
must remain inside that snapshot. This is not a sandbox: review command effects
and ensure arguments/fixtures do not reach back into the original worktree. A
check that requires Git metadata may need a project-supported snapshot mode;
report that limitation instead of silently testing the working tree. Package
source does not use this exception: its staged and working
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

Establish RED in an owned isolated pre-fix snapshot outside Drive. Do not stash,
rewind, or replace the user's shared worktree just to reproduce a failure. The
failure must be the relevant assertion, not missing dependencies or setup.
When a project target loads compiled code, run its official compile-first target
against that exact snapshot; inspect the target rather than assuming a command
named `test` recompiles. Then test the fixed source with fresh compilation or
the canonical source-first runner. Preserve unrelated build artifacts.

If official standalone checks require Cask and Cask is unavailable, report the
official command as blocked. A targeted `emacs -Q --batch` run with source plus
active-profile dependencies is only narrow, partial evidence.

## Deliberate documentation deferral

A deliberate multi-commit feature-branch refactor may defer manual updates to
specified milestones. First inspect the current branch, its role, and any
existing local setting; preserve that prior value. Use the actual branch name
as one quoted argument (the following is a template, not a literal branch):

```bash
git config --local 'branch.ACTUAL-BRANCH.deferDocUpdates' true
```

Restore the prior setting, or unset only the value this task created, when the
milestone is complete and before merge. The guard ignores this setting on
`main` and `master`; a differently named default/release branch is not thereby
an appropriate place to defer. Detached HEAD has no branch-local deferral.
This is not a routine bypass for ordinary commits or a substitute for resolving
a genuine documentation-policy conflict.
