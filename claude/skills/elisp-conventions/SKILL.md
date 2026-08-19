---
name: elisp-conventions
description: Apply local coding and verification rules when modifying or testing Emacs Lisp, or when explicitly reviewing code against these conventions. Use for dotfiles extras, standalone Elpaca packages, and other .el files; not for non-Elisp work or read-only bug, security, or design reviews where these conventions are not material.
user-invocable: false
---

# Emacs Lisp conventions

Use this skill before changing a `.el` file and keep it active through
verification. For an explicit read-only conventions review, apply only the
relevant style rules; do not turn the review into an edit or live-Emacs task.

Use `lint-elisp` in addition when the user asks for compiler, checkdoc, or lint
diagnostics. Use `document-elisp-package` when an Org manual must be created or
refreshed.

## Coding style

- Write atomic, focused functions. Prefer a small function with a clear name to
  a comment that explains a long implementation.
- Do not insert empty lines within a function.
- Put helper functions after the function that calls them.
- Document every argument in docstrings with its uppercase name.
- Fill docstrings to 80 characters. Start with a one-sentence summary.
- In a multiline docstring, continue the first paragraph on the next line
  without an empty line. Separate later paragraphs with one empty line.
- Do not end error messages with a period.
- Add comments only when the code cannot make the reason clear.

## Choose the layout

Every edited `.el` file uses one of these paths:

1. **Dotfiles extra:** production source under
   `~/My Drive/dotfiles/emacs/extras/`. This is canonical source; the active
   Elpaca dotfiles checkout is a committed mirror. Use `dotfiles-context` for
   source routing.
2. **Standalone Elpaca package:** source under the active profile's
   `elpaca/sources/<package>/`. The source checkout is canonical.
3. **Non-package Elisp:** files such as `.dir-locals.el`, `lockfile.el`, and
   files outside the two package roots. Use the owning project's checks. Do not
   force an Elpaca package rebuild or package-manual workflow onto these files.

Test-only files inside a package follow the package's batch and focused ERT
paths, but they do not need a rebuild, live package reload, or manual update
unless they also change production code or user documentation.

## Verification workflow

1. Edit the canonical source.
2. Establish clean batch evidence for the changed code:
   - For a dotfiles extra, run the absolute `batch-test.sh PACKAGE` command
     below. It must load the canonical extras source.
   - For standalone production source, run `batch-test.sh PACKAGE` against the
     canonical source. The live rebuild is a later, separate check.
   - For non-package Elisp, run the owning project's clean batch or compile
     check through `elisp-check-evidence file:RELATIVE-PATH -- PROJECT-CHECK`.
     `PROJECT-CHECK` must be a tracked executable in that repository. Do not
     substitute an unrelated package check.
   - A deleted production file cannot be loaded. Use the same `file:RELATIVE-PATH`
     label with a project check that proves the repository is valid without it.
3. Run focused ERT when behavior needs it. Read
   [references/testing.md](references/testing.md) for command forms and
   stale-compiled-code safeguards.
4. Before any live Emacs check, read
   [references/live-verification.md](references/live-verification.md).
   - After the verified commit, use `elisp-live-verify PACKAGE -- EXPR` for a
     standalone package or dotfiles extra. It waits for the package rebuild,
     exercises the named live path, and emits package/commit-bound evidence.
   - For a deleted package, use `elisp-live-verify deleted:PACKAGE -- EXPR`.
     The helper removes only that package's safe Elpaca build, unloads the
     feature, and requires a non-nil expression that verifies its absence.
   - Non-package Elisp uses its owning workflow; do not load it into the active
     session unless that workflow makes the operation safe.

```bash
~/My\ Drive/dotfiles/claude/bin/elpaca-rebuild-wait PACKAGE
~/My\ Drive/dotfiles/claude/bin/elisp-live-verify PACKAGE -- '(PACKAGE-CHECK)'
~/My\ Drive/dotfiles/claude/bin/batch-test.sh PACKAGE
~/My\ Drive/dotfiles/claude/bin/elisp-check-evidence file:RELATIVE-PATH -- PROJECT-CHECK
~/My\ Drive/dotfiles/claude/bin/elisp-ert PACKAGE TEST-FILE [TEST-NAME]
```

Test evidence counts only when it identifies the repository, package, and
source revision or equivalent content identity that was tested. A generic
session marker or a clean test for another package is not evidence for the
current change. Live evidence must also identify the committed repository and
package. Treat any stale-load warning as a failed verification.

Never use `load-file`, `eval-buffer`, `eval-defun`, or manual
`byte-compile-file` to reload edited package code. Use the bounded rebuild helper
and the layout-specific sequence above.

## Documentation and commit gates

For behavior-changing production `.el` files, stage the matching package manual
when one exists:

- Dotfiles extras use `emacs/extras/doc/<package>.org`.
- Standalone packages normally use root `README.org`, or `README.md` only when
  there is no Org manual.

Manual updates are not required only because a commit changes a test file or a
generated/machine-owned file such as `lockfile.el`, `*-autoloads.el`, or
`*-pkg.el`. Read [references/testing.md](references/testing.md) before using the
branch-local documentation deferral for a deliberate multi-commit refactor.
