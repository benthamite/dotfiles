---
name: elisp-conventions
description: Emacs Lisp coding conventions and elpaca rebuild patterns. Use when writing, reviewing, or testing Elisp code.
user-invocable: false
---

- Write atomic, focused functions. When tempted to add a comment explaining code, refactor it into a function with a clear intention, so that the comment is no longer necessary. Functions should generally be only a few lines long.
- Never insert empty lines within a function.
- Put helper functions *after* the function that calls them, not before.
- Docstrings should document all arguments, capitalized.
- Fill all docstrings to 80 characters.
- The first line of the docstring should be a single-sentence summary.
- When writing multiline docstrings, do not leave a newline between the first and second lines. But do leave a newline between all successive paragraphs.
- Do not end error messages with a period.
- Only add comments if truly necessary to understand the code. Avoid commenting every detail.

# Two package layouts

Every `.el` file you edit belongs to one of two layouts. The verification workflow is the same; the underlying mechanics differ.

**Extras (dotfiles two-clone)**: file lives under `~/My Drive/dotfiles/emacs/extras/` (the canonical source). Elpaca clones the dotfiles repo into `elpaca/sources/dotfiles/` and builds from there. The two clones diverge between edit and commit. A git `post-commit` hook (`sync-elpaca-clone.sh`) syncs the elpaca clone after each commit and triggers `elpaca-rebuild` + `elpaca-extras-reload`.

**Standalone**: file lives directly under `elpaca/sources/<pkg>/` (canonical source IS the elpaca clone). No two-clone problem. The PostToolUse hook on save triggers `elpaca-rebuild` + reload, and the running Emacs immediately has the new code. There is no post-commit sync because the package's git repo is not the dotfiles repo.

# Unified workflow

Same five steps for both layouts:

1. **Edit** the `.el` file.
2. **Auto-rebuild fires.** `load-elisp-after-edit.sh` (PostToolUse) asks Emacs to `elpaca-rebuild` + `elpaca-extras-reload` the affected package. For standalone packages this is authoritative — the running Emacs now has the new code. For extras it compiles against the (still stale) elpaca clone — useful sanity check but not a real verification.
3. **Run `batch-test.sh PACKAGE`** to verify in a clean Emacs. The script auto-detects the layout: for extras it pushes `emacs/extras` to the front of `load-path` so the canonical source wins; for standalone it just `(require 'PKG)` from `elpaca/builds/`. This run also creates the test marker the commit hook requires.
4. **Commit.** For extras, the post-commit hook then syncs the elpaca clone and rebuilds — so the running Emacs gets the canonical code. For standalone there is no post-commit step (already in sync).
5. **Verify in live Emacs** via `emacsclient -e` exercising the changed code path.

```bash
~/My\ Drive/dotfiles/claude/bin/batch-test.sh PACKAGE
~/My\ Drive/dotfiles/claude/bin/batch-test.sh PACKAGE '(message "%S" (PACKAGE-some-fn))'
```

**Never** use `load-file`, `eval-buffer`, `eval-defun`, or manual `byte-compile-file` to reload Elisp. The auto-rebuild hook + `elpaca-extras-rebuild-and-reload` are the only sanctioned reload paths.

A PreToolUse hook (`block-elpaca-rebuild-uncommitted.sh`) blocks manual `elpaca-rebuild` calls when there are uncommitted `.el` changes in dotfiles/extras — running rebuild against a stale clone silently loads old code.

See `emacs/extras/doc/elisp-development-workflow.org` for the dotfiles-specific design rationale.

# Pre-commit checks (hook-enforced)

- **Batch test**: `batch-test.sh PACKAGE` must run cleanly in this session before `git commit` is allowed (`require-elisp-test-before-commit.sh` enforces this).
- **Stage documentation**: stage the corresponding `emacs/extras/doc/<package>.org` manual update alongside `.el` files. Use `/doc-elisp` to generate or update documentation.

# Stale-load detection

The PostToolUse hook (`track-elisp-test.sh`) inspects batch-test output for actual stale-load warnings (`newer than byte-compiled file`, `using older file`). If it sees one, the test marker is NOT created and the commit hook stays blocked. Don't try to bypass this — fix the load-path order instead.

# Emacs session safety

- Never run ERT test suites (`ert-run-tests-batch`, etc.) in the active Emacs session via emacsclient. Use a separate `emacs --batch` process. The active session is for loading code and interactive testing, not test suites.
- Never use `emacsclient -e` to print large Emacs objects (hash tables, EIEIO objects, etc.). Serializing them freezes Emacs. Always extract specific slots or counts instead (e.g. `(hash-table-count ht)` instead of `ht`).
- Never send potentially blocking or long-running expressions via `emacsclient -e`: no `font-lock-ensure`, no interactive commands, no unbounded loops, no operations whose runtime is unpredictable. A hung emacsclient blocks the Emacs server queue and makes Emacs unresponsive.
- Never send emacsclient expressions that iterate over buffer positions char-by-char. Use `next-single-property-change` for O(regions) jumps, or skip the diagnostic entirely and reason from the code.
- Never output large emoji data structures to the terminal. The eat terminal emulator freezes on bulk emoji rendering. Write emoji data to a file or check specific keys only.
- If `sed` or other Bash commands are used to edit `.el` files (bypassing the Edit tool), manually call `emacsclient --eval '(load-file "...")'` afterward — the PostToolUse hook only fires on Edit/Write.
- Do not run multiple background agents that edit `.el` files simultaneously — the elpaca rebuild hook fires on each save, flooding Emacs with concurrent rebuilds. Serialize edits or batch into a single agent.

# Transient menus

After adding or modifying a `transient-define-prefix`, verify that **every suffix symbol** is an interactive command. Transient defers suffix validation to invocation time, so byte-compilation and `commandp` on the prefix itself will not catch non-interactive suffixes. Before committing, check every suffix via `emacsclient -e '(interactive-form (quote SYMBOL))'` — any that return `nil` need an `(interactive)` spec added to their `defun`.
