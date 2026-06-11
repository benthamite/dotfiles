---
name: elisp-conventions
description: Use when writing, editing, reviewing, or testing Emacs Lisp code, including dotfiles extras packages, Elpaca rebuild/reload behavior, batch-test verification, docstring and error-message conventions, and transient menu suffix checks.
---

# Emacs Lisp conventions

Use this skill before changing any `.el` file and keep it open through
verification. It supplies local coding conventions plus the rebuild/test path.
Use `lint-elisp` in addition when the user asks for compiler, checkdoc, or
lint diagnostics; use `document-elisp-package` when package documentation must be generated
or refreshed.

Do not use this as the primary workflow for non-Elisp changes, broad design
reviews, or documentation-only work unrelated to an Elisp edit.

# Coding style

- Write atomic, focused functions. When tempted to add a comment explaining code, refactor it into a function with a clear intention, so that the comment is no longer necessary. Functions should generally be only a few lines long.
- Never insert empty lines within a function.
- Put helper functions *after* the function that calls them, not before.
- Docstrings should document every argument using uppercase argument names.
- Fill all docstrings to 80 characters.
- The first line of the docstring should be a single-sentence summary.
- When writing multiline docstrings, do not leave a newline between the first and second lines. But do leave a newline between all successive paragraphs.
- Do not end error messages with a period.
- Only add comments if truly necessary to understand the code. Avoid commenting every detail.

# Two package layouts

Every `.el` file you edit belongs to one of two layouts. The verification workflow is the same; the underlying mechanics differ.

**Extras (dotfiles two-clone)**: file lives under `~/My Drive/dotfiles/emacs/extras/` (the canonical source). Elpaca clones the dotfiles repo into `elpaca/sources/dotfiles/` and builds from there. The two clones diverge between edit and commit. A git `post-commit` hook (`sync-elpaca-clone.sh`) syncs the elpaca clone after each commit and triggers `elpaca-rebuild` + `elpaca-extras-reload`.

**Standalone**: file lives directly under `elpaca/sources/<pkg>/` (canonical source IS the elpaca clone). No two-clone problem. The PostToolUse hook on save schedules `elpaca-rebuild` + reload in the running Emacs, then waits for the returned status token with short `emacsclient` polls. There is no post-commit sync because the package's git repo is not the dotfiles repo.

# Verification workflow

The verification sequence differs by layout because standalone packages can
usually reload edited source before commit, while dotfiles extras require the
commit-triggered sync/reload before live Emacs sees canonical code.

1. **Edit** the `.el` file.
2. **Let auto-rebuild fire when the edit path supports it.** `load-elisp-after-edit.sh` (PostToolUse) asks Emacs to enqueue `elpaca-rebuild` and reload the affected package through `elpaca-post-queue-hook` when the build finishes. The hook waits for the returned status token with bounded, short `emacsclient` polls; it does not call `elpaca-wait` inside Emacs. For extras it may compile against the (still stale) elpaca clone — useful sanity check but not a real verification. If a shell command changed the file and no PostToolUse rebuild fired, continue with the batch-test and commit workflow below rather than using an ad-hoc reload.
3. **Run `batch-test.sh PACKAGE`** to verify in a clean Emacs. The script auto-detects the layout: for extras it pushes `emacs/extras` to the front of `load-path` so the canonical source wins; for standalone it just `(require 'PKG)` from `elpaca/builds/`. This run also creates the test marker the commit hook requires. When you need to run ERT, use `elisp-ert PACKAGE TEST-FILE [TEST-NAME]`; never hand-build an `emacs --batch -L ...` ERT command.
4. **For standalone packages, live-verify before commit when auto-reload loaded the edited code.** Exercise the exact changed command/path in live Emacs via a targeted `emacsclient -e` check or the real UI action. If auto-reload did not run or cannot safely load the edit, report the blocked live verification and do not imply the live workflow was checked.
5. **For dotfiles extras, commit only after batch-test passes.** The post-commit hook then syncs the elpaca clone and rebuilds, so the running Emacs gets the canonical code.
6. **For dotfiles extras, immediately live-verify after the commit-triggered sync/reload.** Exercise the exact changed command/path in live Emacs before pushing, reporting success, or moving on.

```bash
~/My\ Drive/dotfiles/claude/bin/batch-test.sh PACKAGE
~/My\ Drive/dotfiles/claude/bin/batch-test.sh PACKAGE '(message "%S" (PACKAGE-some-fn))'
~/My\ Drive/dotfiles/claude/bin/elisp-ert PACKAGE test/PACKAGE-test.el
~/My\ Drive/dotfiles/claude/bin/elisp-ert PACKAGE test/PACKAGE-test.el TEST-NAME
```

**Never** use `load-file`, `eval-buffer`, `eval-defun`, or manual `byte-compile-file` to reload Elisp. The edit/commit hooks and `elpaca-extras-rebuild-and-reload` are the only sanctioned reload paths; for dotfiles extras with uncommitted changes, wait for the commit-triggered sync/reload before live Emacs verification.

A PreToolUse hook (`block-elpaca-rebuild-uncommitted.sh`) blocks manual `elpaca-rebuild` calls when there are uncommitted `.el` changes in dotfiles/extras — running rebuild against a stale clone silently loads old code.

See `emacs/extras/doc/elisp-development-workflow.org` for the dotfiles-specific design rationale.

# Pre-commit checks (hook-enforced)

- **Batch test**: `batch-test.sh PACKAGE` must run cleanly in this session before `git commit` is allowed (`require-elisp-test-before-commit.sh` enforces this). If the commit hook blocks and prints a specific `batch-test.sh PACKAGE` command, run that exact package name before retrying, even when it differs from the repo or package root you expected.
- **Stage documentation**: stage the matching Org manual update alongside
  `.el` files. Extras packages use `emacs/extras/doc/<package>.org`;
  standalone packages normally use the package root `README.org`, or
  `README.md` when the repo has no Org manual. Use `/document-elisp-package` to generate
  or update Org documentation.

# Stale-load detection

The PostToolUse hook (`track-elisp-test.sh`) inspects batch-test output for actual stale-load warnings (`newer than byte-compiled file`, `using older file`). If it sees one, the test marker is NOT created and the commit hook stays blocked. Don't try to bypass this — fix the load-path order instead.

# Emacs session safety

- Never run ERT test suites (`ert-run-tests-batch`, etc.) in the active Emacs session via emacsclient. Use a separate `emacs --batch` process. The active session is for loading code and interactive testing, not test suites.
- Never use `emacsclient -e` to print large Emacs objects (hash tables, EIEIO objects, etc.). Serializing them freezes Emacs. Always extract specific slots or counts instead (e.g. `(hash-table-count ht)` instead of `ht`).
- Never send potentially blocking or long-running expressions via `emacsclient -e`: no `font-lock-ensure`, no interactive commands, no unbounded loops, no operations whose runtime is unpredictable. A hung emacsclient blocks the Emacs server queue and makes Emacs unresponsive.
- Never send emacsclient expressions that iterate over buffer positions char-by-char. Use `next-single-property-change` for O(regions) jumps, or skip the diagnostic entirely and reason from the code.
- Never output large emoji data structures to the terminal. The eat terminal emulator freezes on bulk emoji rendering. Write emoji data to a file or check specific keys only.
- If `sed` or other shell commands change `.el` files and bypass the edit hook, do not call `load-file`; rely on `batch-test.sh`, the commit-triggered sync/rebuild, and a targeted live `emacsclient -e` verification after commit.
- Do not run multiple background agents that edit `.el` files simultaneously — the elpaca rebuild hook fires on each save, flooding Emacs with concurrent rebuilds. Serialize edits or batch into a single agent.

# Transient menus

After adding or modifying a `transient-define-prefix`, verify that **every suffix symbol** is an interactive command. Transient defers suffix validation to invocation time, so byte-compilation and `commandp` on the prefix itself will not catch non-interactive suffixes. Before committing, check every suffix via `emacsclient -e '(interactive-form (quote SYMBOL))'` — any that return `nil` need an `(interactive)` spec added to their `defun`.
