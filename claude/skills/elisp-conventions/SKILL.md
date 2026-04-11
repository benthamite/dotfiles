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

# Two-phase reload lifecycle

Elisp changes propagate to the running Emacs in two phases. Understanding both is critical to avoid testing stale code.

## Phase 1: PostToolUse hook (pre-commit, potentially stale)

A PostToolUse hook (`load-elisp-after-edit.sh`) fires on edits to `.el` files under `elpaca/sources/` or `dotfiles/emacs/extras/`. It triggers `elpaca-rebuild` + `elpaca-extras-reload`. However, **`elpaca-rebuild` compiles from the elpaca source clone** (`elpaca/sources/dotfiles/`), which is a separate git checkout from the canonical `~/My Drive/dotfiles/`. Pre-commit, the elpaca clone has the old code, so this rebuild may load stale definitions. This is expected and harmless.

## Phase 2: git post-commit hook (authoritative)

A git `post-commit` hook (`sync-elpaca-clone.sh`) runs after every commit. It fetches from the local gitdir into the elpaca source clone, resets it to FETCH_HEAD, then runs `elpaca-rebuild` + `elpaca-extras-reload` for each changed package. **Only after committing does the running Emacs have the latest code.**

## Correct workflow

1. Edit canonical files in `~/My Drive/dotfiles/emacs/extras/`
2. Run `batch-test.sh` to verify compilation (this uses the canonical source, not the elpaca clone)
3. Commit — the post-commit hook syncs the elpaca clone and reloads
4. **Then** verify via `emacsclient -e` expressions

**You must never use `load-file`, `eval-buffer`, `eval-defun`, or manual byte-compile commands to reload Elisp.** If you find yourself reaching for these, the correct action is to commit so the post-commit hook propagates the changes.

A PreToolUse hook (`block-elpaca-rebuild-uncommitted.sh`) blocks manual `elpaca-rebuild` calls when there are uncommitted `.el` changes, to prevent accidentally testing stale code.

See `emacs/extras/doc/elisp-development-workflow.org` for the full design rationale.

# Pre-commit checks (hook-enforced)

Do these proactively to avoid being blocked by pre-commit hooks:

- **Batch test**: run `emacs --batch` before committing (details below).
- **Stage documentation**: stage the corresponding `emacs/extras/doc/<package>.org` manual update alongside `.el` files. Use `/doc-elisp` to generate or update documentation.

# Batch testing before commit (CRITICAL)

A PreToolUse hook blocks `git commit` when `.el` files are staged until an `emacs --batch` test has run. Use the helper script:

```bash
~/My\ Drive/dotfiles/claude/bin/batch-test.sh YOUR-PACKAGE
~/My\ Drive/dotfiles/claude/bin/batch-test.sh YOUR-PACKAGE '(message "Result: %S" (YOUR-TEST-EXPRESSION))'
```

The script handles profile resolution, load-path setup, and stale `.elc` cleanup automatically.

# Transient menus

After adding or modifying a `transient-define-prefix`, verify that **every suffix symbol** is an interactive command. Transient defers suffix validation to invocation time, so byte-compilation and `commandp` on the prefix itself will not catch non-interactive suffixes. Before committing, check every suffix via `emacsclient -e '(interactive-form (quote SYMBOL))'` — any that return `nil` need an `(interactive)` spec added to their `defun`.

# Batch testing details

If you need to understand what the script does (e.g., for debugging), the key details:
- All `elpaca/builds/*/` directories are added to `load-path` via `file-expand-wildcards`.
- `emacs/extras` is pushed to the **front** so edited `.el` sources take precedence over stale `.elc` in elpaca builds.
- Never use `load-prefer-newer` (causes version mismatches) or `append` (puts extras at the end where elpaca builds take precedence).
