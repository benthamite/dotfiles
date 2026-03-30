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

# Rebuilding elpaca packages after changes

A PostToolUse hook (`load-elisp-after-edit.sh`) automatically rebuilds and reloads the elpaca package whenever you edit a `.el` file under `elpaca/sources/` or `dotfiles/emacs/extras/`. It runs:

```elisp
(elpaca-rebuild PKG t)
(elpaca-wait)
(elpaca-extras-reload PKG)
```

This is synchronous — it byte-compiles the source into `elpaca/builds/`, waits for completion, then reloads all features via `load`. The running Emacs always has the latest byte-compiled code, matching what the user gets after a restart.

**You must never use `load-file`, `eval-buffer`, `eval-defun`, or manual byte-compile commands to reload Elisp.** The hook handles it. If you find yourself reaching for these, something is wrong.

After the hook runs, **verify the change works** via a small targeted `emacsclient -e` expression before asking the user to test. Simulate the actual user action and check edge cases (e.g., cursor on different buffer positions).

# Batch testing before commit (CRITICAL)

A PreToolUse hook blocks `git commit` when `.el` files are staged until an `emacs --batch` test has run. Use the helper script:

```bash
~/My\ Drive/dotfiles/claude/bin/batch-test.sh YOUR-PACKAGE
~/My\ Drive/dotfiles/claude/bin/batch-test.sh YOUR-PACKAGE '(message "Result: %S" (YOUR-TEST-EXPRESSION))'
```

The script handles profile resolution, load-path setup, and stale `.elc` cleanup automatically.

If you need to understand what the script does (e.g., for debugging), the key details:
- All `elpaca/builds/*/` directories are added to `load-path` via `file-expand-wildcards`.
- `emacs/extras` is pushed to the **front** so edited `.el` sources take precedence over stale `.elc` in elpaca builds.
- Never use `load-prefer-newer` (causes version mismatches) or `append` (puts extras at the end where elpaca builds take precedence).
