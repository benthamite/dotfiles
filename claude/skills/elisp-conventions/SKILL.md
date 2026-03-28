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

A PostToolUse hook (`load-elisp-after-edit.sh`) automatically rebuilds and reloads the elpaca package whenever you edit a `.el` file under `elpaca/sources/`. It runs:

```elisp
(elpaca-rebuild PKG t)
(elpaca-wait)
(elpaca-extras-reload PKG)
```

This is synchronous — it byte-compiles the source into `elpaca/builds/`, waits for completion, then reloads all features via `load`. The running Emacs always has the latest byte-compiled code, matching what the user gets after a restart.

**You must never use `load-file`, `eval-buffer`, `eval-defun`, or manual byte-compile commands to reload Elisp.** The hook handles it. If you find yourself reaching for these, something is wrong.

After the hook runs, **verify the change works** via a small targeted `emacsclient -e` expression before asking the user to test. Simulate the actual user action and check edge cases (e.g., cursor on different buffer positions).

# Batch testing before commit (CRITICAL)

A PreToolUse hook blocks `git commit` when `.el` files are staged until an `emacs --batch` test has run. The correct pattern:

```bash
ELPACA=$HOME/.config/emacs-profiles/$(emacsclient -e 'init-current-profile' | tr -d '"')/elpaca
rm -f "$PWD/emacs/extras/YOUR-PACKAGE.elc"
emacs --batch \
  --eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))" \
  --eval "(push \"$PWD/emacs/extras\" load-path)" \
  --eval "(require 'YOUR-PACKAGE)" \
  --eval "(message \"Result: %S\" (YOUR-TEST-EXPRESSION))" \
  2>&1
```

**The `push` MUST come AFTER the `dolist`.** This puts `emacs/extras` at the front of `load-path` so your edited `.el` source loads instead of the stale `.elc` in `elpaca/builds/`. Getting this order wrong silently loads old code — the test passes but verifies nothing.

**Never use `load-prefer-newer`** — it causes version mismatches between elpaca dependencies.

**Never use `append`** to add `emacs/extras` — it puts it at the end where elpaca builds take precedence.
