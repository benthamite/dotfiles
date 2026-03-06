---
name: elisp-conventions
description: Emacs Lisp coding conventions, elpaca profile resolution, and byte-compilation/testing patterns. Use when writing, reviewing, or testing Elisp code.
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
- Never create "fallbacks" to handle problems silently: always choose errors over silent unexpected behavior.
- Whenever possible, test things end-to-end. You can use emacsclient for this.

# Elpaca profile

The current elpaca profile name is stored in the Elisp variable `init-current-profile`. Query it with:

```bash
emacsclient -e 'init-current-profile'
```

Always use this to resolve the active profile path (`~/.config/emacs-profiles/<profile>/elpaca/`) rather than hardcoding a profile name, since it changes over time.

# Byte-compilation and testing

To byte-compile or test an Elisp package in batch mode with all dependencies:

```bash
ELPACA=/Users/pablostafforini/.config/emacs-profiles/$(emacsclient -e 'init-current-profile' | tr -d '"')/elpaca
emacs --batch \
  --eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))" \
  --eval "(push \"$PWD/emacs/extras\" load-path)" \
  --eval "(require 'YOUR-PACKAGE)" \
  --eval "(message \"Result: %S\" (YOUR-TEST-EXPRESSION-HERE))" \
  2>&1
```

**How the load-path is set up:**

1. All elpaca build directories are prepended to `load-path` (overriding system packages like `transient` with elpaca's versions).
2. `$PWD/emacs/extras` is pushed to the very front, so extras under test always load from the working tree source `.el` files, not stale `.elc` from elpaca builds.

**Do not use `load-prefer-newer`** — it causes version mismatches between elpaca dependencies (e.g. magit's `.elc` expecting a transient API that differs from transient's `.el` source). The load-path ordering above already ensures the correct files are loaded.

For interactive testing against the running Emacs session, use `emacsclient -e '(EXPRESSION)'`.

# Rebuilding elpaca packages after changes

After changing an extras `.el` file, always rebuild the `.elc` and reload in the running session yourself — never ask the user to do it. Steps:

```bash
ELPACA=/Users/pablostafforini/.config/emacs-profiles/$(emacsclient -e 'init-current-profile' | tr -d '"')/elpaca
# Byte-compile from the repo source
emacs --batch \
  --eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))" \
  --eval "(push \"$PWD/emacs/extras\" load-path)" \
  --eval "(byte-compile-file \"$ELPACA/repos/dotfiles/emacs/extras/PACKAGE.el\")" \
  2>&1
# Copy the .elc to the builds directory
cp "$ELPACA/repos/dotfiles/emacs/extras/PACKAGE.elc" "$ELPACA/builds/PACKAGE/PACKAGE.elc"
# Reload in the running session
emacsclient -e '(load-file "$ELPACA/repos/dotfiles/emacs/extras/PACKAGE.el")'
```
