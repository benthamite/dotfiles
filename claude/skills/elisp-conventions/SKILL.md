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
