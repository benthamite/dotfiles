---
name: elisp-conventions
description: Emacs Lisp coding conventions, rebuild patterns, and batch-testing recipes. Use when writing, reviewing, or testing Emacs Lisp.
---

- Write atomic, focused functions.
- Do not insert empty lines within a function.
- Put helper functions after the function that calls them.
- Docstrings should document all arguments, capitalized.
- Fill docstrings to 80 characters.
- The first docstring line should be a single-sentence summary.
- In multiline docstrings, keep the summary and following sentence in the same
  paragraph; separate later paragraphs with blank lines.
- Do not end error messages with a period.
- Add comments only when they carry real explanatory value.

# Reloading edited code

For the dotfiles/elpaca workflow, use `elpaca-rebuild` plus reload rather than
`load-file`, `eval-buffer`, `eval-defun`, or ad hoc byte-compilation commands.

# Batch testing before commit

Before committing Elisp changes, run:

```bash
~/My\ Drive/dotfiles/claude/bin/batch-test.sh YOUR-PACKAGE
~/My\ Drive/dotfiles/claude/bin/batch-test.sh YOUR-PACKAGE '(message "Result: %S" (YOUR-TEST-EXPRESSION))'
```

If Codex hooks are enabled, commits may be blocked until a matching
`emacs --batch` test has run in the current session.

# Transient menus

After adding or modifying a `transient-define-prefix`, verify that every suffix
symbol is interactive:

```bash
emacsclient -e '(interactive-form (quote SYMBOL))'
```

Transient defers suffix validation to invocation time, so byte-compilation does
not catch non-interactive suffixes.

# Batch-testing details

- Add all `elpaca/builds/*/` directories to `load-path`.
- Push `emacs/extras` to the front so edited `.el` sources win over stale
  `.elc` files.
- Do not use `load-prefer-newer` for this workflow.
