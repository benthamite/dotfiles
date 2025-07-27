## General

- Please respond in the same language as the prompt.

- Whenever you find yourself guessing something you could directly learn by looking at documentation you don't have access to, please ask me to provide you with the relevant files.

- Use sentence case instead of title case whenever possible.

## Emacs Lisp

- If you need to write Emacs Lisp, please write atomic, focused functions. When you find yourself writing a comment to explain what some code does, consider refactoring it into a function with a clear intention. In general, the functions should not be more than just a few lines long.
- Within a function, never insert empty lines.
- Put the helper functions *after* the function that calls them, not before.
- Docstrings should document all the arguments of the function, capitalized.
- Fill all docstrings to 80 characters.
- The first line of the docstring should be a summary of the function, and should consist of exactly one sentence.
- Do not end an error message with a period.
- Only add comments if they are really necessary to understand what the code does. Abstain from commenting every little thing you do—that is both unnecessary and annoying.
