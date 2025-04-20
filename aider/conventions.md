## Emacs Lisp

- If you need to write Emacs Lisp, please write brief, focused functions. When you find yourself writing a comment to explain what some code does, consider refactoring it into a function. In general, the functions should not be more than 10 lines long, though you can exceed that limit if really necessary.
- Within a function, never insert empty lines.
- Put the helper functions *after* the function that calls them, not before.
- Docstrings should document all the arguments of the function, capitalized.
- Fill all docstrings to 80 characters.
- The first line of the docstring should be a summary of the function, and should consist of exactly one sentence.
- Do not end an error message with a period.
