# Global conventions

## General

- Respond in the same language as the prompt. If I prompt you in e.g. English, respond in English.
- When trying to guess something that could be learned from documentation you don't have access to, try to obtain that documentation, or else ask me to find it for you.
- Use sentence case instead of title case whenever possible.
- If you create temporary files or code, make sure to delete them afterwards.

## Filesystem organization

### Emacs

- My emacs configuration is stored in ~/Library/CloudStorage/Dropbox/dotfiles/emacs/config.org
- This is a "literate" config: the resulting Elisp files are tangled into the relevant Emacs profile.
- **To find the active Emacs profile directory, run: `emacsclient -e "user-emacs-directory"`**
- The Emacs packages (both external ones and my own) are located in the 'elpaca/repos/' subdirectory of the active profile.

### Projects

- My non-Emacs projects are stored in ~/Library/CloudStorage/Dropbox/repos/.
- Note that a non-Emacs project may have a companion Emacs package. For example, the tango-wiki project is stored at ~/Library/CloudStorage/Dropbox/repos/tango-wiki, but the Emacs package tango-wiki-mode is stored at ~/.config/emacs-profiles/<profile>/elpaca/repos/tango-wiki-mode.

## Version control

- By default, commit all changes unless I specify otherwise or the changes are for testing purposes only.

## Programming languages

### Emacs Lisp

- Write atomic, focused functions. When tempted to add a comment explaining code, refactor it into a function with a clear intention, so that the comment is no longer necessary. Functions should generally be only a few lines long.
- Never insert empty lines within a function.
- Put helper functions *after* the function that calls them, not before.
- Docstrings should document all arguments, capitalized.
- Fill all docstrings to 80 characters.
- The first line of the docstring should be a single-sentence summary.
- When writing multiline docstrings, do not leave a newline between the first and second lines.
- Do not end error messages with a period.
- Only add comments if truly necessary to understand the code. Avoid commenting every detail.
- Never create "fallbacks" to handle problems silently: always choose errors over silent unexpected behavior.
