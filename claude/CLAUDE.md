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
- **Important**: config.org must always be edited at its canonical Dropbox path above, not at the copy in the profile's elpaca/repos/dotfiles/. The Elisp extras files (e.g. `emacs/extras/*.el`) should be edited in the profile's elpaca/repos/dotfiles/ directory (the git working tree).

### Projects

- My non-Emacs projects are stored in ~/Library/CloudStorage/Dropbox/repos/.
- Note that a non-Emacs project may have a companion Emacs package. For example, the tango-wiki project is stored at ~/Library/CloudStorage/Dropbox/repos/tango-wiki, but the Emacs package tango-wiki-mode is stored at ~/.config/emacs-profiles/<profile>/elpaca/repos/tango-wiki-mode.

## Version control

- By default, commit all changes, unless I specify otherwise or the changes are temporary.
- Do not mix unrelated changes in the same commit; if you need to make multiple unrelated changes, split them into separate commits.
