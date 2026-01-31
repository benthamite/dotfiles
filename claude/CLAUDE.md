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

- By default, and unless I specify otherwise, commit all the changes you make. You may skip this rule if you are making changes for testing purposes only, or if committing is otherwise not appropriate. But in general I prefer to have a record of all changes, because that makes it easier to review them later, and to revert them if necessary.

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
- Never create “fallbacks” to handle problems silently: always choose errors over silent unexpected behavior.

## Personal

### About me

I’m the director of [Tlön](https://tlon.team/), an organization that translates content related to [effective altruism](https://www.effectivealtruism.org/), [existential risk](https://existential-risk.com/concept), and [global priorities research](https://80000hours.org/problem-profiles/global-priorities-research/) into various languages. I am also a co-host of the podcast [La bisagra de la historia](https://labisagradelahistoria.org/) and a member of the [Samotsvety](https://samotsvety.org/) forecasting group. I used to write a [newsletter](https://futurematters.news/) together with [Matthew van der Merwe](https://matthewvandermerwe.com/).

I’ve been part of the effective altruism community since its inception and have collaborated with several EA orgs and people over the years. As [Will MacAskill](https://www.williammacaskill.com/)‘s research assistant, I was responsible for much of the background research for *[Doing Good Better](https://www.effectivealtruism.org/doing-good-better)*. More recently, I created, edited and wrote most of the content of the [Effective Altruism Wiki](https://forum.effectivealtruism.org/topics/all).

### Web presence

- Personal website: https://stafforini.com
- GitHub: https://github.com/benthamite
- ORCID: https://orcid.org/0000-0002-2753-6652
- Goodreads: https://www.goodreads.com/author/show/3093249.Pablo_Stafforini
- Letterboxd: https://letterboxd.com/stafforini/
- Effective Altruism Forum: https://forum.effectivealtruism.org/users/pablo_stafforini
- LessWrong: https://www.lesswrong.com/users/pablo_stafforini
- Reddit: https://www.reddit.com/user/Benthamite/

