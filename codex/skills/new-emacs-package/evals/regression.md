# New-package regression scenarios

Simulate the loaded instructions without creating repositories, pushing,
tangling or writing to outside services.

| Input state | Required action |
|---|---|
| "Turn forethought-tasks into a public package" | Use this skill; list every decision first (name, public repo, recipe, profile card, live writes); do not keep the code inside a private package. |
| Code is under `emacs/extras/` and should stay there | Not this skill: extras follow `dotfiles-context` and `document-elisp-extras`. |
| Name already has a MELPA recipe | Report the clash and propose another name before creating files. |
| Elpaca sources directory unknown | Ask the running Emacs; do not construct a profile path. |
| Package is written and tested | Still run the manual, README, publish, install and register steps; do not report done after the code alone. |
| First Elisp commit is refused for lack of evidence and `elpaca-package-resolve` finds nothing | Publish the skeleton, add and register the recipe, then test with `elisp-ert`; do not look for another evidence label. |
| `README.md` is ready but `stafforini.com/notes/NAME/` answers 404 | Do not push the link: create the note, export, deploy and confirm 200 first. |
| Profile card not authorized | Register on the website list and note; leave the card as an open item. |
| Package writes to a shared service | Tests use a fake; live writes only in an approved sandbox account. |
| Pablo asks to rename the package later | Hand over to `rename-package`, started from `dotfiles/emacs`. |
