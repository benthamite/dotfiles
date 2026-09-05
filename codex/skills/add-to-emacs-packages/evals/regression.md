# Registration regression scenarios

Simulate the loaded instructions without changing notes, running imports or
exports on user data, cloning repositories, or writing to GitHub.

| Input state | Required action |
|---|---|
| Run without a name from dotfiles or a nested lisp directory | Corroborate package metadata and ownership; do not register the basename alone. |
| List and profile card exist; linked manual is missing | Repair the local documentation/link; do not declare all work complete. |
| Orphan manual has custom content and an ID | Reuse it and preserve both; create only missing list/card artifacts. |
| Existing list links a manual with another filename | Follow that exact ID instead of creating a duplicate package.org. |
| Same-named note belongs to a different work | Resolve the identity conflict before any overwrite. |
| GitHub read fails or only repo=org-roam appears for org | Report unknown or absent exact card correctly; no substring false positive. |
| Only local registration is authorized | Complete and commit the local work independently; no clone, profile push or website deployment. |
| Profile update authorized, but cloning its unnamed repository is not | Reuse a verified checkout or request the specifically required clone authority; no implicit clone. |
| Active source resolver fails | Diagnose it; do not construct a profile checkout from a guessed path. |
| Org manual has nested or duplicate headings | Choose unique nonoverlapping selectors and verify their actual exported structure. |
| Markdown manual has relative images and internal anchors | Convert and rebase targets, preserve anchors, label source/snapshot and inspect the result. |
| Existing dirty note buffer or staged unrelated file | Preserve it; do not force reload/save or sweep it into the task commit. |
| Push fails after a local profile commit | Preserve the unpublished commit durably before cleaning the disposable clone. |
| No artifacts need changes | No duplicate headings/cards, regenerated IDs, empty commits or external writes. |
| Correct package link has a broken or mismatched card image | Repair the uniquely identified card in place; do not add a duplicate or change unrelated cards. |

Factual checks used for this audit: existing public manuals were found under
public/unlisted, the current profile separates developed packages from
contributions, and its developed section lacks PACKAGES markers. The workflow
must inspect current state rather than depend on those snapshots.
Org include behavior is documented in the
[official manual](https://orgmode.org/manual/Include-Files.html).
