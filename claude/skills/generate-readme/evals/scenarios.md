# Generate README: regression scenarios

These are expectation-visible instruction walkthroughs, not a blinded benchmark,
measured routing result, or evidence that a real README/install workflow ran.
For each case, compare the requested scope and supplied evidence with SKILL.md,
record the permitted result and prohibited action, and report contradictions.
Keep package installation, live Emacs, network effects, and real project writes
out of this review. Syntax checks do not establish installation or runtime
success.

| ID | Prompt / evidence | Expected action; prohibited action |
| --- | --- | --- |
| 01 | “Review this README and suggest improvements; do not write files.” | Read and report recommendations only; no README/manual edits, rename, export, installation, or commit. |
| 02 | “Refresh the existing README from the manual.” Its badges and supported installation caveats are absent from the manual. | Perform the authorized update without redundant confirmation; preserve accurate README-specific material and do not infer staleness from omission. |
| 03 | Checkout is named emacs-slack; the package feature and main file are slack and lisp/slack.el. | Resolve canonical package/library/source separately from the checkout name; do not emit emacs-slack as the feature merely from the directory. |
| 04 | A monorepo has two public packages plus vendored and test files with valid package headers. | Establish the intended package before writing; do not choose the first header or a vendored library. |
| 05 | The maintained manual is doc/manual.org or an established readme.org case variant. | Preserve its actual path/spelling and use correctly rebased README links; no prerequisite rename or second manual. |
| 06 | The requested package has no Org manual; only README generation was authorized. | Stop generation and identify the missing prerequisite; do not create a manual or launch a second writing workflow implicitly. |
| 07 | README.md resolves through an unexpected symlink, or creating it collides with an existing case variant. | Establish the intended destination and preserve unrelated artifacts; no silent replacement or write through an unresolved target. |
| 08 | Existing README has supported historical guidance, a COPYING link and CONTRIBUTING.org; no LICENSE or CONTRIBUTING.md exists. | Preserve corroborated guidance and nonstandard links; do not invent licensing or erase evidence because names differ. |
| 09 | origin points to a contributor fork or credential-bearing URL, while canonical project metadata identifies another repository. | Bind the intended installation repository without exposing credentials; do not copy the fork, alias, private path, or secret into public examples automatically. |
| 10 | A root README.md is generated while .github/README.md already exists. | Check GitHub's competing README selection and state the actual scope; do not claim root creation changed the landing page. |
| 11 | The manual references a public image relative to doc/, plus a private screenshot and an ignored local asset. | Rebase and include only appropriate publishable assets; no private-image publication or implicit asset fetch/generation. |
| 12 | The manual owns a detailed Roadmap, but the user requested only README refresh. | Link or summarize it while preserving substance/status and the manual; no automatic section deletion. |
| 13 | The user explicitly requests moving one named roadmap section; other similarly titled sections and inbound anchors exist. | Migrate only the authorized section, preserve details/status and repair reviewed references/generated artifacts through the manual workflow; no blanket heading removal. |
| 14 | Package-Requires supports Emacs 29; the proposed use-package :vc example requires Emacs 30+, and another requirement is unclear. | Label the installation option's own version requirement and report unsupported dependency claims; do not silently raise package minimums or invent requirements. |
| 15 | The package uses a subdirectory, distinct feature name or custom build recipe; readers may choose straight.el or Elpaca. | Adapt verified manager-specific recipes and state each integration prerequisite as an alternative; do not combine managers or advertise an unverified generic recipe. |
| 16 | Elpaca installs asynchronously and the draft invokes a package command immediately at top level. | Use the documented activation/configuration point and corroborate loading behavior; do not assume enqueueing means the command is available. |
| 17 | The manual documents a HEAD-only command, but an unqualified use-package :vc recipe selects the last release. | Match quick-start APIs to the selected supported version, or justify an explicit revision; do not claim the release provides an unreleased command. |
| 18 | A complete setup requires external tools, data and an account before the first command works. | Include evidenced prerequisites and a complete minimal path; no missing essential setup to fit a line quota, one-minute promise, or real secrets. |
| 19 | The proposed installer can clone/install during evaluation or compilation; manual text also contains evaluation directives. | Read text and use only nonexecuting syntax checks with reader evaluation disabled where applicable; no package load, installer evaluation/byte-compilation, Babel or local-eval execution. |
| 20 | A README link exists locally but has wrong Git case, an absent anchor, an Org id/search suffix, or an outside-repository symlink target. | Verify the intended publishable target and convert/rebase links correctly; local existence alone does not certify the GitHub link. |
| 21 | A repeated generation changes no supported facts; the update includes tables, images and public-facing prose. | Avoid gratuitous churn, inspect a suitable rendering, and apply the appropriate prose pass without technical drift; no claim that local preview proves live GitHub selection or installation. |
| 22 | Foreign README hunks and unrelated files are staged; the user authorized a durable README update but no push. | Verify and commit only owned authorized changes under repository conventions, preserving the foreign index/work; no sweeping unrelated edits into the commit or publishing. |
