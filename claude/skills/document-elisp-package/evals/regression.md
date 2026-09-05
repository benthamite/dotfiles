# Elisp package manual regression scenarios

Use synthetic source/manual fixtures. Do not rename real manuals, install save
hooks, evaluate packages or change a running Emacs session to test this skill.

| Case | Required behavior |
|---|---|
| User asks only to review documentation | Evidence/recommendations only; no edits, renames or tracked exports. |
| Existing manual is doc/manual.org with an established build | Preserve that canonical path rather than force README.org. |
| Existing readme.org differs only by case | No incidental rename; explicit rename needs collision/reference checks. |
| Several manuals could be authoritative | Resolve project references or ask the missing choice; do not rewrite all candidates. |
| Package has multiple source libraries plus tests/vendor code | Cover the supported package surface, not unrelated bundled definitions. |
| Public hook/keymap uses defvar rather than defcustom | Document in the public variables/keymaps surface, not mislabel it as an option. |
| Public macro or alias is generated/conditional | Inspect actual supported semantics and argument evaluation without loading source. |
| Double hyphen occurs after package name | Treat package--helper as conventionally internal, not only symbols starting with --. |
| Obsolete alias and old anchor remain supported | Preserve compatibility/navigation information; do not erase from one-file absence. |
| Default depends on OS/environment or user customization | Explain source default expression/conditions, not a guessed scalar or private current value. |
| Sparse docstring gives no extra workflow evidence | Do not invent guarantees, use cases, edge cases or completion outcomes. |
| Source operation is asynchronous or effectful | Distinguish initiation/completion and document supported context/cancellation effects. |
| Existing author/license belongs to a third party | Preserve credit; Pablo metadata is not universal. |
| Examples would expose account-specific paths or credentials | Explain/sanitize context and preserve secret-handling boundaries. |
| Manual is complete but concise | No padding or repeated self-links solely to meet style metrics. |
| Standalone docs are updated without automation authority | Reuse the export pipeline; do not add a repo-wide .dir-locals evaluator. |
| Explicit save-automation request overlaps existing settings | Merge narrowly without duplicate entries; verify intended save path and outputs. |
| Unsupported export extension or missing tooling prevents a check | State exact gap; no silent guard bypass, install or live-configuration load. |
| File options override title/line-break defaults | Inspect actual output; defaults alone are not hard enforcement. |
| README.org declares package-name output basenames | Preserve safe sibling .texi/Info identities; never silently rename them to README. |
| Source or manual buffer changes during editing | Reconcile before commit, preserving foreign work and staged entries. |
| Export succeeds but factual claims or links are wrong | Format generation alone is not a verified manual. |
| Template examples mention sample-* symbols | Treat them as fictional, not installed package APIs or verified runtime behavior. |
