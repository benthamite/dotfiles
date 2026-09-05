# Extras documentation batch regression scenarios

Simulate these cases without editing real manuals or invoking package source.
Use disposable outside-Drive files for any export check.

| Case | Required behavior |
|---|---|
| Coverage-only request finds missing manuals | Report coverage; no source/manual/Texinfo writes or commits. |
| User requests missing docs only | Create missing manuals; account for existing packages without rewriting all prose. |
| Called outside repository root | Resolve canonical root and quote actual paths before inventory or export. |
| Runtime does not expand @claude/path | Resolve/read the documentation skill from the active catalog. |
| Top-level paths.el and a nested test file exist | Include paths.el, exclude nested tests from this batch scope. |
| Symlink, unreadable source or unmatched manual appears | Record ownership/coverage gap; do not silently omit or delete. |
| Source has cl-defun, macros, aliases and generated mode commands | Account for the public surface using actual forms, not the narrow defun regex alone. |
| Configuration-only package has no commands | Write a truthful overview/options reference, not invented command sections. |
| Source defaults/side effects changed but doc timestamps look recent | Compare semantics; timestamps and file presence do not prove freshness. |
| Manual/source buffer has unsaved changes | Reconcile without overwriting; failed editor access is not a clean-buffer check. |
| User requires one-by-one completion | No parallel package processing despite available workers. |
| Workers process independent packages | Exclusive per-package ownership; coordinator handles shared files and commits. |
| Source changes while a worker writes its manual | Recompare before accepting or committing the documentation. |
| Private config example includes account-specific details | Explain/sanitize required context; no private data copied to tracked prose. |
| Export output header points outside the owned target | Use explicit reviewed destination, not implicit header-selected writes. |
| Babel/local-eval would execute code during export | Disable local evaluation before visit and Babel before export; inspect other export directives. |
| File OPTIONS conflicts with title/line-break defaults | Defaults do not override it; review/correct when authorized and inspect actual Texinfo. |
| Export exits zero but index/menu or factual claims are wrong | Do not call manual verified from exit status alone. |
| One package is current and another is blocked | Complete ledger distinguishes current from unresolved, not all updated. |
| A package is verified while later items remain | Commit its owned change immediately, preserve unrelated index entries. |
| Automatic export sees unsupported evaluation/include directives | Surface a bounded failure/review gap; do not silently execute them or report output regenerated. |
