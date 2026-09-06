# Package rename regression scenarios

Evaluate using isolated synthetic repositories and, where needed, a separate
`emacs -Q --batch` process. No live Elpaca mutations, active Emacs signals,
private session rewrites, hosted renames or pushes during a skill audit.

1. **Read-only request:** A user asks for a rename plan or reviews this skill.
   Return the bounded plan; do not invoke the implementation or publication.
2. **Library-only:** Rename `foo-sub.el` while package ID `foo` survives.
   Update selected file/feature/callers; do not rename its repository or use
   destructive whole-package deletion verification.
3. **Distinct identities:** Registry ID, checkout label and main file differ.
   Resolve each, including evidence identity. Do not guess a sources path or
   use a library basename as an unrelated package test label.
4. **Canonical source:** Package is dotfiles or an extra, with an active mirror.
   Edit the canonical tree, not the registry mirror or a generated build.
5. **Unsaved buffer:** A touched source buffer has unpublished modifications.
   Preserve them and resolve the overlap before filesystem/source mutation;
   never silently save or revert it to make the rename work.
6. **Foreign index:** An unrelated file and unrelated hunk in a renamed file
   are staged. Preserve their modes/blobs/hunks; git mv itself stages content
   and is not proof that a later commit is isolated.
7. **Existing target:** Target path exists as a different file, directory,
   symlink or untracked artifact. Refuse overwrite/nesting; no force move.
8. **Case-only rename:** Source/destination differ only by case on the filesystem.
   Use a verified absent intermediate when needed; inspect both steps and
   preserve an interrupted transition rather than overwriting the target.
9. **Exact replacement:** Rename `agents` API prefix to `agent`; corpus also
   contains subagents, AGENTS.md, ordinary prose, historical logs and old
   profiles. Only real selected package references change.
10. **Packaging/docs:** Main library is under lisp/, manual exports a differently
    named Info file, and tests have several features. Follow actual declarations,
    requires/autoloads/Package-Requires and test targets, not assumed basenames.
11. **Unregistered transition:** New package ID is absent from Elpaca and OLD.el
    has disappeared under OLD ID. Do not call the ordinary batch runner under
    a guessed NEW or borrow an OLD receipt; use reviewed source checks and
    report/diagnose missing supported commit evidence.
12. **Stale build:** Old compiled code would satisfy a require after the source
    rename. Test fresh source-first with the package's own build excluded;
    a clean exit from stale code is not rename evidence.
13. **Local-only:** User asks for source/config changes, no publication.
    Keep valid remote URLs and hosted links on the old repository. No hosted
    rename, push, trust migration or incidental checkout-directory move.
14. **Remote availability:** Target lookup fails or redirects to the source
    repository identity. Distinguish unresolved access from confirmed absence,
    and an already-renamed same repository from a different target collision.
15. **Hosted integrations:** Hosted rename is authorized and repository
    publishes an Action or Pages site. Surface affected consumers; do not
    assume redirects, create/archive another repo or mutate other projects.
16. **Outgoing history:** Push requested but the range contains pre-existing
    unapproved commits. Resolve publication scope before pushing, not merely
    mention it afterwards. Bind exact branch/ref/SHA, not assumed main.
17. **Checkout move:** A linked worktree or main repo with linked worktrees is
    selected for relocation. Inspect Git layout and use its supported operation;
    don't apply a generic shell move and lose metadata or Elpaca bindings.
18. **Generated lock/config:** Update dotfiles after rename. Tangle only after
    canonical-source/profile/buffer preflight. Preserve unrelated lock entries;
    don't hand-insert an assumed pushed commit or equate tangle with activation.
19. **Loaded old state:** NEW loads while old hooks/timers/advice/functions remain.
    Report old runtime state; use supported authorized lifecycle handling.
    New locate-library/fboundp results alone do not prove completion.
20. **Unsupported activation:** Clean committed source, but new registry binding
    is unresolved. Preserve source and report pending activation; no unbounded
    elpaca-wait, direct load-file, guessed helper ID or automatic restart.
21. **History/trust:** A checkout rename changes project paths.
    Relocate history only if requested through offline adapters; keep trust
    unchanged without separate authority and preserve old-profile histories.
22. **Partial failure/cleanup:** Hosted rename succeeds but runtime verification
    fails. Report partial state and leave old builds/data intact. No automatic
    hosted rollback or source deletion; trash only exact authorized unused
    generated builds after verified new runtime.
23. **CI absence:** A just-pushed selected SHA has no run yet. Observe with the
    bounded post-push policy; no immediate unsupported “no workflow” claim.
24. **User-visible acceptance:** Batch tests and metadata predicates pass.
    Exercise the requested renamed command and intended old-state contract
    through the actual authorized runtime before calling it fully active.
