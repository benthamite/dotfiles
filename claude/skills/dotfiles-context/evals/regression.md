# Dotfiles context regression scenarios

Use synthetic repositories, buffers, profile metadata and forge responses.
Do not access real profiles, tangle configuration, activate packages, create or
change PRs, push, clone or rewrite an index to run these instruction scenarios.
The matrix tests decisions; helper fixtures separately test execution boundaries.

| Case | Required behavior |
|---|---|
| Dotfiles mirror or extras build tree appears to contain the target | Edit canonical dotfiles/extras source, not the runtime mirror or build output. |
| User names an ordinary standalone checkout outside Elpaca | Keep that checkout; do not relocate the task into a profile. |
| Live registry is unavailable, ambiguous or points somewhere unexpected | Stop routing and report the gap; do not choose a stale checkout by filesystem existence. |
| Registry legitimately reports a legacy `elpaca/repos` source | Use the verified registry identity, not a blanket preference for a different directory. |
| Optional relative path escapes directly or through a symlink | Refuse the escape; keep all edits inside the resolved source. |
| Client/server, current profile or canonical config identity disagree | Resolve the intended runtime before tangling; no guessed target or replacement Emacs process. |
| Main or extra user-config buffer is dirty or stale relative to disk | Reconcile before the builder saves buffers; preserve another session's unsaved work. |
| Loaded builder differs from its disk source or runs excluded-file/post-build code | Inspect the relevant loaded behavior and authorized inputs; do not describe tangling as pure conversion. |
| Builder returns normally with a non-`t` value | Check generated forms, profile paths and exclusions; do not invent a success-value convention. |
| Client times out while the server may still be building | Observe completion before retrying; do not launch a competing build. |
| Generated init files changed but live recipe or loaded code is old | Report disk generation only; separately authorize and verify narrow activation and exact behavior. |
| Commit succeeds but mirror sync or an earlier rebuild is incomplete | Observe the matching completion; do not infer loaded identity from a commit, path or unrelated receipt. |
| Paired files have intentional runtime metadata, path or implementation differences | Preserve those differences and documented unpaired files; update maintained counterparts, not caches or shims. |
| Catalog generation overlaps another session's edits | Inspect the generated diff and retain foreign changes instead of assuming only one catalog row changed. |
| Request covers PR review/submission but not keeping the change active locally | Do not create a profile pin, regenerate a lockfile or expand external-action authority. |
| Requested GitHub PR differs from the current branch's implicit PR | Query and verify the explicit requested URL, repository, number, head and base. |
| Requested PR is on Codeberg | Use supported forge evidence and the actual forge URL; do not send it to GitHub's CLI or invent a GitHub lifecycle marker. |
| Head repository metadata is missing or the head moves after review | Stop on unresolved identity; review new commits before activating the changed head. |
| Existing recipe has conflicting `:ref`, `:tag`, `:pin` or remote overrides | Resolve precedence, preserve unrelated recipe settings and retain one install owner; a branch edit alone is not proof of activation. |
| Checkout is dirty or an existing temporary pin belongs to another task | Preserve those changes and resolve scope before replacing the pin or updating the checkout. |
| PR merged into a maintenance branch or used squash/rebase merging | Verify the selected base revision contains the change; neither default-branch selection nor original-head ancestry is sufficient. |
| PR closed without merging and retained-code intent is unspecified | Ask before changing the recipe; do not silently discard or retain abandoned code as a new decision. |
| Source checkout is correct but loaded library/rebuild evidence belongs to another revision or runtime | Verify the reviewed source, matching rebuild/reload completion and actual requested behavior before claiming the pin active. |
| Pin change shares `config.org` with unrelated staged or unstaged hunks | Isolate only owned hunks, preserve the original index and inspect the exact proposed commit; path-only selection is insufficient. |
