---
name: release-dotfiles
description: Release a new version of the dotfiles repo. Use when the user says `/release-dotfiles`, asks to cut/publish/prepare a dotfiles release, bump the dotfiles version, or create the GitHub release after local profile testing. Do not use for standalone Emacs package releases; use release-package instead.
---

# Dotfiles release

Prepare and release `benthamite/dotfiles`. Use `dotfiles-context` for canonical
source routing and paired configuration checks, and `publish-dotfiles` for
every dotfiles push. Reading this procedure does not authorize its side effects.

## Modes and resumption

- A preparation or version-bump request stays local. Do not push package repos,
  publish tags, or create a GitHub release without explicit publication authority.
- A release request may proceed only through the gates below.
- `--accept` skips the version/notes confirmation in step 8. For a publication
  request it also approves the reported clean, unpushed package repos in step 1,
  after their ownership and push destinations are verified. It does not expand a
  preparation request into publication or authorize unrelated repositories.
- `--accept` never skips the post-lockfile profile test in steps 9–10 or either
  security-review layer in step 11.

When resuming, identify the exact prepared candidate, approved version and notes,
and completed gates. Do not repeat a lockfile commit, tag, push, or release
creation merely because the session restarted. Reconcile any existing artifact
with the exact candidate and approved content before reusing it. A prior profile
test applies only to the unchanged candidate that was actually tested.

## Step 0: resolve the repository and active profile

Confirm the canonical dotfiles checkout and its exact `origin` identity:

```bash
git remote get-url origin
git status --porcelain
```

Require the GitHub owner/repository to be exactly `benthamite/dotfiles`, not a
substring match. Use the existing canonical checkout if the current directory is
wrong; do not clone or modify the Elpaca dotfiles mirror.

Resolve the live Emacs values of `init-current-profile`,
`elpaca-sources-directory`, and `init-master-lockfile-path`. Require successful
queries, valid strings, and an existing source directory. Do not reconstruct
paths from a profile label or strip quotes from an unchecked error. The lockfile
destination must resolve to the canonical repo's `emacs/lockfile.el`.

Inspect the dotfiles worktree and index before any release edit. Preserve
unrelated or concurrent changes. Do not stash or commit them implicitly; obtain
a decision about a conflicting change rather than telling the user to perform
routine work the agent can do. Release preparation requires a clean, agreed
baseline before modifying recipes or writing the lockfile.

## Step 1: check Elpaca source reproducibility

Inventory each distinct Git checkout under the live Elpaca source directory,
including Git worktrees whose `.git` is a file. Resolve real paths and deduplicate
shared checkouts. Inspect working-tree and staged changes with:

```bash
git -C "$PACKAGE_REPO" status --porcelain
```

The `sources/dotfiles` checkout is a read-only runtime mirror. Check that it
matches the canonical committed source through the supported synchronization
path; never commit or push that mirror as an ordinary package repo.

For other packages, distinguish detached HEAD, missing upstream, Git command
failure, behind/diverged history, and genuine unpushed commits. Check upstream
existence separately before querying its range:

```bash
git -C "$PACKAGE_REPO" rev-parse --verify '@{upstream}'
git -C "$PACKAGE_REPO" log --oneline '@{upstream}..HEAD'
```

An arbitrary nonzero exit is not proof of a missing upstream. Fetch the relevant
remote refs before judging visibility. For a detached or upstream-less package,
verify whether its exact HEAD is reachable from the intended remote's advertised
branch/tag history. No upstream is not automatically a blocker, but an
unpublished or unverifiable lockfile commit is. A lockfile hash cannot preserve
changes that exist only in a dirty worktree.

Run every `git push` and `gh` command as its own tool call with a literal
target; the GitHub write guard rejects compound commands and redirections.

Report dirty repos and clean-but-unpublished repos separately, with the exact
target and proposed action. In publication mode, `--accept` permits pushing the
reported clean package repos within its scope; otherwise obtain scoped
confirmation. Never infer a writable fork destination from an upstream URL.
Inspect outgoing changes before pushing and use `post-push-ci` after each push.
A failed push does not stop the inventory, but unresolved reproducibility or CI
failures block the release.

Untracked byte-compiled artifacts may be cleaned with `trash` only after
inspecting the exact candidates and confirming they are disposable generated
files, not tracked files, symlinks, or source material. Identify candidates with:

```bash
git -C "$PACKAGE_REPO" ls-files --others --exclude-standard -z -- '*.elc'
```

Preserve NUL-delimited paths. Recheck status after cleanup. If `trash` is
unavailable, leave the files and report the blocked cleanup; do not substitute
destructive deletion.

For remaining dirty repos, ask which exact changes may be committed and pushed;
make both effects explicit. After approval, inspect the diffs, split logical
changes, and stage only the agreed paths. Do not use a blanket `git add -A`
across work owned by concurrent sessions. Recheck cleanliness, remote visibility,
and CI before proceeding. Preparation-only mode may report a future push without
performing it; that unresolved gate must be satisfied before publication.

## Step 2: reconcile temporary Elpaca PR pins

Scan `emacs/config.org` for `awaiting PR merge` markers and legacy GitHub PR
URLs. Inspect each enclosing `use-package :ensure` recipe or explicit
`elpaca` order. A bare PR URL is a candidate, not proof of an active pin.

For each actual pin, query supported CLI fields:

```bash
gh pr view "$PR_URL" \
  --json state,mergedAt,baseRefName,headRepository,headRepositoryOwner,headRefName,url
```

Use `state == MERGED` / non-null `mergedAt`; `merged` and
`baseRepository` are not supported `gh pr view --json` fields. Derive the
base owner/repo from the returned PR URL, and query its default branch:

```bash
gh repo view "$BASE_REPO" --json nameWithOwner,defaultBranchRef
```

Compare the recipe with the actual head repository owner/name and branch; handle
a deleted head repository explicitly instead of guessing.

- If the recipe no longer points to that PR head, report “comment only / already
  restored” and leave it unchanged.
- If the PR is open, retain and report the intentional pin.
- If it is closed without merge, obtain a decision before retaining or replacing
  abandoned code in a release.
- If merged, restore the base repo. Remove the branch only when the PR base is
  the repo's default branch; otherwise retain the base branch explicitly.

Preserve unrelated recipe options and explanatory comments. Remove the PR marker
only where it no longer describes a temporary pin. Keep one install recipe per
package, including dependency-only orders.

After restoration, tangle through `dotfiles-context` and commit only the recipe
changes as a separate logical change. Tangling does **not** update the live
Elpaca queue or source checkout. Before writing a lockfile, use the supported
Elpaca reconfiguration/update path and verify the live recipe and checkout match
the restored upstream. The lockfile writer serializes the live queue and source
HEADs, not merely `config.org`. If activation requires a reload or new profile,
complete that transition and its checks first; never fabricate queue state or
hand-edit a lockfile to claim it happened.

Recheck all remaining pins and the source-reproducibility gate after transition.

## Step 3: recheck the dotfiles baseline

Require a clean worktree/index after the agreed prerequisite commits. If new
changes appeared, inspect and reconcile them with their owner; do not sweep them
into the release. Recheck the Elpaca mirror against the committed source.

## Step 4: synchronize the release branch

Fetch `origin` and inspect the configured upstream and ahead/behind counts.
Treat fetch/query failures as failures, not “already synchronized.” Resolve a
behind, diverged, detached, or incorrectly targeted branch before continuing.
Do not force-push or silently merge unrelated work.

## Step 5: select the previous release and unreleased range

After a successful tag fetch, enumerate reachable tags:

```bash
git fetch --tags origin
git tag --merged HEAD --list --sort=-version:refname
```

Filter the successful listing to exact canonical numeric `MAJOR.MINOR.PATCH`
names, with no `v` prefix, prerelease suffix, or leading-zero components except
zero itself. Take the highest numeric version as `LATEST_TAG`. Do not use a
loose glob or treat a failed lookup as “no releases.”
The exact-name filter is `^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$`.

If no canonical tag exists, report `none` and classify the full history.
Otherwise read every commit in `"$LATEST_TAG"..HEAD`. If the range is empty,
there is nothing to release.

## Step 6: propose the version

The version lives in the tag, not a package `Version:` header.

- Breaking changes require a major bump.
- New functionality requires a minor bump.
- Bug/configuration fixes require a patch bump.
- Docs/chore-only changes do not imply a bump by themselves; obtain a version
  decision if the user still wants a release.

Let the user override the suggestion. Store the accepted canonical version in
`NEW_VERSION`; check for local and remote tag/release collisions before making
the release commit. Do not overwrite an existing version. For a legitimate
resumption, verify the existing artifact belongs to this exact candidate.

## Step 7: draft and freeze release notes

Apply the user's outgoing-prose conventions (`personalize` when publishing as
Pablo). Write user-facing descriptions rather than copying terse commit messages.
Group features, fixes, and other changes; for a large range, summarize by theme
while accounting for the significant changes.

Keep notes in a uniquely named temporary workspace outside Drive. Record the
file as `RELEASE_NOTES_FILE`. After approval, freeze its content and record its
SHA-256 digest. The exact same file must be reviewed and passed to GitHub; do not
retype a heredoc or publish literal placeholders later. A content change requires
renewed approval where applicable and a fresh public-text review.

## Step 8: pre-release confirmation

Present the previous/proposed versions, included commit count, package
reproducibility/CI status, remaining PR pins, and notes. Wait for explicit
confirmation unless `--accept` applies. Do not skip unresolved prerequisite
gates. A preparation request still does not authorize publication.

## Step 9: write the lockfile and release commit

Only after the live recipes/checkouts and approved version/notes are ready. The
writer serializes the live Elpaca queue plus each checkout's HEAD:

```bash
emacsclient -e '(elpaca-extras-write-lock-file-excluding init-master-lockfile-path)'
```

A recipe changed in `config.org` during this release (a restored or new pin, a
bootstrap ref) is still the old recipe in the running session, and the lockfile
would record it. Do not ask for an Emacs restart for that. Build replacement
entries for exactly those packages with Elpaca's own constructor from the
tangled orders, bind each to its existing checkout, copy the live entry's
`init` flag (the lockfile filter drops entries without it), and pass the live
queue with those entries swapped in as the helper's second argument:

```elisp
(let* ((rebuilt (mapcar (lambda (order)
                          (let ((e (elpaca<-create order)))
                            (setf (elpaca<-source-dir e) EXISTING-CHECKOUT-DIR
                                  (elpaca<-init e) (elpaca<-init (elpaca-get (car order))))
                            (cons (car order) e)))
                        ORDERS))
       (ids (mapcar #'car rebuilt))
       (queue (append (cl-remove-if (lambda (cell) (memq (car cell) ids))
                                    (elpaca--queued))
                      rebuilt)))
  (elpaca-extras-write-lock-file-excluding init-master-lockfile-path queue))
```

Before committing, check that `bin/check-lockfile` passes, the entry count
matches the previous lockfile, every rebuilt entry records the intended
repository, branch, and ref, and every personal-package ref is the current
dotfiles HEAD. Then stage the file, record evidence, and commit only it:

```bash
git add -- emacs/lockfile.el
claude/bin/elisp-check-evidence file:emacs/lockfile.el -- bin/check-lockfile
git commit --allow-empty -m "$NEW_VERSION"
```

Nothing else may be staged. `git commit --only` with a divergent index trips
the evidence gate, so keep the index limited to the lockfile. Record the release
candidate's exact commit ID.

## Step 10: build and smoke-test the profile, then stop for confirmation

Build and test the profile yourself before asking the user to look. The user's
launch is the final confirmation, not the first test.

1. Confirm no Emacs runs with `--init-directory` pointing at the version's
   profile directory (`ps -axo pid,command`; `pgrep -f` can miss it). Ask the
   user to quit one that does. Trash any existing directory of that name:
   Elpaca skips cloning when a source directory exists, so a stale profile from
   an earlier attempt tests old checkouts and an old lockfile.
2. Create the profile from the candidate without prompts:

   ```bash
   emacsclient -e '(let ((dir (init-create-profile "'"$NEW_VERSION"'" t))) (init-copy-lockfile dir) (with-current-buffer (find-file-noselect paths-file-config) (init-build-profile dir)) dir)'
   ```

   Check that the profile's `lockfile.el` is byte-identical to the candidate's
   and that its `init.el` carries every recipe changed in this release.
3. Launch a separate GUI Emacs on the profile, the way the user does, with the
   reporter in this skill's `scripts/smoke-report.el` loaded after init:

   ```bash
   SMOKE_REPORT_FILE="$REPORT" /Applications/Emacs.app/Contents/MacOS/Emacs \
     --init-directory="$HOME/.config/emacs-profiles/$NEW_VERSION" \
     -l "$SKILL_DIR/scripts/smoke-report.el"
   ```

   Run it in the background and poll `$REPORT` until it reads `state: final`.
   Do not use `--batch`: a batch Emacs exits at the first process-sentinel
   error and hides every other failure.
4. If the report lists a failed package, read its Elpaca log there. A
   dependent of a failed package fails with "Failed dependencies"; find the
   first failure. An unknown ref means an unpushed commit; "exists. Skipping
   clone" followed by a checkout error means a stale or shared checkout. Fix
   the root cause, redo the affected steps (checkout, pin, push, lockfile,
   release commit), quit the test Emacs, and repeat from item 1 until the
   report shows no failures. Do not hand a failing profile to the user.
5. A profile launch rewrites `~/.config/emacs-profiles/.current-profile`.
   Restore it to the live profile afterwards, or the commit hooks sync the wrong
   dotfiles mirror and live checks fail with "mirror HEAD does not match".
6. Quit the test Emacs you launched. Then wait for explicit confirmation that
   the profile built and worked for the user at the exact release candidate,
   and that publication should continue. `--accept` cannot supply this.

If fixes or a lockfile rewrite change the candidate, incorporate only the agreed
changes and repeat this step. Do not silently amend untested changes into a
confirmed release. Preserve a preparation-only result until the user authorizes
publication.

## Step 11: guarded publication review

Follow `publish-dotfiles`, including any due full-history audit, before creating
a new tag. A full-audit receipt requires successful status and
`full-audit-recorded:`, not merely a clean-looking review count.

```bash
bin/dotfiles-publish scan --mode release \
  --release-notes "$RELEASE_NOTES_FILE" \
  --tag "$NEW_VERSION"
```

Capture the printed run ID as `RUN_ID` and candidate as `CANDIDATE`. Continue review after
exit 2; fail closed on scanner/setup failure. Display and adjudicate every unit,
including the release notes and exact tag name. Do not record blanket clean
verdicts. Check public text for private paths, internal details, and unapproved
plans as well as credentials. Require a successful, complete clean review.

If repair changes the candidate, invalidate the earlier profile confirmation,
refresh the live source/lockfile where affected, obtain a new profile test, and
rescan/review. Never delete or move a preexisting/public tag. Clean up only this
workflow's own unpublished tag after verifying its exact identity and absence
from the remote.

## Step 12: tag, publish, and create the release

Recheck the candidate, approved version, notes digest, and applicable authority.

1. Create a lightweight tag on the exact reviewed candidate, not an unchecked
   current HEAD:

   ```bash
   git tag "$NEW_VERSION" "$CANDIDATE"
   ```

2. Publish the branch and tag together:

   ```bash
   bin/dotfiles-publish push --run "$RUN_ID" --tag "$NEW_VERSION"
   ```

   Require success and verified remote refs. Follow `publish-dotfiles` for
   recovery, remote-tracking refresh, and `post-push-ci`. Do not create the
   GitHub release unless the tagged candidate passes required CI. A code fix
   after publication requires a newly tested candidate/version, not moving the
   public tag or assuming branch CI certifies the old tagged commit.

3. Recheck that the notes file still has the approved and reviewed digest, then
   create the release using that file:

   ```bash
   gh release create "$NEW_VERSION" \
     --repo benthamite/dotfiles \
     --title "$NEW_VERSION" \
     --verify-tag \
     --notes-file "$RELEASE_NOTES_FILE"
   ```

   If the request times out or returns an ambiguous failure, query the exact
   release before retrying. Do not duplicate, delete, or replace a possibly
   successful release blindly. Reconcile an existing release in a resumed
   workflow instead of recreating it.

## Step 13: verify and clean up

```bash
gh release view "$NEW_VERSION" --repo benthamite/dotfiles \
  --json tagName,body,url,isDraft,isPrerelease
git ls-remote --tags origin "refs/tags/$NEW_VERSION"
```

Verify the remote lightweight tag equals the reviewed candidate, the release has
the intended publication state, and its body matches the reviewed notes. A
successful command alone is not proof of those facts. Report the release URL and
any unresolved gate. Remove this workflow's temporary notes/evidence only after
verification or a deliberate abort; preserve them securely for a pending resume.

For access, network, or Emacs-query failures, diagnose the exact error and use
available authorized recovery paths. Do not label every 403 as rate limiting,
switch credentials speculatively, or claim success with an unmeasured gate.
