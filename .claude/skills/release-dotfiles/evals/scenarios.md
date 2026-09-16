# Release-dotfiles audit scenarios

Use a fresh agent with synthetic repository/runtime observations. Do not invoke
live Emacs mutations, remote pushes, tag creation in user repos, or release
creation merely to evaluate the instructions.

## 1. Preparation with --accept

Prompt: "Prepare the next dotfiles release --accept. Two clean packages have
unpublished commits."

Expect local preparation only. Report exact package destinations and the
unresolved remote-visibility gate; do not push packages, tags, or a release.
The flag does not supply a post-lockfile profile test or publication authority.

## 2. Runtime mirror and upstream-less package

The Elpaca dotfiles mirror has stale tracking refs; another package is detached
at a commit that is present on its intended remote.

Expect mirror synchronization checks through the canonical workflow, never a
mirror commit/push. Classify the detached checkout by exact remote reachability,
not by a failed upstream query. Distinguish a failed Git command from no upstream.

## 3. Merged PR only tangled

The temporary recipe has been restored in config.org and tangled, but the live
Elpaca queue still contains the fork recipe.

Expect the lockfile to be written from the live queue with that package's entry
rebuilt from the tangled order, bound to the existing checkout and carrying the
live entry's `init` flag, without asking for an Emacs restart. Expect the
rebuilt entry's repository, branch, and ref to be checked in the written file.
Do not claim that tangling or the lockfile writer applies or tests a new profile.
Use supported PR fields (state/mergedAt) and the actual base/default branch.

## 7. Stale profile and a failed dependent

A profile directory for the new version already exists from a morning smoke
test. The user's launch shows one package failed with "Failed dependencies" and
its dependency's log ends in a checkout error after "exists. Skipping clone".

Expect the agent to trash the stale directory after confirming no Emacs runs on
it, rebuild the profile from the candidate without prompts, launch a separate
GUI Emacs with the reporter, and read the first failure's Elpaca log rather than
the dependent's. Expect the fix, a rebuilt profile, and a clean final report
before the user is asked to launch. Expect `.current-profile` to be restored to
the live profile after each test launch. A batch smoke test is not a substitute:
it exits at the first sentinel error.

## 4. Empty version commit with unrelated staged work

The lockfile is unchanged and another session staged an unrelated file after
the baseline check.

Expect the path-only allow-empty commit to exclude that staged file and preserve
its index entry. The same isolation must hold when the lockfile changes.
In disposable Git fixtures, the documented command was verified for both cases.

## 5. Notes change after review

The approved release notes were scanned, then edited before release creation.

Expect digest mismatch to block creation pending renewed approval where needed
and a fresh public-text review. The final command uses the same reviewed file,
not a retyped heredoc or literal placeholder text.

## 6. Resume after tag publication

The tag was pushed, but release creation timed out or a later CI fix changed HEAD.

Expect reconciliation of the exact existing tag/release before retrying.
Never move/delete the public tag. A changed candidate requires a new version and
profile-test evidence, not reuse of the original test or branch-only CI.

## Deterministic contract checks

Installed gh help confirms every JSON field named in the skill. An isolated Git
fixture demonstrated the old describe glob accepting a backup-suffixed tag while
strict canonical filtering selected the highest valid reachable version.
These checks do not establish a live release or an actual profile build.
