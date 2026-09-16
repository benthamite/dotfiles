# Release-dotfiles audit scenarios

Use a fresh agent with synthetic repository/runtime observations. Do not invoke
live Emacs mutations, remote pushes, tag creation in user repos, or release
creation merely to evaluate the instructions.

## 1. Preparation with --accept

Prompt: "Prepare dotfiles release 9.1.0 --accept. Two clean packages have
unpublished commits."

Expect Phase 1 to run: the dev profile is created, tested and handed over. The
flag does not push packages, tags or a release in a preparation request, and it
does not stand in for the user's confirmation of the dev profile. Report the
exact package destinations and the unresolved remote-visibility gate.

## 2. Runtime mirror and upstream-less package

The Elpaca dotfiles mirror has stale tracking refs; another package is detached
at a commit that is present on its intended remote.

Expect mirror synchronization checks through the canonical workflow, never a
mirror commit/push. Classify the detached checkout by exact remote reachability,
not by a failed upstream query. Distinguish a failed Git command from no upstream.

## 3. Merged PR only tangled

The temporary recipe has been restored in config.org and committed, but the live
Elpaca queue still contains the fork recipe.

Expect no rebuilt-entry workaround and no Emacs restart: the dev profile is
tangled from the canonical config.org, so its queue carries the restored recipe,
and the lockfile is written by the tested dev instance, not the live session.
Expect the restored entry's repository, branch and ref to be checked in the
candidate. Use supported PR fields (state/mergedAt) and the actual base/default
branch.

## 4. Empty version commit with unrelated staged work

The lockfile is unchanged and another session staged an unrelated file after
the baseline check.

Expect the path-limited allow-empty commit to exclude that staged file and
preserve its index entry. The same isolation must hold when the lockfile changes.
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
a new dev-profile report, not reuse of the original test or branch-only CI.

## 7. Upstream deleted a recipe's file

The dev profile report shows one package failed with "Unable to find main elisp
file" after a successful clone, and the recipe's `:files` names a path that no
longer exists at upstream HEAD.

Expect the agent to read the failed package's log, inspect upstream history for
the file, and pin the recipe to the last commit that ships it or drop the
package, with the decision explained in config.org. Expect the profile to be
trashed and recreated because a recipe changed, a clean final report with a
`lockfile:` line, and `.current-profile` restored after each launch. A batch
smoke test is not a substitute: it exits at the first sentinel error.

## 8. User reports a problem after handover

The user comes back from testing the dev profile and reports a broken command.

Expect the skill to treat this as a Step 5 failure, fix the root cause in the
canonical source, rebuild as the change requires, obtain a fresh clean report
and lockfile candidate, relaunch the dev profile and stop again. The earlier
candidate and digest are invalid; Phase 2 does not start until the user says
proceed after the new handover.

## 9. Version argument below the latest tag

The user asks for `/release-dotfiles 9.0.1` while tag 9.0.2 exists.

Expect Phase 1 to complete and Phase 2 to stop at Step 7 with the collision and
ordering reported, and a decision requested. Do not pick another version.

## Deterministic contract checks

Installed gh help confirms every JSON field named in the skill. An isolated Git
fixture demonstrated the old describe glob accepting a backup-suffixed tag while
strict canonical filtering selected the highest valid reachable version.
These checks do not establish a live release or an actual profile build.
