# Guarded Dotfiles Publication Design

## Goal

Keep `benthamite/dotfiles` public and preserve the existing commit-based
workflow while preventing unpublished secrets and other cybersecurity-sensitive
material from reaching GitHub.

The user continues to commit locally as often as useful. Publication becomes an
explicit agent workflow that combines deterministic secret detection with an
exhaustive LLM review, repairs unpublished history when necessary, verifies the
repair, and pushes only the exact commit that passed both reviews.

## Policy decision

The canonical repository remains the existing public repository. There is no
separate private source repository and no generated public snapshot history.
Local commits are private until pushed, and findings in unpublished history are
removed by amending or rebasing those commits before publication.

This preserves public-by-default dotfiles and ordinary public commit history.
It accepts the small operational cost of rewriting unpublished commits when a
pre-publication finding is real.

Routine publication and formal releases remain separate:

- `publish-dotfiles` reviews, repairs, verifies, and pushes the current branch;
  it does not create a version tag or GitHub release.
- `release-dotfiles` retains its versioning, profile-test, tagging, and GitHub
  release workflow, but reuses the same security review and repair machinery
  before publishing the branch and tag.

## Threat model

The primary concern is material that helps a malicious attacker, especially:

- API keys, passwords, private keys, access tokens, cookies, session material,
  and authentication headers;
- credential-bearing URLs, commands, logs, histories, caches, and application
  state;
- plaintext versions of files that are supposed to be encrypted;
- configuration combinations that expose access even though no individual
  line resembles a conventional secret; and
- operational details that materially enable an attack when combined, such as
  an exposed administrative endpoint and usable authentication information.

Ordinary personal information is not a publication blocker. Egregiously
intimate information remains a finding even when it is not a credential. Purely
privacy-oriented observations may be reported separately, but they must not
obscure or inflate the cybersecurity findings.

The controls are designed to prevent accidental publication. They are not a
security boundary against malware or a person who already controls the local
account: such an actor can deliberately bypass local hooks and read the source
material directly.

## User-facing workflows

### Ordinary publication

1. The user commits normally.
2. An ordinary `git push` is refused with a short instruction to invoke
   `publish-dotfiles`.
3. `publish-dotfiles` fetches the public remote and captures the remote tip and
   exact local candidate commit.
4. It runs the deterministic and LLM outgoing-history reviews described below.
5. If either review finds a problem, the skill stays active and enters the
   repair loop. A finding blocks the unsafe push, not the publication workflow.
6. After each rewrite, both review layers run again against the new exact
   candidate.
7. When both layers pass, the workflow authorizes and pushes only that candidate
   to the intended public branch.
8. It verifies that the remote branch points to the candidate and closes the
   normal post-push CI loop. Any CI repair creates a new candidate and must
   re-enter `publish-dotfiles`; it cannot use a direct follow-up push.

Invoking `publish-dotfiles` explicitly authorizes the branch push after all
checks pass. The workflow does not add a redundant final confirmation prompt.

`publish-dotfiles --dry-run` performs discovery and proposes remedies but does
not rewrite commits, create authorization state, or push.

Only an explicit publication invocation authorizes the push. Requests to audit,
check readiness, explain a finding, or use `--dry-run` remain non-publishing.

### Formal release

`release-dotfiles` calls the same outgoing-history review and repair workflow
before its final publication step. It then continues through its existing
profile-test confirmation, version tag, branch/tag push, release-note
publication, and post-release checks.

The authorization for a release names the exact branch and tag updates. Release
notes and any annotated tag message are included in the semantic review because
they are also public output.

Security repair that changes the release candidate invalidates any earlier
profile-test confirmation. The revised candidate must pass the profile test and
both publication reviews before the tag is created. No release flag, including
the existing acceptance shortcut, skips the deterministic or LLM review.

### Full audit

`publish-dotfiles --full-audit` reviews the complete reachable repository and
public GitHub ref surface rather than only unpublished commits. Its scope
includes local branches, remote branches, tags, and fetchable pull-request refs.
Temporary audit refs are placed under a private local namespace and removed
afterward.

An explicitly invoked full audit does not publish the branch or rewrite public
history. When a stale full audit is run as part of an explicit publication, a
clean result returns to that publication workflow; a public finding enters
incident handling before publication continues.

A full audit runs:

- once when the publication system is installed;
- on demand;
- after material scanner-rule changes or a suspected incident; and
- on the next publication when the recorded successful full audit is more than
  30 days old.

The last successful full-audit time and ruleset identity are stored as local
Git metadata, not committed public content. Tying the monthly check to the next
publication avoids a background agent service while ensuring that a repository
that continues to publish cannot indefinitely skip the full audit.

If a full audit finds material that is already public, the workflow does not
treat it as an unpublished rebase problem. It enters the existing incident
workflow: determine exposure, rotate or revoke live credentials first, remove
the source, and clean public history where useful. Resolved historical findings
receive narrow fingerprinted incident records so later audits can distinguish
them from new exposure without embedding a secret in the record.

## Outgoing-history boundary

The routine boundary is the exact remote branch tip through the captured local
candidate. The scan covers every commit, diff, and introduced blob in that
range, including content added in one unpublished commit and deleted in a later
one. It also scans the candidate's complete tree.

Only the captured object ID may be pushed. If `HEAD`, the remote tip, the target
branch, the ruleset, or relevant repository policy changes after review, the
authorization becomes invalid and the scan must run again. A remote update is
handled as a synchronization problem before publication; the skill never
force-pushes over an unexpected remote tip.

Routine publication pushes the current branch only. It does not implicitly push
other branches or tags. Formal release authorizes only its explicitly created
tag in addition to the reviewed branch update.

## Deterministic review

A tracked helper owns the repeatable, non-LLM checks and emits structured,
redacted findings. It combines:

- Gitleaks' built-in rules;
- repository-specific rules derived from the audit, including generic
  credential assignments and service patterns not covered by provider rules;
- risky path and filename policy for logs, histories, cookies, sessions,
  authentication state, private keys, environment files, and similar material;
- checks for credential-bearing URLs and shell commands;
- archive, binary, symlink, and unexpectedly large-file checks appropriate to a
  dotfiles repository; and
- verification of the raw Git blob at every relevant revision for files that
  `.gitattributes` says are encrypted.

The repository-specific Gitleaks configuration extends the upstream defaults;
it does not replace them. Exceptions are narrow, documented, and
version-controlled. A deterministic finding cannot be cleared merely because
the LLM calls it harmless: it must be removed from history or matched by a
specific reviewed exception.

Scanner reports are written to permission-restricted temporary files. Console,
agent, and committed output includes rule, path, commit, line location, and a
non-reversible fingerprint, but never the detected value. Temporary reports are
deleted after use.

## LLM semantic review

The agent running `publish-dotfiles` is the required LLM layer. The deterministic
helper does not embed a second provider-specific LLM API.

For outgoing review, the helper generates an exact manifest of:

- every outgoing commit;
- every path changed by each commit;
- every added and removed patch section;
- complete contents of new files;
- complete contents of files classified as high risk; and
- deterministic findings with redacted nearby context.

The LLM processes the manifest exhaustively. It may chunk a large range, but it
must record completion for every manifest item and may not sample. It looks for
semantic risks that pattern matching often misses, including credentials under
unusual names, session-bearing logs, commands with inline authentication,
secrets split across files, retrieval code that bypasses the repository's
secret stores, and apparently harmless values that become usable access when
combined with other configuration.

The LLM also classifies deterministic findings, selects the applicable repair
policy, and checks that cybersecurity findings are not diluted by irrelevant
privacy observations. Its output uses locations and fingerprints rather than
secret values.

Recognized secret-shaped values are redacted before manifest content enters the
conversation. Semantic review can nevertheless expose a previously unknown
secret to the configured model provider; that residual exposure is inherent in
asking a hosted LLM to inspect source text and is accepted by this design.

After any history rewrite, the manifest is regenerated from the rewritten
range and both review layers repeat. Prior LLM approval is not carried across a
changed object ID or changed ruleset.

## Repair loop

Findings do not terminate the skill. The skill groups them by underlying cause,
identifies every affected unpublished commit, and selects among four remedies:

1. **Ignore:** remove a file that should never be tracked and introduce the
   narrow `.gitignore` rule at the first relevant commit.
2. **Encrypt:** introduce the `.gitattributes` policy before the file's first
   committed version and rewrite every affected blob so no plaintext revision
   remains in the outgoing range.
3. **Externalize:** replace an embedded value with the repository's established
   `pass`, 1Password, environment, or generated-local-file mechanism and rewrite
   all affected commits.
4. **Allow:** add a narrow, reasoned rule/fingerprint exception for a confirmed
   false positive. Broad path or detector suppression is not allowed merely to
   make a scan pass.

When existing repository policy makes the repair unambiguous, the skill applies
it automatically. Examples include removing a tracked log already covered by
ignore policy or repairing a plaintext revision of a file already designated
for encryption. When intent changes behavior or determines whether a file
should be ignored, encrypted, externalized, or allowed, the skill asks one
focused question and then carries out the selected repair itself.

Before rewriting, the skill creates a temporary local recovery reference to the
old candidate. It uses amend/rebase for focused recent changes and a bounded
history-filtering operation when a finding spans many unpublished commits. It
preserves commit messages, authorship, and topology where the chosen tool
permits.

Verification after a rewrite includes:

- a tree comparison proving that only the intended files or lines changed;
- a range comparison of the old and new unpublished histories;
- raw-object verification for ignored and encrypted content;
- confirmation that no public remote ref changed during repair; and
- a fresh deterministic and exhaustive LLM review of the rewritten range.

The recovery reference is removed after a verified successful push. A failed
repair remains recoverable and the skill diagnoses it rather than discarding
the original history.

## Push enforcement

The existing fetch URL remains unchanged. Its ordinary push URL uses a tracked
refusing Git remote helper that prints an instruction to use
`publish-dotfiles`. This also protects against an accidental
`git push --no-verify origin`, which would bypass only the hook and still reach
the refusing transport.

A tracked pre-push hook independently refuses any update without a short-lived
authorization record. The record names:

- the expected remote URL;
- the remote ref and its previously fetched object ID;
- the exact reviewed local object ID;
- the ruleset identity; and
- the permitted update type, either ordinary branch publication or formal
  release branch/tag publication.

The hook compares that record with Git's actual proposed ref updates and
consumes it before allowing the push. A network failure therefore requires a
fresh authorization, and any changed commit or refspec requires a fresh scan.
The publisher uses the real GitHub URL and a fully explicit object-ID-to-ref
refspec.

This is a strong accidental-publication guard, not an unbypassable local access
control. A person can still deliberately supply the real URL and disable Git
hooks. GitHub secret scanning and push protection remain enabled as independent
provider-side backstops.

## Automation shape

This is an agent skill because remediation requires repository discovery,
cross-file judgment, iterative Git operations, and focused user decisions. The
deterministic checks and push authorization live in a normal executable so they
are testable and shared by Claude, Codex, the pre-push hook, and
`release-dotfiles`.

Expected implementation surfaces are:

- paired `publish-dotfiles` skills for Claude and Codex;
- a tracked deterministic publication helper, pre-push hook, and refusing Git
  remote helper;
- a repository-specific Gitleaks configuration;
- tests using disposable repositories and local bare remotes;
- paired skill behavior scenarios covering clean publication, semantic
  findings, repair, false positives, and already-public incidents;
- integration updates to both copies of `release-dotfiles`;
- skill inventories and paired-configuration documentation; and
- local installation of the tracked hook plus the Gitleaks dependency.

The skill bodies remain equivalent except for tool-specific frontmatter. The
helper is the single source of truth for scan scope, redaction, authorization,
and hook validation; the two skills do not reimplement those rules in prose.

## Failure handling

- A finding keeps the publication workflow active and enters repair.
- Missing scanner or hook dependencies are setup failures; the skill installs
  or repairs them within the approved local workflow and reruns the check.
- A dirty worktree is not silently stashed or overwritten. Publication is based
  on committed objects, and history rewriting waits until the worktree can be
  protected without losing user changes.
- Unexpected remote divergence is reconciled before scanning or pushing.
- A scanner crash, incomplete LLM manifest, stale authorization, or uncertain
  encryption state fails closed and is diagnosed. None is converted into a
  warning-only fallback.
- A finding that may already be public enters incident handling before any
  claim that sanitizing local history resolved the exposure.

## Rejected alternatives

### Private canonical repository plus public export

This provides a stronger structural boundary but adds a second repository and
generated history. It is unnecessary for the chosen policy because the user is
comfortable rewriting unpublished commits and wants public commit history to
remain the normal history.

### Generated public branch in the same repository

This avoids rewriting source commits but loses the direct correspondence
between ordinary commits and public history. It adds ceremony without enough
benefit for the accepted threat model.

### Deterministic pre-push scan alone

This misses contextual risks and cannot choose or perform safe remediation.
The user explicitly requires LLM review in addition to deterministic rules.

### LLM review alone

An LLM review is nondeterministic and can skip or overlook material. The exact
manifest, deterministic rules, raw encryption checks, and exact-object push
authorization remain mandatory.

## Non-goals

- Concealing ordinary personal information or benign configuration topology.
- Treating a private repository as a substitute for `pass`, 1Password, or
  proper local secret generation.
- Making local hooks resistant to an attacker who controls the user's account.
- Automatically rewriting history that is already public without first
  handling credential rotation and public incident scope.
- Pushing unrelated branches, tags, package repositories, or external changes
  during ordinary publication.

## Acceptance criteria

- A normal `git push origin ...` cannot update the public repository and points
  to `publish-dotfiles`.
- `publish-dotfiles` scans every commit and blob in the exact outgoing range and
  the candidate tree using deterministic and LLM review.
- A secret introduced and then deleted in separate unpublished commits is
  detected.
- The LLM manifest proves that no outgoing commit or changed path was sampled or
  skipped.
- Reports and agent-visible output never contain detected secret values.
- Unambiguous findings are repaired automatically; ambiguous remedies receive
  one focused user decision and are then implemented by the skill.
- Rewritten history differs only by the approved sanitation, remains
  recoverable until publication succeeds, and passes both reviews again.
- The pre-push hook accepts only a fresh authorization for the exact scanned
  object and exact intended refs.
- A change to `HEAD`, the remote tip, target refs, or ruleset invalidates the
  authorization.
- Routine publication does not create or push tags or GitHub releases.
- `release-dotfiles` reuses the same security checks before its explicitly
  authorized branch/tag publication.
- A full audit covers local and public branches, tags, and fetchable pull-request
  refs, records its successful ruleset and time locally, and becomes due again
  after 30 days or a material ruleset change.
- Findings already present on public refs route to incident handling rather
  than being described as fixed by a local rewrite.
- Tests exercise detection, redaction, repair verification, authorization
  races, direct-push refusal, release integration, and full-audit staleness with
  disposable local remotes and no external publication.
- Skill behavior scenarios verify exhaustive LLM review, continued remediation
  after a finding, one-question handling of ambiguous policy, and correct
  routing of already-public exposure.
- Paired Claude/Codex skills and documentation pass the repository's sync and
  audit checks.
