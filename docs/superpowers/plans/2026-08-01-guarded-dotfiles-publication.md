# Guarded Dotfiles Publication Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an explicit `publish-dotfiles` workflow that blocks ordinary pushes, scans the exact outgoing history with Gitleaks and an exhaustive agent review, helps sanitize unpublished commits, and pushes only the reviewed object.

**Architecture:** A single Python executable owns repository discovery, deterministic scanning, redaction, review manifests, repair state, short-lived push authorization, and full-audit state. A tracked pre-push hook and refusing Git remote helper enforce that executable's authorization protocol. Equivalent project-local Claude and Codex skills drive the LLM review and history-repair loop; `release-dotfiles` calls the same machinery before publishing its branch and tag.

**Tech Stack:** Python 3 standard library, Git 2.55+, Gitleaks 8.29+, POSIX shell, `unittest`, paired Claude/Codex Markdown skills, Org documentation.

---

## Fixed contracts

Implement these names and data boundaries consistently across the tasks below:

- User-facing skill: `publish-dotfiles`.
- Shared executable: `bin/dotfiles-publish`.
- Refusing transport: `bin/git-remote-dotfiles-blocked`.
- Tracked hook: `bin/dotfiles-pre-push`.
- Local state directory: `$(git rev-parse --git-dir)/dotfiles-publish/`, created with mode `0700`.
- Current run files: `runs/RUN_ID/run.json`, `findings.json`, `manifest.json`, and `review.json`, each mode `0600`.
- Fingerprint key: `fingerprint.key`, 32 random bytes generated once with mode `0600` and never committed.
- Authorization file: `authorization.json`, mode `0600`, consumed by the hook before an allowed push.
- Recovery refs: `refs/dotfiles-publish/recovery/RUN_ID`.
- Temporary full-audit refs: `refs/dotfiles-publish/audit/pull/NUMBER/head`.
- Full-audit receipt: `full-audit.json`, containing the reviewed ref tips, ruleset identity, and UTC completion time.
- Resolved-public-incident receipt: `incidents.json`, keyed by local HMAC fingerprint and containing only resolution class, UTC time, and redacted evidence digest.
- Blocked push URL: `dotfiles-blocked::origin`; the fetch URL remains the real GitHub URL.
- Ordinary publication target: the current branch's configured upstream branch on `origin`.
- A review run is immutable. Any candidate, remote-tip, ref, or ruleset change creates a new run rather than modifying an attestation for an old run.

`run.json` must record these exact security inputs:

```json
{
  "schema": 1,
  "mode": "publish",
  "run_id": "sha256-derived identifier",
  "created_at": "UTC RFC3339 time",
  "repository": "absolute repository path",
  "remote_name": "origin",
  "remote_url": "real fetch URL",
  "remote_ref": "refs/heads/master",
  "remote_oid": "40-or-64-character object ID",
  "candidate_oid": "40-or-64-character object ID",
  "ruleset_id": "sha256 digest",
  "manifest_id": "sha256 digest",
  "full_audit_due": false,
  "public_refs": []
}
```

`authorization.json` must record `schema`, `run_id`, `mode`, `created_at`, `expires_at`, `remote_url`, `ruleset_id`, and an exact `updates` array. Each update contains `local_oid`, `local_ref`, `remote_oid`, and `remote_ref`. Ordinary publication has one branch update; a release has that branch update plus one tag update. Set expiry to five minutes after creation.

Persist only redacted finding records. Each record contains `source`, `rule_id`, `path`, `commit`, line and column coordinates when known, `fingerprint`, and redacted context. Compute the fingerprint as the first 20 hexadecimal characters of HMAC-SHA-256 under `fingerprint.key`. For Gitleaks, authenticate its own fingerprint plus the canonical location; for custom detectors, authenticate the sensitive value in memory plus the canonical location. A plain or truncated unkeyed secret hash is forbidden because low-entropy values could be guessed offline. Never persist or print the value itself; Gitleaks must redact its permission-restricted temporary report too.

## Task 1: Pin the behavior with failing integration tests

**Files:**

- Create: `tests/test_dotfiles_publish.py`

- [ ] Add a `unittest.TestCase` fixture that creates a temporary source repository and a local bare remote, configures test author identity, commits a public base, pushes it with `git -c core.hooksPath=/dev/null push`, then creates unpublished commits. Keep every test offline.

- [ ] Add helpers that invoke `bin/dotfiles-publish` with an isolated `PATH`, fixed clock via `DOTFILES_PUBLISH_NOW`, and test-only Gitleaks executable via `DOTFILES_PUBLISH_GITLEAKS`. The fake scanner must accept the production CLI arguments and write controlled JSON to the requested report path.

- [ ] Add failing tests with these exact names:

  - `test_scan_covers_secret_added_then_deleted_before_head`
  - `test_scan_redacts_value_from_stdout_stderr_and_persisted_state`
  - `test_manifest_has_every_commit_path_patch_and_full_new_file`
  - `test_authorize_refuses_incomplete_llm_review`
  - `test_authorization_is_exact_short_lived_and_single_use`
  - `test_changed_head_remote_ref_remote_tip_or_ruleset_invalidates_authorization`
  - `test_pre_push_rejects_without_authorization`
  - `test_refusing_transport_blocks_push_even_with_no_verify`
  - `test_publish_pushes_only_candidate_to_configured_branch`
  - `test_repair_ref_survives_failure_and_is_removed_after_verified_push`
  - `test_git_crypt_path_with_plaintext_historical_blob_is_rejected`
  - `test_full_audit_due_after_thirty_days_or_ruleset_change`
  - `test_full_audit_finding_on_public_ref_is_classified_as_incident`

- [ ] Make assertions directly cover the acceptance criteria. In particular, use the literal test value `DOTFILES_TEST_SECRET_7b4c1fa9e62d` and assert it is absent from captured output and every file under the local state directory.

- [ ] Run the tests and confirm they fail because the executable does not exist:

```bash
python3 -m unittest tests.test_dotfiles_publish -v
```

Expected result: 13 errors or failures whose first root cause is the missing `bin/dotfiles-publish`; no test may reach a network remote.

- [ ] Commit the test contract:

```bash
git add tests/test_dotfiles_publish.py
git commit -m "test: specify guarded dotfiles publication"
```

## Task 2: Build repository discovery and immutable run state

**Files:**

- Create: `bin/dotfiles-publish`
- Modify: `tests/test_dotfiles_publish.py`

- [ ] Add tests for repository discovery from a subdirectory, upstream resolution, non-fast-forward rejection, state permissions, deterministic ruleset identity, and immutable run IDs.

- [ ] Implement `bin/dotfiles-publish` as a Python 3 CLI with these subcommands:

```text
scan [--mode publish|release|full-audit] [--release-notes PATH] [--tag TAG]
review-list --run RUN_ID
review-show --run RUN_ID --unit UNIT_ID
review-record --run RUN_ID --unit UNIT_ID --verdict clean|finding --findings PATH
review-status --run RUN_ID
repair-start --run RUN_ID
repair-verify --run RUN_ID --allowed-path PATH [--allowed-path PATH ...]
incident-record --run RUN_ID --fingerprint FINGERPRINT --resolution rotated|revoked|invalid --evidence PATH
authorize --run RUN_ID [--tag TAG]
push --run RUN_ID [--tag TAG]
hook REMOTE_NAME REMOTE_LOCATION
install
```

- [ ] Use `argparse`, `dataclasses`, `hashlib`, `hmac`, `json`, `pathlib`, `secrets`, `stat`, `subprocess`, `tempfile`, and `time`; add no Python package dependency.

- [ ] Centralize Git execution in `run_git(repo, *args, input_bytes=None, check=True)`. Always pass an argument vector and never invoke a shell. Return bytes when object content is requested and UTF-8 text with replacement only for metadata.

- [ ] Resolve the repository with `git rev-parse --show-toplevel` and the separate Git directory with `git rev-parse --absolute-git-dir`. Resolve the current symbolic branch, its upstream, the real fetch URL, the remote-tracking tip, and `HEAD`. Fetch `origin` before capturing the boundary in live publish and release mode; tests use the local bare remote.

- [ ] Require the remote tip to be an ancestor of the candidate with `git merge-base --is-ancestor`. Treat unexpected divergence as a synchronization failure; never force-push.

- [ ] Compute `ruleset_id` over bytes and relative names of `.gitleaks.toml`, `.gitignore`, `.gitattributes`, `bin/dotfiles-publish`, and the fixed schema version. A missing policy file contributes an explicit `MISSING` marker. This makes a policy change invalidate prior review.

- [ ] Create `fingerprint.key` with `secrets.token_bytes(32)` and an exclusive mode-`0600` create the first time local state is initialized. Refuse a missing, short, symlinked, group-readable, or world-readable existing key instead of silently replacing it and changing incident identities.

- [ ] Derive `run_id` from mode, repository identity, remote URL/ref/OID, candidate OID, ruleset ID, release tag, and release-notes digest. Write JSON atomically through a sibling temporary file, `fsync`, mode `0600`, and rename. Refuse to overwrite a different object at an existing run ID.

- [ ] Honor `DOTFILES_PUBLISH_NOW` only when `DOTFILES_PUBLISH_TESTING=1`; otherwise use current UTC. This supports expiry tests without creating a production clock bypass.

- [ ] Run the focused state tests until green while the scan tests still fail for missing scanner behavior:

```bash
python3 -m unittest tests.test_dotfiles_publish.DotfilesPublishStateTests -v
```

Expected result: all state tests pass.

- [ ] Commit:

```bash
git add bin/dotfiles-publish tests/test_dotfiles_publish.py
git commit -m "feat: model guarded publication state"
```

## Task 3: Add deterministic outgoing-history and tree scanning

**Files:**

- Create: `.gitleaks.toml`
- Modify: `bin/dotfiles-publish`
- Modify: `tests/test_dotfiles_publish.py`

- [ ] Add `[extend] useDefault = true` to `.gitleaks.toml`, then add repository rules for generic credential assignments, authorization and cookie headers, credential-bearing URLs, shell commands that inline tokens or passwords, PEM private-key headers, and common session/cookie exports. Use `secretGroup` so Gitleaks fingerprints only the sensitive value. Add no initial allowlist. A later false-positive repair may add only the exact rule/path or fingerprint exception demonstrated by its behavior scenario; global path suppression is forbidden.

- [ ] Add a config test that runs `gitleaks git --config .gitleaks.toml --log-opts=--all --no-banner --redact=100 --report-format json` in the disposable repository and proves both an upstream default detector and one repository-specific rule fire. Skip only this live-binary test when Gitleaks is absent; the fake-scanner tests remain mandatory.

- [ ] Implement the outgoing commit list as `git rev-list --reverse REMOTE_OID..CANDIDATE_OID`. Scan the complete patch range with:

```text
gitleaks git --config CONFIG --no-banner --redact=100 --report-format json --report-path REPORT --log-opts=REMOTE_OID..CANDIDATE_OID REPOSITORY
```

Accept Gitleaks exit code 0 as clean and 1 as findings; treat every other code as a scanner failure. Capture its output and redact it again before any error is reported.

- [ ] Export the exact candidate tree with `git archive --format=tar CANDIDATE_OID`, extract it into a mode-`0700` temporary directory with traversal checks, and scan it with `gitleaks dir` using the same config/report/redaction flags. This catches secrets inherited from history that are not present in outgoing patches.

- [ ] For every outgoing commit, enumerate changed paths with `git diff-tree --root --no-commit-id --name-status -r -z COMMIT`. Add deterministic findings for risky names (`.env`, history, log, cookie, session, credential, auth-state, private-key patterns), symlinks, archives, binary files, and blobs larger than 2 MiB. Large or binary files are findings requiring inspection, not silently skipped content.

- [ ] For every path at every outgoing commit and the candidate tree, resolve the `filter` attribute using `git check-attr --source=COMMIT -z filter --stdin`. If the value is `git-crypt`, read the raw blob with `git cat-file blob COMMIT:PATH` and require the `NUL + GITCRYPT + NUL` header. Do not inspect the smudged working-tree copy for this check.

- [ ] Parse Gitleaks JSON only inside a `TemporaryDirectory` created under the local state directory with mode `0700`. Convert each result to the redacted finding schema, then delete the raw report before returning. Sanitize scanner exception strings and captured process output with both known detected values and structural secret patterns.

- [ ] Store redacted findings only. Print one line per finding containing source, rule, path, commit abbreviation, location, and fingerprint. Return exit code 2 for findings, 3 for setup/scanner failure, and 0 for a clean deterministic scan.

- [ ] Run:

```bash
python3 -m unittest \
  tests.test_dotfiles_publish.DotfilesPublishScanTests \
  tests.test_dotfiles_publish.DotfilesPublishRedactionTests \
  -v
```

Expected result: all deterministic scan, intermediate-secret, raw-git-crypt, and redaction tests pass.

- [ ] Commit:

```bash
git add .gitleaks.toml bin/dotfiles-publish tests/test_dotfiles_publish.py
git commit -m "feat: scan outgoing dotfiles history"
```

## Task 4: Generate an exhaustive LLM manifest and enforce attestation

**Files:**

- Modify: `bin/dotfiles-publish`
- Modify: `tests/test_dotfiles_publish.py`

- [ ] Define stable manifest unit kinds: `commit`, `path`, `patch`, `new-file`, `high-risk-file`, `deterministic-finding`, and `public-text`. Unit IDs are SHA-256 over kind, commit, path, and ordinal. `manifest.json` contains `schema`, `run_id`, `manifest_id`, `units`, and exact counts by kind.

- [ ] For each outgoing commit, add one `commit` unit with author, date, subject, and parents; one `path` unit per changed path; one `patch` unit per added and removed hunk; full blob content for every new regular file; and full blob content for every risky-path regular file. Add one redacted unit per deterministic finding. In release mode add the complete release notes and tag text as `public-text` units.

- [ ] Generate patches from committed objects with `git diff-tree --root --find-renames --format= --patch COMMIT`; do not read the working tree. Split every hunk into a separate unit while retaining file path and old/new line coordinates.

- [ ] Before writing a manifest unit, replace every known detected value with `[REDACTED:FINGERPRINT]` and apply structural redaction for authorization headers, private-key bodies, credential URLs, and credential assignments. Set the state directory and files to modes `0700` and `0600`. Unknown semantic risks remain visible to the configured model by design.

- [ ] Implement `review-list` as a content-free list of unit IDs, kinds, commits, and paths. Implement `review-show` to return exactly one unit. This forces the skill to process bounded units rather than dumping an unbounded report.

- [ ] Implement `review-record` so it accepts only an existing unit and writes a verdict keyed by unit ID. A finding file must use the same redacted finding schema and is rejected if it includes a value field or any scanner-known value. Re-recording a unit is idempotent only when the verdict and finding digest match.

- [ ] Implement `review-status` to compare the exact set of manifest unit IDs with the exact set of recorded unit IDs. It reports counts and missing IDs without content. A review is clean only when every unit is recorded and no LLM or deterministic finding remains unresolved.

- [ ] Extend tests to prove that a range with two commits, a modified file, a file added then deleted, a new file, and a risky file produces every expected unit. Delete one review entry and prove `authorize` fails with the missing unit ID. Add an extra forged entry and prove it also fails.

- [ ] Run:

```bash
python3 -m unittest tests.test_dotfiles_publish.DotfilesPublishManifestTests -v
```

Expected result: manifest coverage and attestation tests pass, including incomplete and extra-unit rejection.

- [ ] Commit:

```bash
git add bin/dotfiles-publish tests/test_dotfiles_publish.py
git commit -m "feat: require exhaustive publication review"
```

## Task 5: Add recoverable history-repair state and verification

**Files:**

- Modify: `bin/dotfiles-publish`
- Modify: `tests/test_dotfiles_publish.py`

- [ ] Implement `repair-start --run RUN_ID` to require that the run has a blocking finding, create `refs/dotfiles-publish/recovery/RUN_ID` at the original candidate using `git update-ref`, and write a repair record containing the original candidate, tree, remote tip, and finding locations. Refuse to overwrite a recovery ref that points elsewhere.

- [ ] Implement `repair-verify` after the agent has amended, rebased, or filtered unpublished commits. Require the remote tip to be unchanged and the new `HEAD` to descend from it. Produce a redacted comparison using `git range-diff REMOTE_OID..OLD REMOTE_OID..NEW` and `git diff --stat OLD NEW`.

- [ ] Require one or more repeated `--allowed-path` arguments. Compare all changed paths between old and new candidate trees with that allowlist and fail if any other path changed. For line-level externalization or allowlist edits, the skill must inspect the complete diff and record its approval before starting a new scan.

- [ ] Never carry review records to the new candidate. `repair-verify` returns the new candidate and instructs the caller to create a new `scan` run. Preserve the recovery ref until a verified successful push; keep it on every failure.

- [ ] Add tests for allowed-path enforcement, remote-tip races, review invalidation after rewrite, recovery-ref retention on failure, and cleanup only after remote verification.

- [ ] Run:

```bash
python3 -m unittest tests.test_dotfiles_publish.DotfilesPublishRepairTests -v
```

Expected result: all repair and recovery tests pass.

- [ ] Commit:

```bash
git add bin/dotfiles-publish tests/test_dotfiles_publish.py
git commit -m "feat: verify dotfiles history sanitation"
```

## Task 6: Enforce exact, expiring, single-use push authorization

**Files:**

- Modify: `bin/dotfiles-publish`
- Create: `bin/dotfiles-pre-push`
- Create: `bin/git-remote-dotfiles-blocked`
- Modify: `tests/test_dotfiles_publish.py`

- [ ] Make `bin/dotfiles-pre-push` a minimal POSIX wrapper that resolves the repository root and `exec`s `bin/dotfiles-publish hook "$@"`. It must pass Git's proposed updates on standard input unchanged.

- [ ] Make `bin/git-remote-dotfiles-blocked` print exactly `Push blocked: use publish-dotfiles so outgoing history is reviewed.` to standard error and exit 1 without implementing the remote-helper protocol.

- [ ] Implement `authorize` to require a clean deterministic result, exhaustive clean LLM review, unchanged candidate/remote/ref/ruleset, and a non-stale full audit. Build the exact updates array from the current upstream and, in release mode, the requested tag. Write a five-minute authorization atomically.

- [ ] Implement `hook` to parse every standard-input line as `LOCAL_REF LOCAL_OID REMOTE_REF REMOTE_OID`, read and unlink the authorization before deciding, and require exact equality with the proposed update set, remote URL, ruleset, and current time. Reject deletions, wildcard updates, extra refs, expired records, changed local objects, and changed old remote objects.

- [ ] Implement `push` to revalidate the run, call the same internal authorization function used by `authorize`, and immediately call Git with the real fetch URL and explicit object-ID refspecs. The skill uses this combined command; standalone `authorize` exists for hook diagnostics and integration tests, not as an extra routine user step.

```text
git push REAL_URL CANDIDATE_OID:REMOTE_REF
git push REAL_URL CANDIDATE_OID:REMOTE_REF TAG_OID:refs/tags/TAG
```

Use the first form for ordinary publication and the second for a release. Never use `--all`, `--tags`, `--follow-tags`, a force option, or the blocked named remote.

- [ ] After Git returns success, verify the exact refs with `git ls-remote REAL_URL REMOTE_REF` and, for release, the tag ref. Only then delete the recovery ref and mark the run published. A network failure consumes authorization but retains the recovery ref and requires a fresh `authorize` call.

- [ ] Add integration tests against the local bare remote for successful branch publication, release branch-plus-tag publication, extra-tag refusal, expiry, replay, candidate race, remote race, ruleset race, and exact remote verification.

- [ ] Run:

```bash
python3 -m unittest tests.test_dotfiles_publish.DotfilesPublishAuthorizationTests -v
```

Expected result: every exact-authorization and local-push test passes.

- [ ] Commit:

```bash
git add bin/dotfiles-publish bin/dotfiles-pre-push bin/git-remote-dotfiles-blocked tests/test_dotfiles_publish.py
git commit -m "feat: authorize exact dotfiles pushes"
```

## Task 7: Install the push guard without disturbing existing hooks

**Files:**

- Modify: `bin/dotfiles-publish`
- Modify: `tests/test_dotfiles_publish.py`

- [ ] Implement `install` to verify the repository is `benthamite/dotfiles`, the tracked helper files are executable, Gitleaks is available, and `bin` is present on `PATH` so Git can find `git-remote-dotfiles-blocked`.

- [ ] Inspect the existing `pre-push` hook before changing it. Replace only a missing hook, a symlink to `bin/dotfiles-pre-push`, or the exact temporary bootstrap blocker installed during design. Refuse any other existing content with a clear merge instruction; never overwrite an unrelated hook. Symlink the accepted hook path to the tracked `bin/dotfiles-pre-push` and leave `post-commit`, `post-rewrite`, and every other hook untouched.

- [ ] Preserve `remote.origin.url` and set only `remote.origin.pushurl` to `dotfiles-blocked::origin`. Record the real fetch URL in local state for diagnostics, but always resolve it again when starting a run.

- [ ] Add install tests proving unrelated hooks retain byte-identical content, an unknown pre-push hook is refused, rerunning install is idempotent, the named remote blocks `git push --no-verify`, and an explicitly authorized URL push still passes through the tracked pre-push hook.

- [ ] Run:

```bash
python3 -m unittest tests.test_dotfiles_publish.DotfilesPublishInstallTests -v
```

Expected result: all installation and direct-push refusal tests pass.

- [ ] Commit:

```bash
git add bin/dotfiles-publish tests/test_dotfiles_publish.py
git commit -m "feat: install dotfiles push guard"
```

## Task 8: Implement complete-history audits and incident routing

**Files:**

- Modify: `bin/dotfiles-publish`
- Modify: `tests/test_dotfiles_publish.py`

- [ ] In `scan --mode full-audit`, fetch branch and tag refs normally, enumerate fetchable pull-request heads with `git ls-remote REAL_URL refs/pull/*/head`, and fetch each exact object into `refs/dotfiles-publish/audit/pull/NUMBER/head`. Tests use the same mechanism against a local bare remote, so this path needs neither a GitHub API session nor an external test double.

- [ ] Enumerate all unique commits reachable from local branches, remote branches, tags, and temporary PR refs with `git rev-list --topo-order --reverse`. Run Gitleaks over the complete reachable surface and create manifest units for every unique commit and changed path, deduplicated by object ID and unit ID. Scan every unique ref-tip tree.

- [ ] Classify a finding as `public-incident` when its containing commit is reachable from a fetched public branch, tag, or PR ref. Do not offer unpublished rebase remediation for that class. Emit a redacted handoff to `gitguardian-triage` that starts with rotation or revocation, then source removal, then public-history cleanup when useful.

- [ ] Implement `incident-record` for a public finding only after triage. Accept only resolution classes `rotated`, `revoked`, or `invalid`; require a redacted evidence JSON file containing the fingerprint, action time, and verification summary; reject any scanner-known value or a `value` field. Store only its SHA-256 digest and metadata in `incidents.json`. A still-present historical finding is resolved for audit purposes only when its exact HMAC fingerprint has this receipt; a path-wide or rule-wide incident exception is forbidden.

- [ ] Remove only refs under `refs/dotfiles-publish/audit/` in a `finally` path. Never delete recovery refs. Record `full-audit.json` only after every deterministic finding is absent or matched by an exact resolved-incident receipt and every full-audit manifest unit has a clean LLM attestation.

- [ ] Define staleness as no receipt, a receipt older than 30 days, a changed ruleset ID, or an incident flag written by the triage path. Record public ref tips as audit evidence, but do not make an ordinary branch advance invalidate the receipt: that advance has its own exhaustive outgoing review. `authorize` must refuse with `full audit required`; an explicitly invoked publication skill then runs the audit and resumes publication.

- [ ] Add tests for 29-day validity, 31-day staleness, ruleset and incident-flag invalidation, continued validity after an incrementally reviewed branch advance, exact-fingerprint incident resolution, rejection of broad or value-bearing incident records, temporary-ref cleanup after success and scanner crash, public incident classification, and no publication side effect from `--mode full-audit`.

- [ ] Run:

```bash
python3 -m unittest tests.test_dotfiles_publish.DotfilesPublishFullAuditTests -v
```

Expected result: all cadence, coverage, cleanup, and incident-routing tests pass.

- [ ] Commit:

```bash
git add bin/dotfiles-publish tests/test_dotfiles_publish.py
git commit -m "feat: audit complete dotfiles history"
```

## Task 9: Create and behavior-test the paired publication skill

**Files:**

- Create: `.claude/skills/publish-dotfiles/SKILL.md`
- Create: `.claude/skills/publish-dotfiles/evals/scenarios.md`
- Create: `.codex/skills/publish-dotfiles/SKILL.md`
- Create: `.codex/skills/publish-dotfiles/evals/scenarios.md`
- Modify: `bin/ai-config-sync`
- Modify: `tests/test_ai_config_sync_audit.py`
- Modify: `README.org`

- [ ] Before creating the skill, use the required `superpowers:writing-skills` RED phase. Give fresh subagents the seven prompts below without access to the new skill, in disposable copies of a local repository. Record whether they skip commits, stop at a finding, reveal test-secret values, push without exact authorization, create a tag during ordinary publication, or misroute a public incident.

  1. Publish a clean branch with three outgoing commits and no tag.
  2. Publish a branch where a token is added in commit one and deleted in commit two.
  3. Handle a credential-shaped configuration where ignore, encrypt, and externalize would produce materially different behavior.
  4. Handle a deterministic false positive that should receive one narrow exception.
  5. Handle a valid credential found on an already-public tag.
  6. Check publication readiness without publishing.
  7. Prepare a dotfiles release after the profile was tested, then discover a finding that requires rewriting the release commit.

Expected RED evidence: at least one baseline run violates an exhaustive-review, continue-to-repair, exact-push, ordinary-no-tag, or public-incident requirement. If all baseline runs unexpectedly comply, strengthen the scenarios with time pressure and an explicit request to skip ceremony, then rerun before writing the skill.

- [ ] Add `publish-dotfiles` to `DOTFILES_PROJECT_LOCAL_SKILLS` in `bin/ai-config-sync`. Add a test in `tests/test_ai_config_sync_audit.py` proving that the project-local Claude/Codex pair and its evaluation file are recognized and compared after frontmatter normalization.

- [ ] Write equivalent skill bodies with this trigger-only description:

```yaml
description: Use when publishing, pushing, checking publish readiness, or running a full security audit of the public dotfiles repository, including when an outgoing finding requires sanitizing unpublished commits. Do not use for standalone Emacs package releases.
```

- [ ] Keep the body operational and concise. It must:

  - distinguish explicit publication, readiness or `--dry-run`, and `--full-audit` before any side effect;
  - install or repair Gitleaks and the tracked push guard during an explicit publication when missing, but only report missing setup during readiness or `--dry-run`;
  - run an initial or stale full audit before publication, then resume the same explicit publication only after that audit is clean;
  - call `bin/dotfiles-publish scan` and continue after exit 2 rather than aborting;
  - iterate every ID from `review-list`, read each unit with `review-show`, reason about attacker-useful risk, and record every verdict;
  - report locations and fingerprints, never secret values;
  - start repair state before rewriting and use the focused remedy order ignore, encrypt, externalize, allow;
  - apply unambiguous policy itself and ask exactly one focused question only when the remedy changes user intent;
  - protect a dirty worktree and never silently stash, reset, or overwrite it;
  - run `repair-verify`, scan the rewritten range from scratch, and re-review every new manifest unit;
  - invoke `gitguardian-triage` for already-public credentials before describing local removal as remediation;
  - treat attacker-useful information as blocking, ordinary personal information as non-blocking, and egregiously intimate information as a finding;
  - call `push` only for an explicit publication request and then use `post-push-ci`;
  - route every CI-fix commit back through a fresh publication run;
  - treat scanner crashes, incomplete manifests, uncertain encryption state, and stale authorization as closed failures requiring diagnosis, never warning-only fallbacks;
  - never create a tag or GitHub release during ordinary publication.

- [ ] Put the seven exact prompts, setup facts, expected decisions, forbidden behaviors, and observable pass criteria in both `evals/scenarios.md` files.

- [ ] Add `publish-dotfiles` to the root README Skills section with one cyber-security-focused sentence.

- [ ] Run the GREEN phase with fresh subagents that have access to the new skill and the same disposable scenarios. Require all seven scenarios to meet their observable criteria. Tighten the skill and rerun any failure; do not relax a scenario to match the implementation.

- [ ] Verify discovery and pair equivalence:

```bash
bin/agent-skill path publish-dotfiles --tool claude
bin/agent-skill path publish-dotfiles --tool codex
bin/ai-config-sync audit
```

Expected result: both commands resolve the project-local skill, and the sync audit exits 0.

- [ ] Commit:

```bash
git add .claude/skills/publish-dotfiles .codex/skills/publish-dotfiles bin/ai-config-sync tests/test_ai_config_sync_audit.py README.org
git commit -m "feat: add guarded dotfiles publication skill"
```

## Task 10: Integrate formal releases without weakening either gate

**Files:**

- Modify: `.claude/skills/release-dotfiles/SKILL.md`
- Modify: `.codex/skills/release-dotfiles/SKILL.md`
- Create: `tests/test_release_dotfiles_publication.py`

- [ ] Write failing structural tests that load both skill files and require: a `publish-dotfiles` dependency, release-mode scan after profile confirmation, scan restart after history repair, semantic review of release notes, exact authorized branch-plus-tag push, and no remaining `git push origin`, `--follow-tags`, `--tags`, or force-push command.

- [ ] Update both release skills equivalently. Keep the existing package-repository checks, version choice, lockfile commit, user profile test, and final release confirmation. After profile confirmation and before tag creation, invoke the publication workflow in release-preparation mode with the release notes file and intended lightweight tag.

- [ ] If publication review repairs history, invalidate the earlier profile confirmation, remove any unpushed release tag, rerun the lockfile/profile-test gate, and then create a fresh scan for the new candidate. No `--accept` behavior may skip either security review.

- [ ] After a clean exhaustive review, create the lightweight tag and call the combined authorization-and-push command:

```bash
bin/dotfiles-publish push --run "$RUN_ID" --tag "$NEW_VERSION"
```

Only after remote branch and tag verification may the skill create the GitHub release from the already-reviewed notes.

- [ ] Run:

```bash
python3 -m unittest tests.test_release_dotfiles_publication -v
bin/ai-config-sync audit
```

Expected result: structural integration tests and paired sync audit pass.

- [ ] Commit:

```bash
git add .claude/skills/release-dotfiles/SKILL.md .codex/skills/release-dotfiles/SKILL.md tests/test_release_dotfiles_publication.py
git commit -m "feat: guard formal dotfiles releases"
```

## Task 11: Document, install, and verify the live non-publishing path

**Files:**

- Modify: `agents/README.org`
- Modify: `README.org`
- Modify: `tests/test_dotfiles_publish.py`

- [ ] Document `publish-dotfiles` in the master skill inventory as project-local to dotfiles. Document the two-layer review, ordinary push refusal, local-only authorization state, monthly-on-next-publication full audit, and the difference between ordinary publication and formal release.

- [ ] Add a test asserting that the master inventory and root README both name the project-local skill and that the documented executable and hook paths exist.

- [ ] Install Gitleaks through Homebrew and record the actual version in the verification output:

```bash
brew install gitleaks
gitleaks version
```

Expected result: `gitleaks version` exits 0 and reports version 8.29 or newer.

- [ ] Make the tracked executables executable and install the live guard:

```bash
chmod 755 bin/dotfiles-publish bin/dotfiles-pre-push bin/git-remote-dotfiles-blocked
bin/dotfiles-publish install
```

Expected result: the temporary bootstrap pre-push file is replaced by a symlink to `bin/dotfiles-pre-push`, existing `post-commit` and `post-rewrite` hooks are unchanged, the fetch URL is unchanged, and `remote.origin.pushurl` is `dotfiles-blocked::origin`.

- [ ] Verify the ordinary path is blocked without contacting GitHub:

```bash
git push --dry-run origin HEAD:refs/heads/master
git push --no-verify --dry-run origin HEAD:refs/heads/master
```

Expected result: both exit nonzero and print the `publish-dotfiles` instruction from the refusing transport.

- [ ] Run a live non-publishing review against the current branch through the skill with `--dry-run`. It must perform deterministic scanning and exhaustive manifest review, disclose no known secret value, create no authorization, rewrite no commit, and push nothing.

- [ ] Run the complete offline suite and pair audit:

```bash
python3 -m unittest \
  tests.test_dotfiles_publish \
  tests.test_release_dotfiles_publication \
  -v
bin/ai-config-sync audit
git status --short
```

Expected result: all tests pass, sync audit exits 0, and only the intended documentation/test changes are present before the final commit.

- [ ] Commit:

```bash
git add agents/README.org README.org tests/test_dotfiles_publish.py
git commit -m "docs: explain guarded dotfiles publication"
```

## Task 12: Final acceptance verification

**Files:**

- Verify only; modify a prior task's files if a check exposes a defect, and amend that task's commit.

- [ ] Run every direct acceptance check in a fresh disposable repository and local bare remote:

```bash
python3 -m unittest tests.test_dotfiles_publish tests.test_release_dotfiles_publication -v
```

Expected result: all tests pass with no skipped fake-scanner test; the live Gitleaks test also passes now that the dependency is installed.

- [ ] Run the repository-wide paired-artifact audit and skill discovery:

```bash
bin/ai-config-sync audit
bin/agent-skill path publish-dotfiles --tool claude
bin/agent-skill path publish-dotfiles --tool codex
```

Expected result: audit exits 0 and both resolvers print project-local absolute paths.

- [ ] Inspect the complete implementation diff and commit sequence:

```bash
git diff HEAD~11..HEAD --check
git log --oneline --reverse HEAD~11..HEAD
git status --short
```

Expected result: no whitespace errors, each commit has one logical purpose, and the worktree is clean.

- [ ] Re-run the two live direct-push dry runs and confirm both remain blocked. Do not perform a real GitHub push as part of implementation verification.

- [ ] Invoke the actual `publish-dotfiles --dry-run` skill once more against the final implementation. Confirm it reviews the final exact candidate and remains non-mutating and non-publishing. This is the user-visible end-to-end check; unit tests alone are not sufficient for completion.

- [ ] Search the implementation and documentation for unsafe publication commands and leaked test values:

```bash
rg -n "git push origin|--follow-tags|git push .*--tags|git push .*--force|DOTFILES_TEST_SECRET_7b4c1fa9e62d" \
  bin .claude/skills/publish-dotfiles .codex/skills/publish-dotfiles \
  .claude/skills/release-dotfiles .codex/skills/release-dotfiles \
  README.org agents/README.org
```

Expected result: no unsafe push command appears; the test secret appears only inside `tests/test_dotfiles_publish.py`, outside this search scope.

- [ ] Report separately: deterministic scan result, exhaustive LLM unit counts, direct-push refusal, full-audit status, paired-skill audit, and the fact that no external publication occurred.
