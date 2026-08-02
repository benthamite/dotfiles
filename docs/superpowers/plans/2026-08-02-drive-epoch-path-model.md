# Epoch External Repository Path Model Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Introduce a mixed-state Epoch path model in which notes remain in Drive and each code repository has one explicit current path that can flip safely during its own migration transaction.

**Architecture:** Notes and repository paths become separate explicit fields. The committed registry initially preserves every live old path; tests support a mix of old and external entries without probing either location. Each plan-6 transaction flips only its moved slugs and exact consumers, while live guards, hooks, trust entries, and launch jobs remain on their current working paths until their owner moves.

**Tech Stack:** Python 3, JSON, Make, Bash, launchd plists, paired Claude/Codex skills, `unittest`, Git.

---

This is program plan 5 of 12. It adds mixed-state schema and inactive cutover
support but does not flip a live repository path or scheduler. The resulting commits will make some repositories locally ahead;
the migration plan must stop until those commits are present on advertised
remotes. No push is implicit.

## File map

Shared Epoch root:

- Modify: `Epoch/projects/shared/scripts/build_project_registry.py`
- Modify: `Epoch/projects/shared/tests/test_project_registry.py`
- Modify: `Epoch/projects/shared/project-registry.json`
- Create: `Epoch/projects/shared/repo-paths.json` — exact mixed-state slug-to-current-path map, initially all old roots.
- Modify: `Epoch/projects/shared/scripts/check_project_repos.py`
- Create: `Epoch/projects/shared/tests/test_check_project_repos.py`
- Modify: `Epoch/projects/shared/githooks/pre-commit` — resolve the dashboard through the registry's exact current entry.
- Create: `Epoch/projects/shared/tests/test_pre_commit_hook.py`
- Modify: `Epoch/README.org`, `Epoch/AGENTS.md`, `Epoch/CLAUDE.md`, `Epoch/.gitignore`, `Epoch/scripts/check-retired-slack-refs.sh`

Automations Dashboard and Time Tracker:

- Modify: `Epoch/projects/automations-dashboard/repo/{Makefile,scripts/import_catalog.py,scripts/collect_activity.py,scripts/refresh_dashboard.sh,tests/test_import_catalog.py,tests/test_collect_activity.py,README.md,data/automations.json}`
- Create: `Epoch/projects/automations-dashboard/repo/{scripts/render_launchd_plist.py,tests/test_render_launchd_plist.py}`; inspect the current live plist without changing its path.
- Modify: `Epoch/projects/time-tracker/repo/{Makefile,time_tracker/sources.py,scripts/generate_daily_report.py,scripts/serve_reports.py,scripts/update_current_report.py,tests/test_pipeline.py,tests/fixtures/project_registry.json,README.md}`
- Delete after tests transfer ownership: `Epoch/projects/time-tracker/repo/scripts/install_launch_agents.py`
- Create: `repos/launchd/bin/render-epoch-job-paths.py`, `repos/launchd/tests/test_render_epoch_job_paths.py`; modify `repos/launchd/registry/jobs.json`, `repos/launchd/tests/test_launchd_audit.py` to support an explicit rendered root; inspect both canonical and loaded Time Tracker plists without changing their current paths.

Other consumers:

- Modify paired Epoch skills: `automation-project`, `new-project`, `rename-project`, `ahrefs-api`, `automation-operation`, `epoch-website`, `gdocs-addon`, `media-mentions-rescore`, `routing-guide-update`, `slack-command`, `team-identity-reconciliation`.
- Modify paired dashboard skills: `audit-project-shareability`, `share-project-repos`, and their canonical scripts/tests.
- Modify: `Epoch/projects/modeling-impact/.claude/skills/estimate-impact/SKILL.md`, `Epoch/projects/modeling-impact/.codex/skills/estimate-impact/SKILL.md`, `Epoch/projects/agent-readiness/AGENTS.md`, `Epoch/projects/agent-readiness/CLAUDE.md` — consume the registry/current-path contract.
- Modify: `dotfiles/claude/hooks/block-github-write-command.sh`, `dotfiles/codex/hooks/block-github-write-command.sh`, `dotfiles/tests/test_github_write_guard.py`, `dotfiles/tests/test_github_write_guard_paths.py` — accept one configured exact dashboard path while preserving the live old value.
- Inspect only: `dotfiles/codex/config.toml`; trust keys flip in plan 6 with their repositories.
- Move workflow ownership: delete `Epoch/.github/workflows/team-identity-drift.yml`; create `.github/workflows/team-identity-drift.yml` in the `staff-data` repository.

### Task 1: Split notes and repository roots in the shared registry

**Files:** shared registry builder, checker, tests, generated JSON

- [ ] **Step 1: Add failing registry tests**

Add a mixed fixture with notes under `/drive/Epoch/projects/{foo,bar}`, code
for `foo` at `/home/repos/epoch/foo`, and code for `bar` at
`/drive/Epoch/projects/bar/repo`. Require:

```python
self.assertEqual(["~/repos/epoch/foo"], record["repo_paths"])
self.assertEqual(
    ["~/My Drive/Epoch/projects/bar/repo"],
    records["bar"]["repo_paths"],
)
```

Test missing declared checkout, an external entry with recreated old
`projects/foo/repo`, an old entry with an unexpected external duplicate, normal
`.git` directory, valid absolute `.git` pointer file, wrong origin, and an
unexpanded relative path. Test `--output` with a mode-`0700` external temporary
directory and require the generated bytes there while the tracked registry
remains unchanged.

- [ ] **Step 2: Run tests and require the old-root failures**

Run:

```bash
python3 -m unittest projects.shared.tests.test_project_registry -v
python3 -m unittest projects.shared.tests.test_check_project_repos -v
```

- [ ] **Step 3: Implement separate roots**

Give the builder `--projects-root`, required `--repo-path-map`, and explicit
`--output` options. The production command may default `--output` to the
tracked `projects/shared/project-registry.json`, but a supplied path is used
exactly and never also rewrites the default.
Discover note metadata only from the former and serialize the exact path for
each slug from the path map. The map initially contains all 34 current
`~/My Drive/Epoch/projects/<slug>/repo` paths. It accepts only those exact old
paths or `~/repos/epoch/<slug>`, rejects duplicate/missing slugs, and never
probes to choose one. Consumers expand the serialized path once.

Rewrite `check_project_repos.py` to enumerate expected slugs from notes and
validate the exact declared checkout, origin, and `.git` representation. For
an external entry it also requires the old path absent; for an old entry it
requires the external destination absent.

- [ ] **Step 4: Update the pre-commit lifecycle owner**

Make the parent hook read the exact current `automations-dashboard` entry from
the generated registry and execute its `scripts/validate_lifecycle.py`.
Missing or ambiguous declared code is a hard failure. Add mixed-state tests and
prove there is no old/new fallback.

- [ ] **Step 5: Regenerate and verify**

Run:

```bash
python3 -m unittest discover -s projects/shared/tests -v
python3 projects/shared/scripts/build_project_registry.py --projects-root "$HOME/My Drive/Epoch/projects" --repo-path-map projects/shared/repo-paths.json
python3 projects/shared/scripts/check_project_repos.py
sh scripts/check-retired-slack-refs.sh
git diff --check
```

Require the generated registry to contain exactly the 34 approved slugs, every
live entry still at its exact old path, and the checker to pass. No future path
is activated in this plan.

- [ ] **Step 6: Commit the shared model**

Commit only the shared Epoch root changes with
`projects: separate note and repository roots`.

### Task 2: Convert Automations Dashboard consumers

**Files:** dashboard files listed above

- [ ] **Step 1: Add failing catalog/activity tests**

Require `build_repo_index()` to consume injected mixed registry entries,
serialize each exact `local_repo_path`, and make both `earliest_local_commit()`
and `project_activity()` reject missing, undeclared, or ambiguous paths.

- [ ] **Step 2: Implement one strict resolver**

Add a single helper that accepts only the exact path serialized for the slug
and validates it below one of the two allowed roots. Use it in catalog import
and activity collection. Derive `refresh_dashboard.sh`'s own root from its
script location. Add a renderer that accepts an explicit repository root and
produces a plist; do not modify the current plist or loaded job in this plan.

- [ ] **Step 3: Regenerate and verify dashboard data**

Run:

```bash
python3 -m unittest tests.test_import_catalog tests.test_collect_activity -v
make test
make validate
bash -n scripts/refresh_dashboard.sh
plutil -lint launchd/ai.epoch.automations-dashboard.refresh.plist
```

Regenerate `data/automations.json`; require all path values to remain at their
current old paths while schema-only changes are exact. Render a future plist
to a temporary external file, lint it, and leave live/canonical plist bytes
unchanged.

- [ ] **Step 4: Commit the dashboard model**

Commit with `dashboard: resolve external Epoch repositories`. Do not push.

### Task 3: Convert Time Tracker and centralize launchd ownership

**Files:** time-tracker and launchd files listed above

- [ ] **Step 1: Add failing separate-root tests**

Update fixtures to include both allowed roots in one registry. Require Git
collection to expand each exact serialized path while preserving explicitly
named historical old-slug aliases. Add launchd audit tests parameterized by an
explicit current repository root.

- [ ] **Step 2: Remove the duplicate launch-agent installer**

Delete `scripts/install_launch_agents.py` and its Make install/uninstall
targets after tests prove `repos/launchd` owns both jobs. Document the launchd
repository as the only writer of live plist state.

- [ ] **Step 3: Update scripts and canonical plists**

Pass notes and exact current repository paths separately through report
generation and Git collection. Add a tested render/update operation for both
canonical plists in `bin/render-epoch-job-paths.py`, but leave canonical and loaded paths at the current old root
until the Time Tracker move transaction.

- [ ] **Step 4: Verify both repositories**

Run:

```bash
make test
make update-current
python3 -m unittest tests.test_update_current_report -v
cd "$HOME/My Drive/repos/launchd"
python3 -m unittest discover -s tests -v
python3 -m unittest tests.test_render_epoch_job_paths -v
plutil -lint agents/ai.epoch.time-tracker.server.plist
plutil -lint agents/ai.epoch.time-tracker.updater.plist
python3 bin/launchd-audit.py
```

- [ ] **Step 5: Commit Time Tracker and launchd separately**

Do not combine the repositories in one commit and do not reload jobs yet.

### Task 4: Convert skills, guards, trust entries, and Emacs-facing paths

**Files:** paired skills, dotfiles guards/tests/config

- [ ] **Step 1: Update project lifecycle skills**

`new-project` must create notes at `~/My Drive/Epoch/projects/<name>` and code
at `~/repos/epoch/<name>`. `rename-project` must journal and rename them as two
distinct transactions. Every other listed skill and the four explicit
modeling-impact/agent-readiness files must resolve the exact registry path
without probing the old or new root. Preserve unrelated existing
pair divergence; change only path-model hunks.

- [ ] **Step 2: Update GitHub write guards with tests first**

Teach the guards to accept one configured exact dashboard source and test both
allowed roots. Keep the live configured value at
`~/My Drive/Epoch/projects/automations-dashboard/repo`. Missing declared
registry, wrong owner, or a second candidate denies. Keep Claude/Codex bodies
equivalent.

- [ ] **Step 3: Test future trust entries without activating them**

Add manifest consumer tests for the `ai-productivity-digest` and `email-triage`
trust keys. Do not edit `codex/config.toml`; plan 6 flips each key only after
its repository moves. Preserve the unrelated dirty hunk and all project-note
trust entries.

- [ ] **Step 4: Verify paired configuration**

Run:

```bash
python3 -m unittest tests.test_github_write_guard tests.test_github_write_guard_paths -v
bin/ai-config-sync audit
git diff --check
```

- [ ] **Step 5: Commit by subsystem**

Use separate commits for Epoch skills and dotfiles guard/trust changes. Do not
push.

### Task 5: Fix team-identity CI ownership

**Files:** old Epoch workflow, new staff-data workflow

- [ ] **Step 1: Reproduce the ownership failure**

Prove the Epoch workflow references ignored, absent
`projects/staff-data/repo` content that `actions/checkout` cannot supply.

- [ ] **Step 2: Move the workflow definition to `staff-data`**

Preserve schedule, secrets, dry-run behavior, and test command, changing only
paths to repository-relative commands. Delete the broken parent workflow.

- [ ] **Step 3: Validate and commit both ownership changes**

Validate YAML and run the referenced test locally. Commit the deletion in
Epoch and addition in staff-data separately. A real Actions run remains pending
until an explicitly authorized push.

### Task 6: Close the path-model gate

- [ ] **Step 1: Search live configuration for implicit old-root construction**

Require every live code consumer to read one exact registry/configured path,
with no existence-based old/new probe. At this pre-migration gate, require all
34 registry entries, both loaded Time Tracker jobs, the dashboard job, GitHub
guards, and trust keys to remain on their current working old paths. Allow only
named historical aliases in Time Tracker and historical prose/logs.

- [ ] **Step 2: Run all affected suites**

Run shared Epoch tests, dashboard tests, Time Tracker tests, launchd tests,
GitHub guard tests, `bin/ai-config-sync audit`, and `git diff --check`.

- [ ] **Step 3: Record remote blockers**

Fresh-fetch every changed repository and list each preparatory commit not yet
reachable from an advertised remote. The repository migration plan cannot
start that unit until the list is empty; do not push here.
