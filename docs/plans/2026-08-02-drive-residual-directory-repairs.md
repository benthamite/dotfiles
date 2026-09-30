# Residual Drive Directory Repairs Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Resolve the 14 non-repository directory symlinks and establish the first verified zero-directory-symlink baseline under My Drive.

**Architecture:** Cache and environment links are removed only after permanent recreation controls exist; proofread receives a purpose-built external runtime; the two irreplaceable local HTML report vendor trees are restored independently as regular content. Every path uses the repair journal and cloud rollback protocol, followed by a complete rebaseline before file-representation work.

**Tech Stack:** Python/unittest/pytest, npm/TypeScript, launchd, Google Drive v3, browser verification, macOS Trash.

---

This is program plan 7 of 12. Start after all personal and Epoch repository
transactions close. The only acceptable phase result is no directory symlink
under My Drive; a path that cannot be made durable stops the later plans. Use
one `drive-workspace capture-path` journal per path and require local plus cloud
verification before closing its rollback window.

## File map

Exact paths:

```text
Epoch/.cache
Epoch/.pytest_cache
Epoch/projects/.cache
Epoch/projects/shared/scripts/.pytest_cache
Epoch/projects/shared/scripts/__pycache__
dotfiles/.pytest_cache
dotfiles/claude/bin/__pycache__
dotfiles/claude/skills/proofread/node_modules
home/.pytest_cache
courses/Machine Learning (Ng)/Week 2/venv
courses/Programmimg-test/venv
courses/Crafting quality code/Week 2/Peer-graded assignment/__pycache__
Apps/html-report/ui2libs/bootstrap-3.3.1/dist
Documents/html-report-data/ui2libs/bootstrap-3.3.1/dist
```

Dotfiles runtime files:

- Create: `bin/install-proofread-runtime`, `bin/proofread`, `tests/test_proofread_runtime.py`.
- Modify: paired `claude/skills/proofread/{SKILL.md,README.md}` and
  `codex/skills/proofread/{SKILL.md,README.md}`; the existing pairing already
  owns these files, so `ai-config-sync.json` remains unchanged.
- Modify: `bin/README.org`, `claude/README.org`, `codex/README.org`.

### Task 1: Repair the five Epoch-root caches

**Files:**

- Modify: `Epoch/.gitignore`, `Epoch/projects/shared/githooks/pre-commit`
- Modify: `Epoch/projects/shared/tests/{test_audit_project_todos.py,test_check_doc_field_caps.py,test_project_registry.py,test_project_todos.py,test_triage_project_todos.py}`
- Modify: `Epoch/.claude/skills/triage-project-todos/SKILL.md`, `Epoch/.codex/skills/triage-project-todos/SKILL.md`
- Remove through Trash: five exact Epoch links

- [ ] **Step 1: Prove exact producers and dispositions**

Journal complete manifests before classifying anything as disposable. Require
`Epoch/.cache` and `Epoch/projects/.cache` to contain only the recorded partial
`lever-assistant` npm tree and name `~/repos/epoch/lever-assistant` plus its
successful post-migration `npm ci` as the recreation source. Require the two
pytest caches to contain only pytest metadata from the named shared tests and
`__pycache__` to contain only bytecode from the named shared scripts. Any
other content preserves that target and stops for reclassification.

- [ ] **Step 2: Add permanent bytecode/cache-free commands**

Use `python3 -B` for supported direct Python commands and `-p no:cacheprovider`
or a repository pytest config for every supported pytest command. Add tests that
run representative shared scripts and assert none of the five paths appears.

- [ ] **Step 3: Run the full shared suite before mutation**

Run the complete discovered shared suite with bytecode and pytest cache
disabled, record the exact discovered test count, and require every test to
pass.
Preserve existing changes in `inbox.org`,
`projects/current-list-of-automation-projects.org`, and
`projects/shared/project-registry.json`.

- [ ] **Step 4: Remove the five links in one shared-config rollback boundary**

Capture each preimage independently, pause Drive once, move only the five links
to Trash, rerun representative CLI and test workflows, resume, and verify all
five native transitions and cloud representations.

- [ ] **Step 5: Commit only policy changes**

Do not stage the preserved unrelated Epoch changes.

### Task 2: Build the external proofread runtime and clean dotfiles caches

**Files:** dotfiles runtime files listed above; two cache links and one node_modules link

- [ ] **Step 1: Add failing runtime tests**

With a task-specific injected `DOTFILES_RUNTIME_ROOT` and fake npm/tsx, require the installer to stage
`package.json`, `package-lock.json`, `tsconfig.json`, and `scripts/` at
`~/.local/share/dotfiles-runtimes/proofread`, run `npm ci` there, write a
source-lock hash, and atomically replace only a known runtime. Require
`bin/proofread check FILE --engine spellcheck` and `bin/proofread apply FILE
all` to fail closed when the runtime is absent or stale.

- [ ] **Step 2: Implement installer and runner**

`install-proofread-runtime` must never run npm below My Drive. It copies only
the declared source files to a temporary external sibling, runs `npm ci`,
verifies `tsx`, records a SHA-256 over the declared inputs, and atomically
renames the staging directory. `bin/proofread` recomputes that hash and invokes
the external `tsx` with either `scripts/proofread.ts` or
`scripts/apply-suggestions.ts`.

- [ ] **Step 3: Update both skill copies**

Replace `yarn -s proofread`/`apply` instructions with the stable
`~/My Drive/dotfiles/bin/proofread` entrypoint. Keep paired bodies and README
contracts equivalent.

- [ ] **Step 4: Install and verify the real external runtime**

Run the installer, check temporary Markdown with `bin/proofread check FILE
--engine spellcheck`, and apply its deterministic fixture suggestions with
`bin/proofread apply FILE all`.
Require the expected output and no source mutation outside the requested temp
file.

- [ ] **Step 5: Remove the three dotfiles links transactionally**

Run the full dotfiles suite with `PYTHONDONTWRITEBYTECODE=1`, inspect
`claude/bin`, remove `.pytest_cache`, `claude/bin/__pycache__`, and the old
proofread `node_modules` link, then rerun the suite, proofread checks,
`bin/ai-config-sync audit`, and docs audit. Finalize only obsolete external
targets after the rollback window.

- [ ] **Step 6: Commit runtime and policy changes**

Preserve the unrelated `codex/config.toml` modification.

### Task 3: Repair Home Assistant test/runtime policy

**Files:**

- Modify: `home/.claude/skills/home-assistant-health-check/launchd/com.stafforini.ha-health-check.plist`
- Modify: `home/.codex/skills/home-assistant-health-check/launchd/com.stafforini.ha-health-check.plist`
- Modify: `repos/launchd/agents/com.stafforini.ha-health-check.plist`
- Remove through Trash: `home/.pytest_cache`

- [ ] **Step 1: Add bytecode prevention to all three plist sources**

Add `PYTHONDONTWRITEBYTECODE=1` to each job environment without changing its
label, interval, arguments, or logs. Add parity/audit tests for the three
copies.

- [ ] **Step 2: Exercise tests and the live job**

Run `python3 -B -m unittest discover -s tests -v`, the focused launchd audit,
and the controlled health-check with its existing `--dry-run` flag so it cannot
send mail. Record the current healthy baseline of last exit zero.

- [ ] **Step 3: Remove the cache link and observe a real trigger**

Use the repair transaction. Before reloading the live job, obtain the user's
explicit confirmation required by the launchd repository. Then reload only
this job and require the next actual trigger to succeed without cache
recreation; leave the plan pending until that authorization and observation.

### Task 4: Repair the three course paths

**Files:** three exact links; create `Machine Learning (Ng)/Week 2/ENVIRONMENT.md`; modify `.idea/Week 2.iml` and `.idea/misc.xml`

- [ ] **Step 1: Preserve and classify both old environments**

Record full manifests. Both design-time Python 3.7 environments were
non-executable and contained only pip/setuptools metadata. Preserve them for
rollback; do not call them working environments.

- [ ] **Step 2: Preserve Machine Learning at a named external path**

Move its journaled external target atomically to
`~/.local/share/python-envs/machine-learning-ng-week-2-legacy`; do not copy or
claim the Python 3.7 binary works. Create `ENVIRONMENT.md` with that exact
archival path and the statement that the course is currently Octave/Matlab and
has no supported Python activation command. Remove the stale local `venv`
exclude from `.idea/Week 2.iml` and the nonexistent `Python 3.7 (Week 2)` SDK
binding from `.idea/misc.xml`, leaving project-default SDK selection. Verify
all other IDE metadata byte-identical.

- [ ] **Step 3: Create the Programmimg-test external environment**

Create a named environment below
`~/.local/share/python-envs/programmimg-test`, install NumPy, pandas, and
Matplotlib from the course's current imports, and run `ng-week1` with
`MPLBACKEND=Agg`. Document the exact activation/invocation path.

- [ ] **Step 4: Remove all three links and verify**

Run Crafting Quality Code tests with `python3 -B`, remove the cache and both venv
links through their journals, exercise the named external environment, and
verify no native/cloud duplicate.

### Task 5: Restore both local HTML report vendor trees as regular directories

**Files:** two exact `dist` paths and their external targets

- [ ] **Step 1: Compare both trees independently**

Hash and mode-compare the 13 substantive Bootstrap files. Record the four empty
`Icon\r` Finder metadata files present only in the Apps copy. Because the two
complete copies therefore differ, preserve both targets and stop for the
explicit source choice required by the approved design before staging either
replacement. Do not treat the empty files as an implicit exception.

- [ ] **Step 2: Stage two regular replacements**

After that source choice is recorded, create a verified regular-directory
staging tree for each original path while Drive is paused. Do not deduplicate
or make one copy canonical unless the explicit decision says so. Move only
each symlink to Trash, then atomically rename its approved staging directory
into place.

- [ ] **Step 3: Verify bytes and browser consumers**

Rehash every restored file. Open `Apps/html-report/index.html` and
`Documents/html-report-data/index.html` through the browser testing workflow;
require local Bootstrap CSS, JavaScript, and font resources to load without a
missing-file error.

- [ ] **Step 4: Resume and verify cloud objects**

Require regular Drive folders at their recorded parents, matching content,
cleared path-level errors, and no duplicate. After the rollback window, move
the now-obsolete external vendor copies to Trash.

### Task 6: Establish the zero-directory-symlink baseline

- [ ] **Step 1: Run a complete no-follow traversal**

Run:

```bash
find "$HOME/My Drive" -type l -print
```

Compare the output as a sorted exact set with the 20 journaled regular-file
links owned by plans 9 through 11: two Claude bridges, two Uqbar commands, and
16 Enchant forwarding links. Reject any missing or additional link. For each
of those 20, require `test -f` and reject `test -d`. Independently run
`bin/drive-workspace audit` and require successful traversal with zero
directory-link findings.

- [ ] **Step 2: Restart and rebaseline file errors**

Perform one normal Drive restart, wait for queues to settle, and record the
native/API/log identity of every remaining `.gdoc` and file-symlink error.
Do not reuse the earlier total; produce the exact path list that gates the four
regular-file repair plans.

- [ ] **Step 3: Verify registration and route**

Require the journaled My Drive root ID, `is_my_drive=1`, zero `machine_root`
rows, no unexpected cloud creation, and no old repository path recreation.
