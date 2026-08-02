# Dotfiles Documentation Architecture Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the repository's stale, centralized documentation with a root map, accurate subsystem READMEs, a generated tracked-skill inventory, and deterministic checks for documentation ownership and drift.

**Architecture:** A new standard-library command, `bin/docs-audit`, reads `docs/readme-policy.json`, generates `agents/skill-inventory.org`, and audits top-level README coverage, root-map coverage, relative Org links, declared source/generated pairs, and manual skill-catalog drift. `bin/ai-config-sync` delegates dotfiles documentation validation to that command and stops accepting arbitrary root README edits as proof that project-local skills were documented. Human prose moves to the directory that owns the behavior; machine inventories are generated from tracked `SKILL.md` frontmatter.

**Tech Stack:** Python 3 standard library, `unittest`, Org-mode documentation, JSON policy data, Git.

---

## Fixed implementation contracts

- Main command: `bin/docs-audit`.
- Policy: `docs/readme-policy.json`.
- Generated file: `agents/skill-inventory.org`.
- Tests: `tests/test_docs_audit.py` and `tests/test_ai_config_sync_audit.py`.
- `bin/docs-audit generate` writes only the generated inventory.
- `bin/docs-audit audit` is read-only and reports every problem before exiting 1.
- `bin/docs-audit audit --root PATH` permits disposable-repository tests. Production defaults to the command's repository root.
- Policy paths are repository-relative POSIX paths.
- Inventory input is the set of tracked files ending in `/SKILL.md`; untracked runtime and plugin skills are outside scope.
- `private-skills` and `skills/.system` paths are excluded from the public inventory.
- Root map rows use the machine-readable first cell `=directory=` beneath the exact heading `* Directory map`.
- The root map contains every tracked top-level directory exactly once, including declared README exemptions.
- README link auditing checks tracked `README.org` files and the root `README.org`. It validates only relative `file:` links, strips Org `::` search anchors, and ignores external URLs and absolute local paths.
- Generated output must be byte-stable across repeated runs.
- Public documentation must not quote values or contents from encrypted files, private voice samples, tokens, account identifiers, or machine-local credential stores.

## Task 1: Add the documentation policy and coverage/link audit

**Files:**

- Create: `docs/readme-policy.json`
- Create: `tests/test_docs_audit.py`
- Create: `bin/docs-audit`

- [ ] Write `docs/readme-policy.json` with this schema and exact top-level ownership decision:

```json
{
  "version": 1,
  "required_readmes": [
    ".agent-learnings", "agents", "ai-marketplace-monitor", "aider",
    "archive", "aspell", "bin", "claude", "codex", "colima",
    "dark-reader", "docs", "emacs", "enchant", "enhancer-for-youtube",
    "karabiner", "ledger", "letterboxd-extras", "macos", "mbsync",
    "moon+reader", "moonlander", "mpv", "pantalaimon", "qBittorrent",
    "rectangle", "shell", "tests", "uBlacklist", "uBlock", "vimium"
  ],
  "exempt_readmes": {
    ".claude": "Dotfiles-local agent behavior is owned by agents/README.org and the generated inventory.",
    ".codex": "Dotfiles-local agent behavior is owned by agents/README.org and the generated inventory.",
    ".github": "The single workflow is documented by the root testing section."
  },
  "source_generated": [
    {"source": "macos/defaults.org", "generated": "macos/macos"},
    {"source": "karabiner/modifications.org", "generated": "karabiner/karabiner.edn"}
  ],
  "skill_inventory": "agents/skill-inventory.org",
  "project_local_skill_documentation_owner": "agents/skill-inventory.org"
}
```

- [ ] Write failing tests in `tests/test_docs_audit.py` by loading the extensionless script with `SourceFileLoader`, following `tests/test_ai_config_sync_audit.py`. Cover these functions and messages:

```python
def test_missing_required_readme_is_reported(self):
    root = self.make_repo(["alpha/config", "README.org"])
    policy = policy_for(required=["alpha"])
    self.assertEqual(
        ["Required subsystem README is missing: alpha/README.org"],
        module.readme_coverage_problems(root, policy),
    )

def test_new_top_level_directory_requires_policy_decision(self):
    root = self.make_repo(["alpha/README.org", "new-app/config", "README.org"])
    policy = policy_for(required=["alpha"])
    self.assertEqual(
        ["Top-level directory has no README policy decision: new-app"],
        module.readme_coverage_problems(root, policy),
    )

def test_root_map_reports_missing_and_duplicate_directories(self):
    text = "* Directory map\n| =alpha= | A |\n| =alpha= | Again |\n"
    self.assertEqual(
        ["Root directory map contains duplicate row: alpha",
         "Root directory map is missing: beta"],
        module.root_map_problems(text, {"alpha", "beta"}),
    )

def test_relative_org_link_strips_search_anchor(self):
    root = self.make_repo(["README.org", "alpha/README.org", "alpha/manual.org"])
    (root / "alpha/README.org").write_text("[[file:manual.org::*Setup][Setup]]\n")
    self.assertEqual([], module.readme_link_problems(root))

def test_broken_relative_org_link_is_reported(self):
    root = self.make_repo(["README.org", "alpha/README.org"])
    (root / "alpha/README.org").write_text("[[file:missing.org][Missing]]\n")
    self.assertEqual(
        ["Broken README file link: alpha/README.org -> alpha/missing.org"],
        module.readme_link_problems(root),
    )

def test_missing_declared_generated_file_is_reported(self):
    root = self.make_repo(["README.org", "source.org"])
    policy = policy_for(source_generated=[{"source": "source.org", "generated": "out"}])
    self.assertEqual(
        ["Declared generated file is missing: out (source: source.org)"],
        module.source_generated_problems(root, policy),
    )
```

- [ ] Run the focused tests and verify RED:

```bash
python3 -m unittest tests.test_docs_audit -v
```

Expected: import error because `bin/docs-audit` does not exist.

- [ ] Implement `bin/docs-audit` with these focused functions:

```python
REPO_ROOT = Path(__file__).resolve().parents[1]

def git_files(root: Path) -> list[str]: ...
def tracked_top_level_directories(root: Path) -> set[str]: ...
def load_policy(root: Path) -> dict: ...
def readme_coverage_problems(root: Path, policy: dict) -> list[str]: ...
def root_map_directories(text: str) -> list[str]: ...
def root_map_problems(text: str, expected: set[str]) -> list[str]: ...
def tracked_readmes(root: Path) -> list[Path]: ...
def org_file_links(text: str) -> list[str]: ...
def readme_link_problems(root: Path) -> list[str]: ...
def source_generated_problems(root: Path, policy: dict) -> list[str]: ...
```

Implementation rules:

- call Git with argument arrays and `shell=False`;
- use tracked files rather than walking untracked runtime directories;
- sort every returned problem list;
- treat a policy overlap between required and exempt as an error;
- reject a policy entry for a top-level directory that no longer exists;
- resolve links relative to the README containing them;
- reject relative links escaping the repository;
- do not read the contents of non-README link targets.

- [ ] Add `audit_problems(root)` and an `argparse` CLI. At this stage `generate` may print `Skill inventory generation is not implemented` and exit 2; Task 2 replaces it. `audit` combines the coverage, map, link, and source/generated problem lists.

- [ ] Run the tests and verify GREEN:

```bash
python3 -m unittest tests.test_docs_audit -v
```

Expected: all coverage, map, link, and source/generated tests pass.

- [ ] Commit:

```bash
git add docs/readme-policy.json tests/test_docs_audit.py bin/docs-audit
git commit -m "docs: enforce subsystem readme ownership"
```

## Task 2: Generate the tracked skill inventory

**Files:**

- Modify: `bin/docs-audit`
- Modify: `tests/test_docs_audit.py`
- Create: `agents/skill-inventory.org`

- [ ] Add failing tests for frontmatter parsing, pair consolidation, scope classification, exclusion, Org escaping, stable ordering, generation, and stale-file detection:

```python
def test_inventory_combines_paired_skill_paths(self):
    root = self.make_repo([
        "claude/skills/example/SKILL.md",
        "codex/skills/example/SKILL.md",
        "README.org",
    ])
    skill = "---\nname: example\ndescription: Use when testing.\n---\n"
    for path in root.glob("*/skills/example/SKILL.md"):
        path.write_text(skill)
    self.commit_all(root)
    rows = module.skill_inventory_rows(root)
    self.assertEqual("example", rows[0].name)
    self.assertEqual(("Claude", "Codex"), rows[0].tools)
    self.assertEqual("Global", rows[0].scope)

def test_inventory_excludes_private_and_system_skills(self):
    root = self.make_repo([
        "claude/private-skills/x/SKILL.md",
        "codex/skills/.system/y/SKILL.md",
        "README.org",
    ])
    self.assertEqual([], module.skill_inventory_rows(root))

def test_inventory_audit_reports_stale_output(self):
    root = self.make_repo(["claude/skills/example/SKILL.md", "README.org"])
    (root / "agents").mkdir()
    (root / "agents/skill-inventory.org").write_text("stale\n")
    policy = policy_for(skill_inventory="agents/skill-inventory.org")
    self.assertEqual(
        ["Generated skill inventory is stale: agents/skill-inventory.org"],
        module.skill_inventory_problems(root, policy),
    )
```

- [ ] Run only the new inventory tests and confirm they fail because the functions are absent.

- [ ] Add an immutable `SkillInventoryRow` dataclass with `name`, `description`, `scope`, `tools`, and `paths`. Parse only the `name` and single-line `description` frontmatter keys; reject a missing description rather than inventing one.

- [ ] Classify paths with this exact precedence:

```text
archive/**/SKILL.md                                      -> Archived
**/programmatic-skills/**                                -> Programmatic
emacs/.claude/** or emacs/.codex/**                      -> Project-local: emacs
macos/.claude/** or macos/.codex/**                      -> Project-local: macos
.claude/** or .codex/**                                  -> Project-local: dotfiles
claude/skills/** or codex/skills/**                      -> Global
```

Reject a tracked `SKILL.md` outside known public roots so a new scope receives an explicit design decision.

- [ ] Render deterministic Org with this header and one table per nonempty scope:

```org
#+title: Tracked agent skill inventory

This file is generated by =bin/docs-audit generate=. Do not edit it manually.
It covers tracked public skills in this repository. Dynamic plugins, ignored
runtime system skills, private skills, and skills in other repositories are
outside its scope.

* Global

| Skill | Tools | Description | Paths |
```

Escape backslashes and `|` characters in table cells. Sort sections by fixed scope order and rows case-insensitively by name. Consolidate identical name, description, and scope across tools; report conflicting descriptions as audit problems.

- [ ] Implement `write_skill_inventory(root, policy)` as an atomic same-directory write and implement `skill_inventory_problems`. Add it to `audit_problems` and replace the temporary `generate` branch.

- [ ] Run RED/GREEN verification:

```bash
python3 -m unittest tests.test_docs_audit.DocsAuditInventoryTests -v
bin/docs-audit generate
bin/docs-audit generate
git diff --exit-code -- agents/skill-inventory.org
```

Expected: inventory tests pass and the second generation changes nothing.

- [ ] Commit:

```bash
git add bin/docs-audit tests/test_docs_audit.py agents/skill-inventory.org
git commit -m "docs: generate tracked skill inventory"
```

## Task 3: Correct project-local documentation ownership

**Controller-approved correction:** The initial plan required
`agents/skill-inventory.org` to appear in every dotfiles local-skill commit.
That is incorrect for body-only `SKILL.md` edits because the generated inventory
uses only the skill name, description, and path and therefore remains
byte-identical. The final contract does not require a no-op Git change in
generated mode. `bin/docs-audit audit` enforces inventory freshness when the
documentation audit is integrated in Task 7. Generated mode is not a generic
bypass: all linked worktrees identified by the canonical dotfiles Git common
directory share the exception, while unrelated repositories retain the manual
same-commit requirement.

**Files:**

- Modify: `bin/ai-config-sync`
- Modify: `tests/test_ai_config_sync_audit.py`
- Modify: `ai-config-sync.json`
- Modify: `agents/README.org`
- Modify: `claude/README.org`
- Modify: `codex/README.org`
- Modify: `docs/superpowers/specs/2026-08-01-dotfiles-documentation-architecture-design.md`
- Modify: `docs/superpowers/plans/2026-08-01-dotfiles-documentation-architecture.md`

- [ ] Add failing regression tests:

```python
def test_loaded_script_belongs_to_test_checkout(self):
    expected_root = Path(__file__).resolve().parents[1]
    self.assertEqual(
        expected_root / "bin" / "ai-config-sync",
        Path(self.module.__file__).resolve(),
    )

def test_dotfiles_project_local_allowlist_matches_actual_skills(self):
    actual = {p.parent.name for p in (DOTFILES / ".claude/skills").glob("*/SKILL.md")}
    self.assertEqual(actual, self.module.DOTFILES_PROJECT_LOCAL_SKILLS)

def test_dotfiles_generated_owner_allows_body_only_skill_change(self):
    changed = {
        ".claude/skills/config-audit/SKILL.md",
        ".codex/skills/config-audit/SKILL.md",
    }
    with mock.patch.object(self.module, "ROOT", DOTFILES):
        self.assertEqual(
            [],
            self.module.local_skill_readme_problems(
                changed, "config-audit", "skills", DOTFILES
            ),
        )

def test_noncanonical_generated_mode_still_requires_configured_owner(self):
    repo = self.make_repo(["README.md"])
    self.write_file(
        repo,
        "ai-config-sync.json",
        json.dumps({"policy": {
            "project_local_documentation_owner": "agents/skill-inventory.org",
            "project_local_documentation_mode": "generated",
        }}),
    )
    problems = self.module.local_skill_readme_problems(
        {".claude/skills/example/SKILL.md"}, "example", "skills", repo
    )
    self.assertIn("agents/skill-inventory.org", problems[0])
```

- [ ] Verify RED. The locality test must show that the suite previously loaded
  the main checkout, the allowlist test must fail because `move-session-log` is
  stale, and the owner tests must fail because the new signature and manifest
  fields do not exist.

- [ ] Remove `move-session-log` from `DOTFILES_PROJECT_LOCAL_SKILLS`. Change
  the function signature to
  `local_skill_readme_problems(changed_paths, skill, skill_root, repo_root)`.
  Resolve and validate the owner from
  `policy.project_local_documentation_owner`, defaulting to manual
  `README.org`. Manual mode requires that exact owner path in `changed_paths`
  and requires the post-change owner to remain a contained, non-symlink regular
  file. The dotfiles manifest sets `agents/skill-inventory.org` to generated
  mode, which suppresses only the canonical dotfiles same-commit touch gate.
  Compare Git common directories so linked dotfiles worktrees qualify but
  unrelated repositories declaring generated mode still use the manual gate.
  Commit guarding reads the manifest, exact owner blob, paired skill bodies,
  auxiliary files, moves, and removals from one candidate tree: the current
  index plus deterministic paths in an immediately preceding `git add ... &&
  git commit` chain. Unsupported interactive/pathspec content modes block
  explicitly rather than authorizing working-tree state. Treat literal
  newlines, Boolean alternatives, wrapper assignments/options,
  present-but-invalid manifests, and candidate auxiliary symlinks as explicit
  blockers; quoted newlines and exact `command git`/`env git` wrappers remain
  supported.

- [ ] Update the three agent READMEs narrowly in this commit: replace claims that project-local skill changes must touch root `README.org` with the configured-owner/generated-inventory rule. Do not perform the large prose consolidation until Task 7. Do not yet invoke `bin/docs-audit` from `bin/ai-config-sync`; that integration waits until the required documentation exists, so intermediate commits remain possible.

- [ ] Run:

```bash
python3 -m unittest tests.test_ai_config_sync_audit -v
python3 -m unittest tests.test_docs_audit -v
```

Expected: both test modules pass. `bin/docs-audit audit` still reports the missing READMEs that Tasks 4 and 5 will create; `bin/ai-config-sync` does not delegate to it yet.

- [ ] Commit:

```bash
git add bin/ai-config-sync tests/test_ai_config_sync_audit.py ai-config-sync.json agents/README.org claude/README.org codex/README.org docs/superpowers/specs/2026-08-01-dotfiles-documentation-architecture-design.md docs/superpowers/plans/2026-08-01-dotfiles-documentation-architecture.md
git commit -m "agents: validate documentation ownership"
```

## Task 4: Rewrite the root and major subsystem documentation

**Files:**

- Modify: `README.org`
- Modify: `emacs/README.org`
- Create: `shell/README.org`
- Create: `bin/README.org`
- Modify: `macos/README.org`
- Modify: `macos/defaults.org`
- Create: `karabiner/README.org`
- Move: `moonlander/README.md` to `moonlander/UPSTREAM-README.md`
- Create: `moonlander/README.org`
- Create: `docs/README.org`
- Create: `archive/README.org`
- Create: `tests/README.org`
- Create: `.agent-learnings/README.org`

- [ ] Run `bin/docs-audit audit` and save the exact missing-README and root-map failures as the RED baseline. Do not weaken policy to reduce the list.

- [ ] Rewrite the root README in Pablo's plain technical voice with these exact sections:

```org
* Overview
* Activation model
* Directory map
* Public and private material
* Maintenance and verification
```

The directory table has one `=directory=` row per tracked top-level directory. Link every required directory to its README. Link `.claude` and `.codex` to `agents/README.org`; link `.github` to `.github/workflows/test-extras.yml`. Remove the `* Skills` section completely.

- [ ] Rewrite `emacs/README.org` around the canonical `config.org`, profile-aware tangle command, extras/manual structure, snippets, lockfile, active-profile source mirror, post-commit reload, and verification. Replace copied skill descriptions with a link to `../agents/skill-inventory.org`.

- [ ] Create `shell/README.org` covering zsh load order (`.zshenv`, `.zprofile`, `.zshrc`), home symlinks, shims, `zsh-history-security.zsh`, and the git-crypt status of `.zshenv-secrets`. Name the encrypted file but never inspect or quote it.

- [ ] Create `bin/README.org` as a complete one-line index of every tracked executable returned by:

```bash
git ls-files -s bin | awk '$1 == "100755" {sub(/^bin\//, "", $4); print $4}'
```

Group commands by agents, Emacs, external services, macOS/system maintenance, and general utilities. Link detailed behavior to owning docs instead of repeating it.

- [ ] Rewrite `macos/README.org` to lead with `defaults.org` and generated `macos`, then LaunchAgents, personal updates, and project-local security skills. Replace the reasoning-task implementation narrative with one scheduling cross-reference. Correct the `defaults.org` link from nonexistent `config.org` to `macos`.

- [ ] Create `karabiner/README.org` as an index to `modifications.org`, identifying `karabiner.edn` and layout SVG/YAML files as generated and `automatic_backups` as historical application state.

- [ ] Preserve the vendor Moonlander text by moving it to `UPSTREAM-README.md`. Write `moonlander/README.org` for the checked-in layout, exact source directory, ZSA/QMK build workflow, artifact checksums, and relationship to the Karabiner thumb-key layer. Remove `_layout_` and other unresolved template text from the local README.

- [ ] Add focused READMEs for `docs`, `archive`, `tests`, and `.agent-learnings`. State retention and activation boundaries explicitly: design records are not live configuration, archived skills are not discoverable, tests use `unittest`, and learning inbox/archive files are workflow state.

- [ ] Run:

```bash
bin/docs-audit audit
python3 -m unittest tests.test_docs_audit -v
rg -n '_layout_|qmk\*firmware|\[insert|TBD|TODO' README.org emacs/README.org shell/README.org bin/README.org macos/README.org karabiner/README.org moonlander/README.org docs/README.org archive/README.org tests/README.org .agent-learnings/README.org
```

Expected: docs audit now reports only the still-missing small package READMEs and any agent-catalog work reserved for Tasks 5–7. The prose scan is empty.

- [ ] Commit:

```bash
git add README.org emacs/README.org shell/README.org bin/README.org macos/README.org macos/defaults.org karabiner/README.org moonlander/README.org moonlander/UPSTREAM-README.md docs/README.org archive/README.org tests/README.org .agent-learnings/README.org
git commit -m "docs: explain core dotfiles subsystems"
```

## Task 5: Document the small configuration packages

**Files:**

- Create: `ai-marketplace-monitor/README.org`
- Create: `aider/README.org`
- Create: `aspell/README.org`
- Create: `colima/README.org`
- Create: `dark-reader/README.org`
- Create: `enchant/README.org`
- Create: `enhancer-for-youtube/README.org`
- Create: `ledger/README.org`
- Create: `letterboxd-extras/README.org`
- Create: `mbsync/README.org`
- Create: `moon+reader/README.org`
- Create: `mpv/README.org`
- Create: `pantalaimon/README.org`
- Create: `qBittorrent/README.org`
- Create: `rectangle/README.org`
- Create: `uBlacklist/README.org`
- Create: `uBlock/README.org`
- Create: `vimium/README.org`

- [ ] For each directory, inspect filenames, Git metadata, safe configuration keys, and symlink metadata. Do not print values from credential-bearing fields. If activation or export provenance cannot be proved, state that it is not recorded rather than inventing a command.

- [ ] Write concise local READMEs answering purpose, tracked files, activation/import, update, verification, and security where applicable. Use Org links to tracked files. Specific requirements:

  - `aider`: distinguish `.aider.conf.yml` from `global_conventions.md`.
  - `aspell` and `enchant`: distinguish personal dictionaries from replacement dictionaries and explain language coverage.
  - browser-extension exports: name the extension and whether the file is settings, filters, or a backup.
  - `mbsync`: describe the public configuration and external credential lookup without quoting commands that resolve a secret.
  - `mpv` and `pantalaimon`: record the live directory symlink established during the audit.
  - `ledger`: identify the home symlink if it exists; otherwise state the expected consumer path as unverified.
  - `qBittorrent` and `rectangle`: distinguish exports from live application state.

- [ ] Run:

```bash
bin/docs-audit audit
```

Expected: no top-level README coverage or root-map problem remains. Remaining failures, if any, concern agent catalogs or broken links addressed in Tasks 6–7.

- [ ] Commit:

```bash
git add ai-marketplace-monitor/README.org aider/README.org aspell/README.org colima/README.org dark-reader/README.org enchant/README.org enhancer-for-youtube/README.org ledger/README.org letterboxd-extras/README.org mbsync/README.org moon+reader/README.org mpv/README.org pantalaimon/README.org qBittorrent/README.org rectangle/README.org uBlacklist/README.org uBlock/README.org vimium/README.org
git commit -m "docs: document application configuration packages"
```

## Task 6: Add agent subtree indexes

**Files:**

- Create: `claude/bin/README.org`
- Create: `claude/context/README.org`
- Create: `claude/hooks/README.org`
- Create: `claude/skills/README.org`
- Create: `codex/bin/README.org`
- Create: `codex/hooks/README.org`
- Create: `codex/rules/README.org`
- Create: `codex/skills/README.org`
- Modify: `tests/test_docs_audit.py`

- [ ] Add a failing test that each behavior-owning agent subtree has its declared README. Store the required paths in `docs/readme-policy.json` under `required_nested_readmes` and extend `readme_coverage_problems` to validate them.

- [ ] Verify RED, then add the eight READMEs. Each executable/hook index must cover every tracked direct child command or script exactly once. Skills READMEs explain placement, pairing, frontmatter, auxiliary files, private skills, programmatic skills, and generated inventory ownership without listing individual skills.

- [ ] `claude/context/README.org` indexes the three reference documents and states when each is safe to read. It must not quote their sensitive contents. `codex/rules/README.org` explains activation and scope of `default.rules`.

- [ ] Extend `bin/docs-audit audit` to compare direct executable/script basenames against the relevant Org index tables. Use policy entries rather than hard-coded directories:

```json
"indexed_directories": [
  {"path": "bin", "readme": "bin/README.org", "mode": "executable"},
  {"path": "claude/bin", "readme": "claude/bin/README.org", "mode": "file"},
  {"path": "claude/hooks", "readme": "claude/hooks/README.org", "mode": "file"},
  {"path": "codex/bin", "readme": "codex/bin/README.org", "mode": "file"},
  {"path": "codex/hooks", "readme": "codex/hooks/README.org", "mode": "file"}
]
```

Index table rows use `=basename=` in their first cell. Exclude each `README.org` itself and support policy-level ignored basenames.

- [ ] Run:

```bash
python3 -m unittest tests.test_docs_audit -v
bin/docs-audit audit
```

Expected: nested README and command-index tests pass.

- [ ] Commit:

```bash
git add docs/readme-policy.json bin/docs-audit tests/test_docs_audit.py claude/bin/README.org claude/context/README.org claude/hooks/README.org claude/skills/README.org codex/bin/README.org codex/hooks/README.org codex/rules/README.org codex/skills/README.org
git commit -m "docs: index agent configuration subtrees"
```

## Task 7: Consolidate agent documentation and activate the audit guard

**Files:**

- Modify: `agents/README.org`
- Modify: `claude/README.org`
- Modify: `codex/README.org`
- Modify: `ai-config-sync.json`
- Modify: `tests/test_docs_audit.py`
- Modify: `bin/docs-audit`
- Modify: `tests/test_ai_config_sync_audit.py`
- Modify: `bin/ai-config-sync`

- [ ] Add failing tests that the three overview READMEs contain no heading equal to a tracked skill name, that `agents/README.org` contains no `Master Skill Inventory`, and that every `note` string in `ai-config-sync.json` is at most 600 characters.

- [ ] Add the integration regression test only now that the repository documentation can pass:

```python
def test_ai_config_audit_propagates_docs_audit_failure(self):
    with mock.patch.object(
        self.module, "documentation_audit_problems", return_value=["broken docs"]
    ):
        self.assertIn("broken docs", self.module.audit_problems())
```

- [ ] Implement `manual_skill_catalog_problems(root, rows)` by normalizing Org heading markup and comparing headings with generated inventory names. Allow prose references and links; reject only per-skill catalog headings. Implement `manifest_note_problems(root)` with the 600-character limit and add both to `audit_problems`.

- [ ] Verify RED against the current monoliths.

- [ ] Refactor `agents/README.org` to retain cross-tool topology, peer model, external write gates, pairing/guard rules, verification, and limits. Replace the manual inventory with a link to `skill-inventory.org`.

- [ ] Refactor `claude/README.org` and `codex/README.org` to retain tool-specific layout, activation, settings, registrations, and genuine divergences. Move hook-by-hook material to the new hook READMEs. Remove individual skill summaries; link to the generated inventory and the skills tree README.

- [ ] Shorten the eight manifest notes currently over 600 characters. Move durable behavior to the owning hook/bin README and historical incident detail to an appropriate dated design document or `agents/README.org` only when it is still needed. Preserve every machine-consumed field and the semantics of `status` and `equivalence`.

- [ ] Add `documentation_audit_problems(root=ROOT)` to run:

```text
python3 bin/docs-audit audit --root ROOT
```

Capture stdout and stderr, convert each nonempty line into an audit problem, and report a missing or crashing checker as a closed failure. Add it to `audit_problems()` only when auditing the canonical dotfiles root. This is the point at which ordinary `bin/ai-config-sync audit` and commit guards begin enforcing the now-green documentation audit.

- [ ] Run:

```bash
python3 -m unittest tests.test_docs_audit -v
bin/docs-audit generate
bin/docs-audit audit
bin/ai-config-sync audit
```

Expected: all commands exit 0 with no stale catalog or long-note finding.

- [ ] Commit:

```bash
git add agents/README.org agents/skill-inventory.org claude/README.org codex/README.org ai-config-sync.json tests/test_docs_audit.py bin/docs-audit tests/test_ai_config_sync_audit.py bin/ai-config-sync
git commit -m "agents: consolidate configuration documentation"
```

## Task 8: Final verification and documentation review

**Files:**

- Verify all changed files; amend the responsible commit for any defect.

- [ ] Run focused tests:

```bash
python3 -m unittest tests.test_docs_audit tests.test_ai_config_sync_audit -v
```

Expected: all tests pass.

- [ ] Run the complete repository test suite:

```bash
python3 -m unittest discover -s tests -v
```

Expected: all tests pass. If an unrelated platform-dependent test is unavailable, record its exact skip; do not convert a failure to a skip.

- [ ] Run repository audits and generation stability:

```bash
bin/docs-audit generate
git diff --exit-code -- agents/skill-inventory.org
bin/docs-audit audit
bin/ai-config-sync audit
```

Expected: no generated diff; both audits exit 0.

- [ ] Check prose and links:

```bash
git diff --check
rg -n -i 'TBD|TODO|FIXME|\[insert|_layout_|qmk\*firmware' --glob 'README.org' --glob 'README.md'
```

Expected: no unfinished text in the changed documentation. Existing intentional uses in archived or upstream material are reviewed separately.

- [ ] Confirm the exact top-level coverage:

```bash
git ls-tree -d --name-only HEAD
bin/docs-audit audit
```

Expected: every directory is represented once in the root map and has either a local README or one declared exemption.

- [ ] Review the full branch diff for scope, secret safety, and public voice. Confirm no private voice sample, credential value, or machine-local token entered any tracked file.

- [ ] Inspect commit structure and worktree state:

```bash
git log --oneline --reverse master..HEAD
git status --short
```

Expected: focused commits and a clean isolated worktree.
