# Dotfiles documentation architecture design

## Goal

Make the repository understandable from its documentation. The root README
should explain what the repository contains and direct readers to the right
subsystem. Each maintained top-level configuration package should explain its
own files, activation, update procedure, verification, and security boundary.

The documentation checks should detect objective drift. In particular, they
should reject a missing subsystem README, a stale generated inventory, and a
broken local documentation link. They should not accept a meaningless README
edit merely because some documentation file changed in the same commit.

## Current problem

Only six of the 34 tracked top-level directories have an immediate README.
The root README links four subsystems and then becomes a partial skill catalog.
The Emacs README consists of seven lines about skills despite the directory
containing the main literate configuration, hundreds of extras and snippets,
and several generated or profile-dependent artifacts.

The existing agent documentation has the opposite problem. The same skill and
hook behavior is described in `agents/README.org`, `claude/README.org`,
`codex/README.org`, and long `note` fields in `ai-config-sync.json`. These
copies have already diverged. The master inventory omits tracked skills and
lists a skill that is no longer present.

This drift is partly mechanical. The project-local skill guard accepts any
change to the repository's root `README.org`. It neither checks that the
changed skill is documented nor verifies the description. The guard therefore
creates README churn without establishing documentation truth.

## Documentation policy

### README by default

Every tracked, user-maintained top-level configuration package has a local
`README.org`. The exceptions are:

- `.claude` and `.codex`, whose dotfiles-local agent behavior is documented by
  `agents/README.org` and the generated skill inventory;
- `.github`, whose single test workflow is described by the root testing
  section.

The rule applies to top-level deployment units, not recursively to every
directory. Individual skill directories already have `SKILL.md`. Generated
directories, fixtures, caches, font directories, and collections of snippets
do not receive ceremonial READMEs unless they have an independent maintenance
workflow.

The required top-level READMEs are therefore:

```text
.agent-learnings/README.org
agents/README.org
ai-marketplace-monitor/README.org
aider/README.org
archive/README.org
aspell/README.org
bin/README.org
claude/README.org
codex/README.org
colima/README.org
dark-reader/README.org
docs/README.org
emacs/README.org
enchant/README.org
enhancer-for-youtube/README.org
karabiner/README.org
ledger/README.org
letterboxd-extras/README.org
macos/README.org
mbsync/README.org
moon+reader/README.org
moonlander/README.org
mpv/README.org
pantalaimon/README.org
qBittorrent/README.org
rectangle/README.org
shell/README.org
tests/README.org
uBlacklist/README.org
uBlock/README.org
vimium/README.org
```

### What each local README owns

A local README answers the questions that apply to its directory:

1. What consumes these files?
2. Which files are authoritative, generated, exported, or archived?
3. Where are they installed, linked, copied, or imported?
4. How are changes applied or reloaded?
5. How are changes verified?
6. Does the directory contain encrypted or machine-local material?
7. Where is the deeper manual, if one exists?

These are questions, not mandatory headings. A one-file browser export can
answer them in a short document. `emacs`, `shell`, and `bin` need fuller
manuals. The audit checks ownership and structure that can be established
mechanically; it does not pretend to prove that arbitrary prose is true.

### Root README

The root `README.org` becomes the map for the repository. It contains:

- a short description of the repository;
- the activation model, including the fact that some packages are symlinked
  while others are application exports or generated outputs;
- a linked table covering every tracked top-level directory;
- the public/private boundary, including git-crypt and machine-local state;
- common maintenance and verification commands;
- links to the agent, Emacs, macOS, Karabiner, and shell documentation.

It does not contain a skill inventory or individual tool implementation
history.

### Agent documentation

`agents/README.org` owns the cross-tool topology and synchronization contract.
It links to a generated tracked-skill inventory instead of embedding a manual
catalog.

`claude/README.org` and `codex/README.org` describe only tool-specific
activation, settings, and behavior that genuinely differs between the tools.
Shared policy is stated once in `agents/README.org`. Individual skill behavior
remains in `SKILL.md`.

Behavior-owning agent subdirectories receive focused indexes:

```text
claude/bin/README.org
claude/context/README.org
claude/hooks/README.org
claude/skills/README.org
codex/bin/README.org
codex/hooks/README.org
codex/rules/README.org
codex/skills/README.org
```

The paired hook READMEs may share common descriptions where behavior is
equivalent, but each document names only the files and activation details of
its own tool. `agents/README.org` owns the relationship between the two trees.

### Generated skill inventory

`agents/skill-inventory.org` is generated from tracked `SKILL.md` frontmatter.
It contains separate sections for:

- global paired and tool-specific skills;
- dotfiles project-local interactive skills;
- Emacs and macOS project-local skills;
- programmatic-only skills;
- archived skills.

The generator does not claim to inventory dynamic plugins, ignored runtime
system skills, or project-local skills outside this repository. The document
says so explicitly and points readers to runtime skill discovery for those
categories.

Each row is derived from the skill name, description, path, tool, and scope.
The generated file is deterministic. The audit regenerates it in memory and
fails when the tracked copy differs.

## Validation architecture

### `bin/docs-audit`

A new Python standard-library command owns documentation validation. It has
two subcommands:

```text
bin/docs-audit generate
bin/docs-audit audit
```

`generate` writes `agents/skill-inventory.org` atomically from tracked skill
frontmatter. `audit` performs read-only checks:

- every required top-level package has `README.org`;
- every exemption is declared and still exists;
- the root directory map covers every tracked top-level directory exactly
  once;
- the generated skill inventory is current;
- relative Org `file:` links in README files resolve, after removing search
  anchors;
- source/generated relationships declared in policy refer to existing files;
- no hand-maintained skill catalog remains in the root README or the three
  agent overview READMEs.

The command reports all problems in one run and exits nonzero. It does not
silently fix files during `audit`.

### Policy data

`docs/readme-policy.json` contains data rather than prose:

- required top-level README directories;
- exempt top-level directories and a short reason;
- source/generated file pairs such as `macos/defaults.org` to `macos/macos`
  and `karabiner/modifications.org` to `karabiner/karabiner.edn`;
- the documentation owner for dotfiles project-local skills;
- directories ignored by link and inventory checks.

The human explanation remains in `docs/README.org`. Tests compare policy data
with the actual top-level tree so that a newly added package cannot bypass the
choice between a README and an explicit exemption.

### Integration with the agent sync guard

`bin/ai-config-sync audit` calls `bin/docs-audit audit` for the dotfiles
repository. A documentation failure therefore fails the existing configuration
audit.

The project-local skill commit check stops requiring an arbitrary root README
edit. For the dotfiles repository it requires the generated inventory to be
current and uses `agents/README.org` as the human documentation owner. Other
repositories keep their existing root README default unless their local
manifest names a different documentation owner.

The stale `move-session-log` entry is removed from
`DOTFILES_PROJECT_LOCAL_SKILLS`. Tests cover the exact current local skill set
so another migration cannot leave the allowlist behind.

## Existing documents

### Emacs

`emacs/README.org` is rewritten around the actual configuration. It explains
`config.org`, profile-aware tangling, the extras and their manuals, snippets,
the lockfile, generated artifacts, the canonical dotfiles checkout, and the
post-commit rebuild/reload path. Its short project-local automation section
links to the generated inventory rather than copying skill descriptions.

### macOS

`macos/README.org` begins with `defaults.org` and its generated `macos` script.
It documents the LaunchAgents and personal-update policy owned by the
directory. The reasoning-task worktree sync explanation moves to the owning
agent or command documentation; the macOS README retains only the scheduling
cross-reference. The broken `config.org` link in `defaults.org` is corrected to
the generated `macos` script.

### Karabiner

`karabiner/README.org` is a short index. It identifies
`modifications.org` as the literate source, `karabiner.edn` and layouts as
generated outputs, and the backup directory as historical application state.
It links to `modifications.org` for the full setup and binding manual.

### Moonlander

The generic vendor `moonlander/README.md` is retained as
`moonlander/UPSTREAM-README.md`. A new `README.org` documents the checked-in
layout, the exact source directory, the build/import workflow, and its
relationship to the Karabiner layer. It contains no unresolved template text.

### Shell and commands

`shell/README.org` explains zsh startup order, symlink targets, command shims,
history-secret filtering, and the git-crypt boundary. It names the encrypted
file without quoting its contents.

`bin/README.org` groups commands by purpose and gives a one-line contract for
each tracked executable. Detailed workflows remain in their owning subsystem
README or skill; the command index links there instead of copying them.

### Small configuration packages

The small top-level packages receive concise READMEs. Each one records whether
the tracked file is a symlink target, a generated file, or an application
export, together with the refresh and verification procedure. Where the live
activation method cannot be established from tracked configuration or symlink
metadata, the README states that fact rather than inventing a setup command.

## Manifest cleanup

`ai-config-sync.json` remains a machine manifest. Long incident narratives and
behavior descriptions move to the owning hook README, skill, or a dated design
record. Manifest entries retain paths, status, equivalence semantics, and a
short reason for intentional divergence. This migration is mechanical and
must not alter the sync behavior interpreted by `bin/ai-config-sync`.

## Tests

`tests/test_docs_audit.py` covers:

- missing README detection;
- undeclared new top-level directories;
- root map coverage and duplicate rows;
- broken relative Org links;
- source/generated path validation;
- deterministic skill inventory generation;
- stale, added, removed, and moved skill detection;
- exclusion of dynamic and ignored runtime skills;
- absence of hand-maintained overview catalogs.

Existing `tests/test_ai_config_sync_audit.py` gains regression tests for the
documentation-owner behavior, the corrected project-local allowlist, and
propagation of `docs-audit` failures.

Documentation is also reviewed directly. Passing a structural check does not
establish that a prose claim matches application behavior.

## Rollout

The work is divided into four independently reviewable commits:

1. Add the failing documentation-policy tests, implement `docs-audit`, and
   integrate it with the sync checker.
2. Rewrite the root and major subsystem documentation: Emacs, shell, commands,
   macOS, Karabiner, Moonlander, docs, archive, tests, and agent-learning state.
3. Add concise READMEs to the remaining top-level configuration packages.
4. Generate the skill inventory, reduce the agent overview documents and
   manifest notes, then run the full documentation and configuration audits.

Each commit updates only its declared documentation owners. The implementation
uses an isolated worktree because unrelated plans and tests are currently
present in the main checkout.

## Alternatives considered

### Keep a complete catalog only in the root README

This would make the root file longer without explaining the behavior local to
each package. It would also recreate the same update bottleneck that produced
the stale skill list.

### Add a README to every directory recursively

This would create many files that say little and drift quickly. The useful
boundary is the independently maintained or deployed subsystem. Individual
skills, fixtures, generated outputs, and snippet collections already have a
clear owner.

### Keep manual skill inventories but strengthen the commit guard

A stronger guard could require a changed skill name to appear in prose, but it
could not keep several descriptions semantically equivalent. Generating the
inventory from the same frontmatter used for routing removes that duplication.

## Acceptance criteria

- The root README is a repository map and contains no manual skill catalog.
- Every required top-level package has a local README; every exception is
  explicit and checked.
- Major subsystem READMEs describe actual source, activation, update, and
  verification behavior.
- The generated skill inventory exactly matches tracked skills within its
  declared scope.
- `bin/docs-audit audit` detects coverage, inventory, and link drift.
- `bin/ai-config-sync audit` fails when the documentation audit fails.
- Project-local skill changes no longer pass because an unrelated line in the
  root README changed.
- Shared agent policy has one canonical owner, with tool-specific differences
  documented only in the relevant tool README.
- No secret value or private voice sample appears in committed documentation.
- The relevant unit tests, documentation audit, agent sync audit, and local
  link checks pass from the isolated implementation worktree.
