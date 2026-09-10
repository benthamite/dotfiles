# Selected agent skills

Browse 62 selected skills maintained in this dotfiles repository. Each row links
to the Claude Code and Codex source instructions. The complete skill directory
contains its supporting scripts, references, templates and evaluation fixtures.

## Using the skills

These are the author's working skills. Some are reusable procedures; others
require this repository's macOS, Emacs or agent configuration. Read the selected
`SKILL.md` and its referenced setup before running it. Dependencies on commands,
sibling skills and private user configuration are declared in those sources.
Supply your own account configuration, writing samples and other private inputs
where required; those inputs are not part of this collection.

Use the source variant for your runtime. Preserve the repository layout when a
skill uses repository helpers or computes paths from its source location. For a
standalone skill, retain its entire source directory and satisfy its declared
dependencies when installing it. A link to `SKILL.md` is a reading entry point,
not a promise that downloading that one file produces a working installation.

Global and project-local names retain their original scopes. Programmatic skills
are for explicit invocation by a caller; listing them here does not activate
them globally. The Emacs documentation entry is a local discovery shim for its
global counterpart. The global and programmatic walk-list entries remain separate
runtime entry points.

## Maintaining this selection

The rows below are the explicit selection. Additions require publication review
of instructions and supporting files, followed by an intentional catalog edit;
new skills are not included automatically. Edit the linked source directories,
which keep companions and repository-relative paths intact. There are no copied
skill bodies to synchronize here.

The [tracked inventory](../skill-inventory.org) remains the generated inventory of
all tracked skill sources. Catalog selection does not control Git tracking,
remove existing public files or hide their history. The repository's guarded
publication process reviews the outgoing history and tree before a push.

## Global

| Skill | Purpose | Claude Code | Codex |
| --- | --- | --- | --- |
| `add-bib-entry` | Add bibliography entries and matching attachments through Emacs. | [Source](../../claude/skills/add-bib-entry/SKILL.md) | [Source](../../codex/skills/add-bib-entry/SKILL.md) |
| `add-to-emacs-packages` | Register Emacs packages in the public package list, manuals and GitHub profile. | [Source](../../claude/skills/add-to-emacs-packages/SKILL.md) | [Source](../../codex/skills/add-to-emacs-packages/SKILL.md) |
| `automate` | Choose and implement a reusable AI workflow in the appropriate tool. | [Source](../../claude/skills/automate/SKILL.md) | [Source](../../codex/skills/automate/SKILL.md) |
| `build` | Refine a specification into an implementation plan and authorized build. | [Source](../../claude/skills/build/SKILL.md) | [Source](../../codex/skills/build/SKILL.md) |
| `chrome-permission-audit` | Review and revoke standing Claude in Chrome site permissions. | [Source](../../claude/skills/chrome-permission-audit/SKILL.md) | [Source](../../codex/skills/chrome-permission-audit/SKILL.md) |
| `code-audit` | Find evidenced correctness and security defects within selected code. | [Source](../../claude/skills/code-audit/SKILL.md) | [Source](../../codex/skills/code-audit/SKILL.md) |
| `design-audit` | Review architecture for concrete maintenance costs and refactoring opportunities. | [Source](../../claude/skills/design-audit/SKILL.md) | [Source](../../codex/skills/design-audit/SKILL.md) |
| `diagnose` | Trace a specific tooling or workflow incident to its underlying cause. | [Source](../../claude/skills/diagnose/SKILL.md) | [Source](../../codex/skills/diagnose/SKILL.md) |
| `document-elisp-extras` | Inventory and refresh manuals for the dotfiles Emacs extras. | [Source](../../claude/skills/document-elisp-extras/SKILL.md) | [Source](../../codex/skills/document-elisp-extras/SKILL.md) |
| `document-elisp-package` | Write a source-grounded Org and Texinfo manual for an Emacs Lisp package. | [Source](../../claude/skills/document-elisp-package/SKILL.md) | [Source](../../codex/skills/document-elisp-package/SKILL.md) |
| `dotfiles-context` | Route dotfiles and Elpaca edits to canonical sources and verification commands. | [Source](../../claude/skills/dotfiles-context/SKILL.md) | [Source](../../codex/skills/dotfiles-context/SKILL.md) |
| `dx-audit` | Audit a repository for AI developer onboarding and verification friction. | [Source](../../claude/skills/dx-audit/SKILL.md) | [Source](../../codex/skills/dx-audit/SKILL.md) |
| `elisp-conventions` | Apply local Elisp coding, documentation and verification rules. | [Source](../../claude/skills/elisp-conventions/SKILL.md) | [Source](../../codex/skills/elisp-conventions/SKILL.md) |
| `end-to-end` | Verify a software acceptance criterion through the decisive live surface. | [Source](../../claude/skills/end-to-end/SKILL.md) | [Source](../../codex/skills/end-to-end/SKILL.md) |
| `fix-drive-errors` | Diagnose and repair Google Drive desktop sync errors. | [Source](../../claude/skills/fix-drive-errors/SKILL.md) | [Source](../../codex/skills/fix-drive-errors/SKILL.md) |
| `generate-readme` | Generate a GitHub-facing Emacs package README from its Org manual. | [Source](../../claude/skills/generate-readme/SKILL.md) | [Source](../../codex/skills/generate-readme/SKILL.md) |
| `google-sheets-comments` | Review and act on current Google Sheets comment threads through gdoc. | [Source](../../claude/skills/google-sheets-comments/SKILL.md) | [Source](../../codex/skills/google-sheets-comments/SKILL.md) |
| `handoff` | Prepare a faithful next-session prompt for the Emacs agent-handoff consumer. | [Source](../../claude/skills/handoff/SKILL.md) | [Source](../../codex/skills/handoff/SKILL.md) |
| `humanize` | Rewrite formulaic English prose while preserving meaning and voice. | [Source](../../claude/skills/humanize/SKILL.md) | [Source](../../codex/skills/humanize/SKILL.md) |
| `interpretability-audit` | Review code for intent transparency and likely reader misunderstandings. | [Source](../../claude/skills/interpretability-audit/SKILL.md) | [Source](../../codex/skills/interpretability-audit/SKILL.md) |
| `lint-elisp` | Diagnose and fix Elisp byte-compilation and checkdoc findings. | [Source](../../claude/skills/lint-elisp/SKILL.md) | [Source](../../codex/skills/lint-elisp/SKILL.md) |
| `move-session-log` | Relocate selected Claude or Codex session metadata. | [Source](../../claude/skills/move-session-log/SKILL.md) | [Source](../../codex/skills/move-session-log/SKILL.md) |
| `orchestrate-review` | Coordinate author/reviewer work in two live Emacs sessions. | [Source](../../claude/skills/orchestrate-review/SKILL.md) | [Source](../../codex/skills/orchestrate-review/SKILL.md) |
| `org-note-conventions` | Preserve personal Org note structure and paragraph conventions. | [Source](../../claude/skills/org-note-conventions/SKILL.md) | [Source](../../codex/skills/org-note-conventions/SKILL.md) |
| `overnight-todos` | Classify and process authorized personal Org TODOs unattended. | [Source](../../claude/skills/overnight-todos/SKILL.md) | [Source](../../codex/skills/overnight-todos/SKILL.md) |
| `paste-via-kill-ring` | Stage actual manual-paste handoffs in Emacs or macOS. | [Source](../../claude/skills/paste-via-kill-ring/SKILL.md) | [Source](../../codex/skills/paste-via-kill-ring/SKILL.md) |
| `personalize` | Draft prose using private writing samples. | [Source](../../claude/skills/personalize/SKILL.md) | [Source](../../codex/skills/personalize/SKILL.md) |
| `pin-elisp-pr` | Pin an effective Elpaca package owner to a fork, branch or PR. | [Source](../../claude/skills/pin-elisp-pr/SKILL.md) | [Source](../../codex/skills/pin-elisp-pr/SKILL.md) |
| `post-push-ci` | Observe exact pushed-commit CI and repair within authorization. | [Source](../../claude/skills/post-push-ci/SKILL.md) | [Source](../../codex/skills/post-push-ci/SKILL.md) |
| `pr-audit` | Review frozen PR revisions, checks and integration evidence. | [Source](../../claude/skills/pr-audit/SKILL.md) | [Source](../../codex/skills/pr-audit/SKILL.md) |
| `profile-ai-cli-performance` | Benchmark declared Claude/Codex client conditions. | [Source](../../claude/skills/profile-ai-cli-performance/SKILL.md) | [Source](../../codex/skills/profile-ai-cli-performance/SKILL.md) |
| `proofread` | Produce and review Markdown spelling or Gemini proofreading edits. | [Source](../../claude/skills/proofread/SKILL.md) | [Source](../../codex/skills/proofread/SKILL.md) |
| `record-decisions` | Record evidence-backed decisions and maintain an index. | [Source](../../claude/skills/record-decisions/SKILL.md) | [Source](../../codex/skills/record-decisions/SKILL.md) |
| `release-package` | Audit, prepare and release public Emacs packages. | [Source](../../claude/skills/release-package/SKILL.md) | [Source](../../codex/skills/release-package/SKILL.md) |
| `request-review` | Request one independent committed-plan review in the opposite CLI. | [Source](../../claude/skills/request-review/SKILL.md) | [Source](../../codex/skills/request-review/SKILL.md) |
| `session-learning-capture` | Capture de-identified session lessons as proposals. | [Source](../../claude/skills/session-learning-capture/SKILL.md) | [Source](../../codex/skills/session-learning-capture/SKILL.md) |
| `symptom-check` | Check proposed repairs against invariants and analogous paths. | [Source](../../claude/skills/symptom-check/SKILL.md) | [Source](../../codex/skills/symptom-check/SKILL.md) |
| `test-suite` | Build meaningful behavioral and regression tests. | [Source](../../claude/skills/test-suite/SKILL.md) | [Source](../../codex/skills/test-suite/SKILL.md) |
| `triage-personal-todos` | Prioritize personal Org tasks and select actionable work. | [Source](../../claude/skills/triage-personal-todos/SKILL.md) | [Source](../../codex/skills/triage-personal-todos/SKILL.md) |
| `update-log` | Preserve project progress and complete local bookkeeping. | [Source](../../claude/skills/update-log/SKILL.md) | [Source](../../codex/skills/update-log/SKILL.md) |
| `verify` | Define and evaluate requirements against explicit evidence. | [Source](../../claude/skills/verify/SKILL.md) | [Source](../../codex/skills/verify/SKILL.md) |
| `walk-list` | Enforce sequential or bounded-pool list processing. | [Source](../../claude/skills/walk-list/SKILL.md) | [Source](../../codex/skills/walk-list/SKILL.md) |

## Global · programmatic

| Skill | Purpose | Claude Code | Codex |
| --- | --- | --- | --- |
| `open-session-log` | Open the exact current agent transcript in Emacs. | [Source](../../claude/programmatic-skills/open-session-log/SKILL.md) | [Source](../../codex/programmatic-skills/open-session-log/SKILL.md) |
| `try-hartree-skills` | Run explicit, bounded trials of selected third-party skills. | [Source](../../claude/programmatic-skills/try-hartree-skills/SKILL.md) | [Source](../../codex/programmatic-skills/try-hartree-skills/SKILL.md) |
| `walk-list` | Enforce sequential or bounded-pool list processing through a queue helper. | [Source](../../claude/programmatic-skills/walk-list/SKILL.md) | [Source](../../codex/programmatic-skills/walk-list/SKILL.md) |

## Dotfiles project

| Skill | Purpose | Claude Code | Codex |
| --- | --- | --- | --- |
| `config-audit` | Audit redundant, conflicting and stale agent configuration. | [Source](../../.claude/skills/config-audit/SKILL.md) | [Source](../../.codex/skills/config-audit/SKILL.md) |
| `find-skills` | Discover, vet and optionally install third-party skills. | [Source](../../.claude/skills/find-skills/SKILL.md) | [Source](../../.codex/skills/find-skills/SKILL.md) |
| `gitguardian-triage` | Investigate and remediate credential incidents with exact ownership and validity evidence. | [Source](../../.claude/skills/gitguardian-triage/SKILL.md) | [Source](../../.codex/skills/gitguardian-triage/SKILL.md) |
| `install-mcp-server` | Install, authenticate and verify Claude MCP servers. | [Source](../../.claude/skills/install-mcp-server/SKILL.md) | [Source](../../.codex/skills/install-mcp-server/SKILL.md) |
| `optimize-agent-instructions` | Simplify persistent agent instructions while preserving scope and discoverability. | [Source](../../.claude/skills/optimize-agent-instructions/SKILL.md) | [Source](../../.codex/skills/optimize-agent-instructions/SKILL.md) |
| `release-dotfiles` | Prepare reproducible Elpaca lockfiles and verified dotfiles releases. | [Source](../../.claude/skills/release-dotfiles/SKILL.md) | [Source](../../.codex/skills/release-dotfiles/SKILL.md) |
| `skill-audit` | Audit skills and reconstruct conservative Codex invocation evidence. | [Source](../../.claude/skills/skill-audit/SKILL.md) | [Source](../../.codex/skills/skill-audit/SKILL.md) |
| `skill-resolver` | Resolve exact skill identities across Claude and Codex roots. | [Source](../../.claude/skills/skill-resolver/SKILL.md) | [Source](../../.codex/skills/skill-resolver/SKILL.md) |

## Dotfiles project · programmatic

| Skill | Purpose | Claude Code | Codex |
| --- | --- | --- | --- |
| `open-session-log` | Identify the current agent conversation and open its exact log in Emacs. | [Source](../../.claude/programmatic-skills/open-session-log/SKILL.md) | [Source](../../.codex/programmatic-skills/open-session-log/SKILL.md) |

## Emacs project

| Skill | Purpose | Claude Code | Codex |
| --- | --- | --- | --- |
| `document-elisp-extras` | Route project-local documentation requests to the maintained global workflow. | [Source](../../emacs/.claude/skills/document-elisp-extras/SKILL.md) | [Source](../../emacs/.codex/skills/document-elisp-extras/SKILL.md) |
| `emacs-freeze` | Diagnose unresponsive Emacs with bounded sampling and server probes. | [Source](../../emacs/.claude/skills/emacs-freeze/SKILL.md) | [Source](../../emacs/.codex/skills/emacs-freeze/SKILL.md) |
| `migrate-profile` | Coordinate session/history housekeeping after an Emacs profile migration. | [Source](../../emacs/.claude/skills/migrate-profile/SKILL.md) | [Source](../../emacs/.codex/skills/migrate-profile/SKILL.md) |
| `rename-package` | Rename Emacs package source, API and selected repository/runtime identities. | [Source](../../emacs/.claude/skills/rename-package/SKILL.md) | [Source](../../emacs/.codex/skills/rename-package/SKILL.md) |

## macOS project

| Skill | Purpose | Claude Code | Codex |
| --- | --- | --- | --- |
| `audit-mac-app` | Inspect a macOS application before running it or granting permissions. | [Source](../../macos/.claude/skills/audit-mac-app/SKILL.md) | [Source](../../macos/.codex/skills/audit-mac-app/SKILL.md) |
| `reconcile-contacts` | Compare contact populations and prepare merge candidates. | [Source](../../macos/.claude/skills/reconcile-contacts/SKILL.md) | [Source](../../macos/.codex/skills/reconcile-contacts/SKILL.md) |
| `review-lulu-alert` | Investigate a firewall alert and recommend a scoped response. | [Source](../../macos/.claude/skills/review-lulu-alert/SKILL.md) | [Source](../../macos/.codex/skills/review-lulu-alert/SKILL.md) |
| `security-audit` | Review secrets handling, supply chains, macOS controls and agent permissions. | [Source](../../macos/.claude/skills/security-audit/SKILL.md) | [Source](../../macos/.codex/skills/security-audit/SKILL.md) |
