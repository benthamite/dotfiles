---
name: optimize-agent-instructions
description: Optimize project agent instruction files. Use for optimize/improve/clean up/review Claude.md or making persistent CLAUDE.md or AGENTS.md guidance more concise, concrete, and reliable; not for ordinary docs or code reviews.
argument-hint: "[--accept] [path-to-CLAUDE.md or AGENTS.md]"
argument-choices: "--accept"
---

# Optimize agent instructions

Analyze and rewrite the project's agent instruction files to maximize agent instruction-following while minimizing context waste. Treat flags such as `--accept` as flags, not paths. If a non-flag path is given in `$ARGUMENTS`, use that file; otherwise, look for `CLAUDE.md or AGENTS.md` in the current project root (or `.claude/CLAUDE.md or AGENTS.md`).

Use this skill for persistent agent instructions: `CLAUDE.md`, `AGENTS.md`,
`.claude/CLAUDE.md`, `CLAUDE.local.md`, and `.claude/rules/**/*.md`, according
to the target runtime. `--accept` or an explicit request to apply the changes
authorizes high-confidence edits. A review-only request stays read-only.

Use the canonical placement policy in `agents/instruction-placement.org` from the dotfiles repo root when deciding whether guidance belongs in an instruction file, skill, hook, linter, reference doc, or project brief.

## Background: why this matters

Instruction files provide context rather than mechanically enforced
configuration. Their loading rules differ by runtime: Claude's `@` imports
and `.claude/rules` are not native Codex instruction mechanisms. Check the
target tool's actual loading path before moving guidance. Stale or vague
instructions can degrade behavior more than volume alone does.

Long, vague, stale, or non-universal instructions consume context and reduce adherence. The goal is not to make instructions clever; it is to keep them scoped, concrete, current, and easy for the agent to verify.

## Phase 1: Read and measure

1. Read the target CLAUDE.md or AGENTS.md in full
2. Resolve and read all `@`-imported files, noting import depth and size
3. Read applicable ancestor and nested instruction files for each supported
   runtime, including Claude local/rules files and Codex `AGENTS.md` files as
   relevant. Record which tool actually loads each source.
4. Treat local or personal instruction files as private context: use them to detect conflicts, but do not quote sensitive content or copy local-only rules into shared files unless the user explicitly targets those files
5. Count:
   - Total lines
   - Discrete instructions/rules
   - `@`-imports, rules files, and their sizes

## Phase 2: Evaluate every instruction

For each discrete instruction or rule, evaluate against these criteria. Use subagents in parallel when the active agent environment supports them; otherwise divide the sections explicitly in your own notes.

### Criterion 1: Universal applicability

"Is this important in nearly every session in this file's scope?"

Keep the project map, hard invariants, and useful routing in root instructions.
Consider moving detailed task-specific procedures into a supported path-scoped
rule, skill, or reference. Preserve a discoverable route and verify that each
supported runtime still receives the guidance where needed.

### Criterion 2: Specificity and verifiability

"Is this concrete enough that compliance is unambiguous?"

Good: "Use 2-space indentation", "Run `npm test` before committing", "API handlers live in `src/api/handlers/`"
Bad: "Format code properly", "Test your changes", "Keep files organized", "Write clean code", "Be more natural"

Vague instructions waste budget and may be interpreted differently every time. Either make them concrete or cut them.

### Criterion 3: Pointer vs. copy

"Does this embed information that exists authoritatively elsewhere?"

Prefer a concise routed link to authoritative content over a copy that can
become stale. Claude `@` imports load their target into context; they reduce
duplication but do not provide on-demand context savings. A plain link with a
clear read-when condition can serve on-demand disclosure in either runtime.

### Criterion 4: Linter's job

"Is this a code style rule that a linter or formatter could enforce deterministically?"

Prefer existing deterministic enforcement for mechanical rules. Do not remove
a useful instruction because a linter or hook could hypothetically replace it.
First establish actual coverage across tools and runtimes. New enforcement is
a separate implementation change and must fit the requested scope.

### Criterion 5: Default behavior

"Would the active agent do this anyway without being told?"

Check active instructions and observed behavior before treating a preference as
a default. Model/runtime defaults vary; predicted behavior alone is insufficient
reason to remove a useful user requirement.

Examples of likely defaults: don't mix unrelated changes in a commit, use descriptive variable names, don't introduce security vulnerabilities.

### Criterion 6: Conflicts

"Does this contradict another instruction in CLAUDE.md or AGENTS.md, rules files, or the system prompt?"

Review direct contradictions, semantic tension, and instructions that give
different guidance for the same situation. Apply instruction precedence and
distinguish intentional overrides from accidental conflict.

### Criterion 7: Operational vs. reference

"Is this operational (trigger → action) or reference (a fact to look up)?"

Repeatable task workflows often belong in skills; stable project invariants and
short required verification commands may belong in the project instructions.
Skill discovery depends on the runtime, metadata, and invocation policy. It is
not a guaranteed keyword hook, and an explicit-only skill must remain explicit.

Reference content — facts, lookups, and architectural background — can live in a
linked document with a clear condition for reading it. Use a Claude `@` import
only when that content should load with the parent instruction file.

Before externalizing content, test how an agent will find it for a realistic
request. Retain essential routing if removing it would hide an expected action.

### Criterion 8: Instruction positioning

"Are the most critical instructions positioned for maximum attention?"

LLMs bias toward instructions at the beginning and end of a block. The most important rules should be at the top and bottom; less critical guidance goes in the middle. Check whether the current ordering reflects actual priority.

## Phase 3: Evaluate structure

### WHAT / WHY / HOW coverage

A good CLAUDE.md or AGENTS.md covers three dimensions:

- **WHAT**: technology stack, project structure, codebase topology — a map so the agent knows where to find things. Especially important for monorepos.
- **WHY**: what the project does and the role of each component. Without this, the agent can't make judgment calls.
- **HOW**: build tooling, test commands, verification procedures — execution instructions the agent needs to do meaningful work.

Check which dimensions are covered, which are missing, and which are over-represented.

### Progressive disclosure

Content that doesn't need to be in every session should be externalized. Match the externalization mechanism to the content type (see Criterion 7):

- **Repeatable task workflows** → skills with discriminating descriptions and
  the existing intended invocation policy.
- **File-specific conventions** → path-scoped rules where supported; otherwise
  a supported scoped instruction file, skill, or routed reference.
- **Reference material** → linked docs with read-when conditions; Claude `@`
  imports only for content intended to load with the parent.
- **Lengthy architectural explanations** → standalone files with brief CLAUDE.md or AGENTS.md pointers (agent pulls on demand).

CLAUDE.md or AGENTS.md should list available resources with one-line descriptions so the agent knows they exist, not embed their full contents.

Flag links whose purpose or read-when condition is missing. Add useful routing
before choosing a more elaborate mechanism; do not convert every link into a
skill or an always-loaded import.

### Structure

- Reference data (paths, commands, tables) is structured; behavioral rules are short prose that carries the reason
- Logical grouping: related rules are adjacent, not scattered

## Phase 4: Output and applied-mode closeout

For analysis-only runs, report actionable findings and the proposed revision,
using the detail below when it helps. For authorized apply runs, apply the safe
edits first, then give a concise closeout with material changes, verification,
and unresolved issues. Counts measure size, not instruction quality. Do not
paste a full rewritten file after applying it unless the user asks.

### 1. Metrics

| Metric | Current |
|--------|---------|
| Total lines | N |
| Instruction count | N |
| Imports/rules | N |

### 2. Per-instruction analysis

A table with columns: Instruction (short), Universal?, Specific?, Pointer?, Not linter?, Not default?, No conflict?, Right placement?, Positioned?. Mark failures.

### 3. Structural assessment

- WHAT/WHY/HOW coverage gaps
- Progressive disclosure opportunities (instructions that should be externalized)
- Positioning issues (critical rules buried in the middle)
- Markdown structure issues

### 4. Proposed changes

For each change, state:
- What to cut, move, merge, rewrite, or add
- Where it goes, using a mechanism the target runtime supports (scoped
  `AGENTS.md`, Claude rules/imports, a skill, or a routed reference)
- Why

### 5. Rewritten CLAUDE.md or AGENTS.md

A complete rewritten version with:
- Dead weight removed
- Task-specific detail externalized where discoverability is preserved
- Vague instructions made specific or cut
- Style-rule duplicates removed only when equivalent enforcement is verified
- Pointers replacing embedded copies
- Critical instructions at top and bottom
- Clear WHAT/WHY/HOW structure
- No new rules added — only consolidation and restructuring

Show a diff summary of what changed.

### 6. Externalized files

Describe any required new scoped instructions, rules, or referenced docs. In
authorized apply mode, create or edit those files directly and summarize them
in the closeout; a review-only run presents the proposed content without writes.

## Phase 5: Verification

When edits are applied:

1. Re-read every changed instruction file for consistency and scope drift
2. Recount lines, discrete instructions, imports, and rules for the edited instruction set
3. Verify links, imports, and rule paths resolve in each supported runtime,
   including intended path matching; confirm no private/local-only instruction
   was copied into a shared file unintentionally
4. Run project-specific checks or docs-sync checks when available
5. Inspect `git diff` and `git status --short` before committing or reporting completion

## Guidelines

- **Don't add content.** This skill optimizes existing instructions — it consolidates, restructures, externalizes, and cuts. It does not invent new rules.
- **Preserve intent.** Every instruction that survives should mean the same thing it meant before. Rewriting for clarity is fine; changing the rule is not.
- **Respect intentional overrides.** Preserve deliberate user preferences and
  project overrides; do not assume every runtime has the same defaults.
- **Check enforcement coverage.** Keep guidance that remains useful despite a
  hook, or that supports a runtime/tool the hook does not cover.
- **Preserve discoverability.** Externalized guidance must still reach every
  supported runtime for the intended task.
- **Respect authorization.** `--accept` or an explicit apply request authorizes
  high-confidence changes. Keep review-only runs read-only and report material
  scope expansions separately.
