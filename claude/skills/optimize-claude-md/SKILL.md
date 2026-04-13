---
name: optimize-claude-md
description: Optimize a project's CLAUDE.md for maximum agent effectiveness. Applies best practices from Anthropic docs and community research. Use when the user says "optimize claude md", "improve claude md", "clean up claude md", "review my claude md", or wants to make their project instructions more effective.
argument-hint: [--accept] [path-to-CLAUDE.md]
argument-choices: "--accept"
---

# Optimize CLAUDE.md

Analyze and rewrite the project's CLAUDE.md to maximize agent instruction-following while minimizing context waste. If a path is given in `$ARGUMENTS`, use that file; otherwise, look for `CLAUDE.md` in the current project root (or `.claude/CLAUDE.md`).

If `--accept` is present in `$ARGUMENTS`, apply all changes without asking for confirmation. Otherwise, present the analysis and proposed rewrite, then wait for approval.

## Background: why this matters

LLMs are stateless. CLAUDE.md is the only file loaded into every single session — it's the highest-leverage point in the harness. A flawed CLAUDE.md degrades every phase of every workflow. But the instruction budget is finite: frontier thinking models reliably follow ~150-200 instructions total, and Claude Code's system prompt already consumes ~50 of those. Every line in CLAUDE.md competes for the remainder.

Worse, Claude Code injects: "this context may or may not be relevant to your tasks. You should not respond to this context unless it is highly relevant to your task." If CLAUDE.md contains too much non-universally-applicable content, Claude may discount the entire file.

## Phase 1: Read and measure

1. Read the target CLAUDE.md in full
2. Resolve and read all `@`-imported files
3. Read any `.claude/rules/*.md` files and `.claude/CLAUDE.md` if separate
4. Read `CLAUDE.local.md` if present
5. Count:
   - Total lines (target: under 200; absolute max: 300)
   - Discrete instructions/rules (target: under 100, accounting for system prompt's ~50)
   - `@`-imports and their sizes

## Phase 2: Evaluate every instruction

For each discrete instruction or rule, evaluate against these criteria. Use subagents in parallel to cover multiple sections.

### Criterion 1: Universal applicability

"Would this instruction be useful in every single session, regardless of what task the user is working on?"

Instructions that only matter for specific tasks (e.g., database schema conventions, deployment procedures, API endpoint patterns) should be moved to `.claude/rules/` files with `paths` frontmatter, skills, or referenced docs — not in the root CLAUDE.md. The root file must contain only universally applicable guidance.

### Criterion 2: Specificity and verifiability

"Is this concrete enough that compliance is unambiguous?"

Good: "Use 2-space indentation", "Run `npm test` before committing", "API handlers live in `src/api/handlers/`"
Bad: "Format code properly", "Test your changes", "Keep files organized", "Write clean code", "Be more natural"

Vague instructions waste budget and may be interpreted differently every time. Either make them concrete or cut them.

### Criterion 3: Pointer vs. copy

"Does this embed information that exists authoritatively elsewhere?"

Prefer `@path/to/file` imports or `file:line` references to authoritative source code over pasting snippets that become stale. CLAUDE.md should point to the truth, not duplicate it.

### Criterion 4: Linter's job

"Is this a code style rule that a linter or formatter could enforce deterministically?"

Never send an LLM to do a linter's job. LLMs are comparably expensive and incredibly slow compared to traditional tools for style enforcement. Style rules in CLAUDE.md bloat context and degrade instruction-following across the board. Move these to linter configs, pre-commit hooks, or Claude Code stop hooks.

### Criterion 5: Default behavior

"Would Claude do this anyway without being told?"

Claude's system prompt and training already establish many behaviors. Instructions that restate defaults consume tokens without changing anything. If removing the instruction wouldn't change behavior, it's dead weight.

Examples of likely defaults: don't mix unrelated changes in a commit, use descriptive variable names, don't introduce security vulnerabilities.

### Criterion 6: Conflicts

"Does this contradict another instruction in CLAUDE.md, rules files, or the system prompt?"

Contradictions cause arbitrary behavior — Claude picks one at random. Review for direct contradictions, semantic tension, and instructions that give different guidance for the same situation.

### Criterion 7: Instruction positioning

"Are the most critical instructions positioned for maximum attention?"

LLMs bias toward instructions at the beginning and end of a block. The most important rules should be at the top and bottom; less critical guidance goes in the middle. Check whether the current ordering reflects actual priority.

## Phase 3: Evaluate structure

### WHAT / WHY / HOW coverage

A good CLAUDE.md covers three dimensions:

- **WHAT**: technology stack, project structure, codebase topology — a map so the agent knows where to find things. Especially important for monorepos.
- **WHY**: what the project does and the role of each component. Without this, the agent can't make judgment calls.
- **HOW**: build tooling, test commands, verification procedures — execution instructions the agent needs to do meaningful work.

Check which dimensions are covered, which are missing, and which are over-represented.

### Progressive disclosure

Content that doesn't need to be in every session should be externalized:

- Task-specific procedures → skills
- File-type-specific conventions → `.claude/rules/` with `paths` frontmatter
- Detailed reference material → separate docs referenced with `@` imports
- Lengthy architectural explanations → standalone files with brief CLAUDE.md pointers

CLAUDE.md should list available resources with one-line descriptions so the agent knows they exist, not embed their full contents.

### Markdown structure

- Uses headers and bullets to group related instructions (Claude scans structure like a reader)
- No dense paragraphs of prose — bullets are more reliably followed
- Logical grouping: related rules are adjacent, not scattered

## Phase 4: Output

### 1. Metrics

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| Total lines | N | < 200 | ok/over |
| Instruction count | N | < 100 | ok/over |
| Imports | N | — | — |

### 2. Per-instruction analysis

A table with columns: Instruction (short), Universal?, Specific?, Pointer?, Not linter?, Not default?, No conflict?. Mark failures.

### 3. Structural assessment

- WHAT/WHY/HOW coverage gaps
- Progressive disclosure opportunities (instructions that should be externalized)
- Positioning issues (critical rules buried in the middle)
- Markdown structure issues

### 4. Proposed changes

For each change, state:
- What to cut, move, merge, rewrite, or add
- Where it goes (stays in CLAUDE.md, moves to `.claude/rules/`, becomes a skill, becomes an `@`-import)
- Why

### 5. Rewritten CLAUDE.md

A complete rewritten version with:
- Dead weight removed
- Non-universal instructions externalized
- Vague instructions made specific or cut
- Style rules removed (moved to linter/hook)
- Pointers replacing embedded copies
- Critical instructions at top and bottom
- Clear WHAT/WHY/HOW structure
- No new rules added — only consolidation and restructuring

Show a diff summary of what changed.

### 6. Externalized files

For each file that needs to be created (`.claude/rules/*.md`, imported docs), provide the full content.

## Guidelines

- **Don't add content.** This skill optimizes existing instructions — it consolidates, restructures, externalizes, and cuts. It does not invent new rules.
- **Preserve intent.** Every instruction that survives should mean the same thing it meant before. Rewriting for clarity is fine; changing the rule is not.
- **Respect intentional overrides.** Some instructions deliberately override Claude's system prompt defaults (e.g., "commit all changes immediately" overrides the default "only commit when asked"). These are high-value — flag them as intentional overrides, don't cut them.
- **Hooks trump instructions.** If a hook mechanically enforces a rule, the CLAUDE.md statement is redundant unless it provides context the hook can't (explaining *why*).
- **Context skills trump CLAUDE.md for scoped rules.** If a rule only applies in certain contexts (e.g., Elisp conventions), it belongs in a context skill or `.claude/rules/`, not the root CLAUDE.md.
- **Ask before applying** (unless `--accept`). Present the full analysis and wait for confirmation before making any changes.
