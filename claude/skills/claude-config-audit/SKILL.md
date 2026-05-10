---
name: claude-config-audit
description: Audit Claude Code configuration across CLAUDE.md, skills, memory, hooks, settings, and documentation for redundancy, conflicts, dead weight, staleness, and instruction drift. Use when the user says "audit config", "audit setup", "clean up my config", "prune my instructions", "review my claude.md", "check for duplicate rules", "audit hooks", "audit memory", or wants to optimize their Claude Code instruction set.
---

# Audit Claude Code configuration

Perform a comprehensive audit of all Claude Code instruction sources to find redundancy, conflicts, staleness, dead weight, and documentation drift.

## Scope and safety

- Use this for Claude Code configuration audits, not ordinary app config linting, code review, repo architecture review, or general security scans unless the question is specifically about Claude Code instructions or automation behavior.
- Treat settings, MCP configuration, memory, and hook payload examples as potentially sensitive. Read them locally, but do not quote secrets, OAuth tokens, API keys, credential helper output, or unnecessary account identifiers in the report. Redact values and report only the setting names or rule effects needed for the audit.
- If the audit runs inside this dotfiles repo and secret material may be involved, follow the repo secret-handling instructions before inspecting credential-related files.
- If an expected source is missing, unreadable, generated, or intentionally absent, record that explicitly instead of inferring its contents.
- If the user supplied `--accept` or explicitly asked you to apply the cleanup, treat that as confirmation to apply high-confidence, scope-preserving edits after the audit. Otherwise, present the audit and ask before changing files.

## Phase 1: Discovery

Create an inventory first, then read every instruction source in the setup. For each source, record the resolved path, symlink target when relevant, whether it is tracked, and what role it plays. Use subagents in parallel when available; when they are not available, partition the inventory manually and use parallel file reads where possible.

### Instruction files

- `~/.claude/CLAUDE.md` (global instructions — may be a symlink)
- Every `CLAUDE.md` in the project directory ancestry (project-level instructions)
- Resolve `@` includes in any CLAUDE.md and read the referenced files

### Skills

- Every `SKILL.md` in `~/.claude/skills/` and the project's skill directories
- Focus on skills with `user-invocable: false` (context skills) — these are loaded automatically and contribute rules to every relevant session
- For user-invocable skills, check only the frontmatter and any top-level rules outside procedural sections — the procedural body is loaded on demand and doesn't compete for attention with CLAUDE.md

### Memory

- `MEMORY.md` index and every linked memory file in the project memory directory
- `~/.claude/memory/MEMORY.md` (global memory) and its files
- Pay special attention to `type: feedback` memories — these are the most likely to duplicate rules that have since been promoted to CLAUDE.md or enforced by hooks

### Hooks

- Read `settings.json` to find all hook definitions
- Read each hook script to understand what it enforces mechanically
- A hook that enforces a rule makes an instruction-level statement of that same rule partially or fully redundant

### Settings

- `settings.json` and `settings.local.json` for any behavioral configuration
- `~/.claude.json` for MCP server definitions (referenced by instructions?)
- Redact sensitive values. The audit needs behavioral meaning, configured paths, and rule effects, not raw credentials.

### Documentation

- Read `claude/README.org` (if present in the project) — this documents the claude/ subdirectory (skills, hooks, settings) and serves as a cross-reference for the audit
- Flag any discrepancies between README.org and the actual configuration (missing skills, outdated hook descriptions, etc.)

### Third-party skills

- Check `.agents/skills/` or any other non-standard skill locations

## Phase 2: Extraction

For each source, extract every discrete rule, instruction, or preference, while respecting the Phase 1 limits for user-invocable procedural skill bodies. A "rule" is any statement that constrains or directs behavior. Examples:

- "Use sentence case instead of title case" — a rule
- "After editing config.org, tangle with init-build-profile" — a rule
- Step 3 of a 10-step procedure in a release skill — NOT a rule (it's a procedural step, only loaded when that skill runs)

Assign each rule a short ID and note which file it came from.

## Phase 3: Analysis

Evaluate every extracted rule on five criteria. Present the results in a table grouped by source file.

### Criterion 1: Default behavior

"Is this something I already do by default without being told?"

Claude's system prompt and training already establish many behaviors. Instructions that merely restate defaults consume tokens without changing anything. To evaluate this, consider what you would do if the instruction were absent — if the answer is "the same thing," it's dead weight.

**Examples of likely defaults**: don't mix unrelated changes in a commit, use descriptive variable names, don't introduce security vulnerabilities.

**Examples of likely NOT defaults**: commit all changes immediately (system prompt says the opposite), use `trash` instead of `rm`, prefer Opus over Haiku for subagents.

### Criterion 2: Conflicts

"Does this contradict or conflict with another rule somewhere else in the setup?"

Check for:
- Direct contradictions (one rule says X, another says not-X)
- **Global vs. project CLAUDE.md conflicts**: a project file can intentionally narrow or override a global rule (e.g., global says "commit immediately" but a project says "don't commit without PR review"). Flag these and ask whether the override is intentional. If intentional, the project file should say so explicitly. If accidental, one of them needs to change.
- **Project CLAUDE.md shadowing**: a project file that restates a global rule with slightly different wording creates ambiguity about which version governs. These are repetition bugs, not conflicts — resolve by removing the project-level copy or adding an explicit "overrides global" note.
- Tension between a blanket prohibition and a skill that needs to do the prohibited thing (e.g., "never kill Emacs" vs. a skill that sends SIGUSR2 with safeguards)
- Instructions that intentionally override system prompt defaults — flag these as intentional overrides, not bugs
- Rules that give different guidance for the same situation depending on which file is loaded

### Criterion 3: Repetition

"Does this repeat something already covered by a different rule or file?"

Check for:
- Exact duplicates across files (same rule, same words)
- Semantic duplicates (different words, same effect)
- Subset rules (rule A says "commit everything immediately"; rule B says "commit skills immediately" — B is a strict subset of A)
- Rules that are now mechanically enforced by a hook, making the instruction-level statement redundant

When a rule appears in N places, identify the **canonical location** (usually CLAUDE.md or the most general file) and flag the others as duplicates.

### Criterion 4: Reactive one-off fixes

"Does this read like it was added to fix one specific bad output rather than improve outputs overall?"

Telltale signs:
- The rule's `Why` section names a single incident ("missed during the whisperx rewrite")
- The rule bans a specific word or pattern rather than addressing the underlying behavior ("never say 'should'")
- The rule is a `type: feedback` memory whose content is a strict subset of a CLAUDE.md rule that was added later
- The rule has been superseded by a hook that now enforces it mechanically

Not all reactive rules are bad — some capture genuinely useful corrections. The question is whether the underlying principle is already covered elsewhere. If so, the reactive rule is redundant.

### Criterion 5: Vagueness

"Is this so vague that it would be interpreted differently every time?"

Telltale signs:
- Subjective adjectives without calibration ("be more natural", "use a good tone", "write clean code")
- No examples or criteria for what counts as compliance
- The rule could justify opposite actions depending on interpretation

Note: most well-written rules with examples or specific constraints will pass this criterion. It mainly catches hastily written preferences.

## Phase 4: Output

Present results in this order:

### 1. Per-rule analysis table

For each source file, a table with columns: Rule (short description), Default?, Conflicts?, Repeats?, Reactive?, Vague?. Use checkmarks or short notes in each cell.

### 2. Cut list

A flat table of every rule recommended for removal, with:
- The rule (short description)
- The source file
- One-line reason for cutting

### 3. Conflict list

Every conflict found, with:
- The two (or more) conflicting rules and their source files
- Whether the conflict is intentional (override) or a bug
- Suggested resolution

### 4. Cleaned-up CLAUDE.md

A rewritten version of the global CLAUDE.md with:
- Dead weight removed
- Overlapping rules merged
- Conflicts resolved
- No new rules added — only consolidation

Show a clear summary of what changed and why.

### 5. Other file changes

For each non-CLAUDE.md file that needs changes (skills, memory), list the specific edits:
- Which rules to remove from which files
- Which memory files to delete
- Which MEMORY.md entries to remove

## Applying changes

If applying changes after confirmation or `--accept`:

1. Apply only high-confidence edits that preserve the intended scope of the user's configuration.
2. Prefer consolidation, wording tightening, and removal of confirmed duplicates over broad rewrites.
3. Do not delete memory files or other user-authored records destructively; use the repo's deletion policy and make the deletion explicit in the summary.
4. Keep paired Claude/Codex configuration artifacts synchronized when the repo defines a sync relationship.
5. Commit each logical change when working in a git repo, unless the user explicitly asked not to commit.

## Verification

Before calling the audit complete:

- Re-read every changed file and check for internal contradictions, broken references, and accidental scope changes.
- Run any available repo-specific checks, especially sync checks for paired Claude/Codex artifacts.
- Review the final diff and staged files so unrelated user changes are not included.
- In the final response, list files changed, verification performed, and unresolved issues or sources that could not be checked.

## Guidelines

- **Do not remove rules that are working.** If a rule changes behavior in a useful way and isn't duplicated elsewhere, it stays — even if it was reactive in origin.
- **Canonical location principle.** When a rule must exist somewhere, prefer CLAUDE.md (always loaded) over context skills (loaded conditionally) over memory (loaded per-project). If a rule is in CLAUDE.md AND a skill, cut it from the skill.
- **Hooks trump instructions.** If a hook mechanically enforces a rule, the instruction-level statement is documentation at best. It can be cut unless it provides context the hook can't (e.g., explaining *why* the rule exists).
- **Don't touch procedural skills.** The body of user-invocable skills is procedural, not a competing instruction set. Don't recommend cutting steps from release, twitter-digest, etc. unless they contain general rules that duplicate CLAUDE.md.
- **Respect the confirmation boundary.** `--accept` or an explicit apply request authorizes high-confidence cleanup; otherwise, present the full analysis and wait for confirmation before changing files.
