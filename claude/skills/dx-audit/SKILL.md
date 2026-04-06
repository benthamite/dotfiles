---
name: dx-audit
description: Audit a codebase from the AI's perspective and recommend concrete improvements that would reduce errors, enable verification, and speed up collaboration. Use when starting work on a new or unfamiliar project, or when collaboration feels friction-heavy.
argument-hint: [--accept] [dir]
argument-choices: "--accept"
---

# AI developer experience audit

Thoroughly explore $ARGUMENTS (if no argument provided, default to the current project) and produce a prioritized set of recommendations for making this codebase easier for an AI coding agent to work with reliably and efficiently.

If `--accept` is present in `$ARGUMENTS`, after completing the audit, immediately implement **all** recommendations without asking for confirmation. Commit the result.

The goal is **frictionless human-AI collaboration**: fewer errors, faster orientation, confident verification of changes, and less time spent on back-and-forth clarification.

Use subagents to explore the codebase in parallel. Read actual code — don't guess from file names.

## Process

### 1. Inventory what exists

Before recommending anything, map what the project already has. For each category below, note what is present, what is partial, and what is absent.

#### Verification infrastructure

- **Tests**: test framework, test runner, how to run them, approximate coverage, whether tests actually pass right now
- **Type checking**: static types, type checker config, strictness level
- **Linting/formatting**: linter config, formatter config, pre-commit hooks
- **Build checks**: does the project build cleanly? Are there CI scripts that can be run locally?
- **REPL or playground**: is there a way to quickly test small changes interactively?

#### Project instructions

- **CLAUDE.md / AI instructions**: are there project-level instructions? Are they accurate, complete, and current?
- **README / onboarding docs**: could a fresh session understand how to build, test, and navigate the project?
- **Architecture documentation**: are the major design decisions and module boundaries explained anywhere?
- **Contributing guide**: are conventions (commit format, branch strategy, PR process) documented?

#### Code navigability

- **Module structure**: are boundaries clear? Can you tell where to find the code for a given feature?
- **Naming conventions**: are names consistent and self-documenting?
- **Entry points**: can you identify where execution starts and how control flows?

#### Automation and scripts

- **Build scripts**: is there a single command to build the project?
- **Test scripts**: is there a single command to run all tests?
- **Dev environment setup**: is there a script or clear instructions for bootstrapping a development environment from scratch?
- **Common tasks**: are frequently needed operations (migrations, seeding, deployment) scripted?

### 2. Simulate the "fresh session" experience

Imagine you are a new Claude session that has just been asked to fix a bug or add a feature in this codebase. Walk through the experience:

- How long does it take to understand the project structure?
- Can you figure out how to build and test without asking the user?
- If you make a change, can you verify it works?
- Are there any "traps" — things that look straightforward but have non-obvious gotchas (unusual build systems, generated code that shouldn't be edited, files that must be kept in sync)?

### 3. Identify gaps and recommend improvements

For each gap, produce a recommendation. Recommendations must be:

- **Concrete**: not "add tests" but "add pytest tests for the payment module, using the existing fixtures in `tests/conftest.py`, starting with the `charge()` and `refund()` functions which handle money"
- **Justified from the AI's perspective**: explain specifically how this helps the AI agent (e.g., "without this, I cannot verify that my changes to the parser don't break existing behavior, so I have to ask you to test manually every time")
- **Effort-estimated**: rough T-shirt size (S/M/L/XL) so the user can prioritize
- **Ordered by impact**: the recommendation that would most reduce friction comes first

Think broadly. The examples below are illustrative, not exhaustive:

- A test suite so the AI can verify changes
- A CLAUDE.md with build/test commands so fresh sessions can self-orient
- Type annotations on key interfaces so the AI makes fewer type errors
- A linter config so the AI matches project style automatically
- Scripts for common tasks the AI currently has to figure out from scratch
- Architecture docs so the AI doesn't have to reverse-engineer module boundaries
- A `.env.example` so the AI knows what environment variables exist
- Pre-commit hooks so the AI catches mistakes before committing
- A Makefile or task runner so the AI can build/test/lint with one command
- Seed data or fixtures so the AI can test against realistic data
- Inline comments on non-obvious code (e.g., workarounds, perf hacks, compatibility shims) so the AI doesn't "fix" intentional oddities

## Output format

### Current state

One paragraph summarizing the project's current AI-readiness: what works well, what's painful, and the overall level of friction.

### Recommendations

Numbered list, ordered by impact (highest first). For each:

1. **Title** (one line)
   - **What**: the specific action to take
   - **Why (AI perspective)**: how this reduces friction, errors, or wasted time for the AI agent — be specific about the failure mode it prevents
   - **Effort**: S / M / L / XL
   - **Depends on**: list any recommendations that should be done first (by number), or "none"

### Quick wins

A short subsection pulling out any recommendations that are both high-impact and small-effort (S or M). These are the obvious starting points.

At the end, offer to implement the quick wins immediately.
