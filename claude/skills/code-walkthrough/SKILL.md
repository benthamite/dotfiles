---
name: code-walkthrough
description: Generate a structured walkthrough of a codebase using showboat. Use when the user says "walkthrough", "code walkthrough", "explain this codebase", "walk me through the code", "how does this code work", or wants a detailed guided tour of how a project works.
argument-hint: [dir] [--output <file>]
---

# Code walkthrough

Generate a structured, linear walkthrough of a codebase that explains how it works in detail. The walkthrough is built with [showboat](https://github.com/simonw/showboat), which captures real code snippets via shell commands rather than manual copying — guaranteeing accuracy and eliminating hallucination risk.

Based on the [linear walkthroughs](https://simonwillison.net/guides/agentic-engineering-patterns/linear-walkthroughs/) pattern from Simon Willison's agentic engineering patterns guide.

## Procedure

### 1. Determine the target

Parse `$ARGUMENTS` for:
- **Directory**: the codebase to walk through (default: current working directory)
- **`--output <file>`**: path for the walkthrough document (default: `walkthrough.md` in the target directory)

### 2. Read and understand the codebase

Use subagents to explore the codebase in parallel. Read actual code — don't guess from file names. Understand:
- The project's purpose and structure
- Entry points and main execution flow
- Key modules, their responsibilities, and how they interact
- Important data structures and algorithms
- Configuration and build setup

### 3. Plan the walkthrough

Before writing anything, plan the walkthrough structure. A good walkthrough is **linear** — it follows a logical reading order that builds understanding progressively. Common orderings:
- **Entry point first**: start with the main entry point and follow the execution flow
- **Bottom-up**: start with foundational types/utilities and build toward high-level orchestration
- **By feature**: walk through each major feature or subsystem in turn

Present the planned outline to the user and ask for confirmation before proceeding.

### 4. Build the walkthrough with showboat

Initialize the document:

```bash
uvx showboat init <output-file> "<Project Name> — code walkthrough"
```

Then build the walkthrough by alternating between commentary and code:

- **Commentary** (`showboat note`): explain what a file or section does, why it exists, and how it connects to the rest of the codebase. Use clear, pedagogical prose.
- **Code snippets** (`showboat exec`): use shell commands (`grep`, `sed`, `head`, `tail`, `cat`, `awk`) to extract the relevant code from source files. **Never paste code manually** — always use `showboat exec` so the snippet is grounded in the actual source.

Examples:

```bash
# Add a section heading and commentary
uvx showboat note <file> "## Configuration

The app's configuration lives in \`config.py\`. It uses environment variables with sensible defaults:"

# Show a specific code block by line range
uvx showboat exec <file> bash "sed -n '10,25p' src/config.py"

# Show a function definition
uvx showboat exec <file> bash "sed -n '/^def process_request/,/^def \|^class \|^$/p' src/handler.py"

# Show imports to illustrate dependencies
uvx showboat exec <file> bash "head -20 src/main.py"

# Show a specific pattern across files
uvx showboat exec <file> bash "grep -n 'register_route' src/*.py"
```

If an `exec` command produces wrong or noisy output, use `showboat pop` to remove it and try a better extraction command.

### 5. Walkthrough structure

Each walkthrough should include:

1. **Overview**: what the project does, its tech stack, and high-level architecture
2. **File-by-file or module-by-module tour**: the core of the walkthrough, with real code snippets and explanations
3. **How it all connects**: a summary of how the pieces fit together, the execution flow, and any non-obvious design decisions

### 6. Verify

After building the walkthrough, run:

```bash
uvx showboat verify <output-file>
```

This re-executes all code blocks and confirms the outputs still match. If any diffs appear, fix them with `pop` and re-do the affected sections.

## Guidelines

- **Ground truth only**: every code snippet must come from `showboat exec`. If you need to show code, write a shell command that extracts it. This is the core invariant of the pattern.
- **Pedagogical tone**: write for someone who has never seen this codebase. Explain the "why", not just the "what".
- **Be selective**: don't show every line of every file. Show the interesting parts — the architecture, the clever bits, the non-obvious decisions. Skip boilerplate.
- **Connect the dots**: after showing a piece of code, explain how it relates to what came before and what comes next.
- **Use the right extraction tool**: `sed -n 'X,Yp'` for line ranges, `grep` for patterns across files, `head`/`tail` for file beginnings/endings, `awk` for more complex extractions.
