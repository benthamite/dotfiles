---
name: code-walkthrough
description: Generate a structured walkthrough of a codebase as an org-mode literate document. Use when the user says "walkthrough", "code walkthrough", "explain this codebase", "walk me through the code", "how does this code work", or wants a detailed guided tour of how a project works.
argument-hint: [dir] [--output <file>]
---

# Code walkthrough

Generate a structured, linear walkthrough of a codebase that explains how it works in detail. The output is an org-mode literate document that interleaves prose with executable source blocks. Each code snippet is extracted via a shell command (`grep`, `sed`, `cat`, etc.) — never pasted from memory — guaranteeing accuracy.

Based on the [linear walkthroughs](https://simonwillison.net/guides/agentic-engineering-patterns/linear-walkthroughs/) pattern from Simon Willison's agentic engineering patterns guide.

## Procedure

### 1. Determine the target

Parse `$ARGUMENTS` for:
- **Directory**: the codebase to walk through (default: current working directory)
- **`--output <file>`**: path for the walkthrough document (default: `walkthrough.org` in the target directory)

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

### 4. Build the walkthrough as an org-mode document

Write the document directly using the Write tool. The format is org-mode with `#+begin_src bash :results output` blocks for code extraction.

#### Document structure

```org
#+title: <Project Name> — code walkthrough
#+date: <today's date>

* Overview

Prose explaining the project...

* Section title

Prose explaining this part of the code...

#+begin_src bash :results output :dir <source-dir>
sed -n '10,25p' config.py
#+end_src

#+RESULTS:
: <actual output from running the command>

More commentary connecting this to the next piece...
```

#### How to include code snippets

Every code snippet MUST be extracted via a shell command in a source block. The agent:

1. Runs the extraction command via Bash to get the output
2. Writes both the source block and its `#+RESULTS:` into the org file

This ensures every snippet is grounded in the actual source. The reader can later re-execute any block with `C-c C-c` in Emacs to verify it still matches.

**Extraction command examples:**

```bash
# Show a specific code block by line range
sed -n '10,25p' src/config.py

# Show a function definition
sed -n '/^def process_request/,/^def \|^class \|^$/p' src/handler.py

# Show imports to illustrate dependencies
head -20 src/main.py

# Show a specific pattern across files
grep -n 'register_route' src/*.py
```

#### Results formatting

- For short outputs (< 10 lines), use the `: ` prefix format (one `: ` per line)
- For longer outputs, use a `#+begin_example` / `#+end_example` block

### 5. Walkthrough structure

Each walkthrough should include:

1. **Overview**: what the project does, its tech stack, and high-level architecture
2. **File-by-file or module-by-module tour**: the core of the walkthrough, with real code snippets and explanations
3. **How it all connects**: a summary of how the pieces fit together, the execution flow, and any non-obvious design decisions

### 6. Verify

After building the walkthrough, verify a sample of source blocks by re-running their commands and comparing the output to what's in the document. If anything has drifted, update the affected blocks.

The reader can also verify any block interactively in Emacs with `C-c C-c`.

## Guidelines

- **Ground truth only**: every code snippet must come from an executed shell command. If you need to show code, write a command that extracts it. This is the core invariant of the pattern.
- **Pedagogical tone**: write for someone who has never seen this codebase. Explain the "why", not just the "what".
- **Be selective**: don't show every line of every file. Show the interesting parts — the architecture, the clever bits, the non-obvious decisions. Skip boilerplate.
- **Connect the dots**: after showing a piece of code, explain how it relates to what came before and what comes next.
- **Use the right extraction tool**: `sed -n 'X,Yp'` for line ranges, `grep` for patterns across files, `head`/`tail` for file beginnings/endings, `awk` for more complex extractions.
