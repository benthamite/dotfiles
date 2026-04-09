---
name: new-project
description: Create a new Epoch project directory with an org file capturing all relevant context from the current conversation, save a session log, commit, and transition to a new session in the project directory. Use when the user says "new project", "create project", or wants to spin off the current conversation into a dedicated project.
user-invocable: true
allowed-tools: Read, Write, Edit, Bash, Glob, Grep, Agent, AskUserQuestion, WebSearch, WebFetch
argument-hint: "[project description, Slack URL, or context]"
---

# Create a new Epoch project

Spin off the current conversation into a dedicated Epoch project: create the directory, write an org file with full context, save a session log, commit, and transition to a new session focused on the project.

## Arguments

`$ARGUMENTS` contains a project description, Slack URL, or other seed context. If empty, derive the project from what has been discussed in the current conversation. If the conversation has no clear project context, ask the user what the project is about.

## Step 1: Gather and synthesize context

Collect all relevant information from:
- The current conversation (messages, research, tool results, decisions)
- Any Slack URLs provided (fetch the message + full thread)
- Any web research performed in the session

From this, determine:
1. A short **kebab-case directory name** (e.g., `chatgpt-usage-analysis`)
2. A **project title** in sentence case (for the org `#+title:`)
3. **Background** — why this project exists, who requested it, key links (Slack threads, docs, etc.)
4. **TODO items** — concrete next steps identified in the conversation
5. **Reference material** — instructions, research findings, data gathered

If the name is ambiguous, confirm with the user before proceeding.

## Step 2: Create the project directory and org file

The Epoch project root is the closest ancestor directory containing `CLAUDE.md` with "Epoch AI" in it. All projects live under `projects/` in that root.

1. Verify the directory does not already exist. If it does, ask the user whether to merge or pick a different name.
2. Create `projects/<name>/`.
3. Write `projects/<name>/<name>.org` following existing project org file conventions:
   - `#+title:` header
   - Top-level `*` heading with a short description of the project
   - `** Background` section with context (who asked for it, why, links to Slack threads or docs)
   - One `** TODO` heading per concrete next step
   - `** Reference` section with any gathered material (step-by-step instructions, research, links)
4. The org file must capture **everything useful** from the current conversation so that a fresh session reading it has full context to continue.

## Step 3: Set up session logging in the new project

1. Create `projects/<name>/logs/`.
2. Write a session log at `projects/<name>/logs/YYYY-MM-DD.md` (today's date) summarizing the current session's work on this project:
   - Title: `# YYYY-MM-DD: Project kickoff`
   - **What was done**: decisions made, research performed, communications sent (Slack messages, emails)
   - **Open questions**: anything unresolved or awaiting a response
   - **Next steps**: what the next session should pick up
3. Create `projects/<name>/CLAUDE.md` with a brief project description and a log reference:
   ```
   # <Project title>

   <One-sentence description of the project.>

   ## Latest session

   @logs/YYYY-MM-DD.md
   ```

## Step 4: Add entry to current list of automation projects

Open `projects/current-list-of-automation-projects.org` and add an entry for the new project under the appropriate section (usually `** In active development`). Follow the exact format used by existing entries:

```org
*** *[[file:<name>/<name>.org][<Project title>]]*
:PROPERTIES:
:ID:       <name>
:END:
  1. Comments: <Brief context — who requested it, why it exists.>
  2. Status: *Kickoff*. <One-sentence summary of where the project stands.>
  3. Next step: <Most immediate TODO.>
```

Use a `file:` link (relative to `projects/`) pointing to the project's org file. For the `:ID:` property, use the kebab-case directory name. Keep comments, status, and next step terse to match the style of surrounding entries.

## Step 5: Commit

Stage all new files under `projects/<name>/` **and** the updated `projects/current-list-of-automation-projects.org`, then commit with message: `Add <name> project`.

## Step 6: Transition

Print the following to the user:

> Project created at `projects/<name>/`.
>
> To start a focused session, exit and run:
> ```
> claude --cwd "projects/<name>"
> ```

Then suggest the user type `/exit` to close the current session.
