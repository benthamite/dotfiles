---
name: build
description: Spec-based build workflow. Reads spec.md in the current project and interviews the user in depth to flesh it out, then executes the spec. Use when the user says "/build", "build from spec", "interview me about the spec", or wants to turn a rough spec into a detailed plan and implementation.
argument-hint: [spec-file]
---

# Spec-based build

Interview the user about their spec to produce a thorough, detailed specification, then execute it. Based on Thariq Shaukat's spec-driven workflow for building large features with Claude Code.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

### Execution steps

#### Step 1: Locate the spec file

If an argument was provided (`$ARGUMENTS`), use that as the spec file path. Otherwise, look for `spec.md` in the current working directory.

If the spec file does not exist, ask the user: "No spec file found. Would you like me to create one? What feature or project are you building?" Then create the spec file with their initial description and proceed to Step 2.

Read the spec file.

#### Step 2: Interview the user

Read the spec file and interview the user in detail using the `AskUserQuestion` tool about literally anything: technical implementation, UI & UX, concerns, tradeoffs, edge cases, error handling, data models, API design, testing strategy, deployment, performance, security, accessibility, and anything else that is relevant. Make sure the questions are not obvious — ask probing, substantive questions that surface hidden complexity and unstated assumptions.

Guidelines for the interview:

- Ask 1-2 questions at a time to keep the conversation focused
- Build on previous answers — each round should go deeper
- Cover both high-level architecture and low-level implementation details
- Challenge assumptions and suggest alternatives when appropriate
- Continue interviewing continually until the spec is comprehensive and complete
- Aim for thoroughness: for large features, this may mean 20-40+ questions

#### Step 3: Write the spec

Once the interview is complete, write the finalized, detailed spec back to the spec file. The spec should be structured, actionable, and detailed enough that a developer (or Claude in a new session) could implement it without further clarification.

Tell the user: "The spec is ready. You can now start a new session and ask Claude to implement it, or I can proceed with the implementation in this session."

#### Step 4: Execute (if requested)

If the user asks to proceed with implementation in the current session, execute the spec step by step:

1. Break the spec into discrete, ordered tasks
2. Implement each task, committing after each logical unit of work
3. Run tests and verify each step before moving on
4. Flag any spec ambiguities that surface during implementation

If the user prefers to start a new session, remind them they can simply tell Claude: "Read spec.md and implement it."
