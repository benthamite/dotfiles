---
name: build
description: Spec-based build workflow. Use when the user says "/build", "build from spec", "interview me about the spec", "turn this rough spec into a plan", or asks to develop spec.md into an implementation-ready plan. Reads a spec file, interviews the user, writes the finalized spec, and implements it only after explicit confirmation.
---

# Spec-based build

Interview the user about their spec to produce a thorough, detailed specification, then optionally execute it after they confirm. Based on Thariq Shaukat's spec-driven workflow for building large features with Claude Code.

## When this skill is invoked

When triggered for an actual build workflow, follow the execution steps below instead of just describing the skill.

Do not use this skill for ordinary small fixes, code review, bug diagnosis, or open-ended brainstorming where the user has not asked to create or refine a spec.

### Execution steps

#### Step 1: Locate the spec file

If an argument was provided (`$ARGUMENTS`), use that as the spec file path. Otherwise, look for `spec.md` in the current working directory.

If the spec file does not exist, ask the user: "No spec file found. Would you like me to create one? What feature or project are you building?" Then create the spec file with their initial description and proceed to Step 2.

Read the spec file.

#### Step 2: Interview the user

Read the spec file and interview the user in detail. Use the available user-input mechanism: in Claude Code, prefer `AskUserQuestion` for bounded decisions; in Codex Plan mode, prefer `request_user_input` for bounded decisions; otherwise ask concise questions directly in chat. Do not name or rely on unavailable tools.

Ask about technical implementation, UI and UX, constraints, concerns, tradeoffs, edge cases, error handling, data models, API design, testing strategy, deployment, performance, security, accessibility, and any other relevant implementation risks. Make sure the questions are not obvious: ask probing, substantive questions that surface hidden complexity and unstated assumptions.

Guidelines for the interview:

- Ask 1-2 questions at a time to keep the conversation focused
- Build on previous answers; each round should go deeper
- Cover both high-level architecture and low-level implementation details
- Challenge assumptions and suggest alternatives when appropriate
- Continue interviewing until the spec is comprehensive enough to implement without guesswork
- Aim for thoroughness: for large features, this may mean 20-40+ questions
- Stop only when unresolved decisions are answered, explicitly deferred, or captured as implementation assumptions

#### Step 3: Write the spec

Once the interview is complete, write the finalized, detailed spec back to the spec file. The spec should be structured, actionable, and detailed enough that a developer or agent in a new session could implement it without further clarification.

Include, when relevant:

- Goals and non-goals
- User workflows and UX requirements
- Technical design, data model, API, and integration details
- Edge cases, failure modes, security, accessibility, and performance requirements
- Ordered implementation plan
- Testing and verification plan
- Open questions or assumptions that were intentionally deferred

Tell the user: "The spec is ready. You can now start a new session and ask an agent to implement it, or I can proceed with the implementation in this session."

#### Step 4: Execute (if requested)

If the user asks to proceed with implementation in the current session, execute the spec step by step:

1. Break the spec into discrete, ordered tasks
2. Implement each task, committing after each logical unit of work
3. Run tests and verify each step before moving on
4. Flag any spec ambiguities that surface during implementation

When implementation is complete, report the spec path, files changed, commits made, verification performed, and any unresolved issues.

If the user prefers to start a new session, remind them they can simply tell the next agent: "Read spec.md and implement it."
