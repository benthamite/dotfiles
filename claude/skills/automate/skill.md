---
name: automate
description: Design and build an AI automation. Analyzes the task to decide whether to create a Claude Code skill or a gptel Emacs command, then builds it. Use when the user says "automate", "create automation", "new workflow", or wants AI-assisted automation and isn't sure which tool to use.
argument-hint: <description of what to automate>
---

# Automate: meta-skill for AI workflow design

Given a description of something the user wants to automate with AI, analyze the requirements, choose the right implementation approach, and build it.

## Arguments

`$ARGUMENTS` contains the user's description of what they want to automate. If empty, ask them to describe the workflow.

## Step 1: Analyze the requirements

Consider these dimensions:

| Dimension | Favors gptel (Emacs) | Favors Claude Code skill |
|---|---|---|
| **Context source** | Already in an Emacs buffer (email, code, org entry) | Spread across files, needs discovery |
| **Tool access** | None needed — pure text transformation | Needs filesystem, shell, git, web, MCP |
| **Multi-step reasoning** | Single-shot parse/transform/summarize | Branching logic, iterative exploration |
| **Speed** | Needs to be fast / inline | Latency is acceptable |
| **Emacs integration** | Should modify buffers, use Emacs APIs | Operates on files outside Emacs |
| **Authoring cost** | Requires writing + maintaining Elisp | Just a markdown prompt file |
| **Interactivity** | One-shot, no clarifying questions | May need back-and-forth with the user |

### Sub-types within each approach

**gptel has three sub-types:**
- **Directive**: Just a system prompt preset for a category of conversations. Use when the automation is "talk to AI with specific context/persona." Stored in `gptel-directives` alist.
- **Tool**: A capability gptel can invoke during conversations (via `gptel-make-tool`). Use when the automation is a well-defined function with clear inputs/outputs that the model should be able to call.
- **Command**: A standalone Elisp command using `gptel-request`. Use when the automation is a specific workflow triggered by the user — the Elisp handles context extraction and result insertion, gptel does the AI reasoning in between.

**Claude Code has one type:**
- **Skill**: A markdown file with a prompt and procedure. Can use all Claude Code tools (filesystem, shell, git, web, MCP, subagents).

## Step 2: Present the recommendation

Present your analysis concisely:

1. Which approach you recommend and why (1-2 sentences)
2. If it's a borderline case, explain the tradeoff
3. Ask the user to confirm before building

Do NOT proceed to building without confirmation.

## Step 3: Build it

### If building a Claude Code skill

1. Derive a short kebab-case name for the skill.
2. Create the directory: `~/.claude/skills/<name>/`
3. Write `skill.md` following the conventions of existing skills. Use existing skills as reference:
   - Frontmatter: `name`, `description`, `argument-hint` (if applicable), and `user-invocable: true` unless the skill is only meant to be triggered automatically.
   - Body: clear procedural steps the agent should follow.
4. Tell the user the skill is ready and how to invoke it (`/name` or via trigger phrases in the description).

### If building a gptel directive

1. Draft the directive (a system prompt string).
2. Show it to the user for review.
3. Tell the user to add it to their `gptel-directives` customization. Provide the exact Elisp form:
   ```elisp
   ("name" . "system prompt")
   ```

### If building a gptel tool

1. Write a `gptel-make-tool` form.
2. Show it to the user for review.
3. Tell the user where to add it (in `gptel-extras.el` or their config).

### If building a gptel command

1. Write the Elisp function:
   - Extract context from the appropriate Emacs source (buffer, mu4e message, region, etc.)
   - Call `gptel-request` with a focused system prompt and the extracted context as the user message
   - In the callback, process the response (insert into buffer, create org entry, etc.)
   - Use `interactive` spec so it can be bound to a key or called via `M-x`
2. Show the full function to the user for review.
3. After approval, write it to `gptel-extras.el` at the appropriate location.
4. Tell the user to evaluate it or restart Emacs, and suggest a keybinding if appropriate.

## Important

- When building gptel commands, the response processing (callback) should be done in Elisp, not by asking the model to produce Elisp. The model's job is text transformation; Elisp handles the structured I/O.
- When building Claude Code skills, follow the style of existing skills in `~/.claude/skills/`. Read 2-3 existing skills for reference before writing.
- If the task could reasonably go either way, prefer the approach that minimizes ongoing maintenance. Typically that means: Claude Code skill for complex/evolving workflows, gptel command for stable/well-defined ones.
- Always test what you build. For Claude Code skills, do a dry-run walkthrough. For Elisp, at minimum check that it byte-compiles cleanly via `emacsclient -e '(byte-compile-file "FILE")'`.
