---
name: automate
description: Design and build a reusable AI automation. Use for automate, create automation, turn this into a skill/command, new workflow, AI helper, or deciding whether a workflow should become a skill, directive, tool, or Emacs command.
argument-hint: <description of what to automate>
---

# Automate: meta-skill for AI workflow design

Given a description of something the user wants to automate with AI, analyze the requirements, choose the right implementation approach, and build it.

## Arguments

`$ARGUMENTS` contains the user's description of what they want to automate. If empty, ask them to describe the workflow.

## Step 1: Analyze the requirements

Consider these dimensions:

| Dimension | Favors gptel (Emacs) | Favors agent skill |
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

**Agent skills have two local variants:**
- **Claude Code skill**: A markdown file under `claude/skills/<name>/SKILL.md`. Use when the workflow is meant for Claude Code.
- **Codex skill**: A markdown file under `codex/skills/<name>/SKILL.md`. Use when the workflow is meant for Codex.

Honor an explicit target if the user names one. If they do not, recommend the option that best fits the workflow and the environment they are currently using.

## Step 2: Present the recommendation

Present your analysis concisely:

1. Which approach you recommend and why (1-2 sentences)
2. If it's a borderline case, explain the tradeoff
3. The concrete files you expect to create or edit
4. Ask the user to confirm before building, unless they already explicitly asked you to implement the chosen approach

If required details are missing, ask focused questions before building. Do not ask the user to add code or move files manually when you can make the change yourself.

## Step 3: Build it

Work in the repository's canonical paths. In this dotfiles repo, `~/.claude/skills` and `~/.codex/skills` are symlinks into `claude/skills` and `codex/skills`; edit the dotfiles paths directly.

### If building an agent skill

1. Derive a short kebab-case name for the skill.
2. Choose the target root:
   - Claude Code: `claude/skills/<name>/`
   - Codex: `codex/skills/<name>/`
3. Read 2-3 nearby skills for conventions, plus `skill-creator` if you are creating a skill from scratch.
4. Write `SKILL.md` following the conventions of existing skills:
   - Frontmatter: `name`, `description`, and optional fields already used by that tool's local skills.
   - Body: clear procedural steps the agent should follow.
5. If you change `claude/`, update `claude/README.org` when the repo instructions require it.
6. Tell the user the skill is ready and how to invoke it, either by slash command if supported or by trigger phrases in the description.

### If building a gptel directive

1. Draft the directive (a system prompt string).
2. Inspect the local gptel configuration to decide whether this should be a directive, preset, or package-level custom variable.
3. After approval, add it to the canonical Emacs config file yourself. In this repo, likely locations are `emacs/config.org` for gptel setup and `emacs/extras/gptel-extras.el` for reusable package code.
4. Use the exact Elisp form when appropriate:
   ```elisp
   ("name" . "system prompt")
   ```

### If building a gptel tool

1. Write a `gptel-make-tool` form.
2. Keep side effects explicit in the tool description and require confirmation before destructive or externally visible actions.
3. After approval, add it to `emacs/extras/gptel-extras.el` or the relevant gptel config block, following nearby patterns.

### If building a gptel command

1. Write the Elisp function:
   - Extract context from the appropriate Emacs source (buffer, mu4e message, region, etc.)
   - Call `gptel-request` with a focused system prompt and the extracted context as the user message
   - In the callback, process the response (insert into buffer, create org entry, etc.)
   - Use `interactive` spec so it can be bound to a key or called via `M-x`
2. Show the full function to the user for review when the behavior or insertion point is not already clear.
3. After approval, write it to `emacs/extras/gptel-extras.el` or the relevant package file.
4. Add a keybinding only when the user requested one or an existing nearby convention clearly applies.

## Step 4: Verify and close out

Verify the artifact before calling it done:

- Agent skill: re-read the edited `SKILL.md`, run the local skill resolver if available, and do a dry-run walkthrough against the user's example prompt.
- Emacs Lisp: byte-compile changed `.el` files, run relevant ERT tests when available, and tangle `emacs/config.org` if you changed it.
- Documentation: update required docs for touched subsystems.
- Git: commit each logical change unless the user explicitly asked not to.

Final response:

- State the chosen automation type and why.
- List changed files.
- Include the verification performed.
- Mention any unresolved setup or follow-up.

## Important

- When building gptel commands, handle response processing in Elisp, not by asking the model to produce Elisp. The model's job is text transformation; Elisp handles the structured I/O.
- If the task could reasonably go either way, prefer the approach that minimizes ongoing maintenance. Typically that means: Claude Code skill for complex/evolving workflows, gptel command for stable/well-defined ones.
- Preserve user confirmation boundaries for external side effects: do not post messages, open PRs, send email, or take other externally visible actions unless the user explicitly confirmed that behavior.
