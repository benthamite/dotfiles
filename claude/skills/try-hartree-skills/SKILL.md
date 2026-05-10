---
name: try-hartree-skills
description: Evaluate HartreeWorks third-party skills one at a time. Use when the user says "/try-hartree-skills", asks to try Hartree skills, continue the Hartree skill queue, or record a verdict on a tested Hartree skill. Do not use for general skill discovery, local skill audits, or arbitrary third-party installs.
---

# Try Hartree skills one by one

## Purpose and boundaries

Use this skill only for the HartreeWorks queue below. It is an interactive
evaluation workflow, not a general skill search or installer.

Only install and test one listed skill per turn. Treat an explicit
`/try-hartree-skills` request as authorization to work on the next untried table
entry, but do not clone anything outside this table or continue to a second
candidate without explicit user confirmation.

## Workflow

1. Re-read this file and identify the next table row whose status is blank.
   If no blank rows remain, report that the queue is complete and stop.
2. State the chosen skill and repository before installing it.
3. Install the candidate using the procedure below, then invoke it with a
   realistic, low-risk test that exercises the skill's main workflow.
4. Report what was installed, the exact test performed, the observed result,
   and any limitations or setup gaps.
5. Wait for the user's verdict. Do not update the table until the user decides.
6. After the verdict, update this file with one status and a brief note:
   `keep`, `removed`, `revisit`, or `skipped`.
7. If the verdict removes an installed candidate, move the candidate directory
   to Trash rather than deleting it destructively.
8. Re-read the edited row, inspect `git diff` and `git status --short`, and
   commit the verdict plus any installed/removed skill files as one scoped
   change when the user asked the workflow to make persistent changes.

Use `revisit` when the skill is interesting but not ready to install, such as a
case where a custom local version would be better. Use `skipped` when the skill
is intentionally not tested because its dependencies, credentials, or scope make
it a poor fit.

### How to install a skill

1. Clone the listed repo to `/tmp`, move its `.git` directory to Trash, then
   move the skill to the tool-appropriate tracked skill directory:
   - Claude Code: `claude/skills/<name>/`
   - Codex: `codex/skills/<name>/`
2. For skills that should be kept globally, mirror the final accepted version to
   both `claude/skills/<name>/` and `codex/skills/<name>/` unless the difference
   is intentionally tool-specific and documented.
3. Run `yarn install` or `npm install` if the skill has a `package.json`.
4. If the skill accepts arguments, add `argument-hint: <hint>` to the
   SKILL.md frontmatter for autocompletion.
5. Do **not** use git submodules.

Source: https://github.com/HartreeWorks/skills

## Skills

| Skill                    | Repo                                            | Status  | Notes |
|--------------------------|-------------------------------------------------|---------|-------|
| ask-many-models          | HartreeWorks/skill--ask-many-models             | removed | Broken CLI, removed from repo |
| best-of-n               | HartreeWorks/skill--best-of-n                   | keep    | Works with Gemini + Anthropic keys; no xAI key so Grok fails gracefully |
| chief-of-staff           | HartreeWorks/skill--chief-of-staff              | revisit | Consider designing a custom daily briefing system inspired by this |
| day-tracker              | HartreeWorks/skill--day-tracker                 | revisit | Consider designing a custom screenshot-based activity tracker inspired by this |
| proofread                | HartreeWorks/skill--proofread                   | keep    | Both aspell and Gemini engines work; fixed URL-encoding bug for paths with spaces |
| project-management       | HartreeWorks/skill--project-management          | revisit | Consider designing a custom project scaffolding system inspired by this |
| schedule-task            | HartreeWorks/skill--schedule-task               | revisit | Useful once there are recurring autonomous Claude tasks to schedule |
| send-email               | HartreeWorks/skill--send-email                  | skipped | Requires Gmail OAuth setup; hardcoded author details |
| slack                    | HartreeWorks/skill--slack                       | skipped | Redundant; already have official Slack MCP plugin |
| summarise-granola        | HartreeWorks/skill--summarise-granola           | skipped | Granola-specific; deeply tied to author's project/email workflow |
| download-twitter-video   | HartreeWorks/skill--download-twitter-video      | skipped | Simple yt-dlp wrapper; can do this ad-hoc |
| make-image               | HartreeWorks/skill--make-image                  | skipped | Niche; requires Krea API key |
| transcribe-audio         | HartreeWorks/skill--transcribe-audio            | skipped | Requires parakeet-mlx, fluidaudio, ffmpeg |
| transcribe-call          | HartreeWorks/skill--transcribe-call             | skipped | Requires BlackHole virtual audio driver + transcribe-audio |
| transcribe-twitter-video | HartreeWorks/skill--transcribe-twitter-video    | skipped | Thin wrapper over yt-dlp + transcribe-audio |
| twitter                  | HartreeWorks/skill--twitter                     | skipped | Requires bird CLI + X auth cookies |
| youtube-download         | HartreeWorks/skill--youtube-download            | skipped | Simple yt-dlp wrapper |
| youtube-transcribe       | HartreeWorks/skill--youtube-transcribe          | skipped | Requires yt-dlp + transcribe-audio |
| french-tutor             | HartreeWorks/skill--french-tutor                | skipped | Language-specific; requires mochi-srs |
| lesswrong-and-ea-forum   | HartreeWorks/claude-skill--lesswrong-and-ea-forum | skipped | Digest feature not compelling enough; can query forums ad-hoc |
| mochi-srs                | HartreeWorks/skill--mochi-srs                   | skipped | Requires Mochi API key |
| audit-mac-app            | HartreeWorks/skill--audit-mac-app               | keep    | Shell script using macOS built-ins + npx asar; tested on Discord; 7-phase audit with severity ratings |
| secure-mcp-install       | HartreeWorks/skill--secure-mcp-install          | skipped | Meta-tool for installing MCP servers |
| save-conversation        | HartreeWorks/skill--save-conversation           | skipped | Lightweight but not needed currently |
| save-for-later           | HartreeWorks/skill--save-for-later              | skipped | Session resume registry; not needed currently |
| share-command            | HartreeWorks/skill--share-command               | skipped | Author-specific publishing tool |
| share-plugin             | HartreeWorks/skill--share-plugin                | skipped | Author-specific publishing tool |
| share-scripts            | HartreeWorks/skill--share-scripts               | skipped | Author-specific publishing tool |
| share-skill              | HartreeWorks/skill--share-skill                 | skipped | Author-specific publishing tool |
| sync-skill-to-claude-desktop | HartreeWorks/skill--sync-skill-to-claude-desktop | skipped | Niche bridging tool for Claude Desktop |
| update-skills            | HartreeWorks/skill--update-skills               | skipped | Only useful with submodule-based skills |
