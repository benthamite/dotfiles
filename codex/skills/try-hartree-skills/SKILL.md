---
name: try-hartree-skills
description: Try third-party skills one by one. Use when the user says "/try-hartree-skills". Picks the next untried skill from the list, installs and tests it, then waits for the user's verdict.
---

# Try Hartree skills one by one

## Instructions for Claude

Read this file at the start of the session. Pick the next untried skill from the list below. Install it, then invoke it with a realistic test. Wait for the user to evaluate. After their decision, update this file: change the status to `keep`, `removed`, or `revisit` and add a brief note. Use `revisit` when the skill is interesting but not ready to install — e.g., worth designing a custom version inspired by it. Then move on to the next skill, or stop if the user wants to.

Only install and test one skill per turn. Wait for the user to confirm before moving to the next.

### How to install a skill

1. Clone the repo to `/tmp`, remove `.git`, move to `claude/skills/<name>/`.
2. Run `yarn install` or `npm install` if the skill has a `package.json`.
3. If the skill accepts arguments, add `argument-hint: <hint>` to the SKILL.md frontmatter for autocompletion.
4. Do **not** use git submodules.

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
