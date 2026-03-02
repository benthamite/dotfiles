---
name: try-skills
description: Try third-party skills one by one. Use when the user says "/try-skills". Picks the next untried skill from the list, installs and tests it, then waits for the user's verdict.
---

# Try skills one by one

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

| #  | Skill                    | Repo                                            | Status  | Notes |
|----|--------------------------|-------------------------------------------------|---------|-------|
| 1  | ask-many-models          | HartreeWorks/skill--ask-many-models             | removed | Broken CLI, removed from repo |
| 2  | best-of-n               | HartreeWorks/skill--best-of-n                   | keep    | Works with Gemini + Anthropic keys; no xAI key so Grok fails gracefully |
| 3  | chief-of-staff           | HartreeWorks/skill--chief-of-staff              | revisit | Consider designing a custom daily briefing system inspired by this |
| 4  | day-tracker              | HartreeWorks/skill--day-tracker                 | revisit | Consider designing a custom screenshot-based activity tracker inspired by this |
| 5  | proofread                | HartreeWorks/skill--proofread                   | keep    | Both aspell and Gemini engines work; fixed URL-encoding bug for paths with spaces |
| 6  | project-management       | HartreeWorks/skill--project-management          | untried |       |
| 7  | schedule-task            | HartreeWorks/skill--schedule-task               | untried |       |
| 8  | send-email               | HartreeWorks/skill--send-email                  | untried |       |
| 9  | slack                    | HartreeWorks/skill--slack                       | untried |       |
| 10 | summarise-granola        | HartreeWorks/skill--summarise-granola           | untried |       |
| 11 | download-twitter-video   | HartreeWorks/skill--download-twitter-video      | untried |       |
| 12 | make-image               | HartreeWorks/skill--make-image                  | untried |       |
| 13 | transcribe-audio         | HartreeWorks/skill--transcribe-audio            | untried |       |
| 14 | transcribe-call          | HartreeWorks/skill--transcribe-call             | untried |       |
| 15 | transcribe-twitter-video | HartreeWorks/skill--transcribe-twitter-video    | untried |       |
| 16 | twitter                  | HartreeWorks/skill--twitter                     | untried |       |
| 17 | youtube-download         | HartreeWorks/skill--youtube-download            | untried |       |
| 18 | youtube-transcribe       | HartreeWorks/skill--youtube-transcribe          | untried |       |
| 19 | french-tutor             | HartreeWorks/skill--french-tutor                | untried |       |
| 20 | lesswrong-and-ea-forum   | HartreeWorks/claude-skill--lesswrong-and-ea-forum | untried |       |
| 21 | mochi-srs                | HartreeWorks/skill--mochi-srs                   | untried |       |
| 22 | audit-mac-app            | HartreeWorks/skill--audit-mac-app               | untried |       |
| 23 | secure-mcp-install       | HartreeWorks/skill--secure-mcp-install          | untried |       |
| 24 | save-conversation        | HartreeWorks/skill--save-conversation           | untried |       |
| 25 | save-for-later           | HartreeWorks/skill--save-for-later              | untried |       |
| 26 | share-command            | HartreeWorks/skill--share-command               | untried |       |
| 27 | share-plugin             | HartreeWorks/skill--share-plugin                | untried |       |
| 28 | share-scripts            | HartreeWorks/skill--share-scripts               | untried |       |
| 29 | share-skill              | HartreeWorks/skill--share-skill                 | untried |       |
| 30 | sync-skill-to-claude-desktop | HartreeWorks/skill--sync-skill-to-claude-desktop | untried |       |
| 31 | update-skills            | HartreeWorks/skill--update-skills               | untried |       |
