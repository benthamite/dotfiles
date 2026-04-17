---
name: ai-journal
description: Scan recent Claude Code and Codex sessions, identify AI use cases worth writing about, and help draft informal journal entries for stafforini.com/ai. Use when the user says "ai journal", "journal candidates", "ai diary", "study diary", or wants to turn recent sessions into journal posts.
user-invocable: true
---

# AI journal

Scan Claude Code and Codex sessions since the last run, surface interesting AI use cases (novel prompting, clever skill combinations, debugging surprises, meta-observations about where AI helps or fails, automations built), present them to the user for selection, and draft informal study-diary posts for publication at **stafforini.com/ai**.

## Locations

- **Skill directory**: `~/My Drive/dotfiles/claude/skills/ai-journal/`
- **State file**: `last-run.txt` in the skill directory (ISO 8601 UTC timestamp)
- **Scan script**: `find-sessions.py` in the skill directory
- **Website**: `~/My Drive/repos/stafforini.com/`
- **Journal section**: `content/ai/` (create on first run)
- **Hugo config**: `hugo.toml`

## Session sources

- Claude Code: `~/.claude/projects/<slug>/*.jsonl` (includes subagent sessions under `<session-id>/subagents/`). `~/.claude-epoch/projects` symlinks to the same tree; the scan script deduplicates automatically.
- Codex: `~/.codex/sessions/YYYY/MM/DD/rollout-*.jsonl`

## Procedure

### 1. Determine cutoff

Read `last-run.txt`. If the file is missing or empty, ask the user how far back to scan (e.g. "last 7 days", "since 2026-04-01"). Do **not** assume a default.

Convert the user's answer to an ISO 8601 UTC timestamp before invoking the scan script.

### 2. First-run journal setup

If `~/My Drive/repos/stafforini.com/content/ai/` does not exist:

1. Create `content/ai/_index.md` with:
   ```
   +++
   title = "AI journal"
   description = "An informal study diary about using AI tools better."
   +++
   ```
2. Add a menu entry to `hugo.toml`, placed before the "About" entry:
   ```toml
   [[menus.main]]
     name = "AI"
     pageRef = "/ai/"
     weight = 3
   ```
   Bump the weight of the "About" entry to 4.
3. Ask the user to confirm the section title and description before committing. Commit the setup as a separate commit (`ai-journal: scaffold /ai section`).

### 3. Scan sessions

Run the scan script with the cutoff:

```bash
python3 ~/.claude/skills/ai-journal/find-sessions.py "<cutoff-iso>"
```

Output is tab-separated, one row per session:

```
kind<TAB>start_iso<TAB>cwd<TAB>path<TAB>first_user_excerpt
```

The excerpt is the first non-wrapper user message, flattened and truncated to 300 chars. Sessions are sorted by start timestamp ascending. `kind` is `claude` or `codex`.

If the scan returns many sessions (say, >30), delegate the triage in step 4 to a subagent so the main context isn't flooded with session content. See `subagent-prompt.md` below for a template.

### 4. Triage for candidates

Read promising sessions in full and extract candidates. Look for:

- **Novel prompting techniques** or workflow patterns the user discovered
- **Clever tool/skill combinations** (e.g. chaining skills, unusual subagent uses, MCP + bash interplay)
- **Debugging surprises** where AI succeeded or failed in unexpected ways, especially with a clear lesson
- **Meta-observations** about when AI helps vs. fails on specific kinds of tasks
- **Automations built** (hooks, skills, gptel commands, scripts)
- **Iteration moments** where the user refined a skill/hook/prompt to get a better result, and the diff between before and after is informative

Skip noise: routine refactors, one-shot commits, boilerplate Q&A, sessions that ended with "never mind" or no clear insight, sessions that duplicate an already-selected topic.

Group related sessions: if the user worked on the same skill/feature across multiple sessions, surface it once as a single candidate.

### 5. Present candidates

Show the user a numbered list. For each candidate:

- **Title** — snappy, informal, blog-post-style
- **Hook** — one sentence: why this is interesting for the journal
- **Origin** — date + cwd (trimmed) + kind (`Claude Code` / `Codex`) + session path
- **Notes** — 2-4 bullets on what actually happened and what was learned

Format example:

```
## 1. Teaching Claude to debug without hallucinating fixes

**Hook**: Found a prompting pattern that reliably pushes Claude to investigate before patching.

**Origin**: 2026-04-10, `~/repos/consensus-trader` (Claude Code)
`/Users/.../consensus-trader/949744ea-....jsonl`

- Bug 3 session: Claude kept proposing speculative fixes
- Added a "state your diagnosis before the fix" step to the prompt
- Reduced round-trips from 6 → 2 on the next bug
- Worth writing up the exact prompt scaffold
```

Then ask: "Which should I draft? (e.g. `1, 3, 4` or `all` or `none`)"

### 6. Draft posts

For each selected candidate, create `~/My Drive/repos/stafforini.com/content/ai/<slug>.md`.

**Slug**: lowercase, hyphen-separated, kept short. Use `emacsclient -e '(simple-extras-slugify "TITLE")'` if unsure.

**Front matter** (match existing site style):

```
+++
title = "<title>"
author = ["Pablo Stafforini"]
date = YYYY-MM-DD
lastmod = YYYY-MM-DD
draft = false
tags = ["ai-journal"]
+++
```

**Body**: 300-800 words, first-person, informal study-diary tone. Structure flexibly but typically:

- What I was trying to do (1 short paragraph)
- What I tried first and why it didn't fully work
- What I changed and why
- What I learned / would do differently / still unsure about
- Concrete artefacts: exact prompts, commands, code snippets, diffs — readers learn more from the specifics than the abstractions

**Tone**: conversational, honest about dead ends, not performative. The user values epistemic honesty — if a technique only worked once or the lesson is provisional, say so.

**Do not fabricate**. Every claim about what happened must be grounded in the session transcript. If you need context that isn't in the session, ask the user before writing.

Read an existing note (e.g. `content/notes/agent-log.md`) if unsure about the site's markdown conventions.

### 7. Update state and commit

After drafts are written and the user approves:

1. Update `last-run.txt` with the current UTC timestamp:
   ```bash
   date -u +"%Y-%m-%dT%H:%M:%SZ" > ~/.claude/skills/ai-journal/last-run.txt
   ```
2. Commit in `~/My Drive/repos/stafforini.com`: one commit per post, scope `ai-journal`, description lowercase imperative (e.g. `ai-journal: add post on prompting claude to diagnose before patching`).
3. Commit the `last-run.txt` bump in the dotfiles repo: `ai-journal: bump last-run to <date>`.

If the user declines all candidates, still bump `last-run.txt` — they've been reviewed.

### 8. Confirm before shared-system actions

Do **not** push, deploy, or publish. Leave the posts committed locally. Tell the user what was created and let them decide when to push.

## Subagent prompt template

When session count is large, delegate to a subagent. Paste or adapt this prompt:

> You are reviewing Claude Code and Codex session transcripts to surface interesting use cases for Pablo's AI journal at stafforini.com/ai.
>
> **Criteria** (any one qualifies a session as a candidate):
> - Novel prompting technique or workflow pattern
> - Clever tool/skill/subagent combination
> - Debugging surprise (positive or negative) with a clear lesson
> - Meta-observation about when AI helps vs. fails
> - Automation built (skill, hook, script)
> - Iteration on a prompt/skill where the before→after diff is informative
>
> **Skip**: routine refactors, one-shot commits, boilerplate Q&A, abandoned sessions, noise.
>
> **Input**: the following list of session paths with first-user-message excerpts. Read the full `.jsonl` for any session that looks promising, extract the insight, and return candidates in this format:
>
> ```
> ## <snappy title>
> **Hook**: <one sentence>
> **Origin**: <date>, <cwd>, <kind>, <path>
> - bullet 1
> - bullet 2
> ```
>
> Group related sessions into a single candidate when they cover the same topic. Aim for 3-10 candidates total. Under 600 words.
>
> Sessions:
> ```
> <tsv output of find-sessions.py>
> ```

## Notes

- Sessions may include client names, repo details, or private info. Per user preference, do **not** pre-filter or anonymize — surface candidates as-is and the user will flag anything that needs redaction before publication.
- `first_user_excerpt` can be a `/resume` continuation dump or handoff context (`## Context ...`). That's still a real session; include it if the follow-on work is interesting.
- File mtime is the filter, so sessions that started before the cutoff but had activity after are included (correct behavior).
- If the user ran the skill yesterday and a session is still open today, it will reappear — that's fine; the candidate dedupe in step 4 handles it.
