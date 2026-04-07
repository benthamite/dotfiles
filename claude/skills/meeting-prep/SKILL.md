---
name: meeting-prep
description: Generate a pre-meeting org file for the biweekly 1:1 with María. Gathers progress from session logs, project statuses, Slack activity, and GitHub activity since the last meeting. Use when the user says "meeting prep", "prep for María", "prepare for 1:1", "meeting with María", or wants to prepare for their manager meeting.
user-invocable: true
allowed-tools: Read, Write, Edit, Bash, Glob, Grep, Agent, mcp__slack-unofficial-epochai__conversations_search_messages, mcp__slack-unofficial-epochai__channels_list, mcp__slack-unofficial-epochai__conversations_history, mcp__slack-unofficial-epochai__conversations_replies, mcp__google-docs-personal__readDocument, mcp__google-docs-personal__insertText
argument-hint: "[YYYY-MM-DD date override]"
---

# Meeting prep for María 1:1

Generate a pre-populated org-mode meeting file for the biweekly 1:1 with María (Head of Operations at Epoch AI). The file captures everything done since the last meeting, surfaces blockers, and structures the agenda.

## Constants

- **Meetings directory**: `meetings/maria/` (relative to the Epoch project root — the closest ancestor directory containing `CLAUDE.md` with "Epoch AI" in it)
- **Shared meetings Google Doc ID**: `1mTsZWI9ImtI4OAI3XsPI4K6b2cKugTBp2q-blGoR2tE`
- **Slack user ID**: `U0AKT7H6G2H` (Pablo)
- **GitHub username**: `benthamite`
- **GitHub org**: `epoch-research`

## Step 0: Determine dates

1. If `$ARGUMENTS` contains a date, use it as **meeting date**. Otherwise use today's date.
2. Find the **last meeting date** by listing files in the meetings directory and picking the most recent one. Extract the date from the filename (format: `YYYY-MM-DD.org`).
3. Compute the **since date** as the day after the last meeting date (this is the start of the period to search).

## Step 1: Gather information (use parallel agents)

Launch the following data-gathering tasks in parallel using subagents. Each agent should return structured, concise results.

### 1a. Session logs

Read all session log files in the project's `logs/` directory with dates between the since date and today (inclusive). Summarize the key work done, findings, and decisions from each log entry.

### 1b. Project statuses

Read `projects/current-list-of-automation-projects.org`. For each project in the "In active development" section, extract:
- Project name
- Current status
- Next step

Also run `git log --since="SINCE_DATE" --oneline -- projects/current-list-of-automation-projects.org` to see what changed in the project list since the last meeting.

### 1c. Individual project changes

Run `git log --since="SINCE_DATE" --oneline -- projects/` to identify which project directories had commits. For each project with changes, briefly summarize what was done (read the most recent log entries or git diff summaries).

### 1d. Slack activity

Search Slack for messages sent by Pablo since the last meeting:

```
from:U0AKT7H6G2H after:SINCE_DATE
```

Use `conversations_search_messages` from `slack-unofficial-epochai`. Review the results to identify:
- Substantive messages (not just reactions or brief replies)
- Any commitments made ("I'll do X", "will follow up on Y")
- Any questions asked that are still unanswered
- Any threads where Pablo is waiting for a response

Discard noise (greetings, reactions, trivial messages).

### 1e. GitHub activity

Run these commands to find GitHub activity:

```bash
gh search prs --author=benthamite --created=">SINCE_DATE" --json title,url,state,repository --limit 20
gh search issues --author=benthamite --created=">SINCE_DATE" --json title,url,state,repository --limit 20
```

Also check for PRs that are open and awaiting review:

```bash
gh search prs --author=benthamite --state=open --json title,url,repository --limit 20
```

### 1f. Previous meeting action items

Read the previous meeting file. Extract any action items (lines matching `- [ ]` patterns) that are still unchecked. These will be carried forward.

## Step 2: Synthesize progress

From the gathered data, compile a progress summary organized by project. For each project that had activity:

1. **What was done** — concrete accomplishments, using specifics from the logs, commits, and Slack messages.
2. **Current state** — where the project stands now.
3. **What's next** — the immediate next step.

**Link everything.** Whenever a Slack message, Google Doc, GitHub PR/issue, or other external resource is mentioned, include an org-mode link to it. For Slack messages, use the deep link format `https://epochai.slack.com/archives/CHANNEL_ID/pTIMESTAMP` (remove the dot from the message `ts`). For GitHub, link to the PR/issue URL. For Google Docs, link to the doc URL. Never reference a message or document without linking to it.

Also note any cross-cutting activity (e.g., responding to ad-hoc requests, Slack conversations not tied to a specific project).

If there are discrepancies between sources (e.g., the session log says something different from the project status file), flag them — they will be addressed in Step 3.

## Step 3: Update project docs

The gathering phase often reveals that project documentation has drifted from reality. Before generating the meeting file, reconcile the docs with what you just learned.

### 3a. Update individual project org files

For each project that had activity since the last meeting, compare what you gathered (session logs, git history, Slack, GitHub) against the project's org file(s). Update any fields or sections that are stale — e.g., status, next steps, open questions, blockers, links to new PRs/issues. Do not rewrite narrative sections or add speculative information; only update what the evidence supports.

### 3b. Update the master project list

Read `projects/current-list-of-automation-projects.org` and compare it against the gathered data. Update:

- **Status** of each project (e.g., if a project moved from "blocked" to "in progress", or was completed).
- **Next steps** that have changed.
- **New projects** that appeared in the period (e.g., a new repo was created, or a Slack thread kicked off a new initiative) — add them to the appropriate section.
- **Completed or paused projects** — move them to the right section if they're in the wrong one.

### 3c. Commit doc updates

Stage and commit all updated project files with message: `Update project docs based on meeting prep for YYYY-MM-DD`. This commit is separate from the meeting file commit in Step 6.

If no updates were needed, skip this step.

## Step 4: Generate the meeting file

Write the org file to `meetings/maria/YYYY-MM-DD.org` (using the meeting date):

```org
#+title: Meeting with María — YYYY-MM-DD

* Meeting with María — YYYY-MM-DD
** Progress since last meeting (LAST_MEETING_DATE)

[Synthesized progress from step 2, organized by project. Use org sub-headings
(***) for each project. Include specifics — numbers, PR links, deployment dates.
Keep each project entry to 2-5 lines.]

** Blockers and decisions needed

[Extracted from the data: things that are stuck, waiting on someone else,
or where María's input would help. If nothing is blocked, write "None currently."
Do not fabricate blockers.]

** Priorities until next meeting

[Based on the "next step" field from each active project, plus any commitments
made on Slack. List in suggested priority order, but note this is a draft
for the user to reorder.]

** Discussion

[Carry forward any unresolved discussion items from the previous meeting.
Otherwise leave empty for the user to fill in before the meeting.]

** Action items from previous meeting

[Unchecked action items from the last meeting file. If all were completed,
write "All resolved."]

** Action items
** Notes
```

## Step 4b: Update the shared meetings Google Doc

María's shared meetings document (ID in Constants) follows a reverse-chronological format where the newest meeting section is at the top. Each meeting section has a "Pablo" list with one bullet point per active project.

1. Read the shared Google Doc with `readDocument` (format `markdown`) to understand the current structure.
2. From the synthesized progress (Step 2), generate **one bullet point per project that had activity** since the last meeting. Each bullet should be a single sentence in the format: `**Project name**: concise status/headline.` Include only projects with meaningful progress — skip projects where the only activity was waiting.
3. Insert a new meeting section at **index 1** (beginning of the document body) using `insertText`. The section should follow this exact format:

```
---

Attendees:

- Maria's points

- Pablo
  - **Project A**: One-sentence summary.
  - **Project B**: One-sentence summary.
  ...

```

Note the trailing blank line after the last bullet. This ensures clean separation from the previous meeting section below.

## Step 5: Review and open

1. Read the generated file back and verify it looks correct.
2. Open it in Emacs:
   ```bash
   emacsclient -e "(progn (find-file \"FULL_PATH\") (goto-char (point-min)) (org-fold-show-all))"
   ```
3. Tell the user the file is ready for review, and note any items that need their attention (e.g., ambiguous priorities, conflicting information, or items they should add to the Discussion section before the meeting).

## Step 6: Commit

Stage and commit the new meeting file with message: `Add meeting prep for María YYYY-MM-DD`.

$ARGUMENTS
