---
name: meeting-prep
description: Generate a pre-meeting org file for the biweekly 1:1 with María. Gathers progress from session logs, project statuses, Slack activity, and GitHub activity since the last meeting. Use when the user says "meeting prep", "prep for María", "prepare for 1:1", "meeting with María", or wants to prepare for their manager meeting.
user-invocable: true
allowed-tools: Read, Write, Edit, Bash, Glob, Grep, Agent, mcp__slack-unofficial-epochai__conversations_search_messages, mcp__slack-unofficial-epochai__channels_list, mcp__slack-unofficial-epochai__conversations_history, mcp__slack-unofficial-epochai__conversations_replies
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

## Google Docs access

All Google Docs/Drive operations run through the `gdoc` CLI, authenticated as `pablo@epoch.ai` (account name: `epoch`). Always pass `--account epoch`. Do not use `mcp__google-docs-personal__*`.

## Step 0: Determine dates

1. If `$ARGUMENTS` contains a date, use it as **meeting date**. Otherwise use today's date.
2. Find the **last meeting date** by listing files in the meetings directory and picking the most recent one. Extract the date from the filename (format: `YYYY-MM-DD.org`).
3. Compute the **since date** as the day after the last meeting date (this is the start of the period to search).

## Step 1: Reconcile past meeting checkboxes

Before gathering new data, walk backward through past meeting files and tick any `- [ ]` items whose underlying work is now demonstrably complete according to project-file ground truth. This keeps the meeting history aligned with reality and ensures Step 2f (carry-forward) produces a clean list.

### 1a. List candidate files

List every `.org` file in `meetings/maria/` in reverse chronological order. Process at most **10** files in a single run to cap blast radius; log a note if the walk is truncated.

### 1b. Process each file in turn

For each meeting file, starting with the most recent:

1. Read the file and collect every `- [ ]` line under `** Action items` and `** Action items from previous meeting`.
2. If the file has **no unchecked items**, stop the walk — earlier files are assumed reconciled from prior runs. (This is the terminating condition.)
3. Otherwise, for each unchecked item:
   1. **Identify the project.** In order:
      - Match verbatim against any `- [ ] (…) <text> — [[file:…]]` line under a `** Open action items from meetings` heading in project files (these were mirrored by `/meeting-debrief` Step 9).
      - If no verbatim match, infer the project from keywords in the item text (e.g., "media mentions" → `projects/media-mentions-automation/`, "email-triage" → `projects/email-triage/`, "add-on" → `projects/epoch-ai-addon/`).
      - If still ambiguous, **skip the item** (leave unchecked). Do not guess.
   2. **Check for completion.** Read the project's main org file, its `CLAUDE.md` "Latest session" field, and its entry in `projects/current-list-of-automation-projects.org`. Treat any of the following as confirmation:
      - The mirrored checkbox under `** Open action items from meetings` is already `[X]`.
      - The project's `CLAUDE.md` "Latest session" or a recent entry in `logs/` explicitly describes completing this action.
      - The project's Status/Next-step in `current-list-of-automation-projects.org` has moved past this action.
      - For items assigned to others (María, Caroline, etc.): a subsequent meeting file already recorded the outcome (e.g., 2026-04-16 notes `[X] Message Robert…`).
   3. If confirmed, change `- [ ]` to `- [X]` in the meeting file. If the mirrored checkbox in the project file is still `[ ]`, tick it too so both sides stay in sync. If not confirmed, leave the item unchecked.
4. Move to the next-most-recent meeting file and repeat.

### 1c. Commit reconciliation separately

If any meeting or project files were modified, stage and commit them with message: `Reconcile meeting action items through YYYY-MM-DD` (use the meeting date). Keep this commit separate from the doc-update commit (Step 4c) and the meeting-file commit (Step 7). If nothing changed, skip the commit.

## Step 2: Gather information (use parallel agents)

Launch the following data-gathering tasks in parallel using subagents. Each agent should return structured, concise results.

### 2a. Session logs

Read all session log files in the project's `logs/` directory with dates between the since date and today (inclusive). Summarize the key work done, findings, and decisions from each log entry.

### 2b. Project statuses

Read `projects/current-list-of-automation-projects.org`. For each project in the "In active development" section, extract:
- Project name
- Current status
- Next step

Also run `git log --since="SINCE_DATE" --oneline -- projects/current-list-of-automation-projects.org` to see what changed in the project list since the last meeting.

### 2c. Individual project changes

Run `git log --since="SINCE_DATE" --oneline -- projects/` to identify which project directories had commits. For each project with changes, briefly summarize what was done (read the most recent log entries or git diff summaries).

### 2d. Slack activity

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

### 2e. GitHub activity

Run these commands to find GitHub activity:

```bash
gh search prs --author=benthamite --created=">SINCE_DATE" --json title,url,state,repository --limit 20
gh search issues --author=benthamite --created=">SINCE_DATE" --json title,url,state,repository --limit 20
```

Also check for PRs that are open and awaiting review:

```bash
gh search prs --author=benthamite --state=open --json title,url,repository --limit 20
```

### 2f. Previous meeting action items

Read the previous meeting file (already reconciled in Step 1). Extract any action items (lines matching `- [ ]` patterns) that are still unchecked. These will be carried forward.

## Step 3: Synthesize progress

From the gathered data, compile a progress summary organized by project. For each project that had activity:

1. **What was done** — concrete accomplishments, using specifics from the logs, commits, and Slack messages.
2. **Current state** — where the project stands now.
3. **What's next** — the immediate next step.

**Link everything.** Whenever a Slack message, Google Doc, GitHub PR/issue, or other external resource is mentioned, include an org-mode link to it. For Slack messages, use the deep link format `https://epochai.slack.com/archives/CHANNEL_ID/pTIMESTAMP` (remove the dot from the message `ts`). For GitHub, link to the PR/issue URL. For Google Docs, link to the doc URL. Never reference a message or document without linking to it.

Also note any cross-cutting activity (e.g., responding to ad-hoc requests, Slack conversations not tied to a specific project).

If there are discrepancies between sources (e.g., the session log says something different from the project status file), flag them — they will be addressed in Step 4.

## Step 4: Update project docs

The gathering phase often reveals that project documentation has drifted from reality. Before generating the meeting file, reconcile the docs with what you just learned.

### 4a. Update individual project org files

For each project that had activity since the last meeting, compare what you gathered (session logs, git history, Slack, GitHub) against the project's org file(s). Update any fields or sections that are stale — e.g., status, next steps, open questions, blockers, links to new PRs/issues. Do not rewrite narrative sections or add speculative information; only update what the evidence supports.

### 4b. Update the master project list

Read `projects/current-list-of-automation-projects.org` and compare it against the gathered data. Update:

- **Status** of each project (e.g., if a project moved from "blocked" to "in progress", or was completed).
- **Next steps** that have changed.
- **New projects** that appeared in the period (e.g., a new repo was created, or a Slack thread kicked off a new initiative) — add them to the appropriate section.
- **Completed or paused projects** — move them to the right section if they're in the wrong one.

### 4c. Commit doc updates

Stage and commit all updated project files with message: `Update project docs based on meeting prep for YYYY-MM-DD`. This commit is separate from the reconciliation commit (Step 1c) and the meeting file commit (Step 7).

If no updates were needed, skip this step.

## Step 5: Generate the meeting file

Write the org file to `meetings/maria/YYYY-MM-DD.org` (using the meeting date):

```org
#+title: Meeting with María — YYYY-MM-DD

* Meeting with María — YYYY-MM-DD
** Progress since last meeting (LAST_MEETING_DATE)

[Synthesized progress from step 3, organized by project. Use org sub-headings
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

## Step 5b: Update the shared meetings Google Doc

María's shared meetings document (ID in Constants) follows a reverse-chronological format where the newest meeting section is at the top. Each meeting section has a "Pablo" list with one bullet point per active project.

1. Read the shared Google Doc to understand the current structure:
   ```bash
   gdoc cat --account epoch 1mTsZWI9ImtI4OAI3XsPI4K6b2cKugTBp2q-blGoR2tE
   ```
2. From the synthesized progress (Step 3), generate **one bullet point per project that had activity** since the last meeting. Each bullet should be a single sentence in the format: `**Project name**: concise status/headline.` Include only projects with meaningful progress — skip projects where the only activity was waiting.
3. Write the new meeting section to a temporary markdown file, following this exact format (note the trailing blank line for clean separation from the section below):

   ```
   ---

   Attendees:

   - Maria's points

   - Pablo
     - **Project A**: One-sentence summary.
     - **Project B**: One-sentence summary.
     ...

   ```

4. Insert the new section at the top of the first tab using `gdoc insert`:
   ```bash
   gdoc insert --account epoch --tab t.0 --position start \
     1mTsZWI9ImtI4OAI3XsPI4K6b2cKugTBp2q-blGoR2tE /tmp/meeting-prep-section.md
   ```
5. Delete the temporary file once the insert succeeds.

## Step 6: Review and open

1. Read the generated file back and verify it looks correct.
2. Open it in Emacs:
   ```bash
   emacsclient -e "(progn (find-file \"FULL_PATH\") (goto-char (point-min)) (org-fold-show-all))"
   ```
3. Tell the user the file is ready for review, and note any items that need their attention (e.g., ambiguous priorities, conflicting information, or items they should add to the Discussion section before the meeting).

## Step 7: Commit

Stage and commit the new meeting file with message: `Add meeting prep for María YYYY-MM-DD`.

$ARGUMENTS
