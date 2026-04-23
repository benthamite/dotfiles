---
name: meeting-debrief
description: Process a Gemini meeting summary, extract action items and notes, and update the relevant meeting org file. Infers the meeting from today's calendar and the current project context. Use when the user says "meeting debrief", "debrief", "process meeting notes", "meeting followup", or wants to process notes from a recent meeting.
user-invocable: true
allowed-tools: Read, Write, Edit, Bash, Glob, Grep, Agent, mcp__google-workspace-epoch__query_gmail_emails, mcp__google-workspace-epoch__gmail_get_message_details, mcp__google-workspace-epoch__calendar_get_events
argument-hint: "[person name, group label (e.g. ops-team), or YYYY-MM-DD date override]"
---

# Meeting debrief

After any meeting, find the Gemini-generated meeting summary, extract action items and key discussion notes, and populate the corresponding org file.

## Constants

- **Epoch project root**: `/Users/pablostafforini/My Drive/Epoch`
- **Meetings base directory**: `meetings/` (relative to Epoch project root)
- **Gmail sender for Gemini notes**: `gemini-notes@google.com`
- **María meetings directory**: `meetings/maria/` (relative to Epoch project root)

## Google Docs access

All Google Docs/Drive operations in this skill run through the `gdoc` CLI, authenticated as `pablo@epoch.ai` (account name: `epoch`). Always pass `--account epoch`. Do not use `mcp__google-docs-personal__*` (personal account) or `mcp__google-workspace-epoch__docs_*` / `drive_*` (superseded by `gdoc`). Gmail and Calendar still use `mcp__google-workspace-epoch__*` since `gdoc` doesn't cover those.

## Step 0: Identify the meeting

### From arguments

`$ARGUMENTS` can contain any combination of:
- A **date** (`YYYY-MM-DD`) — use this date instead of today.
- A **person name** — filter calendar events by attendee.
- A **group label** (e.g. `ops-team`, `all-hands`) — treat as a directory-name override for group meetings with no single primary attendee. An argument is a group label if a directory `meetings/<label>/` already exists, or if it contains a hyphen and doesn't match a calendar attendee's first name.

When a group label is given, filter calendar events by title/recurrence rather than by a single attendee, and skip "primary non-Pablo attendee" resolution in Step 0's Record step.

### From calendar

Query today's calendar events using `mcp__google-workspace-epoch__calendar_get_events` (use today's date range). List all meetings that have already ended.

### From project context

Examine `$CWD`. If it's under a project directory (e.g., `projects/analytics-aggregation/`), use the project context as a hint: check whether any of today's meetings have attendees or titles related to the project (e.g., a meeting titled "Analytics handoff" or with a known project collaborator).

### Resolution

- If `$ARGUMENTS` names a person, match calendar events by that attendee/title.
- If `$ARGUMENTS` is a group label, match by calendar title (e.g. "Ops team sync") or by recurring-event metadata.
- If exactly one meeting matches the combined context (arguments + project + recency), use it.
- If multiple matches, present the list and ask the user to pick.
- If no matches, ask the user which meeting to debrief.

Record: the **meeting title**, **meeting date** (`YYYY-MM-DD`), and **attendee names** (first names only).

Determine the **directory label** (the folder name under `meetings/`):
- If `$ARGUMENTS` included a group label, use it verbatim (e.g. `ops-team`).
- Otherwise, identify the primary non-Pablo attendee and use their first name lowercased (e.g. `markov`).

## Step 1: Find the Gemini meeting summary via Gmail

Search for the Gemini notification email:

```
from:gemini-notes@google.com newer_than:1d
```

Use `mcp__google-workspace-epoch__query_gmail_emails`. Match results against the meeting title from Step 0 — the email subject typically contains the calendar event title and date.

If no results, broaden to `newer_than:3d` and filter manually. If still nothing, inform the user that no Gemini summary was found and ask if they want to write notes manually.

## Step 2: Read the notification email

Use `mcp__google-workspace-epoch__gmail_get_message_details` to read the full email body.

The email contains:
1. **Summary** ("Resumen" / "Summary") — brief overview
2. **Discussion sections** — topic-level summaries
3. **Suggested next steps** ("Próximos pasos" / "Next steps") — action items with assignees in brackets, e.g., `[Maria de la Lama]` or `[Pablo Stafforini]`

The email also references a Google Doc with richer detail (timestamped notes, full transcript).

## Step 3: Find and read the Google Doc

Prefer extracting the Google Doc ID directly from the email body in Step 2 — the Gemini notification always links to the full notes doc. Parse the URL of the form `https://docs.google.com/document/d/<DOC_ID>/edit` and record `<DOC_ID>`.

If the email has no doc link, fall back to searching Drive with `gdoc`:

```bash
gdoc find --account epoch --title --plain "Notes by Gemini"
```

Match results against the meeting title and date from Step 0. You can narrow further by piping to `grep` on the title + date (the filename format is `<meeting title> - YYYY/MM/DD HH:MM TZ - Notes by Gemini`).

Read the doc contents:

```bash
gdoc cat --account epoch <DOC_ID>
```

The Doc typically contains everything in the email **plus**:
- **Details** ("Detalles") — timestamped, richer topic summaries
- **Transcript** ("Transcripción") — full meeting transcript

Use the Google Doc as the primary source. Fall back to the email body if the Doc cannot be found.

Record the **Google Doc ID** for use in the org file's `:GEMINI_DOC_ID:` property.

## Step 4: Determine the mode

### María mode

Activates when the meeting title matches a María 1:1 pattern (e.g., contains "Maria" and "Pablo", or the primary attendee is María de la Lama).

- **Target file**: `meetings/maria/YYYY-MM-DD.org` (must already exist from `/meeting-prep`)
- If the file doesn't exist, inform the user and suggest running `/meeting-prep` first.
- Proceed to Step 5.

### General mode

For all other meetings (1:1 or group).

- **Target directory**: `meetings/<label>/` where `<label>` is the directory label from Step 0 — either a primary attendee's first name (e.g. `markov`, `caroline`) or a group label (e.g. `ops-team`).
- Create the directory if it doesn't exist.
- **Target file**: `meetings/<label>/YYYY-MM-DD.org`.
- If the file already exists, update it. Otherwise, create it.
- Proceed to Step 5.

## Step 5: Extract and structure the information

From the Google Doc (or email body as fallback), extract:

### Action items

Parse the "Próximos pasos" / "Next steps" / "Suggested next steps" section. Each item has an assignee in brackets. Convert to org checkbox format:

- `- [ ] (Pablo) <action>`
- `- [ ] (<Name>) <action>`

Translate to English if in Spanish. Keep concise but preserve essential meaning.

### Discussion notes

From the "Detalles" / "Details" section (or topic sections in the email), extract key discussion points. For each:
- Summarize in 1–2 sentences (in English)
- Note any decisions made
- Note useful context for future reference

Skip small talk, audio issues, and noise.

### Decisions and commitments

Identify decisions or commitments not captured as action items but worth recording (e.g., "María agreed that X approach is fine", "Markov will share the repo by Friday").

## Step 6: Populate the target file

### María mode

The file already has the structure from `/meeting-prep`. Update it using the Edit tool:

**Action items section** — replace the empty `** Action items` content:
```org
** Action items

- [ ] (Pablo) ...
- [ ] (María) ...
```

**Notes section** — replace the empty `** Notes` content:
```org
** Notes
*** Key discussion points
- *Topic*: Summary of discussion. Decision or outcome if any.
- ...

*** Decisions
- Decision or commitment made during the meeting.
- ...

*** Gemini summary
[Verbatim Gemini summary, translated to English if in Spanish]
```

**Previous action items** — if discussed and resolved, change `[ ]` to `[X]`.

### General mode

If the file is new, create it with this structure. For a 1:1, `<Heading>` is "Meeting with <Name>"; for a group meeting, use the calendar event title (e.g. "Ops team sync"). Include an `:ATTENDEES:` property for group meetings so future runs have the attendee list.

```org
#+title: YYYY-MM-DD
#+date: YYYY-MM-DD

* <Heading> — YYYY-MM-DD
:PROPERTIES:
:GEMINI_DOC_ID: <Google Doc ID>
:MEETING_TITLE: <calendar event title>
:ATTENDEES: <comma-separated first names>  ;; group meetings only
:END:

** Action items

- [ ] (Pablo) ...
- [ ] (<Name>) ...

** Notes
*** Key discussion points
- *Topic*: Summary of discussion. Decision or outcome if any.
- ...

*** Decisions
- Decision or commitment made during the meeting.
- ...

*** Gemini summary
[Verbatim Gemini summary, translated to English if in Spanish]
```

If the file already exists (e.g., from a previous run or manual prep), update the existing sections instead of overwriting.

## Step 7: Archive the Gemini notification email

After extracting all information, archive the Gemini notification email (from Step 1) to keep the inbox clean:

```bash
python3 ~/My\ Drive/dotfiles/claude/skills/meeting-debrief/google-workspace-api.py archive-email <EMAIL_ID>
```

Replace `<EMAIL_ID>` with the Gemini email ID from Step 1.

Note: this uses the same OAuth credentials as the `google-workspace-epoch` MCP server (`GOOGLE_WORKSPACE_*` env vars). The server doesn't expose a modify-labels tool, but the refresh token has `gmail.modify` scope. Do NOT use the `gmail-epoch-triage` server (that's for the `email-triage@epoch.ai` bot account).

## Step 8: Mirror action items into project org files

For each project discussed in the meeting that has a directory under `projects/`, find the project's main org file (the `.org` file whose name matches the project directory, e.g., `analytics-aggregation.org`). In 1:1 general mode, the meeting usually concerns a single project (inferred from `$CWD` or attendees). In María mode and group-meeting mode, the meeting often covers multiple projects — update all that had substantive discussion.

The goal is to make every project-relevant action item **trackable as a real checkbox in the project file**, so later reconciliation (via `/meeting-prep`) can verify whether it's been done. This replaces the old practice of burying action items inside prose meeting summaries.

### 8a. Select items to mirror

From the meeting's `** Action items` section, pick every item that is:
- Assigned to Pablo, **or** blocks Pablo's work (e.g., `(Caroline) Complete cybersecurity handoff`), **and**
- Scoped to a specific project that has a directory under `projects/<name>/`.

Skip cross-cutting items that aren't tied to a specific project (e.g., "Adopt a backlog-based prioritization method"). Also skip items assigned to others that Pablo is not waiting on.

### 8b. Append checkboxes to the project file

In each target project org file, find or create a heading named exactly:

```org
** Open action items from meetings
```

Place it at second-outline level, under the project's main top-level heading, near the top of the file (after Purpose/Background but before design or log sections). If the heading already exists, reuse it.

Under this heading, append each selected action item as a checkbox with this **exact** format:

```org
- [ ] (<Assignee>) <verbatim action text> — [[file:../../meetings/<label>/YYYY-MM-DD.org][<Link text> YYYY-MM-DD]]
```

`<label>` is the directory label from Step 0. `<Link text>` is the primary attendee's name for 1:1s, or a human-readable form of the group label for group meetings (e.g. "Ops team" for `ops-team`).

Rules:
- The action text must match the meeting file's checkbox text **verbatim** (same phrasing, punctuation, capitalization). `/meeting-prep` reconciliation relies on exact text match.
- Do **not** duplicate: if a line under this heading already has the same verbatim action text (regardless of which meeting it links to), skip it.
- Do not reword, translate, or summarize. Copy the item as it appears in the meeting file.

### 8c. One-line progress reference (optional)

In addition to the mirrored checkboxes, add a single line under the project's existing progress/log-style heading (create one named `** Meeting references` if none exists) in this format:

```org
- YYYY-MM-DD — [[file:../../meetings/<label>/YYYY-MM-DD.org][Meeting with <Name or group>]]: <1-sentence headline>.
```

This gives narrative context. If a line for the same date already exists, skip.

### 8d. Do not duplicate other content

Do **not** copy discussion notes, decisions, or the Gemini summary into the project file — those stay in the meeting file only. The project file receives (i) mirrored action-item checkboxes under `** Open action items from meetings`, and (ii) one line under `** Meeting references`.

## Step 9: Update the current projects list

For each project updated in Step 8, check whether it has an entry in `/Users/pablostafforini/My Drive/Epoch/projects/current-list-of-automation-projects.org`. If it does, update that entry's **Status** and **Next step** fields to reflect the meeting outcomes. Keep the same terse, numbered-list format used by existing entries. Do not touch entries for other projects.

If a project has no entry, skip it.

## Step 10: Review

Read the updated files and verify:
1. All action items from the Gemini notes are captured in the meeting file
2. Notes accurately reflect the discussion
3. No untranslated Spanish in action items (notes can include Spanish quotes if useful)
4. Project org files link to the meeting notes and capture project-relevant outcomes

Present a brief summary: "Extracted N action items (X Pablo, Y <Name>) and Z discussion points."

## Step 11: Commit

Stage and commit all changed files together:
- María mode: `Add meeting debrief for María YYYY-MM-DD`
- General mode (1:1): `Add meeting debrief: <Name> YYYY-MM-DD`
- General mode (group): `Add meeting debrief: <group label> YYYY-MM-DD` (e.g. `ops-team 2026-04-22`)

$ARGUMENTS
