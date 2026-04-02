---
name: meeting-debrief
description: Process the Gemini meeting summary from a María 1:1, extract action items and notes, and populate the meeting org file. Use when the user says "meeting debrief", "debrief María", "process meeting notes", "meeting followup", or wants to fill in the post-meeting sections of a María 1:1 org file.
user-invocable: true
allowed-tools: Read, Write, Edit, Bash, Glob, Grep, Agent, mcp__google-workspace-epoch__drive_search_files, mcp__google-workspace-epoch__docs_get_content_as_markdown
argument-hint: "[YYYY-MM-DD date override]"
---

# Meeting debrief for María 1:1

After a 1:1 with María, find the Gemini-generated meeting summary in Google Drive, extract action items and key discussion notes, and populate the corresponding meeting org file.

## Prerequisites

The meeting org file must already exist (created by `/meeting-prep`). If it doesn't, inform the user and suggest running `/meeting-prep` first, then filling in the post-meeting sections manually.

## Constants

- **Meetings directory**: `meetings/maria/` (relative to the Epoch project root)
- **Drive search pattern**: Gemini notes are named `Maria <> Pablo - YYYY/MM/DD HH:MM TZ - Notes by Gemini`

## Step 0: Determine the meeting date

If `$ARGUMENTS` contains a date, use it. Otherwise use today's date. Format as `YYYY-MM-DD` for the org file and `YYYY/MM/DD` for the Drive search.

## Step 1: Find the Gemini meeting summary

Search Google Drive for the Gemini notes document:

```
name contains 'Maria' and name contains 'Pablo' and name contains 'Notes by Gemini' and name contains 'YYYY/MM/DD'
```

Use `mcp__google-workspace-epoch__drive_search_files`. If no results, try broadening the search (e.g., drop the date constraint and filter manually by examining the results). If still no results, inform the user that no Gemini notes were found for this date and ask whether to proceed manually.

## Step 2: Read the Gemini document

Use `mcp__google-workspace-epoch__docs_get_content_as_markdown` to read the full document.

The document typically contains (often in Spanish):
1. **Summary** ("Resumen") — a brief overview of the meeting
2. **Next steps** ("Próximos pasos") — action items with assignees in brackets, e.g., `[Maria de la Lama]` or `[Pablo Stafforini]`
3. **Details** ("Detalles") — timestamped summaries of each discussion topic
4. **Transcript** ("Transcripción") — the full meeting transcript

## Step 3: Extract and structure the information

From the Gemini document, extract:

### Action items

Parse the "Próximos pasos" / "Next steps" section. Each item has an assignee. Convert to org checkbox format:

- Items assigned to Pablo → `- [ ] (Pablo) <action>`
- Items assigned to María → `- [ ] (María) <action>`

Translate action items to English if they are in Spanish. Keep them concise but preserve the essential meaning.

### Discussion notes

From the "Detalles" / "Details" section, extract the key discussion points. For each:
- Summarize in 1-2 sentences (in English)
- Note any decisions made
- Note any context that would be useful for future reference

Skip small talk, audio issues, and other noise from the transcript.

### Decisions and commitments

Identify any decisions made or commitments given during the meeting that are not captured as action items but are worth recording (e.g., "María agreed that X approach is fine", "We decided to wait for Y before proceeding with Z").

## Step 4: Read the existing meeting org file

Read `meetings/maria/YYYY-MM-DD.org`. Verify it exists and has the expected structure (sections for Action items and Notes).

## Step 5: Populate the meeting org file

Update the org file using the Edit tool:

### Action items section

Replace the empty `** Action items` section with the extracted action items:

```org
** Action items

- [ ] (Pablo) ...
- [ ] (María) ...
```

### Notes section

Replace the empty `** Notes` section with structured notes:

```org
** Notes
*** Key discussion points
- *Topic*: Summary of discussion. Decision or outcome if any.
- ...

*** Decisions
- Decision or commitment made during the meeting.
- ...

*** Gemini summary
[Include the Gemini-generated summary verbatim (translated to English if in Spanish)
as a reference. This is useful for quick recall.]
```

### Update other sections if needed

- If the Gemini notes reveal blockers or priorities that weren't in the prep, add them to the relevant sections.
- If action items from the previous meeting were discussed and resolved, update their status in the "Action items from previous meeting" section (change `[ ]` to `[X]`).

## Step 6: Review

Read the updated file back and verify:
1. All action items from the Gemini notes are captured
2. The notes section accurately reflects the discussion
3. No Spanish left untranslated in the action items (notes can include Spanish quotes if useful for context)

Present a brief summary to the user of what was extracted (e.g., "Extracted 5 action items (3 Pablo, 2 María) and 4 discussion points").

## Step 7: Commit

Stage and commit with message: `Add meeting debrief for María YYYY-MM-DD`.

$ARGUMENTS
