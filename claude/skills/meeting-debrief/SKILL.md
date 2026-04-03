---
name: meeting-debrief
description: Process a Gemini meeting summary, extract action items and notes, and update the relevant meeting org file. Infers the meeting from today's calendar and the current project context. Use when the user says "meeting debrief", "debrief", "process meeting notes", "meeting followup", or wants to process notes from a recent meeting.
user-invocable: true
allowed-tools: Read, Write, Edit, Bash, Glob, Grep, Agent, mcp__google-workspace-epoch__query_gmail_emails, mcp__google-workspace-epoch__gmail_get_message_details, mcp__google-workspace-epoch__drive_search_files, mcp__google-workspace-epoch__docs_get_content_as_markdown, mcp__google-workspace-epoch__calendar_get_events
argument-hint: "[person name or YYYY-MM-DD date override]"
---

# Meeting debrief

After any meeting, find the Gemini-generated meeting summary, extract action items and key discussion notes, and populate the corresponding org file.

## Constants

- **Epoch project root**: `/Users/pablostafforini/My Drive/Epoch`
- **Meetings base directory**: `meetings/` (relative to Epoch project root)
- **Gmail sender for Gemini notes**: `gemini-notes@google.com`
- **María meetings directory**: `meetings/maria/` (relative to Epoch project root)
- **Personal Drive María folder ID**: `1ifujqJKGgn7x2zIiSIMLgAl7Idgp0CX4`
- **gdrive config dir**: `~/.config/gdrive3/pablo.stafforini@gmail.com/`

## Step 0: Identify the meeting

### From arguments

If `$ARGUMENTS` contains a person's name, use it to filter calendar events. If it contains a date (`YYYY-MM-DD`), use that date instead of today.

### From calendar

Query today's calendar events using `mcp__google-workspace-epoch__calendar_get_events` (use today's date range). List all meetings that have already ended.

### From project context

Examine `$CWD`. If it's under a project directory (e.g., `projects/analytics-aggregation/`), use the project context as a hint: check whether any of today's meetings have attendees or titles related to the project (e.g., a meeting titled "Analytics handoff" or with a known project collaborator).

### Resolution

- If `$ARGUMENTS` names a person, match calendar events by that attendee/title.
- If exactly one meeting matches the combined context (arguments + project + recency), use it.
- If multiple matches, present the list and ask the user to pick.
- If no matches, ask the user which meeting to debrief.

Record: the **meeting title**, **meeting date** (`YYYY-MM-DD`), and **attendee names** (first names only). Identify the primary non-Pablo attendee — this determines the meeting directory name.

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

Search Google Drive for the full document using `mcp__google-workspace-epoch__drive_search_files`:

```
name contains '<key words from meeting title>' and name contains 'Notes by Gemini' and name contains 'YYYY/MM/DD'
```

Then use `mcp__google-workspace-epoch__docs_get_content_as_markdown` to read it.

The Doc typically contains everything in the email **plus**:
- **Details** ("Detalles") — timestamped, richer topic summaries
- **Transcript** ("Transcripción") — full meeting transcript

Use the Google Doc as the primary source. Fall back to the email body if the Doc cannot be found.

Record the **Google Doc ID** for later use (Drive shortcut, linking).

## Step 4: Determine the mode

### María mode

Activates when the meeting title matches a María 1:1 pattern (e.g., contains "Maria" and "Pablo", or the primary attendee is María de la Lama).

- **Target file**: `meetings/maria/YYYY-MM-DD.org` (must already exist from `/meeting-prep`)
- If the file doesn't exist, inform the user and suggest running `/meeting-prep` first.
- Proceed to Step 5.

### General mode

For all other meetings.

- **Target directory**: `meetings/<person>/` where `<person>` is the primary attendee's first name, lowercased (e.g., `meetings/markov/`, `meetings/caroline/`).
- Create the directory if it doesn't exist.
- **Target file**: `meetings/<person>/YYYY-MM-DD.org`.
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

If the file is new, create it with this structure:

```org
#+title: YYYY-MM-DD
#+date: YYYY-MM-DD

* Meeting with <Name> — YYYY-MM-DD
:PROPERTIES:
:GEMINI_DOC_ID: <Google Doc ID>
:MEETING_TITLE: <calendar event title>
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

## Step 7: Drive shortcut (María mode only)

Create a shortcut to the Gemini Google Doc so it appears locally at:

```
/Users/pablostafforini/My Drive/Epoch/meetings/maria/YYYY-MM-DD.gdoc
```

### Background

The Gemini doc is owned by the Epoch account (`pablo@epoch.ai`), but the local "My Drive" mount is the personal account (`pablo.stafforini@gmail.com`). The doc won't appear on the local filesystem unless the personal account can access it.

### Step 7a: Share the doc with the personal account

Use `gcloud` (authenticated as `pablo@epoch.ai`) to grant read access:

```bash
TOKEN=$(gcloud auth print-access-token --account=pablo@epoch.ai) && \
curl -s -X POST \
  "https://www.googleapis.com/drive/v3/files/<DOC_ID>/permissions" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"role":"reader","type":"user","emailAddress":"pablo.stafforini@gmail.com"}'
```

If the gcloud token is expired (error about refreshing credentials), ask the user to run `! gcloud auth login --account=pablo@epoch.ai --enable-gdrive-access` and retry.

### Step 7b: Create a shortcut in the personal Drive

Use gdrive's stored refresh token to get an access token for the personal account, then create a shortcut via the Drive API:

```python
import json, urllib.request, urllib.parse

# Load gdrive credentials
with open('<HOME>/.config/gdrive3/pablo.stafforini@gmail.com/tokens.json') as f:
    tokens = json.load(f)
with open('<HOME>/.config/gdrive3/pablo.stafforini@gmail.com/secret.json') as f:
    secret = json.load(f)

token_data = tokens[0]['token']

# Refresh the access token
data = urllib.parse.urlencode({
    'client_id': secret['client_id'],
    'client_secret': secret['client_secret'],
    'refresh_token': token_data['refresh_token'],
    'grant_type': 'refresh_token'
}).encode()
req = urllib.request.Request('https://oauth2.googleapis.com/token', data=data)
resp = urllib.request.urlopen(req)
access_token = json.loads(resp.read())['access_token']

# Create the shortcut
shortcut_metadata = {
    'name': 'YYYY-MM-DD',
    'mimeType': 'application/vnd.google-apps.shortcut',
    'shortcutDetails': {'targetId': '<DOC_ID>'},
    'parents': ['1ifujqJKGgn7x2zIiSIMLgAl7Idgp0CX4']
}
req2 = urllib.request.Request(
    'https://www.googleapis.com/drive/v3/files',
    data=json.dumps(shortcut_metadata).encode(),
    headers={'Authorization': f'Bearer {access_token}', 'Content-Type': 'application/json'}
)
resp2 = urllib.request.urlopen(req2)
print(json.loads(resp2.read()))
```

Replace `<DOC_ID>` with the Google Doc ID from Step 3 and `YYYY-MM-DD` with the meeting date.

## Step 8: Update the project org file (general mode only)

If `$CWD` is under a project directory (e.g., `projects/analytics-aggregation/`), find the project's main org file (the `.org` file whose name matches the project directory, e.g., `analytics-aggregation.org`).

Read the org file and add a new heading or update an existing meeting-related section with:
- A brief summary of the meeting (1–2 sentences)
- A link to the meeting notes file: `[[file:../../meetings/<person>/YYYY-MM-DD.org][Meeting with <Name> — YYYY-MM-DD]]`
- Key action items and decisions that are relevant to the project (not all action items — only those that affect project direction or next steps)

Place this under the most appropriate existing heading. If the project org file already has a section about collaboration with this person or about the handoff/meeting topic, update that section rather than creating a new one.

Do **not** duplicate the full meeting notes — the project org file should contain a concise project-relevant summary and a link to the detailed notes.

## Step 9: Update the current projects list (general mode only)

Check whether the project has an entry in `/Users/pablostafforini/My Drive/Epoch/projects/current-list-of-automation-projects.org`. If it does, update that entry's **Status** and **Next step** fields to reflect the meeting outcomes. Keep the same terse, numbered-list format used by existing entries. Do not touch entries for other projects.

If the project has no entry, skip this step.

## Step 10: Review

Read the updated files and verify:
1. All action items from the Gemini notes are captured in the meeting file
2. Notes accurately reflect the discussion
3. No untranslated Spanish in action items (notes can include Spanish quotes if useful)
4. (General mode) The project org file links to the meeting notes and captures project-relevant outcomes

Present a brief summary: "Extracted N action items (X Pablo, Y <Name>) and Z discussion points."

## Step 11: Commit

Stage and commit all changed files together:
- María mode: `Add meeting debrief for María YYYY-MM-DD`
- General mode: `Add meeting debrief: <Name> YYYY-MM-DD`

$ARGUMENTS
