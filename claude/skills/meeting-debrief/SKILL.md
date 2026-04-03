---
name: meeting-debrief
description: Process the Gemini meeting summary from a María 1:1, extract action items and notes, and populate the meeting org file. Use when the user says "meeting debrief", "debrief María", "process meeting notes", "meeting followup", or wants to fill in the post-meeting sections of a María 1:1 org file.
user-invocable: true
allowed-tools: Read, Write, Edit, Bash, Glob, Grep, Agent, mcp__google-workspace-epoch__query_gmail_emails, mcp__google-workspace-epoch__gmail_get_message_details, mcp__google-workspace-epoch__drive_search_files, mcp__google-workspace-epoch__docs_get_content_as_markdown
argument-hint: "[YYYY-MM-DD date override]"
---

# Meeting debrief for María 1:1

After a 1:1 with María, find the Gemini-generated meeting summary, extract action items and key discussion notes, and populate the corresponding meeting org file.

## Prerequisites

The meeting org file must already exist (created by `/meeting-prep`). If it doesn't, inform the user and suggest running `/meeting-prep` first, then filling in the post-meeting sections manually.

## Constants

- **Meetings directory**: `meetings/maria/` (relative to the Epoch project root)
- **Gmail sender**: `gemini-notes@google.com`
- **Email subject pattern**: `Notes: "Maria <> Pablo" <Mon DD>, <YYYY>`
- **Google Doc name pattern**: `Maria <> Pablo - YYYY/MM/DD HH:MM TZ - Notes by Gemini`

## Step 0: Determine the meeting date

If `$ARGUMENTS` contains a date, use it. Otherwise use today's date. Format as `YYYY-MM-DD` for the org file.

## Step 1: Find the Gemini meeting summary via Gmail

Search for the notification email that Gemini sends after every meeting:

```
from:gemini-notes@google.com subject:"Maria <> Pablo" newer_than:1d
```

Use `mcp__google-workspace-epoch__query_gmail_emails`. If no results, broaden the time range (e.g., `newer_than:3d`) and filter by date manually. If still no results, inform the user that no Gemini notification was found.

## Step 2: Read the notification email

Use `mcp__google-workspace-epoch__gmail_get_message_details` to read the full email.

The email body contains the meeting notes in full:
1. **Summary** ("Resumen") — a brief overview of the meeting
2. **Discussion sections** — topic-level summaries of each discussion area
3. **Suggested next steps** ("Próximos pasos") — action items with assignees in brackets, e.g., `[Maria de la Lama]` or `[Pablo Stafforini]`

The email also references a Google Doc ("Open meeting notes" / "Meeting records Document Notes by Gemini"). The doc contains additional detail (timestamped notes, full transcript) that the email omits.

## Step 3: Find and read the Google Doc

Search Google Drive for the full document to get its file ID and the complete content:

```
name contains 'Maria' and name contains 'Pablo' and name contains 'Notes by Gemini' and name contains 'YYYY/MM/DD'
```

Use `mcp__google-workspace-epoch__drive_search_files`. Then use `mcp__google-workspace-epoch__docs_get_content_as_markdown` to read the document.

The Google Doc typically contains everything in the email **plus**:
- **Details** ("Detalles") — timestamped summaries of each discussion topic (richer than the email sections)
- **Transcript** ("Transcripción") — the full meeting transcript

Use the Google Doc as the primary source since it has more detail. Fall back to the email body if the Doc cannot be found.

## Step 4: Create a Drive shortcut to the Google Doc

Create a shortcut to the Gemini Google Doc in the meetings directory so it appears locally at:

```
/Users/pablostafforini/My Drive/Epoch/meetings/maria/YYYY-MM-DD.gdoc
```

### Background

The Gemini doc is owned by the Epoch account (`pablo@epoch.ai`), but the local "My Drive" mount is the personal account (`pablo.stafforini@gmail.com`). The doc won't appear on the local filesystem unless the personal account can access it. This requires two steps: sharing the doc with the personal account, then creating a shortcut in the personal Drive.

### Constants

- **Personal Drive `maria` folder ID**: `1ifujqJKGgn7x2zIiSIMLgAl7Idgp0CX4`
- **gdrive config dir**: `~/.config/gdrive3/pablo.stafforini@gmail.com/`

### Step 4a: Share the doc with the personal account

Use `gcloud` (authenticated as `pablo@epoch.ai`) to grant the personal account read access:

```bash
TOKEN=$(gcloud auth print-access-token --account=pablo@epoch.ai) && \
curl -s -X POST \
  "https://www.googleapis.com/drive/v3/files/<DOC_ID>/permissions" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"role":"reader","type":"user","emailAddress":"pablo.stafforini@gmail.com"}'
```

If the gcloud token is expired (error about refreshing credentials), ask the user to run `! gcloud auth login --account=pablo@epoch.ai --enable-gdrive-access` and retry.

### Step 4b: Create a shortcut in the personal Drive

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

Replace `<DOC_ID>` with the Google Doc ID from Step 3 and `YYYY-MM-DD` with the meeting date. Google Drive for Desktop will sync the shortcut as a `.gdoc` file.

## Step 5: Extract and structure the information

From the Google Doc (or email body as fallback), extract:

### Action items

Parse the "Próximos pasos" / "Next steps" / "Suggested next steps" section. Each item has an assignee. Convert to org checkbox format:

- Items assigned to Pablo → `- [ ] (Pablo) <action>`
- Items assigned to María → `- [ ] (María) <action>`

Translate action items to English if they are in Spanish. Keep them concise but preserve the essential meaning.

### Discussion notes

From the "Detalles" / "Details" section (or the topic sections in the email), extract the key discussion points. For each:
- Summarize in 1-2 sentences (in English)
- Note any decisions made
- Note any context that would be useful for future reference

Skip small talk, audio issues, and other noise.

### Decisions and commitments

Identify any decisions made or commitments given during the meeting that are not captured as action items but are worth recording (e.g., "María agreed that X approach is fine", "We decided to wait for Y before proceeding with Z").

## Step 6: Read the existing meeting org file

Read `meetings/maria/YYYY-MM-DD.org`. Verify it exists and has the expected structure (sections for Action items and Notes).

## Step 7: Populate the meeting org file

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

## Step 8: Review

Read the updated file back and verify:
1. All action items from the Gemini notes are captured
2. The notes section accurately reflects the discussion
3. No Spanish left untranslated in the action items (notes can include Spanish quotes if useful for context)

Present a brief summary to the user of what was extracted (e.g., "Extracted 5 action items (3 Pablo, 2 María) and 4 discussion points").

## Step 9: Commit

Stage and commit with message: `Add meeting debrief for María YYYY-MM-DD`.

$ARGUMENTS
