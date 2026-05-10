---
name: org-note
description: Create or update an org-mode note in the user's notes directory. Use when the user asks to save, capture, draft, create, or revise a note based on conversation content, including put this in my notes requests.
user-invocable: true
---

# Org-mode note

Create or update an org-mode note from the conversation. Distill the material into a useful note rather than dumping raw chat.

## When not to use

- Do not use for project documentation, package manuals, READMEs, or other files outside the notes directory.
- Use `transcribe-note` instead when the primary request is to transcribe an audio or voice note.
- If the user wants externally visible publication content, confirm the target workflow instead of assuming a private note is enough.

## Notes directory

`/Users/pablostafforini/My Drive/notes/`

## Procedure

1. **Identify the title and target file.** Use the user's requested title when provided. If no title is given, infer a concise title from the content; ask only when the title or content is genuinely ambiguous.

2. **Slugify the title.** Run `emacsclient -e '(simple-extras-slugify "TITLE")'` to get the slug. Escape `TITLE` as a valid Elisp string if it contains quotes or backslashes. Use the result minus surrounding quotes as the filename, appending `.org`.

3. **Check if the file exists.** Read the file at the notes directory path. If it exists, you are updating; if not, creating.

4. **Write the content.**
   - Use org-mode markup: `*` headings, `*bold*`, `/italic/`, `[[url][description]]` links, `- ` lists.
   - When creating a new file, start with `#+title: TITLE` followed by a blank line, then the content under a top-level `* TITLE` heading.
   - When updating an existing file, read the existing structure first, then append or merge content under the most relevant heading. Preserve existing content, IDs, and property drawers unless the user explicitly asks for removal.
   - Write substantive, well-organized content. Distill key points from the conversation rather than dumping raw chat.

5. **Generate an org ID for new files.** After creating the note, run:

   ```bash
   emacsclient -e '(with-current-buffer (find-file-noselect "FILEPATH") (goto-char (point-min)) (org-next-visible-heading 1) (org-id-get-create) (save-buffer))'
   ```

   This adds a `:PROPERTIES:` block with an `:ID:` to the top heading and saves it to disk. Escape `FILEPATH` as a valid Elisp string before running the command. Only do this when creating a new file.

6. **Verify.** Re-read the note from disk and confirm:
   - the file path matches the slugified title;
   - a new file has `#+title: TITLE`, a top-level `* TITLE` heading, and an `:ID:` property on that heading;
   - an updated file kept existing content and property drawers unless the user asked for changes;
   - the note is organized org-mode prose, not an unprocessed transcript.

## Final response

Report whether the note was created or updated, give the file path, and mention the verification performed.
