---
name: org-note
description: Create or update an org-mode note in the user's notes directory. Use when the user asks to save, create, or update a note based on conversation content.
user-invocable: true
---

# Org-mode note

Create or update an org-mode note. The user's message will specify both the title and what content to include.

## Notes directory

`/Users/pablostafforini/My Drive/notes/`

## Procedure

1. **Slugify the title.** Run `emacsclient -e '(simple-extras-slugify "TITLE")'` to get the slug. Use the result (minus surrounding quotes) as the filename, appending `.org`.

2. **Check if the file exists.** Read the file at the notes directory path. If it exists, you are updating; if not, creating.

3. **Write the content.**
   - Use org-mode markup: `*` headings, `*bold*`, `/italic/`, `[[url][description]]` links, `- ` lists.
   - When creating a new file, start with `#+title: TITLE` followed by a blank line, then the content under a top-level `* TITLE` heading.
   - When updating an existing file, append or merge content as appropriate. Do not remove existing content unless asked.
   - Write substantive, well-organized content. Distill key points from the conversation rather than dumping raw chat.

4. **Generate an org ID.** Run `emacsclient -e '(with-current-buffer (find-file-noselect "FILEPATH") (goto-char (point-min)) (org-next-visible-heading 1) (org-id-get-create))'` to add a `:PROPERTIES:` block with an `:ID:` to the top heading. Only do this when creating a new file.
