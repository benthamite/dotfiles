---
name: org-note-conventions
description: Use when creating or editing an org-mode note file. Applies note-structure and prose-formatting conventions for private or personal Org notes, regardless of directory.
---

# Org Note Conventions

Use these conventions when creating or editing an Org note file.

## Scope

- Use for private or personal Org notes, regardless of directory.
- Do not use for package manuals, project documentation, READMEs, literate config files, or other Org files whose format is governed by a separate project convention.

## Structure

- Use `#+title:` for the note title.
- If the file uses a root note heading, reserve the only level-one heading (`*`) for the note title/root.
- Put top-level content sections under the note root as level-two headings (`**`).
- Do not create sibling level-one content headings unless the existing file already uses that structure or the user asks for it.
- Preserve existing Org structure: property drawers, IDs, tags, TODO keywords, lists, tables, source/example blocks, citations, links, and local variables.

## Prose

- Keep prose paragraphs unfilled: one physical line per paragraph.
- Do not hard-wrap prose to a fill column.
- Preserve structural line breaks in headings, lists, tables, drawers, blocks, and other non-prose Org elements.

## Updating Existing Notes

Follow the file's established structure unless the user asks to normalize it. If the existing structure conflicts with these conventions, make the smallest change that satisfies the user request and state the convention mismatch if it matters.
