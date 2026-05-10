---
name: google-sheets-comments
description: Handle Google Sheets comment review, replies, resolves, reopens, and notification emails correctly. Use this whenever the user asks to process, review, reply to, resolve, reopen, audit, verify, or summarize Google Sheets comments; mentions Google Sheets comment notification emails; asks to handle spreadsheet feedback; or opens/inspects a Google Sheet specifically to deal with comments. Do not use for ordinary spreadsheet editing or non-comment data analysis. This skill prevents treating Gmail notifications as the comment source of truth.
---

# Google Sheets Comments

Use the Drive comment API through `gdoc` as the source of truth for Google
Sheets comments. Gmail notifications are only pointers: they may collapse
multiple Sheet replies into one email, omit current thread state, and email
replies may not appear in the Sheet comment thread.

## When Not To Use

- Do not use this skill for ordinary Sheets data entry, formula work,
  formatting, import/export, or analysis unless the task is about comment
  threads or spreadsheet feedback.
- Do not use it merely because a Sheet is open. The user intent must involve
  comments, replies, resolved state, or comment notifications.
- Do not send email replies to notification messages unless the user explicitly
  asks for that fallback and accepts that it may not update the Sheet thread.

## Source Of Truth

For Epoch/work Sheets, use the Epoch account:

```bash
gdoc comments --account epoch --json --all '<sheet-url-or-id>'
```

If the account is unclear, read the local Google services context first:

```bash
sed -n '1,220p' "$HOME/My Drive/dotfiles/claude/context/google-services.md"
```

Use Gmail only to discover that a Sheet has comment activity or to extract a
Sheet URL/comment hint. After that, switch to `gdoc comments`.

## Processing Workflow

1. Identify the Sheet URL or ID and the Google account to use.
2. Run `gdoc comments --account <account> --json --all '<sheet-url-or-id>'`.
3. Treat the returned Drive comments and replies as the canonical list.
4. Process exactly one comment or reply at a time. Keep the comment ID,
   quoted context, current resolved/open state, and relevant replies visible in
   your notes.
5. Quote the exact comment text to the user, including enough context such as
   `quotedFileContent.value` or the comment ID.
6. Match the requested action:
   - For review, audit, summarize, or verify-only requests, report the current
     state and any proposed next action, then stop before externally visible
     changes.
   - For replies, draft exactly one reply.
   - For resolves, confirm the target thread and draft an optional resolve
     message only if one is useful.
   - For reopens, confirm the target thread and ask for approval.
7. Wait for the user's explicit approval before posting a reply, resolving, or
   reopening. These actions are externally visible.
8. Use only the matching `gdoc` command after approval:

```bash
gdoc reply --account <account> '<sheet-url-or-id>' '<comment-id>' '<reply text>'
gdoc resolve --account <account> '<sheet-url-or-id>' '<comment-id>'
gdoc resolve --account <account> --message '<message>' '<sheet-url-or-id>' '<comment-id>'
gdoc reopen --account <account> '<sheet-url-or-id>' '<comment-id>'
```

9. Verify the posted reply, resolve, or reopen before moving on:

```bash
gdoc comment-info --account <account> --json '<sheet-url-or-id>' '<comment-id>'
```

Confirm that a reply appears in the `replies` array with the expected
author/content, or that the returned comment object shows the expected
resolved/open state.

## Important Pitfalls

- Do not use a Google Docs/Sheets notification email as the source of truth.
- Do not post by emailing a notification `Reply <...@docs.google.com>` address
  unless the user explicitly asks for that fallback and accepts that it may not
  update the Sheet comment thread.
- Do not collapse several replies inside one Drive comment thread into one
  item unless the user explicitly asks for grouped handling.
- Do not create a new comment unless the user explicitly asks for a new
  comment, not a reply to an existing thread.
- If the user says "one by one", handle one Drive comment or reply, ask whether
  to take the proposed action, verify after the action, then continue.
- If you already sent an email reply by mistake, check `gdoc comment-info`
  before claiming it posted. If it is missing, tell the user exactly what
  happened and use `gdoc reply` for the actual Sheet thread after approval.

## Approval Format

Use this structure when asking for approval:

```markdown
Comment `<comment-id>` on `<quoted cell/header>`:
State: `<open|resolved>`

> exact quoted comment

Proposed action: `<reply|resolve|reopen>`

Draft text:

> reply or resolve message, if applicable

Proceed?
```
