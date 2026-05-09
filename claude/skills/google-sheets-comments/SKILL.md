---
name: google-sheets-comments
description: Handle Google Sheets comments and comment notifications correctly. Use this whenever the user asks to process, review, reply to, resolve, or verify Google Sheets comments; mentions Google Sheets comment notification emails; asks to handle spreadsheet feedback; or opens/inspects a Google Sheet specifically to deal with comments. This skill prevents treating Gmail notifications as the comment source of truth.
---

# Google Sheets Comments

Use the Drive comment API through `gdoc` as the source of truth for Google
Sheets comments. Gmail notifications are only pointers: they may collapse
multiple Sheet replies into one email, omit current thread state, and email
replies may not appear in the Sheet comment thread.

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

1. Identify the Sheet URL or ID.
2. Run `gdoc comments --account <account> --json --all '<sheet-url-or-id>'`.
3. Treat the returned Drive comments and replies as the canonical list.
4. Process exactly one comment or reply at a time.
5. Quote the exact comment text to the user, including enough context such as
   `quotedFileContent.value` or the comment ID.
6. Draft one reply.
7. Wait for the user's explicit approval before posting. Replying, resolving,
   or reopening comments is externally visible.
8. Post only with:

```bash
gdoc reply --account <account> '<sheet-url-or-id>' '<comment-id>' '<reply text>'
```

9. Verify the posted reply before moving on:

```bash
gdoc comment-info --account <account> --json '<sheet-url-or-id>' '<comment-id>'
```

Confirm that the new reply appears in the `replies` array with the expected
author/content.

## Important Pitfalls

- Do not use a Google Docs/Sheets notification email as the source of truth.
- Do not post by emailing a notification `Reply <...@docs.google.com>` address
  unless the user explicitly asks for that fallback and accepts that it may not
  update the Sheet comment thread.
- Do not collapse several replies inside one Drive comment thread into one
  item unless the user explicitly asks for grouped handling.
- If the user says "one by one", handle one Drive comment or reply, ask whether
  to post, verify after posting, then continue.
- If you already sent an email reply by mistake, check `gdoc comment-info`
  before claiming it posted. If it is missing, tell the user exactly what
  happened and use `gdoc reply` for the actual Sheet thread after approval.

## Reply Draft Format

Use this structure when asking for approval:

```markdown
Comment `<comment-id>` on `<quoted cell/header>`:

> exact quoted comment

Draft reply:

> reply text

Post this reply?
```
