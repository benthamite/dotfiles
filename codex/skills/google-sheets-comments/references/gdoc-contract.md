# Installed gdoc comment contract

Checked against installed gdoc 0.21.0 on 2026-09-05, its
`gdoc/api/comments.py`, relevant `gdoc/cli.py` handlers/parser, and the official
Drive references below. Recheck changed installations; this is a scoped
contract, not a guarantee about unobserved account data or the Sheet UI.

## Reads and coverage

- `comments --json` returns an object containing `comments`; `comment-info
  --json` returns an object containing `comment`.
- The wrapper follows comment pagination with a page size of 100.
  `--all` disables client-side resolved filtering. Requests use
  `includeDeleted=False`, so “all” does not include deleted comment history.
- List results include parent ID, plain content, author display name/email
  fields, state/timestamps, quoted content and chronological replies. Its reply
  projection omits IDs. `comment-info` includes reply IDs, content, action,
  author fields and creation times; it does not include every possible API
  field, such as reply modification time, deleted status, assignments, mentions
  or precise anchors. Posting text alone does not prove notification delivery
  or assignment; verify those separately if required.
- Google documents replies on a comment as the full chronological list.
  A missing projected field does not prove the underlying property absent.
  Quoted file content may be missing or ambiguous; do not infer cell coordinates.
- Google does not populate author email/permission ID in these comment/reply
  resources. The installed projection also omits `author.me`. Do not require
  an email match as an impossible verification step or invent identity from a
  display name. Bind the invocation account and new resource ID, then describe
  only the author evidence actually returned.
- Standard text output can hide action-only replies; use JSON for state work.
  Read commands may update local gdoc interaction bookkeeping; this is distinct
  from mutating the Sheet, posting, or marking Gmail notifications read.

## Writes and receipts

- `reply --json` emits `commentId`, `replyId`, and creation status.
- `resolve` and `reopen` create action replies. The current CLI discards their
  reply IDs and prints parent ID plus status; use pre/post reply IDs, action,
  exact message if any, and current state to reconcile. Do not pretend the
  parent ID is a new action receipt.
- All three handlers perform the server write before later version lookup and
  bookkeeping. A failure after the write can lose the receipt or report failure
  despite success. They do not provide an idempotency key. Never automatically
  repeat an uncertain write.
- Literal-safe argv matters. Put options before `--`, then positional IDs/text.
  For a resolve message beginning with a dash, pass one argv element
  `--message=THE_EXACT_MESSAGE`; an option parser can misread a separate
  dash-leading value. Shell examples are templates, not safe interpolation
  recipes. Inspect the final argv's structure without logging private text.

## Inspection and safety

Use installed source or command-specific help to check flags before service
calls. In this installation, top-level `gdoc --help` can auto-update the tool;
do not use it as a supposedly non-mutating diagnostic. Normal dispatch also
checks for updates. Do not run `auth`, reconfigure an account, or mutate the
installed tool as an implicit response to a permission/coverage gap. Use the
mapped service tools; do not bypass them with credential-bearing ad-hoc APIs.

Primary references:

- [comments.list: pagination and deleted coverage](https://developers.google.com/workspace/drive/api/reference/rest/v3/comments/list)
- [Comment: reply ordering, state, author and quote fields](https://developers.google.com/workspace/drive/api/reference/rest/v3/comments)
- [Reply: IDs, actions, author and content](https://developers.google.com/workspace/drive/api/reference/rest/v3/replies)
