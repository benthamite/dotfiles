---
name: mercury-receipts
description: Resolve Mercury "transaction requires additional information" notifications by attaching a receipt and (when supported) setting a note. Use when the user says "mercury receipts", "mercury notification", "process mercury reminders", "submit mercury receipt", or wants to clear pending receipt/note requests on Epoch's Mercury card.
user-invocable: true
---

# Mercury receipts and notes

Epoch's Mercury cards trigger an email notification whenever a charged transaction is missing a receipt and/or a note. This skill resolves those by:

1. Finding the receipt for each pending transaction (per-merchant playbook).
2. Forwarding the receipt to `receipts@mercury.com` so Mercury auto-matches it to the right transaction.
3. Setting the note via the Mercury API (when an API token is available).

The Mercury web app (`app.mercury.com`) is on the Chrome extension's safety denylist, so the browser-driven flow used by other skills (e.g. `afip-invoice`) is not available. Everything must go through email + API.

## Inputs

This skill operates on the unresolved Mercury notification(s) currently in the `pablo@epoch.ai` inbox. Each notification email contains:

- The merchant (e.g. OpenAI, Ahrefs, Twitterapi.io).
- The amount.
- A `View transaction` link of the form `https://app.mercury.com/transactions/{transactionId}?orgUserId=...`. The `transactionId` (UUID) is the only field needed for the API.

## Step 1 — Inventory pending transactions

Search the Epoch inbox:

```bash
~/My\ Drive/dotfiles/claude/bin/gmail.py query \
  'from:hello@mercury.com (subject:"requires additional information" OR subject:"Reminder")' \
  --max=30
```

For each result, fetch the message body with `gmail.py get <id>`. Group by `transactionId` (extract from the `View transaction` URL). De-duplicate first-notification + reminder pairs. The most recent unresolved Mercury thread per `transactionId` is the target.

Filter out transactions that already have a `Mercury Receipts <receipts@mercury.com>` "successfully matched" reply in the same thread or with `in_reply_to` referencing the original notification — those are already done on the receipt side.

## Step 2 — Locate the receipt per merchant

Receipts vary by vendor. The merchant inferred from the subject drives the lookup:

| Merchant | Where the receipt lives | How to fetch |
|---|---|---|
| **OpenAI** (ChatGPT Pro/Plus/Business) | Stripe receipt email to `pablo@stafforini.com`, forwarded to `pablo.stafforini@gmail.com`. | `mcp__gmail-epoch-triage__search_gmail_messages` against `pablo.stafforini@gmail.com` with query `from:(stripe.com OR openai.com) ChatGPT newer_than:5d`. Download the PDF attachment. |
| **OpenAI API (epoch.ai org)** | OpenAI sends "Your OpenAI API account has been funded" to `finance@epoch.ai` and several recipients including `pablo@epoch.ai`. No PDF — body is the receipt. | `~/My Drive/dotfiles/claude/bin/gmail.py query 'from:noreply@tm.openai.com subject:"funded"'`. Save body as PDF (see Step 3 helper). |
| **Ahrefs** | `billing@ahrefs.com` → `pablo@epoch.ai`. PDF attached. | Search `from:billing@ahrefs.com newer_than:7d` in the Epoch inbox. |
| **Twitterapi.io** | No automatic email; receipts are at `https://twitterapi.io/dashboard` (login from `op://Operations/twitterapi.io login` or wherever it's stored). | Skip auto-attachment if no email exists; set the note only. |
| **Other** | Ask the user where the receipt lives if the merchant is unfamiliar. Capture the answer back into this table on completion. |

If the receipt isn't yet in either inbox, it may not have been delivered. Wait or ask the user to forward it manually.

## Step 3 — Forward the receipt to Mercury

Mercury auto-matches by **amount + date** when you reply with an attachment to the original notification thread, or to `receipts@mercury.com`. The matching rule has been verified on past OpenAI/Ahrefs charges (see Apr 1 and Apr 25 thread history).

**Preferred — reply within the original notification thread** (preserves threading):

1. Get the original notification's RFC `Message-Id` and `Thread-ID` via `gmail.py get <id>` (printed in the header block).
2. Download the receipt attachment from the source inbox to a temp file:
   ```
   mcp__gmail-epoch-triage__get_gmail_attachment_content
     return_base64: true
   ```
3. Send via `mcp__gmail-epoch-triage__send_gmail_message`:
   - `user_google_email`: `pablo@epoch.ai`
   - `to`: `receipts@mercury.com`
   - `subject`: `Re: <original subject>`
   - `in_reply_to` + `references`: original `message_id` so Gmail threads correctly
   - `thread_id`: original `thread_id`
   - `body`: empty or your standard signature
   - `attachments`: `[{ "content": "<base64>", "filename": "<merchant>-<YYYY-MM-DD>.pdf", "mime_type": "application/pdf" }]`

If the receipt is the body of an email rather than an attachment (e.g. the OpenAI API funding emails), generate a PDF first:

```bash
# Save the email HTML or text from get_gmail_message_details, then:
wkhtmltopdf input.html /tmp/openai-api-2026-05-01.pdf
# or
pandoc input.html -o /tmp/openai-api-2026-05-01.pdf
```

After sending, expect a reply from `Mercury Receipts <receipts@mercury.com>` titled `Re: Your transaction at <merchant> requires additional information` with `We successfully matched your receipt`. Wait up to 60 seconds and re-search the inbox to confirm the match landed.

## Step 4 — Set the note via Mercury API (optional)

Mercury exposes a transaction-update endpoint that takes `note` and `categoryId`:

```
PATCH https://api.mercury.com/api/v1/transaction/{transactionId}
Authorization: Bearer <token>
Content-Type: application/json

{ "note": "ChatGPT Business subscription (May 2026)" }
```

The token is at `op://Operations/Mercury API Token/credential` (see Setup below if the item doesn't exist).

```bash
TOKEN=$(op read "op://Operations/Mercury API Token/credential")
TX=72f9872a-4513-11f1-8c95-35ac3c3c5ef9
NOTE="ChatGPT Business subscription (May 2026)"
curl -sS -X PATCH "https://api.mercury.com/api/v1/transaction/$TX" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d "$(jq -n --arg n "$NOTE" '{note:$n}')"
```

A 200 with the updated transaction body confirms success.

If `op read` returns `not found`, the API token hasn't been provisioned yet. Skip the note (Mercury will keep reminding, but no harm done) and tell the user that adding the token is a one-time setup. Don't fall back to driving the web UI — it's denylisted and will fail.

### Receipt upload via API (alternative to email forwarding)

The same token allows direct attachment upload, useful when the receipt is already on disk:

```bash
curl -sS -X POST "https://api.mercury.com/api/v1/transaction/$TX/attachments" \
  -H "Authorization: Bearer $TOKEN" \
  -F "file=@/tmp/openai-2026-05-01.pdf" \
  -F "attachmentType=receipt"
```

Returns `{ attachmentId, downloadUrl }`. Prefer this over email-forwarding when the token exists, because matching is exact (no amount/date guessing).

## Note text conventions

Default note format per merchant:

| Merchant | Note text |
|---|---|
| OpenAI ChatGPT Pro/Business | `ChatGPT Business subscription (<Month> <Year>)` |
| OpenAI API | `OpenAI API credit top-up — $<amount>` |
| Ahrefs | `Ahrefs subscription (<Month> <Year>)` |
| Twitterapi.io | `Twitter API usage (<Month> <Year>)` |
| Other | Ask the user. |

## Setup — one-time

### Mercury API token

1. Ricardo (Mercury admin) opens https://app.mercury.com/settings/tokens and creates a token named `Pablo automations`.
2. Token is stored in 1Password (Operations vault):
   - Title: `Mercury API Token`
   - Field: `credential` (concealed)
   - Owner: Pablo
3. `op read "op://Operations/Mercury API Token/credential"` should return the token.

If only Pablo has admin rights, he can do steps 1–2 himself.

### Personal Gmail access for receipts

The OpenAI ChatGPT subscription emails go to `pablo@stafforini.com` and forward to `pablo.stafforini@gmail.com`. The `gmail-epoch-triage` MCP needs OAuth for that account:

1. First call to `mcp__gmail-epoch-triage__search_gmail_messages` with `user_google_email: pablo.stafforini@gmail.com` returns an OAuth URL.
2. The user clicks, signs in as `pablo.stafforini@gmail.com`, and approves.
3. After approval, retry — credentials persist.

## Known limitations

- **`app.mercury.com` is denylisted** in the Chrome extension. Browser-based flows fail at navigate time. Don't attempt them.
- **Note + receipt are decoupled**: receipts are submitted by email (forwarded), notes by API. If the API token is missing, notes can't be set automatically — Mercury will keep sending reminders until the user adds a note in the iOS/Android app.
- **Email matching is amount + date based**, so if two transactions of identical amount happen on the same day, the receipt may attach to the wrong one. Use the API upload path when in doubt.
- **No bulk endpoint** for either notes or receipts — process transactions one at a time.

## Verification

After processing, re-run the Step 1 search and confirm:

- For each handled `transactionId`, a `Mercury Receipts <receipts@mercury.com>` "successfully matched" reply exists in the thread.
- The Mercury API GET `/api/v1/transaction/{transactionId}` returns the expected `note` field (when a token is available).

## End-of-run summary

Report a table of processed transactions:

| transactionId (short) | Merchant | Amount | Receipt | Note |
|---|---|---|---|---|
| 72f9872a | OpenAI | $200.00 | matched | set |

And list any that were skipped with the reason (no receipt yet / no API token / unknown merchant).
