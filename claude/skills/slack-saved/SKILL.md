---
name: slack-saved
description: Read all Slack "saved for later" messages and present an actionability report. Use when the user says "slack saved", "saved messages", "saved for later", "check saved", "pending slack", or wants to review bookmarked Slack messages.
user-invocable: true
argument-hint: "[--unsave | --no-unsave]"
argument-choices: "--unsave, --no-unsave"
argument-default: --no-unsave
model: opus
---

# Slack saved messages report

Fetch all Slack messages the user has saved for later, assess each for pending actions, and present a structured report in an Emacs org-mode buffer.

## Procedure

### 1. Fetch saved messages

Call `conversations_search_messages` with:
- `search_query: "is:saved"`
- `limit: 100`

If 100 results are returned, paginate using the cursor from the last row until all saved messages are retrieved.

### 2. Resolve channel IDs

Call `channels_list` with `channel_types: "public_channel,private_channel,im,mpim"` and `limit: 999` to build a mapping of channel names to IDs. You need the IDs for Slack deep links.

### 3. Fetch thread context

For each saved message that appears to be part of a thread (i.e. its `ThreadTs` differs from its `MsgID`, or it looks like a reply), call `conversations_replies` to get the full thread. This context is essential for understanding what action — if any — is pending.

Also fetch thread context for root messages that may have received replies since being saved, as the replies may resolve the pending action.

### 4. Classify each message

For each saved message, determine whether it implies a pending action for the user. Classify into:

- **Action required**: the message asks the user to do something, answer a question, make a decision, follow up, or complete a task — and the thread context shows this has **not yet been resolved**. Describe the specific action needed.
- **Waiting on others**: the user has already responded or the ball is in someone else's court, but the item is still open. Note who owns the next step.
- **Resolved / stale**: the thread shows the issue has been addressed, the question answered, or the message is old enough to be irrelevant. Recommend unsaving.
- **Reference**: the message was likely saved as a bookmark for future reference (e.g. a useful link, policy doc, announcement) rather than an action item. Summarize what it contains.

Use the staff directory and project context from CLAUDE.md to understand roles and judge relevance.

### 5. Format the report

Build an org-mode buffer:

```org
#+title: Slack saved messages — YYYY-MM-DD HH:MM

* Action required (N)
** CHANNEL — Summary of pending action
CONTEXT: who said what and why action is needed.
ACTION: specific thing the user should do.
Saved: YYYY-MM-DD
[[https://epochai.slack.com/archives/CHANNEL_ID/pTIMESTAMP][View in Slack]]

* Waiting on others (N)
** CHANNEL — Summary
CONTEXT: what's pending and who owns the next step.
Saved: YYYY-MM-DD
[[https://epochai.slack.com/archives/CHANNEL_ID/pTIMESTAMP][View in Slack]]

* Resolved / stale (N)
** CHANNEL — Summary
WHY: reason this appears resolved or stale.
Saved: YYYY-MM-DD
[[https://epochai.slack.com/archives/CHANNEL_ID/pTIMESTAMP][View in Slack]]

* Reference (N)
** CHANNEL — Summary of bookmarked content
CONTENT: what the saved message contains.
Saved: YYYY-MM-DD
[[https://epochai.slack.com/archives/CHANNEL_ID/pTIMESTAMP][View in Slack]]
```

Guidelines:
- Each entry gets a `**` heading with channel name and a concise summary.
- Below the heading, provide enough context to understand the situation without opening Slack (2-4 sentences).
- For **Action required** items, always include a clear `ACTION:` line stating exactly what to do.
- Include a Slack deep link. Format: `https://epochai.slack.com/archives/CHANNEL_ID/pTIMESTAMP` where TIMESTAMP is the message `ts` with the dot removed. For threaded messages, append `?thread_ts=THREAD_TS&cid=CHANNEL_ID`.
- Include the date the message was sent (as `Saved: YYYY-MM-DD`).
- Show item counts in each category heading.
- Within each category, order by date (newest first).
- Omit empty categories.

### 6. Open in Emacs

Write the report to a temporary file and open it:

```bash
TMPFILE=$(mktemp /tmp/slack-saved-XXXXXX.org)
# (write content to $TMPFILE)
emacsclient -e "(progn (find-file \"$TMPFILE\") (goto-char (point-min)) (org-fold-show-all))"
```

### 7. Handle --unsave flag

If `--unsave` was passed: after presenting the report, ask the user which categories to bulk-unsave (e.g. "Unsave all resolved/stale items?"). Do **not** unsave anything without confirmation.

To unsave messages programmatically, use the Slack internal `saved.delete` API. Retrieve the auth token from 1Password and call for each message:

```bash
XOXC=$(op read "op://Automations/Slack MCP - Epoch Unofficial/xoxc_token")
XOXD=$(op read "op://Automations/Slack MCP - Epoch Unofficial/xoxd_token")

curl -s -X POST "https://slack.com/api/saved.delete" \
  -H "Authorization: Bearer $XOXC" \
  -H "Cookie: d=$XOXD" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "item_type=message&item_id=CHANNEL_ID&ts=MESSAGE_TS"
```

Each call requires `item_type=message`, `item_id=<channel_id>`, and `ts=<message_ts>`. A successful response returns `{"ok":true}`.

If `--no-unsave` was passed (the default): skip this step entirely.
