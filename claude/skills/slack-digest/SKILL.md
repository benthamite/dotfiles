---
name: slack-digest
description: Pull unread Slack messages, triage for actionability, and present a digest in an Emacs buffer. Use when the user says "slack digest", "check slack", "unread messages", "slack unreads", or wants to catch up on Slack.
user-invocable: true
argument-hint: "[--mark-read | --no-mark-read]"
---

# Slack digest

Pull all unread Slack messages, triage them for relevance and actionability, and present a digest in an Emacs org-mode buffer. Ignore noise; surface only what the user may want to know or act on.

## Procedure

### 1. Fetch unreads

Call `conversations_unreads` with:
- `include_messages: true`
- `max_channels: 100`
- `max_messages_per_channel: 20`
- `include_muted: false`

### 2. Resolve channel IDs

The unreads response contains channel names but not IDs. Call `channels_list` with `channel_types: "public_channel,private_channel,im,mpim"` and `limit: 999` to get a full mapping of channel names to IDs (including DMs and group DMs). You will need the IDs to construct Slack deep links in step 5. **Do not guess or fabricate channel IDs.**

### 3. Fetch thread context where needed

For messages that appear to be mid-thread or are replying to something (i.e. they have a `ThreadTs` that differs from their `MsgID`), call `conversations_replies` to get the full thread so you can understand the context.

### 4. Triage

For each message, classify it into one of:

- **Action required**: the user is mentioned, asked a question, assigned a task, or needs to respond.
- **Worth knowing**: notable news, decisions, announcements, or developments relevant to the user's work. Use the project context in CLAUDE.md to judge relevance.
- **Noise**: routine standups, bot messages, casual chatter, reactions-only, messages in channels the user is passively in. Skip these entirely.

Discard anything classified as noise.

### 5. Format the digest

Build an org-mode buffer with this structure:

```org
#+title: Slack digest — YYYY-MM-DD HH:MM

* Action required
** CHANNEL — Summary of actionable item
CONTEXT AND DETAILS
[[https://epochai.slack.com/archives/CHANNEL_ID/pTIMESTAMP][View in Slack]]

* Worth knowing
** CHANNEL — Summary
CONTEXT AND DETAILS
[[https://epochai.slack.com/archives/CHANNEL_ID/pTIMESTAMP][View in Slack]]
```

Guidelines for the content:
- Each entry gets a `**` heading with the channel name and a concise summary.
- Below the heading, provide enough context to understand the message without opening Slack (1-3 sentences).
- Include a Slack deep link. The link format is: `https://epochai.slack.com/archives/CHANNEL_ID/pTIMESTAMP` where `TIMESTAMP` is the message `ts` with the dot removed (e.g. `1773429414.836549` becomes `p1773429414836549`). For threaded messages, append `?thread_ts=THREAD_TS&cid=CHANNEL_ID`.
- Group entries by the two categories above. Within each category, order by importance.
- If a category has no entries, omit it.

### 6. Open in Emacs

Write the digest to a temporary file and open it in Emacs:

```bash
TMPFILE=$(mktemp /tmp/slack-digest-XXXXXX.org)
# (write content to $TMPFILE)
emacsclient -e "(progn (find-file \"$TMPFILE\") (goto-char (point-min)) (org-fold-show-all))"
```

### 7. Mark conversations as read

If the argument `--mark-read` was passed, mark all channels as read without asking. If `--no-mark-read` was passed, skip marking. If neither was passed, ask the user: "Mark all conversations as read?"

To mark as read, call `conversations_mark` on every channel that had unreads (not just the ones surfaced in the digest — mark **all** channels that were fetched in step 1).
