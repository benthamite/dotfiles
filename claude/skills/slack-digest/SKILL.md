---
name: slack-digest
description: Pull unread Slack messages, triage for actionability, and present a digest in an Emacs buffer. Use when the user says "slack digest", "check slack", "unread messages", "slack unreads", or wants to catch up on Slack.
user-invocable: true
argument-hint: "[--mark-read | --no-mark-read]"
argument-choices: "--mark-read, --no-mark-read"
argument-default: --mark-read
model: sonnet
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

#### Pablo channels (top priority)

Any channel whose name starts with `pablo-` (e.g. `#pablo-maria`, `#pablo-caroline`) is a direct collaboration channel. If the latest message in such a channel is from someone other than the user (i.e. not `pablo.stafforini`), classify it as **Action required** and place it at the top of the digest — these are the highest-priority items. Note who sent the last message and summarize what they said.

#### AI assistance opportunities

Scan all channels for messages where someone is struggling with an AI tool — Claude (from Slack, Claude Code, claude.ai), ChatGPT, Cursor, Copilot, Gemini, or any other AI assistant. Signals include:

- Error messages or unexpected behavior from an AI tool (e.g. "Claude can't connect to GitHub", "it keeps hallucinating", "the bot isn't responding")
- Frustration or confusion about how to use an AI tool effectively (e.g. "how do I get Claude to do X?", "this isn't working the way I expected")
- Workarounds that suggest the person doesn't know the proper fix (e.g. manually merging branches when a reconnect would solve it)
- Repeated failed attempts to get an AI tool to do something

When you spot one of these, classify it as **Action required** with a note like "AI assistance opportunity — [person] is struggling with [tool/issue]. You may know a fix." Include enough context about the problem for the user to assess whether they can help before opening the thread.

#### Ops-support channels

Apply special handling to any channel whose name starts with `ops-support` (this includes the shared `#ops-support` channel, `#ops-support-temp`, and each team member's personal `#ops-support-<name>` channel). The user is on the ops team and wants visibility into all requests to decide whether to help. For each request posted in these channels:

1. Fetch the thread replies (if any) using `conversations_replies`.
2. Check whether an ops team member has already replied claiming the request (e.g. "I'll take this", "on it", "handling this", or any substantive response indicating ownership).
3. If no one has claimed it, classify it as **Action required** with a note like "Unclaimed ops request — no one has replied yet."
4. If someone has already claimed it, classify it as **Worth knowing** with a note indicating who claimed it and the gist of their response. Never discard ops-support requests as noise.

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

### 6. Save and open in Emacs

Write the digest to a **persistent** file under `~/My Drive/notes/Slack/`, named `YYYY-MM-DD-HHMM-slack-digest.org` using the current local date/time. Never use `/tmp` or `mktemp` — those locations are wiped by macOS and the user loses the digest if the buffer is closed.

```bash
OUTDIR="$HOME/My Drive/notes/Slack"
mkdir -p "$OUTDIR"
OUTFILE="$OUTDIR/$(date +%Y-%m-%d-%H%M)-slack-digest.org"
# (use the Write tool to write the digest content to "$OUTFILE")
emacsclient -e "(progn (find-file \"$OUTFILE\") (goto-char (point-min)) (org-fold-show-all))"
```

The file must be written to disk **before** step 7 (marking channels as read), so that if marking succeeds the user still has a durable record of what was in the unreads.

### 7. Mark conversations as read

If the argument `--mark-read` was passed (this is the default), mark all channels as read without asking. If `--no-mark-read` was passed, skip marking. If neither was passed, mark all channels as read without asking.

To mark as read:
1. First, compile the complete list of channel IDs from step 1 (every channel that had unreads, not just the ones surfaced in the digest). **Include all channels, even those triaged as noise.**
2. For each channel, determine the **latest timestamp** across all messages fetched from that channel (including thread replies fetched in step 3). This is critical: if you fetched thread replies with timestamps later than the root message, use the latest reply timestamp, not the root message timestamp.
3. Call `conversations_mark` for **each** channel ID, passing the latest timestamp as `ts`. Do not skip any. Use parallel tool calls to batch them efficiently — call as many as you can in a single message.
4. After all calls complete, count the number of successful marks and compare against the total. Report the count to the user (e.g. "Marked 23/23 channels as read").
