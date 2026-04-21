---
name: slack-emacs
description: Open Slack links/threads in Emacs and handle drafting replies to Slack messages. Use when the user says "open slack", "open slack thread", "open slack message", "open the slack link", "draft a slack reply", "reply to this slack message", "reply to the slack thread", or whenever the task involves drafting a reply to a specific Slack message or opening a Slack permalink. Also trigger when the user provides a Slack URL (epochai.slack.com/archives/...) and wants to act on it.
---

# Slack in Emacs

## When to invoke this skill

- The user wants to open a Slack message, thread, channel, or permalink in Emacs.
- The user asks you to draft a reply to a Slack message (even if they don't mention Emacs). Drafting a reply **always** requires opening the thread in Emacs so the user can paste the draft directly into the thread — this is a hard rule, not an optional step.
- The user shares a Slack URL (`https://epochai.slack.com/archives/...`) and asks you to do anything with it beyond reading its content programmatically via the Slack MCP tools.

## Opening a Slack permalink in Emacs

Use `slack-browse-url` via `emacsclient`:

```bash
emacsclient --no-wait --eval '(progn (slack-browse-url "PERMALINK") nil)'
```

The trailing `nil` suppresses the return value that would otherwise clutter the shell output.

### Permalink format

- **Message:** `https://epochai.slack.com/archives/<channel-id>/p<ts-without-dot>`
  - Example: ts `1776265426.017059` becomes `p1776265426017059`.
- **Threaded message:** append `?thread_ts=<thread-ts>&cid=<channel-id>`.
  - Example: `https://epochai.slack.com/archives/C0APL0BFNE7/p1776265509202259?thread_ts=1776265426.017059&cid=C0APL0BFNE7`

If you only have `channel_id` and `ts` from an MCP call, construct the permalink yourself — do not ask the user to supply it.

## Drafting a reply to a Slack message

When the user asks you to draft a reply to a specific Slack message or thread:

1. Draft the reply.
2. Push the draft to the Emacs kill ring (see "Copying drafts" below). This also syncs to the system clipboard, and has the advantage that the draft survives in kill-ring history (accessible via `M-y`) even if the user copies something else in the meantime.
3. **Open the thread in Emacs** using `slack-browse-url` so the user can paste the draft directly into the thread input.

Steps 2 and 3 are both required every time. Do not skip step 3 just because the user hasn't explicitly asked for it — it's the default behavior for this operation.

### Copying drafts

Prefer the Emacs kill ring over `pbcopy`. `select-enable-clipboard` is `t` and `interprogram-cut-function` is `gui-select-text`, so `kill-new` updates both the kill ring and the system clipboard in one go.

For multi-line drafts, write to a temp file and read it back inside Emacs to avoid elisp string-escaping pain:

```bash
TMPFILE=$(mktemp)
cat > "$TMPFILE" <<'EOF'
multi
line
draft
EOF
emacsclient --eval "(with-temp-buffer (insert-file-contents \"$TMPFILE\") (kill-new (buffer-string)))" >/dev/null
rm "$TMPFILE"
```

## Do not trigger this skill for

- Reading Slack content via MCP tools (`conversations_history`, `conversations_replies`, `conversations_search_messages`, etc.) — those don't require Emacs.
- Sending a message via the Slack MCP — not currently supported in this workflow; the user pastes manually in Emacs.
- General Slack digests / unreads — use `slack-digest` or `slack-saved` instead.
