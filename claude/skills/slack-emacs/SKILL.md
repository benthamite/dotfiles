---
name: slack-emacs
description: Read Slack permalinks programmatically and handle guarded Emacs reply drafting. Use when the user says "open slack", "open slack thread", "open slack message", "open the slack link", "draft a slack reply", "reply to this slack message", "reply to the slack thread", or whenever the user provides a Slack URL (epochai.slack.com/archives/...) and wants to act on it.
---

# Slack permalinks and Emacs

## When to invoke this skill

- The user shares a Slack URL (`https://epochai.slack.com/archives/...`) and wants to read, investigate, summarize, or act on it.
- The user asks you to draft a reply to a Slack message (even if they don't mention Emacs). Drafting a reply **always** requires opening the thread in Emacs so the user can paste the draft directly into the thread — this is a hard rule, not an optional step.

## Reading a Slack permalink

For reading or investigation, never open Slack UI. Fetch the permalink via the Slack API wrapper:

```bash
~/My\ Drive/dotfiles/claude/bin/slack.py permalink "PERMALINK"
```

This parses the channel and timestamp from the URL and returns the message or full thread context as JSON. Use this first even when the eventual task is to draft a reply.

### Permalink format

- **Message:** `https://epochai.slack.com/archives/<channel-id>/p<ts-without-dot>`
  - Example: ts `1776265426.017059` becomes `p1776265426017059`.
- **Threaded message:** append `?thread_ts=<thread-ts>&cid=<channel-id>`.
  - Example: `https://epochai.slack.com/archives/C0APL0BFNE7/p1776265509202259?thread_ts=1776265426.017059&cid=C0APL0BFNE7`

If you only have `channel_id` and `ts` from an MCP call, construct the permalink yourself — do not ask the user to supply it.

## Opening a Slack permalink in Emacs

Only open Slack in Emacs when the user explicitly asks to open the Slack UI, or after drafting a reply so the user can paste it. Use a guarded `slack-browse-url` call that blocks the package's default-browser fallback:

```bash
emacsclient --no-wait --eval '(progn
  (require (quote cl-lib))
  (cl-letf (((symbol-function (quote browse-url-default-browser))
             (lambda (&rest _) (error "Blocked browser fallback from slack-browse-url"))))
    (slack-browse-url "PERMALINK"))
  nil)'
```

If this fails, report that Emacs Slack could not open the thread. Do not retry with an external browser unless the user explicitly asks for that.

## Drafting a reply to a Slack message

When the user asks you to draft a reply to a specific Slack message or thread:

1. Read the permalink with `slack.py permalink "PERMALINK"` if the full context is not already loaded.
2. Draft the reply.
3. Push the draft to the Emacs kill ring (see "Copying drafts" below). This also syncs to the system clipboard, and has the advantage that the draft survives in kill-ring history (accessible via `M-y`) even if the user copies something else in the meantime.
4. **Open the thread in Emacs** using the guarded `slack-browse-url` command above so the user can paste directly.

Steps 3 and 4 are both required every time. Do not skip step 4 just because the user hasn't explicitly asked for it — it's the default behavior for reply drafting.

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

- General Slack digests / unreads — use `slack-digest` or `slack-saved` instead.
- Sending a message programmatically — not currently supported in this workflow; the user pastes manually in Emacs.

## Hard rules

- Do not call unguarded `slack-browse-url` from Codex; it can fall back to `browse-url-default-browser`.
- Do not open Slack UI just to read a permalink. Use `slack.py permalink`.
