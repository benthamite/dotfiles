---
name: twitter-digest
description: Fetch recent tweets from a curated list of accounts, triage for relevance, and present a digest. Use when the user says "twitter digest", "check twitter", "twitter update", "what's new on twitter", or "twitter roundup".
argument-hint: "[list-name]"
argument-source: "lists/*.md"
argument-multiple: true
user-invocable: true
model: sonnet
---

# Twitter digest

## COST RULE — READ THIS FIRST

Each tool call resends ~20K tokens of system context. **You MUST complete this skill in exactly 2 tool calls:**

1. **Bash**: run `fetch-tweets.sh` (handles list reading, cutoff, fetching, RT discovery)
2. **Bash**: write org file + open in Emacs + update last-run timestamp (one heredoc command)

Any extra tool calls (Read, Write, separate Bash) waste $0.02-0.05 each. Do NOT read list files or last-run files separately — the fetch script handles that.

Never use MCP twitter tools. The fetch script calls the REST API directly.

## Step 1: Fetch

```bash
bash ~/.claude/skills/twitter-digest/fetch-tweets.sh <list-name>
```

Output:
```
LIST:<name>
CUTOFF:<iso-timestamp>
DESCRIPTION:<triage rubric or empty>
---TWEETS---
@user|date|likes|views|OG/RT|tweet_id|rt_user|text
...
---RT_DISCOVERY---
@user|date|likes|views|OG/RT|tweet_id|rt_user|text
```

If no cutoff file exists and running non-interactively, defaults to 48h. If interactive, ask the user.

## Step 2: Triage + output

If DESCRIPTION is non-empty, triage tweets: keep only notable ones matching the rubric. Be aggressive — 5-15 tweets from 20+ accounts. Skip noise, hype, self-promotion, low-signal RTs.

For RT-discovered authors (after `---RT_DISCOVERY---`), triage separately.

Then emit ONE bash command that does everything:

```bash
TMPFILE=$(mktemp /tmp/twitter-digest-XXXXXX.org) && cat > "$TMPFILE" <<'ORGEOF'
#+title: Twitter digest: <name> — YYYY-MM-DD

* Notable

** @username (Mon Mar 17 14:30)
Tweet text
Likes: N | Views: N | Like rate: X.X%
[[https://x.com/username/status/ID][View on X]]

* Discovered accounts
...

* Accounts with nothing notable
@user1, @user2, ...
ORGEOF
emacsclient -e "(progn (find-file \"$TMPFILE\") (goto-char (point-min)) (org-fold-show-all))" && echo "<iso-timestamp-of-newest-tweet>" > ~/.claude/skills/twitter-digest/last-run/<name>.txt
```

Order by like rate (likes÷views) descending. Omit "Discovered accounts" if none. Unfiltered lists (no description): show all tweets reverse-chrono under `* All tweets`, no other sections.

## Multiple lists

Run fetch-tweets.sh once per list. Combine into one org buffer with `* <list-name>` top-level headings.

## List management

Lists: `~/.claude/skills/twitter-digest/lists/<name>.md`. YAML frontmatter with optional `description` (triage rubric), then `- @username` lines. No arg + 1 list → use it; multiple → ask.
