---
name: twitter-digest
description: Fetch recent tweets from a curated list of accounts, triage for relevance, and present a digest. Use when the user says "twitter digest", "check twitter", "twitter update", "what's new on twitter", or "twitter roundup".
argument-hint: "[list-name]"
argument-source: "lists/*.md"
argument-multiple: true
user-invocable: true
---

# Twitter digest

@claude/skills/twitter-vet/SKILL.md

## COST RULE — READ THIS FIRST

Each tool call resends ~20K tokens of system context. The core digest is **exactly 2 tool calls**. Vetting discovered accounts adds N more (typically 0-5, ~$0.003 each):

1. **Bash**: run `fetch-tweets.sh` (handles list reading, cutoff, fetching, RT discovery)
2. **Bash**: write org file + open in Emacs + update last-run timestamp + save digest (one heredoc command)
3. **MCP** (0-5 calls): vet discovered accounts — see Step 3

Any extra tool calls (Read, Write, separate Bash) waste $0.02-0.05 each. Do NOT read list files or last-run files separately — the fetch script handles that.

Never use MCP twitter tools for the digest itself. The fetch script calls the REST API directly. MCP tools are only used for vetting discovered accounts.

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
---RT_AUTHORS---
@user|followers|bio_text
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
emacsclient -e "(progn (find-file \"$TMPFILE\") (goto-char (point-min)) (org-fold-show-all))" && echo "<iso-timestamp-of-newest-tweet>" > ~/.claude/skills/twitter-digest/last-run/<name>.txt && cp "$TMPFILE" ~/.claude/skills/twitter-digest/digests/<name>-YYYY-MM-DD.org
```

Order by like rate (likes÷views) descending. Omit "Discovered accounts" if none. Unfiltered lists (no description): show all tweets reverse-chrono under `* All tweets`, no other sections.

## Saving digests

Always save a copy of the digest to `~/.claude/skills/twitter-digest/digests/<name>-YYYY-MM-DD.org`. For multi-list digests, use the combined filename (e.g. `ai-tools-ai-macrostrategy-2026-03-20.org`).

## Step 3: Vet discovered accounts

If `---RT_DISCOVERY---` yielded accounts:

1. **Check registry**: read `~/.claude/skills/twitter-vet/vetted/<list-name>.md` if it exists. Any RT-discovered account already listed there: skip re-vetting, use the recorded score directly.
2. **Quick filter** (Procedure A from twitter-vet): use `---RT_AUTHORS---` metadata (bio, followers) and `---RT_DISCOVERY---` tweets to filter remaining accounts. Cap at top 5 candidates by quick-filter confidence. Skip the rest.
3. **Full scoring** (Procedure B from twitter-vet): for each account that passes, call `mcp__twitterapi-io__get_user_tweets` (userName, count "20"). Score 1-10 against the list's `description`.
4. **Auto-add**: score >= 7 (or `vet-threshold` from list YAML) → append `- @username` to the list file. Score 5-6 → report as borderline in the digest. Score <= 4 → skip.
5. **Update digest**: emit one final Bash call to append a `* Vetted accounts` section to the org file (replacing `* Discovered accounts`) showing each account's score and rationale, and append any new `- @username` lines to the list file.
6. **Update vet registry**: append newly scored accounts to `~/.claude/skills/twitter-vet/vetted/<list-name>.md` (creating the file with the standard header if it doesn't exist). Include all accounts that went through full scoring, regardless of outcome.

## Multiple lists

Run fetch-tweets.sh once per list. Combine into one org buffer with `* <list-name>` top-level headings.

## List management

Lists: `~/.claude/skills/twitter-digest/lists/<name>.md`. YAML frontmatter with optional `description` (triage rubric), then `- @username` lines. No arg + 1 list → use it; multiple → ask.
