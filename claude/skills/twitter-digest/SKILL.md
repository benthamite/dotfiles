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

Fetch recent tweets from a named list of accounts, optionally triage them against the list's criteria, and present a digest. If the list has a `description`, it acts as a triage rubric to skip noise and surface only what matters. If the list has no `description`, all tweets are presented unfiltered.

## Lists

Lists are stored in `~/.claude/skills/twitter-digest/lists/`. Each list is a markdown file named `<list-name>.md` with this format:

```markdown
---
name: <list-name>
description: <optional — what the user wants from this list, used as the triage rubric>
---

- @username
- @username
- @username
```

The `description` field controls filtering:
- **With description**: acts as a triage rubric — tweets are classified as notable or skip. Examples:
  - "Practical AI tool workflows, tips, and tutorials. Skip product announcements, hype, personal posts, and retweets unless the RT itself is high-signal."
  - "AI safety research and policy developments. Skip casual takes and culture war commentary."
- **Without description**: no triage — all tweets from the list are presented chronologically. Useful for small, curated lists where you want to see everything.

### Managing lists

- If invoked with no argument and no lists exist, ask the user to create one: what should it be called, which accounts to include, and optionally what content they want from it (if they don't specify a filter/description, create an unfiltered list).
- If invoked with no argument and exactly one list exists, use that list.
- If invoked with no argument and multiple lists exist, ask which list to use.
- If invoked with a list name that doesn't exist, offer to create it.
- The user can say "add @username to <list>" or "remove @username from <list>" to modify lists.
- The user can say "show lists" or "list my twitter lists" to see all lists and their accounts.

## Procedure

### 1. Load the list

Read the list file. Extract the account usernames and the description (triage rubric), if present. If the list has no `description`, note that this is an **unfiltered list** — steps 4 and 5 will be skipped.

### 2. Check last run

Check if `~/.claude/skills/twitter-digest/last-run/<list-name>.txt` exists. If it does, read the ISO timestamp inside — this is the cutoff. Only tweets newer than this timestamp should be included in the digest.

If the file doesn't exist, this is the first run for this list. Ask the user how far back to look (e.g., "last 24 hours", "last 3 days", "past week"). Default suggestion: 48 hours. Use their answer to compute a cutoff timestamp. If running non-interactively (`/loop` mode, `claude -p`, or `$ARGUMENTS` already specifies the list), default to 48 hours without asking.

### 3. Fetch recent tweets

**IMPORTANT: Use the fetch script, NOT MCP tools.** MCP tool responses are massive JSON blobs (~120K chars per account) that consume enormous context. The fetch script curls the API directly and extracts only the fields we need, producing ~130 chars per tweet instead of ~6,000.

Run the fetch script:

```bash
SKILL_DIR=~/.claude/skills/twitter-digest
bash "$SKILL_DIR/fetch-tweets.sh" "$SKILL_DIR/lists/<list-name>.md" "<cutoff-iso-timestamp>" 2>/tmp/rt-discovery-<list-name>.txt
```

The script outputs pipe-delimited lines to stdout:
```
@user|date|likes|views|OG/RT|tweet_id|rt_user|text
```

And RT discovery candidates to stderr (captured in the file above):
```
RT_DISCOVERY:@username
```

### 4. Triage

**Skip this step if the list has no `description`** — all tweets pass through unfiltered.

Using the list's `description` as your rubric, classify each tweet from the fetch output:

- **Notable**: matches what the user wants, high signal, worth reading.
- **Skip**: doesn't match criteria, noise, low-signal RTs, promotional, casual/personal.

Be aggressive about skipping. A good digest surfaces 5-15 tweets from 20 accounts, not 200.

### 5. Explore RT authors

**Skip this step if the list has no `description`** — RT exploration requires a rubric to evaluate discovered accounts against.

Read the RT discovery file from step 3. For each unique discovered username, fetch their tweets with the same script:

```bash
bash "$SKILL_DIR/fetch-tweets.sh" "$SKILL_DIR/lists/<list-name>.md" "<cutoff-iso-timestamp>" user1 user2 user3 2>/dev/null
```

(Extra usernames passed as arguments after the cutoff are fetched in addition to the list.)

Wait — actually, for RT discovery we only want the extra users' tweets, not the full list again. Instead, just call the API directly for each discovered user:

```bash
for user in $(sort -u /tmp/rt-discovery-<list-name>.txt | sed 's/RT_DISCOVERY:@//'); do
  curl -sf "https://api.twitterapi.io/twitter/user/last_tweets?userName=$user" \
    -H "X-API-Key: $TWITTER_API_IO" | python3 -c "
import json, sys
data = json.load(sys.stdin)
for t in data.get('data', {}).get('tweets', []):
    text = t.get('text', '')[:300].replace('\n', ' ').replace('|', '/')
    user = t.get('author', {}).get('userName', '?')
    likes = t.get('likeCount', 0)
    views = t.get('viewCount', 0)
    tid = t.get('id', '')
    date = t.get('createdAt', '')[:25]
    print(f'@{user}|{date}|{likes}|{views}|OG|{tid}||{text}')
"
done
```

Triage these against the same rubric. Include notable ones in a "Discovered accounts" section.

### 6. Present the digest

Format the digest as an **org-mode** buffer:

```org
#+title: Twitter digest: <list-name> — YYYY-MM-DD HH:MM

* Notable

** @username (date)
Tweet text (full or lightly trimmed)
Likes: N | Views: N | Like rate: X.X%
[[https://x.com/username/status/ID][View on X]]

** @username (date)
...

* Discovered accounts
Accounts found via retweets from listed users. Consider adding them to the list.

** @newuser (via RT from @listeduser)
Tweet text
Likes: N | Views: N | Like rate: X.X%
[[https://x.com/newuser/status/ID][View on X]]

* Accounts with nothing notable
@user1, @user2, @user3 — nothing new matching criteria.
```

**Unfiltered lists** (no `description`): use the same org format but replace the "Notable" heading with "All tweets", order tweets reverse-chronologically, and omit the "Discovered accounts" and "Accounts with nothing notable" sections entirely.

Guidelines:
- Order tweets by **like rate** (likes ÷ views) descending — this surfaces tweets that resonated most relative to their reach.
- Include the tweet URL as an org link so the user can open it from Emacs.
- For the "nothing notable" section (filtered lists only), just list the usernames in a single line.
- If a tweet is part of a thread and the beginning is clearly missing context, note "(thread)" so the user knows to click through.
- Omit the "Discovered accounts" section if no RT authors produced notable tweets.

### 7. Open in Emacs

Write the digest to a temporary file and open it in Emacs:

```bash
TMPFILE=$(mktemp /tmp/twitter-digest-XXXXXX.org)
# (write content to $TMPFILE)
emacsclient -e "(progn (find-file \"$TMPFILE\") (goto-char (point-min)) (org-fold-show-all))"
```

Also print a brief summary to the terminal (number of notable tweets, number of discovered accounts) so the user gets immediate feedback even before switching to Emacs.

### 8. Update last-run timestamp

After presenting the digest, write the ISO timestamp of the most recent tweet seen to:

```
~/.claude/skills/twitter-digest/last-run/<list-name>.txt
```

This ensures the next run only surfaces new tweets. Always do this, whether invoked manually or via `/loop`.

### 9. Save the digest (optional)

If the user asks to save, write to `~/.claude/skills/twitter-digest/digests/<list-name>/YYYY-MM-DD-HHMM.md`.

## Multiple lists

When invoked with multiple list names (e.g., `/twitter-digest ai-tools ai-macrostrategy`), process each list sequentially and combine the results into a single org buffer with a top-level heading per list:

```org
#+title: Twitter digest: ai-tools + ai-macrostrategy — YYYY-MM-DD

* ai-tools
:PROPERTIES:
:cutoff: <timestamp>
:END:

** Notable
...

* ai-macrostrategy
:PROPERTIES:
:cutoff: <timestamp>
:END:

** Notable
...
```

## Periodic digests

This skill pairs with `/loop` for periodic execution:

```
/loop 4h /twitter-digest ai-tools
```

## Cost awareness

The fetch script calls the twitterapi.io REST API directly (not MCP), so the only Claude token cost is triaging the compact tweet lines (~130 chars each). Typical cost for a full digest of 20-25 accounts: **~$0.02-0.05** (down from ~$0.75 per list with MCP).
