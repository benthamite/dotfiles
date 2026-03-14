---
name: twitter-digest
description: Fetch recent tweets from a curated list of accounts, triage for relevance, and present a digest. Use when the user says "twitter digest", "check twitter", "twitter update", "what's new on twitter", or "twitter roundup".
argument-hint: "[list-name]"
user-invocable: true
---

# Twitter digest

Fetch recent tweets from a named list of accounts, triage them against the list's criteria, and present a curated digest. Skips noise and surfaces only what matters.

## Lists

Lists are stored in `~/.claude/skills/twitter-digest/lists/`. Each list is a markdown file named `<list-name>.md` with this format:

```markdown
---
name: <list-name>
description: <what the user wants from this list — used as the triage rubric>
---

- @username
- @username
- @username
```

The `description` field is critical — it tells you what to surface and what to skip. Examples:
- "Practical AI tool workflows, tips, and tutorials. Skip product announcements, hype, personal posts, and retweets unless the RT itself is high-signal."
- "AI safety research and policy developments. Skip casual takes and culture war commentary."

### Managing lists

- If invoked with no argument and no lists exist, ask the user to create one: what should it be called, what content they want from it, and which accounts to include.
- If invoked with no argument and exactly one list exists, use that list.
- If invoked with no argument and multiple lists exist, ask which list to use.
- If invoked with a list name that doesn't exist, offer to create it.
- The user can say "add @username to <list>" or "remove @username from <list>" to modify lists.
- The user can say "show lists" or "list my twitter lists" to see all lists and their accounts.

## Procedure

### 1. Load the list

Read the list file. Extract the account usernames and the description (triage rubric).

### 2. Check last run

Check if `~/.claude/skills/twitter-digest/last-run/<list-name>.txt` exists. If it does, read the ISO timestamp inside — this is the cutoff. Only tweets newer than this timestamp should be included in the digest.

If the file doesn't exist, this is the first run for this list. Ask the user how far back to look (e.g., "last 24 hours", "last 3 days", "past week"). Default suggestion: 48 hours. Use their answer to compute a cutoff timestamp. If running in `/loop` mode (no interactive input), default to 48 hours.

### 3. Fetch recent tweets

For each account in the list, call `mcp__twitterapi-io__get_user_tweets` with `count: 10`.

**Rate limiting**: make at most 3 calls in parallel. If the list has more than 20 accounts, warn the user about cost before proceeding (~$0.003 per account, so 20 accounts = ~$0.06).

### 4. Parse results

Tool results come back as large JSON files. Use a Python one-liner or the helper script to extract tweet text, date, engagement stats, and author from each result file:

```bash
python3 -c "
import json, sys
with open(sys.argv[1]) as f: raw = json.load(f)
inner = json.loads(raw[0]['text']) if isinstance(raw, list) else raw
for t in inner.get('tweets', []):
    text = t.get('text','')[:300].replace('\n',' ')
    date = t.get('createdAt','')[:16]
    user = t.get('author',{}).get('userName','?')
    likes = t.get('likeCount',0)
    views = t.get('viewCount',0)
    is_rt = 'RT @' in text[:4]
    print(f'@{user}|{date}|{likes}|{views}|{\"RT\" if is_rt else \"OG\"}|{text}')
" "$FILE"
```

### 5. Triage

Using the list's `description` as your rubric, classify each tweet:

- **Notable**: matches what the user wants, high signal, worth reading.
- **Skip**: doesn't match criteria, noise, low-signal RTs, promotional, casual/personal.

Be aggressive about skipping. A good digest surfaces 5-15 tweets from 20 accounts, not 200.

### 6. Present the digest

Format as a clean summary:

```
## Twitter digest: <list-name> — YYYY-MM-DD HH:MM

### Notable

**@username** (date)
Tweet text (full or lightly trimmed)
Likes: N | Views: N
https://x.com/username/status/ID

**@username** (date)
...

### Accounts with nothing notable
@user1, @user2, @user3 — nothing new matching your criteria.
```

Guidelines:
- Order notable tweets by relevance to the rubric, not by engagement.
- Include the tweet URL so the user can open it.
- For the "nothing notable" section, just list the usernames in a single line.
- If a tweet is part of a thread and the beginning is clearly missing context, note "(thread)" so the user knows to click through.

### 7. Update last-run timestamp

After presenting the digest, write the ISO timestamp of the most recent tweet seen to:

```
~/.claude/skills/twitter-digest/last-run/<list-name>.txt
```

This ensures the next run only surfaces new tweets. Always do this, whether invoked manually or via `/loop`.

### 8. Save the digest (optional)

If the user asks to save, write to `~/.claude/skills/twitter-digest/digests/<list-name>/YYYY-MM-DD-HHMM.md`.

## Periodic digests

This skill pairs with `/loop` for periodic execution:

```
/loop 4h /twitter-digest ai-tools
```

## Cost awareness

See the `twitter` skill for full pricing details. Quick reference:
- 10 tweets from one account: ~$0.0015
- Full digest of 20 accounts (10 tweets each): ~$0.06
- Running every 4 hours for a day (6 runs): ~$0.36
