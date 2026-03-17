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

Read the list file. Extract the account usernames and the description (triage rubric), if present. If the list has no `description`, note that this is an **unfiltered list** — steps 5 and 6 will be skipped.

### 2. Check last run

Check if `~/.claude/skills/twitter-digest/last-run/<list-name>.txt` exists. If it does, read the ISO timestamp inside — this is the cutoff. Only tweets newer than this timestamp should be included in the digest.

If the file doesn't exist, this is the first run for this list. Ask the user how far back to look (e.g., "last 24 hours", "last 3 days", "past week"). Default suggestion: 48 hours. Use their answer to compute a cutoff timestamp. If running non-interactively (`/loop` mode, `claude -p`, or `$ARGUMENTS` already specifies the list), default to 48 hours without asking.

### 3. Fetch recent tweets

For each account in the list, call `mcp__twitterapi-io__get_user_tweets` with `count: 10`.

**Rate limiting**: make at most 3 calls in parallel. If the list has more than 20 accounts, warn the user about cost before proceeding (~$0.003 per account, so 20 accounts = ~$0.06).

### 4. Parse results

Tool results come back as large JSON files saved to the tool-results directory. Use a Python one-liner or the helper script to extract tweet text, date, engagement stats, and author from each result file:

```bash
python3 -c "
import json, sys
with open(sys.argv[1]) as f: raw = json.load(f)
inner = json.loads(raw[0]['text']) if isinstance(raw, list) else raw
for t in inner.get('data', {}).get('tweets', []):
    text = t.get('text','')[:300].replace('\n',' ')
    date = t.get('createdAt','')[:25]
    user = t.get('author',{}).get('userName','?')
    likes = t.get('likeCount',0)
    views = t.get('viewCount',0)
    tid = t.get('id','')
    is_rt = bool(t.get('retweetedTweet'))
    rt_user = t.get('retweetedTweet',{}).get('author',{}).get('userName','') if is_rt else ''
    print(f'@{user}|{date}|{likes}|{views}|{\"RT\" if is_rt else \"OG\"}|{tid}|{rt_user}|{text}')
" "$FILE"
```

Note: the tweet data is nested under `data.tweets` in the API response. Retweets have a `retweetedTweet` object containing the original tweet and its `author` — extract `retweetedTweet.author.userName` for the RT discovery step.

### 5. Triage

**Skip this step if the list has no `description`** — all tweets pass through unfiltered.

Using the list's `description` as your rubric, classify each tweet:

- **Notable**: matches what the user wants, high signal, worth reading.
- **Skip**: doesn't match criteria, noise, low-signal RTs, promotional, casual/personal.

Be aggressive about skipping. A good digest surfaces 5-15 tweets from 20 accounts, not 200.

### 6. Explore RT authors

**Skip this step if the list has no `description`** — RT exploration requires a rubric to evaluate discovered accounts against.

When a tweet in the results is a retweet, treat the **original author** (the person being retweeted) as a discovery candidate:

1. Collect all unique original authors from RTs that passed the cutoff filter.
2. Exclude any that are already in the list.
3. For each new author, call `mcp__twitterapi-io__get_user_tweets` with `count: 10` (same rate-limiting rules: max 3 in parallel).
4. Triage their tweets against the same rubric. If any are notable, include them in the digest under a separate "Discovered accounts" section.
5. After the digest, report these accounts so the user can decide whether to add them to the list permanently.

This turns every RT into a lightweight discovery signal — if a listed account amplifies someone, that person is probably worth a look.

### 7. Present the digest

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

### 8. Open in Emacs

Write the digest to a temporary file and open it in Emacs:

```bash
TMPFILE=$(mktemp /tmp/twitter-digest-XXXXXX.org)
# (write content to $TMPFILE)
emacsclient -e "(progn (find-file \"$TMPFILE\") (goto-char (point-min)) (org-fold-show-all))"
```

Also print a brief summary to the terminal (number of notable tweets, number of discovered accounts) so the user gets immediate feedback even before switching to Emacs.

### 9. Update last-run timestamp

After presenting the digest, write the ISO timestamp of the most recent tweet seen to:

```
~/.claude/skills/twitter-digest/last-run/<list-name>.txt
```

This ensures the next run only surfaces new tweets. Always do this, whether invoked manually or via `/loop`.

### 10. Save the digest (optional)

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
