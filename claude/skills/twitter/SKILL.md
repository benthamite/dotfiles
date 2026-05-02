---
name: twitter
description: Read tweets, threads, user timelines, and search Twitter/X via twitterapi.io. Use when the user says "read tweet", "get tweet", "fetch tweet", "read thread", "user's tweets", "tweets from @handle", "search twitter", or "save tweets".
user-invocable: true
model: sonnet
---

# Twitter

Read and search Twitter/X content via the twitterapi.io API, accessed through the `twitterapi.sh` wrapper script.

## The wrapper script

All access goes through `~/.claude/skills/twitter/lib/twitterapi.sh`. It outputs raw JSON on stdout. Each call hits one twitterapi.io endpoint.

Pipe results through `python3 -c '...'` (or save to a file and parse) to extract what you need — JSON shapes are documented at `https://docs.twitterapi.io/`. Use `WebFetch` for endpoint details if needed.

| Subcommand | Use for | Endpoint |
|---|---|---|
| `tweet <id> [<id>...]` | Single (or batch) tweet by ID | `/twitter/tweets` |
| `replies <id> [--type=Latest\|Likes\|Relevance]` | Replies/thread for a tweet | `/twitter/tweet/replies/v2` |
| `tweets <username> [--include-replies]` | A user's recent tweets | `/twitter/user/last_tweets` |
| `user <username>` | User profile info | `/twitter/user/info` |
| `search <query> [--type=Latest\|Top]` | Keyword search (default: Top) | `/twitter/tweet/advanced_search` |
| `followers <username> [--page-size=N]` | A user's followers | `/twitter/user/followers` |
| `following <username> [--page-size=N]` | Who a user follows | `/twitter/user/followings` |
| `users <query>` | Find users by name/keyword | `/twitter/user/search` |

All subcommands accept `--cursor=...` for pagination.

## Parsing input

When the user provides a tweet URL like `https://x.com/username/status/1234567890`, extract the numeric tweet ID (`1234567890`) and use `tweet <id>`. The script strips a leading `@` from usernames automatically.

## Procedure

### Reading a single tweet

1. Run `twitterapi.sh tweet <id>`.
2. Present: author name, handle, date, full text, media descriptions if any, engagement stats (likes, retweets, replies, views).

### Reading a thread or replies

1. Run `twitterapi.sh tweet <id>` for the root tweet.
2. Run `twitterapi.sh replies <id> --type=Latest` to get replies (defaults to Relevance, which is rarely what you want for threads).
3. If the user asked for the thread (i.e. the author's own continuation), filter replies to only those where the author matches the root tweet's author. Present them in chronological order.
4. If the user asked for replies/reactions, present all replies grouped by engagement.

### Reading a user's timeline

1. Run `twitterapi.sh tweets <username>`. The endpoint returns ~20 tweets per page; pass `--cursor=<next_cursor>` for more.
2. Present each tweet with: date, text (truncated if very long), engagement stats.
3. Highlight tweets with notably high engagement relative to the user's average.

### Searching tweets

1. Run `twitterapi.sh search '<query>'`. Default queryType is `Top`; pass `--type=Latest` for recent.
2. Present results with: author, date, text, engagement stats.

### User profile lookup

1. Run `twitterapi.sh user <username>`.
2. Present: name, handle, bio, follower/following counts, verified status, account creation date, pinned tweet if any.

## Saving tweets

When the user asks to save tweets, write them to `~/.claude/skills/twitter/saved-tweets/YYYY-MM-DD/`.

**Markdown format** (default):

```markdown
# @username — YYYY-MM-DD HH:MM UTC

TWEET TEXT

---
Likes: N | Retweets: N | Replies: N | Views: N
URL: https://x.com/username/status/ID
```

**Filename:** `YYYY-MM-DD-username-description.md` where description is a short slugified summary (max 5 words).

When saving a thread, concatenate all tweets in order in a single file with `---` separators.

## Related skills

- `/twitter-digest` — curated digests from account lists with triage, last-run tracking, and `/loop` support.
- `/twitter-discover` — graph-based discovery of high-value accounts in any niche.

## Pricing and rate limits

### Pricing (1 USD = 100,000 credits)

| Service   | Cost              | Credits      |
|-----------|-------------------|--------------|
| Tweets    | $0.15 per 1K      | 15 per tweet |
| Profiles  | $0.18 per 1K      | 18 per user  |
| Followers | $0.15 per 1K      | 15 per follower |
| List calls| $0.0015 per call  | 150 per call |

Minimum charge: 15 credits ($0.00015) per API call, waived for bulk data responses. Charging is per *returned* item: if 4 tweets come back, that's 60 credits; if 0 or 1, it's the 15-credit minimum.

**Typical costs:**
- Single tweet lookup: ~$0.00015
- 20 tweets from a user timeline: ~$0.003
- 100 followers/following: ~$0.015
- Search (20 results): ~$0.003
- A casual session (a few lookups + searches): under $0.01
- A discovery session (20 timelines + 10 following lists + searches): ~$0.50-$1.00

Flag to the user if a request would fetch more than ~500 tweets or the session is likely to exceed $2.

### Rate limits (QPS)

QPS scales with account credit balance:

| Account balance (credits) | QPS limit |
|---------------------------|-----------|
| Free tier                 | 1 req / 5 seconds |
| >= 1,000                  | 3 |
| >= 5,000                  | 6 |
| >= 10,000                 | 10 |
| >= 50,000                 | 20 |

**Practical guidance:**
- At typical balances (1K-5K credits), you can do 3-6 requests/second.
- Do not fire more than 3 parallel API calls unless you know the balance supports it.
- If you get a 429 rate limit error, back off and make calls sequentially.
- Recharged credits never expire; bonus credits expire after 30 days.
