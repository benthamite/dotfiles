---
name: twitter
description: Read tweets, threads, user timelines, and search Twitter/X via twitterapi.io. Use when the user says "read tweet", "get tweet", "fetch tweet", "read thread", "user's tweets", "tweets from @handle", "search twitter", or "save tweets".
user-invocable: true
model: sonnet
---

# Twitter

Read and search Twitter/X content using the `twitterapi-io` MCP server. Supports single tweets, threads/replies, user timelines, and keyword search.

## Available MCP tools

Use these tools from the `twitterapi-io` MCP server:

| Tool | Use for |
|---|---|
| `mcp__twitterapi-io__get_tweet_by_id` | Single tweet by ID |
| `mcp__twitterapi-io__get_tweet_replies` | Replies/thread for a tweet |
| `mcp__twitterapi-io__get_user_tweets` | A user's recent tweets |
| `mcp__twitterapi-io__get_user_by_username` | User profile info |
| `mcp__twitterapi-io__search_tweets` | Keyword search (recent, popular, or mixed) |
| `mcp__twitterapi-io__get_user_followers` | A user's followers |
| `mcp__twitterapi-io__get_user_following` | Who a user follows |
| `mcp__twitterapi-io__search_users` | Find users by name/keyword |

For API documentation details, use the `twitterapi-io-docs` MCP server tools (e.g. `mcp__twitterapi-io-docs__get_twitterapi_endpoint`).

## Parsing input

When the user provides a tweet URL like `https://x.com/username/status/1234567890`, extract the numeric tweet ID (`1234567890`) and use `get_tweet_by_id`. If they provide a username with or without `@`, strip the `@` and use the bare username.

## Procedure

### Reading a single tweet

1. Call `get_tweet_by_id` with the tweet ID.
2. Present: author name, handle, date, full text, media descriptions if any, engagement stats (likes, retweets, replies, views).

### Reading a thread or replies

1. Call `get_tweet_by_id` for the root tweet.
2. Call `get_tweet_replies` with `count: 100` to get replies.
3. If the user asked for the thread (i.e. the author's own continuation), filter replies to only those where the author matches the root tweet's author. Present them in chronological order.
4. If the user asked for replies/reactions, present all replies grouped by engagement.

### Reading a user's timeline

1. Call `get_user_tweets` with the username and requested count (default 20).
2. Present each tweet with: date, text (truncated if very long), engagement stats.
3. Highlight tweets with notably high engagement relative to the user's average.

### Searching tweets

1. Call `search_tweets` with the query. Default to `result_type: "recent"` unless the user asks for popular/top tweets.
2. Present results with: author, date, text, engagement stats.

### User profile lookup

1. Call `get_user_by_username`.
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
