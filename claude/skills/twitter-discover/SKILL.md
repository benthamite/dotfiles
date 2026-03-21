---
name: twitter-discover
description: Discover high-value Twitter/X accounts in any niche using iterative graph-based exploration. Use when the user says "discover accounts", "find twitter accounts", "twitter discovery", "curate a list", "find people to follow", "who should I follow", or "twitter explore".
user-invocable: true
---

# Twitter account discovery

@claude/skills/twitter-vet/SKILL.md

Iterative, graph-based discovery of high-value Twitter/X accounts in any topic area. Uses a "hot promise score" algorithm that combines following-list overlap with bio relevance filtering and continuous score updates.

## MCP tools used

From the `twitterapi-io` MCP server:

| Tool                                       | Use for                                  |
|--------------------------------------------|------------------------------------------|
| `mcp__twitterapi-io__search_tweets`        | Find initial seed accounts by topic      |
| `mcp__twitterapi-io__get_user_tweets`      | Sample tweets to evaluate an account     |
| `mcp__twitterapi-io__get_user_following`   | Get who an account follows (max 100)     |
| `mcp__twitterapi-io__get_user_by_username` | Check bio/profile before sampling tweets |
| `mcp__twitterapi-io__search_users`         | Find users by keyword                    |

## Procedure

### Phase 1: Interview

Ask the user two things:

1. **What content are you interested in?** Get specific: topics, the kind of person they want to find (practitioners sharing workflows? researchers? commentators? builders?), what makes an account valuable vs. noise.
2. **Do you have seed accounts?** An optional list of Twitter usernames they already know are good. Even 2-3 seeds dramatically improve results.

Synthesize the answers into:
- `TOPIC_KEYWORDS`: a list of search queries for Phase 2 (if no seeds provided).
- `RELEVANCE_CRITERIA`: a rubric for scoring accounts (what makes a 9 vs. a 5 vs. a skip). This replaces the generic scoring — tailor it to what the user actually wants.

Confirm the criteria with the user before proceeding.

### Phase 2: Seed acquisition

**If the user provided seeds**, skip to Phase 3.

**If not**, find seeds via search:

1. Run 3-5 `search_tweets` calls using `TOPIC_KEYWORDS` with `result_type: "popular"` and `count: 20`.
2. Extract unique authors from results, filtering out:
   - Accounts with <500 followers (too small to anchor discovery).
   - Accounts whose bio or content is clearly irrelevant (use your judgment).
   - Reply-only results.
3. For the top 5-8 authors by engagement, sample their tweets (`get_user_tweets`, count 20).
4. Score each using `RELEVANCE_CRITERIA`. Any scoring 6+ become seeds.
5. If fewer than 3 seeds found, try different search queries and repeat.

### Phase 3: Score and expand (iterative)

This is the core loop. Repeat until the user is satisfied.

#### 3a. Score seed accounts

For each unscored seed:
1. Fetch 20 tweets with `get_user_tweets`.
2. Evaluate against `RELEVANCE_CRITERIA`. Assign a score 1-10.
3. Record the score.

#### 3b. Fetch following lists

For each scored account with score >= 6, fetch their following list (`get_user_following`, count 100).

**Important**: Do these calls sequentially (not in parallel) to avoid rate limiting. A 2-second pause between calls is not needed but avoid firing more than 3 simultaneously.

#### 3c. Compute hot promise scores

For every account that appears in 2+ following lists of scored accounts:

```
promise_score = sum(score of each scored account that follows them)
```

This score updates continuously — every time a new account is scored and its following list fetched, all promise scores are recalculated. This is what makes the scores "hot."

#### 3d. Quick filter candidates

Apply **Procedure A** (quick filter) from twitter-vet to the candidate list from `compute_promise_scores`. Use bio, follower count, and who follows them. Sort passing candidates by likely relevance, breaking ties by promise score.

#### 3e. Full scoring

For the top 5-8 candidates that pass the quick filter, apply **Procedure B** (full scoring) from twitter-vet using `RELEVANCE_CRITERIA` as the rubric. For any scoring 7+, also fetch their following list and update all promise scores (making the loop "hotter"). Repeat from 3c with updated scores.

### Phase 4: Checkpoint

After profiling ~10-15 new accounts in a round, present findings:

1. Show the full scoreboard grouped by tier:
   - **Tier 1 (8-10)**: Must follow
   - **Tier 2 (6-7)**: Strong follow
   - **Tier 3 (4-5)**: Situational / niche value
   - Below 4: skipped, don't list
2. For each account show: handle, score, one-line description of why.
3. Show discovery stats: how many accounts profiled, how many following lists fetched, approximate API cost.
4. Ask: "Want to continue discovery, or is this a good list?"

If the user says continue, go back to Phase 3. If they say stop, save the results.

## Helper script

To efficiently parse the large JSON responses from the API, create a temporary Python script at `/tmp/twitter_discover_helpers.py` at the start of the session:

```python
#!/usr/bin/env python3
"""Helpers for Twitter account discovery."""
import json, re, sys

def extract_tweets(filepath):
    """Extract tweet text + engagement from a tool result file."""
    with open(filepath) as f:
        raw = json.load(f)
    if isinstance(raw, list) and raw and 'text' in raw[0]:
        inner = json.loads(raw[0]['text'])
    else:
        inner = raw
    tweets = inner.get('tweets', inner.get('data', {}).get('tweets', []))
    if isinstance(tweets, dict):
        tweets = tweets.get('tweets', [])
    for t in tweets:
        text = t.get('text', '')[:300].replace('\n', ' ')
        date = t.get('createdAt', '')[:16]
        likes = t.get('likeCount', '?')
        views = t.get('viewCount', '?')
        is_rt = text.startswith('RT @')
        prefix = 'RT' if is_rt else '  '
        print(f'{prefix} [{date}] {text}')
        print(f'   likes:{likes} views:{views}')
        print()

def extract_following(filepath):
    """Extract following list from a tool result file."""
    with open(filepath) as f:
        raw = json.load(f)
    if isinstance(raw, list) and raw and 'text' in raw[0]:
        inner = json.loads(raw[0]['text'])
    else:
        inner = raw
    followings = inner.get('followings', [])
    for u in followings:
        username = u.get('userName', '')
        bio = (u.get('description', '') or '')[:120].replace('\n', ' ')
        followers = u.get('followers', 0)
        print(f'@{username}\t{followers}\t{bio}')

def extract_search_authors(filepath, exclude_set=None):
    """Extract unique authors from search results, sorted by engagement."""
    exclude_set = exclude_set or set()
    with open(filepath) as f:
        raw = json.load(f)
    if isinstance(raw, list) and raw and 'text' in raw[0]:
        inner = json.loads(raw[0]['text'])
    else:
        inner = raw
    tweets = inner.get('tweets', [])
    authors = {}
    for t in tweets:
        if t.get('isReply'):
            continue
        author = t.get('author', {})
        username = author.get('userName', '?')
        if username.lower() in {x.lower() for x in exclude_set}:
            continue
        likes = t.get('likeCount', 0)
        bio = author.get('description', '') or ''
        if username not in authors or likes > authors[username]['likes']:
            authors[username] = {
                'text': t.get('text', '')[:200].replace('\n', ' '),
                'likes': likes,
                'views': t.get('viewCount', 0),
                'followers': author.get('followers', 0),
                'bio': bio[:150].replace('\n', ' ')
            }
    sorted_authors = sorted(authors.items(), key=lambda x: x[1]['likes'], reverse=True)
    for username, data in sorted_authors:
        print(f'@{username} ({data["followers"]:,} flw, {data["likes"]} likes)')
        print(f'  Tweet: {data["text"]}')
        if data['bio']:
            print(f'  Bio: {data["bio"]}')
        print()

def compute_promise_scores(following_files, scores):
    """Compute hot promise scores from following lists.

    following_files: dict of {username: filepath}
    scores: dict of {username: int_score}
    """
    all_following = {}
    for username, filepath in following_files.items():
        try:
            with open(filepath) as f:
                raw = json.load(f)
            if isinstance(raw, list) and raw and 'text' in raw[0]:
                inner = json.loads(raw[0]['text'])
            else:
                inner = raw
            followings = inner.get('followings', [])
            all_following[username] = {
                u.get('userName', ''): {
                    'followers': u.get('followers', 0),
                    'bio': (u.get('description', '') or ''),
                    'name': u.get('name', ''),
                } for u in followings if u.get('userName')
            }
        except Exception as e:
            print(f'Error loading {username}: {e}', file=sys.stderr)

    promise = {}
    scored_lower = {k.lower() for k in scores}
    for scorer, fdict in all_following.items():
        sc = scores.get(scorer, 0)
        if sc == 0:
            continue
        for followed, info in fdict.items():
            if followed.lower() in scored_lower:
                continue
            if followed not in promise:
                promise[followed] = {
                    'score': 0, 'followed_by': [],
                    'bio': info['bio'], 'followers': info['followers'],
                    'name': info.get('name', '')
                }
            promise[followed]['score'] += sc
            promise[followed]['followed_by'].append(f'{scorer}({sc})')

    results = []
    for username, data in promise.items():
        if len(data['followed_by']) < 2:
            continue
        data['username'] = username
        results.append(data)

    results.sort(key=lambda d: d['score'], reverse=True)

    print(f'{"":2} {"Username":20} {"Promise":>7} {"Flw":>8}  {"Followed by":40}  Bio')
    print('-' * 130)
    for i, d in enumerate(results[:50]):
        fb = ', '.join(d['followed_by'])
        bio = d['bio'][:60].replace('\n', ' ')
        print(f'{i+1:2}. @{d["username"]:19} {d["score"]:>7} {d["followers"]:>8,}  {fb:40}  {bio}')

if __name__ == '__main__':
    cmd = sys.argv[1] if len(sys.argv) > 1 else ''
    if cmd == 'tweets':
        extract_tweets(sys.argv[2])
    elif cmd == 'following':
        extract_following(sys.argv[2])
    elif cmd == 'search':
        exclude = set(sys.argv[3].split(',')) if len(sys.argv) > 3 else set()
        extract_search_authors(sys.argv[2], exclude)
```

Call it via:
- `python3 /tmp/twitter_discover_helpers.py tweets <file>` — extract tweets
- `python3 /tmp/twitter_discover_helpers.py following <file>` — extract following list
- `python3 /tmp/twitter_discover_helpers.py search <file> [exclude_csv]` — extract search authors

For `compute_promise_scores`, write a one-off Python snippet inline since it requires the current session's `scores` dict and `following_files` dict.

## Saving results

When the user is done, save results in two ways:

### 1. Discovery log (always)

Save to `~/.claude/skills/twitter-discover/results/YYYY-MM-DD-topic-slug.md`:

```markdown
# Twitter discovery: TOPIC

Date: YYYY-MM-DD
Criteria: RELEVANCE_CRITERIA summary

## Tier 1 (8-10)

- **@handle** (score) — one-line description

## Tier 2 (6-7)

- **@handle** (score) — one-line description

## Discovery stats

- Accounts profiled: N
- Following lists fetched: N
- Approximate API cost: $X.XX
```

### 2. Digest list (if twitter-digest skill is installed)

Check if `~/.claude/skills/twitter-digest/SKILL.md` exists. If it does:

1. Ask the user for a short list name (e.g., `ai-tools`, `ml-research`). Suggest one based on the topic.
2. Ask the user which tiers to include (default: Tier 1 + Tier 2, i.e. score >= 6).
3. Write the list to `~/.claude/skills/twitter-digest/lists/<list-name>.md` using this format:

```markdown
---
name: <list-name>
description: <derive from RELEVANCE_CRITERIA — what to surface and what to skip>
---

- @handle1
- @handle2
- @handle3
```

4. Tell the user they can now run `/twitter-digest <list-name>` or set up a recurring digest with `/loop 4h /twitter-digest <list-name>`.
5. Write all scored accounts (score + rationale) to `~/.claude/skills/twitter-vet/vetted/<list-name>.md` using the registry format defined in twitter-vet. Include all tiers and below-threshold accounts. This lets twitter-digest skip re-vetting accounts already evaluated here.

If the digest skill is not installed, skip steps 1-5 silently — don't suggest installing it.

## Cost awareness

See the `twitter` skill for full pricing and rate limit details. Key numbers for discovery:

- 20-tweet timeline sample: ~$0.003 (300 credits)
- 100-account following list: ~$0.015 (1,500 credits)
- Search (20 results): ~$0.003 (300 credits)
- A full discovery round (scoring ~15 accounts + fetching ~8 following lists + 5 searches): ~$0.18
- A complete session (3-4 rounds): ~$0.50-$1.00

Flag to the user if costs are likely to exceed $2. At typical credit balances (1K-5K), do not fire more than 3 parallel API calls.

## Key principles

1. **Hot scores**: Always recompute promise scores after scoring a new account and fetching its following list. Never use stale scores.
2. **Evaluate bios first**: Read bios and context before spending API calls on tweet sampling. Use your judgment rather than regex — you can assess relevance more accurately than keyword matching.
3. **Sequential API calls**: Avoid firing many parallel API calls to the same endpoint — rate limits will block you. 2-3 parallel calls are fine; 5+ are not.
4. **Tailored criteria**: The scoring rubric must reflect what the user actually wants, not a generic "is this account good." A researcher and a marketer want very different things.
5. **Transparent checkpoints**: Show the user what you found and let them steer. Don't run 50 API calls without checking in.
