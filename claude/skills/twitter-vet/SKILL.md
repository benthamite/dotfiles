---
name: twitter-vet
description: Shared account vetting procedure for evaluating whether a Twitter account is a good fit for a curated list. Used by twitter-digest and twitter-discover.
---

# Twitter account vetting

Shared procedure for evaluating whether a Twitter account belongs on a curated list. Referenced by `twitter-digest` and `twitter-discover`.

## Procedure A: Quick filter (no API calls)

Input: username, bio, follower count, available recent tweets (text + engagement).
Output: **PASS** (advance to full scoring) or **SKIP** (with one-line reason).

Rules:

1. Skip if followers < 500 (too small) or > 2M (likely media/celebrity, rarely original niche content).
2. Skip if bio is clearly irrelevant to the scoring rubric. Use judgment, not keyword matching.
3. Skip if all available tweets are low-effort RTs, self-promotion, or off-topic for the rubric.
4. Otherwise, PASS.

## Procedure B: Full scoring (1 API call per account)

Input: username, scoring rubric (the list's `description` or the discover skill's `RELEVANCE_CRITERIA`).

Steps:

1. Fetch 20 tweets via `mcp__twitterapi-io__get_user_tweets` (userName: username, count: "20").
2. Evaluate the tweet corpus against the rubric. Assign a score 1-10:
   - **8-10**: Consistently produces content matching the rubric. Auto-add.
   - **6-7**: Some signal, some noise. Strong candidate but not overwhelming.
   - **4-5**: Occasional relevant content but mostly off-topic.
   - **1-3**: Not relevant.
3. Write a one-line rationale for the score.

## Auto-add rule

- Score >= 7: append `- @username` to the list file.
- Score 5-6: report as borderline in the digest output.
- Score <= 4: skip silently.

The threshold of 7 can be overridden via an optional `vet-threshold` field in the list's YAML frontmatter.

## Cost

~$0.003 per account vetted (one `get_user_tweets` call). Quick filter is free.
