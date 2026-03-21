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

## Vet registry

All accounts ever scored against a specific list's rubric are recorded in a persistent registry at:

```
~/.claude/skills/twitter-vet/vetted/<list-name>.md
```

**Before vetting any account**, check the registry for the current list. If the account appears in any section, skip it entirely — do not re-score or re-filter.

**After each evaluation**, record the account in the appropriate section (create the file if it doesn't exist):

- Procedure A rejection → "Skipped (quick filter)"
- Procedure B score 1-3 → "Below threshold"
- Procedure B score 4-5 → "Tier 3"
- Procedure B score 6-7 → "Tier 2"
- Procedure B score 8-10 → "Tier 1"
- User manually says no → "Rejected (manual)"

Format:

```markdown
# Vet registry: <list-name>

## Tier 1 (8-10)

- **@handle** (score, YYYY-MM-DD) — one-line rationale

## Tier 2 (6-7)

- **@handle** (score, YYYY-MM-DD) — one-line rationale

## Tier 3 (4-5)

- **@handle** (score, YYYY-MM-DD) — one-line rationale

## Below threshold (1-3)

- **@handle** (score, YYYY-MM-DD) — one-line rationale

## Rejected (manual)

- **@handle** (YYYY-MM-DD) — reason or original score

## Skipped (quick filter)

- **@handle** (YYYY-MM-DD) — one-line reason
```

## Cost

~$0.003 per account vetted (one `get_user_tweets` call). Quick filter is free.
