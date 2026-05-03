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

1. Fetch ~20 tweets via `~/.claude/skills/twitter/lib/twitterapi.sh tweets <username>`.
2. Evaluate the tweet corpus against the rubric **and** the epistemic quality gate below. Assign a score 1-10:
   - **8-10**: Consistently produces content matching the rubric with good epistemics. Auto-add.
   - **6-7**: Some signal, some noise. Strong candidate but not overwhelming.
   - **4-5**: Occasional relevant content but mostly off-topic.
   - **1-3**: Not relevant.
3. Write a one-line rationale for the score.

### Epistemic quality gate

This is a universal criterion applied to **all** lists, independent of topical relevance. Accounts that fail this gate should be penalized by 2-3 points regardless of how well they match the rubric.

**Red flags** (penalize):
- Claims presented with more confidence than the evidence warrants
- Engagement-optimized framing: superlatives ("the BEST way"), false urgency ("you NEED to"), listicle bait ("5 things that will change how you...")
- No caveats, qualifications, or acknowledgment of limitations
- Cherry-picked examples presented as general truths
- Hype amplification without independent analysis

**Green flags** (reward):
- Confidence calibrated to evidence ("in my experience", "early results suggest", "this worked for X but YMMV")
- Explicit caveats and limitations ("this breaks down when...", "caveat: only tested on...")
- Willingness to say "I don't know" or "I was wrong"
- Distinguishing personal experience from general claims
- Engaging with counterarguments or tradeoffs rather than selling a single narrative

When in doubt, ask: "Is this person trying to inform me, or trying to impress me?" Accounts optimized for impressions over information should not pass regardless of topical fit.

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

## Committing changes

After vetting is complete, commit all changed markdown files (list files, vet registry) in one commit:

```bash
git -C ~/My\ Drive/dotfiles add claude/skills/twitter-vet/vetted/ claude/skills/twitter-digest/lists/ && git -C ~/My\ Drive/dotfiles commit -m "twitter-vet: update <list-name> registry"
```

Only stage paths that actually changed. When called from twitter-digest, skip this — twitter-digest's Step 4 handles the commit.

## Cost

~$0.003 per account vetted (one `get_user_tweets` call). Quick filter is free.
