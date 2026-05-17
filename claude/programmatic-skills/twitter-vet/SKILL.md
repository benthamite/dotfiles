---
name: twitter-vet
description: Vet, score, or decide whether to add Twitter/X accounts to a curated list. Use when asked to vet a handle, score accounts, decide whether an account belongs on a list, or when twitter-digest/twitter-discover need shared account scoring.
user-invocable: true
---

# Twitter account vetting

Shared procedure for evaluating whether a Twitter account belongs on a curated list. Referenced by `twitter-digest` and `twitter-discover`.

Use this skill when:
- A user asks to vet, score, rank, or decide whether a Twitter/X account belongs on a list.
- `twitter-digest` needs to evaluate RT-discovered accounts against an existing list rubric.
- `twitter-discover` needs shared scoring criteria for candidate accounts.

Do not use this skill for direct tweet/profile lookup or Twitter search; use `twitter` for that. Do not use it for open-ended account discovery; use `twitter-discover`, then apply this procedure only to candidate scoring. Do not mutate Twitter/X itself or use authenticated browser actions.

## Inputs and paths

Required inputs:
- Account handle(s) to evaluate.
- A list name and scoring rubric, usually the list file's `description` or `twitter-discover`'s `RELEVANCE_CRITERIA`.
- An optional `vet-threshold`; default threshold is 7.

If there is no rubric, ask for one or have the caller synthesize one before scoring. Do not invent a hidden rubric after fetching tweets.

All API access goes through the active `twitter` skill wrapper. Set `TWITTERAPI` before the first API call, then verify it exists with `test -x "$TWITTERAPI"`:

- Codex: `TWITTERAPI="/.codex/programmatic-skills/twitter/lib/twitterapi.sh"`
- Claude: `TWITTERAPI="/.claude/programmatic-skills/twitter/lib/twitterapi.sh"`

The shared vet registry source of truth is the Claude skill data path, even when invoked from Codex:

```
~/.claude/programmatic-skills/twitter-vet/vetted/<list-name>.md
```

This keeps Claude and Codex callers from splitting vetting history. Treat resolved API keys and credential environment variables as secrets: do not echo `TWITTERAPI_API_KEY*`, do not enable shell xtrace, and do not print curl commands that include API headers.

## Operating order

1. Resolve the list name, rubric, threshold, and caller context (`standalone`, `twitter-digest`, or `twitter-discover`).
2. Check the vet registry for the current list before evaluating any account.
3. Apply Procedure A using already-available metadata.
4. Apply Procedure B only to accounts that pass Procedure A and have not already been scored for this list.
5. Apply the auto-add rule only when an existing `twitter-digest` list is being updated.
6. Record results in the registry unless the caller has an explicit later persistence step, such as `twitter-discover`'s save phase.
7. Verify changed files and commit only when running standalone.

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

1. Fetch ~20 tweets via `"$TWITTERAPI" tweets <username>`.
2. Confirm the API response belongs to the requested account and contains usable tweet data. If the response is empty or malformed, report that directly instead of inferring missing content.
3. Evaluate the tweet corpus against the rubric **and** the epistemic quality gate below. Assign a score 1-10:
   - **8-10**: Consistently produces content matching the rubric with good epistemics. Auto-add.
   - **6-7**: Some signal, some noise. Strong candidate but not overwhelming.
   - **4-5**: Occasional relevant content but mostly off-topic.
   - **1-3**: Not relevant.
4. Write a one-line rationale for the score.

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

Apply this rule only when vetting for an existing `twitter-digest` list file. When invoked from `twitter-discover` before the user has chosen to save a digest list, return the score to the caller instead of editing any list file.

- Score >= 7: append `- @username` to the list file.
- Score 5-6: report as borderline in the digest output.
- Score <= 4: skip silently.

The threshold of 7 can be overridden via an optional `vet-threshold` field in the list's YAML frontmatter.

## Vet registry

All accounts ever scored against a specific list's rubric are recorded in a persistent registry at:

```
~/.claude/programmatic-skills/twitter-vet/vetted/<list-name>.md
```

**Before vetting any account**, check the registry for the current list. If the account appears in any section, skip it entirely and use the recorded outcome. Re-score only if the user explicitly asks to revisit a prior judgment.

**After each evaluation**, record the account in the appropriate section (create the file if it doesn't exist):

- Procedure A rejection → "Skipped (quick filter)"
- Procedure B score 1-3 → "Below threshold"
- Procedure B score 4-5 → "Tier 3"
- Procedure B score 6-7 → "Tier 2"
- Procedure B score 8-10 → "Tier 1"
- User manually says no → "Rejected (manual)"

Never record a scored account under "Skipped (quick filter)". That section is only for accounts rejected before a full scoring pass.

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

When running standalone, after vetting is complete, commit all changed markdown files (list files, vet registry) in one commit:

```bash
git -C ~/My\ Drive/dotfiles add claude/programmatic-skills/twitter-vet/vetted/ claude/programmatic-skills/twitter-digest/lists/ && git -C ~/My\ Drive/dotfiles commit -m "twitter-vet: update <list-name> registry"
```

Only stage paths that actually changed. When called from `twitter-digest` or `twitter-discover`, skip this; the caller's save/commit step owns persistence.

## Verification

Before reporting completion:

1. Re-read the registry entry and confirm each evaluated account appears exactly once in the correct section for its score or quick-filter outcome.
2. If auto-add applies, re-read the list file and confirm each score-above-threshold account appears exactly once as `- @username`; if auto-add does not apply, confirm the list file was not edited.
3. If an API call was used, state whether the response was successful, how many tweet samples were reviewed, and whether any cursor remained unused.
4. Run `git diff --check` after file edits and inspect `git status --short` before committing so unrelated changes are not staged.

## Cost

~$0.003 per account vetted (one `get_user_tweets` call). Quick filter is free.
