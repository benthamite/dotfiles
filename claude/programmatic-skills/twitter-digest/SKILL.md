---
name: twitter-digest
description: Fetch recent tweets from a curated account list, triage them against its rubric, save an org digest, and update last-run state. Use twitter for direct lookups, twitter-discover for finding accounts, and twitter-vet for account scoring.
argument-hint: "[list-name]"
argument-source: "lists/*.md"
argument-multiple: true
user-invocable: true
---

# Twitter digest

When Step 3 runs, read the active `twitter-vet` skill copy:

- Codex: `$HOME/.codex/skills/twitter-vet/SKILL.md`
- Claude: `$HOME/.claude/skills/twitter-vet/SKILL.md`

For each Bash command below, set paths for the current agent before using them:

- Codex: `DIGEST_DIR="$HOME/.codex/skills/twitter-digest"; DIGEST_REPO_DIR="codex/skills/twitter-digest"; VET_SKILL_DIR="$HOME/.codex/skills/twitter-vet"; TWITTERAPI="$HOME/.codex/skills/twitter/lib/twitterapi.sh"`
- Claude: `DIGEST_DIR="$HOME/.claude/skills/twitter-digest"; DIGEST_REPO_DIR="claude/skills/twitter-digest"; VET_SKILL_DIR="$HOME/.claude/skills/twitter-vet"; TWITTERAPI="$HOME/.claude/skills/twitter/lib/twitterapi.sh"`

Shared paired data:

- Curated list files must stay synchronized in both `claude/skills/twitter-digest/lists/` and `codex/skills/twitter-digest/lists/`.
- The vet registry source of truth is `VET_REGISTRY_DIR="$HOME/.claude/skills/twitter-vet/vetted"` (`VET_REGISTRY_REPO_DIR="claude/skills/twitter-vet/vetted"`), as documented by `twitter-vet`.

Digest archives and last-run timestamps are active-tool runtime state; do not write those into the other tool's skill tree.

## COST RULE — READ THIS FIRST

Each tool call resends ~20K tokens of system context. Core digest creation is **exactly 2 Bash calls**; the final commit is one additional Bash call. Vetting discovered accounts adds N read-only API calls through the `twitterapi.sh` wrapper (typically 0-5, ~$0.003 each):

1. **Bash**: run `fetch-tweets.sh` (handles list reading, cutoff, fetching, RT discovery)
2. **Bash**: write org file + open in Emacs + update last-run timestamp + save digest (one heredoc command)
3. **Bash/API** (0-5 calls): vet discovered accounts — see Step 3
4. **Bash**: commit all changed files — see Step 4

Any extra tool calls (Read, Write, separate Bash) waste $0.02-0.05 each. Do NOT read list files or last-run files separately — the fetch script handles that.

Never use MCP twitter tools for the digest itself or for discovered-account vetting. Use the digest script and the active `twitterapi.sh` wrapper only.

Treat resolved API keys and account-specific environment variables as secrets: do not echo `TWITTERAPI_API_KEY*`, do not enable shell xtrace, and do not print curl headers.

## Step 1: Fetch

```bash
DIGEST_DIR="<active twitter-digest skill dir from above>"
bash "$DIGEST_DIR/fetch-tweets.sh" <list-name>
```

Output:
```
LIST:<name>
CUTOFF:<iso-timestamp>
DESCRIPTION:<triage rubric or empty>
---TWEETS---
@user|date|likes|views|OG/RT|tweet_id|rt_user|text
...
---RT_DISCOVERY---
@user|date|likes|views|OG/RT|tweet_id|rt_user|text
---RT_AUTHORS---
@user|followers|bio_text
```

If no cutoff file exists and running non-interactively, defaults to 48h. If interactive, ask the user.

## Step 2: Triage + output

If DESCRIPTION is non-empty, triage tweets: keep only tweets that **directly match** the rubric. The rubric is a hard filter, not a soft signal — if a tweet doesn't clearly fit the described topic, drop it regardless of engagement. Be aggressive — 5-15 tweets from 20+ accounts. Skip noise, hype, self-promotion, low-signal RTs, and anything the rubric's "skip" list covers (read it carefully).

For RT-discovered authors (after `---RT_DISCOVERY---`), triage separately. **Before including any discovered account in the output**, check `$VET_REGISTRY_DIR/<list-name>.md` — if the account appears in "Rejected", "Below threshold", or "Skipped" sections, exclude their tweets from the digest entirely.

Then emit ONE bash command that does everything:

```bash
DIGEST_DIR="<active twitter-digest skill dir from above>"
TMPFILE=$(mktemp /tmp/twitter-digest-XXXXXX.org) && cat > "$TMPFILE" <<'ORGEOF'
#+title: Twitter digest: <name> — YYYY-MM-DD

* Notable

** @username (Mon Mar 17 14:30)
Tweet text
Likes: N | Views: N | Like rate: X.X%
[[https://x.com/username/status/ID][View on X]]

* Discovered accounts
...

* Accounts with nothing notable
@user1, @user2, ...
ORGEOF
emacsclient -e "(progn (find-file \"$TMPFILE\") (goto-char (point-min)) (org-fold-show-all))" && echo "<iso-timestamp-of-newest-tweet>" > "$DIGEST_DIR/last-run/<name>.txt" && cp "$TMPFILE" "$DIGEST_DIR/digests/<name>-YYYY-MM-DD.org"
```

Order by like rate (likes÷views) descending. Omit "Discovered accounts" if none. Unfiltered lists (no description): show all tweets reverse-chrono under `* All tweets`, no other sections.

## Saving digests

Always save a copy of the digest to `$DIGEST_DIR/digests/<name>-YYYY-MM-DD.org`. For multi-list digests, use the combined filename (e.g. `ai-tools-ai-macrostrategy-2026-03-20.org`).

## Step 3: Vet discovered accounts

If `---RT_DISCOVERY---` yielded accounts:

1. **Check registry**: read `$VET_REGISTRY_DIR/<list-name>.md` if it exists. Any RT-discovered account already listed there: skip re-vetting, use the recorded score directly.
2. **Quick filter** (Procedure A from twitter-vet): use `---RT_AUTHORS---` metadata (bio, followers) and `---RT_DISCOVERY---` tweets to filter remaining accounts. Cap at top 5 candidates by quick-filter confidence. Skip the rest.
3. **Full scoring** (Procedure B from twitter-vet): for each account that passes, call `"$TWITTERAPI" tweets <username>`. Score 1-10 against the list's `description`.
4. **Auto-add**: score >= 7 (or `vet-threshold` from list YAML) → append `- @username` to both paired list files (`claude/skills/twitter-digest/lists/<list-name>.md` and `codex/skills/twitter-digest/lists/<list-name>.md`). Score 5-6 → report as borderline in the digest. Score <= 4 → skip.
5. **Update digest and lists**: emit one final Bash call to append a `* Vetted accounts` section to the org file (replacing `* Discovered accounts`) showing each account's score and rationale, and append any new `- @username` lines to both paired list files.
6. **Update vet registry**: append newly scored accounts to `$VET_REGISTRY_DIR/<list-name>.md` (creating the file with the standard header if it doesn't exist). Include all accounts that went through full scoring, regardless of outcome.

If the API returns an auth/rate-limit/error response during vetting, report the failure directly. Do not auto-add accounts or advance the vet registry based on incomplete data.

## Step 4: Commit

After all steps are complete, commit all changed files in one commit:

```bash
DIGEST_REPO_DIR="<active repo-relative twitter-digest dir from above>"
VET_REGISTRY_REPO_DIR="claude/skills/twitter-vet/vetted"
git -C "$HOME/My Drive/dotfiles" add "$DIGEST_REPO_DIR/digests/" "$DIGEST_REPO_DIR/last-run/" "claude/skills/twitter-digest/lists/" "codex/skills/twitter-digest/lists/" "$VET_REGISTRY_REPO_DIR/" && git -C "$HOME/My Drive/dotfiles" commit -m "twitter-digest: <list-name(s)> YYYY-MM-DD"
```

Only stage paths that actually changed. If vetting didn't run, omit the list and vet registry paths.

## Multiple lists

Run fetch-tweets.sh once per list. Combine into one org buffer with `* <list-name>` top-level headings.

## List management

Lists: `$DIGEST_DIR/lists/<name>.md`. YAML frontmatter with optional `description` (triage rubric), then `- @username` lines. No arg + 1 list → use it; multiple → ask. Treat list files as paired source data: any list membership edit must be applied to both Claude and Codex copies.

## Verification

Before committing or reporting completion:

1. Confirm fetch output includes `LIST:`, `CUTOFF:`, `DESCRIPTION:`, and `---TWEETS---`. If the script exits with "no listed account fetches succeeded", stop and report the credential/connectivity problem.
2. Confirm the saved digest exists and is non-empty at `$DIGEST_DIR/digests/<name>-YYYY-MM-DD.org`.
3. Confirm `$DIGEST_DIR/last-run/<name>.txt` contains the newest returned tweet timestamp used for the run.
4. Run `git -C "$HOME/My Drive/dotfiles" diff --check -- "$DIGEST_REPO_DIR" claude/skills/twitter-digest/lists codex/skills/twitter-digest/lists "$VET_REGISTRY_REPO_DIR"` and inspect `git status --short` so unrelated dirty files are not staged.
