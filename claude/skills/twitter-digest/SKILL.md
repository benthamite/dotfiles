---
name: twitter-digest
description: Fetch recent tweets from a curated list of accounts, triage for relevance, and present a digest. Use when the user says "twitter digest", "check twitter", "twitter update", "what's new on twitter", or "twitter roundup".
argument-hint: "[list-name]"
argument-source: "lists/*.md"
argument-multiple: true
user-invocable: true
model: haiku
---

# Twitter digest

Fetch tweets from a list, triage for relevance, present an org-mode digest.

**CRITICAL**: Never use MCP twitter tools. Use only `fetch-tweets.sh` (direct REST API, ~24x less data).

## Lists

Stored in `~/.claude/skills/twitter-digest/lists/<name>.md`. Format: YAML frontmatter with optional `description` (triage rubric), then `- @username` lines. If `description` is present, triage tweets; otherwise show all unfiltered.

List selection: no arg + 1 list → use it; no arg + multiple → ask; missing name → offer to create.

## Procedure

**IMPORTANT: Minimize tool calls.** Each round-trip resends the full context. Combine operations into as few bash calls as possible. Target: 3 tool calls total (fetch, write+open, update timestamp).

### 1. Fetch

Read the list file and last-run timestamp (`~/.claude/skills/twitter-digest/last-run/<name>.txt`), then run the fetch script — all in one bash call:

```bash
SKILL_DIR=~/.claude/skills/twitter-digest
LIST="<name>"
CUTOFF=$(cat "$SKILL_DIR/last-run/$LIST.txt" 2>/dev/null || echo "")
# If no cutoff and non-interactive, default to 48h ago:
# CUTOFF=$(date -u -v-48H +%Y-%m-%dT%H:%M:%SZ)
bash "$SKILL_DIR/fetch-tweets.sh" "$SKILL_DIR/lists/$LIST.md" "$CUTOFF"
```

Output format: `@user|date|likes|views|OG/RT|tweet_id|rt_user|text`, with `---RT_DISCOVERY---` separating listed accounts from RT-discovered authors.

### 2. Triage + format

From the fetch output, triage tweets using the list's `description` as rubric. Be aggressive — surface 5-15 tweets, not 200. Skip noise, hype, self-promotion, low-signal RTs.

For RT-discovered authors (after `---RT_DISCOVERY---`), triage separately and include notable ones under "Discovered accounts".

Write the org digest to a temp file and open it, all in one bash call:

```bash
cat > "$TMPFILE" <<'ORGEOF'
#+title: Twitter digest: <name> — YYYY-MM-DD
...org content...
ORGEOF
emacsclient -e "(progn (find-file \"$TMPFILE\") (goto-char (point-min)) (org-fold-show-all))"
```

Org format:
- Order by **like rate** (likes ÷ views) descending
- `[[https://x.com/user/status/ID][View on X]]` links
- Sections: `* Notable`, `* Discovered accounts` (if any), `* Accounts with nothing notable`
- Unfiltered lists: use `* All tweets` (reverse-chrono), omit other sections

### 3. Update timestamp

Write the most recent tweet's ISO timestamp to `~/.claude/skills/twitter-digest/last-run/<name>.txt`.

## Multiple lists

Process each list with a separate fetch call. Combine into one org buffer with top-level headings per list.

## Periodic use

```
/loop 4h /twitter-digest ai-tools
```
