---
name: skill-discovery
description: Scan recent Claude Code and Codex sessions to discover repeated manual workflows worth turning into skills, present the candidates, and optionally build them via automate. Use when the user says "skill discovery", "what should I automate", "find automation candidates", "repeated workflows", "discover skill candidates", or wants a periodic review of what workflows to automate next. Do not use for one-off requests to build an already specified automation, AI journal posts, or third-party skill installation.
user-invocable: true
---

# Skill discovery

Scan Claude Code and Codex sessions since the last run, surface workflows the user has been doing manually more than once, and propose them as skills (or improvements to existing skills). After the user picks, dispatch each pick to the local `automate` skill to actually build it.

This is the "automate your automations" loop: the goal is for the user to wake up knowing what to automate next, rather than noticing opportunities ad-hoc.

## Locations

- **Claude skill directory**: `~/My Drive/dotfiles/claude/skills/skill-discovery/`
- **Codex skill directory**: `~/My Drive/dotfiles/codex/skills/skill-discovery/`
- **State file**: `last-run.txt` in both paired skill directories (ISO 8601 UTC timestamp; keep the two copies identical)
- **Scan script**: reused from `ai-journal`; use the current tool's paired copy:
  - Claude Code: `~/My Drive/dotfiles/claude/skills/ai-journal/find-sessions.py`
  - Codex: `~/My Drive/dotfiles/codex/skills/ai-journal/find-sessions.py`
- **Existing skills indexes**:
  - `~/My Drive/dotfiles/claude/skills/`
  - `~/My Drive/dotfiles/codex/skills/`

## Procedure

### 1. Determine cutoff

Read `last-run.txt` from both paired skill directories. They should match; if they differ, resolve the drift before scanning rather than choosing one silently. If both files are missing or empty, ask the user how far back to scan, suggesting "last 7 days" as one option. Convert the user's answer to an ISO 8601 UTC timestamp before invoking the scan script.

### 2. Scan sessions

```bash
# Claude Code
python3 ~/My\ Drive/dotfiles/claude/skills/ai-journal/find-sessions.py "<cutoff-iso>"

# Codex
python3 ~/My\ Drive/dotfiles/codex/skills/ai-journal/find-sessions.py "<cutoff-iso>"
```

Run the command for the current tool; the script copies should stay identical.

Output is tab-separated: `kind<TAB>start_iso<TAB>cwd<TAB>path<TAB>first_user_excerpt`.

If the scan returns more than ~30 sessions, delegate triage (step 3) to a subagent so the transcripts don't flood the main context. Use the subagent prompt template at the end of this file.

### 3. Triage for repeatable workflows

Read the sessions and look for **patterns that repeat across two or more sessions**. Good signals:

- The user ran the same multi-step procedure in different sessions (e.g. "open X, grep Y, edit Z, commit")
- The user had to remind the agent of the same context, conventions, or gotchas
- The user fixed the same *class* of bug or handled the same *class* of request more than once
- The user wrote a custom one-off prompt that they clearly could have reused
- The user chained the same 2-3 tools/commands repeatedly

**Ignore one-offs.** The whole point is to find *repeated* friction, not to propose a skill for every interesting thing that happened.

**Before proposing a new skill, check both `~/My Drive/dotfiles/claude/skills/` and `~/My Drive/dotfiles/codex/skills/` for an existing skill that already covers the workflow.** If one exists, the candidate should be framed as a *gap* ("existing skill X is missing feature Y") rather than a new skill. Use `rg -l "<keyword>" ~/My\ Drive/dotfiles/{claude,codex}/skills` to check quickly. If a skill exists on only one side, mention the pairing gap instead of proposing a duplicate.

Group related sessions: if the same pattern shows up in 4 sessions, surface it once with 4 linked occurrences.

### 4. Present candidates

Show the user a numbered list. For each candidate:

- **Proposed name** — short kebab-case name (or `extend: <existing-skill>` if it's a gap)
- **Trigger phrases** — the natural-language the user would type to invoke it
- **What it would do** — one-sentence purpose
- **Evidence** — 2-4 bullets pointing at specific sessions (dates + short quotes from `first_user_excerpt`)
- **Approach hint** — gptel command vs agent skill (since `automate` will ask anyway, but the hint speeds the decision)

Format example:

```
## 1. /pr-describe — generate PR descriptions from commit history

**Trigger phrases**: "describe this PR", "write a PR description", "summarize branch"

**What it would do**: Read commits since the branch diverged, classify them, and produce a PR description in the project's house style.

**Evidence**:
- 2026-04-12, `~/repos/tlon.el`: "can you write a PR description for this branch"
- 2026-04-14, `~/repos/stafforini.com`: "summarize the changes since master and draft a PR body"
- 2026-04-16, `~/repos/consensus-trader`: same request, different repo

**Approach hint**: Claude Code skill (needs git + filesystem). Low complexity.
```

Then ask: "Which should I build? (e.g. `1, 3` or `all` or `none`)"

### 5. Build selected candidates

For each selected candidate, dispatch to the local `automate` skill with the candidate's description as the argument (`/automate` in Claude Code). `automate` will:

1. Decide gptel vs agent skill
2. Ask the user to confirm the approach
3. Build the skill (or gptel command) and tell the user how to invoke it

Do not build new skills yourself — `automate` is the canonical builder. Invoke it once per candidate so each gets its own design pass.

If the candidate is `extend: <existing-skill>`, skip `automate` and edit the existing paired `SKILL.md` files directly to add the missing procedure/section. Update directly required docs, run the relevant verification for that skill, and keep Claude/Codex copies synchronized unless `ai-config-sync.json` records an intentional divergence.

### 6. Update state and commit

After the user confirms the candidates are built (or declined):

1. Update both paired `last-run.txt` files to the same timestamp:
   ```bash
   stamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
   printf '%s\n' "$stamp" > ~/My\ Drive/dotfiles/claude/skills/skill-discovery/last-run.txt
   printf '%s\n' "$stamp" > ~/My\ Drive/dotfiles/codex/skills/skill-discovery/last-run.txt
   ```
2. Commit the paired `last-run.txt` bump: `skill-discovery: bump last-run to <date>`.
3. Any new skills built by `automate` will have been committed by `automate` itself (one commit per skill).

If the user declined all candidates, still bump `last-run.txt` — they've been reviewed.

## Scheduling

To run this weekly on autopilot (Ole Lehmann's pattern), use `/schedule` and set email as the notification method:

```
/schedule create "skill-discovery" "every Monday at 09:00" "/skill-discovery"
```

If `/schedule` prompts for notification settings or exposes them separately, choose email; do not leave the schedule on an in-app/default-only notification.

The cron trigger will fire the skill; the agent will run steps 1-4 and stop at "Which should I build?" — the user picks up from there interactively when they next open Claude Code.

## Subagent prompt template

When session count is large, delegate step 3 to a subagent:

> You are reviewing Claude Code and Codex session transcripts to find **repeated manual workflows** the user could turn into skills.
>
> **Criteria for a candidate** (must have more than one occurrence):
> - Same multi-step procedure across different sessions
> - Same context/conventions the user had to re-supply
> - Same class of bug or request, resolved the same way
> - Same custom one-off prompt used more than once
> - Same 2-3 tools/commands chained repeatedly
>
> **Skip**: one-off tasks, things that only happened once, tasks already covered by an existing skill in `~/My Drive/dotfiles/claude/skills/` or `~/My Drive/dotfiles/codex/skills/` (check both before proposing).
>
> **Input**: the session list below (tab-separated `kind<TAB>start<TAB>cwd<TAB>path<TAB>excerpt`). Read the full `.jsonl` for any session whose excerpt looks like a repeat of another, extract the pattern, and return candidates in this format:
>
> ```
> ## <proposed-name> — <one-line purpose>
> **Trigger phrases**: "phrase 1", "phrase 2"
> **What it would do**: <one sentence>
> **Evidence**:
> - <date>, <cwd>: <quote>
> - <date>, <cwd>: <quote>
> **Approach hint**: <gptel command | Claude Code skill>, <complexity note>
> ```
>
> Aim for 3-8 candidates. Under 800 words. If nothing repeats enough to justify a skill, say so explicitly rather than padding the list.
>
> Sessions:
> ```
> <tsv output of find-sessions.py>
> ```

## Notes

- This skill does **not** build new skills itself. It's a scout: it finds opportunities and hands them to `automate`. Keeping the roles separate means skill-discovery can evolve its triage heuristics independently of how skills get built.
- This is a paired Claude/Codex skill. Keep `SKILL.md` and `last-run.txt` synchronized across both copies unless `ai-config-sync.json` records an intentional divergence.
- Sessions older than the cutoff but with activity after are included (mtime-based), which is correct — ongoing sessions still count.
- Do not count sessions where the user explicitly abandoned the task ("never mind", "forget it") unless the abandoned attempts themselves form a pattern worth automating.
