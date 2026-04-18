---
name: skill-audit
description: Scan recent Claude Code and Codex sessions to find repeated manual workflows worth turning into skills, present the candidates, and optionally build them via /automate. Use when the user says "skill audit", "audit my skills", "what should I automate", "find automation candidates", "repeated workflows", or wants a periodic review of what to automate next.
user-invocable: true
---

# Skill audit

Scan Claude Code and Codex sessions since the last run, surface workflows the user has been doing manually more than once, and propose them as skills (or improvements to existing skills). After the user picks, dispatch each pick to `/automate` to actually build it.

This is the "automate your automations" loop: the goal is for the user to wake up knowing what to automate next, rather than noticing opportunities ad-hoc.

## Locations

- **Skill directory**: `~/My Drive/dotfiles/claude/skills/skill-audit/`
- **State file**: `last-run.txt` in the skill directory (ISO 8601 UTC timestamp)
- **Scan script**: reused from `ai-journal` — `~/.claude/skills/ai-journal/find-sessions.py`
- **Existing skills index**: `~/My Drive/dotfiles/claude/skills/` (one dir per skill, each with `SKILL.md`)

## Procedure

### 1. Determine cutoff

Read `last-run.txt`. If missing or empty, ask the user how far back to scan (default suggestion: "last 7 days"). Convert to an ISO 8601 UTC timestamp before invoking the scan script.

### 2. Scan sessions

```bash
python3 ~/.claude/skills/ai-journal/find-sessions.py "<cutoff-iso>"
```

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

**Before proposing a new skill, check `~/My Drive/dotfiles/claude/skills/` for an existing skill that already covers the workflow.** If one exists, the candidate should be framed as a *gap* ("existing skill X is missing feature Y") rather than a new skill. Use `grep -l "<keyword>"` across the skills directory to check quickly.

Group related sessions: if the same pattern shows up in 4 sessions, surface it once with 4 linked occurrences.

### 4. Present candidates

Show the user a numbered list. For each candidate:

- **Proposed name** — short kebab-case name (or `extend: <existing-skill>` if it's a gap)
- **Trigger phrases** — the natural-language the user would type to invoke it
- **What it would do** — one-sentence purpose
- **Evidence** — 2-4 bullets pointing at specific sessions (dates + short quotes from `first_user_excerpt`)
- **Approach hint** — gptel command vs Claude Code skill (since `/automate` will ask anyway, but the hint speeds the decision)

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

For each selected candidate, dispatch to `/automate` with the candidate's description as the argument. `/automate` will:

1. Decide gptel vs Claude Code skill
2. Ask the user to confirm the approach
3. Build the skill (or gptel command) and tell the user how to invoke it

Do not build the skill yourself — `/automate` is the canonical builder. Invoke it once per candidate so each gets its own design pass.

If the candidate is `extend: <existing-skill>`, skip `/automate` and edit the existing `SKILL.md` directly to add the missing procedure/section.

### 6. Update state and commit

After the user confirms the candidates are built (or declined):

1. Update `last-run.txt`:
   ```bash
   date -u +"%Y-%m-%dT%H:%M:%SZ" > ~/My\ Drive/dotfiles/claude/skills/skill-audit/last-run.txt
   ```
2. Commit the `last-run.txt` bump: `skill-audit: bump last-run to <date>`.
3. Any new skills built by `/automate` will have been committed by `/automate` itself (one commit per skill).

If the user declined all candidates, still bump `last-run.txt` — they've been reviewed.

## Scheduling

To run this weekly on autopilot (Ole Lehmann's pattern), use `/schedule`:

```
/schedule create "skill-audit" "every Monday at 09:00" "/skill-audit"
```

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
> **Skip**: one-off tasks, things that only happened once, tasks already covered by an existing skill in `~/My Drive/dotfiles/claude/skills/` (check before proposing).
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

- This skill does **not** build new skills itself. It's a scout: it finds opportunities and hands them to `/automate`. Keeping the roles separate means skill-audit can evolve its triage heuristics independently of how skills get built.
- Sessions older than the cutoff but with activity after are included (mtime-based), which is correct — ongoing sessions still count.
- Do not count sessions where the user explicitly abandoned the task ("never mind", "forget it") unless the abandoned attempts themselves form a pattern worth automating.
