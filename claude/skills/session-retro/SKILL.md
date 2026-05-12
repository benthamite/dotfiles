---
name: session-retro
description: Review dotfiles agent-learning inbox suggestions, interview the user about each candidate with implement/defer/drop choices, then implement accepted improvements and archive or discard the processed records.
---

# Session retro

Use this skill in the dotfiles repo when the user asks to review agent-learning
inbox items, process session retros, triage improvement suggestions, or clear
`.agent-learnings/inbox`.

This skill requires Plan mode or another runtime mode where the native
structured multiple-choice question tool is available. If the tool is
unavailable, stop before the interview phase and tell the user to rerun
`session-retro` in Plan mode. Do not fall back to typed implement/defer/drop
prompts.

The goal is to turn raw end-of-session suggestions into deliberate changes. Do
not silently promote suggestions into durable instructions, hooks, skills, or
docs. The user decides whether each candidate is implemented, deferred, or
dropped before any implementation begins.

## Storage

Use these paths relative to the dotfiles repo root:

- Inbox: `.agent-learnings/inbox/`
- Archive: `.agent-learnings/archive/`

Inbox files are local working material. Archive files record suggestions that
were implemented and the changes that implemented them. Dropped suggestions are
deleted with the system trash command rather than destructive removal.

## Workflow

### 1. Establish scope

Work from the dotfiles repo root. If the current directory is not inside
`~/My Drive/dotfiles`, stop and ask the user to rerun from the dotfiles repo.

List `.agent-learnings/inbox/` for suggestion files with an ignore-aware
filesystem command, because inbox records are intentionally ignored local
working material. Prefer:

```bash
find .agent-learnings/inbox -maxdepth 1 -type f ! -name .gitkeep -print
```

`rg --files` is not sufficient unless run with ignore-bypassing flags such as
`rg -uuu --files .agent-learnings/inbox`, because otherwise it can hide the
very records this skill is meant to process. Ignore `.gitkeep`, temporary
editor files, and directories. If no files remain after an ignore-aware check,
report that the inbox is empty and stop.

Read every inbox file before interviewing the user. Treat each distinct
actionable recommendation as a separate suggestion, even when several appear in
one session file. Preserve the source file path and enough excerpted context to
recover why the suggestion exists.

When a file is vague, split only on clear recommendation boundaries. If a note
contains background with no actionable recommendation, create one candidate
with the best concise summary and mark the uncertainty.

### 2. Rank suggestions

Before interviewing the user, rate every suggestion for expected value and sort
the interview queue from highest value to lowest value. Use judgment, but make
the rating explicit enough that the ordering is auditable. Favor suggestions
that:

- Prevent recurring failures, data loss, security mistakes, externally visible
  mistakes, or expensive user interruptions.
- Capture a clear user correction or repeated workflow friction.
- Are actionable in a specific instruction, skill, hook, script, test, or docs
  target.
- Have a broad blast radius across projects or future sessions.

Deprioritize suggestions that are one-off, already covered by existing durable
guidance, speculative, low-impact, or too vague to implement safely. Use a
simple `high`, `medium`, or `low` value rating plus one short reason. Preserve
the source file and candidate number so the original record remains traceable.

### 3. Interview the user

Process every suggestion before implementing anything. Present one suggestion
at a time, in descending value order, with:

- A short title
- The source file
- The value rating and reason
- A concise summary of the proposed improvement
- Why it might matter
- Any obvious risk, cost, or missing information
- The implementation proposal if the user chooses `implement`, including:
  - Target type: `instruction`, `skill`, `hook`, `script`, `docs`, `test`,
    `decision`, or `unknown`
  - Likely owner: the specific existing skill, hook, README, instruction file,
    script, or project area to change
  - Expected edit shape: add guidance, revise trigger text, add a helper
    script, add/update tests, update docs, or another concrete action
  - Verification plan: the command, audit, fixture, or manual check that would
    prove the implementation worked

Ask the user to choose exactly one option:

- `implement`: apply this suggestion after the interview phase
- `defer`: leave this suggestion in the inbox for a later session
- `drop`: discard this suggestion

Use the native structured multiple-choice question tool for this choice. Use
exactly these three choices, with `implement` first when it is the recommended
option for high-value actionable suggestions, otherwise put the best
conservative option first and mark it as recommended. Include the
implementation proposal in the question text or option descriptions so the user
knows what implementation would mean before choosing. If the native
multiple-choice tool is unavailable, stop and report that `session-retro` must
run in Plan mode or another mode that exposes the tool. Do not ask the user to
type `implement`, `defer`, or `drop`.

If the user gives a free-form answer, map it conservatively:

- Clear approval, "yes", "do it", or equivalent means `implement`.
- "Later", "not now", "maybe", "park it", or unresolved questions mean
  `defer`.
- "No", "not useful", "irrelevant", "obsolete", or equivalent means `drop`.

If the answer is ambiguous, ask once for clarification.

### 4. Plan accepted work

After all suggestions are classified, group implemented suggestions into
logical changes. Identify which existing skill, hook, README, instruction file,
or script owns each change.

Before editing, report a compact implementation plan listing:

- Implemented suggestions grouped by target area
- Deferred suggestions that will stay in the inbox
- Dropped suggestions that will be trashed
- Verification commands you expect to run

Do not implement deferred or dropped suggestions. Do not use an accepted
suggestion as permission for externally visible actions such as posting,
opening issues, creating PRs, sending messages, or changing remote services.
Ask for explicit confirmation if an accepted suggestion requires one of those
actions.

### 5. Implement accepted suggestions

Apply each accepted suggestion using the normal dotfiles rules:

- Use the most specific relevant skill for the implementation work.
- Keep Claude and Codex counterparts synchronized when editing paired agent
  configuration.
- Update `README.org`, `claude/README.org`, `codex/README.org`, or other
  documentation required by the touched area.
- Preserve unrelated user changes.
- Verify the behavior as far as the change allows.

If an accepted suggestion turns out to be unsafe, obsolete, impossible, or based
on a false premise, stop implementing that suggestion and report it. Ask whether
to defer or drop it instead of quietly changing its classification.

### 6. Reconcile inbox files

After implementation and verification, rewrite the suggestion records at
suggestion granularity:

- For each implemented suggestion, write an archive record under
  `.agent-learnings/archive/YYYY-MM-DD/` with:
  - Original source file
  - Suggestion title and summary
  - User decision: `implement`
  - Files changed
  - Verification performed
  - Commit hash, if committed
- For deferred suggestions, keep them in `.agent-learnings/inbox/` as concise
  Markdown records. If the original source file contained a mix of implemented,
  deferred, and dropped suggestions, replace it with one or more deferred-only
  records so processed items do not reappear.
- For dropped suggestions, delete their processed records using `trash` when
  available. If `trash` is unavailable, move them to the platform trash through
  another non-destructive mechanism. If no non-destructive deletion mechanism is
  available, ask before permanent deletion.

Never leave an implemented or dropped suggestion in the inbox.

### 7. Commit and report

Commit the skill/config/docs implementation as normal dotfiles work. Do not
commit raw inbox suggestion files unless the user explicitly asks; they are
local working material.

Before finishing:

1. Run `bin/ai-config-sync audit`.
2. Inspect `git status --short`.
3. Confirm that the inbox contains only deferred suggestions.
4. Confirm that implemented suggestions have archive records.

Report:

- Implemented suggestions and the commit hash
- Deferred suggestions still in the inbox
- Dropped suggestions trashed
- Archive path(s)
- Verification run and anything not verified
