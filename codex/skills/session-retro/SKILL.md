---
name: session-retro
description: Review dotfiles agent-learning inbox suggestions, interview the user about each candidate with implement/defer/drop choices, then implement accepted improvements and archive or discard the processed records.
---

# Session retro

Use this skill in the dotfiles repo when the user asks to review agent-learning
inbox items, process session retros, triage improvement suggestions, or clear
`.agent-learnings/inbox`.

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

List `.agent-learnings/inbox/` for suggestion files. Ignore `.gitkeep`,
temporary editor files, and directories. If no files remain, report that the
inbox is empty and stop.

Read every inbox file before interviewing the user. Treat each distinct
actionable recommendation as a separate suggestion, even when several appear in
one session file. Preserve the source file path and enough excerpted context to
recover why the suggestion exists.

When a file is vague, split only on clear recommendation boundaries. If a note
contains background with no actionable recommendation, create one candidate
with the best concise summary and mark the uncertainty.

### 2. Interview the user

Process every suggestion before implementing anything. Present one suggestion
at a time with:

- A short title
- The source file
- A concise summary of the proposed improvement
- Why it might matter
- Any obvious risk, cost, or missing information

Ask the user to choose exactly one option:

- `implement`: apply this suggestion after the interview phase
- `defer`: leave this suggestion in the inbox for a later session
- `drop`: discard this suggestion

If a structured question tool is available, use it with those three choices.
Otherwise ask a concise plain-text question and wait for the user. Continue
until every suggestion has a recorded decision.

If the user gives a free-form answer, map it conservatively:

- Clear approval, "yes", "do it", or equivalent means `implement`.
- "Later", "not now", "maybe", "park it", or unresolved questions mean
  `defer`.
- "No", "not useful", "irrelevant", "obsolete", or equivalent means `drop`.

If the answer is ambiguous, ask once for clarification.

### 3. Plan accepted work

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

### 4. Implement accepted suggestions

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

### 5. Reconcile inbox files

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

### 6. Commit and report

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
