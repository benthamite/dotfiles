---
name: update-log
description: End-of-session bookkeeping. Use when the user says /update-log, update log, session log, close out, wrap up, or asks to save project progress. Also use at the end of any session that changed durable project state, including first-run projects without existing log conventions.
---

# End-of-session log update

Perform the following bookkeeping steps to preserve this session's work for future sessions.

## Triage first (may be a no-op)

Before touching any file, decide whether this session changed durable project state, using the criteria in **When to run** below. This decision is part of the skill, not a precondition the caller is assumed to have checked: the skill is meant to be invoked unconditionally at session end — manually or via the end-of-session chain — and **doing nothing is a valid, friction-free outcome**.

- **If nothing durable changed** (purely conversational Q&A, a quick read-only lookup, or trivial edits with nothing a future agent would need to recover), do nothing: write no log, change no files, make no commit. Report `No durable changes — skipping update-log.` and stop.
- **Otherwise**, proceed to Step 0.

Exception: if the user explicitly typed `/update-log` or asked for specific bookkeeping, honor it — proceed even if the session was marginal, and do what was asked.

### Non-interactive runs (`--auto`)

`--auto` marks a run from the end-of-session chain rather than a deliberate user invocation. In this mode, never block on input:

- If the project has no existing log conventions, perform **First-run setup**
  with deterministic defaults instead of asking: use `logs/` as the log
  directory, keep session logs local-only by adding the log directory to
  `.gitignore`, do not create `decisions/` or `decisions-summary.md`, create or
  update `CLAUDE.md`, then continue to write the session log.
- Make no other interactive prompts. If a step would need a decision only the user can make, record it in the log and final report and continue with the safe default or skip that step, rather than asking.

## When to run

Run this skill at the end of any session that changed durable project state. Durable project state includes:

- code, docs, configuration, workflow, or data changes;
- new findings, debugging results, validation results, or service-state changes that future sessions should know;
- decisions, active TODOs, blockers, next steps, or status changes;
- external-service work whose outcome matters later, such as Slack/Google/GitHub/Asana changes;
- any session whose results should be visible in the project note, current project list, or next meeting prep.

Do not run it after purely conversational Q&A, quick read-only lookups, or sessions where nothing durable changed. If uncertain, prefer running it when the project already has logs and the session produced information a future agent would otherwise need to recover from the transcript.

Session-end hooks or reminders should point to this policy instead of duplicating their own criteria. Hooks may invoke this skill unattended with `--auto`; in that mode it uses deterministic first-run defaults and safe no-op behavior instead of prompting. Non-auto hooks may remind or block session close when durable state appears unsaved, but should not silently perform the mutating workflow without user intent.

If the user explicitly invokes `/update-log` or otherwise asks for end-of-session
bookkeeping after a log, pointer, or opportunistic summary was already written,
still run the complete checklist below. An existing log or `CLAUDE.md` pointer is
only a starting state: continue through decision-record review, post-update-log
hooks, staging verification, commit completeness, and publish checks. Do not
stop at "the log already exists" unless every later step has also been checked.

## Step 0: Detect project setup

Determine the project's log directory and whether decisions are tracked:

0. **Snapshot the working tree before editing** with `git status --short`. If `CLAUDE.md`, `decisions-summary.md`, existing `decisions/` files, or any hook-managed status files are already dirty, inspect their current diff before changing them. Use this baseline in Step 5 so pre-existing user or concurrent-agent hunks do not get staged with this bookkeeping commit.

1. **Read `CLAUDE.md`** in the project root. For this workflow, `CLAUDE.md` is the canonical session-log index unless the project explicitly says otherwise; in Codex sessions, `AGENTS.md` may contain agent instructions but is not the session-log pointer. Look for a reference to a session log file — either:
   - A path like `<dir>/YYYY-MM-DD.md` in a "Latest session" section (current format), or
   - A legacy `@<dir>/YYYY-MM-DD.md` import (old format — will be migrated in Step 2).
   
   Extract the directory portion — that is the log directory.

2. **Check for a decisions directory**: look for a `decisions/` directory in the project root and a `decisions-summary.md` file.

3. **If no log reference was found in CLAUDE.md** (or CLAUDE.md doesn't exist), this is a first-run setup. Proceed to the **First-run setup** section below. Otherwise, skip to **Step 1**.

### First-run setup

This project doesn't have session logging set up. In an interactive run, gather
setup preferences before proceeding. In a `--auto` run, do not ask: use `logs/`
as the log directory, add the log directory to `.gitignore`, leave decision
tracking disabled, create or update `CLAUDE.md`, and continue.

1. If not running with `--auto`, ask all first-run questions together. If a structured user-input tool such as `AskUserQuestion` is available, use it; otherwise ask a concise plain-text question:
   - "Where should session logs be stored?" Suggest `logs/` as the default and `docs/logs/` as an alternative, while allowing a custom path.
   - "Should the session log directory be added to `.gitignore` so logs remain local-only?" Recommend Yes for public repos or any repo where session logs might contain private context; No is valid for private repos where logs are intended to be shared.
   - "Track architectural decisions in a `decisions/` directory?" Both Yes and No are valid.
   If running with `--auto`, set the answers internally to `logs/`, "Yes,
   gitignore the log directory", and "No decisions."

2. Create the chosen log directory if it doesn't exist.

3. If the user chose local-only logs, ensure the chosen log directory is ignored
   before writing the first log:
     - Add a root-relative directory pattern for the log directory to
       `.gitignore` if no existing ignore rule already covers it, e.g. `/logs/`
       for `logs/` or `/docs/logs/` for `docs/logs/`.
     - Do not remove existing `.gitignore` entries or reorder unrelated rules.
     - Verify the dated log path will be ignored with `git check-ignore -v`
       after the log file exists.

4. If decisions were opted in:
     - Create `decisions/` directory.
     - Create `decisions-summary.md` with the initial table header:
       ```
       # Decision records (summary)

       Full details with rejected alternatives and evidence are in `decisions/`. Read the relevant file before proposing changes to a covered subsystem.

       | #   | Topic | Decision | Status |
       |-----|-------|----------|--------|
       ```
     - Add a `@decisions-summary.md` reference to CLAUDE.md so decisions are auto-loaded into context.

5. **Update CLAUDE.md**:
   - If CLAUDE.md doesn't exist, create it with a minimal structure containing the project name (from the directory name or `package.json`/`pyproject.toml` if available), a "Latest session" section, and (if decisions were opted in) a "Decision records" section with the `@decisions-summary.md` reference.
   - If CLAUDE.md exists but has no "Latest session" section, append one.
   - The "Latest session" section will be populated in Step 2 with a summary + pointer (not an `@` import).

Then proceed to Step 1.

## Step 1: Create a session log file

Create a new file at `<log_dir>/YYYY-MM-DD.md` using the environment/session date. If date context is ambiguous or conflicting, state the date and timezone you are using before writing. If a file for today already exists, append to it with a horizontal rule separator.

The file should contain:

- **Title**: `# YYYY-MM-DD: <brief title>`
- **What was done**: Summary of experiments, code changes, and analysis performed.
- **Key findings**: Any new discoveries, bugs found/fixed, or validated/invalidated hypotheses.
- **Results**: Performance numbers, sweep outcomes, or other quantitative results.
- **Open questions**: What was left unfinished or what should be explored next.

Be concise but specific. Include exact numbers where available (counts, percentages, timings). Future sessions may need to understand *why* decisions were made, so document reasoning for non-obvious choices.

## Step 2: Update the CLAUDE.md session pointer

How CLAUDE.md is maintained depends on its shape — the file itself tells you which mode to use:

### Map mode

CLAUDE.md has a `## Current focus` section (and no `## Latest session`). The file is a stable map and the session narrative belongs in the project's brief (e.g. the `.org`), not here. The brief itself is refreshed by the post-update-log hook in Step 4. In this mode:

1. Update `## Current focus` to a **single line** reflecting the session's net current state.
2. Keep the `## Read first` pointers accurate if any referenced file moved or was added.
3. Do **not** add a `## Latest session` narrative, and do not lengthen `Current focus` past one line.

This is the shape defined by a project's documentation conventions (for Epoch, `projects/context/project-doc-conventions.md`). When in doubt about what belongs in the map versus the brief, follow that doc.

### Latest-session mode

CLAUDE.md has a `## Latest session` section, or neither section (default/legacy). Update the "Latest session" section with:

1. A **2–4 sentence summary** of this session's work (what was done, key outcomes, important numbers).
2. A **pointer** to the full log file: `Full details: <log_dir>/YYYY-MM-DD.md`

Example:

```
## Latest session

Externalized reconciliation conventions from CLAUDE.md, removing @-imports of session log and decisions summary. Effective context load dropped from 479 to 62 lines. Created `docs/reconciliation_conventions.md` as the new authoritative location.

Full details: session-logs/2026-04-13.md
```

Do NOT use an `@` import — the full log can be hundreds of lines and should not be injected into every session. The summary gives the next session enough context to orient; the pointer lets it read deeper on demand.

If CLAUDE.md currently has a legacy `@<log_dir>/...` import, replace it with the summary + pointer format.

### Sibling AGENTS.md

Many Epoch projects keep a sibling `AGENTS.md` next to `CLAUDE.md` as a Codex-side mirror of the same content. When that pairing exists, the same edit must land in both files in this run; project-local hooks otherwise fire a reciprocal reminder and block the bookkeeping commit. Procedure:

1. Before editing, `diff -u CLAUDE.md AGENTS.md`. If they were already in sync, treat them as mirrors. If they were already drifted, do not assume mirror semantics — apply the Latest-session edit by hand in both, preserving each file's intended divergence.
2. After updating CLAUDE.md, mirror the change with the first available helper:
   use the project-local `bin/mirror-claude-agents <project-dir>` when present,
   otherwise fall back to
   `/Users/pablostafforini/My Drive/dotfiles/bin/mirror-claude-agents
   <project-dir>`. The helper copies CLAUDE.md → AGENTS.md, no-ops if they're
   already in sync, and reports if only one of the two exists. Pass `--check`
   first to confirm exactly what it would copy, then run the same helper without
   `--check` if mirroring is needed.
3. Verify with a final `diff -u CLAUDE.md AGENTS.md` returning no output before staging.

## Step 3: Record decisions

If a `decisions/` directory exists in the project root, use the `record-decisions` skill if available to check whether any architectural or algorithmic decisions were made this session. If the skill is unavailable, follow the local workflow directly: inspect `decisions/`, identify qualifying decisions, add `decisions/NNN.md` entries, and update `decisions-summary.md`. If new entries are added, they will be included in the commit.

If no `decisions/` directory exists, skip this step.

## Step 4: Run post-update-log hooks

Walk up ancestor directories from the project root to the git root (inclusive). For each ancestor, check whether `<ancestor>/context/post-update-log-hook.md` exists. If found, read and follow its instructions. Run all matching hooks, innermost first.

This lets parent directories define project-family-level bookkeeping — master project list updates, shared status syncing, meeting action item reconciliation, etc. — that fires automatically after every `/update-log` invocation, without bloating per-session CLAUDE.md context.

For Epoch project notes, the main project `.org` file is a concise ground-truth brief, not a chronological session dossier; its target shape is defined in `projects/context/project-doc-conventions.md`. When a hook or local convention asks you to update it:

- refresh current-state fields and sections from the session log just written;
- keep active work as org `TODO` headings, not checkbox mirrors;
- archive completed `DONE` headings and stale historical narrative into `<project>_archive.org`;
- do not create routine `** Meeting references` sections; only record meeting links when they support durable decisions or constraints.

If no such file is found at any level, skip this step. In the final report, state which hooks were found, which hooks ran, and which files they changed.

## Step 5: Commit And Push

`/update-log` is not complete while session work is only local. Many Epoch
automations run from GitHub, so local commits that are not pushed can leave the
live automation on stale code. Before the final report, every relevant repo must
be either clean and pushed, or explicitly reported as blocked.

### Step 5A: Publish work repos

Identify all git repos touched by the session before committing the bookkeeping
repo. Include at least:

- the project root repo from `git rev-parse --show-toplevel`;
- a project `repo/` subdirectory when it is its own git repo;
- any other nested repo where files were edited, committed, or referenced as the
  source of runtime automation changes.

For each relevant repo:

1. Run `git status --short --branch`.
2. If the repo has unstaged, staged, or untracked changes, inspect the diff and
   commit all intended session changes before moving on. Use separate logical
   commits when there are unrelated hunks. Do not commit secrets, credential
   files, generated junk, or unrelated pre-existing user edits; if such changes
   are present and cannot be safely separated, stop and ask rather than leaving
   the repo silently dirty.
3. If verification has not already been run for code/config changes in that
   repo, run the project-appropriate test or check before committing. If
   verification is impossible, record exactly why in the session log and final
   report.
4. Push every local commit in the repo. If the branch has no upstream, either
   push with the appropriate upstream when obvious (`origin` and the current
   branch), or stop and ask if the target is ambiguous.
5. Re-run `git status --short --branch` after the push. The repo must not show
   `ahead N`. If a push is rejected or CI-gated, resolve it or report the
   blocker; do not call bookkeeping complete while relevant commits remain
   unpushed.

### Step 5B: Commit bookkeeping

Stage and commit only the new log file when logs are intended to be shared,
updated CLAUDE.md, the `.gitignore` entry for local-only first-run logs, any
changes to `decisions/` or `decisions-summary.md`, and any files modified by
post-hooks with a descriptive message. If the first-run setup chose local-only
logs, do not stage or force-add the ignored log file; the log remains available
only in the local working copy. If any intended file was already dirty in the
Step 0 baseline, use hunk-level staging or an index patch so the commit contains
only the changes made by this `/update-log` run; do not stage the whole file
unless every hunk belongs to this run.

**Before running `git commit`, verify that every intended path was actually staged.** `git add` silently exits 0 for gitignored paths; if the project's notes are accidentally ignored at a parent repo, the log file you just created will not be committed and the orphaning will go undetected. Concretely:

1. Build a list of paths you intended to stage (the new log file unless
   local-only, CLAUDE.md, `.gitignore`, etc.) and a separate list of local-only
   paths that must remain unstaged.
2. After `git add`, compare the intended staged paths against
   `git diff --cached --name-only`. For any intended staged path that does not
   appear in the cached diff, run `git check-ignore -v <path>` to identify the
   matching ignore rule.
3. If any intended staged path is ignored, **stop**: report the path and the
   matching `.gitignore` rule to the user, and ask whether to un-ignore the path
   (preferred — likely a misconfigured parent repo, as in
   `backlinks-health-automation` 2026-05-04) or to force-add with `git add -f`
   (only if the user confirms the ignore is intentional and they want this
   single file through anyway). Do not silently proceed with a partial commit.
4. For each local-only log path, verify `git check-ignore -v <path>` identifies
   the intended log-directory ignore rule and verify the path is absent from
   `git diff --cached --name-only`.
5. Inspect `git diff --cached` and confirm the staged content contains only the intended bookkeeping changes before committing.

After committing the bookkeeping change, push the bookkeeping repo too and
verify it is not ahead of its upstream. If the bookkeeping repo is also one of
the work repos from Step 5A, this second push may be only the new bookkeeping
commit.

## Step 6: Report and exit (if requested)

Report the log file path, whether it was committed or kept local-only by
`.gitignore`, the `CLAUDE.md` pointer that was written, any decisions created or
skipped, hooks found and run, files changed by hooks, the commit hash, the
verification performed for staging and commit completeness, and a per-repo
publish receipt: repo path, commits created or already present, push target, and
final `git status --short --branch` showing no unpushed commits. If any repo
remains dirty or ahead, say that bookkeeping is blocked rather than complete.

If `--exit` was passed in the arguments, end the session using the host environment's normal exit mechanism after all steps are complete. If no explicit exit mechanism is available, state that bookkeeping is complete and stop.

$ARGUMENTS
