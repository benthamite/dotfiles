---
name: update-log
description: End-of-session bookkeeping. Use at the end of a work session to update the session log and project status. Only consider using proactively (e.g. when the session is running low on context) in projects that already have a log directory and a log reference in CLAUDE.md from a previous user invocation. Never invoke autonomously in projects without existing logs.
user-invocable: true
allowed-tools: Read, Write, Edit, Bash, Glob, Grep, AskUserQuestion
argument-hint: "[--exit] [optional summary of what was done]"
---

# End-of-session log update

Perform the following bookkeeping steps to preserve this session's work for future sessions.

## Step 0: Detect project setup

Determine the project's log directory and whether decisions are tracked:

1. **Read `CLAUDE.md`** in the project root. Look for a reference to a session log file — either:
   - A path like `<dir>/YYYY-MM-DD.md` in a "Latest session" section (current format), or
   - A legacy `@<dir>/YYYY-MM-DD.md` import (old format — will be migrated in Step 2).
   
   Extract the directory portion — that is the log directory.

2. **Check for a decisions directory**: look for a `decisions/` directory in the project root and a `decisions-summary.md` file.

3. **If no log reference was found in CLAUDE.md** (or CLAUDE.md doesn't exist), this is a first-run setup. Proceed to the **First-run setup** section below. Otherwise, skip to **Step 1**.

### First-run setup

This project doesn't have session logging set up. Use the `AskUserQuestion` tool to gather setup preferences before proceeding:

1. **Ask both questions in a single `AskUserQuestion` call**:
   - Question 1 (header: "Log dir"): "Where should session logs be stored?" with options like `logs/` (recommended), `docs/logs/`, and the user can type a custom path via "Other".
   - Question 2 (header: "Decisions"): "Track architectural decisions in a `decisions/` directory?" with options Yes and No (no recommendation — both are valid).

2. **After receiving answers**, create the chosen log directory if it doesn't exist.

3. If decisions were opted in:
     - Create `decisions/` directory.
     - Create `decisions-summary.md` with the initial table header:
       ```
       # Decision records (summary)

       Full details with rejected alternatives and evidence are in `decisions/`. Read the relevant file before proposing changes to a covered subsystem.

       | #   | Topic | Decision | Status |
       |-----|-------|----------|--------|
       ```
     - Add a `@decisions-summary.md` reference to CLAUDE.md so decisions are auto-loaded into context.

3. **Update CLAUDE.md**:
   - If CLAUDE.md doesn't exist, create it with a minimal structure containing the project name (from the directory name or `package.json`/`pyproject.toml` if available), a "Latest session" section, and (if decisions were opted in) a "Decision records" section with the `@decisions-summary.md` reference.
   - If CLAUDE.md exists but has no "Latest session" section, append one.
   - The "Latest session" section will be populated in Step 2 with a summary + pointer (not an `@` import).

Then proceed to Step 1.

## Step 1: Create a session log file

Create a new file at `<log_dir>/YYYY-MM-DD.md` (using today's date). If a file for today already exists, append to it with a horizontal rule separator.

The file should contain:

- **Title**: `# YYYY-MM-DD: <brief title>`
- **What was done**: Summary of experiments, code changes, and analysis performed.
- **Key findings**: Any new discoveries, bugs found/fixed, or validated/invalidated hypotheses.
- **Results**: Performance numbers, sweep outcomes, or other quantitative results.
- **Open questions**: What was left unfinished or what should be explored next.

Be concise but specific. Include exact numbers where available (counts, percentages, timings). Future sessions may need to understand *why* decisions were made, so document reasoning for non-obvious choices.

## Step 2: Update CLAUDE.md

Update the "Latest session" section in CLAUDE.md with:

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

## Step 3: Record decisions

If a `decisions/` directory exists in the project root, run `/record-decisions` to check if any architectural or algorithmic decisions were made this session. If new entries are added, they will be included in the commit.

If no `decisions/` directory exists, skip this step.

## Step 4: Run post-update-log hooks

After writing the log and updating CLAUDE.md, check whether any ancestor directory (walking up from the project root toward the git root) contains a CLAUDE.md with a `## Post-`/update-log` hook` section. If found, read and follow the instructions in that section.

This allows parent directories to define project-family-level bookkeeping — such as updating a master project list or syncing a shared status document — that fires automatically after every `/update-log` invocation.

If no ancestor CLAUDE.md contains this section, skip this step.

## Step 5: Commit

Stage and commit the new log file, updated CLAUDE.md, any changes to `decisions/` or `decisions-summary.md`, and any files modified by post-hooks with a descriptive message.

## Step 6: Exit (if requested)

If `--exit` was passed in the arguments, type `/exit` to end the session after all steps are complete.

$ARGUMENTS
