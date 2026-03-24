---
name: update-log
description: End-of-session bookkeeping. Use at the end of a work session to update the session log and project status. Only consider using proactively (e.g. when the session is running low on context) in projects that already have a log directory and a log reference in CLAUDE.md from a previous user invocation. Never invoke autonomously in projects without existing logs.
user-invocable: true
allowed-tools: Read, Write, Edit, Bash, Glob, Grep, AskUserQuestion
argument-hint: "[optional summary of what was done]"
---

# End-of-session log update

Perform the following bookkeeping steps to preserve this session's work for future sessions.

## Step 0: Detect project setup

Determine the project's log directory and whether decisions are tracked:

1. **Read `CLAUDE.md`** in the project root. Look for an `@` reference to a log file — a path matching a pattern like `@<dir>/YYYY-MM-DD.md` (e.g. `@logs/2026-01-15.md`, `@results/logs/2026-01-15.md`). Extract the directory portion — that is the log directory.

2. **Check for a decisions directory**: look for a `decisions/` directory in the project root and a `decisions-summary.md` file.

3. **If no log reference was found in CLAUDE.md** (or CLAUDE.md doesn't exist), this is a first-run setup. Proceed to the **First-run setup** section below. Otherwise, skip to **Step 1**.

### First-run setup

This project doesn't have session logging set up. Ask the user the following:

1. **Log directory**: "This project doesn't have session logging set up yet. Where should session logs be stored? (default: `logs/`)"
   - Create the chosen directory if it doesn't exist.

2. **Decision records**: "Would you also like to track architectural decisions in a `decisions/` directory? This records design trade-offs so future sessions don't re-propose rejected alternatives. (y/n, default: n)"
   - If yes:
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

Update the `@<log_dir>/...` reference under "Latest session" to point to the new log file.

Do NOT duplicate the log contents into CLAUDE.md. The `@` reference ensures Claude reads the file automatically at the start of the next session.

## Step 3: Record decisions

If a `decisions/` directory exists in the project root, run `/record-decisions` to check if any architectural or algorithmic decisions were made this session. If new entries are added, they will be included in the commit.

If no `decisions/` directory exists, skip this step.

## Step 4: Commit

Stage and commit the new log file, updated CLAUDE.md, and any changes to `decisions/` or `decisions-summary.md` with a descriptive message.

$ARGUMENTS
