---
name: refresh-cr-tracker
description: Rebuild and republish the CR-task master tracker artifact. Use when the user says "refresh cr tracker", "update the CR task tracker", "/refresh-cr-tracker", or wants the conceptual-reasoning task master list re-pulled from live reasoning-tasks data + GitHub issues and redeployed to its claude.ai artifact.
---

# Refresh the CR-task master tracker

Rebuilds `~/Trajectory/cr-scout-runs/cr-task-master-list.html` from live sources and
redeploys it to the existing claude.ai artifact (so newly-registered GitHub issues, new
cohort tasks, and updated scores show up).

**Must run in an INTERACTIVE session** — the Artifact tool is not available in headless
`claude -p` or cloud routines. Requires the local `~/Trajectory/reasoning-tasks/main` worktree and
a working `gh`/GitHub token (both present on Pablo's Mac).

## Steps

1. **Build.** Run:
   ```
   python3 ~/Trajectory/cr-scout-runs/cr-tracker/build.py
   ```
   It re-derives every row from the reasoning-tasks branches (`ryan/cr-collect-cohort` results +
   crawler pools, `ryan/collect-lw-gdocs`, `pablo/cr-scout-batch-1`), fuzzy-matches gdoc
   tasks to source articles, re-queries GitHub issues via the REST API, verifies each task
   dir exists before linking, dedupes, regenerates the HTML, and prints a one-line JSON
   summary (`rows / links / words / issues / task_links / html_sha`). Takes ~1–2 min.
   - **If it exits non-zero** (e.g. it can't read GitHub issues — it deliberately refuses to
     publish a tracker with zero issue links): report the error and **STOP. Do not publish.**

2. **Redeploy** (only on a successful build). Use the **Artifact** tool with:
   - `file_path`: `/Users/pablostafforini/Trajectory/cr-scout-runs/cr-task-master-list.html`
   - `url`: `https://claude.ai/code/artifact/15852b96-f4b3-4838-b35f-e8b3fd941012`
   - `favicon`: 🎯
   - `label`: `manual-refresh`

3. **Report** the JSON summary (rows / article-links / word-counts / issues) and confirm the
   artifact URL was redeployed.

## Notes
- Pipeline internals: `~/Trajectory/cr-scout-runs/cr-tracker/README.md`.
- Expected, non-error gaps: ~21 gdoc-sourced rows have no article link/word count (their
  sources aren't in the crawler's indexed pools), and batch ranks 41–50 are ungraded.
- A `git push` of the regenerated HTML is **not** required — the artifact is the deliverable;
  pushing the repo is optional bookkeeping and stays gated on Pablo's explicit OK.
