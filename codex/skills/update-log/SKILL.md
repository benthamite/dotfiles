---
name: update-log
description: End-of-session bookkeeping. Do NOT invoke this on your own initiative, and do NOT hand-write a session log yourself instead of invoking it — writing or editing any file under a project's logs/ directory is this skill's job alone. Run it only when the user explicitly asks (the user types /update-log or says update log, session log, close out, wrap up, or asks to save project progress) or when the deliberate end-of-session chain invokes it with --auto. Never trigger it proactively mid-session or at session end on your own judgment, even when the session clearly changed durable project state — wait for an explicit request or the --auto chain.
user-invocable: true
argument-hint: "[--exit] [--auto] [--receipt-file PATH] [optional summary of what was done]"
---

# End-of-session log update

Perform the following bookkeeping steps to preserve this session's work for future sessions.

## Receipt contract

When `$ARGUMENTS` contains `--receipt-file PATH`, that file is the closeout
gate. Write it exactly once as the final bookkeeping action with:

`python3 "/Users/pablostafforini/My Drive/dotfiles/codex/skills/update-log/scripts/write_receipt.py"`

Use status `success` only after all required edits, deterministic checks,
commits, and final inspections succeed. Use `no-op` only when the triage below
finds no durable change. On any incomplete or failed path, write `failure`
before the final response. For an Epoch project, pass the successful harness
receipt files with repeated `--evidence`; the writer rejects failed, malformed,
or `ok: false` evidence. Do not hand-write or pre-create the receipt, and do not
report success before the writer exits zero.

> **The prohibition is on the act, not just on the skill.** Do not write a session
> log by hand as a substitute for invoking this skill. Creating or editing any file
> under a project's `logs/` directory is this skill's job, and only when invoked as
> below. Bypassing the skill and writing the file directly is the same violation,
> not a way around it — that is exactly how the rule gets defeated in practice,
> because the agent never consults a policy attached to a skill it did not consider
> using. If the same reasoning would stop you invoking `update-log`, it stops you
> hand-writing the log.
>
> **Invocation policy:** Do not invoke this skill on your own initiative. Run it only when (a) the user explicitly asks — `/update-log`, or asking to log / close out / wrap up / save progress — or (b) the deliberate end-of-session chain invokes it with `--auto` (see *Non-interactive runs* below). Both of those are intentional; what is prohibited is the agent proactively deciding to run it mid-session or at session end on its own judgment, even when the session clearly changed durable state. If you think a log would help but neither (a) nor (b) applies, you may briefly suggest it, then stop — do not run it until asked.

## Triage first (may be a no-op)

Before touching any file, decide whether this session changed durable project state, using the criteria in **What counts as durable project state** below. This decision is part of the skill: even once invoked (explicitly or via the `--auto` chain), **doing nothing is a valid, friction-free outcome** if the session changed nothing durable.

- **If nothing durable changed** (purely conversational Q&A, a quick read-only lookup, or trivial edits with nothing a future agent would need to recover), write no log, change no project files, and make no commit. If a receipt path was supplied, write a `no-op` receipt as specified above. Report `No durable changes — skipping update-log.` and stop.
- **Otherwise**, proceed to Step 0.

Exception: if the user explicitly typed `/update-log` or asked for specific bookkeeping, honor it — proceed even if the session was marginal, and do what was asked.

### Non-interactive runs (`--auto`)

`--auto` marks a run from the end-of-session chain rather than a deliberate user invocation. In this mode, never block on input:

- If the project has no existing log conventions, perform **First-run setup**
  with deterministic defaults instead of asking. For an Epoch notes project,
  use the tracked `logs/` directory and tracked `CLAUDE.md` / `AGENTS.md` map.
  For any other project, use `logs/` and keep session bookkeeping local-only by
  adding the log directory, `CLAUDE.md`, and `AGENTS.md` to `.gitignore`. Do not
  create `decisions/` or `decisions-summary.md`; create or update `CLAUDE.md`,
  then continue to write the session log.
- Make no other interactive prompts. If a step would need a decision only the user can make, record it in the log and final report and continue with the safe default or skip that step, rather than asking.

## What counts as durable project state

Once invoked (explicitly or via the `--auto` chain), use these criteria for the triage decision above (write a log vs. no-op). Durable project state includes:

- code, docs, configuration, workflow, or data changes;
- new findings, debugging results, validation results, or service-state changes that future sessions should know;
- decisions, active TODOs, blockers, next steps, or status changes;
- external-service work whose outcome matters later, such as Slack/Google/GitHub/Asana changes;
- any session whose results should be visible in the project note, current project list, or next meeting prep.

Do not run it after purely conversational Q&A, quick read-only lookups, or sessions where nothing durable changed. If uncertain, prefer running it when the project already has logs and the session produced information a future agent would otherwise need to recover from the transcript.

The deliberate end-of-session chain invokes this skill with `--auto` (see *Non-interactive runs* above); that path is intentional and should run normally. What is prohibited is the *agent* invoking the skill on its own initiative — proactively mid-session, or at session end by its own judgment — outside that chain or an explicit user request. A non-`--auto` reminder may surface that durable state looks unsaved and suggest `/update-log`, but the agent must not silently perform the workflow itself.

If the user explicitly invokes `/update-log` or otherwise asks for end-of-session
bookkeeping after a log, pointer, or opportunistic summary was already written,
still run the complete checklist below. An existing log or `CLAUDE.md` pointer is
only a starting state: continue through decision-record review, post-update-log
hooks, staging verification, commit completeness, and publish checks. Do not
stop at "the log already exists" unless every later step has also been checked.

## Step 0: Detect project setup

Determine the project's log directory and whether decisions are tracked:

0. **Resolve an Epoch notes project before using cwd as the project root.** Run:

   ```bash
   python3 "/Users/pablostafforini/My Drive/Epoch/projects/shared/scripts/project_harness.py" resolve --cwd "$PWD"
   ```

   If it succeeds, use the returned notes `directory` as the bookkeeping
   project root even when the session started in `~/repos/epoch/...` or an
   approved linked worktree. Record the returned slug, primary brief, and all
   declared local repositories for later checks. If it reports no match, use
   the generic project-root discovery below. If it reports ambiguity or an
   invalid project model, stop and write a failure receipt; do not fall back to
   creating logs in the code checkout.

1. **Snapshot the working tree before editing** with `git status --short`. If `CLAUDE.md`, `decisions-summary.md`, existing `decisions/` files, or any hook-managed status files are already dirty, inspect their current diff before changing them. For an Epoch project, snapshot both the Epoch notes repo and every returned local repository. Use this baseline in Step 5 so pre-existing user or concurrent-agent hunks do not get staged with this bookkeeping commit.

2. **Read `CLAUDE.md`** in the project root. For this workflow, `CLAUDE.md` is the canonical session-log index unless the project explicitly says otherwise; in Codex sessions, `AGENTS.md` may contain agent instructions but is not the session-log pointer. Look for a reference to a session log file — either:
   - A path like `<dir>/YYYY-MM-DD.md` in a "Latest session" section (current format), or
   - A legacy `@<dir>/YYYY-MM-DD.md` import (old format — will be migrated in Step 4).
   
   Extract the directory portion — that is the log directory.

3. **Check for a decisions directory**: look for a `decisions/` directory in the project root and a `decisions-summary.md` file.

4. **If no log reference was found in CLAUDE.md** (or CLAUDE.md doesn't exist), this is a first-run setup. Proceed to the **First-run setup** section below. Otherwise, skip to **Step 1**.

### First-run setup

This project doesn't have session logging set up. In an interactive generic
project run, gather setup preferences before proceeding. In a generic `--auto`
run, do not ask: use `logs/` as a local-only directory in the repo working tree,
add the log directory, `CLAUDE.md`, and `AGENTS.md` to `.gitignore`, leave
decision tracking disabled, create or update `CLAUDE.md`, and continue.

For a project resolved through the Epoch harness, do not ask these setup
questions and do not add notes files to `.gitignore`. Use tracked `logs/`,
tracked maps, and the primary Org brief under the returned notes directory.

1. If not running with `--auto`, ask all first-run questions together. If a structured user-input tool such as `AskUserQuestion` is available, use it; otherwise ask a concise plain-text question:
   - "Keep a local session-log folder inside this repo's working tree?" Recommend Yes for public repos or any repo where session logs might contain private context; No is valid when the user wants shared, committed logs or a path outside the repo.
   - "Where should session logs be stored?" If the user wants local logs in the repo, suggest `logs/` as the default and `docs/logs/` as an alternative, while allowing a custom path. If not, ask for the intended shared or external path.
   - "Track architectural decisions in a `decisions/` directory?" Both Yes and No are valid.
   If running with `--auto`, set the answers internally to "Yes, keep local
   logs in the repo", `logs/`, and "No decisions."

2. Create the chosen log directory if it doesn't exist.

3. If the user chose to keep a local session-log folder inside the repo, ensure
   the chosen log directory and agent pointer files are ignored before writing
   the first log:
     - Add a root-relative directory pattern for the log directory to
       `.gitignore` if no existing ignore rule already covers it, e.g. `/logs/`
       for `logs/` or `/docs/logs/` for `docs/logs/`.
     - Add root-relative file patterns for `/CLAUDE.md` and `/AGENTS.md` if no
       existing ignore rule already covers them.
     - Do not remove existing `.gitignore` entries or reorder unrelated rules.
     - Verify the dated log path, `CLAUDE.md`, and `AGENTS.md` will be ignored
       with `git check-ignore -v` after they exist.

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
   - The "Latest session" section will be populated in Step 4 with a summary + pointer (not an `@` import).

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

## Step 2: Record decisions

If a `decisions/` directory exists in the project root, use the `record-decisions` skill if available to check whether any architectural or algorithmic decisions were made this session. If the skill is unavailable, follow the local workflow directly: inspect `decisions/`, identify qualifying decisions, add `decisions/NNN.md` entries, and update `decisions-summary.md`. If new entries are added, they will be included in the commit.

If no `decisions/` directory exists, skip this step.

## Step 3: Run post-update-log hooks

Walk up ancestor directories from the project root to the git root (inclusive). For each ancestor, check whether `<ancestor>/context/post-update-log-hook.md` exists. If found, read and follow its instructions. Run all matching hooks, innermost first.

This lets parent directories define project-family-level bookkeeping — master project list updates, shared status syncing, meeting action item reconciliation, etc. — that fires automatically after every `/update-log` invocation, without bloating per-session CLAUDE.md context.

For Epoch project notes, the main project `.org` file is a concise ground-truth brief, not a chronological session dossier; its target shape is defined in `projects/context/project-doc-conventions.md`. When a hook or local convention asks you to update it:

- refresh current-state fields and sections from the session log just written;
- keep active work as org `TODO` headings, not checkbox mirrors;
- archive completed `DONE` headings and stale historical narrative into `<project>_archive.org`;
- do not create routine `** Meeting references` sections; only record meeting links when they support durable decisions or constraints.

If no such file is found at any level, skip this step. In the final report, state which hooks were found, which hooks ran, and which files they changed.

## Step 4: Update the CLAUDE.md session pointer

This step runs after the hooks on purpose. The project brief is the source the
map summarises, so it has to be current before the map is written; doing it the
other way round produces a digest of the state the project was in before this
session. Until 2026-08-13 this step sat *before* the hooks while instructing the
reader to work from a brief the hooks had not refreshed yet — an instruction
nobody following the document in order could satisfy.

How CLAUDE.md is maintained depends on its shape — the file itself tells you which mode to use:

### Map mode

CLAUDE.md has a `## Current focus` section (and no `## Latest session`). The file is a stable map and the session narrative belongs in the project's brief (e.g. the `.org`), not here. Step 3 has already refreshed that brief. In this mode:

For a project resolved through the Epoch harness, do not hand-compose Current
focus. Finish the semantic task and state edits, keep the two map files
mirrored, and let the maintenance transaction below derive Current focus and
`NEXT_STEP` from canonical Active TODOs. The detailed rules below remain the
contract that the generator and its output must satisfy. For generic projects,
apply them directly.

1. **Regenerate `## Current focus` from the brief's open work, replacing the previous content — never append to it.** For `.org` briefs the open work is the `** Active TODOs` headings. Output a short digest: a one-line orientation, optionally followed by up to ~6 bullets of the live open priorities. Hard cap ~120 words. **No dates and no session narrative** (e.g. "On 2026-06-29 did X") — those live in `logs/`; durable state lives in the brief.
   - **Why replace, not append:** appending is what turns `Current focus` into a chronological blob that duplicates the log and the brief. Replacing loses nothing — the session log you just wrote holds the narrative, and the brief's `** Active TODOs` hold the live state. `Current focus` is only a convenience index into those.
   - If the existing `Current focus` is already a multi-paragraph blob, this run is the moment to compact it down to the digest; do not preserve the old chronology.
   - **Choose what to list deterministically**, not by impression, so two runs over the same brief produce the same digest:
     - take open tasks in priority order, highest first;
     - skip tasks whose keyword marks them as not actionable (`WAITING`, `SOMEDAY`, `MAYBE`, `LATER`, `DELEGATED`). If *every* open task is one of those, say so in the orientation line and name what the work is waiting on, rather than listing a blocked task as though it were available;
     - carry each task's own heading text, or a link to it. Do not paraphrase a task into a fresh sentence — a paraphrase is a second copy of the task that can disagree with the original;
     - if there are no open tasks, write one line saying so and naming what closed the last of them. Do not fill the section with recent history.
   - **Do not originate a claim here.** A statement that work is outstanding, pending, blocked, or awaiting somebody is only worth as much as the last time somebody checked, and this section records no evidence and no date. Such a claim belongs on the task that carries the evidence for it — in Epoch briefs, a `TODO` with `VERIFY_WITH` and `LAST_VERIFIED`. `Current focus` may repeat a claim that a task already carries; it must not be the first place the claim appears. If the session established something outstanding that no task records, add or update the task first, then let the digest reflect it.
2. Keep the `## Read first` pointers accurate if any referenced file moved or was added.
3. Do **not** add a `## Latest session` narrative. If the brief's `STATUS_DETAIL` / `NEXT_STEP` dashboard abstracts have themselves blobbed past their word caps (≤120 / ≤40), compact them the same way while updating the brief in Step 3 — they are derived summaries of `** Current state` / `** Active TODOs`, not changelogs.

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

This mode is a record of what happened in one session, so it may say what was
found or left unfinished. It still must not assert that some outside party has
not yet acted unless the session actually checked — write "asked Matt on
2026-08-06, no reply as of that check" rather than "waiting on Matt", which
reads as current however old it gets.

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

## Step 4.5: Preview the isolated Epoch closeout

Skip this step for a project that was not resolved through the Epoch harness.
For an Epoch project, build the exact list of session-owned files. Pass files
under `projects/<slug>/` with `--path`; include the brief, the log, both map
files, and any other changed project file. Pass a project-related meeting or
text/config reference outside that directory with `--related-path`. Do not
include a file that contains a pre-existing or concurrent hunk. Run this preview
from the Epoch root after semantic Org edits and map mirroring are complete.
First run `mktemp -d /tmp/epoch-closeout.XXXXXX`, record the printed absolute
path as `RECEIPT_DIR`, and substitute it literally below. Do not depend on a
shell variable persisting between tool calls:

```bash
python3 projects/shared/scripts/project_harness.py \
  --receipt-file RECEIPT_DIR/preview.json \
  closeout --project <slug> \
  --path projects/<slug>/<first-owned-file> \
  --path projects/<slug>/<next-owned-file> \
  --related-path <optional-owned-meeting-or-reference>
```

Continue only when the receipt has status `needs-apply` or `no-op` and does not
report `ok: false`. The preview starts from exact `HEAD`, overlays only the
listed files, archives closed tasks, derives map/brief summaries, validates the
full canonical model and instruction mirrors, and renders both generated views
without reading unrelated dirty project state. Never edit generated views by
hand or list them as inputs.

## Step 5: Commit and publish only when authorized

Commit each logical local change. A push changes a shared system: perform it
only when the user explicitly asked to publish/push in this session or the
invoking workflow carries that authority. `--auto` does not authorize a push.
An authorized publish is incomplete while a relevant repo remains ahead. A
local-only closeout may succeed with intentional ahead commits, but the receipt
and final report must say which commits were not published.

### Step 5A: Publish work repos

Identify all git repos touched by the session before committing the bookkeeping
repo. Include at least:

- the project root repo from `git rev-parse --show-toplevel`;
- every local repository returned by the Epoch harness resolver;
- any other repository where files were edited, committed, or referenced as the
  source of runtime changes.

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
4. If publishing is authorized, push every intended local commit. If the target
   is ambiguous, do not guess. In `--auto`, record the ambiguity and leave the
   commit local. In an interactive run, ask only when no safe target follows
   from the checked upstream.
5. Re-run `git status --short --branch`. For an authorized publish, the repo
   must not show `ahead N`; resolve a rejection or report failure. For a
   local-only closeout, record the exact ahead count and commit hashes.

### Step 5B: Commit bookkeeping

For an Epoch project, do not stage or run `git commit` by hand. After Step 5A
has committed every intended code change, run the same isolated closeout with
`--apply`, a descriptive notes commit message, and a new durable receipt path:

```bash
python3 projects/shared/scripts/project_harness.py \
  --receipt-file RECEIPT_DIR/apply.json \
  closeout --project <slug> \
  --path projects/<slug>/<first-owned-file> \
  --path projects/<slug>/<next-owned-file> \
  --related-path <optional-owned-meeting-or-reference> \
  --message "Update <slug> project state" --apply
```

The command must return `success` or `no-op`. It commits only the explicit
project files and derived views from its validated `HEAD` candidate, preserves
unrelated index and worktree state, and keeps a recovery journal until the
receipt is durable. Verify the returned commit and its exact path set. If a
session-owned file also contains another actor's changes, stop instead of
passing the whole file to closeout. Omit `--related-path` when it does not
apply.

For a non-Epoch project, use the generic staging procedure below.

Stage and commit only the new log file when logs are intended to be shared,
updated CLAUDE.md, updated AGENTS.md when it is a sibling mirror, the
`.gitignore` entries for local-only first-run bookkeeping, any changes to
`decisions/` or `decisions-summary.md`, and any files modified by post-hooks
with a descriptive message. If the first-run setup chose a local log folder in
the repo, do not stage or force-add the ignored log file, `CLAUDE.md`, or
`AGENTS.md`; they remain available only in the local working copy. If any
intended file was already dirty in the Step 0 baseline, use hunk-level staging
or an index patch so the commit contains only the changes made by this
`/update-log` run; do not stage the whole file unless every hunk belongs to this
run.

**Before running `git commit`, verify that every intended path was actually staged.** `git add` silently exits 0 for gitignored paths; if the project's notes are accidentally ignored at a parent repo, the log file you just created will not be committed and the orphaning will go undetected. Concretely:

1. Build a list of paths you intended to stage (the new log file, `CLAUDE.md`,
   and `AGENTS.md` unless local-only, `.gitignore`, etc.) and a separate list of
   local-only paths that must remain unstaged.
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
4. For each local-only path, including the log path, `CLAUDE.md`, and
   `AGENTS.md`, verify `git check-ignore -v <path>` identifies the intended
   `.gitignore` rule and verify the path is absent from
   `git diff --cached --name-only`.
5. Inspect `git diff --cached` and confirm the staged content contains only the intended bookkeeping changes before committing.

After committing the bookkeeping change, publish the bookkeeping repo only
under the same explicit authority. Verify the resulting ahead state and record
it in the receipt/report.

## Step 6: Report and exit (if requested)

Before reporting, write the requested receipt as the final bookkeeping action.
For a successful Epoch closeout, pass
`RECEIPT_DIR/apply.json` from Step 5B as evidence, with the recorded absolute
path substituted.
The message must name the notes commit and say whether publishing was authorized
and completed. For a generic successful closeout, write `success` without Epoch
evidence. For any incomplete required step, write `failure`. Remove only the
temporary evidence directory created by this run after the final receipt
exists. For an Epoch closeout, run `trash RECEIPT_DIR` after the
final receipt writer succeeds. Also remove this run's evidence directory on
every failure path before reporting.

Report the log file path, whether the log folder and agent pointer files were
committed or kept local-only by `.gitignore`, the `CLAUDE.md` pointer that was
written, any decisions created or skipped, hooks found and run, files changed by
hooks, the commit hash, the verification performed for staging and commit
completeness, and a per-repo publish record: repo path, commits created or
already present, push target when authorized, and final
`git status --short --branch`. Treat an unexpected dirty tree as failure. Treat
an ahead branch as failure only when publishing was authorized; otherwise
identify it as an intentional local-only result.

If `--exit` was passed in the arguments, end the session using the host environment's normal exit mechanism after all steps are complete. If no explicit exit mechanism is available, state that bookkeeping is complete and stop.

$ARGUMENTS
