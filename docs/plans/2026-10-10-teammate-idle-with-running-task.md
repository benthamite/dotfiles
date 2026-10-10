# Keep a teammate working while its own background task runs

## Problem

In the 2026-10-10 Epoch/meetings session (f44df81c):

- At 15:27 BST, agent-team teammate `phase2c` started background shell
  `bfe8k1c3s` (a full verify run).  It then ended its turn to wait for the
  completion notice.
- The task finished at 15:34 with exit 0.
- Claude Code queued the notice in the main transcript but never delivered
  it, either to the lead or to phase2c.  phase2c stayed idle.
- The lead reported "Status: in progress ... final test run before it
  pushes" while nothing ran, and agent.el showed the session idle (green),
  which was correct.

The notice was released only at 16:42, after an outside message woke the
lead and the lead woke phase2c.

An earlier plan, `2026-10-10-undelivered-task-notices.md`, tried to detect
undelivered notices from the transcript's `queue-operation` records.  A
replay of 816 transcripts showed that those records cannot be paired
reliably, so detection was dropped.  This plan prevents the trigger instead.

## Facts relied on

- **Hook docs** (https://code.claude.com/docs/en/hooks, TeammateIdle):
  - The input carries the common fields plus `teammate_name` and, from
    v2.1.290, `agent_id`.
  - Exit code 2 sends stderr to the teammate, and the teammate keeps working.
  - No `background_tasks` field is documented for this event.
  - SubagentStop's `background_tasks` is scoped to the parent session and
    has no owner field.
- **Teammate transcripts** (observed, not documented):
  - Each teammate's transcript is
    `<session-dir>/subagents/agent-<agent_id>.jsonl`, with a `.meta.json`
    next to it holding `taskKind: "in_process_teammate"` and `name`.
- **Background launch records** (observed, not documented):
  - A background launch is a tool result that reads `Command running in
    background with ID: <id>. Output is being written to: <path>`.
  - A delivered completion is a user record containing
    `<task-notification>` with `<task-id><id></task-id>`.
- **Output files** (observed, not documented):
  - Shell task output files can end with `[exited with code N]`.  Of 235
    non-empty output files on disk, 145 had that line.  The other 90 have
    not been classified.  They may be agent outputs, still-running tasks or
    stopped tasks.

## Design

### Step 0: gate on a completion signal

Before writing the hook, establish a reliable, observable signal that a
shell task has finished:

- Classify the 90 output files that lack the marker: still running, stopped
  (TaskStop or session exit), agent output, or finished without the marker.
- If every finished shell task writes `[exited with code N]`, use the
  marker as the signal.
- If not, find another signal (for example, the task's process is gone), or
  stop and report.  No hook ships on an unreliable signal.

### Step 1: TeammateIdle hook

`claude/hooks/hold-idle-teammate-with-running-task.py`, registered under
TeammateIdle in `~/.claude/settings.json`.

1. Read the payload.  Resolve the teammate's transcript from
   `transcript_path`'s session directory and `agent_id`.  If `agent_id` is
   missing, or the file is absent, allow the idle and report on stderr
   without exit 2.
2. Collect the shell tasks this teammate launched in the background, from
   its own transcript's launch records, with their output paths.
3. A task is still open when its output file lacks the completion signal and
   the teammate has neither received its notice nor stopped it with
   TaskStop.  Use only tasks this teammate launched.
4. If any task is open, exit 2 with a message naming each one (id,
   description, output path).  The message tells the teammate to wait for
   it in the foreground with a bounded Bash wait.  The wait ends on the
   completion signal in the output file, for example a loop that checks
   for the marker with a 9-minute timeout.  Once the task has finished, the
   teammate reads the output and carries on.
5. If the teammate's own task already finished but its notice never arrived,
   which is today's case, exit 2 as well.  The message says the task
   finished and gives its output path and exit line.
6. Otherwise, allow the idle.

There is no block counter.  Each block corresponds to a real open task, and
the wait ends only when the task ends.  A task that never ends keeps the
teammate busy.  That is visible (amber), unlike today's silent idle, and the
existing `Status: done` hook still covers the lead.

### Step 2: tests

Fixture transcripts and output files, run through the real script:

- an open task, which blocks;
- a finished task with an undelivered notice, which blocks with "finished";
- a delivered notice, which allows;
- a task stopped with TaskStop, which allows;
- another agent's task, which allows;
- a missing `agent_id` or transcript, which allows and reports;
- malformed JSON, which allows.

### Step 3: live test

Run a throwaway session with an agent team:

- A teammate starts `sleep 90` in the background and tries to go idle.
- Expected: the idle is refused, the teammate waits in the foreground,
  sees the task finish and reports.
- Then confirm in agent.el that the session goes idle with no background
  tasks listed.
- The undelivered-notice case cannot be produced on demand.  Step 5 is
  covered by a fixture only, and that gap will be stated.

### Docs

- `claude/hooks/README.org` index.
- `claude/README.org`.
- `ai-config-sync.json`: Claude-only, because Codex has no agent teams.
- `codex/README.org` note.

## Risks

- Undocumented record shapes (launch text, notice format, output marker,
  transcript path) may change.  Unknown shapes allow the idle and are
  reported.  They never block.
- A teammate that started a task meant to outlive it, such as a server, is
  kept busy.  Agents should then stop the task or not start it from a
  teammate.  The block message says this.
- The lead and plain subagents are not covered.  The defect has been seen
  only for a teammate.
