# Stop agents from reporting "done" over running background jobs

## Problem

Emacs agent.el regularly shows a Claude Code session as busy with background
work after the agent has reported the task finished.  This has recurred for
weeks.  Earlier fixes in agent.el (f6a48b3, 94ddbc8, dc0d128, b13f9cf,
b36a2fc) each corrected a real misclassification: transcript guessing,
subagent activity, idle teammates.  The 2026-10-10 incidents are different:
the jobs are really running.

## Evidence

Live incidents, 2026-10-10 about 15:00 BST:

- dotfiles session 30baff9b: background shell `bam9tx09h`, started about
  12:08, still running about three hours later.  Its test failed at once
  (`/usr/bin/taskpolicy` does not exist).  `$S/stacks/*.txt` then matched
  nothing, so `rg -c PATTERN $f` ran with no path and blocked on stdin, a
  socket Claude Code keeps open.  rg had used 0 s of CPU.  The 14:32 Stop
  payload listed the shell as running, and the agent's final message ended
  `Status: done`.
- Epoch/meetings session f44df81c: 11 shell tasks still running, some for
  over 16 hours.  Several are `until ! pgrep -f "gh pr checks 15"; do sleep
  30; done`.  The only processes matching that pattern are the loops' own
  shells: there is no `gh` process.  macOS `pgrep` excludes itself and its
  ancestors.  The loops deadlock because the agent re-armed the same loop,
  and each copy matches the others.

History, every Claude transcript (main and subagent) since 2026-09-12.
Scripts are in this session's scratchpad.

- 3,962 background tasks: 1,937 `run_in_background`, 1,765 auto-moved to the
  background after the Bash timeout, 252 Monitor.
- 535 never reported completion, across 73 of the 175 sessions that used
  background tasks.  The agent stopped 347 of them with TaskStop.  188 were
  abandoned.
- The abandoned ones split into several unrelated mechanisms:
  - a string-in-file wait loop whose marker never appeared;
  - sibling pgrep deadlocks;
  - an intentionally long job that outlived its subagent;
  - a `tail -f` Monitor stream (Monitors time out without leaving a
    transcript marker, so these overcount);
  - a stdin reader;
  - file-exists loops.
- Hand-written until/while loops fail to report completion at 14%.  Other
  tasks fail at 13%.  Loops are not the dominant cause.
- Caveat: UI stops, Monitor timeouts and subagent teardown leave no
  transcript marker, so "never reported" overcounts hangs.

Claude Code facts (code.claude.com docs: tools-reference, hooks, env-vars):

- Interactive sessions put no time limit on background Bash.
- Monitor watches have a deadline: 5 min by default, 30 min at most.
- No hook fires when a background task starts or ends.
- Stop and SubagentStop payloads include `background_tasks` (`id`, `type`,
  `status`, `description`, `command`, ...), with no start time.
- The Stop payload also carries `last_assistant_message`.  The dotfiles
  payload at 14:32 had it.

## Diagnosis

No single way of waiting causes this.  A ban on one loop shape, or a
deadline, would miss most cases or kill live work.  What all the cases share
is that nothing makes the agent reconcile its running background tasks
before it declares the work finished.  Claude Code wakes the agent when a
task exits, but a task that never exits produces no signal.  The agent ends
its turn, sometimes with `Status: done`, and the leftover task stays running
until the session exits.

agent.el reports Claude Code's task list faithfully.  The user sees a
contradiction between the agent's "done" and agent.el's "busy", and reads it
as an agent.el bug.

## Proposed fix

### 1. Stop hook: no `Status: done` over running background work

Add a Claude Stop hook in `claude/hooks/` and register it in
`~/.claude/settings.json` next to the existing Stop hooks.  It does this:

1. Read the payload.  Take the last `Status:` line of
   `last_assistant_message`.
2. If that line is exactly `Status: done` (not `done, pending
   verification`), collect `background_tasks` with `status == "running"` and
   `type != "teammate"`.  Teammates are excluded because Claude Code reports
   an idle teammate as running.
3. If any such tasks exist, block the stop (`decision: block`).  The reason
   lists each task's id, type and description.  It tells the agent to stop
   the tasks it no longer needs with TaskStop, or else to change the status
   line to `in progress` or `pending verification`, naming the task it is
   waiting on.
4. Allow the stop in every other case.  That covers no status line, a
   non-done status, no running tasks, a missing `background_tasks` field and
   a malformed payload.  A missing field is reported on stderr instead of
   being read as "no tasks", following the agent.el principle that an
   undocumented field must not fail silently.
5. Loop guard.  Do not use `stop_hook_active`: it is set by a continuation
   from any Stop hook, including the Elisp gate.  Keep a hook-specific block
   count keyed by session id and `prompt_id`.  It resets with each new user
   prompt.  After two blocks in one turn, allow the stop but return a
   `systemMessage` naming the still-running tasks, so the user sees the
   contradiction instead of it passing silently.  This is a bounded
   intervention, not a guarantee.  Task cleanup can fail, and a new task
   can start after a block.

Why this boundary:

- It enforces the user's existing written rule ("Don't write 'done' while
  anything is open").
- It needs no timers and no classification of commands, and it covers every
  cause in the history.
- It applies at the moment the contradiction is created.
- Tasks left by subagents appear in the parent's `background_tasks`, so the
  parent's final Stop catches them.  Subagent stops are not gated: 447 of the
  535 un-reported tasks are in subagent transcripts, but they surface at the
  parent.

What it does not cover:

- Turns without a status line.  The rule applies only from approval until
  the task closes.
- Tasks left running under an honest `in progress` or `pending` status,
  where the status already tells the user that work is open.
- Teammate tasks.
- The time between a task hanging and the agent's next "done" turn.

### 2. Correct the agent-guard pgrep explanation

`lib/python/agent_guard/policies/secret_leak.py` tells agents that an
unbracketed `pgrep -f` "matches that shell itself".  That is wrong on macOS.
The real risk is matching sibling loops or other processes whose arguments
contain the pattern.  Correct the text.  The advice to bracket a character
or wait on a PID stays.

### 3. One-off cleanup

Preferred: with the user's approval, message each affected session (via
`message-agent-session`) and ask it to TaskStop the listed task ids.  That
clears Claude Code's own task registry, so the next Stop payload and
agent.el agree.

Fallback, only if a session cannot act: kill directly.  Re-identify the
processes fresh at kill time: shells whose parent is that session's
`claude` process, matched to the task's command by its output file
(`tasks/<id>.output` held open on fd 1).  Never reuse PIDs recorded at
incident time.  Signal the whole process group.  Then verify that no
descendant survives, that the session's next Stop payload no longer lists
the task, and that agent.el shows the session idle.

## Rejected alternatives

- Deadlines on background commands: either too long to matter or short
  enough to kill live work.
- Banning until/while polling loops: they fail no more often than other
  tasks.  80 pgrep loops and most grep loops completed normally.
- Showing task age in agent.el: the payload has no start time.  Age would
  show only that a task exists, not that it is stuck.
- Gating every turn end with running tasks: agents legitimately end turns
  while a long task runs and are woken by its completion notification.

## Open questions

- Codex parity.  Does Codex have background tasks that outlive a turn, and
  does its Stop payload expose them?  If not, record the Claude-only scope in
  `ai-config-sync.json` as the paired-config rules require.
- Should a status-less final turn with running tasks also be gated?
  Deciding when work is "finished" without a status line would require
  parsing the prose.

## Verification

- Unit fixtures for each branch of step 1, including teammate-only, `done,
  pending verification` and a missing field.
- Replay the preserved 14:32 dotfiles Stop payload through the hook: it must
  block.
- Live test: in a throwaway Claude session, start `sleep 600` in the
  background and end the turn with `Status: done`.  The stop must be refused.
  After TaskStop, the stop must succeed and agent.el must show the session as
  idle.
- Live subagent case, since most abandoned tasks came from subagents: in a
  throwaway session, have a subagent start `sleep 600` in the background and
  return.  The parent then ends with `Status: done`.  The parent's Stop must
  list the task and be refused.  Cleanup must succeed and agent.el must show
  idle.
- With the Elisp Stop gate also blocking in the same turn, the block count
  must still reach the exhaustion `systemMessage` correctly.

## Review

Codex reviewed commit 1230ae6f2.  All three findings were accepted and
folded in above:

- the loop guard (hook-specific counter, visible exhaustion);
- the subagent live test;
- safe cleanup with fresh process identification.
