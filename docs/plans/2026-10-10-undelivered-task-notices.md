# Wake Claude sessions whose task notices Claude Code left undelivered

## Problem

On 2026-10-10 the Epoch/meetings Claude session (f44df81c) was shown green
(idle) in agent.el.  Its last message said `Status: in progress: tracker
work about to push`.  Nothing was running.

- **Evidence: the run and its notice.**
  - At 15:27 BST agent-team teammate `phase2c` started background shell
    `bfe8k1c3s` (a full verify run).  It then ended its turn, waiting for
    the completion notice.
  - The run finished at 15:34 with exit 0.
  - The main transcript records `queue-operation` `enqueue` of the
    `<task-notification>` for `bfe8k1c3s` at 14:34:54Z.
- **Evidence: the notice stayed undelivered.**
  - The lead's Stop at 14:50:42Z (15:50 BST) left it undelivered.  Up to that
    point the transcript holds 120 enqueues and 119 dequeue/remove
    operations.
  - Neither the lead's conversation nor phase2c's transcript received it.
  - The lead's message, "in its final test run before it pushes", was stale.
- **Evidence: what released it.**
  - At about 16:41 BST another session sent the lead a message (agent-send)
    saying that the run had finished.
  - The lead woke phase2c.  At 15:42:37Z (16:42 BST) the transcript recorded
    `remove` with the `bfe8k1c3s` notice content, and phase2c resumed.
- **Diagnosis.** This is a Claude Code delivery defect: a teammate's task
  notice is queued in the session but does not wake the idle
  teammate/session.  agent.el's green was correct.  The user will not file a
  bug report, so the remedy must be local.

## Queue record format (observed, undocumented)

Main-session transcript lines:

```json
{"type":"queue-operation","operation":"enqueue","timestamp":...,"sessionId":...,"content":"<task-notification>...<task-id>ID</task-id>..."}
{"type":"queue-operation","operation":"dequeue","timestamp":...,"sessionId":...}
{"type":"queue-operation","operation":"remove","timestamp":...,"sessionId":...,"content":"<task-notification>..."}
```

- `dequeue` carries no content, so which item it consumes is inferred.
- In the meetings transcript, totals balance (120 enqueue = 40 dequeue + 80
  remove) once the notice is delivered.
- Enqueued content can also be `<agent-message ...>` or other kinds.

Over the last 14 days, about 40 distinct transcripts end with more enqueues
than consumptions.  Most are probably sessions that exited with items still
queued; this is unverified.

## Proposed design

### Step 1: replay check (gate)

Before building anything, write a script that replays every Claude
transcript from the last four weeks.  At each turn end (`system` /
`stop_hook_summary`), it computes the outstanding queued items:

- Pair each `remove` with an outstanding enqueue of identical content.
- Pair each `dequeue` with the oldest outstanding enqueue (FIFO).  This
  pairing is an assumption, and the replay must test it, for example by
  checking that a `dequeue` is followed by a user message carrying that
  item's content.

The rule fires when an item enqueued before the turn end is still
outstanding at the turn end and stays outstanding until the next user
prompt or later.

Acceptance:

- Report the number of firings, and classify each by inspection as
  genuinely stuck or as a false positive.
- Proceed only if the false positives are zero, or are explained and
  excluded by a refined rule.
- Report whether FIFO pairing of `dequeue` is consistent with the
  transcripts.
- If the rule cannot be made precise, stop and report.  Ship neither the
  flag nor the wake-up.

### Step 2: detection in agent-claude.el

- On each Stop event (`agent-claude--handle-stop`), read the session
  transcript (`transcript_path` from the Stop payload) incrementally from a
  buffer-local byte offset.  Update a buffer-local outstanding-queue model
  using the pairing rule validated in step 1.
- Mark the session stuck when, after the Stop, an item enqueued before it
  remains outstanding.  Only `<task-notification>` items count, unless step
  1 supports including others.
- Clear the mark when the item's consumption line appears or a new turn
  starts.
- A transcript that cannot be read or parsed leaves the state unchanged and
  logs a warning.  It is never read as "nothing stuck".

### Step 3: automatic wake-up

- When a session is first marked stuck for a given item, agent.el submits
  one message to the session (via `agent-submit`), framed as coming from
  agent.el, not from the user:

  > agent.el: Claude Code queued this notice at HH:MM but has not delivered
  > it to you or your teammates: <notice content>.  Act on it if it is
  > yours, or pass it to the teammate that started the task.

- Send at most one wake-up per item id.  Record sent ids buffer-locally so
  the wake-up cannot loop.
- If the item is still outstanding after the turn that the wake-up started
  ends, show a distinct `stuck` state (its own face) in the switcher, mode
  line and phone page, naming the item and its queue time.  Send nothing
  further.
- A defcustom turns the wake-up off, leaving only the flag.  Default: on,
  if step 1 passes.

### Step 4: tests and verification

- ERT fixtures built from the meetings transcript's queue lines:
  - the stuck case;
  - normal enqueue/dequeue;
  - remove-by-content;
  - an unreadable transcript;
  - the one-wake-up limit;
  - clearing on delivery.
- Live check: arranging the Claude Code defect on purpose may not be
  possible.  If it cannot be reproduced, say so and rely on the replay and
  fixtures.  Then watch the next real occurrence.
- The README.org/agent.texi manual documents the new state and the
  defcustom.

## Risks

- The queue format is undocumented and may change with Claude Code
  releases.  An unknown record shape must not produce a wake-up.
- A wrong pairing rule would inject messages into sessions that are not
  stuck.  Step 1 gates this risk, and the one-per-item limit bounds it.
- A wake-up spends a model turn and may prompt the agent to act.  The
  message limits itself to delivering the notice.
- Codex: not applicable, since Codex has no agent-team notice queue.  This
  is recorded as Claude-only.

## Revisions after Codex review (commit 2fe8f05ad)

All seven findings were accepted.  Where they conflict with the steps above,
these revisions take precedence.

1. **One rule for replay and production.** Production fires only when the
   item is still outstanding at a final recheck immediately before acting.
   The recheck re-reads the transcript after Stop.  The replay must evaluate
   this exact rule, including the case of normal delivery shortly after a
   Stop.
2. **Hook transport.** `hooks/notify-emacs-state.sh` currently forwards only
   the buffer, the send time and `agent_id`.  It must also forward
   `transcript_path` and `session_id`.  agent.el checks that the
   `session_id` matches the session's recorded id before reading.  The
   change is verified through the real fire-and-forget wrapper.
3. **Safe submission.** `agent-submit` types into the live prompt and presses
   Return, so a draft the user is writing would be sent with it.  The
   wake-up is submitted only when the session is waiting and its prompt is
   verifiably empty, as read from the terminal screen.  Otherwise it is
   deferred and the `stuck` flag is shown.  Tests: a populated prompt, and a
   turn that starts between detection and submission.
4. **Observing changes while idle.** Reading the transcript only at Stop
   misses deliveries and enqueues that happen while the session is idle.
   While a session is waiting with an outstanding item, agent.el watches its
   transcript with `file-notify` and re-reads incrementally on change.  A
   transcript change is an event, not a timer.  The watch is removed when
   the session exits, its identity changes, or nothing is outstanding.
5. **Invalid queue knowledge blocks action.** Each of the following marks
   the queue model invalid, which blocks any wake-up until the model is
   rebuilt from the start of the transcript:
   - a failed read or parse;
   - an unknown `queue-operation` shape;
   - a transcript shorter than the stored offset (truncation or
     replacement);
   - an incomplete final line, which is left unconsumed until complete.

   The offset and the queue update are committed together.
6. **Inconclusive replay outcomes.** Each firing is classified as stuck,
   false positive or inconclusive.  Where needed, the intended recipient's
   transcript is correlated: for teammate notices, check the teammate's
   transcript for receipt.  An inconclusive pattern blocks automatic
   wake-up for that pattern, and only the flag ships for it.
7. **Flag-only mode and integration checks.** With the wake-up disabled or
   deferred, the `stuck` flag is set as soon as the production rule fires.
   Verify the path from hook to reader to submission, and the switcher,
   mode-line and phone displays, with controlled inputs: a real hook run
   against a synthetic transcript holding a stuck item.  This holds even if
   the upstream defect cannot be reproduced.
