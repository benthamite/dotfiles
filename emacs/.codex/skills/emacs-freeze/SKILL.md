---
name: emacs-freeze
description: Diagnose a frozen, hung, beachballing, or unresponsive Emacs by sampling the process, reading server buffers when possible, and recommending recovery. Use when Emacs is stuck or not responding; do not use for ordinary Elisp errors while Emacs is responsive.
---

# Emacs freeze diagnosis

Diagnose why Emacs is frozen and recommend how to recover. Be rigorous: a plausible-sounding wrong diagnosis is worse than none, because it leaves the user with false beliefs.

Use this for frozen, hung, beachballing, or otherwise unresponsive Emacs sessions. Do not use it for ordinary Elisp errors, package warnings, or slow commands when Emacs still accepts input.

Safety boundary: diagnostics are allowed, but recovery actions that change Emacs state require explicit user confirmation before you run them. This includes `(top-level)`, `(keyboard-quit)`, `kill -SIGUSR2`, SIGTERM, and SIGKILL. You may recommend them with warnings; do not send signals to an active Emacs session without confirmation.

User workflow invariant: `debug-on-error` is intentionally enabled for the
user's normal workflow. Do not recommend disabling it globally or commenting
out the user's global `debug-on-error` toggle as a prevention or recovery
strategy. If `debug-on-error` turns async, timer, process-filter, or sentinel
errors into disruptive recursive debuggers, recommend a narrow containment
around the specific recurring async path instead: bind `debug-on-error` to nil
inside that timer/sentinel/filter, catch and message the error, and preserve
global `debug-on-error` everywhere else.

This invariant also governs *recovery*, not just prevention. Clearing
`debug-on-error` (or `debug-on-quit`) to break a debugger loop is legitimate as
an emergency measure, but it is a temporary change to the user's environment:
restore it in the same session and say explicitly what you changed and what you
restored. Do not leave it cleared and do not present leaving it cleared as the
prevention plan.

Channel boundary: an `emacsclient` timeout is evidence that the server/eval
channel did not return *within that timeout*. It is not proof that the channel
is dead, and it is not proof that the user's interactive Emacs UI is frozen. If
the user reports that Emacs accepts input or otherwise looks responsive, treat
that as primary evidence about the UI channel and diagnose queued clients,
hooks, and server requests separately.

**A slow server is not a dead server.** A wedged Emacs may still service
process input intermittently — from inside `accept-process-output`, a recursive
edit, or a process filter — so server requests can take a minute or more to
land instead of never landing. In the session that produced this rule, 3-second
and 10-second probes both timed out and were misread as "server unresponsive,"
which pushed the whole diagnosis toward kill options. A 90-second probe returned
normally, and the session was then recovered with no data loss and no kill.
Never conclude "unresponsive" without completing the timeout ladder in step 2.5.

## Procedure

### 1. Find the Emacs process

Run `pgrep -x Emacs` to get the PID. If multiple PIDs are returned, inspect them with `ps -p <pids> -o pid=,pcpu=,rss=,etime=,state=,command=` and pick the best candidate, stating the selection heuristic. A GUI session is the one with `--init-directory` or no `--batch`; `-Q --batch` entries are helper workers and are rarely the culprit. If no Emacs process is found, tell the user and stop.

On macOS, `ps` `%cpu` is "a decaying average over up to a minute of previous (real) time" (`man ps`), **not** a lifetime average — so it *is* usable as evidence of current CPU burn. Take two or three readings a minute apart: a rising or pegged value means active work, and `rss` moving between readings distinguishes a live runaway from a static wedge.

### 2. Collect diagnostics

Set per-process diagnostic paths before collecting data:

```bash
DIAG_DIR="${TMPDIR:-/tmp}/emacs-freeze-${PID}"
mkdir -p "$DIAG_DIR"
SAMPLE_FILE="$DIAG_DIR/sample.txt"
BACKTRACE_FILE="$DIAG_DIR/backtrace.txt"
MESSAGES_FILE="$DIAG_DIR/messages.txt"
```

Run **all** of the following in parallel:

- **Stack sample**: `/usr/bin/sample <PID> 5 -file "$SAMPLE_FILE"` — captures 5 seconds of call-stack samples at 1ms intervals.
- **Process stats**: `ps -p <PID> -o pid=,pcpu=,pmem=,rss=,etime=,state=` — CPU%, memory%, RSS, elapsed time, and process state. Repeat this once or twice more later in the analysis; `%cpu` is a one-minute decaying average and `rss` movement between readings tells you whether the process is still doing work.
- **Open files**: `lsof -p <PID> 2>/dev/null | head -80` — shows files, sockets, and pipes the process has open (can reveal stuck I/O or lock files).
- **Thread count**: `ps -M -p <PID> | wc -l` — number of threads (unusually high counts suggest runaway thread creation).

### 2.5. Try to dump `*Backtrace*` and `*Messages*` via the server

Before analyzing the sample, try to pull these buffers over the server socket. They are primary evidence about *why* the freeze happened; the sample only tells you *where* the stack currently sits.

Find the server socket (usually `/var/folders/.../emacs<uid>/server` or `/tmp/emacs<uid>/server`):

```bash
SOCKET=$(lsof -p <PID> 2>/dev/null | grep -E '(emacs[0-9]+/server|\.emacs\.d/server)' | awk '{print $NF}' | head -1)
```

If no socket is found, say so and continue with the sample. If a socket is found, define a small timeout helper that works on macOS without requiring GNU `timeout`, then use it so you do not hang if the main loop is blocked in C code:

```bash
run_with_timeout() {
  perl -e 'alarm shift; exec @ARGV' "$@"
}

**Establish liveness first, with an escalating timeout ladder.** Do not start
with the buffer dumps — start with the cheapest possible probe and give it real
time before concluding anything:

```bash
for t in 5 30 120; do
  echo "--- probe (${t}s) ---"
  run_with_timeout $t emacsclient -s "$SOCKET" -e '(emacs-pid)' && break
done
```

Only after **the 120-second probe has also failed** may you describe the server
as unresponsive. A shorter failure means "slow", not "dead", and the difference
decides whether recovery is a one-line eval or a kill. This ladder exists
because skipping it once turned a fully recoverable session into a
recommendation to terminate Emacs.

Once a probe returns, pull the buffers using a timeout at least as large as the
one that worked:

```bash
run_with_timeout 120 emacsclient -s "$SOCKET" -e \
  '(when (get-buffer "*Backtrace*")
     (with-current-buffer "*Backtrace*"
       (buffer-substring-no-properties (point-min) (point-max))))' \
  > "$BACKTRACE_FILE"

run_with_timeout 120 emacsclient -s "$SOCKET" -e \
  '(with-current-buffer "*Messages*"
     (buffer-substring-no-properties
       (max (point-min) (- (point-max) 5000)) (point-max)))' \
  > "$MESSAGES_FILE"
```

Also ask the server for state the stack sample cannot give you, in particular
`(recursion-depth)` — see step 3b, where it is required.

If the ladder fully fails, say so precisely ("no reply within 120 s") rather
than "the server is dead", and use the sample, process state, and the user's
report of interactive UI behavior before inferring that Emacs itself is frozen.
If the commands return data, read them — those buffers usually pinpoint the
error immediately.

Keep these probes cheap and bounded. Never send an expression that prints a
large object, iterates a buffer character by character, or has unpredictable
runtime: a hung `emacsclient` occupies the server queue and makes things worse.

### 3. Read and analyze the sample

Read `$SAMPLE_FILE` in a specific order. Do not jump to "what looks slow" — work through these steps.

#### 3a. Stuck or busy?

Find the main thread (the first `Thread_...: Main Thread` block). Walk down from the top. Count how many frames in a row have the **same sample count** as the thread's total (e.g., all 4082). A long linear chain of identical counts means every sample had that exact stack — Emacs is **stuck**, not computing. A stack that branches near the top means Emacs is **busy** doing varied work.

This distinction drives everything else:

- **Stuck**: the cause is whatever put the stack in that position. Look at frame structure.
- **Busy**: the cause is whatever's consuming samples. Look at sample counts.

Getting this wrong leads to diagnosing a symptom (something firing inside a wedged main loop) as a cause.

#### 3b. Count nested debuggers

Grep the sample for `call_debugger` and `Fdebug`. If there is ≥1, **the freeze is a debugger problem, not a computation problem.**

**`call_debugger` frames are not recursive edits.** `debug` renders `*Backtrace*` via `debugger-setup-buffer` *before* it reaches `(recursive-edit)`, so a debugger level that is still rendering has no recursive edit yet. Do not report nesting depth from frame counts alone — ask the server for `(recursion-depth)` and report both. In the session that produced this rule the sample showed ~1,350 `call_debugger` frames while `(recursion-depth)` was **5**: about 1,345 levels were still stuck mid-render. The two numbers mean different things and the gap between them is itself diagnostic.

Identify the error that opened the **outermost** debugger, but be aware you may not be able to: see the truncation warning in 3c. Note also that nested entry can occur during *unwinding* (`unwind_to_catch → unbind_to → bcall0 → call_debugger`), not only on a fresh signal — if you see that chain, the recursion is being driven by unwind forms, and say so rather than assuming plain re-signalling.

Code running *inside* a debugger's recursive edit (timers firing, redisplay, process sentinels) is not the cause of the freeze. Ignore it for root-cause purposes, even if it's consuming samples.

A known self-sustaining pattern worth recognising: `backtrace--print-func-and-args` calls `symbol-file` once per frame, which scans `load-history`. On a large configuration that is ~1–2 ms per frame, so rendering level N costs N × ~1.5 ms and the cost of reaching depth N is quadratic. The render window is seconds long at depth, and it is interruptible, so a quit arriving mid-render opens another level. If the sample shows `symbol-file`/`Fmember` under `backtrace--print-func-and-args` under `debugger-setup-buffer`, this is what you are looking at.

#### 3c. If no debuggers, identify the hot path

Only if 3b found no debugger nesting:

- **Recursion**: look for the same function appearing repeatedly in the call chain (e.g., `avl-tree--do-enter` calling itself dozens of times). Deep recursion is the most common freeze pattern.
- **I/O waits**: `select`, `poll`, `kevent`, `read`, `write` at the bottom of the stack suggest the process is blocked on I/O (network, subprocess, file).
- **GC pressure**: `garbage_collect` or `mark_object` dominating the sample suggests memory pressure.
- **Outermost Elisp package** in the hot path (the one closest to `command_loop` or the hook that triggered it) is usually the actionable culprit.

Hex-encoded native-comp symbol names like `F6f72672d...` are hex-encoded ASCII — decode them to get the Elisp function name. Decoding the whole main-thread block at once and ranking by frequency is the fastest way to see the shape:

```bash
grep -oE 'F[0-9a-f]{6,}_[a-z0-9_-]+_0' "$MAIN_THREAD" | sort | uniq -c | sort -rn | head -30 |
  while read -r c sym; do
    hex=$(echo "$sym" | sed -E 's/^F([0-9a-f]+)_.*/\1/')
    printf "%6d  %s\n" "$c" "$(printf "$(echo "$hex" | sed 's/../\\x&/g')")"
  done
```

**`sample` truncates deep stacks.** It walks back a bounded number of frames from the leaf, so on a very deep stack the real root (`main`, the outermost `command_loop`, `read_char`) is cut off and the topmost frame you see is already somewhere in the middle. Check for this before calling any frame "the outermost": if `read_char` and `main` are absent, the stack was truncated and **you cannot identify the originating error from the sample**. Say so plainly instead of naming the highest visible frame as the cause. The `*Backtrace*` and `*Messages*` dumps from step 2.5 are then your only route to the original error, which is another reason to complete the timeout ladder.

#### 3d. Cross-check with `*Backtrace*` / `*Messages*`

If Step 2.5 succeeded, those buffers override sample-based inference. If they conflict with your reading of the sample, re-read the sample.

### 4. Present the diagnosis

Report, in this order:

1. **What I can see** (from the sample and buffers): state direct observations — e.g., "three nested `call_debugger` frames, outermost opened from a `url-http-async-sentinel` callback." No interpretation yet.
2. **What I infer**: the likely cause, clearly labelled as inference. Explain the reasoning from the observations.
3. **What I don't know**: explicitly state gaps. Examples: "I can't tell from the sample which Elisp code owned the URL callback that errored"; "the specific error condition isn't in the sample." Never paper over gaps with plausible-sounding stories.
4. **How to recover now** — tailored to the diagnosis. Present the least invasive viable action first, ask for explicit confirmation before running any recovery command yourself, and **always warn about data-loss risk before suggesting any kill command**:
   State plainly, for every option you offer, whether it kills Emacs. Users
   reasonably read "recovery" as "without losing my session"; an option list
   that leaves this implicit is a failure of the explanation, not a detail.

   - Do not recommend restart, SIGUSR2, `(keyboard-quit)`, `(top-level)`, or any other recovery action solely because `emacsclient` timed out. Complete the step 2.5 timeout ladder first. Establish that the user-facing UI is actually frozen, or that the action targets only the blocked server/client channel.
   - **If stuck in nested debuggers and the server answers at any timeout** — the common case, and the one that needs no kill. Use two steps, in this order:

     ```bash
     run_with_timeout 180 emacsclient -s "$SOCKET" -e \
       '(setq debug-on-quit nil debug-on-error nil inhibit-debugger t)'
     run_with_timeout 120 emacsclient -s "$SOCKET" -e '(top-level)'
     ```

     The first call starves the loop so no new debugger levels can spawn; without it the unwind can itself re-enter the debugger. The second throws out of *all* recursive edits at once — `top-level` is caught only by the outermost command loop, so depth does not matter.

     `(top-level)` returns **no reply**: the throw abandons the server connection, so `emacsclient` exits non-zero (142 under `run_with_timeout`). **That is success, not failure.** Verify by probing again — `(recursion-depth)` should be 0 and the probe should return promptly.

     Afterwards, report which debug variables you left changed. `inhibit-debugger` is usually restored to `nil` automatically by the unwind, but `debug-on-quit`/`debug-on-error` stay as you set them.
   - **If the server answers but there is no debugger nesting**: `run_with_timeout 60 emacsclient -s "$SOCKET" -e '(keyboard-quit)'`. Run it only after confirmation.
   - **SIGUSR2 (`pkill -SIGUSR2 Emacs`, the user's `emacsk` alias)**: this does **not** kill Emacs and does **not** toggle any debug variable. `debug-on-event` defaults to `sigusr2`, so it breaks Emacs into the Lisp debugger — the standard way to interrupt a long computation and see where it is. It is the right tool for a *busy* Emacs. It is **counterproductive for a debugger-recursion freeze**, where it just adds another level; say so explicitly rather than listing it as a generic option.
   - **Escalation — `kill <PID>` (SIGTERM). This kills Emacs; the session ends.** Verified behaviour: Emacs runs `kill-emacs-hook` and auto-saves modified buffers to their `#file#` auto-save files. It does **not** write your actual files. Tell the user to recover with `M-x recover-session` after restarting. Only after confirmation.
   - **Last resort — never run without explicit user confirmation**: SIGKILL. **Terminates Emacs immediately: no shutdown hooks, no auto-save, all unsaved work lost.** Do NOT put it in a code block the user might copy-paste without reading. Present it as a clearly separated warning and ask the user to confirm.
   - Before proposing any kill, check what is actually at risk: `(length (seq-filter (lambda (b) (and (buffer-file-name b) (buffer-modified-p b))) (buffer-list)))`. "There are 0 modified buffers" changes the conversation.
5. **How to prevent recurrence**: specific configuration changes only when
   supported by the observations. Reference the user's `config.org` if the
   relevant package is configured there. Preserve the user's global
   `debug-on-error` workflow. Do not suggest disabling it. For debugger freezes
   caused by async callbacks, recommend narrowly wrapping only the identified
   recurring timer, process filter, sentinel, or package entry point with local
   `debug-on-error` suppression and explicit error reporting.

   If you cleared any debug variable during recovery, restore it here and state
   both the change and the restoration. A prevention section that quietly
   depends on the user's debugger staying off is not a prevention plan.

### Rules

These exist because this skill previously produced a confident, plausible, and wrong diagnosis. Honor them strictly.

- **No unverified citations.** Do not reference specific external patches, packages, issues, or mailing-list threads unless you have fetched and confirmed they exist. Generic statements about a package's behavior are fine; specific claims like "the auth-source-pass-cache patch" are not.
- **Verify facts before stating them.** If you run `find | wc -l`, exclude irrelevant paths (e.g. `.git/`). If you cite Emacs API semantics (idle timers, hooks, etc.), consult documentation or say "I'm not sure about the exact semantics."
- **Distinguish observation from inference.** "The sample shows X" is observation. "X implies Y" is inference. Label them.
- **Do not fill gaps with plausible stories.** If the sample doesn't show something, say so. The user can investigate further; they cannot easily detect fabrication.
- **Symptoms ≠ causes.** A slow operation visible in the sample is not the cause of the freeze unless Step 3a/3b rules out alternatives.
- **A timeout is not a verdict.** "Did not answer in N seconds" is the observation. "The server is unresponsive" is an inference, and it is only warranted after the full ladder in step 2.5. Getting this wrong points the entire diagnosis at destructive recovery.
- **Answer the question that was asked.** If the user asks what an option does, lead with the direct answer — "yes, that kills Emacs" — before any qualification. Do not restate the diagnosis instead of answering.
- **Prefer measuring to recalling.** Every claim about Emacs internals in this skill was wrong at least once before it was tested. If a fact can be checked with a two-line `emacs -Q --batch` probe or a docstring lookup, check it. When building such a probe, note that a `condition-case` handler around the signal suppresses debugger entry, and that batch mode does not enter the debugger the way an interactive session does — a probe that reports "no debugger entered" may be measuring its own harness.

### 5. Clean up

Clean up the per-PID diagnostic directory after analysis. Prefer `trash "$DIAG_DIR"` when available; otherwise remove only the known files you created (`rm -f "$SAMPLE_FILE" "$BACKTRACE_FILE" "$MESSAGES_FILE"; rmdir "$DIAG_DIR"`). Do not use recursive deletion for cleanup.
