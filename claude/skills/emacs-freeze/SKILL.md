---
name: emacs-freeze
description: Diagnose a frozen Emacs by sampling the process, analyzing the stack trace, and suggesting a fix. Use when Emacs is unresponsive.
---

# Emacs freeze diagnosis

Diagnose why Emacs is frozen and recommend how to recover. Be rigorous: a plausible-sounding wrong diagnosis is worse than none, because it leaves the user with false beliefs.

## Procedure

### 1. Find the Emacs process

Run `pgrep -x Emacs` to get the PID. If multiple PIDs are returned, pick the one consuming the most CPU using `ps -p <pids> -o pid=,pcpu=,rss=`. If no Emacs process is found, tell the user and stop.

### 2. Collect diagnostics

Run **all** of the following in parallel:

- **Stack sample**: `/usr/bin/sample <PID> 5 -file /tmp/emacs-sample.txt` — captures 5 seconds of call-stack samples at 1ms intervals.
- **Process stats**: `ps -p <PID> -o pid=,pcpu=,pmem=,rss=,etime=,state=` — CPU%, memory%, RSS, elapsed time, and process state. Note: on macOS, `pcpu` is the long-term average since process start, not instantaneous CPU. Do not use it to infer "currently burning CPU."
- **Open files**: `lsof -p <PID> 2>/dev/null | head -80` — shows files, sockets, and pipes the process has open (can reveal stuck I/O or lock files).
- **Thread count**: `ps -M -p <PID> | wc -l` — number of threads (unusually high counts suggest runaway thread creation).

### 2.5. Try to dump `*Backtrace*` and `*Messages*` via the server

Before analyzing the sample, try to pull these buffers over the server socket. They are primary evidence about *why* the freeze happened; the sample only tells you *where* the stack currently sits.

Find the server socket (usually `/var/folders/.../emacs<uid>/server` or `/tmp/emacs<uid>/server`):

```bash
lsof -p <PID> 2>/dev/null | grep -E '(emacs[0-9]+/server|\.emacs\.d/server)' | awk '{print $NF}' | head -1
```

Then, with a short timeout so you don't hang if the main loop is blocked in C code:

```bash
timeout 2 emacsclient -s <socket> -e \
  '(when (get-buffer "*Backtrace*")
     (with-current-buffer "*Backtrace*"
       (buffer-substring-no-properties (point-min) (point-max))))' \
  > /tmp/emacs-backtrace.txt

timeout 2 emacsclient -s <socket> -e \
  '(with-current-buffer "*Messages*"
     (buffer-substring-no-properties
       (max (point-min) (- (point-max) 5000)) (point-max)))' \
  > /tmp/emacs-messages.txt
```

If `timeout` kills either command, the main loop is blocked and the sample is your only source. If they return data, read them — those buffers usually pinpoint the error immediately.

### 3. Read and analyze the sample

Read `/tmp/emacs-sample.txt` in a specific order. Do not jump to "what looks slow" — work through these steps.

#### 3a. Stuck or busy?

Find the main thread (the first `Thread_...: Main Thread` block). Walk down from the top. Count how many frames in a row have the **same sample count** as the thread's total (e.g., all 4082). A long linear chain of identical counts means every sample had that exact stack — Emacs is **stuck**, not computing. A stack that branches near the top means Emacs is **busy** doing varied work.

This distinction drives everything else:

- **Stuck**: the cause is whatever put the stack in that position. Look at frame structure.
- **Busy**: the cause is whatever's consuming samples. Look at sample counts.

Getting this wrong leads to diagnosing a symptom (something firing inside a wedged main loop) as a cause.

#### 3b. Count nested debuggers

Grep the sample for `call_debugger` and `Fdebug`. Each pair on the same stack is one level of debugger recursive-edit. If there is ≥1, **the freeze is a debugger problem, not a computation problem.** The cause is the error that opened the **outermost** debugger — find the `Fsignal` frame above the outermost `call_debugger` and trace upward to identify the Elisp context that errored.

Code running *inside* a debugger's recursive edit (timers firing, redisplay, process sentinels) is not the cause of the freeze. Ignore it for root-cause purposes, even if it's consuming samples.

#### 3c. If no debuggers, identify the hot path

Only if 3b found no debugger nesting:

- **Recursion**: look for the same function appearing repeatedly in the call chain (e.g., `avl-tree--do-enter` calling itself dozens of times). Deep recursion is the most common freeze pattern.
- **I/O waits**: `select`, `poll`, `kevent`, `read`, `write` at the bottom of the stack suggest the process is blocked on I/O (network, subprocess, file).
- **GC pressure**: `garbage_collect` or `mark_object` dominating the sample suggests memory pressure.
- **Outermost Elisp package** in the hot path (the one closest to `command_loop` or the hook that triggered it) is usually the actionable culprit.

Hex-encoded native-comp symbol names like `F6f72672d...` are hex-encoded ASCII — decode them to get the Elisp function name.

#### 3d. Cross-check with `*Backtrace*` / `*Messages*`

If Step 2.5 succeeded, those buffers override sample-based inference. If they conflict with your reading of the sample, re-read the sample.

### 4. Present the diagnosis

Report, in this order:

1. **What I can see** (from the sample and buffers): state direct observations — e.g., "three nested `call_debugger` frames, outermost opened from a `url-http-async-sentinel` callback." No interpretation yet.
2. **What I infer**: the likely cause, clearly labelled as inference. Explain the reasoning from the observations.
3. **What I don't know**: explicitly state gaps. Examples: "I can't tell from the sample which Elisp code owned the URL callback that errored"; "the specific error condition isn't in the sample." Never paper over gaps with plausible-sounding stories.
4. **How to recover now** — tailored to the diagnosis, and **always warn about data-loss risk before suggesting any kill command**:
   - **If stuck in nested debuggers** and the server is responsive: try `timeout 2 emacsclient -s <socket> -e '(top-level)'` first — this throws out of all recursive edits and returns to top level, preserving state.
   - **If stuck with server responsive** but not in debugger: `timeout 2 emacsclient -s <socket> -e '(keyboard-quit)'`.
   - **If server unresponsive but process accepts signals**: `kill -SIGUSR2 <PID>` to toggle `debug-on-quit`, then `C-g` in Emacs.
   - **Escalation**: `kill <PID>` (SIGTERM) for graceful shutdown. Warn that SIGTERM may not save unsaved buffers if the main thread is blocked in C code.
   - **Last resort only — never run this without explicit user confirmation**: `kill -9 <PID>` (SIGKILL). **This will terminate Emacs immediately with no chance to save unsaved buffers or run shutdown hooks. Any unsaved changes will be lost.** Do NOT include this command in a code block the user might copy-paste without reading. Always present it as a clearly separated warning and ask the user to confirm before proceeding.
5. **How to prevent recurrence**: specific configuration changes only when supported by the observations. Reference the user's `config.org` if the relevant package is configured there.

### Rules

These exist because this skill previously produced a confident, plausible, and wrong diagnosis. Honor them strictly.

- **No unverified citations.** Do not reference specific external patches, packages, issues, or mailing-list threads unless you have fetched and confirmed they exist. Generic statements about a package's behavior are fine; specific claims like "the auth-source-pass-cache patch" are not.
- **Verify facts before stating them.** If you run `find | wc -l`, exclude irrelevant paths (e.g. `.git/`). If you cite Emacs API semantics (idle timers, hooks, etc.), consult documentation or say "I'm not sure about the exact semantics."
- **Distinguish observation from inference.** "The sample shows X" is observation. "X implies Y" is inference. Label them.
- **Do not fill gaps with plausible stories.** If the sample doesn't show something, say so. The user can investigate further; they cannot easily detect fabrication.
- **Symptoms ≠ causes.** A slow operation visible in the sample is not the cause of the freeze unless Step 3a/3b rules out alternatives.

### 5. Clean up

Delete `/tmp/emacs-sample.txt`, `/tmp/emacs-backtrace.txt`, and `/tmp/emacs-messages.txt` after analysis.
