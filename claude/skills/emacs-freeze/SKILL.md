---
name: emacs-freeze
description: Diagnose a frozen Emacs by sampling the process, analyzing the stack trace, and suggesting a fix. Use when Emacs is unresponsive.
---

# Emacs freeze diagnosis

Diagnose why Emacs is frozen and recommend how to recover.

## Procedure

### 1. Find the Emacs process

Run `pgrep -x Emacs` to get the PID. If multiple PIDs are returned, pick the one consuming the most CPU using `ps -p <pids> -o pid=,pcpu=,rss=`. If no Emacs process is found, tell the user and stop.

### 2. Collect diagnostics

Run **all** of the following in parallel:

- **Stack sample**: `/usr/bin/sample <PID> 5 -file /tmp/emacs-sample.txt` — captures 5 seconds of call-stack samples at 1ms intervals.
- **Process stats**: `ps -p <PID> -o pid=,pcpu=,pmem=,rss=,etime=,state=` — CPU%, memory%, RSS, elapsed time, and process state.
- **Open files**: `lsof -p <PID> 2>/dev/null | head -80` — shows files, sockets, and pipes the process has open (can reveal stuck I/O or lock files).
- **Thread count**: `ps -M -p <PID> | wc -l` — number of threads (unusually high counts suggest runaway thread creation).

### 3. Read and analyze the sample

Read `/tmp/emacs-sample.txt`. Focus on:

- **The heaviest branch** (highest sample count on the main thread). Walk down from the top and identify the deepest Elisp function in the hot path. Hex-encoded native-comp symbol names like `F6f72672d...` are hex-encoded ASCII — decode them to get the Elisp function name.
- **Recursion**: look for the same function appearing repeatedly in the call chain (e.g., `avl-tree--do-enter` calling itself dozens of times). Deep recursion in a single function is the most common freeze pattern.
- **I/O waits**: `select`, `poll`, `kevent`, `read`, `write` at the bottom of the stack suggest the process is blocked on I/O (network, subprocess, file).
- **GC pressure**: `garbage_collect` or `mark_object` dominating the sample suggests memory pressure.
- **Which package**: identify the outermost Elisp package in the hot path (the one closest to `command_loop` or the hook that triggered it). This is usually the actionable culprit.

### 4. Present the diagnosis

Report:

1. **What is frozen**: the specific Elisp function/package consuming all the time, and what it's doing (infinite recursion, blocked I/O, runaway computation, etc.).
2. **Why**: the root cause if identifiable (e.g., corrupt org-element cache, network timeout, regexp backtracking).
3. **How to recover now** — present options in escalating order and **always warn the user about data-loss risk before suggesting any kill command**:
   - If Emacs can still process signals: `kill -SIGUSR2 <PID>` to toggle `debug-on-quit`, then `C-g` in Emacs.
   - If that doesn't work: `kill <PID>` (SIGTERM) to let Emacs attempt a graceful shutdown. Warn the user that SIGTERM may not save unsaved buffers if the main thread is blocked in C code.
   - **Last resort only — never run this without explicit user confirmation**: `kill -9 <PID>` (SIGKILL). **This will terminate Emacs immediately with no chance to save unsaved buffers or run shutdown hooks. Any unsaved changes will be lost.** Do NOT include this command in a code block the user might copy-paste without reading. Always present it as a clearly separated warning and ask the user to confirm before proceeding.
4. **How to prevent recurrence**: specific configuration changes, package updates, or workarounds. Reference the user's `config.org` if the relevant package is configured there.

### 5. Clean up

Delete `/tmp/emacs-sample.txt` after analysis.
