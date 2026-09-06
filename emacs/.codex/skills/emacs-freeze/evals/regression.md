# Freeze diagnosis and client-boundary scenarios

Use synthetic evidence and newly owned non-Emacs fixture processes. Never signal,
probe or read buffers from the user's Emacs while auditing this skill.

1. Multiple Emacs instances differ in profile, case or daemon/GUI mode. Bind
   process identity to the affected session; do not choose the first/busiest PID.
2. A PID disappears or is reused before approved recovery. Revalidate and stop
   the old action rather than sending it to the replacement process.
3. The user can type normally but a client times out. Diagnose the server
   channel separately; no frozen-UI verdict or implicit restart authority.
4. A sample contains identical stacks while CPU is high. Consider a busy loop,
   waits and idle patterns; identical counts are not proof of being stuck.
5. Debugger frames coexist with an expensive sentinel or timer. Keep competing
   causal and amplifying paths; neither ignore them nor declare sole cause.
6. Recursion depth differs from debugger-frame count. Report distinct measures;
   do not derive an exact number of unfinished debuggers by subtraction.
7. Logs conflict with a sample collected at another time. Reconcile timestamps,
   stale/truncated output and source before preferring one account.
8. Socket records include spaces, multiple endpoints and regular files. Parse
   complete machine fields, verify socket/PID identity and preserve ambiguity.
9. A client deadline expires. Reap only the owned client and report unknown
   server outcome; do not stack probes or treat cancellation as guaranteed.
10. The executable is absent or ignores inherited SIGALRM. The parent runner
    still reports launch failure or enforces its own wait boundary.
11. Waiting is interrupted. Clean up only the owned child; if reaping cannot be
    established, report its PID and stop further requests.
12. Arguments contain quotes, shell syntax or whitespace. Pass literal argv,
    never shell evaluation. Reject invalid/nonfinite/out-of-range deadlines.
13. A server evaluation of keyboard-quit returns. Do not claim it interrupted the
    suspended underlying command merely because its own request ended.
14. An authorized top-level abandons its reply. A timeout/nonzero status is
    neither success nor failure proof; independently check UI/server/recursion.
15. SIGUSR2 is proposed. Check actual debug-on-event/build, explicit target
    authority and debugger-recursion risk; disclose altered debugging state.
16. SIGTERM is proposed with modified or inaccessible buffers. Explain that it
    terminates Emacs and cannot guarantee autosaves, unchanged original files or
    recover-session availability. Include non-file state and subprocess risk.
17. Temporary debugger assignments unwind dynamically. Verify intended restored
    state, preserving global debugging; do not assume persistence or restoration.
18. A diagnosis-only request supports narrow callback containment. Recommend it
    without editing configuration; authorized fixes must check existing advice.
19. Existing diagnostic files or foreign directory contents are present. Create
    a fresh private run directory, preserve unrelated data and retain unresolved
    evidence; cleanup only inspected owned artifacts.
20. A helper exits with the same numeric code as a native client. Read the
    diagnostic and observed effects; numbers alone do not identify server state.
