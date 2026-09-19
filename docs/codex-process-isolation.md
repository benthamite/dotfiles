# Codex process isolation from Emacs

`bin/codex-isolated` is a pipe transport client. A separately registered launchd
service runs `bin/codex_process_broker.py serve` and starts the actual
`/opt/homebrew/bin/codex`. Set Emacs's `codex-program` to the client's absolute
path. This covers app-server sessions, plugin queries and noninteractive exec.

The client passes stdin/stdout/stderr file descriptors over a private Unix
socket, preserving binary streams and separate stderr. Arguments, environment
and working directory travel only through that socket; the broker does not
log or persist them. Both endpoints verify the peer UID. The runtime directory
is UID-owned mode 0700, its socket is mode 0600, and symlink ancestors are
rejected. The broker's executable is fixed at startup, never chosen by a client.
The `--binary` service argument exists for harmless fixture testing.

Missing or unavailable service means an explicit failure. There is no local
execution fallback. Each worker has its own process group; client disconnection
or normal broker termination cleans that group without terminating another
session. Descendants that establish another process group or session are outside
this cleanup boundary. Codex command execution does establish separate groups;
the broker cannot guarantee termination of all outstanding commands. SIGKILL
cannot run Python cleanup; launchd's service process-group
behavior must not be confused with worker cleanup because workers have their
own groups. Existing sessions are not migrated by changing `codex-program`.

Terminal file descriptors also pass through, allowing noninteractive exec
callers that use a PTY to preserve their stream behavior. This does not provide
controlling-terminal attachment, job control, signal forwarding or terminal
resize forwarding. Interactive Eat/vterm support is not established.

Register the LaunchAgent through the maintained launchd installer and registry.
Do not run the broker from Emacs: that defeats its intended responsibility
boundary. Inspect macOS responsibility/coalition metadata for a harmless worker
after launchd deployment. A different parent PID alone does not establish the
boundary. Protocol and lifecycle tests establish transport behavior; they do
not prove how XProtect will handle the original detection. Never replay the
suspected triggering payload on the working Mac merely to test this change.

Focused verification: `python3 -B -m unittest discover -s tests -p test_codex_process_broker.py`.

On 2026-09-18, a real app-server initialize and `command/exec` completed through
the installed service. Native `proc_pidinfo(PROC_PIDCOALITIONINFO)` reported
resource/jetsam coalitions 243308/243309 for the broker, Codex and its command,
versus 1292/243179 for Emacs. These IDs are ephemeral; they establish the observed
separate coalition, not a reproduction of XProtect's original container kill.
The live Emacs app-server backend then completed the same handshake and command
in a displayed verification buffer, preserving `CODEX_BUFFER_NAME` and using
the broker coalition. Emacs PID 97836 remained unchanged. The configuration was
tangled for the active profile and `codex-program` updated without restarting it.
PTY `codex exec --help` also completed successfully without a model turn.

A long-running `command/exec` probe survived server termination both through
the broker and with Codex launched directly; the separate-group lifetime limit
above is existing Codex behavior. All owned probe processes were cleaned up.
