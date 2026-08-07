# Fail-Closed Elisp Reload Design

## Goal

Keep asynchronous Elisp rebuilds safe for the live Emacs daemon while making it
impossible to mistake an undelivered, incomplete, or semantically incomplete
reload for success.  A changed package must either be positively acknowledged
as active in the intended daemon or remain visibly marked as requiring reload.

## Failure being addressed

The 2026-08-07 `agent-exit` repair was committed and passed batch tests plus a
cold-start end-to-end test, but the active Emacs daemon continued to run the old
Agent bytecode.  A Codex session started after the commit inherited that stale
runtime and reproduced the original failure.

Two independent conditions caused the stale runtime:

1. The active Emacs server queue was already blocked.  The initial
   `emacsclient` call in `load-elisp-after-edit.sh` therefore never enqueued a
   rebuild, and its `|| exit 0` path hid the delivery failure.
2. The Agent change added advice from `agent-codex--mode-enable`.  Reloading the
   file would redefine the enable function but would not rerun it while the
   global mode was already enabled, leaving the new advice absent.

The disposable verification Emacs started from clean state, so it naturally
installed the advice and did not exercise either hot-reload failure.

## Invariant

A changed Elisp package is either acknowledged as built, loaded, and reconciled
inside the intended active daemon, or every affected workflow retains explicit
reload debt and refuses to claim or depend on freshness.

## Design

### Preserve asynchronous rebuilds

Keep the June 2026 token-based architecture.  The live daemon enqueues the
Elpaca build and returns immediately; completion and reload run from
`elpaca-post-queue-hook`; shell callers use short, bounded status requests.
Never restore `elpaca-wait` or another long event-pumping expression inside an
`emacsclient` request.

The distinction is between a short request and a best-effort request.  Short
requests remain mandatory, but their failure must be durable and visible.

### Record reload debt before delivery

Before attempting the initial enqueue request, the reload hook records reload
debt for each changed production Elisp package.  The record lives outside the
Drive tree in per-profile runtime state and contains at least:

- package and profile;
- canonical source path;
- source revision and dirty-content identity where applicable;
- creation time;
- state (`pending`, `delivery-failed`, `build-failed`, or `reload-failed`);
- the last concrete error.

The record is not keyed only by the Codex or Claude session ID.  A new agent
session must see unresolved debt created by an earlier session.

If the edit event cannot be attributed to a profile, record conservative
unattributed debt rather than silently choosing a profile.  Recovery may refine
the record after the daemon becomes reachable.

### Clear debt only from a positive daemon receipt

The initial enqueue call uses a timeout shorter than the outer hook timeout.  A
timeout, unavailable server, malformed response, or missing token changes the
debt record to `delivery-failed`, emits a clear hook failure, and returns
nonzero.  No `|| exit 0` path is permitted.

After the asynchronous build finishes, the daemon reloads the package,
reconciles runtime wiring, and records a receipt containing:

- package and profile;
- daemon PID and Emacs start time;
- source identity requested by the hook;
- exact loaded artifact path and hash;
- completion time;
- runtime reconciliation result.

Only a receipt matching the outstanding source identity clears reload debt.
Running an arbitrary `emacsclient --eval` expression, polling a token, compiling
in batch, or loading the code in another Emacs process cannot clear it.

### Make package wiring reload-safe

Packages that install advice, hooks, timers, process filters, or global-mode
state must expose one idempotent runtime-reconciliation path.  Initial mode
enablement and hot reload both call that path; mode disablement calls its paired
removal path.

For Agent, `agent-codex-mode` reconciliation must ensure that every owned advice
is present exactly once, including the app-server completion advice.  Reloading
`agent-codex.el` while the mode is already active must reconcile the wiring
without killing or restarting existing Codex processes.

The daemon receipt is written only after reconciliation succeeds.  A
reconciliation error leaves debt in `reload-failed` state.

### Gate freshness-dependent workflows

`agent-start-session` checks Agent's reload state before starting a backend
process.  It refuses to start when:

- Agent has unresolved reload debt for the current profile;
- the active build identity differs from the daemon's receipt; or
- Agent's required runtime wiring is incomplete.

The error names the package, source identity, failure state, and recovery
command.  It never prompts for approval.  Diagnostic, batch-test, rebuild,
reload, and state-inspection commands remain available so a failed daemon can
be repaired.

The shell guard also blocks completion claims and publication paths while
reload debt remains, but it must not block the commands needed to diagnose or
clear that debt.

### Cover real tool envelopes

Hook tests use recorded payload shapes for direct edits, nested
`functions.exec` calls containing `tools.apply_patch`, shell edits, and commits.
They prove that a production Elisp change creates debt before any reload
request.  The hook matcher and payload parser must agree; a test that merely
asserts that fake `emacsclient` was invoked is insufficient.

If the product does not emit a nested `apply_patch` event separately, the outer
`functions.exec` path extracts patch targets and applies the same state
transition.  If it does emit both, processing remains idempotent.

### Separate cold-start and hot-reload verification

The acceptance suite has two distinct live workflows:

1. **Cold start:** a fresh Emacs loads the current build and successfully runs
   the Agent exit chain.
2. **Hot reload:** one long-lived Emacs loads the previous Agent build, the
   source is changed and rebuilt, the package is reloaded without restarting
   Emacs, runtime wiring is reconciled, and a newly started Codex child session
   runs both configured exit skills before its displayed buffer dies.

The hot-reload workflow proves source identity, artifact identity, daemon
identity, the reconciliation receipt, ordered skill turns in the Codex rollout,
and final buffer death.  A pass in a different Emacs process cannot substitute
for it.

## Failure handling

- **Daemon unreachable:** retain `delivery-failed` debt and report the bounded
  connection failure.
- **Build failed:** retain `build-failed` debt with the Elpaca error.
- **Reload or reconciliation failed:** retain `reload-failed` debt with the
  exact Emacs error.
- **Hook interrupted:** the already-written `pending` record remains and is
  treated as unresolved.
- **Process restarted:** stale receipts from the old PID cannot establish
  freshness for the new daemon.  Normal startup loading may create a new
  receipt after validating the build and wiring.
- **Duplicate events:** updates are idempotent by profile, package, and source
  identity.

No branch silently falls back to an older artifact or treats absence of
evidence as success.

## Expected implementation areas

### Dotfiles repository

- `codex/hooks/load-elisp-after-edit.sh`
- `claude/hooks/load-elisp-after-edit.sh`
- shared reload-state helpers under the paired hook trees
- Codex and Claude hook configuration and documentation
- `emacs/extras/elpaca-extras.el` and its Org manual
- reload-hook and Elpaca Extras tests
- verification guards that currently use session-local `/tmp` markers

### Agent repository

- `agent-codex.el` mode-owned wiring reconciliation
- `agent.el` session-start freshness gate
- focused ERT regressions and package documentation

The implementation remains split into logical commits per repository.  Existing
unrelated changes in either worktree are preserved and never staged.

## Non-goals

- Do not make long-running calls through the live Emacs server.
- Do not restart or signal the user's active Emacs daemon automatically.
- Do not make every Elisp package declare custom health checks immediately;
  provide a generic receipt plus an opt-in reconciliation check, and implement
  the Agent check required by this incident.
- Do not treat batch compilation or cold-start verification as active-daemon
  proof.
- Do not introduce an approval prompt as a failure gate.

## Acceptance criteria

- A failed initial enqueue request returns nonzero, produces a visible error,
  and leaves durable reload debt.
- An interrupted hook cannot erase pending debt.
- A matching daemon receipt is the only path that clears debt.
- A receipt from another daemon, profile, package, source identity, or artifact
  does not clear debt.
- Reloading Agent while `agent-codex-mode` is already enabled installs all
  required advice exactly once.
- `agent-start-session` refuses to start against unresolved or mismatched Agent
  runtime state while leaving recovery commands usable.
- Recorded direct and nested tool payloads produce identical reload-debt state.
- Existing Elisp hook, Elpaca Extras, Agent, compile, and documentation checks
  remain green.
- Cold-start and hot-reload end-to-end workflows both pass against the exact
  committed artifacts.
- The live `agent-exit` acceptance run records both configured skills in order
  and observes the session buffer die.
