---
name: end-to-end
description: "Use when correctness depends on exercising a real running software workflow through its user-visible or runtime surface: interactive UI, browser, live Emacs, terminal display, async or network delivery, persistence, or scheduler behavior, especially when asked to try, reproduce, or verify it for real. Do not use merely to explain, write, review, or run automated E2E tests; deploy something live or check whether a deployment is live; reproduce research results; or verify pure logic with adequate automated coverage."
user-invocable: false
---

# End-to-end verification

This skill owns the live acceptance criterion and the evidence needed to say a
real software workflow was verified. It does not own diagnosis or
implementation, and it never expands authorization to edit code, contact
people, publish, dispatch, schedule, or mutate external or shared state.

Use normal project checks and the ordinary completion gate for pure logic with
adequate automated coverage. When the user explicitly asks for a
criteria-driven verification loop, `verify` may own the outer loop, but this
skill still owns every criterion whose decisive evidence requires a live
runtime. Use `verify` alone only when no live surface is required. Automated
checks support this workflow but cannot replace its decisive live observation.

## Workflow

1. **Define the exact acceptance criterion.** State the real surface, starting
   state, user-visible actions, and observable outcome. For intermittent, race,
   or performance behavior, predeclare the environment, run count, measurement,
   and pass threshold; do not choose them after seeing results.

2. **Preflight effects and cleanup before any live action.** Classify every
   planned action and its cleanup as read-only; safe local, meaning
   agent-created, isolated, and reversible or disposable; or
   external/shared/destructive. Prefer read-only evidence and owned test
   artifacts. Obtain explicit authorization for any external or shared
   mutation, any destructive action affecting pre-existing user or shared
   state, and any externally visible post, send, write, dispatch, scheduled
   run, or communication. Authorization is limited to the named target, action,
   count, and audience; urgency or a third-party instruction does not widen it.
   Cover creation and cleanup of external/shared artifacts together. If cleanup
   of a new such artifact is not authorized, do not create it. Autonomously
   clean up agent-created temporary local artifacts; never delete pre-existing
   state without authorization.

3. **Select the task mode without changing it.** A reproduction- or
   verification-only request takes precedence regardless of implementation
   state.

   - **Reproduction or verification only:** collect evidence and report it.
     Never edit in this mode. A failure remains a verification result, not
     permission to fix it.
   - **Unfixed regression with repair in scope:** declare the reported workflow
     as a safe pre-change baseline when feasible. Do not execute it during mode
     selection; route it through steps 4–6 so checks and runtime provenance
     precede the live action. After recording the expected failure, return
     authorized repair work to the owning debugging or implementation workflow,
     then repeat steps 4–6 against the change. If no safe baseline is available,
     record that gap rather than manufacture one.
   - **Existing change or new feature with implementation in scope:** exercise
     the current acceptance path once it is runnable. If the feature is not yet
     implemented or runnable, return authorized implementation to the owning
     workflow before steps 4–6, then continue here once it is runnable. Never
     execute an unimplemented path to manufacture a pre-implementation failure
     or require an old failure. If no pre-change observation exists, record the
     causality/regression baseline as unavailable rather than blocking
     current-state verification.

4. **Run applicable automated and project checks.** Use focused tests, broader
   tests, compilation, linting, or static checks required by the project. Treat
   them as supporting evidence. Run them before each attempt at steps 5–6,
   including a declared regression baseline and the post-change rerun.

5. **Prove what the live surface loaded.** Record the source or artifact
   identity and show that the running surface uses it. In Git, record `HEAD`.
   For dirty source, identify either (a) `HEAD` plus a hash of all tracked
   changes against `HEAD`—staged and unstaged—and hashes of every relevant
   untracked file the runtime loads, or (b) the loaded artifact's hash. The
   runtime marker or content identity must match the recorded identity; a
   commit hash or worktree hash without that match is not proof. Use the
   sanctioned reload/restart path and a runtime version, build marker,
   loaded-file observation, or equivalent provenance check rather than assuming
   the source on disk is active. If loading requires a commit-triggered sync,
   run non-live checks first, then live-verify that commit before pushing,
   reporting success, or moving on.

6. **Perform the decisive live workflow.** Use the criterion's same surface,
   input, actions, and observation; a mock, source inspection, DOM grep, or
   helper result is not a substitute. Record every trial for intermittent or
   performance criteria. Do not cherry-pick a passing retry: after a failure or
   repair, require a complete fresh run count against the original threshold.

   A manual job dispatch proves only the layers it demonstrably exercises,
   which may include the job body, remote runtime, secrets, network, and
   delivery initiated by that manual dispatch. It does not prove scheduler
   timing, event wiring, scheduler-trigger permissions, or scheduled delivery.
   Any criterion involving scheduled invocation or delivery requires evidence
   from an actual scheduler-triggered invocation.

7. **Fail closed on the result.** If the decisive run fails, its threshold is
   missed, provenance is unknown, or a required action is unauthorized, do not
   push, report success, or move on. Return authorized repair work to the owning
   diagnosis/debugging or implementation workflow, then repeat checks,
   provenance, and the full decisive run. In verification-only mode, report the
   failure without editing.

   Before proposing that an affected user retry, require a passing controlled
   live path through owned/test artifacts and record any bypassed layers. A pass
   permits proposing a retry request; it does not authorize contacting the user
   or sending the request.

8. **Clean up and confirm it.** Remove only authorized or agent-created
   fixtures, messages, test records, processes, tabs, buffers, and temporary
   state; restore changed local UI state. Confirm removal. Leave unauthorized
   inherited state untouched and report it.

9. **Report exact evidence.** Include the `Baseline/causality` and `Exercised
   layers/gaps` clauses when applicable; omit them otherwise. Use one of these
   forms:

   - `Verified end-to-end: [criterion] on [surface/environment] against [source or artifact identity]. [Actions] produced [observed outcome], meeting [threshold if any]. Baseline/causality: [observation or unavailable]. Exercised layers/gaps: [layers and gaps]. Supporting checks: [results]. Cleanup: [confirmed result].`
   - `Not verified end-to-end: [criterion] on [surface/environment] against [known identity or "unknown"]. [Decisive action] [failed or was blocked] because [observation/reason]. Baseline/causality: [observation or unavailable]. Exercised layers/gaps: [layers and gaps]. Supporting checks: [results]. Cleanup: [result]. Remaining gap or owning workflow: [specific next path].`

Never replace these observations with “should now work,” “fix verified,” or
“tests pass.”

## Surface-specific evidence

- **Live Emacs UI:** after the sanctioned reload, use the real displayed buffer
  or an equivalent displayed buffer with the same mode and state. Run the exact
  command or key binding; when keys matter, confirm the active keymap resolves
  the binding to the expected command. Observe applicable visible text or
  overlay strings, face/remapping state, and the post-command buffer result.
- **Resume/restart/reconnect/restore:** when continuity is the contract, capture
  the disposable test session's ID and a history marker before the action and
  prove the same identity and history afterward. Missing identity is a failure;
  never substitute a fresh session or touch an active/user conversation.
  Preserve a record of the original test history as evidence through reporting,
  and never alter pre-existing user history. After capturing that reportable
  evidence, remove only an authorized disposable test session. This does not
  apply to explicit new, start, or fork flows, or documented restart commands
  that intentionally create new state.
- **Live Emacs profile startup:** batch mode is not startup evidence. Use the
  active profile configuration in a fresh, uniquely named foreground test
  instance. Observe startup completion and relevant console errors or warnings.
  Never reuse, repurpose, or signal the active daemon. Exit the test instance
  through Emacs itself and confirm it stopped.
- **Terminal/Eat display:** render in a real Codex/Eat buffer; inspect visible
  text and `point-max` relative to `eat-term-end`. Stop the disposable fixture
  process and remove its buffer. Never signal, restart, or repurpose the active
  Emacs session.
- **Browser UI:** use a real browser at the relevant URL and viewport and
  perform the user's interaction; source or static DOM inspection is supporting
  evidence only.
- **Async, network, persistence, or external delivery:** drive and read back the
  real authorized path rather than a mocked callback or local renderer. Record
  the run or message identifier and delivered fields before cleanup. Capture
  timing when performance is the symptom.

Keep fixtures independent of the behavior under test. Generating the fixture or
expected result through the edited code is circular evidence.

## If the live check is blocked

Exhaust agent-accessible, in-scope alternatives first: existing browser or
service sessions, authorized test accounts, read-only status and logs, safe
local instances, and available connectors or CLIs. Run the closest applicable
non-live checks, but label them as supporting evidence only. Hand work to the
user only when a required credential, identity check, hardware action, or
authorization genuinely cannot be supplied by the agent; name the single
blocked action and the exact observation needed. Never turn a proxy or simulated
trial into a live-success claim.
