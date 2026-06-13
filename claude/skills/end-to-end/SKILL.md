---
name: end-to-end
description: Run end-to-end verification when the user has explicitly asked for it, or the change touches a surface whose only meaningful test is the real user workflow (live UI, async/network, terminal display, browser, Slack/Sheets, scheduled job). Use when the user says "end to end", "E2E", "real test", "live", "actually try it", "in a real session", "reproduce it", or after fixes to interactive UI / async / live-Emacs / scheduled-output code paths. Do not use for small refactors with full unit coverage and no live surface.
user-invocable: false
---

# End-to-end verification

Stops a recurring failure: a bug is declared fixed after passing unit tests, byte-compilation, or helper-shape checks while the real workflow the user reproduces still fails. Treat unit tests as necessary but not sufficient evidence here.

## When this applies

Invoke if any is true:

- The user explicitly asked for end-to-end verification (e.g. "test end to end", "really try it", "in a real session", "reproduce it for real").
- The change touches interactive UI behavior, async/network code, live-Emacs state, terminal display, browser rendering, scheduled job output, or any surface whose correctness depends on running the actual workflow.
- An earlier fix in this session passed unit tests but the user reported the same symptom still present.

Skip if the change is a refactor or pure-logic fix with full unit coverage and no live component. Use the broader `verify` skill there, or no skill at all.

## Workflow

1. **State the success criterion in plain words.** What exact sequence of user-visible actions must produce what exact user-visible outcome? Write it down before designing the test. For interactive Emacs UI changes, acceptance criteria should explicitly name whichever contract dimensions are relevant: visible text or overlay strings, face or remapping state, the active key binding and command it resolves to, and the post-command result produced by the actual user action. For commands whose user-visible contract is to resume, restart, reconnect, or restore an existing session, the success criterion and any regression coverage must assert reuse of the original identity/history: capture the pre-action session ID, thread, buffer, or history marker; verify the post-action surface reuses it; and treat missing required identity as a failure that stops before any kill/start action instead of silently creating fresh state. Do not apply this to explicit `new`, `start`, or `fork` flows, or to domain-specific restart commands whose documented contract legitimately creates fresh state.
2. **Reproduce the failure end to end FIRST.** Before changing any code, reproduce the exact user-reported symptom in the same surface the user uses (live Emacs buffer, real browser tab, the actual scheduled run, a fixture posted to a preview channel). If reproduction fails, stop and report what was tried and why; do not proceed to a fix on speculation.
3. **Implement the fix.**
4. **Run the edited code in the user-visible workflow before committing when feasible.** For interactive UI, live Emacs, and network fixes, re-run the exact user-visible workflow against the edited code before `git commit` whenever the edited code can be loaded safely into that surface. If the architecture requires a commit-triggered sync or reload before the running app can see the edited code, commit only after non-live checks pass, then immediately live-verify before pushing, reporting success, or moving on.
5. **Re-run the same end-to-end reproduction.** Same surface, same input, same observation. Unit tests remain useful as supporting evidence; they do not replace this step. For Slack, network, or other external-system workflows, use a read-only preview, clearly labeled opt-in test path, or explicit confirmation before any externally visible mutation.
6. **Gate retry requests to affected users on controlled live evidence.** Before drafting, inserting, or sending language that asks an affected user to "please retry", "try again", or otherwise test the fix again, run the closest controlled live check feasible without relying on that user: owned/test artifacts, synthetic signed events, preview channels, dry-run or test-mode dispatches, KV/log checks, read-backs, or an equivalent opt-in path. Prefer owned/test artifacts. Preserve explicit approval requirements for any synthetic check that creates real Slack, Asana, GitHub, email, calendar, or other external side effects. If the controlled check bypasses a layer, state the exercised layers and the gap. Ask the affected user to retry only after the controlled path passes, or when no controlled path is possible and the blocker or gap is explicit.
7. **Report verbatim what was verified.** Explicitly name the surface and the observed outcome. Forbidden phrasings: "should now work", "fix verified" without saying through what surface, "tests pass" as the sole evidence for an interactive bug.

## Reproduction surfaces (defaults)

- **Live Emacs UI bug**: after the sanctioned reload path, open the real displayed buffer or an equivalent displayed buffer with the same mode and state. Run the exact command or key binding the user invoked; when key behavior matters, confirm the active keymap resolves that binding to the expected command. Observe the relevant visible text or overlay strings, face/remapping state, and post-command buffer result. Helper/temp-buffer checks are supporting evidence only; don't stop at `(fboundp ...)` or helper output without checking the visible effect.
- **Async / network code**: drive the real async path, not a mocked callback. Capture timing if performance is the symptom.
- **Terminal / Eat display**: render in a real Codex/Eat buffer and inspect both `point-max - eat-term-end` and the visible text.
- **Browser-rendered UI**: load the page in a real browser at the relevant viewport size; static HTML grep is not enough.
- **Slack / Sheets / external-system output**: post a fixture to a preview channel or read back the live document, not only the local renderer.
- **Affected-user retry requests**: before asking the affected user to retry or
  test again, exercise the closest controlled live path first. Synthetic events
  and owned/test artifacts are preferred; any check that creates real external
  side effects still needs explicit approval.
- **Scheduled job / CI / cron**: trigger the real schedule (or a manual dispatch of the same workflow), not a local script invocation that bypasses the scheduler harness.
- **Secret-backed remote notifications**: when credentials live only in CI or
  another remote secret store, prefer an explicit opt-in test-alert/manual
  dispatch path that exercises the real remote credentials and delivery channel
  without mutating production state. Label the message clearly, keep the
  audience minimal, and respect external-action confirmation rules.

## When E2E is not possible

Sometimes the live surface cannot be exercised in this session — secrets unavailable, destructive workflow, conditions you cannot create. In that case:

1. State exactly which step is blocked and why.
2. Run the closest local proxy (unit + integration tests) and say so explicitly.
3. Recommend the smallest hand-off step the user can take (command X, observed state Y).

Never substitute the local proxy and report success without naming the gap. Preserve blocked live verification reporting in final messages: use `Not verified end-to-end:` or explicitly name the blocked live verification step instead of implying the user-visible workflow was exercised.

## Anti-patterns

- "I verified the helper function works" when the user asked about a UI command.
- "Tests pass" when the symptom was a perceptible latency or freeze.
- "Code is correct" — correctness is necessary but not sufficient; the question is whether the user-visible outcome is right.
- Mocked Emacs/Slack/network checks treated as equivalent to real ones.
- Synthetic fixtures rendered with the same code path being tested, so the fixture passes because the test uses the fix (circular).
