---
name: end-to-end
description: Run end-to-end verification when the user has explicitly asked for it, or the change touches a surface whose only meaningful test is the real user workflow (live UI, async/network, terminal display, browser, Slack/Sheets, scheduled job). Use when the user says "end to end", "E2E", "real test", "live", "actually try it", "in a real session", "reproduce it", or after fixes to interactive UI / async / live-Emacs / scheduled-output code paths. Do not use for small refactors with full unit coverage and no live surface.
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

1. **State the success criterion in plain words.** What exact sequence of user-visible actions must produce what exact user-visible outcome? Write it down before designing the test.
2. **Reproduce the failure end to end FIRST.** Before changing any code, reproduce the exact user-reported symptom in the same surface the user uses (live Emacs buffer, real browser tab, the actual scheduled run, a fixture posted to a preview channel). If reproduction fails, stop and report what was tried and why; do not proceed to a fix on speculation.
3. **Implement the fix.**
4. **Re-run the same end-to-end reproduction.** Same surface, same input, same observation. Unit tests remain useful as supporting evidence; they do not replace this step.
5. **Report verbatim what was verified.** Explicitly name the surface and the observed outcome. Forbidden phrasings: "should now work", "fix verified" without saying through what surface, "tests pass" as the sole evidence for an interactive bug.

## Reproduction surfaces (defaults)

- **Live Emacs UI bug**: open a real Emacs session, run the exact command or binding the user invoked, observe the buffer state. Don't stop at `(fboundp ...)` — check the visible effect.
- **Async / network code**: drive the real async path, not a mocked callback. Capture timing if performance is the symptom.
- **Terminal / Eat display**: render in a real Codex/Eat buffer and inspect both `point-max - eat-term-end` and the visible text.
- **Browser-rendered UI**: load the page in a real browser at the relevant viewport size; static HTML grep is not enough.
- **Slack / Sheets / external-system output**: post a fixture to a preview channel or read back the live document, not only the local renderer.
- **Scheduled job / CI / cron**: trigger the real schedule (or a manual dispatch of the same workflow), not a local script invocation that bypasses the scheduler harness.

## When E2E is not possible

Sometimes the live surface cannot be exercised in this session — secrets unavailable, destructive workflow, conditions you cannot create. In that case:

1. State exactly which step is blocked and why.
2. Run the closest local proxy (unit + integration tests) and say so explicitly.
3. Recommend the smallest hand-off step the user can take (command X, observed state Y).

Never substitute the local proxy and report success without naming the gap.

## Anti-patterns

- "I verified the helper function works" when the user asked about a UI command.
- "Tests pass" when the symptom was a perceptible latency or freeze.
- "Code is correct" — correctness is necessary but not sufficient; the question is whether the user-visible outcome is right.
- Mocked Emacs/Slack/network checks treated as equivalent to real ones.
- Synthetic fixtures rendered with the same code path being tested, so the fixture passes because the test uses the fix (circular).
