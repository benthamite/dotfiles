---
name: request-review
description: Use when the current agent session should hand a committed plan to a fresh session of the other CLI (Claude ↔ Codex) for one independent review pass, monitor it, and then implement the plan here while adjudicating the feedback.
---

# request-review

## Overview

Run one cross-model plan review from inside the authoring session. The current
session authors (or already has) a committed plan, hands it to a fresh Emacs
`agent.el` session of the **opposite backend** for exactly one review pass,
monitors that session, and then implements the plan **in this session** while
adjudicating every finding. The helper guards the reviewer half; authoring,
adjudication, and implementation stay entirely in the current session.

This is not `orchestrate-review`: there is no second author session, no
spec phase, and no stage handoff. Use that skill when a separate session
should own authoring and implementation; use this one when *you* are the
author and implementer and only the review crosses models.

## Role contract

- The current session creates the plan (when none exists), commits it,
  adjudicates the review findings, and implements.
- The reviewer is a fresh session of the opposite backend: a Claude Code
  session always uses a Codex reviewer, and a Codex session always uses a
  Claude reviewer. The reversal is automatic, never configured.
- The reviewer reviews the plan exactly once. The review is a one-way
  handoff: the reviewer is never asked to re-review, never sent the revised
  artifact, and never asked to adjudicate a rejected finding.
- Valid findings must change the plan or the implementation; rejected
  findings get a concise recorded reason.

Use the helper for every deterministic session operation:

```bash
python "$SKILL_DIR/scripts/request_review.py" --help
```

## Step 1: Commit the plan

Identify the plan to review. If the user has not written one, create it first
using the repository's planning conventions. Either way, the plan must be a
**committed file in a Git repository** before the handoff — there is no
inline-content path. The helper anchors the review to the exact committed
blob and makes the reviewer read it with `git show <commit>:<path>`, never
from the working tree.

## Step 2: Start the opposite-backend reviewer session

Detect the current backend, then start a fresh session of the opposite one
with an explicit instance name (never a command that prompts for a name):

```elisp
(let ((default-directory "/path/to/repo/"))
  (agent-start-session
   (agent-session-create
    :backend 'codex   ; 'claude-code when the current session is Codex
    :account (agent-account-resolve 'codex t)
    :directory default-directory
    :instance "plan-review-codex")))
```

Identify the new session's buffer name and transcript path, then create the
guarded mode-`0600` run file outside the repo or under an ignored state
directory:

```bash
python "$SKILL_DIR/scripts/request_review.py" init-review \
  --run-file /tmp/plan-review-run.json \
  --repo /path/to/repo \
  --plan-path docs/plans/the-plan.md \
  --plan-commit abc1234 \
  --caller-backend claude-code \
  --reviewer-buffer '*codex:plan-review-codex*' \
  --reviewer-backend codex \
  --reviewer-transcript /path/to/codex.jsonl
```

`init-review` enforces the identity invariant, not just this prose: it
rejects a reviewer whose backend equals the caller's, a reviewer transcript
that already has history, a buffer whose configured transcript does not match
the supplied path, and a reviewer session whose working directory is outside
the plan's repository. It also validates the commit and the committed plan
path and records the blob hash.

A fresh Codex reviewer has no transcript file until the first delivery (the
rollout filename embeds an unpredictable timestamp), so for a Codex reviewer
pass the expected-but-not-yet-existing path (or a placeholder in the run
directory). After delivery, `submit-review` discovers the real transcript
and rebinds to it only when it provably contains this run's marker-bearing
prompt. Claude reviewers report their transcript upfront via the status
file, so supply the real path.

## Step 3: Submit the single review pass

```bash
python "$SKILL_DIR/scripts/request_review.py" submit-review \
  --run-file /tmp/plan-review-run.json
```

The helper builds the full review prompt itself: the `git show` anchor, the
one-pass review instructions (concrete correctness gaps, missing
verification, sequencing problems, scope contradictions; prioritized findings
with reasons; no revised-plan requests), and a fixed completion-marker
contract. Pass `--context-file` only for extra reviewer context that the
plan itself cannot carry.

Delivery follows the parent skill's semantics: a pending record is persisted
before the external submission; delivery is acknowledged only by a busy
transition or transcript growth; an ambiguous outcome freezes the run until
`reconcile-submission --delivered` or `--not-delivered` is justified by
concrete session evidence; `retry-delivery` sends only Return and only when
the exact marker is still in the composer. Never retransmit a prompt.

## Step 4: Monitor without ending the turn

Use the Python watcher, never shell `sleep` (the reviewed agent may run broad
process probes such as `pkill -f "sleep 20"`):

```bash
python "$SKILL_DIR/scripts/request_review.py" watch \
  --run-file /tmp/plan-review-run.json --interval 20
```

One line prints per state change. While the reviewer is busy, leave it alone:
no liveness checks, no continuation prompts, no second contact of any kind.

If the reviewer **process dies** after accepting the prompt but before
producing any assistant output, start a fresh session of the same reviewer
backend and run `restart-review` — it proves zero assistant output after the
recorded boundary, re-submits the same review once, rebinds the run to the
fresh transcript, and permits exactly one restart per run. Any other failure
is reported to the user, not retried.

## Step 5: Record the return

When the reviewer is authoritatively awaiting input:

```bash
python "$SKILL_DIR/scripts/request_review.py" finish-review \
  --run-file /tmp/plan-review-run.json
```

The command reads only bytes appended after the current submission and prints
the bounded reviewer return. A return ending with the exact completion marker
completes the review. A return **without** the marker is a **terminal
incomplete review**: the helper records it, the run permits no further
reviewer contact, and you report the partial return to the user. Do not steer
the reviewer and do not restart after any output.

## Step 6: Adjudicate and implement — in this session

Work through every finding here, in the current session:

- Verify each factual claim against the repository before accepting it.
- A valid finding changes the plan or the implementation. Update and commit
  the revised plan with an adjudication record.
- A rejected finding gets a concise recorded reason. Do not ask the reviewer
  to adjudicate the rejection.
- Then implement the plan and verify the result, per the plan's own
  verification section.

Once `finish-review` has recorded the return, kill the reviewer session
(`agent--force-kill-buffer` on its buffer, or `agent-kill-session-buffer`
from inside it). The run file and the reviewer transcript path are the
inspection record; an idle session is not. Name the buffer and transcript
in the final report.

## Stop conditions

Stop and report a blocker when:

- the reviewer session is awaiting input but the review cannot be submitted
- the single restart was already used and the reviewer died again
- the review returned terminal-incomplete
- a finding exposes a decision only the user can make (credentials,
  irreversible actions, spending, underdetermined product choices)

## Final report

When complete, report:

- plan path, commit, and blob hash reviewed
- reviewer backend, instance/buffer name, and transcript path
- findings summary: accepted (and what changed) vs rejected (and why)
- implementation result and its verification evidence
- whether the working tree is clean
