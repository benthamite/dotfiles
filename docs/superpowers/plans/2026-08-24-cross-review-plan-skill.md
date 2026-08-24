# Cross-Review-Plan Skill Implementation Plan

> **Status:** revised after one independent cross-model review (2026-08-24).
> All seven findings were verified against the repository and accepted; the
> adjudication log at the end records each disposition. Self-contained; a
> reader needs no prior conversation context.

**Goal:** Add a paired Claude/Codex skill, `cross-review-plan`, that lets the
*current* agent session hand a plan to a fresh session of the *other* CLI
(Claude ↔ Codex) for one independent review pass, monitor that session, and
then implement the plan in the current session while adjudicating the
feedback — without duplicating the session machinery that already exists in
`orchestrate-agent-review`.

**Architecture:** Extract the single-actor Emacs/agent.el session primitives
(guarded prompt delivery, transcript-boundary acknowledgment, completion
markers, polling, bounded return reads) out of
`orchestrate_agent_review.py` into a shared top-level module,
`lib/python/agent_session_lib.py`, that both skills import.
`orchestrate-agent-review` keeps its two-actor run-file state machine and all
of its doctrine unchanged. The new skill adds a minimal single-phase run
state (one reviewer, one review) plus a thin SKILL.md.

**Tech stack:** Python 3 (stdlib only), Emacs `agent.el` via `emacsclient`,
paired Claude/Codex skills under `dotfiles/`, `ai-config-sync.json` pairing
manifest, `unittest`.

---

## Background: why a new skill instead of extending the existing one

`orchestrate-agent-review` coordinates **two** live Emacs sessions from a
third-party orchestrator that is forbidden from implementing anything. Its
20KB SKILL.md is mostly doctrine enforcing that contract: five fixed phases
(spec → spec-review → plan → plan-review → implementation), stage atomicity,
"never touch Agent 1's work". In the new workflow the current session **is**
the author and implementer, there is **one** external actor, **one** phase
(plan review), and no spec. Folding this in would put conditionals through
every hard rule and loosen the guard script's invariants (fixed two-actor run
file, phase ordering). The overlap worth reusing is mechanical, not doctrinal,
and lives in the Python script.

Existing constraints the implementation must respect:

- Claude and Codex skills are **peer duplicates**, not symlinks:
  `dotfiles/claude/skills/<name>/` and `dotfiles/codex/skills/<name>/` hold
  identical bodies (modulo frontmatter). `ai-config-sync.json` records each
  pair; hooks block one-sided commits.
- The existing script `orchestrate_agent_review.py` (1698 lines) is
  byte-identical between the two trees today.
- **The orchestrator already has a test suite**:
  `tests/test_orchestrate_agent_review.py` (1476 lines). It loads the Codex
  copy of the script as a module via `importlib.util.spec_from_file_location`
  and patches seams as attributes on that module object. The refactor must
  keep that loader working and give every moved function a single,
  deliberate patch point (see Task 1).
- `dotfiles/lib/` exists and is currently empty; the shared module gets a
  documented home there rather than a cross-skill sibling import.
- Any staged change under `claude/skills/` requires a matching
  `claude/README.org` update, enforced by
  `claude/hooks/require-readme-update.sh`. Documentation audits are
  `bin/docs-audit generate`, `bin/docs-audit audit`, and
  `bin/ai-config-sync audit`.

## Desired workflow (what the new SKILL.md encodes)

1. **Plan**: identify the plan to review. If the user has not written one,
   create it first using the repository's planning conventions. Either way
   the plan must be **committed** before the handoff; there is no
   inline-content path. The review is anchored to an immutable object:
   `init-review` validates the repository, verifies
   `git cat-file -e <commit>^{commit}`, resolves the repo-relative path,
   verifies `git cat-file -e <commit>:<path>`, and records the **blob hash**.
   The review prompt instructs the reviewer to read the plan via
   `git show <commit>:<path>`, never from the working tree.
2. **Reviewer session**: detect the current backend (Claude Code vs Codex)
   and start a fresh Emacs `agent.el` session of the **opposite** backend
   with an explicit instance name (no interactive name prompt). Claude
   session → Codex reviewer; Codex session → Claude reviewer. The reversal is
   automatic, never configured. **Identity invariant, enforced by
   `init-review`, not just prose**: the caller passes its own backend;
   initialization rejects a reviewer whose backend equals the caller's,
   a reviewer transcript that already has history (non-fresh session), a
   buffer whose configured transcript does not match the supplied transcript
   path, and a reviewer session whose working directory is not the plan's
   repository.
3. **One-pass review**: submit a single review prompt (reusing the existing
   plan-review prompt shape: concrete correctness gaps, missing verification,
   sequencing problems, scope contradictions; prioritized findings with
   reasons; explicitly *not* a request for a revised plan or another round),
   ending with a fixed completion-marker contract.
4. **Monitor**: poll with the Python watcher (never shell `sleep`; the
   reviewed agent may run broad `pkill` probes). Delivery is acknowledged
   only by busy transition or transcript growth; ambiguous delivery freezes
   the run until reconciled — same semantics as the existing skill, reduced
   to one phase. **Reviewer process loss**: if the reviewer process dies
   after accepting the prompt but before producing any assistant output,
   `restart-review` permits exactly one guarded restart — it requires proof
   of zero assistant output after the recorded transcript boundary, starts a
   fresh session of the same reviewer backend, re-submits the same prompt
   once, and atomically rebinds the run to the new transcript. It refuses to
   run after any reviewer output and refuses a second restart.
   **Markerless return**: a reviewer that returns without the completion
   marker is a *terminal incomplete review* — record the bounded return,
   report it to the user, and never re-contact or steer the reviewer.
5. **Adjudicate and implement**: when the reviewer returns with the marker,
   read the bounded return, then implement the plan **in the current
   session**. Valid findings must change the plan or the implementation;
   rejected findings get a concise recorded reason. The reviewer is never
   asked to re-review or adjudicate rejections.
6. **Report**: plan commit + blob hash, reviewer instance/transcript,
   findings summary (accepted/rejected with reasons), implementation result.
   The reviewer session is left open for inspection and named in the report;
   it is never killed automatically.

## File map

Shared module extraction (behavior-preserving refactor):

- Create: `lib/python/agent_session_lib.py` — moved from
  `orchestrate_agent_review.py`: `EmacsClientError`, `_write_all`,
  `run_emacs_eval`, `elisp_string`, `run_emacs_json`, `json_for_display`,
  `buffer_state`, `_submit_function`, `send_return_to_agent`,
  `pending_prompt_contains`, `agent_transcript_path`, `_user_marker_offset`,
  `_wait_for_transcript_path`, `_transcript_advanced`, `_delivery_observed`,
  `_wait_for_delivery`, `submit_to_agent`, `transcript_messages`, the
  one-shot mode-0600 temp-file status transfer, **and the constants those
  functions depend on**: `VALID_BACKENDS`, `DELIVERY_INITIAL_WAIT_SECONDS`,
  `DELIVERY_RETRY_WAIT_SECONDS`, `DELIVERY_POLL_SECONDS` (plus any further
  dependency surfaced while extracting — the boundary is "everything a moved
  function references", established by running the full suite, not by
  `py_compile`, which cannot catch a missing module-level name).
- Modify: `claude/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`
  — locate `lib/python` relative to the script's **resolved** path (the
  live `~/.claude/skills` / `~/.codex/skills` entries are symlinks into
  `dotfiles/`), then `import agent_session_lib as session` and call every
  moved function as `session.f(...)`. This gives each moved function exactly
  one patch point (`module.session.f`). No behavior change.
- Modify: `tests/test_orchestrate_agent_review.py` — keep the
  file-location loader; update every monkeypatched seam that moved so it
  patches `orchestrator.session.<name>` instead of `orchestrator.<name>`.
  Assertions and behavior expectations stay untouched; if any test patched a
  seam that lib-internal callers now bypass, redesign that seam explicitly
  (indirection through `session`) rather than silently losing coverage.
- Mirror the script change in
  `codex/skills/orchestrate-agent-review/scripts/`.

New skill:

- Create: `claude/skills/cross-review-plan/SKILL.md`
- Create: `claude/skills/cross-review-plan/scripts/cross_review_plan.py` —
  subcommands: `init-review` (guarded mode-0600 review file: repo, plan
  path + commit + **blob hash**, caller backend, reviewer
  buffer/backend/transcript with the identity invariant above, marker,
  transcript offset), `submit-review`, `status`, `watch`, `finish-review`
  (marker-gated, bounded transcript read), `restart-review` (single
  zero-output process-loss recovery, defined in workflow step 4),
  `reconcile-submission` (`--delivered` / `--not-delivered`),
  `retry-delivery` (Return-only, never retransmits). Imports
  `agent_session_lib` from `lib/python` by resolved relative path.
- Create: `tests/test_cross_review_plan.py` (top-level `tests/`, beside the
  orchestrator suite, same loader pattern): review-file state transitions,
  identity-invariant rejections, double-submit rejection, premature-finish
  rejection, markerless-terminal handling, restart-review guards (zero
  output proven, single use), reconcile paths — fake transcripts, stubbed
  `session` seams.
- Create: `tests/test_agent_session_lib.py` — first direct tests for the
  moved pure/parseable parts (marker offsets, transcript-advance detection,
  elisp string escaping, transcript message parsing) with fixture
  transcripts.
- Mirror the whole skill in `codex/skills/cross-review-plan/`.

Manifest and docs:

- Modify: `ai-config-sync.json` — add the `cross-review-plan` pair entry;
  confirm the `orchestrate-agent-review` entry's equivalence note still
  holds after the refactor (auxiliary files must match).
- Modify: `claude/README.org` — tool overviews for the new skill and the
  shared `lib/python/agent_session_lib.py` module (hook-enforced), and the
  Codex-side counterpart documentation per the sync policy.
- Regenerate: `agents/skill-inventory.org` per the sync policy.

## Tasks

- [ ] 1. Map the existing test suite's patch seams (every
  `orchestrator.<name>` attribute it replaces), then extract
  `lib/python/agent_session_lib.py` — moved functions **and their
  constants** — and refactor `orchestrate_agent_review.py` to call through
  `session.<name>`. Update the suite's moved seams to
  `orchestrator.session.<name>`. Verify: full
  `tests/test_orchestrate_agent_review.py` suite passes; every subcommand's
  `--help` runs; `init-run` + `status` golden-output comparison against a
  synthetic run file.
- [ ] 2. Write `tests/test_agent_session_lib.py` for the extracted module's
  pure functions with fixture transcripts.
- [ ] 3. Write `cross_review_plan.py` with the single-phase guarded state
  machine, including `restart-review` and the init-time identity invariant,
  reusing `session` for all Emacs operations. Same safety semantics as the
  parent skill: mode-0600 files, pending-record before external submission,
  no automatic retransmission, marker-gated completion, transcript text
  never overrides a busy lifecycle state.
- [ ] 4. Write `tests/test_cross_review_plan.py` (coverage listed in the
  file map).
- [ ] 5. Write `SKILL.md`: workflow above, backend detection, explicit
  instance names, the one-pass review prompt template (reading the plan via
  `git show <commit>:<path>`), adjudication rules (borrowed verbatim where
  they apply from `orchestrate-agent-review`), markerless-terminal rule,
  single-restart rule, stop conditions (user-only blockers → report), and a
  final-report checklist.
- [ ] 6. Mirror everything into the Codex tree; add the manifest entry;
  update `claude/README.org` and the Codex counterpart docs; regenerate the
  skill inventory; run `bin/docs-audit generate`, `bin/docs-audit audit`,
  and `bin/ai-config-sync audit`; commit as one logical change (hooks
  require both sides, README, and manifest together).
- [ ] 7. Verification beyond the suites:
  - One **complete parent-skill phase** live post-refactor (not just
    `init-run`): a real spec or plan phase through submission,
    acknowledgment, and marker-gated `finish-phase`, exercising the moved
    delivery path end-to-end.
  - **Two live cross-review runs, one per direction** (Claude-initiated with
    Codex reviewer; Codex-initiated with Claude reviewer), each carried
    through to the advertised outcome: at least one accepted finding that
    changes the artifact, at least one rejected finding with a recorded
    reason, the implementation step, its verification, and the final report.
  - Until both directions have run live, the skill is "implemented, not yet
    verified end-to-end" — never "done".

## Explicitly out of scope

- Any change to `orchestrate-agent-review` behavior, prompts, or doctrine.
- Multi-round review, implementation review, or reviewer adjudication of
  rejected findings.
- Non-Emacs session transports (tmux, headless CLI). agent.el is the only
  session substrate, as in the parent skill.

## Adjudication log (review of 2026-08-24)

All seven findings verified and accepted:

1. **Existing test suite missed** (High) — accepted; plan was wrong.
   Task 1 now maps and preserves/redesigns the suite's patch seams and gates
   the refactor on the full suite, not `py_compile`.
2. **`restart-review` missing** (High) — accepted. Added as a guarded
   single-use, zero-output-only recovery subcommand.
3. **Extraction boundary incomplete** (High) — accepted. Constants
   (`VALID_BACKENDS`, `DELIVERY_*`) move with their dependents; boundary is
   closed by the test suite, not compilation.
4. **Immutability not guaranteed** (High) — accepted. `init-review`
   validates `commit^{commit}` and `commit:path`, records the blob hash, and
   the reviewer reads `git show <commit>:<path>`.
5. **Verification too shallow** (High) — accepted. Task 7 now requires one
   complete parent phase and two full cross-direction runs including
   adjudication, implementation, and final report.
6. **Identity invariant unenforced** (Medium) — accepted. `init-review`
   takes the caller backend and rejects same-backend, non-fresh, mismatched,
   or wrong-directory reviewers.
7. **Documentation obligations omitted** (Medium) — accepted. Task 6 adds
   `claude/README.org`, the Codex counterpart, and the three audit commands.

Reviewer's open-question recommendations, all adopted: shared module in
documented top-level `lib/python/` (the sibling-skill import is dropped;
`dotfiles/lib/` exists, correcting this plan's earlier claim); Git-backed
committed plan required with no inline fallback; markerless reviewer output
is a terminal incomplete review with no further contact; exactly one restart,
only for zero-output process death. One nuance kept from the original plan:
the skill may still *author and commit* a plan when none exists (the
handoff itself always requires the committed artifact), and the reviewer
session is left open after the run.
