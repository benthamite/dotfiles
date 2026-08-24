# Cross-Review-Plan Skill Implementation Plan

> **Status:** draft for independent review. Self-contained; the reviewer needs
> no prior conversation context.

**Goal:** Add a paired Claude/Codex skill, `cross-review-plan`, that lets the
*current* agent session hand a plan to a fresh session of the *other* CLI
(Claude ↔ Codex) for one independent review pass, monitor that session, and
then implement the plan in the current session while adjudicating the
feedback — without duplicating the session machinery that already exists in
`orchestrate-agent-review`.

**Architecture:** Extract the single-actor Emacs/agent.el session primitives
(guarded prompt delivery, transcript-boundary acknowledgment, completion
markers, polling, bounded return reads) out of
`orchestrate_agent_review.py` into a shared module that both skills import.
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

Existing pairing constraints that the implementation must respect:

- Claude and Codex skills are **peer duplicates**, not symlinks:
  `dotfiles/claude/skills/<name>/` and `dotfiles/codex/skills/<name>/` hold
  identical bodies (modulo frontmatter). `ai-config-sync.json` records each
  pair; hooks block one-sided commits.
- The existing script `orchestrate_agent_review.py` (1698 lines) is
  byte-identical between the two trees today.
- There is no shared Python library location currently used by skills;
  `dotfiles/lib/` does not exist and no skill imports across skill
  directories today.

## Desired workflow (what the new SKILL.md encodes)

1. **Plan**: identify the plan to review. If the user has not written one,
   create it first using the repository's planning conventions, and commit
   it. The review handoff references the committed path + commit hash, never
   pasted content, so the review is anchored to an immutable artifact.
2. **Reviewer session**: detect the current backend (Claude Code vs Codex)
   and start a fresh Emacs `agent.el` session of the **opposite** backend
   with an explicit instance name (no interactive name prompt). Claude
   session → Codex reviewer; Codex session → Claude reviewer. The reversal is
   automatic, never configured.
3. **One-pass review**: submit a single review prompt (reusing the existing
   plan-review prompt shape: concrete correctness gaps, missing verification,
   sequencing problems, scope contradictions; prioritized findings with
   reasons; explicitly *not* a request for a revised plan or another round),
   ending with a fixed completion-marker contract.
4. **Monitor**: poll with the Python watcher (never shell `sleep`; the
   reviewed agent may run broad `pkill` probes). Delivery is acknowledged
   only by busy transition or transcript growth; ambiguous delivery freezes
   the run until reconciled — same semantics as the existing skill, reduced
   to one phase.
5. **Adjudicate and implement**: when the reviewer returns with the marker,
   read the bounded return, then implement the plan **in the current
   session**. Valid findings must change the plan or the implementation;
   rejected findings get a concise recorded reason. The reviewer is never
   asked to re-review or adjudicate rejections.
6. **Report**: plan commit, reviewer instance/transcript, findings summary
   (accepted/rejected with reasons), implementation result.

## File map

Shared module extraction (behavior-preserving refactor):

- Create: `claude/skills/orchestrate-agent-review/scripts/agent_session_lib.py`
  — moved verbatim from `orchestrate_agent_review.py`: `EmacsClientError`,
  `_write_all`, `run_emacs_eval`, `elisp_string`, `run_emacs_json`,
  `json_for_display`, `buffer_state`, `_submit_function`,
  `send_return_to_agent`, `pending_prompt_contains`,
  `agent_transcript_path`, `_user_marker_offset`,
  `_wait_for_transcript_path`, `_transcript_advanced`, `_delivery_observed`,
  `_wait_for_delivery`, `submit_to_agent`, `transcript_messages`, and the
  one-shot mode-0600 temp-file status transfer. Nothing two-actor moves: the
  run-file schema, phase ordering, steer/adopt/restart logic all stay put.
- Modify: `claude/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`
  — import the moved names from `agent_session_lib`; no behavior change.
- Mirror both changes in `codex/skills/orchestrate-agent-review/scripts/`.

New skill:

- Create: `claude/skills/cross-review-plan/SKILL.md`
- Create: `claude/skills/cross-review-plan/scripts/cross_review_plan.py` —
  subcommands: `init-review` (guarded mode-0600 review file: repo, plan
  path + commit, reviewer buffer/backend/transcript, marker, transcript
  offset), `submit-review`, `status`, `watch`, `finish-review`
  (marker-gated, bounded transcript read), `reconcile-submission`
  (`--delivered` / `--not-delivered`), `retry-delivery` (Return-only, never
  retransmits). Imports `agent_session_lib` from the sibling skill via
  `Path(__file__).resolve().parents[2] / "orchestrate-agent-review" /
  "scripts"` — identical relative layout in both trees.
- Create: tests under `claude/skills/cross-review-plan/scripts/`
  (`test_cross_review_plan.py`): review-file state transitions, marker
  validation, delivery-acknowledgment gating, using fake transcripts and a
  stubbed emacsclient layer. (The existing orchestrate script currently has
  no test suite; the extracted module's pure functions get their first tests
  here.)
- Mirror the whole skill in `codex/skills/cross-review-plan/`.

Manifest and docs:

- Modify: `ai-config-sync.json` — add the `cross-review-plan` pair entry;
  confirm the `orchestrate-agent-review` entry's equivalence note still
  holds after the refactor (auxiliary files must match).
- Modify: whatever generated inventory (`agents/skill-inventory.org`)
  requires regeneration per the sync policy.

## Tasks

- [ ] 1. Extract `agent_session_lib.py` in the Claude tree; refactor
  `orchestrate_agent_review.py` to import it. Verify: `python -m py_compile`
  both files; run every subcommand's `--help`; `init-run` + `status` against
  a synthetic run file must behave identically to pre-refactor (golden-output
  comparison).
- [ ] 2. Write `test_agent_session_lib.py` covering the pure/parseable parts
  (marker offsets, transcript-advance detection, elisp string escaping,
  transcript message parsing) with fixture transcripts.
- [ ] 3. Write `cross_review_plan.py` with the single-phase guarded state
  machine, reusing the lib for all Emacs/session operations. Same safety
  semantics as the parent skill: mode-0600 files, pending-record before
  external submission, no automatic retransmission, marker-gated completion,
  transcript text never overrides a busy lifecycle state.
- [ ] 4. Write `test_cross_review_plan.py` (state transitions, double-submit
  rejection, premature finish rejection, reconcile paths).
- [ ] 5. Write `SKILL.md`: workflow above, backend detection, explicit
  instance names, the one-pass review prompt template, adjudication rules
  (borrowed verbatim where they apply from `orchestrate-agent-review`), stop
  conditions (reviewer session dead before any output → one `restart`-style
  recovery; user-only blockers → report), and a final-report checklist.
- [ ] 6. Mirror everything into the Codex tree; add the manifest entry;
  regenerate the skill inventory; commit as one logical change (hooks require
  both sides plus manifest together).
- [ ] 7. End-to-end verification: from a live Claude Code session, run the
  skill against a small real plan (candidate: the committed model-policy
  plan in the Epoch workspace), with a real Codex reviewer session; confirm
  delivery acknowledgment, marker return, bounded read, and that the
  orchestrate-agent-review skill still initializes a two-actor run
  post-refactor. The reverse direction (Codex-initiated, Claude reviewer)
  should also get one live run before the skill is called done.

## Explicitly out of scope

- Any change to `orchestrate-agent-review` behavior, prompts, or doctrine.
- Multi-round review, implementation review, or reviewer adjudication of
  rejected findings.
- Non-Emacs session transports (tmux, headless CLI). agent.el is the only
  session substrate, as in the parent skill.

## Open questions for the reviewer

1. **Cross-skill import vs duplication.** The sync manifest pairs
   Claude↔Codex copies of the *same* skill; it cannot enforce consistency
   between two *different* skills' copies of a duplicated library. The plan
   therefore imports the lib from the sibling skill directory (one source
   per tree, four copies avoided). Cost: `cross-review-plan` silently breaks
   if `orchestrate-agent-review` is renamed/removed. Is the coupling
   acceptable, or should the lib get a home of its own (e.g. a new
   `dotfiles/lib/python/` with both scripts adding it to `sys.path`)?
2. **Committed-plan requirement.** The handoff requires the plan to be a
   committed file (path + hash). Should there be an inline-content fallback
   for plans that live outside any repo, or is "commit it first, wherever it
   lives" an acceptable universal rule?
3. **Stall handling.** The parent skill permits one targeted whole-stage
   steer after a genuine early return. For a single review phase, is the
   equivalent (one targeted steer, never generic "continue") worth carrying
   over, or should an early/markerless reviewer return simply surface to the
   user?
4. **Reviewer session afterlife.** Proposal: leave the reviewer session open
   and report its buffer name so Pablo can inspect it; never kill it
   automatically. Any reason to prefer automatic teardown?
5. **Plan creation inside the skill.** Step 1 creates the plan when none
   exists. Alternative: the skill *requires* an existing committed plan and
   plan-writing stays a separate concern. The plan proposes create-if-absent
   for one-command ergonomics; is the scope creep acceptable?
6. **Refactor verification depth.** Golden-output comparison of `--help`,
   `init-run`, and `status` plus new unit tests is the proposed bar for "the
   refactor changed nothing", given no pre-existing test suite and the cost
   of a full live two-actor run. Sufficient, or should task 7's live
   two-actor `init-run` smoke be promoted to a full staged run?
