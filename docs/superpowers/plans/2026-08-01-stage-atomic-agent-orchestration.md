# Stage-Atomic Agent Orchestration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking. These tasks are implementation details for the worker; they are not orchestration checkpoints and must never be exposed to the user or sent between Agent 1 and Agent 2.

**Goal:** Make a complete stage, rather than a plan task, the mechanically enforced unit of two-agent orchestration.

**Architecture:** Add a mode-`0600` run-state file and route every prompt through a guarded five-phase state machine. Once implementation starts, reject arbitrary submissions; expose only a fixed `resume-stage` recovery that targets Agent 1 and preserves the whole-stage completion contract. Keep the paired Claude/Codex skill and helper copies byte-identical.

**Tech Stack:** Python standard library, `unittest`, Markdown skills, Org documentation

---

### Task 1: Specify the guarded run-state contract with failing tests

**Files:**
- Modify: `tests/test_orchestrate_agent_review.py`

- [ ] **Step 1: Add failing paired-skill assertions**

Require both skill copies to contain the stage-atomic hard rule, forbid task-level status and recovery, require stage-final verification only after Agent 1 returns, and document `init-run`, guarded `submit`, and `resume-stage`.

- [ ] **Step 2: Add failing helper state-machine tests**

Cover these public behaviors with temporary run files and mocked Emacs calls:

```python
orchestrator.create_run(...)
orchestrator.submit(...)
orchestrator.finish_phase(...)
orchestrator.resume_stage(...)
orchestrator.complete_stage(...)
```

Assert the exact phase sequence `spec -> spec-review -> plan -> plan-review -> implementation`, fixed role routing, an immutable implementation contract, rejection of duplicate/out-of-order/post-implementation arbitrary submissions, stage-only recovery, busy-agent rejection, adoption evidence, mode `0600`, and paired helper identity. Also require a returned top-level actor plus exact completion marker before the next phase, explicit reconciliation after ambiguous delivery, and acceptance evidence before completion.

- [ ] **Step 3: Run the focused tests and verify RED**

Run:

```bash
python3 -m unittest tests.test_orchestrate_agent_review
```

Expected: new tests fail because run-state and stage-recovery APIs do not exist and the current skill lacks the hard-rule language.

### Task 2: Implement the state machine and stage-only recovery

**Files:**
- Modify: `codex/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`
- Modify: `claude/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py`

- [ ] **Step 1: Add run-state loading and atomic persistence**

Implement a versioned JSON state containing repository, stage, Agent 1 and Agent 2 role bundles with required top-level transcript paths, expected phase, status, submissions, transcript boundaries, and resume count. Create new files with `O_EXCL | O_NOFOLLOW` and mode `0600`; reject malformed, symlinked, or unsupported state.

- [ ] **Step 2: Replace arbitrary submission with guarded phase submission**

Change `submit` to require `--run-file`, `--phase`, and `--prompt-file`. Resolve the destination from the fixed role bundle, reject an unexpected phase before Emacs is called, persist an ambiguous pending submission before contacting Emacs, and prepend the fixed whole-stage contract to implementation prompts. Successful delivery marks the phase active rather than complete.

- [ ] **Step 3: Add lifecycle commands**

Add:

```text
init-run
run-status
finish-phase
reconcile-submission
resume-stage
complete-stage
```

`finish-phase` requires the fixed actor to be awaiting input and verifies the exact completion marker only in transcript bytes appended after the current submission. `reconcile-submission` resolves ambiguous external delivery without an automatic retry. `resume-stage` accepts no prompt file, requires `implementation-active` and an awaiting Agent 1, and submits fixed whole-stage wording. `complete-stage` records explicit stage-final acceptance evidence and refuses an implementation that has not returned. Existing implementation may be adopted only with explicit spec commit, plan commit, and completed-review evidence. Remove global buffer enumeration so monitoring cannot substitute internal task/subagent sessions for the fixed actors.

- [ ] **Step 4: Run focused tests and verify GREEN**

Run:

```bash
python3 -m unittest tests.test_orchestrate_agent_review
```

Expected: all orchestrator tests pass.

### Task 3: Make the paired skills and documentation unambiguous

**Files:**
- Modify: `codex/skills/orchestrate-agent-review/SKILL.md`
- Modify: `claude/skills/orchestrate-agent-review/SKILL.md`
- Modify: `codex/skills/orchestrate-agent-review/agents/openai.yaml`
- Modify: `claude/skills/orchestrate-agent-review/agents/openai.yaml`
- Modify: `agents/skill-inventory.org` (generated)

- [ ] **Step 1: Add the hard rule and red flags**

State that implementation is one opaque handoff. Explicitly prohibit task-number status, task transcript/process inspection, task-specific prompts, per-task acceptance gates, task-boundary user updates, and additional implementation submissions. Define these behaviors as workflow violations even when no extra Agent 2 review occurs.

- [ ] **Step 2: Document guarded commands and stage-level monitoring**

Replace raw `submit` examples with `init-run` plus guarded phase submissions. During implementation, permit only phase-level `watch` output and fixed `resume-stage` recovery. Require one independent stage-final acceptance pass after Agent 1 returns.

- [ ] **Step 3: Refresh paired metadata and generated inventory**

Make the default prompt explicitly request a complete stage workflow and regenerate the tracked public-skill inventory. The individual workflow remains owned by `SKILL.md`.

### Task 4: Verify behavior and deployment

**Files:**
- Test: `tests/test_orchestrate_agent_review.py`
- Verify: paired skill/helper/metadata files

- [ ] **Step 1: Run focused and full tests**

Run:

```bash
python3 -m unittest tests.test_orchestrate_agent_review
python3 -m unittest discover -s tests -p 'test_*.py'
```

Expected: both commands exit 0.

- [ ] **Step 2: Validate the skill and paired configuration**

Run:

```bash
python3 codex/skills/.system/skill-creator/scripts/quick_validate.py codex/skills/orchestrate-agent-review
bin/ai-config-sync audit
```

If the local validator path differs, resolve the installed `skill-creator` script and run that exact validator. Expected: validation and sync audit exit 0.

- [ ] **Step 3: Forward-test the repaired skill**

Repeat the three baseline pressure scenarios with fresh agents using the modified skill. They must report only the stage and phase, leave a busy Agent 1 alone, use only fixed `resume-stage` when Agent 1 awaits input, and defer independent acceptance verification until the complete stage returns.

- [ ] **Step 4: Inspect scope and commit**

Run:

```bash
git diff --check
git status --short
```

Stage only the paired orchestrator skill, helper, metadata, tests, generated inventory, and these design/plan artifacts. Leave unrelated `codex/config.toml` untouched. Commit with:

```bash
git commit -m "agents: enforce stage-atomic orchestration"
```
