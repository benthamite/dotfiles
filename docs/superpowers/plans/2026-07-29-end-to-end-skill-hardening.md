# End-to-End Skill Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Harden the paired `end-to-end` skill so routing, task ownership,
external-effect safety, and completion evidence match the approved audit.

**Architecture:** Keep one normalized skill body for Claude and Codex, with
Claude retaining only its tool-specific `user-invocable` key. Store reusable
manual eval cases as identical auxiliary files in each skill directory, and
keep the three skill inventories/summaries synchronized with the new contract.

**Tech Stack:** Markdown agent skills and eval fixtures, Org documentation,
`bin/agent-skill`, `bin/ai-config-sync`, Git.

---

### Task 1: Preserve the RED scenarios

**Files:**
- Create: `claude/skills/end-to-end/evals/scenarios.md`
- Create: `codex/skills/end-to-end/evals/scenarios.md`

- [ ] **Step 1: Write routing cases**

Record positive prompts for real browser, Eat, Slack delivery, live Emacs
startup, session continuity, and controlled retry evidence. Record near misses
for automated E2E commands, explanations, test-code work, deployment status,
research reproduction, and pure-logic verification.

- [ ] **Step 2: Write behavior pressure cases**

Record the already-implemented verification-only case, the unauthorized
Slack/scheduler case, and the intermittent failed-run case. For each, specify
mode, authorization, safety, evidence, cleanup, and reporting assertions.

- [ ] **Step 3: Verify the current skill fails the cases for the expected reason**

Run the three cases through independent agents against the current skill.
Expected failures:

- the existing-change case stops at the unavailable old failure;
- the external case lacks one early fail-closed approval and cleanup gate;
- the intermittent case lacks thresholds, provenance, cleanup, and a return
  loop, while near-miss prompts route too broadly.

- [ ] **Step 4: Commit the eval fixture**

```bash
git add claude/skills/end-to-end/evals/scenarios.md codex/skills/end-to-end/evals/scenarios.md
git commit -m "end-to-end: add skill behavior scenarios"
```

### Task 2: Rewrite the paired skill contract

**Files:**
- Modify: `claude/skills/end-to-end/SKILL.md`
- Modify: `codex/skills/end-to-end/SKILL.md`

- [ ] **Step 1: Tighten frontmatter routing**

Use a `Use when` description limited to real runtime acceptance work and name
the automated-E2E, deployment-status, research-reproduction, and pure-logic
near misses.

- [ ] **Step 2: Add ownership and mode selection**

State that the skill owns live evidence, not expanded authorization. Branch the
workflow into unfixed regression, existing/new change, and
reproduction/verification-only modes.

- [ ] **Step 3: Add the fail-closed preflight**

Before reproduction, classify side effects and require explicit confirmation
for any external post, send, write, dispatch, schedule, or communication.
Cover cleanup in the same confirmation.

- [ ] **Step 4: Add evidence validity and completion rules**

Require supporting project checks, live revision provenance, predefined
thresholds for intermittent/performance symptoms, an explicit failed-run
branch, and cleanup. Correct the `verify` boundary and the scheduled-run
partial-evidence rule.

- [ ] **Step 5: Remove duplicated retry guidance**

Keep the full affected-user retry gate once in the workflow and make clear that
evidence does not authorize contacting the user.

- [ ] **Step 6: Compare the paired copies**

Run:

```bash
zsh -f -c "diff -u <(sed '/^user-invocable:/d' claude/skills/end-to-end/SKILL.md) codex/skills/end-to-end/SKILL.md"
```

Expected: exit 0 with no output.

### Task 3: Synchronize documentation

**Files:**
- Modify: `claude/README.org`
- Modify: `codex/README.org`
- Modify: `agents/README.org`

- [ ] **Step 1: Rewrite the Claude summary**

Describe the three modes, the early safety gate, evidence validity, failed-run
loop, cleanup, and honest blocked reporting without reproducing the whole
skill.

- [ ] **Step 2: Rewrite the Codex summaries**

Make both Codex references agree with the Claude contract and remove stale
reproduce-fix-only wording.

- [ ] **Step 3: Add the master inventory entry**

Insert `end-to-end` alphabetically after `elisp-conventions`, marked global,
with a routing-focused one-sentence summary.

### Task 4: Run GREEN and refactor tests

**Files:**
- Verify: `claude/skills/end-to-end/SKILL.md`
- Verify: `codex/skills/end-to-end/SKILL.md`
- Verify: both `evals/scenarios.md` files

- [ ] **Step 1: Rerun the three RED scenarios**

Expected:

- existing/new verification reaches the live acceptance path without edits;
- external actions stop before mutation pending explicit confirmation and carry
  a cleanup plan;
- an intermittent failure blocks push/success, returns authorized work to
  diagnosis, and reports failure in verification-only mode.

- [ ] **Step 2: Run routing near misses**

Expected: automated E2E scripts, deployment-status questions, research
reproduction, and pure-logic verification do not select the skill.

- [ ] **Step 3: Close any new loopholes**

Make only changes required by a failed scenario, then rerun that scenario and
the related positive case.

### Task 5: Verify and commit the completed revision

**Files:**
- Verify all files listed above.

- [ ] **Step 1: Run the resolvers**

```bash
bin/agent-skill cat end-to-end --tool claude
bin/agent-skill cat end-to-end --tool codex
```

Expected: each resolves and prints the revised skill.

- [ ] **Step 2: Run pair and repository checks**

```bash
cmp claude/skills/end-to-end/evals/scenarios.md codex/skills/end-to-end/evals/scenarios.md
bin/ai-config-sync audit
```

Expected: paired target checks pass. If the repository-wide audit still reports
unrelated pre-existing worktree problems, confirm none names `end-to-end`.

- [ ] **Step 3: Run relevant automated tests**

```bash
UV_CACHE_DIR=/tmp/uv-cache uv run --with pytest python -m pytest -q tests/test_ai_config_sync_audit.py
```

Expected: all tests pass.

- [ ] **Step 4: Inspect exact scope**

```bash
git diff --check
git diff --stat
git status --short
```

Expected: no whitespace errors; only planned target files are staged or changed,
apart from untouched pre-existing untracked files.

- [ ] **Step 5: Commit the implementation**

```bash
git add claude/skills/end-to-end/SKILL.md codex/skills/end-to-end/SKILL.md claude/README.org codex/README.org agents/README.org
git commit -m "end-to-end: harden live verification workflow"
```
