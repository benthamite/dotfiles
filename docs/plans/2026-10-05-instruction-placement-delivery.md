# Deliver the placement policy at the moment of decision

## Context

`agents/instruction-placement.org` defines how agent-facing guidance and facts are persisted. It covers progressive disclosure, a placement matrix, a decision test, and a permission boundary for CLAUDE.md/AGENTS.md. Agents keep violating it because nothing delivers it when they decide to persist something. Today was the eighth incident since June.

Past session logs show the same pattern each time. The fix moved the misplaced text, then counted on a future agent loading the policy. The only pointer to it sits in the body of `dotfiles-context`. That skill's description is about Emacs/Elpaca routing and says "not for unrelated dotfiles work", so it never fires for "save this so Pablo needn't repeat it", especially outside the dotfiles repo. Its other references are a project-local audit skill (`config-audit`), a disabled skill (`optimize-agent-instructions`) and READMEs.

Constraints from earlier sessions:
- Pablo rejected a placement hook (06-09, 09-27).
- The 10-05 remediation plan says "no new hooks; no new lines in CLAUDE.md/AGENTS.md".

The only other always-visible surface is skill descriptions. The design therefore puts the trigger in a description and keeps the policy itself on demand.

## Recommended approach

### 1. One global paired skill that owns the decision moment
- Create `claude/skills/instruction-placement/SKILL.md` and its `codex/skills/` peer. Use the same body; the Claude copy is model-invoked only.
- **Description** (the part that is always loaded) names the decision, not a file type. It applies whenever an agent, in any project, is about to persist something for future sessions:
  - a rule, preference, correction, workflow, or fact about Pablo;
  - "so you don't have to ask again", "remember this", "store this";
  - adding or changing guidance in any instruction file, skill, hook, context/reference doc, project map, brief or note.
- **Body**, a few lines with no restated policy:
  1. Read `/Users/pablostafforini/My Drive/dotfiles/agents/instruction-placement.org` (absolute path, so it resolves from any project) in full before choosing.
  2. Apply its decision test and permission boundary.
  3. For the chosen home, name what will put the content in front of the agent at the moment it is needed (a skill description, a hook, or a link from a map/skill that is already loaded at that time). If nothing does, the placement is incomplete.

### 2. Make the policy state the delivery requirement explicitly
- Add one step to the policy's Decision Test: "Name the trigger that delivers it when needed; a document no loaded artifact points to is not delivered."
- Record this incident class (eight recall-based fixes) in one sentence of history.
- The policy stays the single source.

### 3. Remove the dead-end pointers so there is one route
- `dotfiles-context` (both copies): replace the body sentence with "Use `instruction-placement` before adding or changing agent-facing guidance."
- Leave `config-audit`'s reference as is. It audits; it doesn't place.
- Update the `claude/README.org` and `codex/README.org` lines that say "Agent-facing guidance changes first consult the shared instruction-placement policy" so they name the skill as the delivery route.

### 4. `personal-details.md` (done: baseline)
Completed in a9979771a: `chrome-use` (both) points form filling at the file; READMEs updated. No context-index row: the docs audit admits only tracked files. Nothing further in this plan.

### 5. Commit
- Commit in single-purpose commits, following repo gates (README updates, pair sync):
  - One commit: the new skill pair + `dotfiles-context` repoint + both READMEs + policy step + the regenerated `agents/skill-inventory.org` (`bin/docs-audit generate`).

## Critical files
- `agents/instruction-placement.org`: decision test, one added step.
- `claude/skills/instruction-placement/SKILL.md` and `codex/skills/instruction-placement/SKILL.md`: new.
- `claude/skills/dotfiles-context/SKILL.md` and `codex/skills/dotfiles-context/SKILL.md`.
- `claude/README.org`, `codex/README.org`, `agents/skill-inventory.org` (generated).

## Verification
- `bin/docs-audit generate`, `bin/docs-audit audit` and `bin/ai-config-sync audit` pass; commit gates pass.
- Trigger matrix, every case on both Claude and Codex, fresh sessions with cwd `~/My Drive/Forethought`, each prompt ending "plan only, don't write anything":
  1. "Remember my seat preference is aisle so you don't ask again."
  2. "Add a rule that you should always run the linter before committing Elisp."
  3. "Store my passport expiry somewhere you can find it."
  4. Agent-initiated: a task in which Pablo corrects the agent mid-task (e.g. "you keep forgetting that I want X; make sure this doesn't happen again") without saying where to store it.
  5. Negative: an ordinary coding question; the skill must not load.
- Pass criterion for 1–4: the rendered log shows the skill loaded and the policy read, and the proposed plan (a) picks the home the policy's decision test gives, (b) proposes no CLAUDE.md/AGENTS.md edit without asking permission, (c) names the trigger that will deliver the content when needed. Loading alone is not a pass.
- On any failure, revise the description and rerun the failing cases on both hosts.

## Review
Codex review of de72bb4e (run 64f92d58): five findings, all accepted and applied above (stronger pass criterion; absolute policy path; full host×case matrix incl. agent-initiated case; step 4 treated as completed baseline; inventory regeneration).
