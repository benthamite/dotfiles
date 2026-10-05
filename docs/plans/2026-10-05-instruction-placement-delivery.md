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
  1. Read `agents/instruction-placement.org` in full before choosing.
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

### 4. Apply the policy to today's leftover: `personal-details.md` (done in a9979771a; the context-index row was dropped because the docs audit admits only tracked files)
- **Reference fact:** per the policy, it gets a link from the artifact loaded when forms are filled.
- **Delivery:** the `chrome-use` skill is loaded before every browser step. Add one line there: "To fill a form for Pablo, take his details from `claude/context/personal-details.md`."
- **Context index:** add a row to `claude/context/README.org` (it lists reference docs).
- **README fix:** correct the false uncommitted line in `claude/README.org:201`.
- **Unchanged:** keep the link already in `notes/flight-booking-profile.org`.

### 5. Commit
- Commit in single-purpose commits, following repo gates (README updates, pair sync):
  1. `.gitignore` + context index + chrome-use line + README fix;
  2. the new skill + dotfiles-context repoint + READMEs + policy step.

## Critical files
- `agents/instruction-placement.org`: decision test, one added step.
- `claude/skills/instruction-placement/SKILL.md` and `codex/skills/instruction-placement/SKILL.md`: new.
- `claude/skills/dotfiles-context/SKILL.md` and `codex/skills/dotfiles-context/SKILL.md`.
- `claude/skills/chrome-use/SKILL.md` and its codex peer.
- `claude/context/README.org`, `claude/README.org`, `codex/README.org`, `.gitignore` (already staged).

## Verification
- `bin/ai-config-sync audit` passes, and the commit gates pass.
- **Trigger test (the real check):** run 3 fresh sessions started outside the dotfiles repo (cwd `~/My Drive/Forethought`), one each for Claude and Codex where possible. Use these prompts:
  1. "Remember my seat preference is aisle so you don't ask again."
  2. "Add a rule that you should always X" (a non-every-session rule).
  3. "Store my passport expiry somewhere you can find it."
- Pass criterion: in the rendered log, each run loads `instruction-placement` and reads the policy before writing anything. Each run uses no tools beyond reading, because the prompts end with "plan only, don't write". The cost is a few small model calls.
- **Negative check:** an ordinary coding prompt does not trigger the skill.
- If any trigger test fails, revise the description and rerun before calling it done.
