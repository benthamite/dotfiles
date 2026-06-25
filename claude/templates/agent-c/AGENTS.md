# AGENTS.md - local agent-c overlay

This is Pablo's local-only agent-c instruction overlay. The worktree setup and
sync helpers prepend `~/Trajectory/AGENTS.md`, then append this file into task
worktrees and hide the result from ordinary git status; do not push it to the
shared `trajectory-labs-pbc/agent-c` repo.

Use this checkout here primarily for Trajectory conceptual-reasoning
("grantmaking") tasks. Keep root context small: detailed workflow guidance
belongs in skills and scoped docs, not in always-loaded root instructions.

If you are not working on conceptual-reasoning tasks, use the scoped guidance
under the relevant directory (`tasks/AGENTS.md`, `platform/AGENTS.md`,
`.github/AGENTS.md`) and the matching skill.

## Where To Work

- Work from a task worktree under `~/Trajectory/agent-c/<task-slug>`, not from
  the Trajectory umbrella root.
- Create new task worktrees with `M-x agent-trajectory-new-task` from Emacs, or
  the shell fallback `newtask <task-slug>`. Both create `pablo/<task-slug>` from
  `origin/main`, wire the local `.claude/.env` symlink, and apply this local
  instruction overlay.
- Existing task worktrees should stay current with `origin/main` through the
  session-start sync hook, or manually with `syncagentc` after committing or
  stashing tracked work. The sync hook temporarily restores upstream
  instructions before merging, then reapplies this local overlay.

## Load The Workflow Skill

Load the current skill instead of relying on memory or adding workflow detail
here:

| Work | Skill |
|---|---|
| Build a CR task from a gold article | `article-to-rubric` |
| QA a CR prompt/rubric | `qa-reasoning` |
| Sharpen/harden an existing CR task | `reasoning-brainstorm` |
| Attribute score changes across runs | `analyze-reasoning` |
| General task mechanics | `tasks` |

For Pablo's end-to-end CR orchestration, use the parent Trajectory repo's
`cr-task-build` skill. It routes into the canonical in-repo skills rather than
reimplementing their instructions.

## CR Invariants

- Do not hand-score or improvise graders. Use the supported CR grading loop and
  the current rubric/grader contract.
- When reporting a score, state what was graded (model answer vs gold), the
  rubric version, the run/backend, the per-criterion breakdown, and the
  recomputed weighted mean.
- Grade against the real gold source when fairness matters. `grading/GOLDEN.md`
  is a lossy target extraction, not a substitute for the source.
- Keep prompt and rubric aligned: every prompt ask should be scored or softened;
  every rubric criterion should be elicited by the prompt or explicitly treated
  as implicit expert best practice.
- Prefer fair, task-specific reasoning criteria over recall traps, exact wording
  requirements, or weight tuning to hit a target number.

## Safety And Publishing

- Never `git push`, create/edit/label/assign/move/close GitHub issues or PRs, or
  touch project-board state without Pablo's explicit approval for that action.
- Local commits are fine and expected; keep them single-purpose.
- Do not read, print, export, or inline API keys. The CR engine loads
  `.claude/.env` itself; commands should only create or repair the symlink.

## Keep This File Short

If a future note is detailed enough to need examples, commands, or a checklist,
put it in the relevant skill or scoped AGENTS.md and leave only a pointer here.
