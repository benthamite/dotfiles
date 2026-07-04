# AGENTS.md - local reasoning-tasks overlay

This is Pablo's local-only reasoning-tasks instruction overlay. The worktree setup and
sync helpers prepend `~/Trajectory/AGENTS.md` — the umbrella layer that owns
where-to-work mechanics, skill routing, standing constraints (the approval and
publishing gates), and all pipeline pointers — then append this file into task
worktrees and hide the result from ordinary git status. Do not push it to the
shared `trajectory-labs-pbc/reasoning-tasks` repo, and do not restate umbrella-layer
rules here. If the umbrella layer is ever missing above this line, stop and
treat every push/GitHub/board action as gated on Pablo's explicit OK.

Use this checkout primarily for Trajectory conceptual-reasoning ("grantmaking")
tasks. If you are not working on conceptual-reasoning tasks, use the scoped
guidance under the relevant directory (`tasks/AGENTS.md`, `platform/AGENTS.md`,
`.github/AGENTS.md`) and the matching skill; for general task mechanics, load
the repo `tasks` skill.

CR-scoped repo docs (this overlay replaces the upstream root AGENTS.md — the
attack-task system doc — so jump to these directly): `meta/trajectory_labs/
AGENTS.md` (the `tl grantmaking` engine, Taiga-run normalization, and the v2
`## Q<n>.` question-rubric format) and `platform/environments/grantmaking/
AGENTS.md` (the Taiga task category, image, and MCP corpus server).

## CR Invariants

- Do not hand-score or improvise graders. Use the supported CR grading loop and
  the current rubric/grader contract.
- When reporting a score, state what was graded (model answer vs gold), the
  rubric version, the run/backend, the per-criterion breakdown, and the
  recomputed weighted mean. Always link the Taiga job when discussing run
  results (`https://taiga.ant.dev/jobs/<job_id>`), including interim ones.
- Grade against the real gold source when fairness matters. `grading/GOLDEN.md`
  is a lossy target extraction, not a substitute for the source.
- Keep prompt and rubric aligned: every prompt ask should be scored or softened;
  every rubric criterion should be elicited by the prompt or explicitly treated
  as implicit expert best practice.
- Prefer fair, task-specific reasoning criteria over recall traps, exact wording
  requirements, or weight tuning to hit a target number.

## Keep This File Short

If a future note is detailed enough to need examples, commands, or a checklist,
put it in the relevant skill or scoped AGENTS.md and leave only a pointer here.
