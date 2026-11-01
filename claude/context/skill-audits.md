# Self-audits of exit-time skills

Principle (Pablo, 2026-11-01): every exit-time skill audits itself monthly via the `exit-skill-audit` skill; audits only propose, Pablo approves. (The skill was agreed as `skill-audit`; it is named `exit-skill-audit` because the dotfiles project-local `skill-audit`, an explicit audit of skill routing, already held that name.) An exit-time skill is one that runs unattended at the end of a session, such as a step in the Emacs before-exit chain (`agent-before-exit-skill-names` in `emacs/config.org`). Unattended closeouts fail silently, so the audit compares what they filed with what the session actually surfaced, measures their cost and checks them against their own spec.

How it applies, the same way for every such skill:

- The skill becomes a target of `exit-skill-audit`: a directory in `claude/skills/exit-skill-audit/targets/<id>/` (mirrored in `codex/skills/`) with `target.json` (skill path, covered directories, interval), `checks.md` (what this skill's audit measures) and, where possible, `tests/` (synthetic sessions with the expected result). Target-specific checks live there, in dotfiles, not in the target's repository.
- `exit-skill-audit` runs as the before-exit step after the target, gated by a receipt, for sessions in the target's directories. It writes `no-op` when no audit is due, runs the audit in fresh-context subagents when one is, and writes `held` with the proposals so the session stays open for Pablo.
- The audit never edits the target. Pablo decides each proposal; approved changes go in afterwards as ordinary commits under the target repository's own rules (a shared repository's rules govern shared skills, such as Forethought's ph-ps).
- State, lock and proposals are local machine state in `~/.local/state/exit-skill-audit/<target>/`.

Design and the first audit's method: `~/repos/forethought/notes/2026-11-01--closeout-revision-plan.md`, "Self-maintenance".

## Instances

- Compliant: Forethought `closeout` (`~/repos/forethought/ph-ps/skills/closeout/`), target `forethought-closeout`, added 2026-11-01; first audit by hand on 2026-11-01, so the first automatic one is due on 2026-12-01.
- Not yet compliant (TODO):
  - `update-log` (dotfiles `claude/skills/update-log/`) for sessions outside Epoch and Forethought: needs a target, checks and tests, and a chain entry whose directories cover those sessions.
  - Epoch's closeout, the workspace's `epoch-update-log` skill (`$EPOCH_WORKSPACE/.claude/skills/epoch-update-log/`): needs a target covering `$EPOCH_WORKSPACE` and `$EPOCH_PRIVATE_ROOT`, checks against the workspace's routing rules, tests, and the chain entry's directories widened to Epoch.
  - `session-learning-capture`, the chain's first step: decide whether it counts as an exit-time skill under this principle (it only writes proposals to an inbox), and either add a target or record why not.
