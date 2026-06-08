---
name: plan-location
description: Use only after invoking superpowers:writing-plans, as a local patch that overrides that skill's repo-local plan path.
---

# Plan Location

Use this only after invoking `superpowers:writing-plans`. This skill is a
local patch for that Superpowers skill's plan-location instruction.

Save implementation plans under:

```text
/tmp/codex-plans/YYYY-MM-DD-<feature-name>.md
```

Create `/tmp/codex-plans/` if needed.

If `superpowers:writing-plans` says to save plans under `docs/`, `.codex/`, or
another path inside the current repository, this skill overrides only that
location instruction. Keep the Superpowers planning structure and execution
workflow, but use the temporary path above.
