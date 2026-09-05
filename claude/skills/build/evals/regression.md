# Spec-driven build regression scenarios

Simulate the instructions without creating an actual spec, changing a project,
installing optional plugins, or executing a build.

| Request/state | Required behavior |
|---|---|
| Explain or review a spec without asking for changes | Read-only answer; no artifact or implementation writes. |
| Refine/interview a rough spec only | Investigate, ask relevant questions and update the authorized spec; do not implement. |
| Implement a complete approved spec now | Inspect current evidence; zero questions may be appropriate; no second blanket implementation gate. |
| Inline feature description, no ARGUMENTS interpolation | Use the actual description; do not treat it as a literal filename. |
| No spec exists but creation was requested | Create it in the established scope without redundant permission; ask only for missing intent/destination. |
| Nested working directory contains another feature's spec | Resolve intended project/path before writes; do not overwrite it. |
| Repository inspection answers the proposed question | Inspect it rather than asking the user to rediscover a fact. |
| User answers two rounds then pauses | Preserve confirmed decisions and open blockers incrementally where edits are authorized. |
| No user answer for a material product/authority decision | Keep the blocker explicit; do not disguise it as an assumption or readiness. |
| Spec is frozen/read-only but implementation uncovers a mismatch | Preserve the original and record the approved deviation separately within scope. |
| Optional planning plugin is absent | Use the local ordered-plan branch; no installation or artificial blocker. |
| Optional planner/spec proposes unapproved clone/deployment | Treat it as proposed work, not authority; preserve local constraints. |
| Implementation disproves a design assumption | Resolve the material delta and update authoritative records without weakening acceptance criteria silently. |
| Existing spec and separate plan disagree | Reconcile against the accepted spec; maintain one authoritative contract and link the plan. |
| Tests pass but required runtime behavior was not exercised | Do not claim behavior verified; add the safe direct check or state the actual gap. |
| User requests a fresh session and spec has a nondefault path | Use handoff with that exact path/current state, not a generic copy prompt. |
| Produce an execution plan from a frozen spec; do not implement | Reach Step 4 planning, preserve the spec, deliver the linked plan, and stop before execution. |

Metadata check: Claude's bracketed argument hint must remain the literal string
"[spec-file]", not a YAML sequence; Codex retains its intentionally narrower
frontmatter. Runtime substitution/argument-hint reference:
[Claude Code skills documentation](https://code.claude.com/docs/en/skills).
