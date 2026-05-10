---
name: skill-audit
description: Audit or improve Claude/Codex skills for trigger quality, workflow design, progressive disclosure, verification, portability, and maintainability. Use for SKILL.md reviews, hardening, refactors, or keep/split/merge decisions; not for ordinary code/docs audits.
---

# Skill audit

Review $ARGUMENTS as an agent skill. The goal is to find concrete changes that would make the skill trigger more reliably, guide the agent more effectively, reduce repeated work, and produce more verifiable results.

If `--accept` is present in $ARGUMENTS, after completing the audit, immediately apply all high-confidence fixes that do not change the skill's intended scope. Preserve the skill name unless the user explicitly asked for a rename, then follow the accept-mode verification and commit steps below.

Read the actual skill files before judging. If the user names a skill but does not give a path, use the local skill resolver when available. Choose `--tool claude` for Claude Code skills and `--tool codex` for Codex skills:

```bash
/Users/pablostafforini/My\ Drive/dotfiles/bin/agent-skill path <skill-name> --tool claude
/Users/pablostafforini/My\ Drive/dotfiles/bin/agent-skill cat <skill-name> --tool claude
/Users/pablostafforini/My\ Drive/dotfiles/bin/agent-skill path <skill-name> --tool codex
/Users/pablostafforini/My\ Drive/dotfiles/bin/agent-skill cat <skill-name> --tool codex
```

If the resolver is unavailable, search likely skill roots with `rg --files -g SKILL.md`.

## Scope Boundaries

Use this skill for reusable agent skills, skill directories, `SKILL.md` files, and closely related skill workflow artifacts. Do not use it for ordinary code review, security review, architecture/design audit, documentation proofreading, or general Claude/Codex configuration cleanup unless the artifact being evaluated is itself a skill or skill-adjacent workflow; route those requests to the neighboring audit or maintenance skills.

## Workflow

1. Resolve the target exactly. If a skill name could refer to both Claude and Codex copies, inspect both unless the request narrows the scope to one tool. If paired copies differ, decide whether the difference is intentional before proposing synchronization.
2. Inventory the skill directory. Read `SKILL.md` first, then list bundled scripts, references, assets, evals, or state files and open only the ones needed to judge how the skill uses them.
3. Audit against the checklist below. Prioritize issues that affect triggering, scope boundaries, execution order, safety, verification, or maintainability; skip pure style preferences.
4. In normal mode, report findings using the output format below.
5. In `--accept` mode, apply only high-confidence, scope-preserving edits. Keep paired copies synchronized unless their differences are intentional, update directly required docs, run available checks or re-read changed files, inspect `git diff` and `git status`, and commit only the audited skill/docs changes.
6. After `--accept`, include files changed, commit hash, verification performed, and deferred or unresolved recommendations in the final response.

## What to Look For

### Trigger Quality

- **Weak descriptions**: descriptions that say what the skill is about but not when to use it.
- **Missing user phrasing**: common prompts, synonyms, typos, and adjacent contexts are absent from the description.
- **Undertrigger risk**: the description is too modest for a workflow the user expects the agent to notice proactively.
- **Overtrigger risk**: the description captures broad keywords but not the actual intent boundary.
- **Misplaced trigger guidance**: "when to use" information lives only in the body, where it will not help the model decide whether to open the skill.

### Scope and Fit

- **Vague purpose**: the skill is an idea or topic area, not a repeatable workflow.
- **Omnibus scope**: unrelated workflows are packed into one skill and should be split.
- **Over-narrow scope**: the skill is so tied to one example that it will not generalize to nearby real prompts.
- **Missing "when not to use" guidance**: near-miss cases are not separated from valid uses.
- **Duplicate coverage**: the skill substantially overlaps another local skill without a clear division of responsibility.

### Workflow Design

- **Unordered advice**: the body lists tips but does not tell the agent what to do first, next, and last.
- **Missing operational details**: required commands, tools, source-of-truth locations, accounts, or files are not named.
- **Manual handoffs**: the skill asks the user to do things the agent can do itself.
- **Silent fallback patterns**: the skill encourages workaround behavior without labeling tradeoffs or asking for approval where needed.
- **Unclear stop conditions**: the skill does not define what "done" means.

### Progressive Disclosure

- **Overloaded SKILL.md**: the main file is long enough that important instructions are hard to find.
- **Missing references**: large background material belongs in `references/` with clear instructions for when to read it.
- **Missing scripts**: agents repeatedly recreate deterministic helper code that should live in `scripts/`.
- **Unused bundled resources**: assets, references, or scripts exist but the skill body does not say when to use them.
- **Poor variant organization**: framework-, provider-, or account-specific guidance is mixed together instead of split into targeted reference files.

### Instruction Quality

- **Commandments without reasons**: heavy `MUST`/`NEVER` language is used where explaining the reason would guide judgment better.
- **Ambiguous output expectations**: the skill does not specify report structure, file outputs, or final response format.
- **Missing examples**: there are no realistic input/output examples for nuanced behavior.
- **Contradictory guidance**: sections pull the agent in different directions.
- **Local convention drift**: the skill conflicts with repository, tool, or user instructions.

### Verification and Evals

- **No verification loop**: the skill does not say how to check that the workflow succeeded.
- **No realistic test prompts**: the skill has not been tried against prompts a user would actually write.
- **No near-miss trigger tests**: there are no examples that should not trigger the skill.
- **Subjective-only evaluation**: objective outputs are available but not checked with assertions, scripts, or deterministic comparisons.
- **No baseline comparison**: there is no evidence the skill improves behavior compared with not using it.

### Safety and Portability

- **Surprising behavior**: the skill may create external side effects, delete data, expose secrets, or contact services without making that clear.
- **Unstated dependencies**: required CLIs, credentials, MCP servers, browser profiles, or local paths are assumed.
- **Secret handling gaps**: the skill does not protect credentials or sensitive outputs where relevant.
- **Non-portable paths**: local paths are used without explaining whether the skill is personal or reusable.
- **Missing cleanup**: temporary files, servers, or generated artifacts are not cleaned up or handed off explicitly.

## What Not to Flag

- A skill being personal or local-path-specific, if that is intentional and clearly documented.
- Lack of quantitative evals for strongly subjective workflows where human review is the right judge.
- A short SKILL.md, if the workflow is simple and complete.
- Missing scripts when the task is genuinely judgment-heavy and not repetitive.
- Style preferences that do not affect triggering, execution, verification, safety, or maintainability.

## Improvement Guidance

When proposing changes, prefer small edits that preserve the skill's purpose. The highest-value improvements usually are:

1. Rewrite the description so it triggers on realistic user phrasing.
2. Add a concise ordered workflow.
3. Add "when not to use" near-miss guidance.
4. Specify output format and verification.
5. Move long material into references or repeated helper logic into scripts.
6. Add realistic eval prompts, including should-trigger and should-not-trigger cases.

Explain why each recommendation matters. Skills work best when they transmit judgment, not just rules.

## Output Format

Lead with findings, ordered by impact:

1. **High Impact**: issues likely to prevent the skill from triggering, cause wrong behavior, create unsafe side effects, or make results unverifiable.
2. **Medium Impact**: issues that make the skill less reusable, slower, more expensive, or harder to maintain.
3. **Low Impact**: polish that would improve clarity but is not blocking.

For each finding include:

- File path and line number when possible.
- What is wrong.
- Why it matters for agent behavior.
- A concrete edit or replacement text when the fix is straightforward.

Then include:

- **What is working well**: specific strengths worth preserving.
- **Suggested next revision**: the smallest coherent set of edits to make first.
- **Verification**: how to test whether the revised skill is better.

If there are no serious issues, say that clearly and focus on residual risks or optional improvements.
