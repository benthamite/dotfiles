---
name: verify
description: Execute any task with self-generated verification criteria that close the agentic loop. Generates testable success criteria before acting, verifies against them after, and loops until criteria are met. Use when the user says "verify", "verified task", "close the loop", or wants rigorous verification of non-coding work. Works for writing, research, analysis, classification, data work, and any domain where "did I get this right?" isn't automatically answerable.
user-invocable: true
argument-hint: <task description>
---

# Verified task execution

Execute $ARGUMENTS with self-generated verification criteria that close the agentic loop. The goal: never declare a task "done" without concrete evidence that it was done correctly.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

### Execution steps

#### Phase 1: Understand the task

Read the task description. Gather any context needed (files, codebase, prior work, external references). Identify the domain and the type of output expected.

#### Phase 2: Generate verification criteria

Before doing any work on the task itself, generate a set of **concrete, testable verification criteria** — your equivalent of unit tests for this domain. These criteria must be:

- **Specific**: not "the analysis should be good" but "the analysis should address arguments X, Y, Z and provide evidence for each"
- **Testable by you**: you must be able to evaluate each criterion programmatically or through careful inspection — no criterion should require human judgment you can't approximate
- **Comprehensive**: cover the main success dimensions (correctness, completeness, coherence, format, edge cases as applicable)
- **Independent**: each criterion should be evaluable on its own

**Examples by domain:**

| Domain | Example criteria |
|---|---|
| Writing | Does each claim have supporting evidence? Are there logical gaps between sections? Does the tone match the target audience? Is the word count within range? |
| Classification | Correctly classifies curated edge cases: [case1] -> A, [case2] -> B, [case3] -> A. Handles ambiguous cases by flagging rather than guessing. |
| Research | Every factual claim is traceable to a primary source. Sources are from the last 5 years (or justified if older). No circular citations. |
| Data transformation | Sample input X produces output Y. Empty input produces []. Malformed input raises a clear error, not garbage output. |
| Summarization | All key points from the source are represented. No claims appear in the summary that aren't in the source. Length is within N% of target. |

Present the criteria to the user for review before proceeding. The user may add, remove, or modify criteria. This is the one human-in-the-loop moment — get it right.

#### Phase 2.5: Meta-verify the criteria (recursive)

After generating criteria, assess your confidence in them:

- **High confidence**: the criteria are concrete, testable, and you can see exactly how you'd evaluate each one. Proceed to Phase 3.
- **Low confidence**: some criteria are vague, subjective, or you're not sure they capture what matters. **Treat the problem of generating better criteria as its own task and recurse.** Specifically:
  1. Ask: "What would good verification criteria look like for this type of task?" Research this — look at how experts in the domain evaluate quality, find rubrics, examine analogous evaluation frameworks.
  2. Generate improved criteria based on your research.
  3. Assess confidence again.
  4. If still uncertain after 2 levels of recursion, surface to the user: "I couldn't generate criteria I'm confident in. Here's what I considered and why it fell short. Can you help me define what 'done right' looks like?"

The recursion converges because each meta-level is easier than the one below it. Verifying criteria quality ("are these criteria concrete and testable?") is simpler than the original task.

#### Phase 3: Execute the task

Do the work. Use subagents where appropriate. At each major step, check progress against the relevant criteria — don't wait until the end to discover a fundamental problem.

#### Phase 4: Verify against criteria

Systematically evaluate the output against every criterion from Phase 2. For each criterion, report:

- **Pass**: criterion met, with evidence
- **Fail**: criterion not met, with explanation of the gap
- **Uncertain**: unable to evaluate confidently (explain why)

If using curated test cases (e.g., classification), run every case and report accuracy.

#### Phase 5: Loop or complete

- **All pass**: present the result to the user with the verification report. Done.
- **Some fail**: fix the failures, then re-verify (only the failed criteria, plus any that might have been affected by the fix). Loop until all pass or you've exhausted reasonable approaches.
- **Some uncertain**: flag these to the user. Ask whether they want to provide judgment, modify the criteria, or accept the uncertainty.

If you loop more than 3 times on the same criterion, stop and surface the issue to the user rather than spinning.

## Key principles

1. **Verification criteria are first-class work.** Generating them is not a formality — it's often the hardest and most valuable part of the process. Spend real effort here.
2. **The quality ceiling is set by verification, not generation.** You can generate excellent work, but without a way to *know* it's excellent, you're hoping. The criteria make quality legible.
3. **When in doubt, recurse.** If you can't verify something, treat "how to verify this" as its own problem. The meta-levels converge.
4. **When recursion bottoms out, escalate.** Making the epistemic situation transparent ("I don't know how to verify this and here's why") is itself a valuable output.
