---
name: symptom-check
description: Force an invariant-focused root-cause check before applying a non-trivial bug fix. Use when a bug, regression, test failure, or suspicious behavior has a tempting local patch and you need to decide whether to fix locally, fix plus flag in architectural-issues.md, or stop for a refactor. Use after initial debugging when you would otherwise jump to a patch; do not use for trivial typo/formatting fixes or greenfield features.
---

The user (or you, on your own initiative) has flagged an apparent bug, and a fix is being considered. Your job is to **stop and force a structured root-cause analysis before any edit happens**, then recommend whether to fix locally, fix + flag for follow-up, or refactor.

This skill starts when a plausible local fix is already in view. If the root cause is still unknown, run the project's normal debugging workflow first, then return here before editing.

The default failure mode this skill exists to prevent: agents fix the immediate symptom, the abstraction stays broken, the same symptom recurs in different shapes, and the architectural drift is only caught when several seemingly-unrelated symptoms accumulate. By the time the connection is visible, multiple local patches have already shipped.

## Inputs

- A description of the symptom — what's broken or smells off.
- Optionally a proposed local fix.
- The repository working directory (you should read code, not guess).

## When not to use

- Do not use for trivial, mechanically obvious changes such as typo fixes, formatting-only edits, or comment cleanup.
- Do not use for greenfield feature work where no broken behavior or proposed bug fix exists.
- Use `systematic-debugging` first when the root cause is still unknown; use this skill once a local patch is tempting and you need to decide whether it is an isolated repair or a broader architectural issue.
- Use `code-audit` or `design-audit` for broad reviews that are not anchored to a concrete symptom.

## Workflow

### Step 1 — Read the symptom carefully

Restate the symptom in one or two sentences in your own words. If the input is vague ("X seems wrong"), clarify what specifically is observed — what was expected vs. what happened, with file paths and line numbers where possible.

### Step 2 — Read the existing registry

Resolve the project root with `git rev-parse --show-toplevel` when available, then read `architectural-issues.md` there if present. Skim Open and Closed entries. **If the new symptom looks structurally similar to an existing entry, surface the connection explicitly** — state which entry, why you think they're related, and whether the new symptom is a fresh manifestation of the existing root cause or a separate issue.

If no registry exists yet, that's fine. The first invocation seeds it.

### Step 3 — Write the local fix in one paragraph

Describe what fixing this in isolation would look like — the minimal edit that would make the immediate symptom go away. Be concrete: file, function, what changes.

This is the fix you're tempted to apply. Writing it explicitly lets the next steps interrogate it.

### Step 4 — Articulate the invariant

In **one sentence**, state the general property of the system that the local fix would restore.

- Good: "Source amendments target source-level data, and all downstream consumers must read amendment-applied data through one channel."
- Bad: "MB durations should be cleared when the amendment file says so." (specific to the symptom; not a general property)
- Bad: "The data should be correct." (true but unfalsifiable; not an invariant)

If the invariant statement is hard to write — if it keeps collapsing back to symptom-specific phrasing — that is itself the finding. **It means the local fix isn't restoring a general property; it's patching one case.** Note that explicitly and continue.

### Step 5 — Find other manifestations

Given the invariant, search for other places it might be violated. Be concrete: use `rg`, `git grep`, code navigation, or subagents to find parallel patterns; list the call sites and name the data paths. Do not speculate from filenames.

For each candidate manifestation, state whether you confirmed it (read the code and verified) or flagged it (suspicious-looking and worth investigating). Do not pad the list with maybes.

### Step 6 — Recommend a disposition

Pick exactly one:

- **Fix locally.** The local fix restores a clear invariant, no other manifestations exist, and the abstraction is sound. Apply the fix; no registry entry needed.
- **Fix locally + flag.** The local fix is correct for this case, but the same shape may exist elsewhere or warrant later review. Apply the fix when edits are authorized, then produce a registry entry for review or add it if registry updates were explicitly authorized.
- **Stop and refactor.** The local fix would patch one case while leaving the architectural drift in place. Multiple manifestations exist (or one is severe enough that ad-hoc patching is the wrong response). Do NOT apply the local fix yet — design the refactor first, log it in the registry, get user buy-in, then refactor.

The bias should be toward **flag** or **refactor** when in doubt. A registry entry costs nothing to add and pays off whenever the next similar symptom surfaces.

### Step 7 — Produce the registry entry

If the disposition is "fix locally + flag" or "stop and refactor", produce a markdown block ready to append to `architectural-issues.md`. Schema:

```markdown
### <one-line title naming the architectural pattern>
- **Discovered:** YYYY-MM-DD (context: where the symptom surfaced)
- **Symptom:** specific, with file paths / line numbers / data values
- **Root cause:** the architectural invariant that's being violated, in plain prose
- **Other manifestations:**
  - <each one as its own bullet, with file/line or thread reference>
- **Status:** open | deferred | refactor-in-progress | closed
- **Disposition:** what was decided. If refactor: link to decision record (or "TBD"). If flag: a sentence on what would trigger reopening.
```

Offer the entry to the user before writing it unless the original task explicitly authorized registry updates. The user may want to edit the wording.

## Verification

Before editing, verify that Steps 2, 4, and 5 are complete: registry checked, invariant stated in one sentence, and analogous manifestations searched from code evidence rather than filename guesses.

After applying an authorized fix, run the smallest relevant reproduction, test, lint, or build check that proves the symptom is resolved and the invariant still holds. If no executable check exists, re-read the changed code and state exactly what was and was not verified.

## Output format

Present Steps 1–6 as labelled sections in the chat. If Step 7 is needed, put the registry entry in a fenced markdown block at the end.

End with one of three sentences:

- "Disposition: fix locally. Applying now."
- "Disposition: fix locally + flag. Applying the fix; here's the registry entry to add."
- "Disposition: stop and refactor. Not applying the local fix; here's the registry entry and the refactor sketch."

## Anti-patterns

These have all happened. Don't:

- **Skip Step 4** because the local fix "obviously works." If you can't articulate the invariant, you're patching.
- **Pad Step 5** with maybes to make the case feel thorough. Either confirm or admit you didn't check.
- **Recommend "fix locally" as a default** when the invariant is shaky. Default to "flag."
- **Auto-write the registry entry without offering it.** The user reviews; the user approves.
- **Forget the existing registry.** Step 2 is not optional. Pattern recognition over time is the whole point.
- **Use this as a substitute for debugging.** This skill classifies a tempting fix; it does not replace reproducing the issue or identifying the root cause.
