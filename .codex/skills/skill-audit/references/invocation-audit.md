# Codex invocation evidence audit

The bundled collector reconstructs conservative invocation evidence from active
and archived Codex JSONL rollouts; it does not consume native invocation telemetry.
Do not count raw text matches: developer catalogs, quoted logs,
retries, and audit reads contain skill names without proving invocation.

## Run the collector

Record the exclusive UTC cutoff in `CUTOFF` before opening any target skill,
then run from this skill directory:

```bash
python3 scripts/audit_invocations.py \
  --cutoff "$CUTOFF" \
  --format json
```

The default inputs are `~/.codex/sessions` and
`~/.codex/archived_sessions`. The default catalog is the personal skills under
the dotfiles `.codex/skills` and `codex/skills` roots. Useful options are:

- repeat `--input FILE_OR_DIRECTORY` for fixed fixtures or another archive;
- repeat `--skill NAME` to audit an exact historical name;
- repeat `--skill-root DIRECTORY` to replace the default catalog roots;
- use `--include-all` only when plugin and system skills are in scope;
- use human output for orientation and JSON for reconciliation or evidence
  review.

The collector requires `rg`. It scans rollout files that can contain candidate
calls and parses records to apply the timestamp cutoff. Evidence at or after
the cutoff is excluded from invocation counts and retained evidence. Input
diagnostics for missing timestamps or malformed lines cannot always be assigned
to that window. The report does not include prompt text.

## Evidence model

A confirmed invocation requires all of these in one turn:

1. an assistant message that announces the named skill;
2. a read-like tool call for that skill's main `SKILL.md`;
3. successful tool output containing matching YAML frontmatter.

The collector recognizes task-start and turn-context records and supported
plain or structured command-output envelopes. A nonzero command status must not
become successful evidence when its output still contains frontmatter. These
signals establish invocation evidence, not completion of the workflow or proof
that the entire instruction body was loaded.

The collector deduplicates repeated evidence by turn and canonical skill,
including copies in forked or archived rollouts. It then classifies the direct
user prompt:

- **explicit, high confidence:** the prompt directly invokes the skill by name,
  command, or clear “use/invoke” phrasing;
- **autonomous, medium confidence:** a direct prompt exists but has no explicit
  marker;
- **unknown, low confidence:** no direct user-message event is available, or the
  prompt names the skill without a proven positive invocation marker.

Match full skill names, not name prefixes. A plugin and a personal skill with
the same basename retain separate identities even in a combined read. Earliest
evidence is ordered by parsed timestamps, including fractional seconds.

Reads without a matching announcement or frontmatter remain ambiguous. They
often come from skill editing, audits, interrupted calls, or truncated output.
Never add them to confirmed counts without manual evidence.

## Interpret the report

1. Confirm that eligible, ignored, confirmed, and ambiguous totals reconcile.
2. State the time window, input roots, catalog size, and ignored out-of-scope
   candidates.
3. Review all ambiguous evidence for any high-frequency skill before drawing a
   routing conclusion.
4. Rank confirmed autonomous use separately from explicit use. High total use
   is not evidence of bad routing.
5. For the highest-impact autonomous skills, inspect the skill as it exists now
   and run fresh positive and near-miss prompts. Historical logs can show that a
   route happened, but not whether the current description still causes it.

Current limitations belong in the report: this collector covers Codex JSONL,
not Claude transcripts; a historical skill absent from the current catalog is
excluded unless named explicitly; old sessions without announcements remain
ambiguous; and transcript schema changes can reduce coverage until fixtures are
updated.
