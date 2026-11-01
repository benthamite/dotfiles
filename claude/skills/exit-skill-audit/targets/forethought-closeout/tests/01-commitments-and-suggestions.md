# Test 01: others' commitments, decisions and untaken suggestions

## Session summary

Session `00000000-test-0001-0000-000000000001`, Pablo, Claude Code, started
2026-11-03 10:00 in `~/repos/forethought/ph-ps/projects/philosophy-competition`.

1. Pablo: "Ronak replied on Slack: he'll send the judging rubric by Friday 7
   November." The agent reads the Slack thread (tool output gives permalink
   `https://forethought-ea.slack.com/archives/C0TEST/p1793700000000001`).
2. Pablo and the agent compare two ways to score essays. Pablo: "Let's use a
   single 1-to-10 score per judge, not separate criteria: Will said separate
   criteria made the last round slow." The agent agrees.
3. The agent suggests: "We could also build a dashboard of judges' scores."
   Pablo does not respond to that and moves on.
4. Pablo: "I'll write the instructions for the judges tomorrow." The agent
   offers to draft them; Pablo says "no, I'll do it myself".
5. Pablo: "Let's call the people who score essays 'raters', not 'judges', from
   now on." The agent notes the term.
6. The agent edits `projects/philosophy-competition/scoring.md` and commits it
   as `a1b2c3d` (pushed).

## Repo state

`projects/philosophy-competition/README.md`:

```markdown
# Philosophy competition
Run the AI philosophy competition. Linear: <link>

## Now
As of 2026-10-31.
Essays submitted; scoring not yet designed.

Next steps:
- Design the scoring scheme.

## Decisions in force
- 2026-10-28: three essays per entrant ([log](logs/2026-10-28--pablo.md)).

## Open questions
- How should essays be scored?

## Sources
- Competition plan doc.
```

`GLOSSARY.md` has no entry for "rater". No open task mentions judges'
instructions.

## Expected filing

| Item | Home | Notes |
|------|------|-------|
| Ronak owes the rubric by 7 Nov | README Now, "Others' commitments" | Person, commitment, date and the Slack permalink copied from the tool output. Not a board task. |
| Single 1-10 score per judge, because separate criteria slowed the last round | README Decisions in force | Dated 2026-11-03, linking `logs/2026-11-03--pablo.md`; the reason goes in the log entry's Decisions. |
| "How should essays be scored?" answered | README Open questions | Removed. |
| "Design the scoring scheme" done | README Now next steps | Removed; Now updated to say scoring is designed; "As of 2026-11-03". |
| Pablo writes the raters' instructions | Task for Pablo (follow-up `00000000-test-0001-0000-000000000001#1`) | Unprocessed with `--auto`; numbered in the log entry. |
| "Rater" replaces "judge" | `GLOSSARY.md` | New agreed term with its date. |
| Slack thread with Ronak | README Sources | One line. |
| Commit `a1b2c3d` | Log entry Changes | Commit link only, "(pushed)". |

**Must not**
- File the dashboard suggestion anywhere (not a follow-up, not an idea in `project-ideas.md`, not a decision).
- Create a task for the agent's offer to draft the instructions.
- Put Ronak's commitment on the task board.
- Restate the decision's reason in the README as well as the log, beyond its one line.
- Invent a permalink other than the one in the tool output.
