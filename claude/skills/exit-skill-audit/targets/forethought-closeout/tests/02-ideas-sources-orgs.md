# Test 02: project ideas, sources, outside organisations and unsent drafts

## Session summary

Session `00000000-test-0002-0000-000000000002`, Pablo, Codex, started
2026-11-04 15:00 in `~/repos/forethought/ph-ps`.

1. Pablo asks the agent to read a Google Doc, "Distribution channels review"
   (tool output gives `https://docs.google.com/document/d/TESTDOC/edit`). It
   bears on the distribution area as a whole, not on one project.
2. From the doc, the agent reports: "The doc says ExampleOrg has paused its
   newsletter until January." Pablo: "Good to know."
3. Pablo: "An idea: a weekly digest of new Forethought papers for funders.
   Not a project yet." The agent agrees it is not a project yet.
4. The agent drafts a Slack message to Max about the digest idea. Pablo:
   "Keep the draft, don't send it yet."
5. The agent finds that the doc's audience numbers disagree with the area
   README's (the doc says 1,200 subscribers in October; the README says 900,
   citing September). No other home covers audience numbers.
6. No files other than closeout's are changed.

## Repo state

`areas/distribution/README.md` has a Now section stating "Newsletter audience:
900 subscribers (September)" and an Open questions section. There is no
`areas/distribution/orgs/` directory and no file about ExampleOrg anywhere.
`project-ideas.md` has no digest entry. No project folder covers the
distribution area as a whole.

## Expected filing

| Item | Home | Notes |
|------|------|-------|
| The doc | `areas/distribution/README.md`, its sources (or a note in the area's `notes/` only if worth reading in full) | One-line description; the doc stays in Drive. |
| ExampleOrg paused its newsletter until January | `areas/distribution/orgs/exampleorg.md` (new) | Per "What an outside organisation is doing"; cites the doc. |
| Weekly digest for funders | `project-ideas.md` | One bullet under research distribution, `[idea]`, "Pablo, 4 Nov 2026", citing the session log. |
| 1,200 subscribers in October | `areas/distribution/README.md` Now | Newer evidence wins over September's 900: correct it, citing the doc. |
| Draft to Max | Log entry, as drafted and not sent | Never recorded as sent. |
| Session log | `projects/operations/logs/2026-11-04--pablo.md` | Work on an area's big picture logs under Operations. |

**Must not**
- Record the Slack message to Max as sent, or send it.
- Create a project folder or Linear project for the digest.
- Put ExampleOrg's pause in a project README.
- Keep both 900 and 1,200 as current figures.
- Create a task for the digest idea without Pablo asking for one.
