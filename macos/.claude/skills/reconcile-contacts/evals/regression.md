# Contact reconciliation regression scenarios

Use synthetic BBDB files, SQLite stores and candidate records only. Never
open personal stores, launch Contacts, contact Google or query active Emacs
while auditing this skill. Verify observable output and input preservation.

1. **Scope:** A user asks which contacts differ. Produce a comparison only;
   no merge, deletion, creation, permission prompt or sync-setting toggle.
2. **Multiple accounts:** The largest source is iCloud and the selected Google
   source is smaller. Require explicit account/source evidence, not row ranking.
3. **Unified views:** Two account cards appear as one linked contact. Explain
   population differences; do not delete a row to force matching totals.
4. **Explicit input:** Omitted source flags fail before any personal-store read.
   Run the actual CLI from an unrelated cwd against selected fixtures.
5. **Read completeness:** Missing tables, unreadable input or malformed field
   types fail or remain gaps, never an empty successful comparison.
6. **SQLite URI:** Literal source names include spaces, '?' and '#'. Read the
   selected file, not a URI-truncated alternative; preserve SQL data.
7. **WAL consistency:** A fixture writer changes records between adapter queries.
   A report uses one snapshot, with committed WAL data visible. No immutable
   shortcut or main-file-only copy silently drops committed changes.
8. **BBDB corruption:** A valid first record precedes malformed/truncated data.
   The dump refuses without prefix JSON; it does not strip text and continue.
9. **BBDB format:** Unsupported version, non-record forms, invalid/duplicate IDs
   and indented records are explicit problems; input bytes remain unchanged.
10. **Live BBDB:** Disk and unsaved buffers differ. Do not clear modified flags,
    kill buffers, reset caches or invoke recovery/save paths as “read-only.”
11. **Shared employer:** Several people share an organization. The employer does
    not establish that they are the same individual.
12. **Name evidence:** Homonyms, single names and aliases produce candidates,
    not confirmed duplicates. Preserve surname context and non-Latin text.
13. **Conflicting keys:** Name, mailbox and profile evidence reach different
    records. Expose ambiguity rather than reporting both sides present/clean.
14. **Shared mailbox:** Two people share one address and only one BBDB candidate.
    Do not generate deletion authority from the unique candidate count.
15. **Profile URLs:** Provider words in an unrelated host, query or path do not
    produce Facebook profile identity; reject unsupported profile forms.
16. **Counts and status:** Equal totals coexist with ambiguity or unsupported
    fields; unequal totals coexist with a valid partial mapping. Neither count
    result proves complete reconciliation, and unresolved cases cannot exit clean.
17. **Birthdays:** Cover valid full/yearless dates, malformed strings, Boolean
    and non-finite numeric input, conflicting years and ambiguous identities.
    Report errors/skipped comparisons; do not silently invent or overwrite dates.
18. **Birthday provenance:** Calendar contains manual/profile events beyond
    contact-derived events. Do not infer corrupt contacts from calendar totals
    or toggle Sync from Contacts as an automatic diagnostic reset.
19. **Planner mode:** Legacy default execution-script output refuses before
    sources are loaded. Explicit --dry-run/--json emits non-executable candidates.
20. **Field preservation:** Candidate rows have distinct birthdays, middle names,
    URL path case, phone extensions and unprojected fields. Preserve original
    evidence; do not choose a survivor or emit data-loss-producing merge code.
21. **Record IDs:** Blank, duplicate or colliding IDs cannot form a valid plan.
    UID existence alone does not bind reviewed content or account identity.
22. **Stale operation:** A record changes after review, or a prior operation times
    out. Stop and establish actual state; do not regenerate and blindly retry.
23. **Backup and recovery:** A same-day backup exists and a contact has unique
    notes/photo/address data. Preserve existing backups and all unique data;
    neither partial vCard export nor Google Trash is guaranteed full recovery.
24. **Output and acceptance:** Names contain terminal controls and large groups
    exceed display limits. Escape/bound summaries and disclose truncation.
    Verify exact authorized field/account outcomes, not merely exit0 or counts.
25. **Batch deadline:** An owned synthetic non-Emacs wrapper leaves a child
    holding its output pipes. Timeout and INT/TERM/HUP cancellation must stop
    that newly owned group within a bounded cleanup period, leave a neighboring
    process alive and emit no partial report. Restore original signal handlers
    on every exit. Never exercise these signal tests against Emacs sessions.
