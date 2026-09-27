# Book acquisition through paper-fetch

Read the shared bibliography policy before choosing a target or accepting files.
These commands are acquisition/inspection steps; they do not import metadata or
write a bibliography. Use the absolute dotfiles `bin/paper-fetch` path and a
private staging directory outside Drive. Read service-access and secrets context
as required by the main skill before credential-backed acquisition.

1. Write `TARGET.json` from the verified selected edition: `title`, `author`
   (BibLaTeX creator string), `year`, `edition`, `language`, and its exact
   `isbn` when one exists. A pre-ISBN work can use a precise `query`.
   Do not borrow a later edition's ISBN to improve search results.
2. Run `paper-fetch book-candidates --target TARGET.json --out CANDIDATES.json`.
   Inspect status/attempts and candidate metadata. Search results and catalogue
   flags are candidates, not proof. Preserve search limits and unavailable routes.
   Anna's Archive has no search API, so its search normally returns
   `needs-browser` with an `annas_browser_search` handoff (exit 2 when nothing
   else was found). Run it in Chrome, save the page with its snippet, then rerun
   with a new `--out` plus `--annas-search-html FILE`; follow the contract's
   "Anna's Archive search" section. A pending, truncated or `incomplete` route
   keeps `search_complete` false.
   Register independently obtained publisher/archive PDFs with
   `paper-fetch book-register CANDIDATES.json --file LOCAL.pdf --source PUBLIC-URL --out NEW-CANDIDATES.json`.
   Use the resulting inventory for subsequent steps. Registration records the
   source and exact bytes without approving or inventing edition metadata.
3. Stage plausible candidates individually:
   `paper-fetch book-stage CANDIDATES.json --md5 MD5 --out STAGING-DIR`.
   Staging permits inspection; it is not acceptance. For a copy obtained through
   the authorized browser workflow, the same command with `--file LOCAL-PDF`
   verifies and stages that specific candidate. Never select a download by recency.
   A registered local candidate uses its recorded file without downloading again.
4. Run `paper-fetch book-inspect STAGED.pdf --out NEW-INSPECTION-DIR` (optionally
   `--pages 1,2,3,last` or explicit page numbers), then read its text/metadata and
   view the rendered images. Inspect title/copyright pages, contents, ending and
   representative interior pages against authoritative edition information. Follow the shared policy
   for identity, language, completeness and original printed-page fidelity.
   Do not use filename keywords or byte thresholds as substitutes for inspection.
5. Write the tool's review manifest against the exact target/candidate and staged
   file SHA-256. Each positive finding must carry concrete evidence for
   `identity`, `edition`, `language`, `completeness` and `physical_pages`.
   Read the exact manifest schema in
   [the acquisition contract](../../../../docs/book-acquisition.md); do not guess
   fields or claim evidence from images that were not inspected. Mark rejected and unverified candidates
   honestly. A prior review cannot approve changed bytes or another target.
6. Run `paper-fetch book-select CANDIDATES.json --reviews REVIEWS.json`.
   It checks the reviewed bytes and ranks eligible inspected files by actual size.
   Preserve its search/review limits; uninspected candidates prevent a claim of
   an exhaustive minimum. If none qualify, retain the unresolved attachment state.
7. Attach the selected staged PDF through the shared Ebib operation in the main
   skill, using the matching edition's entry/key. Recheck installed content after
   any OCR or renaming. Retain review evidence in the cleanup ledger when invoked
   from `clean-citations`.

The website's old-bibliography maintenance driver is not the entry-creation
route. Its shared acquisition implementation belongs to `paper_fetch.py`;
do not create another Anna's Archive client or copy book ranking into this skill.
