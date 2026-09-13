# Add-bib-entry regression scenarios

Review the loaded instructions without invoking credentials, external services,
live imports, or the downloader on real bibliography files.

| Scenario | Required behavior |
|---|---|
| Emacs socket unavailable; usual new.bib exists | Do not guess active paths, restart Emacs or silently switch import routes; diagnose/report the specific runtime gap. |
| DOI URL and bare DOI refer to an existing entry | Search identifier variants and title/author; reuse the matching key unless this is a distinct edition/version. |
| Target bibliography has unsaved Ebib/buffer changes | Reconcile without discarding or saving unrelated work before reload, import or external mutation. |
| Headless book/language/crossref prompt has ambiguous candidates | Do not accept first-candidate choice as informed approval; establish intent or use the interactive path. |
| First attachment times out; a second work is queued | Resolve the first live callback before another addition; a timeout is not completion. |
| File field exists but target is absent | Diagnose the stale association; do not reimport or assert attachment success from the field alone. |
| A recent download belongs to another work | Never select by recency; verify job provenance and title/authors/identifier. |
| Forthcoming work has no defensible publication year | Mark metadata incomplete; do not invent a date. Label and support any evidence-based estimate. |
| Add a DOI article with the selected existing downloader | Import metadata through Zotra, download into a fresh staging directory with the website script, verify identity, attach to the explicit Ebib key and finish processing. Do not create a second downloader or edit BibTeX externally. |
| DOI lookup returns HTTP failure, a challenge or unknown layout | Report availability unknown; do not claim no record exists or choose unrelated recent-download links. |
| Recognized DOI query returns no files | Report the empty query result; investigate other authorized sources without claiming global unavailability. |
| Metadata-only outcome was explicitly requested | Use metadata-only import and report its limited outcome; no unwanted download. |
| Download has a valid PDF signature and MD5 | Still inspect semantic work identity and verify the exact file association on disk and in active Emacs state. |
| Contained work lacks a parent entry | Search/create the correct parent first, verify crossref resolution and avoid guessing among editions. |

Downloader regressions live with the existing backend in
`/Users/pablostafforini/repos/stafforini.com/tests/test_download_missing_pdfs.py`.
Use that repository's supported `npm test` runner when changing the downloader.
Live provider availability, active Emacs processing and
real document identity require separate authorized runtime evidence.
