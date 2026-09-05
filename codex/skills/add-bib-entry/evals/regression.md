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
| Normal service path fails; bundled downloader is present | Diagnose, justify and obtain approval for the labeled fallback; route through the loaded skill directory and resolved bibliography. |
| Metadata-only outcome was explicitly requested | Use metadata-only import and report its limited outcome; no unwanted download. |
| Download has a valid PDF signature and MD5 | Still inspect semantic work identity and verify the exact file association on disk and in active Emacs state. |
| Contained work lacks a parent entry | Search/create the correct parent first, verify crossref resolution and avoid guessing among editions. |

Run focused offline downloader regressions with:
`PYTHONDONTWRITEBYTECODE=1 python3 tests/test_download_annas_article.py`
from the dotfiles root. Live provider availability, active Emacs processing and
real document identity require separate authorized runtime evidence.
