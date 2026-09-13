---
name: add-bib-entry
description: Use when adding works to Pablo's bibliography, creating BibTeX/BibLaTeX entries, adding DOI/ISBN/URL references, resolving missing citekeys, replicating zotra-extras-add-entry followed by ebib-extras-process-entry, or preparing notes that cite works not yet in the configured bibliography files.
---

# Add Bib Entry

## Core Rule

Use Pablo's Emacs bibliography workflow, not ad hoc BibTeX, whenever a work has a DOI, ISBN, URL, or other identifier. The canonical sequence is `zotra-extras-add-entry`, then `ebib-extras-process-entry` with point on the imported entry. The agent should carry out both steps, including downloading and attaching the paper; do not stop after metadata import. The desired end state is a clean BibLaTeX entry plus obtainable associated files: PDF/HTML for written works and subtitles for videos. Import success alone does not establish correct metadata or attachment completion.

## Workflow

1. Check whether the work is already present.
   - Prefer `search_bibliography` if available.
   - Otherwise search active bibliography files with `rg -n -F -- "IDENTIFIER" BIBFILE...`, then check title/author and alternate identifier forms. Reuse a matching work's existing key; distinguish editions/versions before creating a duplicate.
   - Get active paths from Emacs rather than guessing:
     ```bash
     emacsclient -e '(progn (require '\''paths nil t) (mapcar (lambda (symbol) (cons symbol (and (boundp symbol) (symbol-value symbol)))) '\''(paths-file-personal-bibliography-new paths-file-personal-bibliography-old paths-files-bibliography-all citar-bibliography)))'
     ```

2. Choose the target BibTeX file.
   - For Pablo's personal notes, default to `paths-file-personal-bibliography-new`, normally `/Users/pablostafforini/My Drive/bibliography/new.bib`.
   - Do not add new personal references to `old.bib` or Babel/Tlön bibliography files unless the user or project context explicitly calls for that.
   - Resolve the actual target and active databases before writes. If Emacs is unavailable or paths are unset, diagnose that gap; the usual path is not evidence of the current configuration. Do not restart/signal Emacs or silently switch workflows.
   - Check for unsaved changes in the target's visiting buffers and loaded Ebib database before importing or reloading. Reconcile them without discarding or saving unrelated user edits; stop this addition if that cannot be done safely.

3. Add and process the entry through Zotra/Ebib.
   - If you need to inspect or reload an Elpaca package involved in this workflow, resolve it with `"/Users/pablostafforini/My Drive/dotfiles/bin/elpaca-package-path" PACKAGE`. For Anna's Archive source append `annas-archive annas-archive.el` to that quoted executable. Do not use `locate-library`, `symbol-file`, or `~/.emacs.d/elpaca/...` as the source authority.
   - Manual/full workflow: run `zotra-extras-add-entry` in Emacs. It imports metadata via Zotra/Zotero translators, prompts for the target bibfile, opens the entry in Ebib, and then runs the Ebib processing path.
   - Headless/agent workflow: for DOI articles, use the staged-download sequence below by default: import metadata first, stage and attach the verified PDF with the existing website downloader, then run the actual Ebib processing path. For other routes where Emacs will obtain the attachment, use `add_bib_entry_and_process`. It imports through Zotra, opens the generated key in Ebib, runs the Ebib processing path noninteractively, waits a bounded time for attachment work, and returns the key plus attached-file status. Pass the resolved target explicitly. Do not start two competing downloads.
   - Headless Emacs template (replace both placeholders with properly escaped Lisp strings; prefer structured tool arguments for untrusted identifiers):
     ```bash
     emacsclient -e '(progn (require '\''gptel-extras) (gptel-extras-add-bib-entry-and-process "IDENTIFIER" "RESOLVED-BIBFILE"))'
     ```
   - The headless helper calls the actual `ebib-extras-process-entry` and answers only recognized workflow prompts. It refuses ambiguous choices instead of selecting the first candidate. Inspect the imported entry and candidate metadata to resolve ordinary choices yourself; ask Pablo only when his preference is needed. If processing stops after import, resume processing the existing entry with `gptel-extras--process-bib-entry-headless` once the cause is resolved; do not import a duplicate. Save and re-read the final database.
   - Process one addition at a time. Anna's Archive attachment callbacks use a shared pending key. A returned result or attachment timeout does not prove the callback is finished: reconcile outstanding work before another addition so it cannot attach the previous download to the next key.
   - `add_bib_entry` is metadata-only: it deliberately passes `DO-NOT-OPEN` to `zotra-extras-add-entry`, so it does not run Ebib post-processing or attach files. Use it as the first phase when obtaining a PDF through the selected website downloader below, and carry the remaining attachment and processing phases through. Otherwise use it only when the limited outcome was requested, or as a clearly labeled, justified and approved fallback.

4. Complete post-processing.
   - The full manual path calls `zotra-extras-open-in-ebib`, which confirms the entry type/key and invokes `ebib-extras-process-entry`.
   - `ebib-extras-process-entry` regenerates/validates the key, sets language, calls `ebib-extras-attach-files`, and checks crossrefs.
   - `ebib-extras-attach-files` chooses attachments from DOI, ISBN/book type, video URL, or online/article URL:
     - DOI: searches/downloads through Anna's Archive. This is the configured paper-download route; Sci-Hub is not a second implemented backend. Do not claim to have used it.
     - ISBN/book-like entries: searches/downloads through Anna's Archive.
     - Video URLs: obtains subtitle files, not a paper PDF.
     - Online/article URLs: generates PDF and HTML files with `eww-extras-url-to-file`.
   - Anna's Archive downloads may fall back to the external browser. If the returned file list is empty or incomplete, inspect the `file` field and relevant download job. Match a candidate in Downloads/library by work identity and job evidence, never merely by recency. An existing but stale `file` field can prevent normal attachment; diagnose it rather than repeatedly importing the work.
   - Read `/Users/pablostafforini/My Drive/dotfiles/claude/context/service-access.md` before direct service access and use its mapped `annas-mcp` route when available. Read `/Users/pablostafforini/My Drive/dotfiles/claude/context/secrets.md` before any credential-backed action. Do not print credentials or signed download URLs.
   - For article PDF downloads, reuse `/Users/pablostafforini/repos/stafforini.com/scripts/download-missing-pdfs.py`. Pablo has selected this existing backend for the agent workflow. Use its article mode with a fresh staging directory outside Drive. Keep Zotra/Ebib responsible for metadata and attachment; diagnose an unavailable Emacs session before changing bibliography state:
     ```bash
     python3 /Users/pablostafforini/repos/stafforini.com/scripts/download-missing-pdfs.py --article-doi "DOI" --out "STAGING-DIRECTORY"
     ```
     This mode downloads files only: it does not import metadata or update `new.bib`. Use its established credential mechanism, not a key pasted into command arguments. The hardened version (website commit `b61b79f`) accepts only actual result cards with matching Sci-Hub DOI filenames and an unambiguous record link; it excludes unrelated recent-download links. HTTP failures, bot challenges and unrecognized result layouts leave availability unknown. A recognized no-results response means that query returned no files, not proof that the work is unavailable everywhere. Fast-download diagnostics omit credentials and signed URLs; do not reconstruct or print them while investigating errors.
   - For a PDF obtained through that downloader, a publisher/author source, or another authorized route, inspect its title, authors, DOI, version and language before attaching it. Open and validate the intended Ebib entry/database and set its `langid` to the verified document language before attachment; do not leave a missing importer language to an interactive prompt or assume every paper is English. Then call `ebib-extras-attach-file` with the explicit staged path and citekey (and `t` for PDF post-processing). Return to that same entry and finish the actual `ebib-extras-process-entry` path through the headless helper, passing the key and database explicitly. Save and verify the database and attachment. Do not hand-edit `new.bib` or select the most recent download as a substitute for this attachment step.
   - Re-read the generated entry and do targeted metadata cleanup only for fields the programmatic path missed:
     - Site-hosted articles, blog posts, and other `@online` works need `journaltitle` set to the site/publication name, such as `Planned Obsolescence`. Zotra often omits this field; add it manually when missing.
     - Seek a publication year for every work; `date = {forthcoming}` alone is insufficient. Prefer publisher/DOI metadata, announcements or scheduled issue data. If only an evidence-based estimate exists, record its uncertainty and basis rather than presenting it as a known publication date. If no defensible year exists, leave the entry explicitly incomplete and report that gap; never invent a year to satisfy the convention.
     - Works contained in larger works (`@incollection`, `@inbook`, `@bookinbook`, chapters, encyclopedia entries, stories in collections, and similar cases) should cross-reference the larger work. Search active bibliography files for the parent title/editor/publisher/year; if the parent is missing, add it first. Then set `crossref = {ParentKey}` on the contained work and keep parent-level metadata on the parent entry. Search existing entries with `rg -n "crossref = \\{|@incollection|@inbook|@bookinbook" BIBFILE` for local patterns.

5. Use the returned/generated citekey in notes.
   - Cite as `[cite:@Key]`.
   - After editing notes, verify every citekey resolves in the active bibliography files.

## Fallbacks

- If Zotra cannot import the work, inspect the relevant implementation before hand-writing BibTeX:
  - `/Users/pablostafforini/My Drive/dotfiles/emacs/extras/zotra-extras.el`
  - `/Users/pablostafforini/My Drive/dotfiles/emacs/extras/ebib-extras.el`
  - Anna's Archive download behavior: run `"/Users/pablostafforini/My Drive/dotfiles/bin/elpaca-package-path" annas-archive annas-archive.el` and inspect the returned file.
  - package docs in `/Users/pablostafforini/My Drive/dotfiles/emacs/extras/doc/zotra-extras.org` and `ebib-extras.org`
- Manual BibTeX is acceptable only for genuinely unsupported cases, after the Zotra/Ebib path and relevant implementation have been inspected and the labeled fallback is justified and approved. Preserve the same metadata requirements, including the evidence/uncertainty rule for dates, `journaltitle` for site-hosted works, parent `crossref` entries, valid key style, required fields and citation resolution.
- Exhaust available in-scope agent paths before declaring an attachment blocked. If a credential, unavailable live session or genuinely ambiguous candidate still prevents completion, preserve the entry and report the exact missing piece. Do not substitute a different work or claim the attachment is complete.

## Verification

Before finishing:

- Re-read the added BibTeX entry and cited note lines.
- Confirm the citekey appears in one active bibliography file.
- Confirm every expected `file` field resolves to the intended nonempty PDF/HTML/subtitle file, and inspect content identity rather than trusting its name or extension. Reconcile unfinished attachment callbacks before reporting completion.
- Check both disk and the active Ebib/visiting buffer after any approved external edit; do not overwrite unsaved edits to force them into agreement.
- Keep unrelated user changes in bibliography files unstaged/uncommitted unless explicitly asked.
- If committing, commit bibliography and note changes separately when they live in different repos.
