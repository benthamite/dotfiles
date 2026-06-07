---
name: add-bib-entry
description: Use when adding works to Pablo's bibliography, creating BibTeX/BibLaTeX entries, adding DOI/ISBN/URL references, resolving missing citekeys, or preparing notes that cite works not yet in the configured bibliography files.
---

# Add Bib Entry

## Core Rule

Use Pablo's Emacs bibliography workflow, not ad hoc BibTeX, whenever a work has a DOI, ISBN, URL, or other identifier. The desired end state is a clean BibLaTeX entry plus associated files: always a PDF where the workflow can obtain one, and sometimes an HTML file for web pages.

## Workflow

1. Check whether the work is already present.
   - Prefer `search_bibliography` if available.
   - Otherwise search active bibliography files with `rg`.
   - Get active paths from Emacs rather than guessing:
     ```bash
     emacsclient -e '(progn (require '\''paths nil t) (list paths-file-personal-bibliography-new paths-file-personal-bibliography-old paths-files-bibliography-all citar-bibliography))'
     ```

2. Choose the target BibTeX file.
   - For Pablo's personal notes, default to `paths-file-personal-bibliography-new`, normally `/Users/pablostafforini/My Drive/bibliography/new.bib`.
   - Do not add new personal references to `old.bib` or Babel/Tlön bibliography files unless the user or project context explicitly calls for that.

3. Add and process the entry through Zotra/Ebib.
   - If you need to inspect or reload an Elpaca package involved in this workflow, resolve it with `/Users/pablostafforini/My Drive/dotfiles/bin/elpaca-package-path PACKAGE`. In particular, use `bin/elpaca-package-path annas-archive annas-archive.el` for Anna's Archive source. Do not use `locate-library`, `symbol-file`, or `~/.emacs.d/elpaca/...` as the source authority.
   - Manual/full workflow: run `zotra-extras-add-entry` in Emacs. It imports metadata via Zotra/Zotero translators, prompts for the target bibfile, opens the entry in Ebib, and then runs the Ebib processing path.
   - Headless/agent workflow: prefer `add_bib_entry_and_process` when available. It imports through Zotra, opens the generated key in Ebib, runs the Ebib processing path noninteractively, waits for asynchronous attachment work, and returns the key plus attached-file status.
   - Headless Emacs equivalent:
     ```bash
     emacsclient -e '(progn (require '\''gptel-extras) (gptel-extras-add-bib-entry-and-process "IDENTIFIER" "/Users/pablostafforini/My Drive/bibliography/new.bib"))'
     ```
   - Use `add_bib_entry` only as a metadata-only fallback. It deliberately passes `DO-NOT-OPEN` to `zotra-extras-add-entry`, so it does not run Ebib post-processing or attach files.

4. Complete post-processing.
   - The full manual path calls `zotra-extras-open-in-ebib`, which confirms the entry type/key and invokes `ebib-extras-process-entry`.
   - `ebib-extras-process-entry` regenerates/validates the key, sets language, calls `ebib-extras-attach-files`, and checks crossrefs.
   - `ebib-extras-attach-files` chooses attachments from DOI, ISBN/book type, video URL, or online/article URL:
     - DOI: searches/downloads through Anna's Archive.
     - ISBN/book-like entries: searches/downloads through Anna's Archive.
     - Online/article URLs: generates PDF and HTML files with `eww-extras-url-to-file`.
   - Anna's Archive downloads may fall back to the external browser. If the returned file list is empty or incomplete, inspect the `file` field and Downloads/library directories before reporting the exact missing attachment.
   - For DOI article PDFs, prefer the historical headless download path when live Emacs/EWW is fragile:
     ```bash
     python3 scripts/download_annas_article.py DOI CITEKEY "/Users/pablostafforini/My Drive/bibliography/new.bib"
     ```
     This mirrors `stafforini.com/scripts/download-missing-pdfs.py`: it reads the Anna's Archive key from `pass show tlon/core/annas-archive`, fetches the DOI SciDB page, extracts the MD5, downloads via `dyn/api/fast_download.json`, saves `~/My Drive/library-pdf/CITEKEY.pdf`, and inserts the `file` field. Read `claude/context/secrets.md` before invoking secret-backed commands, and never print the key.
   - Re-read the generated entry and do targeted metadata cleanup only for fields the programmatic path missed:
     - Site-hosted articles, blog posts, and other `@online` works need `journaltitle` set to the site/publication name, such as `Planned Obsolescence`. Zotra often omits this field; add it manually when missing.
     - Every work needs a publication year. Do not leave works undated, and do not use `date = {forthcoming}` as the only date. For forthcoming works, use the year that the available evidence makes most likely: publisher metadata, DOI metadata, announcement pages, scheduled issue data, or the best available estimate.
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
- Manual BibTeX is acceptable only for genuinely unsupported cases, and only after the Zotra/Ebib path and relevant implementation have been inspected. Say that it is a fallback and still enforce the same metadata invariants: a publication year for every work, `journaltitle` for site-hosted works, parent `crossref` entries for contained works, valid key style, required fields, and citation resolution.
- If associated-file download/attachment requires live Emacs interaction, browser/authentication, or a manual choice between candidates, leave the entry in a clearly inspectable state and report the exact missing piece.

## Verification

Before finishing:

- Re-read the added BibTeX entry and cited note lines.
- Confirm the citekey appears in one active bibliography file.
- Confirm the `file` field references existing PDF/HTML files when expected.
- Keep unrelated user changes in bibliography files unstaged/uncommitted unless explicitly asked.
- If committing, commit bibliography and note changes separately when they live in different repos.
