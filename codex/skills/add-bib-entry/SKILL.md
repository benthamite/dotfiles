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

5. Use the returned/generated citekey in notes.
   - Cite as `[cite:@Key]`.
   - After editing notes, verify every citekey resolves in the active bibliography files.

## Fallbacks

- If Zotra cannot import the work, inspect the relevant implementation before hand-writing BibTeX:
  - `/Users/pablostafforini/My Drive/dotfiles/emacs/extras/zotra-extras.el`
  - `/Users/pablostafforini/My Drive/dotfiles/emacs/extras/ebib-extras.el`
  - Anna's Archive download behavior: run `"/Users/pablostafforini/My Drive/dotfiles/bin/elpaca-package-path" annas-archive annas-archive.el` and inspect the returned file.
  - package docs in `/Users/pablostafforini/My Drive/dotfiles/emacs/extras/doc/zotra-extras.org` and `ebib-extras.org`
- Manual BibTeX is acceptable for genuinely unsupported cases such as forthcoming chapters without DOI/ISBN/URL metadata, but say that it is a fallback and verify key style, required fields, and citation resolution.
- If associated-file download/attachment requires live Emacs interaction, browser/authentication, or a manual choice between candidates, leave the entry in a clearly inspectable state and report the exact missing piece.

## Verification

Before finishing:

- Re-read the added BibTeX entry and cited note lines.
- Confirm the citekey appears in one active bibliography file.
- Confirm the `file` field references existing PDF/HTML files when expected.
- Keep unrelated user changes in bibliography files unstaged/uncommitted unless explicitly asked.
- If committing, commit bibliography and note changes separately when they live in different repos.
