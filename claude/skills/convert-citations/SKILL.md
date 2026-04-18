---
name: convert-citations
description: Convert raw bibliographic references in a `.org` file to org-mode `[cite:@KEY]` citations by looking each work up in the user's bibliography. Use when the user says "convert citations", "convert references", "citar-ify this bibliography", or mentions needing to replace raw references in a bibliography .org file with org citations.
argument-hint: "[bibliography-file]"
user-invocable: true
---

# Convert citations

Replace raw bibliographic references in an org file with `[cite:@KEY]` citations, resolving each work against the user's citar bibliography via `emacsclient -e`. Mirrors the behaviour of `tlon-bib-replace-citations-in-file` but runs natively in Claude Code (more reliable than the gptel-based Elisp for agentic lookups).

## Arguments

`$ARGUMENTS` is the absolute path to the `.org` file to convert. If empty, ask the user for the path.

Canonical style reference: `~/My Drive/notes/pablos-miscellany/c-d-broad-a-bibliography.org` — each citation is a list item of the form `- [cite:@KEY]` (with optional locators or multiple keys).

## Output format

- Single work: `[cite:@KEY]`
- With locator: `[cite:@KEY, pp. 123–145]` (or `ch. 5`, `§3`, etc.)
- Multiple consecutive: `[cite:@KEY1;@KEY2;@KEY3]`
- Bold/italic/quotes around the citation are preserved (e.g. `*[cite:@KEY]*` stays bold)

Do NOT introduce `<Cite bibKey="..." />` XML-style tags. That format is for Markdown; this skill targets org-mode only.

## Procedure

### 1. Read the file

Read the full target file. Identify the region(s) that contain references — typically a bulleted list under a heading, but may also be inline paragraph citations. Do NOT touch:

- `:PROPERTIES:` drawers, `#+title:`, `#+hugo_base_dir:`, and other org metadata
- Paragraphs of commentary that aren't references
- Already-converted citations (lines that already contain `[cite:@...]`)
- External org links like `[[https://...][text]]` unless they're part of a citation (see step 3)

### 2. Verify citar cache is populated

Before looking up any keys, ensure the citar cache is non-empty:

```bash
emacsclient -e '(hash-table-count citar-cache--bibliographies)'
```

If it returns `0`, ask the user to run `M-x citar-insert-citation` (or press `H-/`) once to populate the cache, then retry. Do not attempt lookups against an empty cache.

### 3. Find the BibTeX key for each reference

For each raw reference in the file, try to match an entry in the bibliography. Strategies in order of preference:

**a. Exact title match**

```bash
emacsclient -e '(tlon-bibliography-lookup "title" "EXACT TITLE HERE" "=key=")'
```

Returns the key as a quoted string (e.g. `"Broad1906PhilosophyOmarKhayyam"`) on match, `nil` on miss.

Escape double quotes in the title by replacing `"` with `\"` before passing to emacsclient.

**b. Substring title match** (if exact fails)

```bash
emacsclient -e '(tlon-bibliography-lookup "title" "DISTINCTIVE SUBSTRING" "=key=" t)'
```

The trailing `t` enables substring matching. Choose a substring distinctive enough to avoid false positives (at least 4-5 words, or a unique phrase).

**c. DOI match** (if the reference includes a DOI)

```bash
emacsclient -e '(tlon-bibliography-lookup "doi" "10.1234/abc" "=key=")'
```

**d. URL-based lookup** (if the reference is a link without explicit metadata)

Use `WebFetch` to open the URL and extract the title/author/DOI, then retry (a)-(c).

**e. Web search** (last resort)

Use `WebSearch` to find the canonical title/DOI/year for an ambiguous reference, then retry (a)-(c).

### 4. Apply replacements

Build the list of replacements — map each raw reference to `[cite:@KEY]` (with locators preserved). Apply via `Edit` tool in a single pass per region.

**If a work is not found**: leave the original reference untouched. Do NOT fabricate a key, do NOT invent entries, do NOT add `{!...!}` placeholder markers. Just skip it.

**If the reference already contains formatting** (bold, italic, parenthetical, quote marks), preserve the formatting around the citation:

- `*Grover (1998). Cosmological fecundity.*` → `*[cite:@Grover1998CosmologicalFecundity]*`
- `(Edwards, 1967)` → `([cite:@Edwards1967Why])`

**Locators** (page numbers, chapters, sections): append them to the citation with a comma:

- `Blackburn (2009), pp. 132–141.` → `[cite:@Blackburn2009Philosophy, pp. 132–141]`
- `Conee (2005), ch. 5` → `[cite:@Conee2005RiddlesExistence, ch. 5]`

Match locator style to what's already in the canonical example file.

### 5. Report

After applying the edits, summarise:

- How many references were found and converted
- How many were left untouched (and a short list of the skipped ones — title or first few words — so the user can decide whether to add them to the bibliography)
- If any lookups required WebFetch/WebSearch, note which ones (these are the most likely to be wrong)

Do NOT commit. The user commits when they've reviewed the result.

## Notes

- Batch of files: if the user passes a directory or glob, process one file at a time sequentially — each file's lookups and edits should complete before the next starts. (The Elisp equivalent parallelises; here, keep it sequential for interactive reliability.)
- The user's citar cache uses `=key=` as the field name for BibTeX keys — always pass that exact string (with the leading/trailing `=`) as the `assoc-field` argument.
- Per user preference: never send potentially blocking or long-running expressions via `emacsclient -e`. Keep each lookup call to a single form that returns a small string or nil.
- If a reference is ambiguous (multiple potential matches), ask the user rather than guess. False-positive citations are worse than unchanged references.
- Do not re-run conversion on a file that's already fully converted — if you see `[cite:@...]` patterns throughout and no raw references, stop and tell the user the file appears done.
