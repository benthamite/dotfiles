# Bibliography policy

This is the shared source of bibliographic decisions for Pablo's personal
bibliography. `add-bib-entry` owns import, metadata changes and attachment through
Emacs; `clean-citations` owns selecting and replacing references in notes.
Download tools implement acquisition, and `bib-entry-check` enforces the
structural rules below. Both Claude and Codex read this same file. Change a
decision here instead of copying it into skills, project instructions or logs.

## Which work to add

- Add the work supporting the passage, not automatically the page hosting a
  copy. Distinguish the primary work, a translation, a selection/anthology,
  commentary and a genuinely independent secondary source.
- A DOI landing page and an institutional PDF of the same publication describe
  one work. Different editions, translations and separately authored commentary
  need distinct identity checks before any reuse.
- Add a secondary source when its analysis or testimony is actually used.
  Verify a primary passage before replacing an indirect attribution with a
  direct citation. Preserve "quoted in" provenance when the primary was not
  checked.
- A work merely mentioned inside a quotation/example is not thereby a source
  for the note. Navigation, licences and download mirrors do not automatically
  require bibliography entries. Preserve links serving those purposes.
- For a separately cited chapter, essay or entry, add its correct container
  first and use a resolving `crossref`. Keep container metadata on the parent;
  use the chapter's own author/title and applicable locators.
- Search all applicable configured bibliography files before import. Reuse an
  entry only after matching work and edition, including creator roles and
  language. A matching title or collision-free key is insufficient.
  Existing entries illustrate practice; they are not evidence that their
  metadata meets this policy. Fix only the selected entry within the task's
  scope, and do not rewrite unrelated citations when a later edition exists.

## Editions and evidence

Pablo adds first editions. Establish the first publication of the selected work
from the work's title/copyright pages and authoritative publisher or library
records. A convenient current ISBN, reprint or Google Books result is not a
reason to choose a later edition. For posthumously published journals, distinguish
the dates of the entries from the publication date of the book.

Keep all edition-specific fields coherent: title, publication date, publisher,
place, edition, language, translator/editor, ISBN and pagination. Never add a
reprint's ISBN to an earlier edition, or change only a date to make a later
edition appear to be the first. Pre-ISBN first editions need no invented ISBN.
Validate imported metadata against the selected edition before finalizing its
key.

Distinguish an edition from a printing or impression. An unchanged later
printing of the first edition may supply its attachment and locators when the
publication pages and other authoritative evidence establish the same edition,
text, layout and pagination. A printing number alone does not make it a later
edition; a matching title or ISBN alone does not establish equivalence. Record
the actual printing and its evidence in the attachment review, without claiming
it is the first printing or copying a later edition's identifiers into the
first-edition record. Revised or reset editions require their own identity and
locator checks.

Apply the first-edition rule to the work and version actually used. For a
quotation from a translation, select the first publication of that translation,
not a different earlier translation or an original-language edition that does
not contain the quoted wording. Record the original work's publication date in
`origdate` when established; do not invent an unidentified translator.

Prefer the original version when its relevant content and locators can be
verified. When the source actually used is a substantially revised work or an
independently edited transcription, select the first publication of that
version and preserve its earlier publication history. Distinguish manuscript
composition, first facsimile publication and first publication of a scholarly
transcription. Do not claim an earlier version lacks a passage merely because
its full text could not be inspected. Record that evidence limit and the
selected version explicitly instead of leaving an ordinary edition choice
pending. A directly specified edition in the current task takes precedence.

Preserve the evidence for the wording and locator actually quoted. A PDF viewer
page, printed page, folio and chapter/section number are different locators.
Do not transfer an anthology's pages to the original book. Verify the passage
and its locator in the selected first edition, or retain the accurate indirect
attribution and report that reference as unresolved. Never pretend an unchecked
original supports a directly verified quotation.

Prefer a documented original publication date. Treat publication updates,
journal-entry dates and upload timestamps separately from original publication.

For an undated webpage whose original publication date cannot be established,
use the year of its earliest verified Wayback Machine capture as the publication
year. Set `date` to `YYYY`, ignoring the capture's month and day. This is Pablo's
accepted dating convention: it estimates publication from archival evidence,
without claiming that the first capture was the actual publication date.
Check the original/canonical URL and known equivalent URL variants, and verify
that the earliest capture contains the same work rather than a redirect, error
or unrelated page. Retain the archive timestamp, snapshot URL, URL variants
checked and the estimated basis in the entry's review evidence. Keep the
original source URL in `url` and the actual access date in `urldate`.

If neither a documented publication date nor a qualifying Wayback capture is
available, leave the entry incomplete and the reference unconverted. Do not
invent a year to generate a key.

## Metadata acceptance

The JSON block below is the sole machine-readable list of structural field
requirements. `bib-entry-check` reads it directly. These are minima, not a
whitelist or a substitute for editorial review.

Check every field, including values already in an existing entry. Include
applicable volume, issue, pages, contributors, series/volume information,
identifiers and URLs when established. Use BibLaTeX field names, ISO dates,
`Surname, Given` creators joined by ` and `, and the verified `langid`.
Do not fill optional fields merely to maximize the field count. Distinguish an
unavailable fact from an inapplicable field, and record significant gaps in the
work's review evidence.

Keep relevant optional fields and intentional personal metadata. Remove only
values established to be wrong, irrelevant or import debris: HTML/navigation,
cookie notices, sales pitches presented as abstracts, placeholder content,
incorrect creator roles, or identifiers belonging to another work/edition.
A field-name blacklist alone cannot establish garbage.

An abstract must summarize this work's content. Inspect existing and imported
abstracts as well as generated ones; a nonempty field is not a pass. Preserve
a good original abstract. Remove misleading boilerplate, then obtain an actual
abstract or use the existing document-grounded generation path with its
AI-generated attribution when needed. Never summarize a whole book from an
unrepresentative excerpt or disguise generated text as the author's abstract.
If reliable material is unavailable, leave the abstract absent and report the
gap where it matters.

After processing, re-read the saved entry and review all fields again:
asynchronous import, abstract and attachment callbacks may have changed them.
Keep structural validation, semantic review, and attachment completion as
separate outcomes. None alone certifies the complete entry.

```json
{
  "bib_entry_check": {
    "version": 1,
    "types": {
      "book": {"required": ["title", "date", "publisher", "location", "langid"], "required_any": [["author", "editor"]]},
      "mvbook": {"required": ["title", "date", "publisher", "location", "langid"], "required_any": [["author", "editor"]]},
      "collection": {"required": ["title", "date", "editor", "publisher", "location", "langid"]},
      "reference": {"required": ["title", "date", "publisher", "location", "langid"], "required_any": [["author", "editor"]]},
      "article": {"required": ["author", "title", "journaltitle", "date", "langid"]},
      "online": {"required": ["author", "title", "journaltitle", "date", "url", "urldate", "langid"]},
      "incollection": {"required": ["author", "title", "booktitle", "editor", "publisher", "date", "langid"]},
      "inbook": {"required": ["author", "title", "booktitle", "publisher", "date", "langid"]},
      "bookinbook": {"required": ["author", "title", "booktitle", "publisher", "date", "langid"]},
      "inreference": {"required": ["author", "title", "booktitle", "publisher", "date", "langid"]},
      "proceedings": {"required": ["title", "date", "langid"]},
      "inproceedings": {"required": ["author", "title", "booktitle", "date", "langid"]},
      "thesis": {"required": ["author", "title", "date", "institution", "type", "langid"]},
      "report": {"required": ["author", "title", "date", "institution", "langid"]},
      "manual": {"required": ["title", "date", "langid"], "required_any": [["author", "organization"]]},
      "movie": {"required": ["author", "title", "date", "url", "langid"]},
      "video": {"required": ["author", "title", "date", "url", "langid"]}
    },
    "field_rules": {
      "date": "date", "origdate": "date", "urldate": "access_date",
      "doi": "doi", "isbn": "isbn", "url": "url"
    },
    "crossref_inheritance": {
      "booktitle": ["booktitle", "title"], "editor": ["editor"],
      "publisher": ["publisher"], "location": ["location"],
      "date": ["date"], "langid": ["langid"]
    },
    "forbidden_fields": {},
    "semantic_review_required": [
      "Work identity and inclusion are supported by the source.",
      "Edition and identifiers match the chosen publication.",
      "Applicable volume, issue, pages, contributors and other metadata are complete.",
      "Abstract, if present, summarizes the work and is not import boilerplate.",
      "Any attachment is the right work, edition and language; book pages and size preference require separate evidence."
    ]
  }
}
```

Unsupported entry types require an explicit policy decision; they do not pass
through a generic "misc" conversion. Films use the director as author; interviews
use the interviewer, with the interviewee recorded in `note`. For ambiguous
creator roles, consult the actual work rather than guessing from a title.

## Attachments

For books, eligibility comes before filesize:

1. Establish the correct work, selected edition and language.
2. Establish completeness and readable content, including figures, footnotes
   and the quoted passage when relevant.
3. Establish that the PDF preserves the physical book's pages and typography:
   a publisher PDF with the original layout or a scan is acceptable.
   EPUB/MOBI reflow converted to PDF is not.
4. Choose the smallest verified eligible copy among the candidates searched.

Inspect publication pages and rendered representative pages; compare printed
pagination, contents, ending and source page counts. Use extracted text, PDF
metadata and catalogue scan/vector/OCR flags as supporting evidence. Neither a
filename, a valid PDF header, OCR text, page count nor a size threshold proves
layout or completeness. Do not penalize a valid small publisher PDF just because
it is below 2 MB, or treat unknown size as zero. Compare measured file bytes.

Do not recompress, rasterize or convert a book to win the size comparison.
Record search scope and rejected/unknown candidates. Say "smallest verified
among the candidates searched" when exhaustive availability is unproven.
If a potentially better candidate cannot be inspected, report that limit;
never silently substitute another edition or an EPUB conversion.

Article, webpage and video attachment formats follow the existing acquisition
route: the prohibition on reflowed book PDFs does not prohibit archiving an
original web article as PDF/HTML. Keep the work's original source URL as
appropriate, not a signed download URL.

Store candidate reviews against the exact target and downloaded bytes. A review
of a previous file or different edition cannot approve a replacement. Keep
temporary downloads and review state outside Drive. Attach only through the
explicit Ebib operation and recheck identity after attachment/OCR; conversion
or OCR completion does not establish that the right work was acquired.

## Reference cases

These cases calibrate decisions; they do not establish unresearched metadata.

| Case | Required decision |
|---|---|
| DOI and institutional PDF for Krauthausen's article | Resolve one publication and one key; preserve the relevant page locator. |
| Woolf quoted on printed pp. 66–68 of an anthology | Identify original work and anthology; verify the original passage/locator before directly citing its first edition. |
| A blog transcribes Steinbeck's diary | Treat transcription as access/provenance; add the primary publication only after verification; distinguish independent commentary. |
| Thoreau entry dated 1851 in a later published volume | Distinguish composition date, volume publication and the quoted edition's pagination. |
| An existing 1990 book entry and a verified earlier first edition | Retain distinct edition identities; do not reuse the later record solely because its title matches. |
| Creative Commons licence link beside an excerpt | Keep the licence link; it is not automatically a source work to import. |
| Calibre PDF smaller than a complete publisher PDF | Reject the reflowed conversion; choose the eligible publisher PDF. |
| Genuine publisher PDF below 2 MB, larger scan, unknown-size candidate | Verify content/layout; compare actual bytes, never apply the old minimum-size heuristic. |
| Abstract contains a shop description or website navigation | Remove that content and obtain a work-grounded abstract if feasible; structural validity is insufficient. |
