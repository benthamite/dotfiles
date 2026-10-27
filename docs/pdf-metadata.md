# PDF metadata

Library PDFs carry the title, author and date of the bib entry they are
attached to. `bin/pdf-metadata-sync` is the one implementation:

- `ebib-extras-set-pdf-metadata` runs `pdf-metadata-sync write PDF --title …`
  with the entry's raw, crossref-resolved fields whenever a PDF is attached.
- The LaunchAgent `com.stafforini.pdf-metadata-sync` (registered in
  `~/repos/launchd`) runs `pdf-metadata-sync sync` nightly at 03:17 over every
  PDF attached to an entry in the bibliographies Ebib loads.

## Field mapping

| Bib field               | PDF Info       | XMP                              |
|-------------------------|----------------|----------------------------------|
| `title`                 | Title          | `dc:title`                       |
| `author`, else `editor` | Author         | `dc:creator` (one item per name) |
| `date`, else `year`     | CreationDate   | `dc:date`, `xmp:CreateDate`      |

Names become "First Last", joined with ", "; `others` becomes "et al.". LaTeX
in the values is converted to Unicode by a small stdlib converter that covers
the commands the bibliographies use (accents, escapes, `\textbar`, `\textit`
and similar). XMP dates keep the bib's precision (`2013`, `2013:05`); Info
CreationDate needs a full date, so missing parts become `01`, at 12:00 with no
zone so viewers in any zone show the same day. A field the entry lacks is left
untouched in the PDF. Crossref inheritance follows biblatex: a parent's title
is not inherited by in*/supp* children or from mv* parents.

A PDF attached to several entries takes the entry whose key is the file's
name; with none, it is skipped and listed as `ambiguous`.

## Nightly run

exiftool writes in place as an incremental update, so the previous metadata
remains recoverable inside the file. The run skips encrypted PDFs, PDFs
modified in the last hour (an attachment may be in progress) and missing files,
rewrites at most 1,000 PDFs (`--limit`), and verifies each write by reading it
back. `--dry-run` reports without writing.

When exiftool refuses a PDF (a broken xref table or Root object, or junk
before `%PDF-`) or its write does not read back, both `write` and `sync` fall
back to rewriting the file with qpdf in a temporary directory, writing the tags
to the rewrite and copying it over the original in place. The rewrite is used
only if its page count matches and the write reads back; it drops the file's
incremental-update history. If the tags still do not read back (an Info entry
that is not a string, as in `Butler1908Erewhon.pdf`), a second rewrite drops
the Info dictionary and copies back its Subject, Keywords, Creator and
Producer before writing. `sync` logs such files as `repaired` with the
exiftool error. A PDF neither tool can write stays untouched and counts as
`failed`.

`~/.local/state/pdf-metadata-sync/state.json` records, per PDF, its mtime and
size, a hash of the wanted tags, and the outcome. A PDF whose file and wanted
tags are both unchanged is not read again; bump `MAPPING_VERSION` in the
script when the mapping changes. A failed write is recorded and not retried
until the file or its entry changes, so the job exits 1, and the launchd audit
emails, once per new failure. `success` in the same directory is the heartbeat,
written after every run without a failure. Output goes to
`~/Library/Logs/pdf-metadata-sync.{out,err}.log`.

The backfill on 2026-10-25 found 15,759 of 16,095 attached PDFs needing an
update, so the first nights each rewrite 1,000 PDFs and Google Drive
re-uploads them.

The first nightly run, on 2026-10-27, failed on 40 PDFs. All were readable:
76 library PDFs began with the raw HTTP response of an Anna's Archive fast
download (saved by `annas-archive`'s eww path before its fix of 2026-02-15),
and 7 more had structure exiftool rejects. All 83 were repaired by hand that
day (headers stripped, qpdf rewrite where still damaged; originals in the
macOS Trash), and the qpdf fallback was added for the rest of the backfill.
