# Book acquisition interfaces

The shared [bibliography policy](../agents/bibliography-policy.md) owns edition,
metadata and attachment decisions. This document owns the `paper-fetch book-*`
command contract. The commands reuse `lib/python/paper_fetch.py`; they do not
write BibTeX or attach files to Ebib.

## Runtime

`bin/paper-fetch` uses `~/.local/share/paper-fetch/venv/bin/python`, a dedicated
Python 3.13 environment. Its complete dependency pins are in
[`paper-fetch-requirements.txt`](../lib/python/paper-fetch-requirements.txt).
Provision this environment deliberately from reviewed packages. The launcher
does not install dependencies, select ambient Python or use a uv cache path as
its runtime. Missing runtime/dependency errors are configuration failures.

For a missing environment, use an already installed Python 3.13:

```sh
uv venv --no-config --no-python-downloads --python 3.13 "$HOME/.local/share/paper-fetch/venv"
```

Once package installation and native execution meet the applicable agent policy,
install the declared wheels without build scripts or unpinned dependencies:

```sh
uv pip install --no-config --no-python-downloads --only-binary :all: --no-deps \
  --python "$HOME/.local/share/paper-fetch/venv/bin/python" \
  --requirements "$HOME/My Drive/dotfiles/lib/python/paper-fetch-requirements.txt"
uv pip check --no-config --python "$HOME/.local/share/paper-fetch/venv/bin/python"
```

The same install command updates an existing environment from the pins; it does
not recreate it. Verify the canonical `bin/paper-fetch` command afterward.

The shared HTTP client verifies TLS with the pinned `certifi` CA bundle by
default. Explicit `REQUESTS_CA_BUNDLE`, `CURL_CA_BUNDLE`, then `SSL_CERT_FILE`
settings retain their existing precedence. This avoids silently selecting an
older platform certificate file; it does not disable certificate verification.

The Python entry point is `lib/python/paper_fetch_cli.py`. Both launcher and
entry point disable bytecode writes so invoking the tool does not create a
cache under Drive. Keep candidate manifests, staged PDFs and inspection output
outside Drive until the Ebib attachment operation installs a selected file.

## Identify and discover

`paper-fetch book-candidates --target TARGET.json --out NEW-CANDIDATES.json`

The target identifies the already chosen edition. The tool validates the input
shape; it does not choose the first edition or prove the supplied identity.
`title`, `year`, `edition` and `language` are required strings. `author`, `isbn`
and `query` are optional strings; an ISBN must contain 10 or 13 digits (a final
X is allowed in ISBN-10). The year has four digits. `author` uses the ordinary
BibLaTeX name string. A query can identify a pre-ISBN work.

```json
{
  "title": "An Example Book",
  "author": "Smith, Alice",
  "year": "1960",
  "edition": "first",
  "language": "english"
}
```

Discovery queries the shared LibGen ISBN lookup when applicable and Anna's book
search. It reads no membership credential and downloads no book. Its output is
a version-1 manifest with `target`, `query`, `candidates`, `attempts`, `status`
and `search_complete`. Each candidate has a unique `md5`, observed metadata,
`format`, optional `size_bytes`, and provider `observations`. Unknown sizes are
null. Scan/vector/OCR flags and filenames remain observations, never approval.
Provider failures and unfamiliar layouts remain explicit in `attempts`.

The output file must be new. An empty recognized search yields `unavailable`;
an incomplete empty search yields `unknown`. Neither means global absence.

## Register an independently obtained file

`paper-fetch book-register CANDIDATES.json --file LOCAL.pdf --source PUBLIC-URL --out NEW-CANDIDATES.json`

This adds a publisher/archive PDF that is absent from Anna/LibGen results. It
measures MD5, SHA256 and bytes, records the source URL and local path, and leaves
observed title/author/edition/year/language blank. It grants no approval and
does not change the source file or the input manifest. A matching existing MD5
gains the local source observation instead of becoming a duplicate candidate.

Use a permanent public landing or file URL. Userinfo, private/local IPs, local
hosts and credential-like fragments are rejected. Query keys are limited to
`id`, `isbn`, `doi`, `page`, `pid`, `lang` and `download`; use the canonical landing
URL when a download URL contains signed/session parameters. URL shape validation
does not prove availability or bibliographic identity.

## Stage and inspect one candidate

`paper-fetch book-stage CANDIDATES.json --md5 MD5 --out STAGING-DIR [--file LOCAL.pdf] [--name STEM]`

The MD5 must name a PDF candidate in the inventory. A registered local candidate
uses its recorded file; `--file` supplies a browser download explicitly. Local
staging copies and preserves the source, refuses destination collisions, and
checks the MD5 plus any registration SHA256. Otherwise staging uses the existing
paper downloader for exactly that MD5. It does not choose another book.

The JSON result includes `file`, `md5`, `sha256`, `size_bytes`, `target`,
`checks_required`, `attempts` and `status: needs-review`. Staging integrity is a
PDF-signature/hash check, not a decision about the document. It has no minimum
book-size heuristic. Existing remote route statuses retain their normal meaning.

`paper-fetch book-inspect PDF --out NEW-INSPECTION-DIR [--pages 1,2,3,last]`

Inspection requires `pdfinfo`, `pdftotext` and `pdftoppm`. It writes metadata,
extracted text, rendered page PNGs and `inspection.json`. The default renders
pages 1–3 that exist and the last page; request additional copyright, contents
and interior pages when the review needs them. Page arguments are one-based.
Output includes exact hashes, total pages and paths to the images/text/metadata.
It remains `needs-review`: rendering is evidence for an agent to inspect.

## Review and compare

`paper-fetch book-select CANDIDATES.json --reviews REVIEWS.json`

The review is a version-1 object with the same `target` identity and a
`candidates` object keyed by inventory MD5. For an eligible file, provide its
current absolute `file`, `sha256`, and these five evidence objects:

```json
{
  "version": 1,
  "target": {"title": "An Example Book", "author": "Smith, Alice", "year": "1960", "edition": "first", "language": "english"},
  "candidates": {
    "0123456789abcdef0123456789abcdef": {
      "file": "/absolute/staging/candidate.pdf",
      "sha256": "REPLACE_WITH_INSPECTED_SHA256",
      "identity": {"status": "unknown", "evidence": ""},
      "edition": {"status": "unknown", "evidence": ""},
      "language": {"status": "unknown", "evidence": ""},
      "completeness": {"status": "unknown", "evidence": ""},
      "physical_pages": {"status": "unknown", "evidence": ""}
    }
  }
}
```

Use `verified` with actual supporting evidence after applying the shared policy.
Every check must have positive status and nonempty evidence. `rejected` with
evidence on any check excludes a candidate without requiring a download. Missing,
empty or other statuses stay pending. The tool enforces this structure; it does
not independently establish the truth of semantic evidence.

Selection checks the current MD5/SHA256 and measures current file size. Only
eligible reviewed files enter size comparison, irrespective of reported size or
filename. Unknown reported sizes cannot beat measured sizes. Output contains
`selected`, `eligible`, `pending`, `rejected`, `search_complete`, `attempts` and
`selection_scope`. The scope is the reviewed eligible pool. A selection does not
resolve pending candidates or incomplete provider searches; retain those limits
in the workflow's completion decision.

The selected file still needs the explicit Zotra/Ebib attachment operation.
Changing or OCRing bytes invalidates the review's original hashes; preserve the
operation's provenance and verify the installed document through that workflow.

## Exit statuses

| Code | Status | Meaning |
|---|---|---|
| 0 | `ok` | A reviewed eligible file was selected |
| 1 | `error` | Configuration, input or local operation failure |
| 2 | `needs-browser` | Existing downloader needs its browser route |
| 3 | `not-member` | Existing downloader reports lapsed membership |
| 4 | `unavailable` | Recognized searches/routes produced no candidate |
| 5 | `needs-review` | Normal discovery, registration, staging or inspection; or no eligible reviewed file |
| 6 | `unknown` | Search was incomplete and produced no candidates |

All book commands print JSON. `get`, `collect` and their article API remain
unchanged. The website's `download-missing-pdfs.py` is a read-only
discovery/review adapter; its heuristic acceptance and direct-BibTeX modes are
retired. Scheduled acquisition uses `download-missing-pdfs-batch.py` to run
agent review and the explicit Ebib attachment workflow. These checks do not
require human approval of every file or prohibit unattended acquisition.
