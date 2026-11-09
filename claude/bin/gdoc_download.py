"""Extend the installed gdoc CLI: original-file downloads, an empty-write guard,
comment threads on every `cat`, token writes that parallel runs can share, and
round bullets in documents made from Markdown.

Drive blobs use files.get_media; native Workspace documents still use export.
https://developers.google.com/workspace/drive/api/guides/manage-downloads

Upstream `write` and `push` replace a document with whatever the file holds,
including nothing; on 11 Oct 2026 a parsing slip blanked a live doc that way.

Upstream `cat` shows comments only on request, and its `--comments` view
refuses `--tab`. A doc's comments often hold what its body leaves out (on
14 Oct 2026 a read of the retreat doc missed 232 threads), so `cat` appends
them unless `--no-comments` is given.

Upstream saves every refreshed token through one fixed `token.json.tmp`, so
parallel runs that refresh the same expired token collide and the losers
report "Not authenticated"; `write_private` gives each write its own file.

Drive's own Markdown import, which `new --file` and `write` and `push` without
`--tab` use, keeps a `-` or `+` list marker as the bullet glyph, so lists show
hyphens; a `*` marker imports as ● ○ ■ (checked 8 Nov 2026, when Pablo asked
for bullets instead of hyphens). `star_bullets` rewrites the markers before
those uploads. gdoc's own parser (`insert`, `write --tab`) already makes round
bullets.

The same import gives every table evenly distributed columns, so a column of
one-word cells is as wide as a column of paragraphs. After those uploads,
`fit_tables` gives each table fixed column widths that minimise its height in
lines (asked by Pablo on 9 Nov 2026), and `fit-tables` does it for an existing
doc.
"""
import argparse
import contextlib
import functools
import hashlib
import html
import io
import json
import math
import os
from pathlib import Path
import random
import re
import struct
import sys
import tempfile

from gdoc import auth, cli
from gdoc.api import drive, get_drive_service
from gdoc.api.drive import _translate_http_error
from gdoc.frontmatter import parse_frontmatter
from gdoc.util import GdocError
from googleapiclient.errors import HttpError
from googleapiclient.http import MediaIoBaseDownload


def download_file(file_id, output):
    """Install original bytes only after a complete, checked download.

    Exclusive linking also prevents replacing an output created during download.
    Temporary data stays beside the destination so the link is atomic.
    """
    output = Path(output).expanduser()
    if os.path.lexists(output):
        raise GdocError(f"destination already exists: {output}", exit_code=3)
    temporary = None
    try:
        files = get_drive_service().files()
        metadata = files.get(
            fileId=file_id, supportsAllDrives=True,
            fields="id,name,mimeType,size,md5Checksum,capabilities(canDownload)",
        ).execute()
        if metadata["mimeType"].startswith("application/vnd.google-apps."):
            raise GdocError("native Google Workspace files require gdoc export")
        if not metadata.get("capabilities", {}).get("canDownload", False):
            raise GdocError(f"download is not permitted: {file_id}")
        with tempfile.NamedTemporaryFile(
            mode="w+b", prefix=".gdoc-download-", dir=output.parent, delete=False,
        ) as stream:
            temporary = Path(stream.name)
            downloader = MediaIoBaseDownload(
                stream, files.get_media(fileId=file_id, supportsAllDrives=True),
            )
            done = False
            while not done:
                _, done = downloader.next_chunk()
            stream.flush()
            stream.seek(0)
            checksum = hashlib.file_digest(stream, "md5").hexdigest()
            size = stream.seek(0, os.SEEK_END)
        if "size" in metadata and size != int(metadata["size"]):
            raise GdocError("download size differs from Drive metadata; file not saved")
        if metadata.get("md5Checksum") and checksum != metadata["md5Checksum"]:
            raise GdocError("download checksum differs from Drive metadata; file not saved")
        os.link(temporary, output)
        return {"id": file_id, "name": metadata["name"], "mime_type": metadata["mimeType"],
                "path": str(output), "bytes": size, "md5": checksum}
    except HttpError as error:
        _translate_http_error(error, file_id)
    except OSError as error:
        raise GdocError(f"cannot save {output}: {error}", exit_code=3) from error
    finally:
        if temporary is not None:
            temporary.unlink(missing_ok=True)


def cmd_download(args):
    result = download_file(cli._resolve_doc_id(args.doc), args.output)
    if getattr(args, "json", False):
        print(json.dumps(result))
    elif getattr(args, "plain", False):
        for key, value in result.items():
            print(f"{key}\t{value}")
    else:
        print(f"Downloaded {result['name']} ({result['bytes']} bytes) to {result['path']}")
    return 0


def refuse_empty_body(command):
    """Wrap a whole-document upload so an empty source cannot blank the doc.

    Unreadable files pass through so upstream reports them in its usual way.
    """
    def guarded(args):
        if not getattr(args, "allow_empty", False):
            try:
                text = Path(args.file).read_text(encoding="utf-8")
            except OSError:
                return command(args)
            if not parse_frontmatter(text)[1].strip():
                raise GdocError(
                    f"{args.file} has no content; writing it would blank the document "
                    "(pass --allow-empty to clear it deliberately)", exit_code=3)
        return command(args)
    return guarded


_FENCE = re.compile(r"^ {0,3}(`{3,}|~{3,})(.*)$")
_THEMATIC_BREAK = re.compile(r"^\s{0,3}([-*_])(?:\s*\1){2,}\s*$")
_DASH_OR_PLUS_ITEM = re.compile(r"^((?:\s*>)*)(\s*)[-+](\s+\S)")
_LIST_ITEM = re.compile(r"^(?:\s*>)*\s*(?:[-+*]|\d+[.)])\s")


def star_bullets(markdown):
    """Rewrite `-` and `+` list markers as `*`, outside code and maths.

    Drive's Markdown import keeps `-` and `+` as the glyph itself; `*` becomes
    a round bullet. Left alone: fenced and indented code, `$$` maths, `<pre>`
    blocks, leading YAML front matter, and thematic breaks such as `---`.
    """
    lines, fence, verbatim, in_list = [], None, None, False
    source = markdown.split("\n")
    if source and source[0].strip() == "---":
        for i in range(1, len(source)):
            if source[i].strip() in ("---", "..."):
                lines, source = source[:i + 1], source[i + 1:]
                break
    for line in source:
        opener = _FENCE.match(line)
        if fence is not None:
            if (opener and opener.group(1)[0] == fence[0]
                    and len(opener.group(1)) >= len(fence) and not opener.group(2).strip()):
                fence = None
        elif verbatim is not None:
            if verbatim in line:
                verbatim = None
        elif opener:
            fence = opener.group(1)
        elif line.strip().startswith("$$"):
            if line.strip() == "$$" or not line.strip().endswith("$$") or line.strip() == "$$$$":
                verbatim = "$$"
        elif line.lstrip().lower().startswith("<pre") and "</pre>" not in line.lower():
            verbatim = "</pre>"
        elif line.strip() and not _THEMATIC_BREAK.match(line):
            item = _DASH_OR_PLUS_ITEM.match(line)
            indent = len(item.group(2).expandtabs(4)) if item else 0
            if item and (indent < 4 or in_list):
                line = _DASH_OR_PLUS_ITEM.sub(r"\1\2*\3", line, count=1)
            if _LIST_ITEM.match(line):
                in_list = True
            elif not line[:1].isspace():
                in_list = False
        lines.append(line)
    return "\n".join(lines)


def with_star_bullets(upload):
    """Wrap a Drive Markdown upload (`create_doc_from_markdown`,
    `update_doc_content`), whose second argument is the Markdown, so its lists
    import with round bullets."""
    def wrapped(target, content, *args, **kwargs):
        return upload(target, star_bullets(content), *args, **kwargs)
    wrapped.__name__, wrapped.__doc__ = upload.__name__, upload.__doc__
    return wrapped


FONT_DIRECTORIES = (Path("/System/Library/Fonts/Supplemental"), Path("/System/Library/Fonts"),
                    Path("/Library/Fonts"), Path.home() / "Library/Fonts")
BREAK_AFTER = "-–—/"      # Docs can also wrap after a hyphen, dash or slash
TEXT_SAFETY_PT = 2                  # Docs lays text out a little wider than raw advances
MIN_COLUMN_PT = 20
SPLIT_PENALTY_LINES = 1             # a word broken mid-word costs as much as a line
DEFAULT_PADDING_PT = 5              # Docs' default cell padding on each side
RANDOM_STARTS = 12


class FontMetrics:
    """Advance widths read from a TrueType file's `cmap` (format 4) and `hmtx`
    tables, so text can be measured without a font library in gdoc's Python."""

    def __init__(self, path):
        data = Path(path).read_bytes()
        (count,) = struct.unpack_from(">H", data, 4)
        tables = {}
        for i in range(count):
            tag, _, offset, _ = struct.unpack_from(">4sIII", data, 12 + 16 * i)
            tables[tag.decode("latin-1")] = offset
        missing = {"head", "hhea", "hmtx", "cmap"} - tables.keys()
        if missing:
            raise GdocError(f"{path}: no {', '.join(sorted(missing))} table")
        (self.units_per_em,) = struct.unpack_from(">H", data, tables["head"] + 18)
        (metrics,) = struct.unpack_from(">H", data, tables["hhea"] + 34)
        self.advances = [struct.unpack_from(">H", data, tables["hmtx"] + 4 * i)[0]
                         for i in range(metrics)]
        self.glyphs = self._character_map(data, tables["cmap"], path)

    @staticmethod
    def _character_map(data, base, path):
        (count,) = struct.unpack_from(">H", data, base + 2)
        for i in range(count):
            platform, encoding, offset = struct.unpack_from(">HHI", data, base + 4 + 8 * i)
            table = base + offset
            if ((platform, encoding) in ((3, 1), (0, 3))
                    and struct.unpack_from(">H", data, table)[0] == 4):
                break
        else:
            raise GdocError(f"{path}: no format-4 Unicode character map")
        (segments_x2,) = struct.unpack_from(">H", data, table + 6)
        ends = table + 14
        starts = ends + segments_x2 + 2
        deltas = starts + segments_x2
        range_offsets = deltas + segments_x2
        glyphs = {}
        for s in range(segments_x2 // 2):
            (end,) = struct.unpack_from(">H", data, ends + 2 * s)
            (start,) = struct.unpack_from(">H", data, starts + 2 * s)
            (delta,) = struct.unpack_from(">H", data, deltas + 2 * s)
            (range_offset,) = struct.unpack_from(">H", data, range_offsets + 2 * s)
            for code in range(start, min(end, 0xFFFE) + 1):
                if range_offset:
                    at = range_offsets + 2 * s + range_offset + 2 * (code - start)
                    (glyph,) = struct.unpack_from(">H", data, at)
                    glyph = (glyph + delta) % 65536 if glyph else 0
                else:
                    glyph = (code + delta) % 65536
                glyphs[code] = glyph
        return glyphs

    def width(self, text, size):
        """Width of TEXT in points at SIZE points; unmapped characters count
        as the font's missing-glyph box."""
        last = len(self.advances) - 1
        units = sum(self.advances[min(self.glyphs.get(ord(c), 0), last)] for c in text)
        return units * size / self.units_per_em


class MonospaceMetrics:
    """Every character 0.6 em wide: the advance of the usual code fonts
    (Roboto Mono, which Docs uses for inline code, and Source Code Pro)."""

    def width(self, text, size):
        return 0.6 * size * len(text)


@functools.lru_cache(maxsize=None)
def font_metrics(family, bold):
    """Metrics for FAMILY from a local TrueType file.  Docs' web fonts are often
    not installed: a monospace family is measured at 0.6 em per character, and
    any other missing family with Arial's metrics, with a warning."""
    names = [f"{family} Bold.ttf"] if bold else [f"{family}.ttf", f"{family} Regular.ttf"]
    for directory in FONT_DIRECTORIES:
        for name in names:
            if (directory / name).is_file():
                return FontMetrics(directory / name)
    if "mono" in family.lower():
        return MonospaceMetrics()
    if family == "Arial":
        raise GdocError("no local Arial TrueType file, so table text cannot be measured")
    print(f"WARN: no local font file for {family}; measuring it with Arial's metrics",
          file=sys.stderr)
    return font_metrics("Arial", bold)


def _run_font(text_style, base):
    size = (text_style.get("fontSize") or base.get("fontSize") or {}).get("magnitude", 11)
    face = text_style.get("weightedFontFamily") or base.get("weightedFontFamily") or {}
    bold = text_style.get("bold", base.get("bold", False)) or face.get("weight", 400) >= 700
    return face.get("fontFamily", "Arial"), bool(bold), size


def _pieces(characters):
    """Unbreakable pieces of a paragraph as (width, gap) pairs in points, GAP
    being the width of the spaces before the piece."""
    pieces, word, gap = [], 0.0, 0.0
    for character, (family, bold, size) in characters:
        advance = font_metrics(family, bold).width(character, size)
        if character.isspace():
            if word:
                pieces.append((word, gap))
                word, gap = 0.0, 0.0
            gap += advance
        else:
            word += advance
            if character in BREAK_AFTER:
                pieces.append((word, gap))
                word, gap = 0.0, 0.0
    if word:
        pieces.append((word, gap))
    return pieces


def cell_paragraphs(cell, base):
    """A Docs API table cell as a list of paragraphs, each a list of pieces."""
    paragraphs = []
    for block in cell.get("content", []):
        paragraph = block.get("paragraph")
        if paragraph is None:
            continue
        characters = []
        for element in paragraph.get("elements", []):
            run = element.get("textRun")
            if run:
                font = _run_font(run.get("textStyle", {}), base)
                characters.extend((c, font) for c in run["content"] if c != "\n")
        paragraphs.append(_pieces(characters))
    return paragraphs or [[]]


def line_count(paragraphs, available):
    """Lines that PARAGRAPHS wrap to in AVAILABLE points of text width, wrapping
    greedily at piece boundaries and splitting a piece wider than the line."""
    if available <= 0:
        return 10 ** 6
    lines = 0
    for pieces in paragraphs:
        lines += 1
        x = None
        for width, gap in pieces:
            if x is not None and x + gap + width <= available:
                x += gap + width
                continue
            if x is not None:
                lines += 1
            overflow = max(0, math.ceil(width / available) - 1)
            lines += overflow
            x = width - overflow * available
    return lines


def split_count(paragraphs, available):
    """Words of PARAGRAPHS too wide for AVAILABLE points, which Docs breaks
    mid-word."""
    return sum(width > available for pieces in paragraphs for width, _ in pieces)


def _scaled(weights, total, minimum):
    """Integer widths at least MINIMUM, summing to TOTAL, roughly in proportion
    to WEIGHTS."""
    spare = total - minimum * len(weights)
    weights = [max(w, 1e-9) for w in weights]
    shares = [spare * w / sum(weights) for w in weights]
    widths = [minimum + math.floor(s) for s in shares]
    order = sorted(range(len(weights)), key=lambda i: shares[i] - math.floor(shares[i]), reverse=True)
    for i in order[:total - sum(widths)]:
        widths[i] += 1
    return widths


def fit_widths(cells, total, padding, minimum=MIN_COLUMN_PT):
    """Integer column widths, at least MINIMUM points and summing to TOTAL,
    that minimise a table's height.

    CELLS[r][c] holds the paragraphs of cell (r, c), PADDING[c] the horizontal
    padding of column c.  Height is the sum over rows of the tallest cell's
    line count, plus SPLIT_PENALTY_LINES for each word broken mid-word, so a
    column is narrowed past a word only when that saves more lines; ties go
    to fewer broken words, then to fewer lines over all cells, then to the
    smaller unwrapped-width estimate.  The search is a local search from
    several starting splits, so the result is a good split, not a proven
    optimum.
    """
    columns = len(cells[0])
    if minimum * columns > total:
        raise GdocError(f"{columns} columns cannot fit in {total} pt")
    cache = {}
    natural = [[max((sum(w + g for w, g in pieces) for pieces in cell), default=0.0)
                for cell in row] for row in cells]

    def layout(r, c, width):
        key = (r, c, width)
        if key not in cache:
            available = width - padding[c] - TEXT_SAFETY_PT
            cache[key] = (line_count(cells[r][c], available), split_count(cells[r][c], available))
        return cache[key]

    def cost(widths):
        height = all_lines = splits = 0
        estimate = 0.0
        for r in range(len(cells)):
            counts = [layout(r, c, widths[c]) for c in range(columns)]
            height += max(lines for lines, _ in counts)
            all_lines += sum(lines for lines, _ in counts)
            splits += sum(split for _, split in counts)
            estimate += max(natural[r][c] / max(widths[c] - padding[c], 1) for c in range(columns))
        return height + SPLIT_PENALTY_LINES * splits, splits, all_lines, estimate

    def improve(widths):
        best = cost(widths)
        for step in (32, 16, 8, 4, 2, 1):
            improved = True
            while improved:
                improved = False
                for i in range(columns):
                    for j in range(columns):
                        if i == j or widths[j] - step < minimum:
                            continue
                        widths[i] += step
                        widths[j] -= step
                        trial = cost(widths)
                        if trial < best:
                            best, improved = trial, True
                        else:
                            widths[i] -= step
                            widths[j] += step
        return best, widths

    widest = [max(row[c] for row in natural) for c in range(columns)]
    summed = [sum(row[c] for row in natural) for c in range(columns)]
    starts = [[1] * columns, widest, summed, [math.sqrt(w) for w in widest]]
    generator = random.Random(0)
    starts += [[generator.random() for _ in range(columns)] for _ in range(RANDOM_STARTS)]
    return min(improve(_scaled(weights, total, minimum)) for weights in starts)[1]


def _points(dimension, default=0.0):
    return (dimension or {}).get("magnitude", default)


def _horizontal_padding(cell):
    style = cell.get("tableCellStyle", {})
    return (_points(style.get("paddingLeft"), DEFAULT_PADDING_PT)
            + _points(style.get("paddingRight"), DEFAULT_PADDING_PT))


def _tabs(tabs):
    for tab in tabs:
        yield tab
        yield from _tabs(tab.get("childTabs", []))


def plan_table_widths(document, refit=False):
    """Width changes for the top-level tables of DOCUMENT (a Docs API document
    fetched with its tabs), as (report, requests) pairs.

    A table that already has a fixed column width is left alone unless REFIT,
    so widths someone set by hand survive.  The text width is the page width
    less the margins; in a pageless doc that is the width of the page setup.
    """
    plans = []
    for tab in _tabs(document.get("tabs", [])):
        tab_id = tab["tabProperties"]["tabId"]
        body = tab.get("documentTab", {})
        style = body.get("documentStyle", {})
        total = math.floor(_points(style.get("pageSize", {}).get("width"))
                           - _points(style.get("marginLeft")) - _points(style.get("marginRight")))
        base = next((s.get("textStyle", {}) for s in body.get("namedStyles", {}).get("styles", [])
                     if s.get("namedStyleType") == "NORMAL_TEXT"), {})
        for element in body.get("body", {}).get("content", []):
            table = element.get("table")
            if not table:
                continue
            columns = table["columns"]
            rows = table.get("tableRows", [])
            report = dict(tab=tab_id, index=element["startIndex"], rows=len(rows),
                          columns=columns, total=total)
            properties = table.get("tableStyle", {}).get("tableColumnProperties", [])
            if not refit and any(p.get("widthType") == "FIXED_WIDTH" for p in properties):
                plans.append((dict(report, skipped="has fixed widths"), []))
                continue
            if any(len(row["tableCells"]) != columns
                   or any(cell.get("tableCellStyle", {}).get("columnSpan", 1) != 1
                          for cell in row["tableCells"]) for row in rows):
                plans.append((dict(report, skipped="has merged cells"), []))
                continue
            cells = [[cell_paragraphs(cell, base) for cell in row["tableCells"]] for row in rows]
            padding = [max(_horizontal_padding(row["tableCells"][c]) for row in rows)
                       for c in range(columns)]
            fixed = {c: _points(p.get("width")) for c, p in enumerate(properties)
                     if p.get("widthType") == "FIXED_WIDTH"}
            before = [fixed.get(c) or total / columns for c in range(columns)]

            def height(widths):
                return sum(max(line_count(cells[r][c], widths[c] - padding[c] - TEXT_SAFETY_PT)
                               for c in range(columns)) for r in range(len(rows)))

            def broken(widths):
                return sum(split_count(cells[r][c], widths[c] - padding[c] - TEXT_SAFETY_PT)
                           for r in range(len(rows)) for c in range(columns))

            widths = fit_widths(cells, total, padding)
            report.update(widths=widths, lines_before=height(before), lines_after=height(widths),
                          broken_before=broken(before), broken_after=broken(widths))
            requests = [{"updateTableColumnProperties": {
                "tableStartLocation": {"index": element["startIndex"], "tabId": tab_id},
                "columnIndices": [c],
                "tableColumnProperties": {"widthType": "FIXED_WIDTH",
                                          "width": {"magnitude": widths[c], "unit": "PT"}},
                "fields": "widthType,width"}} for c in range(columns)]
            plans.append((report, requests))
    return plans


def fit_tables(doc_id, refit=False, dry_run=False):
    """Fit the column widths of DOC_ID's tables; return one report per table."""
    from gdoc.api.docs import get_docs_service
    documents = get_docs_service().documents()
    document = documents.get(documentId=doc_id, includeTabsContent=True).execute()
    plans = plan_table_widths(document, refit=refit)
    requests = [request for _, table_requests in plans for request in table_requests]
    if requests and not dry_run:
        documents.batchUpdate(documentId=doc_id, body={"requests": requests}).execute()
    return [report for report, _ in plans]


def _current_version(doc_id):
    return int(get_drive_service().files().get(
        fileId=doc_id, fields="version", supportsAllDrives=True).execute()["version"])


def with_fitted_tables(upload, doc_id_of):
    """Wrap a Drive Markdown upload so the doc's tables get fitted column widths.

    The upload's reported version becomes the version after fitting, so gdoc
    records our own width change as part of the write, not as an outside edit.
    A failed fit leaves the upload in place and says so on stderr.
    """
    def wrapped(*args, **kwargs):
        result = upload(*args, **kwargs)
        doc_id = doc_id_of(args, kwargs, result)
        try:
            if any("widths" in report for report in fit_tables(doc_id)):
                version = _current_version(doc_id)
                if isinstance(result, dict):
                    result = dict(result, version=version)
                else:
                    result = version
        except (GdocError, HttpError, OSError, KeyError) as error:
            print(f"WARN: table column widths not fitted: {error}", file=sys.stderr)
        return result
    wrapped.__name__, wrapped.__doc__ = upload.__name__, upload.__doc__
    return wrapped


def cmd_fit_tables(args):
    reports = fit_tables(cli._resolve_doc_id(args.doc), refit=args.refit, dry_run=args.dry_run)
    if getattr(args, "json", False):
        print(json.dumps(reports))
        return 0
    if not reports:
        print("No tables.")
    for report in reports:
        where = f"table at {report['index']} ({report['rows']}×{report['columns']})"
        if "skipped" in report:
            print(f"{where}: skipped, {report['skipped']}")
        else:
            verb = "would set" if args.dry_run else "set"
            print(f"{where}: {report['lines_before']} → {report['lines_after']} lines, "
                  f"{report['broken_before']} → {report['broken_after']} words broken; "
                  f"{verb} widths {' '.join(map(str, report['widths']))} pt "
                  f"(of {report['total']})")
    return 0


SUGGESTIONS_NOTE = ("Suggestion threads are not included: the Drive API omits them "
                    "(google-services.md, \"Reading comments and suggestions\").")


def _folded(text):
    """Lower-cased alphanumerics only, so a quote matches its Markdown rendering."""
    return re.sub(r"[^0-9a-z]+", "", text.lower())


MIN_FOLDED_MATCH = 8  # shorter quotes ("it", "here") must match whole words


def _contains(text, folded_text, quote):
    """Whether QUOTE occurs in TEXT, ignoring case, spacing and punctuation.

    Drive returns quotes HTML-escaped ("&amp;"), the Docs API plain text."""
    quote = html.unescape(quote)
    folded = _folded(quote)
    if not folded:
        return False
    if len(folded) >= MIN_FOLDED_MATCH:
        return folded in folded_text
    words = re.findall(r"[0-9a-z]+", quote.lower())
    return re.search(r"\b" + r"\W+".join(map(re.escape, words)) + r"\b", text.lower()) is not None


def _thread_lines(comment, also_elsewhere=False):
    def who(item):
        author = item.get("author") or {}
        return author.get("displayName") or author.get("emailAddress") or "unknown"
    status = "resolved" if comment.get("resolved") else "open"
    lines = [f"#{comment.get('id', '')} [{status}] {who(comment)} {comment.get('createdTime', '')[:10]}",
             f'  "{comment.get("content", "")}"']
    quoted = html.unescape((comment.get("quotedFileContent") or {}).get("value", ""))
    if quoted:
        lines.append(f'  on "{quoted}"' + (" (this text is also on other tabs)" if also_elsewhere else ""))
    for reply in comment.get("replies") or []:
        if reply.get("content"):
            lines.append(f'  -> {who(reply)}: "{reply["content"]}"')
    return lines


def comments_section(comments, tab_texts=None, tab_index=None):
    """Render comment threads; with TAB_TEXTS, only those on tab TAB_INDEX.

    The Drive API doesn't say which tab a comment is on, so a thread is placed
    by where its quoted text occurs. Threads quoting text found on this tab are
    shown, flagged when the text is on other tabs too; the rest are counted,
    never dropped silently.
    """
    placed = [(c, False) for c in comments]
    elsewhere = unplaced = 0
    if tab_texts is not None:
        folded_texts = [_folded(t) for t in tab_texts]
        placed = []
        for c in comments:
            quote = (c.get("quotedFileContent") or {}).get("value", "")
            hits = [i for i, (t, f) in enumerate(zip(tab_texts, folded_texts)) if _contains(t, f, quote)]
            if tab_index in hits:
                placed.append((c, len(hits) > 1))
            elif hits:
                elsewhere += 1
            else:
                unplaced += 1
    header = f"## Comments ({len(placed)} threads, open and resolved"
    if tab_texts is not None:
        header += (f"; not shown: {elsewhere} on other tabs and {unplaced} whose quoted text is "
                   "no longer in the doc; gdoc comments --all DOC lists every thread")
    lines = ["", "---", header + ")", SUGGESTIONS_NOTE, ""]
    for comment, also_elsewhere in placed:
        lines.extend(_thread_lines(comment, also_elsewhere))
    if not placed:
        lines.append("No comments.")
    return "\n".join(lines) + "\n", [c for c, _ in placed]


def _tab_texts(doc_id, wanted):
    """Plain text of every tab, and the index of WANTED (title, then ID, as upstream)."""
    from gdoc.api.docs import get_document_tabs, get_tab_text
    tabs = get_document_tabs(doc_id)
    index = next((i for i, t in enumerate(tabs) if t["title"].lower() == wanted.lower()),
                 next((i for i, t in enumerate(tabs) if t["id"] == wanted), None))
    return [get_tab_text(t, markdown=False) for t in tabs], index


def with_comments(command):
    """Append the doc's comment threads to `cat`, unless the caller opts out.

    Plain output feeds `gdoc edit` matching and revisions are past content, so
    those get a stderr note instead. A failed comment read is reported, never
    skipped silently.
    """
    def cat(args):
        explicit = getattr(args, "comments", False)
        if getattr(args, "no_comments", False) or explicit or getattr(args, "range", None):
            return command(args)
        if getattr(args, "plain", False) or getattr(args, "revision", None):
            status = command(args)
            sys.stderr.write("Note: comments not shown with --plain or --revision; "
                             "read them with gdoc comments --all DOC\n")
            return status
        captured = io.StringIO()
        with contextlib.redirect_stdout(captured):
            status = command(args)
        output = captured.getvalue()
        doc_id = cli._resolve_doc_id(args.doc)
        try:
            from gdoc.api.comments import list_comments
            comments = list_comments(doc_id, include_resolved=True, include_anchor=True)
            texts, index = _tab_texts(doc_id, args.tab) if getattr(args, "tab", None) else (None, None)
        except Exception as error:  # report, don't drop: the body still prints
            sys.stdout.write(output)
            sys.stderr.write(f"Warning: comments could not be read ({error}); "
                             "this output has none\n")
            return status
        is_json = getattr(args, "json", False)
        section, shown = comments_section(comments, texts, index)
        if is_json:
            data = json.loads(output)
            data["comments"] = shown
            data["comments_note"] = SUGGESTIONS_NOTE
            print(json.dumps(data))
        else:
            sys.stdout.write(output if output.endswith("\n") else output + "\n")
            sys.stdout.write(section)
        return status
    return cat


def write_private(path, content):
    """Replace the secret file PATH with CONTENT through a temp file of its own.

    Upstream's `_write_private` opens one fixed `<name>.tmp` with O_EXCL, so of
    several processes refreshing the same expired token at once all but one
    fail; upstream swallows that error and reports "Not authenticated" for a
    token that is fine.  On 6 and 7 Nov 2026 this failed two of the Forethought
    closeout's Drive-comment targets, which it runs six at a time.  `mkstemp`
    creates the file with mode 0600 and `os.replace` swaps it in atomically,
    so the last writer wins with a valid token.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temporary = tempfile.mkstemp(prefix=f".{path.name}.", suffix=".tmp",
                                     dir=path.parent)
    try:
        with os.fdopen(fd, "w") as stream:
            stream.write(content)
        os.replace(temporary, path)
    except BaseException:
        Path(temporary).unlink(missing_ok=True)
        raise


_original_build_parser = cli.build_parser


def build_parser():
    parser = _original_build_parser()
    subparsers = next(action for action in parser._actions
                      if isinstance(action, argparse._SubParsersAction))
    if "download" in subparsers.choices:
        raise GdocError("installed gdoc now has download; retire the local extension")
    command = subparsers.add_parser("download", help="Download an original Drive file")
    command.add_argument("doc", help="Drive file ID or URL")
    command.add_argument("--output", "--out", required=True, help="New destination path")
    command.add_argument("--account", default=os.environ.get("GDOC_ACCOUNT"))
    modes = command.add_mutually_exclusive_group()
    for flag in ("json", "plain", "verbose"):
        modes.add_argument(f"--{flag}", action="store_true", default=argparse.SUPPRESS)
    command.set_defaults(func=cmd_download)
    if "fit-tables" in subparsers.choices:
        raise GdocError("installed gdoc now has fit-tables; retire the local extension")
    command = subparsers.add_parser(
        "fit-tables", help="Set table column widths that minimise each table's height")
    command.add_argument("doc", help="Document ID or URL")
    command.add_argument("--refit", action="store_true",
                         help="Also refit tables whose column widths are already fixed")
    command.add_argument("--dry-run", action="store_true",
                         help="Report the widths without changing the doc")
    command.add_argument("--account", default=os.environ.get("GDOC_ACCOUNT"))
    modes = command.add_mutually_exclusive_group()
    for flag in ("json", "plain", "verbose"):
        modes.add_argument(f"--{flag}", action="store_true", default=argparse.SUPPRESS)
    command.set_defaults(func=cmd_fit_tables)
    for name in ("write", "push"):
        upload = subparsers.choices[name]
        upload.add_argument("--allow-empty", action="store_true",
                            help="Allow an empty file to clear the document")
        upload.set_defaults(func=refuse_empty_body(upload.get_default("func")))
    cat = subparsers.choices["cat"]
    cat.add_argument("--no-comments", action="store_true",
                     help="Omit the comment threads appended by default")
    cat.set_defaults(func=with_comments(cat.get_default("func")))
    return parser


if __name__ == "__main__":
    auth._write_private = write_private
    drive.create_doc_from_markdown = with_fitted_tables(
        with_star_bullets(drive.create_doc_from_markdown),
        lambda args, kwargs, result: result["id"])
    drive.update_doc_content = with_fitted_tables(
        with_star_bullets(drive.update_doc_content),
        lambda args, kwargs, result: args[0] if args else kwargs["doc_id"])
    cli.build_parser = build_parser
    raise SystemExit(cli.main())
