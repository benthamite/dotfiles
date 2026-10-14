"""Extend the installed gdoc CLI: original-file downloads, an empty-write guard,
and comment threads on every `cat`.

Drive blobs use files.get_media; native Workspace documents still use export.
https://developers.google.com/workspace/drive/api/guides/manage-downloads

Upstream `write` and `push` replace a document with whatever the file holds,
including nothing; on 11 Oct 2026 a parsing slip blanked a live doc that way.

Upstream `cat` shows comments only on request, and its `--comments` view
refuses `--tab`. A doc's comments often hold what its body leaves out (on
14 Oct 2026 a read of the retreat doc missed 232 threads), so `cat` appends
them unless `--no-comments` is given.
"""
import argparse
import contextlib
import hashlib
import html
import io
import json
import os
from pathlib import Path
import re
import sys
import tempfile

from gdoc import cli
from gdoc.api import get_drive_service
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
    cli.build_parser = build_parser
    raise SystemExit(cli.main())
