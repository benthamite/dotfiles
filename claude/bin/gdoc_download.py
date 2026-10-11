"""Extend the installed gdoc CLI: original-file downloads, and an empty-write guard.

Drive blobs use files.get_media; native Workspace documents still use export.
https://developers.google.com/workspace/drive/api/guides/manage-downloads

Upstream `write` and `push` replace a document with whatever the file holds,
including nothing; on 11 Oct 2026 a parsing slip blanked a live doc that way.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
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
    return parser


if __name__ == "__main__":
    cli.build_parser = build_parser
    raise SystemExit(cli.main())
