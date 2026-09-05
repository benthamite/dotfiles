#!/usr/bin/env python3
"""Opt-in DOI PDF attachment helper with a deliberately narrow BibTeX editor.

Only brace-delimited regular entries, braced/quoted values and numeric literals
are supported. Macros, concatenations and special entries require the normal
Emacs workflow. PDF signature/checksum checks establish format and transfer
consistency, not the scholarly identity of a work.
"""

from __future__ import annotations

import argparse
import contextlib
import fcntl
import hashlib
import http.client
import json
import os
import re
import stat
import subprocess
import sys
import tempfile
import urllib.error
import urllib.parse
import urllib.request
from html.parser import HTMLParser
from pathlib import Path


BASE_URL = "https://annas-archive.pk/"
LOCK_ROOT = Path.home() / ".cache" / "annas-bib-locks"
HEADERS = {"User-Agent": "Mozilla/5.0", "Accept": "*/*"}


class AttachmentError(RuntimeError):
    """An actionable error containing no provider response or secret."""


def validate_base_url(base_url: str) -> str:
    if base_url not in (BASE_URL, BASE_URL.rstrip("/")):
        raise AttachmentError("unsupported provider origin; use the configured HTTPS origin")
    return BASE_URL


def normalize_doi(doi: str) -> str:
    doi = doi.strip()
    if doi.lower().startswith("doi:"):
        doi = doi[4:].strip()
    if doi.lower().startswith(("https://", "http://")):
        parsed = urllib.parse.urlsplit(doi)
        if (parsed.netloc.lower() not in ("doi.org", "dx.doi.org")
                or parsed.query or parsed.fragment):
            raise AttachmentError("unsupported DOI URL; supply the DOI identifier itself")
        doi = urllib.parse.unquote(parsed.path.lstrip("/"))
    if not re.fullmatch(r"10\.\d{4,9}/[^\s{}\\]+", doi):
        raise AttachmentError("invalid DOI identifier")
    return doi.lower()


def validate_key(key: str) -> None:
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.+-]*", key):
        raise AttachmentError("unsupported citekey; use a simple filename-safe key")


def validate_download_url(url: str, secret_key: str | None = None) -> str:
    try:
        parsed = urllib.parse.urlsplit(url)
        valid = (parsed.scheme == "https" and parsed.hostname and not parsed.username
                 and not parsed.password and not parsed.fragment and parsed.port in (None, 443))
    except (ValueError, TypeError):
        valid = False
    if not valid or any(ord(char) < 33 for char in url):
        raise AttachmentError("provider returned an unsupported download URL")
    if secret_key and secret_key in urllib.parse.unquote(url):
        raise AttachmentError("provider download URL would disclose the account key")
    return url


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        try:
            if fp is not None:
                fp.close()
        finally:
            raise AttachmentError("provider redirect refused; use the approved source workflow") from None


def fetch_bytes(url: str, *, headers: dict[str, str] | None = None, timeout: int = 30) -> bytes:
    """Never follow redirects or expose a credential-bearing URL through errors."""
    validate_download_url(url)
    try:
        request = urllib.request.Request(url, headers=headers or HEADERS)
        opener = urllib.request.build_opener(NoRedirect())
        with opener.open(request, timeout=timeout) as response:
            return response.read()
    except urllib.error.HTTPError as error:
        # HTTPError owns a response. Leaving it open can emit a ResourceWarning
        # containing its untrusted message even when the exception is redacted.
        try:
            error.close()
        finally:
            raise AttachmentError("provider request failed; no response details were logged") from None
    except (OSError, ValueError, urllib.error.URLError, http.client.HTTPException):
        raise AttachmentError("provider request failed; no response details were logged") from None


def get_secret_key() -> str:
    try:
        result = subprocess.run(
            ["pass", "show", "tlon/core/annas-archive"],
            capture_output=True, check=True, text=True, timeout=30,
        )
        key = result.stdout.splitlines()[0].strip()
    except (OSError, subprocess.SubprocessError, IndexError, UnicodeError):
        raise AttachmentError("could not read the configured account key") from None
    if not key:
        raise AttachmentError("the configured account key is empty")
    return key


class MD5Links(HTMLParser):
    def __init__(self, base_url: str):
        super().__init__()
        self.base_url = base_url
        self.candidates: set[str] = set()

    def handle_starttag(self, tag, attrs):
        if tag.lower() != "a":
            return
        for name, value in attrs:
            if name.lower() != "href" or not value:
                continue
            try:
                parsed = urllib.parse.urlsplit(urllib.parse.urljoin(self.base_url, value))
            except ValueError:
                continue
            if parsed.scheme != "https" or parsed.netloc != urllib.parse.urlsplit(self.base_url).netloc:
                continue
            match = re.fullmatch(r"/md5/([0-9a-fA-F]{32})/?", parsed.path)
            if match:
                self.candidates.add(match[1].lower())


def find_md5_for_doi(doi: str, base_url: str) -> str:
    base_url = validate_base_url(base_url)
    doi = normalize_doi(doi)
    html = fetch_bytes(base_url + "scidb/" + urllib.parse.quote(doi, safe=""))
    parser = MD5Links(base_url)
    parser.feed(html.decode("utf-8", errors="replace"))
    if len(parser.candidates) != 1:
        raise AttachmentError("DOI page has no unique supported MD5 link; inspect candidates in the approved workflow")
    return next(iter(parser.candidates))


def fast_download_url(md5: str, secret_key: str, base_url: str) -> str:
    base_url = validate_base_url(base_url)
    if not re.fullmatch(r"[0-9a-f]{32}", md5):
        raise AttachmentError("invalid download checksum")
    query = urllib.parse.urlencode({"md5": md5, "key": secret_key, "path_index": 0, "domain_index": 0})
    try:
        data = json.loads(fetch_bytes(base_url + "dyn/api/fast_download.json?" + query,
                                      headers={"Accept": "application/json"}))
    except (ValueError, TypeError):
        raise AttachmentError("provider returned an invalid API response") from None
    if not isinstance(data, dict) or data.get("error"):
        raise AttachmentError("provider API refused the download; response details were not logged")
    download_url = data.get("download_url")
    if not isinstance(download_url, str) or not download_url:
        raise AttachmentError("provider API returned no usable download URL")
    return validate_download_url(download_url, secret_key)


def download_content(doi: str, base_url: str) -> tuple[bytes, str]:
    md5 = find_md5_for_doi(doi, base_url)
    download_url = fast_download_url(md5, get_secret_key(), base_url)
    content = fetch_bytes(download_url, timeout=300)
    if not content.startswith(b"%PDF-") or b"%%EOF" not in content[-1024:]:
        raise AttachmentError("download lacks the supported PDF signature or end marker")
    if hashlib.md5(content).hexdigest() != md5:
        raise AttachmentError("download checksum does not match the selected file")
    return content, md5


def skip_space(text: str, position: int) -> int:
    while position < len(text):
        if text[position].isspace():
            position += 1
        elif text[position] == "%":
            newline = text.find("\n", position)
            position = len(text) if newline < 0 else newline + 1
        else:
            break
    return position


def read_value(text: str, position: int) -> tuple[str, int]:
    """Recognize nested braced/quoted values and decimal literals only."""
    start = position
    if position >= len(text):
        raise AttachmentError("unterminated bibliography field; use Emacs to repair it")
    opener = text[position]
    if opener not in ('{', '"'):
        match = re.match(r"\d+", text[position:])
        if match:
            return match[0], position + len(match[0])
        raise AttachmentError("unsupported bibliography macro/value; use the Emacs workflow")
    depth = 1 if opener == "{" else 0
    position += 1
    while position < len(text):
        char = text[position]
        if char == "\\":
            position += 2
            continue
        if char == "{":
            depth += 1
        elif char == "}":
            depth -= 1
            if opener == "{" and depth == 0:
                return text[start + 1:position], position + 1
            if depth < 0:
                break
        elif char == '"' and opener == '"' and depth == 0:
            return text[start + 1:position], position + 1
        position += 1
    raise AttachmentError("unbalanced bibliography value; use Emacs to repair it")


def parse_entries(text: str) -> list[dict]:
    """Fail closed outside the supported subset instead of guessing boundaries."""
    entries = []
    position = skip_space(text, 0)
    while position < len(text):
        header = re.match(r"@([A-Za-z]+)\s*\{\s*([^\s,{}()]+)\s*,", text[position:])
        if not header or header[1].lower() in ("comment", "string", "preamble"):
            raise AttachmentError("unsupported bibliography entry syntax; use the Emacs workflow")
        position += len(header[0])
        fields = {}
        last_value_end = None
        needs_comma = False
        while True:
            position = skip_space(text, position)
            if position >= len(text):
                raise AttachmentError("unterminated bibliography entry")
            if text[position] == "}":
                entries.append({"key": header[2], "fields": fields, "close": position,
                                "last_value_end": last_value_end, "needs_comma": needs_comma})
                position = skip_space(text, position + 1)
                break
            field = re.match(r"([A-Za-z][A-Za-z0-9_-]*)\s*=\s*", text[position:])
            if not field:
                raise AttachmentError("unsupported bibliography field syntax; use the Emacs workflow")
            name = field[1].lower()
            if name in fields:
                raise AttachmentError("duplicate bibliography field; use Emacs to resolve it")
            value, position = read_value(text, position + len(field[0]))
            fields[name] = value
            last_value_end = position
            position = skip_space(text, position)
            needs_comma = True
            if position < len(text) and text[position] == ",":
                position += 1
                needs_comma = False
            elif position >= len(text) or text[position] != "}":
                raise AttachmentError("unsupported concatenated or malformed bibliography value")
    return entries


def attachment_edit(text: str, key: str, doi: str, file_path: Path) -> str:
    entries = [entry for entry in parse_entries(text) if entry["key"] == key]
    if len(entries) != 1:
        raise AttachmentError("citekey must identify exactly one bibliography entry")
    entry = entries[0]
    if "file" in entry["fields"]:
        raise AttachmentError("entry already has a file field; inspect it in the Emacs workflow")
    if "doi" not in entry["fields"] or normalize_doi(entry["fields"]["doi"]) != doi:
        raise AttachmentError("entry DOI is missing or does not match the requested DOI")
    home = Path.home()
    display_path = ("~/" + str(file_path.relative_to(home))
                    if file_path.is_relative_to(home) else str(file_path))
    if any(char in display_path for char in "{}\\;\r\n"):
        raise AttachmentError("attachment path is unsupported by the narrow bibliography editor")
    prefix = text[:entry["close"]]
    if entry["needs_comma"]:
        end = entry["last_value_end"]
        prefix = text[:end] + "," + text[end:entry["close"]]
    return prefix + "\n  file = {" + display_path + "},\n" + text[entry["close"]:]


def file_identity(path: Path) -> tuple:
    info = path.stat()
    return info.st_dev, info.st_ino, info.st_size, info.st_mtime_ns, info.st_ctime_ns


@contextlib.contextmanager
def bibliography_lock(path: Path):
    LOCK_ROOT.mkdir(parents=True, exist_ok=True, mode=0o700)
    name = hashlib.sha256(str(path).encode()).hexdigest() + ".lock"
    fd = os.open(LOCK_ROOT / name, os.O_CREAT | os.O_RDWR, 0o600)
    with os.fdopen(fd, "a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        try:
            yield
        finally:
            fcntl.flock(lock, fcntl.LOCK_UN)


def stage_bytes(path: Path, content: bytes, mode: int = 0o600) -> Path:
    fd, name = tempfile.mkstemp(prefix=".annas-attachment-", dir=path.parent)
    staged = Path(name)
    try:
        with os.fdopen(fd, "wb") as stream:
            stream.write(content)
            stream.flush()
            os.fsync(stream.fileno())
            os.fchmod(stream.fileno(), mode)
    except BaseException:
        staged.unlink(missing_ok=True)
        raise
    return staged


def attach_article(doi: str, key: str, bibfile: Path, base_url: str, library_dir: Path) -> dict:
    """Serialize helper instances and optimistically recheck external edits.

    Noncooperating writers must be quiescent: checks cannot lock their final
    read/replace gap, and this helper cannot see unsaved Emacs buffers.
    """
    base_url = validate_base_url(base_url)
    doi = normalize_doi(doi)
    validate_key(key)
    requested_bibfile = bibfile.expanduser().absolute()
    bibfile = requested_bibfile.resolve(strict=True)
    if not bibfile.is_file():
        raise AttachmentError("bibliography must be an ordinary regular file")
    requested_library = library_dir.expanduser().absolute()
    library_dir = requested_library.resolve()
    path = library_dir / f"{key}.pdf"
    if path.exists() or path.is_symlink():
        raise AttachmentError("destination PDF already exists; inspect it before downloading")
    identity = file_identity(bibfile)
    original = bibfile.read_bytes()
    mode = stat.S_IMODE(bibfile.stat().st_mode)
    try:
        updated = attachment_edit(original.decode("utf-8"), key, doi, path).encode("utf-8")
    except UnicodeError:
        raise AttachmentError("bibliography must be valid UTF-8") from None

    def unchanged():
        if requested_library.resolve() != library_dir:
            raise AttachmentError("library directory changed during attachment; inspect the destination")
        if (requested_bibfile.resolve() != bibfile or not bibfile.is_file()
                or file_identity(bibfile) != identity
                or bibfile.read_bytes() != original):
            raise AttachmentError("bibliography changed during attachment; rerun after reviewing the edit")

    unchanged()
    content, md5 = download_content(doi, base_url)
    with bibliography_lock(bibfile):
        unchanged()
        path.parent.mkdir(parents=True, exist_ok=True)
        staged_pdf = stage_bytes(path, content)
        staged_bib = None
        publication_attempted = False
        # Adding a hard link changes ctime, but not inode, size or mtime.
        pdf_identity = file_identity(staged_pdf)[:4]
        try:
            staged_bib = stage_bytes(bibfile, updated, mode)
            unchanged()
            # An atomic no-clobber link publishes the complete staged PDF.
            os.link(staged_pdf, path)
            unchanged()
            publication_attempted = True
            os.replace(staged_bib, bibfile)
        except BaseException:
            if publication_attempted:
                try:
                    original_still_present = bibfile.read_bytes() == original
                except OSError:
                    original_still_present = False
                if not original_still_present:
                    raise AttachmentError("bibliography publication outcome uncertain; retained PDF, inspect both before retrying") from None
            # Resolve ownership from artifacts even if link() performed its
            # effect and then raised, before control returned to this process.
            if (not path.is_symlink() and path.is_file()
                    and file_identity(path)[:4] == pdf_identity and path.read_bytes() == content):
                path.unlink()
            raise
        finally:
            staged_pdf.unlink(missing_ok=True)
            if staged_bib is not None:
                staged_bib.unlink(missing_ok=True)
    return {"key": key, "file": str(path), "md5": md5, "bib_updated": True}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("doi")
    parser.add_argument("key")
    parser.add_argument("bibfile", type=Path)
    parser.add_argument("--base-url", default=BASE_URL)
    parser.add_argument("--library-dir", type=Path, default=Path.home() / "My Drive/library-pdf")
    args = parser.parse_args(argv)
    try:
        result = attach_article(args.doi, args.key, args.bibfile, args.base_url, args.library_dir)
    except AttachmentError as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1
    except Exception:
        # Third-party exceptions can embed URLs, credentials or response bodies.
        print("ERROR: download or attachment failed; no external error details were logged", file=sys.stderr)
        return 1
    print(json.dumps(result))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
