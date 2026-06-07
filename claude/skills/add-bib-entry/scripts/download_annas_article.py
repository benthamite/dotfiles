#!/usr/bin/env python3
"""Download an Anna's Archive article PDF by DOI and update a BibTeX entry."""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import urllib.parse
import urllib.request
from pathlib import Path


HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0.0.0 Safari/537.36"
    ),
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.5",
}


def fetch_bytes(url: str, *, headers: dict[str, str] | None = None, timeout: int = 30) -> bytes:
    """Return URL response bytes using browser-like headers."""
    req = urllib.request.Request(url, headers=headers or HEADERS)
    with urllib.request.urlopen(req, timeout=timeout) as response:
        return response.read()


def get_secret_key() -> str:
    """Return the Anna's Archive secret key from pass."""
    result = subprocess.run(
        ["pass", "show", "tlon/core/annas-archive"],
        capture_output=True,
        check=True,
        text=True,
    )
    return result.stdout.splitlines()[0]


def find_md5_for_doi(doi: str, base_url: str) -> str:
    """Return the first Anna's Archive MD5 found on DOI's SciDB page."""
    html = fetch_bytes(f"{base_url}scidb/{doi}").decode("utf-8", errors="replace")
    md5s = re.findall(r"/md5/([0-9a-f]{32})", html)
    if not md5s:
        md5s = re.findall(r"[?&]md5=([0-9a-f]{32})", html)
    if not md5s:
        raise RuntimeError(f"no MD5 found on SciDB page for {doi}")
    return md5s[0]


def fast_download_url(md5: str, secret_key: str, base_url: str) -> str:
    """Return a direct download URL from the fast download API."""
    api_url = (
        f"{base_url}dyn/api/fast_download.json"
        f"?md5={urllib.parse.quote(md5)}"
        f"&key={urllib.parse.quote(secret_key)}"
        f"&path_index=0&domain_index=0"
    )
    data = json.loads(fetch_bytes(api_url, headers={"Accept": "application/json"}))
    if error := data.get("error"):
        raise RuntimeError(f"fast download API error: {error}")
    if not (download_url := data.get("download_url")):
        raise RuntimeError("fast download API returned no download_url")
    return download_url


def download_pdf(doi: str, key: str, base_url: str, library_dir: Path) -> tuple[Path, str]:
    """Download DOI's PDF to LIBRARY_DIR using KEY as the filename."""
    md5 = find_md5_for_doi(doi, base_url)
    download_url = fast_download_url(md5, get_secret_key(), base_url)
    content = fetch_bytes(download_url, timeout=300)
    if content[:100].lstrip().lower().startswith((b"<!doctype", b"<html")):
        raise RuntimeError("download returned HTML rather than a file")
    library_dir.mkdir(parents=True, exist_ok=True)
    path = library_dir / f"{key}.pdf"
    path.write_bytes(content)
    return path, md5


def update_bib_file(bibfile: Path, key: str, file_path: Path) -> bool:
    """Insert a file field for KEY in BIBFILE if one is missing."""
    content = bibfile.read_text(encoding="utf-8")
    entry_pattern = re.compile(
        rf"(@[A-Za-z]+\{{{re.escape(key)},.*?)(^\}})",
        re.DOTALL | re.MULTILINE,
    )
    match = entry_pattern.search(content)
    if not match:
        raise RuntimeError(f"entry {key} not found in {bibfile}")
    entry = match.group(1)
    if re.search(r"^\s*file\s*=", entry, re.MULTILINE):
        return False
    before_close = content[: match.start(2)]
    before_close = re.sub(r"([^\s,])([ \t]*\n)$", r"\1,\2", before_close)
    display_path = "~/" + str(file_path.relative_to(Path.home()))
    file_line = f"\tfile = {{{display_path}}}\n"
    bibfile.write_text(before_close + file_line + content[match.start(2) :], encoding="utf-8")
    return True


def main() -> None:
    """Run the CLI."""
    parser = argparse.ArgumentParser()
    parser.add_argument("doi")
    parser.add_argument("key")
    parser.add_argument("bibfile", type=Path)
    parser.add_argument("--base-url", default="https://annas-archive.pk/")
    parser.add_argument("--library-dir", type=Path, default=Path.home() / "My Drive/library-pdf")
    args = parser.parse_args()
    base_url = args.base_url if args.base_url.endswith("/") else f"{args.base_url}/"
    path, md5 = download_pdf(args.doi, args.key, base_url, args.library_dir)
    updated = update_bib_file(args.bibfile, args.key, path)
    print(json.dumps({"key": args.key, "file": str(path), "md5": md5, "bib_updated": updated}))


if __name__ == "__main__":
    main()
