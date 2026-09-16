"""Paper acquisition core shared by ``bin/paper-fetch`` and other consumers.

One implementation of everything that used to be duplicated across
``annas-archive.el``, ``download-missing-pdfs.py``, ``annas-mcp`` and
``mentions.py``: Anna's Archive domain resolution, member fast-download API,
bot-challenge detection, and identity verification of the downloaded PDF.

Route order for a paper (DOI, URL or title):

1. Metadata: Crossref (DOI or title query) with OpenAlex as fallback, so every
   later step can verify the file it obtained is the requested work.
2. Open access: Unpaywall and OpenAlex PDF locations, arXiv, and the
   publisher/landing URL itself, fetched with a Chrome TLS fingerprint.
3. LibGen scimag JSON API (DOI -> md5) then Anna's Archive fast-download JSON
   API. Neither endpoint sits behind a JavaScript bot challenge, so this is the
   CLI route for paywalled papers. It needs an active Anna's Archive membership.
4. Anna's Archive SciDB HTML (DOI -> md5). Normally blocked by DDoS-Guard for
   non-browser clients; attempted anyway because the guard is not permanent.
5. Browser job: every URL that answered with a challenge page (PhilPapers,
   PhilArchive, Anna's Archive HTML) is written to a job file with a JavaScript
   snippet that a real, user-driven Chrome session executes. The snippet fetches
   each URL from inside the cleared origin and saves it through an ordinary
   browser download; ``collect`` then verifies and installs the files.

The Anna's Archive secret key is read from ``ANNAS_SECRET_KEY`` or the
``tlon/core/annas-archive`` pass entry, never printed, and never sent to a host
outside ``annas-archive.*``. Signed download URLs are not printed either.
"""

from __future__ import annotations

import dataclasses
import html
import json
import os
import re
import shutil
import subprocess
import time
import unicodedata
import urllib.parse
from pathlib import Path
from typing import Any, Callable, Iterable

try:  # pragma: no cover - import guard exercised by callers without curl_cffi
    from curl_cffi import requests as _requests
except ImportError:  # pragma: no cover
    _requests = None

VERSION = 1

ANNAS_MIRRORS = ("annas-archive.gl", "annas-archive.gd", "annas-archive.pk")
ANNAS_HOST_RE = re.compile(r"^annas-archive\.[A-Za-z0-9-]+$")
WIKIPEDIA_ANNAS_API_URL = (
    "https://en.wikipedia.org/w/api.php?action=parse&page=Anna%27s_Archive"
    "&prop=wikitext&format=json&formatversion=2"
)
FAST_DOWNLOAD_PATH = "dyn/api/fast_download.json"
LIBGEN_JSON_URL = "https://libgen.li/json.php"
CROSSREF_WORKS_URL = "https://api.crossref.org/works"
OPENALEX_WORKS_URL = "https://api.openalex.org/works"
UNPAYWALL_URL = "https://api.unpaywall.org/v2/"
PASS_ENTRY = "tlon/core/annas-archive"

# Hosts that serve PDFs only after a JavaScript bot challenge. Requests to them
# from a CLI are wasted; they go straight to the browser job.
CHALLENGED_HOSTS = {"philpapers.org", "philarchive.org", "www.philpapers.org"}
CHALLENGE_MARKERS = (
    "ddos-guard",
    "just a moment",
    "checking your browser",
    "attention required",
    "cf-chl",
    "js-challenge",
    "verify you are human",
    "captcha",
)
DOI_RE = re.compile(r"10\.\d{4,9}/[^\s\"'<>]+", re.IGNORECASE)
MD5_RE = re.compile(r"^[0-9a-f]{32}$")
ARXIV_RE = re.compile(r"(?:arxiv\.org/(?:abs|pdf)/|arxiv:\s*|^)([0-9]{4}\.[0-9]{4,5}(?:v\d+)?|[a-z-]+/[0-9]{7})(?:\.pdf)?$", re.I)
DOWNLOAD_PREFIX = "paperfetch"
DEFAULT_DOWNLOADS_DIR = Path.home() / "Downloads"
USER_AGENT = (
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36"
)


class PaperFetchError(Exception):
    """Configuration or environment problem that no route can work around."""


# ---------------------------------------------------------------------------
# HTTP
# ---------------------------------------------------------------------------


@dataclasses.dataclass
class Response:
    status: int
    content: bytes
    url: str
    content_type: str = ""

    @property
    def text(self) -> str:
        return self.content.decode("utf-8", "replace")

    @property
    def is_pdf(self) -> bool:
        return self.content[:5] == b"%PDF-"

    @property
    def is_challenge(self) -> bool:
        if self.is_pdf:
            return False
        head = self.content[:20000].decode("utf-8", "replace").lower()
        return any(marker in head for marker in CHALLENGE_MARKERS)


class Http:
    """Thin wrapper so tests can substitute a fake transport."""

    def __init__(self, timeout: int = 60, impersonate: str = "chrome"):
        if _requests is None:
            raise PaperFetchError("curl_cffi is not importable; install it for python3")
        self.timeout = timeout
        self.session = _requests.Session(impersonate=impersonate)

    def get(self, url: str, *, params: dict | None = None, headers: dict | None = None,
            timeout: int | None = None) -> Response:
        response = self.session.get(url, params=params, headers=headers or {},
                                    timeout=timeout or self.timeout, allow_redirects=True)
        return Response(response.status_code, response.content, str(response.url),
                        response.headers.get("content-type", ""))

    def post(self, url: str, *, data: dict | None = None, timeout: int | None = None) -> Response:
        response = self.session.post(url, data=data, timeout=timeout or self.timeout,
                                     allow_redirects=True)
        return Response(response.status_code, response.content, str(response.url),
                        response.headers.get("content-type", ""))


def contact_email() -> str:
    """Email for the Crossref/Unpaywall/OpenAlex polite pools (their APIs ask for one)."""
    value = os.environ.get("PAPER_FETCH_EMAIL")
    if value:
        return value
    try:
        out = subprocess.run(["git", "config", "--global", "user.email"],
                             capture_output=True, text=True, timeout=10)
        if out.returncode == 0 and out.stdout.strip():
            return out.stdout.strip()
    except (OSError, subprocess.SubprocessError):
        pass
    return "paper-fetch@example.invalid"


# ---------------------------------------------------------------------------
# Identifiers and metadata
# ---------------------------------------------------------------------------


@dataclasses.dataclass
class Work:
    doi: str = ""
    title: str = ""
    authors: list[str] = dataclasses.field(default_factory=list)
    year: str = ""
    url: str = ""
    arxiv: str = ""
    container: str = ""
    source: str = ""

    def slug(self) -> str:
        surname = ""
        if self.authors:
            first = self.authors[0]
            surname = first.split(",")[0] if "," in first else first.split()[-1]
        words = [w for w in re.findall(r"[A-Za-z0-9]+", _ascii(self.title)) if w.lower() not in _STOPWORDS][:4]
        parts = [_ascii(surname).replace(" ", ""), self.year] + [w.capitalize() for w in words]
        text = "".join(p for p in parts if p)
        return re.sub(r"[^A-Za-z0-9]", "", text) or "paper"

    def to_dict(self) -> dict[str, Any]:
        return dataclasses.asdict(self)


_STOPWORDS = {"the", "a", "an", "of", "and", "or", "in", "on", "for", "to", "with", "is", "are",
              "vs", "from", "by", "at", "as", "its", "it", "be", "we", "how", "what", "why"}


def _ascii(text: str) -> str:
    return unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode()


def normalize_doi(text: str) -> str:
    text = text.strip()
    text = re.sub(r"^(?:https?://)?(?:dx\.)?doi\.org/", "", text, flags=re.I)
    text = re.sub(r"^doi:\s*", "", text, flags=re.I)
    match = DOI_RE.search(text)
    if not match:
        return ""
    doi = match.group(0).rstrip(".,;)")
    return urllib.parse.unquote(doi).lower()


def classify_identifier(ident: str) -> tuple[str, str]:
    """Return (kind, value) where kind is doi, md5, arxiv, url or title."""
    ident = ident.strip()
    if MD5_RE.match(ident.lower()):
        return "md5", ident.lower()
    doi = normalize_doi(ident)
    if doi and (ident.lower().startswith(("10.", "doi", "http")) or "doi.org" in ident.lower()):
        return "doi", doi
    arxiv = ARXIV_RE.search(ident)
    if arxiv:
        return "arxiv", arxiv.group(1)
    if re.match(r"^https?://", ident, re.I):
        return "url", ident
    if doi:
        return "doi", doi
    return "title", ident


def crossref_lookup(http: Http, *, doi: str = "", title: str = "", author: str = "") -> Work | None:
    params = {"mailto": contact_email()}
    if doi:
        response = http.get(f"{CROSSREF_WORKS_URL}/{urllib.parse.quote(doi, safe='')}", params=params)
        if response.status != 200:
            return None
        message = json.loads(response.text).get("message") or {}
        return _work_from_crossref(message)
    params.update({"query.bibliographic": title, "rows": "3"})
    if author:
        params["query.author"] = author
    response = http.get(CROSSREF_WORKS_URL, params=params)
    if response.status != 200:
        return None
    for item in (json.loads(response.text).get("message") or {}).get("items") or []:
        work = _work_from_crossref(item)
        if work and title_similarity(title, work.title) >= 0.75:
            return work
    return None


def _work_from_crossref(item: dict) -> Work | None:
    if not item:
        return None
    titles = item.get("title") or []
    authors = []
    for person in item.get("author") or []:
        family = person.get("family") or person.get("name") or ""
        given = person.get("given") or ""
        authors.append(f"{family}, {given}".strip(", ") if family else given)
    year = ""
    for key in ("published-print", "published-online", "issued", "created"):
        parts = (item.get(key) or {}).get("date-parts") or []
        if parts and parts[0] and parts[0][0]:
            year = str(parts[0][0])
            break
    return Work(
        doi=normalize_doi(item.get("DOI") or ""),
        title=html.unescape(titles[0]) if titles else "",
        authors=authors,
        year=year,
        url=item.get("URL") or "",
        container=(item.get("container-title") or [""])[0],
        source="crossref",
    )


def openalex_lookup(http: Http, *, doi: str = "", title: str = "") -> tuple[Work | None, list[str]]:
    """Return (work, candidate PDF urls) from OpenAlex."""
    params = {"mailto": contact_email()}
    if doi:
        url = f"{OPENALEX_WORKS_URL}/doi:{urllib.parse.quote(doi, safe='')}"
    else:
        params.update({"filter": f"title.search:{_search_terms(title)}", "per-page": "3"})
        url = OPENALEX_WORKS_URL
    response = http.get(url, params=params)
    if response.status != 200:
        return None, []
    payload = json.loads(response.text)
    items = payload.get("results") if "results" in payload else [payload]
    for item in items or []:
        work = Work(
            doi=normalize_doi(item.get("doi") or ""),
            title=item.get("title") or item.get("display_name") or "",
            authors=[(a.get("author") or {}).get("display_name", "") for a in item.get("authorships") or []],
            year=str(item.get("publication_year") or ""),
            source="openalex",
        )
        if title and title_similarity(title, work.title) < 0.75:
            continue
        urls: list[str] = []
        for loc in [item.get("best_oa_location")] + (item.get("locations") or []):
            if not loc:
                continue
            for key in ("pdf_url", "landing_page_url"):
                value = loc.get(key)
                if value and value not in urls:
                    urls.append(value)
        return work, urls
    return None, []


def _search_terms(title: str) -> str:
    return re.sub(r"[^A-Za-z0-9 ]+", " ", title)[:150].strip()


def unpaywall_urls(http: Http, doi: str) -> list[str]:
    response = http.get(f"{UNPAYWALL_URL}{urllib.parse.quote(doi, safe='')}",
                        params={"email": contact_email()})
    if response.status != 200:
        return []
    payload = json.loads(response.text)
    urls: list[str] = []
    for loc in [payload.get("best_oa_location")] + (payload.get("oa_locations") or []):
        if not loc:
            continue
        for key in ("url_for_pdf", "url"):
            value = loc.get(key)
            if value and value not in urls:
                urls.append(value)
    return urls


def title_similarity(a: str, b: str) -> float:
    ta, tb = _title_tokens(a), _title_tokens(b)
    if not ta or not tb:
        return 0.0
    return len(ta & tb) / len(ta | tb)


def _title_tokens(text: str) -> set[str]:
    words = re.findall(r"[a-z0-9]+", _ascii(text).lower())
    return {w for w in words if len(w) > 2 and w not in _STOPWORDS}


# ---------------------------------------------------------------------------
# Anna's Archive
# ---------------------------------------------------------------------------


def annas_home_url_from_wikitext(wikitext: str) -> str:
    match = re.search(r"\{\{URL\s*\|\s*(https://annas-archive\.[^\]\[|{}\s]+/?)", wikitext)
    if not match:
        return ""
    url = match.group(1).rstrip("/") + "/"
    return url if re.fullmatch(r"https://annas-archive\.[A-Za-z0-9-]+/", url) else ""


def resolve_annas_hosts(http: Http | None, override: str = "") -> list[str]:
    """Ordered candidate hosts: override, Wikipedia's current domain, known mirrors."""
    hosts: list[str] = []
    override = override or os.environ.get("PAPER_FETCH_ANNAS_HOST", "")
    if override:
        host = re.sub(r"^https?://", "", override).strip("/")
        if not ANNAS_HOST_RE.match(host):
            raise PaperFetchError(f"refusing non-Anna's-Archive host override: {host}")
        hosts.append(host)
    if http is not None:
        try:
            response = http.get(WIKIPEDIA_ANNAS_API_URL, timeout=20)
            if response.status == 200:
                wikitext = (json.loads(response.text).get("parse") or {}).get("wikitext") or ""
                url = annas_home_url_from_wikitext(wikitext)
                if url:
                    host = urllib.parse.urlparse(url).netloc
                    if host not in hosts:
                        hosts.append(host)
        except Exception:  # noqa: BLE001 - Wikipedia is advisory only
            pass
    for host in ANNAS_MIRRORS:
        if host not in hosts:
            hosts.append(host)
    return hosts


def annas_secret_key() -> str:
    key = os.environ.get("ANNAS_SECRET_KEY", "").strip()
    if key:
        return key
    try:
        out = subprocess.run(["pass", "show", PASS_ENTRY], capture_output=True, text=True, timeout=30)
    except (OSError, subprocess.SubprocessError):
        return ""
    if out.returncode != 0 or not out.stdout:
        return ""
    return out.stdout.splitlines()[0].strip()


@dataclasses.dataclass
class FastDownload:
    status: str  # ok | not-member | invalid-key | quota | no-file | challenge | error
    url: str = ""
    detail: str = ""
    quota: dict | None = None


def annas_fast_download(http: Http, host: str, md5: str, key: str, *, domain_index: int = 0,
                        path_index: int = 0) -> FastDownload:
    """Ask the member API for a signed download URL for MD5.

    DOMAIN_INDEX selects the partner server; the first one occasionally 404s for
    a file that another mirror still serves, so callers retry with the next index.
    """
    if not ANNAS_HOST_RE.match(host):
        raise PaperFetchError(f"refusing to send the Anna's Archive key to {host}")
    if not key:
        return FastDownload("invalid-key", detail="no secret key configured")
    response = http.get(f"https://{host}/{FAST_DOWNLOAD_PATH}",
                        params={"md5": md5, "key": key, "path_index": path_index,
                                "domain_index": domain_index})
    if response.is_challenge:
        return FastDownload("challenge", detail=f"HTTP {response.status} challenge page")
    try:
        payload = json.loads(response.text)
    except ValueError:
        return FastDownload("error", detail=f"HTTP {response.status} non-JSON response")
    url = payload.get("download_url")
    if url:
        return FastDownload("ok", url=url, quota=payload.get("account_fast_download_info"))
    error = str(payload.get("error") or f"HTTP {response.status}")
    lowered = error.lower()
    if "not a member" in lowered:
        status = "not-member"
    elif "invalid secret key" in lowered:
        status = "invalid-key"
    elif "quota" in lowered or "limit" in lowered or response.status == 429:
        status = "quota"
    elif "not found" in lowered or response.status == 404:
        status = "no-file"
    else:
        status = "error"
    return FastDownload(status, detail=error[:200])


def libgen_md5s(http: Http, doi: str) -> list[str]:
    """DOI -> md5 list through LibGen's scimag JSON API (no bot challenge)."""
    response = http.get(LIBGEN_JSON_URL, params={"object": "e", "doi": doi}, timeout=45)
    if response.status != 200:
        return []
    try:
        payload = json.loads(response.text)
    except ValueError:
        return []
    md5s: list[str] = []
    if isinstance(payload, dict):
        for edition in payload.values():
            if not isinstance(edition, dict):
                continue
            raw = str(edition.get("doi") or "").strip().lower()
            edition_doi = normalize_doi(raw) or raw
            if edition_doi and edition_doi != doi.lower():
                continue
            for file_ in (edition.get("files") or {}).values():
                md5 = str(file_.get("md5") or "").lower()
                if MD5_RE.match(md5) and md5 not in md5s:
                    md5s.append(md5)
    return md5s


def libgen_isbn_files(http: Http, isbn: str) -> list[dict]:
    """ISBN -> LibGen file records (md5, extension, filesize, title, year, flags).

    Same unchallenged JSON API as ``libgen_md5s``; hyphens and spaces are
    stripped because the API rejects formatted ISBNs. Each record carries the
    edition's title/year plus the file's ``extension``, ``filesize`` (int) and
    the ``scanned``/``vector``/``ocr`` flags LibGen exposes, so callers can rank.
    """
    isbn = re.sub(r"[^0-9Xx]", "", isbn)
    if len(isbn) not in (10, 13):
        return []
    response = http.get(LIBGEN_JSON_URL, params={"object": "e", "isbn": isbn}, timeout=45)
    if response.status != 200:
        return []
    try:
        editions = json.loads(response.text)
    except ValueError:
        return []
    if not isinstance(editions, dict) or not editions:
        return []
    file_ids: dict[str, dict] = {}
    for edition in editions.values():
        if not isinstance(edition, dict):
            continue
        for file_ in (edition.get("files") or {}).values():
            fid = str(file_.get("f_id") or "")
            if fid and fid not in file_ids:
                file_ids[fid] = {"title": edition.get("title") or "", "year": str(edition.get("year") or ""),
                                 "author": edition.get("author") or ""}
    if not file_ids:
        return []
    records: list[dict] = []
    ids = list(file_ids)
    for start in range(0, len(ids), 50):
        chunk = ids[start:start + 50]
        response = http.get(LIBGEN_JSON_URL, params={"object": "f", "ids": ",".join(chunk)}, timeout=45)
        if response.status != 200:
            continue
        try:
            files = json.loads(response.text)
        except ValueError:
            continue
        if not isinstance(files, dict):
            continue
        for fid, info in files.items():
            if not isinstance(info, dict):
                continue
            md5 = str(info.get("md5") or "").lower()
            if not MD5_RE.match(md5):
                continue
            try:
                size = int(info.get("filesize") or 0)
            except ValueError:
                size = 0
            locator = str(info.get("locator") or "").replace("\\", "/")
            records.append({
                "md5": md5, "extension": (info.get("extension") or "").lower(), "size_bytes": size,
                "pages": info.get("pages") or "", "scanned": info.get("scanned") == "Y",
                "vector": info.get("vector") == "Y", "ocr": info.get("ocr") == "Y",
                "filename": locator.rsplit("/", 1)[-1], "source": "libgen",
                **file_ids.get(str(fid), {}),
            })
    return records


def scidb_md5s_from_html(page: str, doi: str) -> list[str]:
    """md5s of result cards whose Sci-Hub filename matches DOI exactly.

    SciDB pages list related papers too; a bare ``/md5/`` scrape returns the wrong
    paper. Only cards naming ``scihub/<doi>.pdf`` are trusted.
    """
    expected = f"scihub/{doi}.pdf"
    md5s: list[str] = []
    for block in re.split(r'(?i)<div[^>]*class="[^"]*(?:h-\[|js-aarecord|aarecord)[^"]*"', page)[1:]:
        text = html.unescape(re.sub(r"<[^>]+>", " ", block)).lower()
        if expected not in text:
            continue
        for md5 in re.findall(r"/md5/([0-9a-f]{32})", block, flags=re.I):
            if md5.lower() not in md5s:
                md5s.append(md5.lower())
    if not md5s:
        # Single-record layout: the page itself is the DOI's record.
        text = html.unescape(re.sub(r"<[^>]+>", " ", page)).lower()
        found = re.findall(r"/md5/([0-9a-f]{32})", page, flags=re.I)
        if expected in text and len(set(found)) == 1:
            md5s = [found[0].lower()]
    return md5s


# ---------------------------------------------------------------------------
# PDF verification
# ---------------------------------------------------------------------------


@dataclasses.dataclass
class Verification:
    is_pdf: bool
    size: int
    pages: int
    title_match: float
    doi_found: bool
    text_available: bool
    verdict: str  # verified | unverified-no-text | mismatch | not-pdf
    excerpt: str = ""

    def to_dict(self) -> dict[str, Any]:
        return dataclasses.asdict(self)


def pdf_text(path: Path, pages: int = 3) -> str:
    if not shutil.which("pdftotext"):
        return ""
    try:
        out = subprocess.run(["pdftotext", "-l", str(pages), str(path), "-"],
                             capture_output=True, timeout=120)
    except (OSError, subprocess.SubprocessError):
        return ""
    return out.stdout.decode("utf-8", "replace")


def pdf_pages(path: Path) -> int:
    if not shutil.which("pdfinfo"):
        return 0
    try:
        out = subprocess.run(["pdfinfo", str(path)], capture_output=True, text=True, timeout=60)
    except (OSError, subprocess.SubprocessError):
        return 0
    match = re.search(r"^Pages:\s+(\d+)", out.stdout, re.M)
    return int(match.group(1)) if match else 0


def verify_pdf(path: Path, work: Work | None) -> Verification:
    data = path.read_bytes()[:8] if path.exists() else b""
    size = path.stat().st_size if path.exists() else 0
    if data[:5] != b"%PDF-" or size < 2000:
        return Verification(False, size, 0, 0.0, False, False, "not-pdf")
    text = pdf_text(path)
    flat = re.sub(r"\s+", " ", text)
    text_available = len(re.findall(r"[A-Za-z]{3,}", flat)) > 40
    excerpt = flat[:240]
    if work is None or (not work.title and not work.doi):
        return Verification(True, size, pdf_pages(path), 0.0, False, text_available,
                            "unverified-no-metadata", excerpt)
    tokens = _title_tokens(work.title)
    lowered = _ascii(flat).lower()
    hits = sum(1 for tok in tokens if tok in lowered)
    ratio = hits / len(tokens) if tokens else 0.0
    doi_found = bool(work.doi) and work.doi.lower() in lowered
    if not text_available:
        verdict = "unverified-no-text"
    elif doi_found or ratio >= 0.6:
        verdict = "verified"
    else:
        verdict = "mismatch"
    return Verification(True, size, pdf_pages(path), round(ratio, 2), doi_found,
                        text_available, verdict, excerpt)


# ---------------------------------------------------------------------------
# Browser job (for challenged hosts)
# ---------------------------------------------------------------------------

BROWSER_SNIPPET = r"""
(async () => {
  const job = __JOB__;
  const out = [];
  for (const item of job.items) {
    if (new URL(item.url).origin !== location.origin) { out.push({i: item.i, skipped: 'other-origin'}); continue; }
    try {
      const r = await fetch(item.url, {credentials: 'include'});
      const b = await r.blob();
      const isPdf = b.type.includes('pdf') || (await b.slice(0, 5).text()) === '%PDF-';
      if (r.status === 200 && isPdf) {
        const a = document.createElement('a');
        a.href = URL.createObjectURL(b); a.download = item.name;
        document.body.appendChild(a); a.click(); a.remove();
        out.push({i: item.i, saved: item.name, bytes: b.size});
      } else if ((item.kind === 'scidb' || item.kind === 'md5') && r.status === 200) {
        // Anna's Archive: resolve the record page, then start a slow partner download.
        // Partner hosts block cross-origin reads, so a hidden iframe carries the
        // download; Chrome saves it under the partner's filename, which embeds the md5.
        let page = new DOMParser().parseFromString(await b.text(), 'text/html');
        let md5Href = null;
        if (item.kind === 'scidb') {
          if (!r.url.includes('/scidb/')) { out.push({i: item.i, result: 'no-record'}); continue; }
          const link = page.querySelector('a[href^="/md5/"]');
          md5Href = link && link.getAttribute('href').slice(0, 37);
          if (!md5Href) { out.push({i: item.i, result: 'no-md5-link'}); continue; }
          page = new DOMParser().parseFromString(await (await fetch(md5Href, {credentials: 'include'})).text(), 'text/html');
        }
        const slow = [...page.querySelectorAll('a[href*="/slow_download/"]')].map(a => a.getAttribute('href'));
        if (!slow.length) { out.push({i: item.i, result: 'no-slow-link'}); continue; }
        let started = false;
        for (const href of slow.slice(0, 3)) {
          const sp = new DOMParser().parseFromString(await (await fetch(href, {credentials: 'include'})).text(), 'text/html');
          const dl = [...sp.querySelectorAll('main a[href]')].find(a => /download now/i.test(a.textContent));
          if (!dl) continue;
          const f = document.createElement('iframe'); f.style.display = 'none'; f.src = dl.getAttribute('href');
          document.body.appendChild(f); started = true; break;
        }
        out.push({i: item.i, result: started ? 'slow-download-started' : 'no-download-link'});
      } else {
        out.push({i: item.i, status: r.status, type: b.type, bytes: b.size});
      }
    } catch (e) { out.push({i: item.i, error: String(e).slice(0, 120)}); }
    await new Promise(res => setTimeout(res, 800));
  }
  return out;
})()
""".strip()


@dataclasses.dataclass
class BrowserJob:
    token: str
    work: dict
    origins: list[dict]  # [{origin, items:[{i,url,name,kind}]}]
    downloads_dir: str
    path: str = ""

    def to_dict(self) -> dict[str, Any]:
        return dataclasses.asdict(self)


def make_browser_job(work: Work, urls: Iterable[tuple[str, str]], job_dir: Path,
                     downloads_dir: Path = DEFAULT_DOWNLOADS_DIR) -> BrowserJob:
    """Group challenged URLs by origin and write JOB.json plus one JS snippet per origin."""
    token = f"{int(time.time()):x}{os.getpid() % 1000:03d}"
    grouped: dict[str, list[dict]] = {}
    for index, (url, kind) in enumerate(urls):
        origin = "{0.scheme}://{0.netloc}".format(urllib.parse.urlparse(url))
        grouped.setdefault(origin, []).append(
            {"i": index, "url": url, "kind": kind, "name": f"{DOWNLOAD_PREFIX}-{token}-{index}.pdf"})
    job = BrowserJob(token, work.to_dict(), [{"origin": o, "items": items} for o, items in grouped.items()],
                     str(downloads_dir))
    job_dir.mkdir(parents=True, exist_ok=True)
    path = job_dir / f"{DOWNLOAD_PREFIX}-{token}.json"
    for group in job.origins:
        snippet = BROWSER_SNIPPET.replace("__JOB__", json.dumps({"items": group["items"]}))
        snippet_path = job_dir / f"{DOWNLOAD_PREFIX}-{token}-{urllib.parse.urlparse(group['origin']).netloc}.js"
        snippet_path.write_text(snippet)
        group["snippet"] = str(snippet_path)
    job.path = str(path)
    path.write_text(json.dumps(job.to_dict(), indent=1))
    return job


def load_browser_job(path: Path) -> BrowserJob:
    data = json.loads(path.read_text())
    job = BrowserJob(data["token"], data["work"], data["origins"], data["downloads_dir"], str(path))
    return job


def collect_browser_downloads(job: BrowserJob, out_dir: Path, name: str = "") -> dict[str, Any]:
    """Verify and install files the browser saved for this job token."""
    downloads = Path(job.downloads_dir)
    work = Work(**{k: v for k, v in job.work.items() if k in Work.__dataclass_fields__})
    candidates = sorted(downloads.glob(f"{DOWNLOAD_PREFIX}-{job.token}-*.pdf"))
    created = Path(job.path).stat().st_mtime if job.path and Path(job.path).exists() else 0
    for partner_file in downloads.glob("*Anna*Archive*.pdf"):
        if partner_file.stat().st_mtime >= created - 5 and partner_file not in candidates:
            candidates.append(partner_file)
    results = []
    installed = None
    for candidate in candidates:
        verification = verify_pdf(candidate, work)
        entry = {"downloaded": str(candidate), "verification": verification.to_dict()}
        if verification.verdict in ("verified", "unverified-no-text", "unverified-no-metadata") and installed is None:
            installed = install_pdf(candidate, out_dir, name or work.slug())
            entry["installed"] = str(installed)
        else:
            entry["kept"] = "not installed"
        results.append(entry)
    status = "ok" if installed else ("mismatch" if results else "no-downloads")
    message = ""
    if status == "no-downloads":
        message = (f"No {DOWNLOAD_PREFIX}-{job.token}-*.pdf in {downloads}. Either the snippet has not run on "
                   "that origin yet, or Chrome blocked the download: it allows one automatic download per site, "
                   "then needs the 'Automatic downloads' permission for that origin (one-time, per site).")
    return {"status": status, "file": str(installed) if installed else "", "job": job.path,
            "results": results, "message": message}


def install_pdf(source: Path, out_dir: Path, stem: str) -> Path:
    out_dir.mkdir(parents=True, exist_ok=True)
    stem = re.sub(r"[^A-Za-z0-9_-]", "", stem) or "paper"
    target = out_dir / f"{stem}.pdf"
    counter = 1
    while target.exists():
        counter += 1
        target = out_dir / f"{stem}-{counter}.pdf"
    shutil.move(str(source), str(target))
    return target


# ---------------------------------------------------------------------------
# Orchestration
# ---------------------------------------------------------------------------


@dataclasses.dataclass
class Attempt:
    route: str
    target: str
    result: str

    def to_dict(self) -> dict[str, str]:
        return dataclasses.asdict(self)


@dataclasses.dataclass
class Outcome:
    status: str  # ok | needs-browser | not-member | unavailable | error
    work: Work | None
    file: str = ""
    route: str = ""
    verification: Verification | None = None
    attempts: list[Attempt] = dataclasses.field(default_factory=list)
    browser_job: BrowserJob | None = None
    message: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {
            "status": self.status,
            "work": self.work.to_dict() if self.work else None,
            "file": self.file,
            "route": self.route,
            "verification": self.verification.to_dict() if self.verification else None,
            "attempts": [a.to_dict() for a in self.attempts],
            "browser_job": self.browser_job.to_dict() if self.browser_job else None,
            "message": self.message,
        }


def _redact(url: str) -> str:
    parsed = urllib.parse.urlparse(url)
    return f"{parsed.scheme}://{parsed.netloc}{parsed.path[:60]}"


class Fetcher:
    def __init__(self, http: Http, out_dir: Path, job_dir: Path, *, annas_host: str = "",
                 key_reader: Callable[[], str] = annas_secret_key, log: Callable[[str], None] | None = None,
                 downloads_dir: Path = DEFAULT_DOWNLOADS_DIR, skip_annas: bool = False):
        self.http = http
        self.out_dir = out_dir
        self.job_dir = job_dir
        self.annas_override = annas_host
        self.key_reader = key_reader
        self.log = log or (lambda _msg: None)
        self.downloads_dir = downloads_dir
        self.skip_annas = skip_annas
        self._hosts: list[str] | None = None

    # -- metadata -----------------------------------------------------------

    def resolve_work(self, ident: str, *, author: str = "") -> tuple[Work | None, list[str], str]:
        """Return (work, extra candidate urls, md5) for an identifier."""
        kind, value = classify_identifier(ident)
        urls: list[str] = []
        md5 = ""
        work: Work | None = None
        if kind == "md5":
            return Work(title="", source="md5"), [], value
        if kind == "arxiv":
            urls.append(f"https://arxiv.org/pdf/{value}")
            work = Work(arxiv=value, title=f"arXiv:{value}", source="arxiv")
            doi = f"10.48550/arxiv.{value.split('v')[0]}"
            found = self._safe(lambda: crossref_lookup(self.http, doi=doi))
            if found:
                found.arxiv = value
                work = found
            return work, urls, md5
        if kind == "url":
            doi = normalize_doi(value) if "doi.org" in value else ""
            if not doi:
                doi = self._doi_from_landing(value)
            if doi:
                work = self._safe(lambda: crossref_lookup(self.http, doi=doi))
            if work is None:
                work = Work(url=value, source="url")
            work.url = work.url or value
            urls.append(value)
            return work, urls, md5
        if kind == "doi":
            work = self._safe(lambda: crossref_lookup(self.http, doi=value))
            if work is None:
                work, oa = self._safe(lambda: openalex_lookup(self.http, doi=value)) or (None, [])
                urls.extend(oa)
            if work is None:
                work = Work(doi=value, source="doi")
            return work, urls, md5
        work = self._safe(lambda: crossref_lookup(self.http, title=value, author=author))
        if work is None:
            work, oa = self._safe(lambda: openalex_lookup(self.http, title=value)) or (None, [])
            urls.extend(oa)
        return work, urls, md5

    def _doi_from_landing(self, url: str) -> str:
        host = urllib.parse.urlparse(url).netloc.lower()
        if host in CHALLENGED_HOSTS:
            return ""
        response = self._safe(lambda: self.http.get(url, timeout=40))
        if not response or response.is_challenge or response.is_pdf:
            return ""
        head = response.text[:200000]
        for pattern in (r'name="citation_doi"\s+content="([^"]+)"', r'name="dc\.identifier"\s+content="([^"]+)"',
                        r'"doi"\s*:\s*"([^"]+)"', r'doi\.org/(10\.[^"\'<>\s]+)'):
            match = re.search(pattern, head, re.I)
            if match:
                doi = normalize_doi(match.group(1))
                if doi:
                    return doi
        return ""

    def _safe(self, fn):
        try:
            return fn()
        except Exception as exc:  # noqa: BLE001 - one failing route must not stop the others
            self.log(f"route error: {type(exc).__name__}")
            return None

    # -- routes -------------------------------------------------------------

    def fetch(self, ident: str, *, author: str = "", name: str = "", md5: str = "") -> Outcome:
        work, urls, found_md5 = self.resolve_work(ident, author=author)
        md5 = md5 or found_md5
        outcome = Outcome("unavailable", work)
        self.log(f"work: {work.title[:80] if work and work.title else ident}")
        if work and work.doi:
            for url in self._safe(lambda: unpaywall_urls(self.http, work.doi)) or []:
                if url not in urls:
                    urls.append(url)
            _oa_work, oa_urls = self._safe(lambda: openalex_lookup(self.http, doi=work.doi)) or (None, [])
            for url in oa_urls:
                if url not in urls:
                    urls.append(url)
        if work and work.arxiv and f"https://arxiv.org/pdf/{work.arxiv}" not in urls:
            urls.insert(0, f"https://arxiv.org/pdf/{work.arxiv}")
        challenged: list[tuple[str, str]] = []
        for url in urls:
            host = urllib.parse.urlparse(url).netloc.lower()
            if host in CHALLENGED_HOSTS:
                if "/archive/" in url or url.endswith(".pdf"):
                    challenged.append((url, "pdf"))
                    outcome.attempts.append(Attempt("open-access", _redact(url), "challenged host; deferred to browser"))
                continue
            result = self._try_direct(url, work, name)
            outcome.attempts.append(Attempt("open-access", _redact(url), result))
            if result.startswith("ok:"):
                return self._finish(outcome, result[3:], "open-access", work)
            if result == "challenge":
                challenged.append((url, "pdf"))
        if not self.skip_annas:
            annas = self._try_annas(work, md5, name, outcome, challenged)
            if annas:
                return annas
        if challenged:
            outcome.status = "needs-browser"
            outcome.browser_job = make_browser_job(work or Work(title=ident), challenged, self.job_dir,
                                                   self.downloads_dir)
            outcome.message = ("Challenged hosts hold the only remaining copies; run the browser job "
                               "then `paper-fetch collect`.")
            return outcome
        if outcome.status == "unavailable" and not outcome.message:
            outcome.message = "No route produced a file; see attempts."
        return outcome

    def _try_direct(self, url: str, work: Work | None, name: str) -> str:
        response = self._safe(lambda: self.http.get(url, timeout=90))
        if response is None:
            return "error"
        if response.is_challenge:
            return "challenge"
        if response.status != 200:
            return f"http-{response.status}"
        if not response.is_pdf:
            pdf_url = self._pdf_link_from_html(response.text, response.url)
            if not pdf_url:
                return "not-pdf"
            response = self._safe(lambda: self.http.get(pdf_url, timeout=90))
            if response is None or response.status != 200 or not response.is_pdf:
                return "challenge" if response is not None and response.is_challenge else "linked-not-pdf"
        staged = self._stage(response.content, work, name)
        verification = verify_pdf(staged, work)
        if verification.verdict == "mismatch":
            staged.unlink(missing_ok=True)
            return f"mismatch(title={verification.title_match})"
        return f"ok:{staged}"

    @staticmethod
    def _pdf_link_from_html(page: str, base: str) -> str:
        for pattern in (r'name="citation_pdf_url"\s+content="([^"]+)"',
                        r'<a[^>]+href="([^"]+\.pdf(?:\?[^"]*)?)"[^>]*>',
                        r'<meta[^>]+content="([^"]+\.pdf[^"]*)"'):
            match = re.search(pattern, page[:400000], re.I)
            if match:
                return urllib.parse.urljoin(base, html.unescape(match.group(1)))
        return ""

    def _stage(self, content: bytes, work: Work | None, name: str) -> Path:
        self.out_dir.mkdir(parents=True, exist_ok=True)
        stem = re.sub(r"[^A-Za-z0-9_-]", "", name) if name else (work.slug() if work else "paper")
        target = self.out_dir / f"{stem}.pdf"
        counter = 1
        while target.exists():
            counter += 1
            target = self.out_dir / f"{stem}-{counter}.pdf"
        target.write_bytes(content)
        return target

    def _finish(self, outcome: Outcome, path: str, route: str, work: Work | None) -> Outcome:
        outcome.status = "ok"
        outcome.file = path
        outcome.route = route
        outcome.verification = verify_pdf(Path(path), work)
        return outcome

    def hosts(self) -> list[str]:
        if self._hosts is None:
            self._hosts = resolve_annas_hosts(self.http, self.annas_override)
        return self._hosts

    def _try_annas(self, work: Work | None, md5: str, name: str, outcome: Outcome,
                   challenged: list[tuple[str, str]]) -> Outcome | None:
        doi = work.doi if work else ""
        md5s = [md5] if md5 else []
        if doi and not md5s:
            md5s = self._safe(lambda: libgen_md5s(self.http, doi)) or []
            outcome.attempts.append(Attempt("libgen-json", doi, f"{len(md5s)} md5" if md5s else "no record"))
        key = self.key_reader()
        hosts = self.hosts()
        if not md5s and doi:
            for host in hosts:
                url = f"https://{host}/scidb/{doi}"
                response = self._safe(lambda: self.http.get(url, timeout=45))
                if response is None:
                    outcome.attempts.append(Attempt("scidb-html", host, "error"))
                    continue
                if response.is_challenge or response.status in (403, 429, 503):
                    outcome.attempts.append(Attempt("scidb-html", host, f"challenge (HTTP {response.status})"))
                    challenged.append((url, "scidb"))
                    break  # one browser job for one mirror is enough
                md5s = scidb_md5s_from_html(response.text, doi)
                outcome.attempts.append(Attempt("scidb-html", host, f"{len(md5s)} md5" if md5s else "no matching card"))
                if md5s:
                    break
        if not md5s:
            return None
        if not key:
            outcome.attempts.append(Attempt("annas-fast-download", "-", "no secret key available"))
            return None
        for candidate in md5s[:3]:
            for host in hosts:
                result = self._safe(lambda: annas_fast_download(self.http, host, candidate, key))
                if result is None:
                    outcome.attempts.append(Attempt("annas-fast-download", host, "error"))
                    continue
                if result.status == "challenge":
                    outcome.attempts.append(Attempt("annas-fast-download", host, result.detail))
                    continue
                if result.status == "ok":
                    response = self._safe(lambda: self.http.get(result.url, timeout=180))
                    # The first partner mirror sometimes 404s a file another mirror
                    # still serves; the API hands out a different mirror per domain_index.
                    for domain_index in (1, 2, 3):
                        if response is not None and response.is_pdf:
                            break
                        alt = self._safe(lambda: annas_fast_download(self.http, host, candidate, key,
                                                                     domain_index=domain_index))
                        if alt is None or alt.status != "ok":
                            break
                        response = self._safe(lambda: self.http.get(alt.url, timeout=180))
                    if response is None or not response.is_pdf:
                        outcome.attempts.append(Attempt("annas-fast-download", host,
                                                        "download url did not return a PDF on any mirror"))
                        continue
                    staged = self._stage(response.content, work, name)
                    verification = verify_pdf(staged, work)
                    if verification.verdict == "mismatch":
                        staged.unlink(missing_ok=True)
                        outcome.attempts.append(Attempt("annas-fast-download", host,
                                                        f"md5 {candidate[:8]}… is a different work"))
                        break
                    outcome.attempts.append(Attempt("annas-fast-download", host, "ok"))
                    return self._finish(outcome, str(staged), "annas-fast-download", work)
                outcome.attempts.append(Attempt("annas-fast-download", host, f"{result.status}: {result.detail}"))
                if result.status == "quota":
                    # Slow partner downloads are unlimited and do not need the API,
                    # but their pages sit behind the bot challenge: hand the md5
                    # page to the browser job.
                    challenged.append((f"https://{host}/md5/{candidate}", "md5"))
                    outcome.message = ("Fast-download quota exhausted for today; the browser job fetches "
                                       "the slow partner copy instead.")
                    return None
                if result.status == "not-member":
                    outcome.status = "not-member"
                    outcome.message = ("Anna's Archive answered 'Not a member': the membership behind the "
                                       f"configured key has lapsed. Renew it, then rerun. md5 known: {candidate}.")
                    return outcome
                if result.status in ("invalid-key", "quota"):
                    outcome.status = "error"
                    outcome.message = f"Anna's Archive fast download: {result.status} ({result.detail})"
                    return outcome
                break  # other mirrors share the same account state
        return None
