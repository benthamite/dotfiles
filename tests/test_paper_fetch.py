"""Regression tests for lib/python/paper_fetch.py (no network)."""

from __future__ import annotations

import importlib.util
import hashlib
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
LIB = ROOT / "lib" / "python" / "paper_fetch.py"
CLI = LIB.with_name("paper_fetch_cli.py")
_SPEC = importlib.util.spec_from_file_location("paper_fetch", LIB)
pf = importlib.util.module_from_spec(_SPEC)
sys.modules["paper_fetch"] = pf
_SPEC.loader.exec_module(pf)
_ZSPEC = importlib.util.spec_from_file_location("paper_fetch_zlib", LIB.with_name("paper_fetch_zlib.py"))
zlib = importlib.util.module_from_spec(_ZSPEC)
sys.modules["paper_fetch_zlib"] = zlib
_ZSPEC.loader.exec_module(zlib)

MD5_A = "a" * 32
MD5_B = "b" * 32
MD5_C = "c" * 32
MD5_D = "d" * 32
MD5_E = "e" * 32
DUMP_NAME = "annas_archive_meta__aacid__zlib3_records__20240809T171652Z--20260706T193143Z.jsonl.seekable.zst"


def zlib_book(zid, md5, title, author, *, extension="pdf", isbns=(), year="1990", **extra):
    """A crawled zlib3 record as the dump stores it."""
    return {"zlibrary_id": zid, "date_added": "2022-08-24", "date_modified": "2023-04-05", "extension": extension,
            "filesize_reported": 1234567, "md5_reported": md5, "title": title, "author": author,
            "publisher": "Publisher", "language": "english", "series": "", "volume": "", "edition": "",
            "year": year, "pages": "300", "description": "Long blurb that the index must not keep.",
            "cover_path": "/covers/x.jpg", "isbns": list(isbns), "category_id": "", **extra}


def annabookinfo(zid, md5, *, deleted="", storage="userbooks", identifier=None, **fields):
    response = {"id": str(zid), "title": fields.get("title", ""), "author": fields.get("author", ""),
                "volume": "", "year": fields.get("year", ""), "edition": "", "publisher": "", "identifier": identifier,
                "language": "", "pages": "0", "cover": "", "storage": storage, "deleted_comment": deleted,
                "filesize": "1234567", "extension": fields.get("extension", "pdf"), "ipfs_cid": None,
                "ipfs_cid_blake2b": None, "md5": md5, "description": "", "downloadUrl": f"https://z-library.se/dl/{zid}/x"}
    return {"zlibrary_id": zid, "annabookinfo": {"errors": [], "response": response}}


def write_dump(directory, records, name=DUMP_NAME):
    """A zstd-compressed zlib3_records dump of METADATA records, in AAC (time) order."""
    lines = "".join(json.dumps({"aacid": f"aacid__zlib3_records__20240809T171652Z__{m['zlibrary_id']}__{i}",
                                "metadata": m}, ensure_ascii=True) + "\n" for i, m in enumerate(records))
    path = Path(directory) / name
    subprocess.run(["zstd", "-q", "-f", "-o", str(path)], input=lines.encode(), check=True)
    return path


_FIXTURE_DIR = tempfile.TemporaryDirectory()


def setUpModule():
    # Discovery searches the Z-Library index by default; point it at a fixture
    # holding one unrelated book so no test reads the real index.
    dump = write_dump(_FIXTURE_DIR.name, [zlib_book(1, MD5_E, "Unrelated Fixture Volume", "Nobody, A.")])
    index = Path(_FIXTURE_DIR.name) / "index.sqlite"
    zlib.build_index(dump, index)
    os.environ[pf.ZLIB_INDEX_ENV] = str(index)


def tearDownModule():
    os.environ.pop(pf.ZLIB_INDEX_ENV, None)
    _FIXTURE_DIR.cleanup()


class HttpTrustTests(unittest.TestCase):
    def test_default_uses_the_pinned_ca_bundle_with_verification_enabled(self):
        with mock.patch.object(pf, "_requests") as requests, \
             mock.patch.object(pf, "_certifi") as certifi, \
             mock.patch.dict(os.environ, {}, clear=True):
            certifi.where.return_value = "/runtime/certifi/cacert.pem"
            pf.Http()
        requests.Session.assert_called_once_with(
            impersonate="chrome", verify="/runtime/certifi/cacert.pem")

    def test_explicit_ca_overrides_keep_the_existing_precedence(self):
        cases = [
            ({"SSL_CERT_FILE": "/custom/ssl.pem"}, "/custom/ssl.pem"),
            ({"SSL_CERT_FILE": "/custom/ssl.pem", "CURL_CA_BUNDLE": "/custom/curl.pem"},
             "/custom/curl.pem"),
            ({"SSL_CERT_FILE": "/custom/ssl.pem", "CURL_CA_BUNDLE": "/custom/curl.pem",
              "REQUESTS_CA_BUNDLE": "/custom/requests.pem"}, "/custom/requests.pem"),
        ]
        for environment, expected in cases:
            with self.subTest(environment=environment), \
                 mock.patch.object(pf, "_requests") as requests, \
                 mock.patch.object(pf, "_certifi") as certifi, \
                 mock.patch.dict(os.environ, environment, clear=True):
                pf.Http()
                requests.Session.assert_called_once_with(impersonate="chrome", verify=expected)
                certifi.where.assert_not_called()


class FakeHttp:
    """Routes requests by URL substring to canned responses; records calls."""

    def __init__(self, routes):
        self.routes = routes
        self.calls = []

    def _respond(self, url, params):
        self.calls.append((url, params or {}))
        for needle, response in self.routes:
            if needle in url:
                if callable(response):
                    return response(url, params or {})
                return response
        return pf.Response(404, b"", url)

    def get(self, url, *, params=None, headers=None, timeout=None):
        return self._respond(url, params)

    def post(self, url, *, data=None, timeout=None):
        return self._respond(url, data)


def html(body, status=200, url="https://x/"):
    return pf.Response(status, body.encode(), url, "text/html")


def pdf_bytes(text="Rational Moral Ignorance Zach Barnett 10.1111/phpr.12684"):
    # Minimal valid-enough PDF: header plus a text stream pdftotext can read.
    stream = f"BT /F1 12 Tf 72 720 Td ({text}) Tj ET".encode()
    body = (
        b"%PDF-1.4\n1 0 obj<</Type/Catalog/Pages 2 0 R>>endobj\n"
        b"2 0 obj<</Type/Pages/Kids[3 0 R]/Count 1>>endobj\n"
        b"3 0 obj<</Type/Page/Parent 2 0 R/MediaBox[0 0 612 792]/Contents 4 0 R"
        b"/Resources<</Font<</F1 5 0 R>>>>>>endobj\n"
        b"4 0 obj<</Length " + str(len(stream)).encode() + b">>stream\n" + stream + b"\nendstream endobj\n"
        b"5 0 obj<</Type/Font/Subtype/Type1/BaseFont/Helvetica>>endobj\n"
        b"trailer<</Root 1 0 R>>\n%%EOF\n"
    )
    return body + b" " * max(0, 2100 - len(body))


class IdentifierTests(unittest.TestCase):
    def test_classifies_each_identifier_kind(self):
        self.assertEqual(pf.classify_identifier("10.1111/phpr.12684"), ("doi", "10.1111/phpr.12684"))
        self.assertEqual(pf.classify_identifier("https://doi.org/10.1111/PHPR.12684"), ("doi", "10.1111/phpr.12684"))
        self.assertEqual(pf.classify_identifier("doi:10.1111/phpr.12684"), ("doi", "10.1111/phpr.12684"))
        self.assertEqual(pf.classify_identifier("arXiv:2402.03204"), ("arxiv", "2402.03204"))
        self.assertEqual(pf.classify_identifier("https://arxiv.org/abs/2402.03204v2"), ("arxiv", "2402.03204v2"))
        self.assertEqual(pf.classify_identifier(MD5_A), ("md5", MD5_A))
        self.assertEqual(pf.classify_identifier("https://link.springer.com/article/x")[0], "url")
        self.assertEqual(pf.classify_identifier("Expected choiceworthiness and fanaticism")[0], "title")

    def test_slug_uses_surname_year_and_title_words(self):
        work = pf.Work(title="Expected Choiceworthiness and Fanaticism", authors=["Baker, Calvin"], year="2024")
        self.assertEqual(work.slug(), "Baker2024ExpectedChoiceworthinessFanaticism")


class AnnasArchiveTests(unittest.TestCase):
    def test_wikitext_domain_extraction_validates_host(self):
        self.assertEqual(pf.annas_home_url_from_wikitext("| url = {{URL|https://annas-archive.gd/}}"),
                         "https://annas-archive.gd/")
        self.assertEqual(pf.annas_home_url_from_wikitext("| url = {{URL|https://evil.example/}}"), "")

    def test_host_order_prefers_override_then_wikipedia_then_mirrors(self):
        http = FakeHttp([("wikipedia.org", html(json.dumps({"parse": {"wikitext": "{{URL|https://annas-archive.pk/}}"}})))])
        self.assertEqual(pf.resolve_annas_hosts(http, "annas-archive.gd")[:2], ["annas-archive.gd", "annas-archive.pk"])
        self.assertEqual(pf.resolve_annas_hosts(None)[0], pf.ANNAS_MIRRORS[0])

    def test_refuses_to_send_key_to_foreign_host(self):
        with self.assertRaises(pf.PaperFetchError):
            pf.resolve_annas_hosts(None, override="evil.example")
        with self.assertRaises(pf.PaperFetchError):
            pf.annas_fast_download(FakeHttp([]), "evil.example", MD5_A, "k")

    def test_fast_download_status_taxonomy(self):
        def api(error=None, url=None, status=200):
            return pf.Response(status, json.dumps({"download_url": url, "error": error}).encode(), "u")
        cases = {
            "not-member": api("Not a member", status=403),
            "invalid-key": api("Invalid secret key", status=403),
            "quota": api("Daily quota exhausted", status=429),
            "ok": api(url="https://partner/x.pdf"),
        }
        for expected, response in cases.items():
            result = pf.annas_fast_download(FakeHttp([("fast_download", response)]), "annas-archive.gl", MD5_A, "k")
            self.assertEqual(result.status, expected, expected)
        challenge = pf.annas_fast_download(FakeHttp([("fast_download", html("<title>DDoS-Guard</title>", 403))]),
                                           "annas-archive.gl", MD5_A, "k")
        self.assertEqual(challenge.status, "challenge")

    def test_libgen_md5_lookup_filters_other_dois(self):
        payload = {"1": {"doi": "10.1/x", "files": {"9": {"md5": MD5_A}}},
                   "2": {"doi": "10.1/other", "files": {"8": {"md5": MD5_B}}}}
        http = FakeHttp([("libgen", html(json.dumps(payload)))])
        self.assertEqual(pf.libgen_md5s(http, "10.1/x"), [MD5_A])

    def test_libgen_isbn_lookup_returns_ranked_file_records(self):
        editions = {"1": {"title": "Introduction to Algorithms", "year": "2009", "author": "Cormen",
                          "files": {"9": {"f_id": "4493924", "md5": MD5_A}}}}
        files = {"4493924": {"md5": MD5_A, "extension": "pdf", "filesize": "5076764", "pages": "1313",
                             "scanned": "", "vector": "", "ocr": "Y", "locator": "L:\\bib\\Cormen.pdf"}}
        http = FakeHttp([("libgen", lambda url, params: html(json.dumps(files if params.get("object") == "f" else editions)))])
        records = pf.libgen_isbn_files(http, "978-0-262-03384-8")
        self.assertEqual(len(records), 1)
        record = records[0]
        self.assertEqual((record["md5"], record["extension"], record["size_bytes"], record["filename"]),
                         (MD5_A, "pdf", 5076764, "Cormen.pdf"))
        self.assertEqual(record["title"], "Introduction to Algorithms")
        # the API rejects formatted ISBNs, so the request must carry digits only
        self.assertEqual(http.calls[0][1]["isbn"], "9780262033848")
        self.assertEqual(pf.libgen_isbn_files(FakeHttp([("libgen", html("[]"))]), "9780199262479"), [])
        self.assertEqual(pf.libgen_isbn_files(http, "12345"), [])

    def test_scidb_parser_ignores_related_cards(self):
        page = ('<div class="js-aarecord">scihub/10.1/other.pdf <a href="/md5/' + MD5_B + '">r</a></div>'
                '<div class="js-aarecord">scihub/10.1/x.pdf <a href="/md5/' + MD5_A + '">r</a></div>')
        self.assertEqual(pf.scidb_md5s_from_html(page, "10.1/x"), [MD5_A])
        self.assertEqual(pf.scidb_md5s_from_html('<a href="/md5/' + MD5_B + '">r</a>', "10.1/x"), [])


class ChallengeDetectionTests(unittest.TestCase):
    def test_challenge_pages_are_recognised_and_pdfs_are_not(self):
        self.assertTrue(html("<title>Just a moment...</title>", 403).is_challenge)
        self.assertTrue(html("Checking your browser before accessing", 200).is_challenge)
        self.assertFalse(pf.Response(200, pdf_bytes(), "u").is_challenge)
        self.assertFalse(html("<h1>No files found.</h1>").is_challenge)


class VerificationTests(unittest.TestCase):
    def test_header_check_does_not_read_the_entire_pdf(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "book.pdf"
            path.write_bytes(pdf_bytes())
            with mock.patch.object(Path, "read_bytes", side_effect=AssertionError("unbounded read")), \
                 mock.patch.object(pf, "pdf_text", return_value=""), mock.patch.object(pf, "pdf_pages", return_value=1):
                self.assertEqual(pf.verify_pdf(path, None).verdict, "unverified-no-metadata")

    def test_verifies_by_doi_or_title_and_rejects_mismatch(self):
        work = pf.Work(doi="10.1111/phpr.12684", title="Rational Moral Ignorance", authors=["Barnett, Zach"])
        with tempfile.TemporaryDirectory() as tmp:
            good = Path(tmp) / "good.pdf"; good.write_bytes(pdf_bytes())
            bad = Path(tmp) / "bad.pdf"; bad.write_bytes(pdf_bytes("Completely Unrelated Treatise on Beekeeping " * 5))
            notpdf = Path(tmp) / "x.pdf"; notpdf.write_bytes(b"<html>challenge</html>" + b" " * 3000)
            with mock.patch.object(pf, "pdf_text", side_effect=lambda p, pages=3: p.read_bytes().decode("latin1")), \
                 mock.patch.object(pf, "pdf_pages", return_value=1):
                self.assertEqual(pf.verify_pdf(good, work).verdict, "verified")
                self.assertEqual(pf.verify_pdf(bad, work).verdict, "mismatch")
            self.assertEqual(pf.verify_pdf(notpdf, work).verdict, "not-pdf")


BOOK_TARGET = {"title": "An Example Book", "author": "Smith, Alice", "year": "1960",
               "edition": "first", "language": "english", "isbn": "9780262033848"}


def libgen_search_page(rows, total=None):
    """LibGen index.php result page in the live layout (Files tab counter + tablelibgen rows)."""
    body = "".join(
        f'<tr><td><a data-toggle="tooltip" title="ID: {fid}" href="edition.php?id=7{fid}">{title} <i></i></a>'
        f'<br><a href="edition.php?id=7{fid}"><i><font color="green"> 9780262033848; 0262033844</font></a></i> '
        f'<nobr><span class="badge badge-primary"><a title="Book">b</a></span></nobr></td>'
        f'<td>{author}</td><td>Publisher</td><td><nobr>{year}</nobr></td><td>English</td><td>300</td>'
        f'<td><nobr><a href="/file.php?id={fid}">5 MB</a></nobr></td><td>pdf</td>'
        f'<td><nobr><a title="libgen" href="/ads.php?md5={"c" * 32}"><span>1</span></a></nobr></td></tr>'
        for fid, title, author, year in rows)
    total = len(rows) if total is None else total
    return (f'<a class="nav-link active " href="/index.php?req=x&res=100&curtab=f">Files '
            f'<span class="badge badge-primary">{total}</span></a>'
            + (f'<table class="table" id="tablelibgen"><thead><tr><th>ID</th></tr></thead><tbody>{body}</tbody></table>'
               if rows else ""))


# One result card in the live Anna's Archive search layout (2026-09), with a
# format outside the old PDF/EPUB/... whitelist.
ANNAS_RESULT_CARD = f"""
<div class="flex pt-3 pb-3 border-b">
  <a href="/md5/{MD5_B}" class="custom-a block mr-2"><div class="w-20"><img src="x.jpg" alt="">
    <div class="hidden js-aarecord-list-fallback-cover"><div data-content="Pride And Prejudice"></div></div></div></a>
  <div class="max-w-full"><div>
    <div class="text-[9px] font-mono">lgli/2005\\Jane Austen - Pride And Prejudice [Rtf].rtf</div>
    <a href="/md5/{MD5_B}" class="line-clamp-[3] font-semibold text-lg">Pride And Prejudice</a>
    <a href="/search?q=Austen, Jane" class="text-sm"><span class="icon"></span> Austen, Jane</a></div>
    <div class="text-gray-800 font-semibold text-sm">English [en] · RTF · 2.2MB · 1813 · 📕&nbsp;Book (fiction) · 🚀/lgli/lgrs/zlib · <a href="#">Save</a></div>
  </div>
</div>"""


def annas_search_page(query, cards=ANNAS_RESULT_CARD):
    return (f'<html><head><title>{query} - Search</title></head><body>'
            f'<input type="search" tabindex="0" name="q" placeholder="Title, author" value="{query}" class="js">'
            f'{cards}</body></html>')


def reviewed_candidate(path, content):
    """An independently supplied review of a disposable test PDF."""
    return {"file": str(path), "sha256": hashlib.sha256(content).hexdigest(),
            "identity": {"status": "verified", "evidence": "Title and author on title page."},
            "edition": {"status": "verified", "evidence": "Copyright page: first edition, 1960."},
            "language": {"status": "verified", "evidence": "English text checked on interior pages."},
            "completeness": {"status": "verified", "evidence": "Contents and final page agree with published extent."},
            "physical_pages": {"status": "verified", "evidence": "Rendered pages match the publisher's pagination."}}


class BookCandidateTests(unittest.TestCase):
    def test_local_publisher_candidate_enters_the_same_review_workflow_unapproved(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "publisher.pdf"
            content = pdf_bytes("An Example Book")
            source.write_bytes(content)
            original = {"version": 1, "target": BOOK_TARGET, "candidates": [], "search_complete": False}
            manifest = pf.register_book_candidate(original, source, "https://publisher.example/books/example")
            self.assertEqual(original["candidates"], [])
            candidate = manifest["candidates"][0]
            self.assertEqual(candidate["title"], "")
            self.assertEqual(candidate["language"], "")
            self.assertEqual(candidate["size_bytes"], len(content))
            self.assertEqual(candidate["md5"], hashlib.md5(content).hexdigest())
            empty_reviews = {"version": 1, "target": BOOK_TARGET, "candidates": {}}
            self.assertIsNone(pf.select_book_candidate(manifest, empty_reviews)["selected"])
            staged = pf.stage_book_candidate(manifest, candidate["md5"], Path(tmp) / "stage")
            self.assertEqual(staged["status"], "needs-review")
            self.assertEqual(Path(staged["file"]).read_bytes(), content)
            reviews = {"version": 1, "target": BOOK_TARGET, "candidates": {
                candidate["md5"]: reviewed_candidate(Path(staged["file"]), content)}}
            self.assertEqual(pf.select_book_candidate(manifest, reviews)["selected"]["md5"], candidate["md5"])

    def test_registration_rejects_private_credentials_and_signed_source_urls(self):
        for url in ("file:///tmp/book.pdf", "https://alice:password@example.com/book", "http://localhost/book",
                    "http://127.0.0.1/book", "https://publisher.example/book?token=secret",
                    "https://publisher.example/book?X-Amz-Signature=secret", "https://publisher.example/book#api_key=secret"):
            with self.subTest(url=url), self.assertRaises(pf.PaperFetchError):
                pf.public_book_source_url(url)
        self.assertEqual(pf.public_book_source_url("https://books.example/books?id=123"),
                         "https://books.example/books?id=123")

    def test_discovery_retains_scan_hints_and_does_not_read_credentials(self):
        editions = {"1": {"title": "An Example Book", "year": "1960", "author": "Smith, Alice",
                          "files": {"9": {"f_id": "9", "md5": MD5_A}}}}
        files = {"9": {"md5": MD5_A, "extension": "pdf", "filesize": "", "pages": "300",
                       "scanned": "Y", "vector": "", "ocr": "Y", "locator": "scan.pdf"}}
        http = FakeHttp([
            ("libgen.li/index.php", html(libgen_search_page([]))),
            ("libgen", lambda url, params: html(json.dumps(files if params.get("object") == "f" else editions))),
            ("/search", html("<title>DDoS-Guard</title>", 403)),
        ])
        with mock.patch.object(pf, "annas_secret_key", side_effect=AssertionError("credentials not permitted")):
            result = pf.discover_book_candidates(http, BOOK_TARGET, annas_host="annas-archive.gl")
        self.assertEqual(result["status"], "needs-review")
        self.assertEqual([a["status"] for a in result["attempts"]], ["ok", "ok", "ok", "needs-browser"])
        self.assertFalse(result["search_complete"])
        self.assertTrue(result["candidates"][0]["scanned"])
        self.assertTrue(result["candidates"][0]["ocr"])
        self.assertIsNone(result["candidates"][0]["size_bytes"])
        self.assertNotIn("selected", result)

    def test_search_distinguishes_a_recognized_empty_page_from_unknown_html(self):
        self.assertEqual(pf.parse_annas_book_results("<div>No files found.</div>"), [])
        with self.assertRaises(pf.PaperFetchError):
            pf.parse_annas_book_results("<html>New site layout</html>")
        with self.assertRaises(pf.PaperFetchError):
            pf.libgen_isbn_files(FakeHttp([("libgen", html("{}", 500))]), "9780262033848", strict=True)
        for payload in ({"error": "temporarily unavailable"}, {"1": {"title": "Book", "files": "9"}},
                        {"1": {"files": {"9": {}}}}):
            with self.subTest(payload=payload), self.assertRaises(pf.PaperFetchError):
                pf.libgen_isbn_files(FakeHttp([("libgen", html(json.dumps(payload)))]), "9780262033848", strict=True)
        http = FakeHttp([("libgen", lambda url, params: html(json.dumps(
            {} if params.get("object") == "f" else {"1": {"files": {"9": {"f_id": "9"}}}})))])
        with self.assertRaises(pf.PaperFetchError):
            pf.libgen_isbn_files(http, "9780262033848", strict=True)

    def test_libgen_isbn_edition_without_files_contributes_nothing(self):
        # LibGen lists chapter and catalogue-only editions under the ISBN without a
        # "files" key (Carruthers 2005, ISBN 0195179676); strict lookup kept failing.
        editions = {"1": {"title": "The Innate Mind", "year": "2005", "files": {"7": {"f_id": "9", "md5": MD5_A}}},
                    "2": {"title": "The Innate Mind || Preface", "year": "2005"},
                    "3": {"title": "The Innate Mind || Copyright Page", "year": "2005", "files": []}}
        files = {"9": {"md5": MD5_A, "extension": "pdf", "filesize": "3266383", "locator": "innate.pdf"}}
        http = FakeHttp([("libgen", lambda url, params: html(json.dumps(files if params.get("object") == "f" else editions)))])
        records = pf.libgen_isbn_files(http, "0195179676", strict=True)
        self.assertEqual([(r["md5"], r["title"]) for r in records], [(MD5_A, "The Innate Mind")])
        only_empty = FakeHttp([("libgen", html(json.dumps({"2": {"title": "The Innate Mind || Preface"}})))])
        self.assertEqual(pf.libgen_isbn_files(only_empty, "0195179676", strict=True), [])
        self.assertEqual(len(only_empty.calls), 1, "no file lookup for editions without files")

    def test_libgen_text_search_reads_files_attached_to_no_edition(self):
        # The Jacobs 1996 "Marque jaune" search lists edition-less comic files as
        # one colspan=5 cell (file name and path) plus pages, size, format, mirrors.
        orphan = ('<tr>\n<td colspan=5>\n  <div class="row"><div class="col"><span data-toggle="tooltip" '
                  'title="Time added/Time modified : 2021-03-14 06:13:55/2023-07-01 09:48:28<br>File ID: 91091253" >'
                  'Jacobs, Edgar Pierre - 1987 - Blake et Mortimer T06 La marque jaune<br><font size=1 color="gray">'
                  'ftp\\[BD.FR] French Comics</font></span></div></div>\n</td>\n<td>72</td>\n'
                  '<td><nobr><a href="/file.php?id=91091253">20 MB</a></nobr></td>\n<td>cbz</td>\n'
                  f'<td><a title="libgen" href="/get.php?md5={MD5_B}"><span>Libgen</span></a></td>\n</tr>')
        page = libgen_search_page([("11", "La marque jaune", "Jacobs, Edgar Pierre", "1996")], total=2)
        page = page.replace("</tbody>", orphan + "</tbody>")
        files = {"11": {"md5": MD5_A, "extension": "pdf", "filesize": "5000000", "locator": "a/jaune.pdf"},
                 "91091253": {"md5": MD5_B, "extension": "cbz", "filesize": "20971520",
                              "locator": "[BD.FR] French Comics\\Blake et Mortimer T06 La marque jaune.cbz"}}
        http = FakeHttp([("libgen.li/index.php", html(page)), ("json.php", lambda url, params: html(json.dumps(files)))])
        records, total = pf.libgen_search_files(http, "La marque jaune Jacobs, Edgar Pierre", strict=True)
        self.assertEqual(total, 2)
        by_md5 = {record["md5"]: record for record in records}
        self.assertEqual(by_md5[MD5_A]["title"], "La marque jaune")
        orphan_record = by_md5[MD5_B]
        self.assertEqual((orphan_record["title"], orphan_record["author"], orphan_record["year"]), ("", "", ""))
        self.assertEqual(orphan_record["filename"], "Blake et Mortimer T06 La marque jaune.cbz")
        self.assertEqual(http.calls[-1][1]["ids"], "11,91091253")

    def test_libgen_text_search_reads_ids_from_html_and_files_from_json(self):
        files = {"11": {"md5": MD5_A, "extension": "pdf", "filesize": "5000000", "scanned": "Y", "locator": "a/b.pdf"}}
        http = FakeHttp([
            ("libgen.li/index.php", html(libgen_search_page([("11", "An Example Book", "Smith, Alice", "1960")],
                                                            total=250))),
            ("json.php", lambda url, params: html(json.dumps(files))),
        ])
        records, total = pf.libgen_search_files(http, "An Example Book Smith", strict=True)
        self.assertEqual(total, 250)
        self.assertEqual(records[0]["md5"], MD5_A)
        self.assertEqual(records[0]["title"], "An Example Book")
        self.assertEqual(records[0]["author"], "Smith, Alice")
        self.assertEqual(records[0]["year"], "1960")
        self.assertEqual(records[0]["size_bytes"], 5000000)
        self.assertEqual(http.calls[-1][1], {"object": "f", "ids": "11"})
        empty = FakeHttp([("libgen.li/index.php", html(libgen_search_page([])))])
        self.assertEqual(pf.libgen_search_files(empty, "nothing", strict=True), ([], 0))
        for page in ("<html>maintenance</html>", libgen_search_page([], total=3)):
            with self.subTest(page=page[:30]), self.assertRaises(pf.PaperFetchError):
                pf.libgen_search_files(FakeHttp([("libgen.li/index.php", html(page))]), "x", strict=True)

    def test_truncated_text_search_keeps_the_inventory_incomplete(self):
        target = {**BOOK_TARGET, "isbn": ""}
        files = {"11": {"md5": MD5_A, "extension": "pdf", "filesize": "5000000"}}
        http = FakeHttp([
            ("libgen.li/index.php", html(libgen_search_page([("11", "An Example Book", "Smith, Alice", "1960")],
                                                            total=250))),
            ("json.php", lambda url, params: html(json.dumps(files))),
        ])
        result = pf.discover_book_candidates(http, target, annas_search_html=annas_search_page(
            "An Example Book Smith, Alice"))
        self.assertEqual(result["status"], "needs-review")
        self.assertTrue(result["attempts"][0]["truncated"])
        self.assertFalse(result["search_complete"])

    def test_parser_reads_the_live_result_card_layout_with_any_format(self):
        record = pf.parse_annas_book_results(annas_search_page("Austen Pride and Prejudice"))[0]
        self.assertEqual(record["md5"], MD5_B)
        self.assertEqual(record["format"], "rtf")
        self.assertEqual(record["title"], "Pride And Prejudice")
        self.assertEqual(record["authors"], "Austen, Jane")
        self.assertEqual(record["size_bytes"], int(2.2 * 1024**2))
        self.assertEqual(record["year"], "1813")
        self.assertTrue(record["filename"].endswith("[Rtf].rtf"))

    def test_challenged_annas_search_names_the_browser_step_instead_of_failing_opaquely(self):
        target = {**BOOK_TARGET, "isbn": ""}
        http = FakeHttp([
            ("libgen.li/index.php", html(libgen_search_page([]))),
            ("/search", html("<title>DDoS-Guard</title>", 403)),
        ])
        result = pf.discover_book_candidates(http, target, annas_host="annas-archive.gl")
        self.assertEqual(result["status"], "needs-browser")
        attempt = result["attempts"][-1]
        self.assertEqual(attempt["status"], "needs-browser")
        self.assertIn("no search API", attempt["error"])
        browser = result["annas_browser_search"]
        self.assertEqual(browser["url"],
                         "https://annas-archive.gl/search?q=An+Example+Book+Smith%2C+Alice&content=book_any")
        name = pf.annas_search_save_name("An Example Book Smith, Alice")
        self.assertTrue(browser["save_as"].endswith(name))
        self.assertIn(json.dumps(name), browser["snippet"])
        self.assertIn("fetch(location.href", browser["snippet"])
        self.assertEqual(browser["rerun_with"], f"--annas-search-html {browser['save_as']}")

    def test_browser_saved_annas_page_completes_the_search_only_for_the_same_query(self):
        target = {**BOOK_TARGET, "isbn": ""}
        http = FakeHttp([("libgen.li/index.php", html(libgen_search_page([])))])
        result = pf.discover_book_candidates(http, target, annas_search_html=annas_search_page(
            "An Example  Book Smith, Alice"))
        self.assertEqual(result["status"], "needs-review")
        self.assertTrue(result["search_complete"])
        self.assertEqual(result["candidates"][0]["md5"], MD5_B)
        self.assertEqual(result["attempts"][-1]["source"], "browser-html")
        self.assertFalse(any("/search" in url for url, _ in http.calls), "saved page replaces the live search")
        self.assertNotIn("annas_browser_search", result)
        with self.assertRaises(pf.PaperFetchError):
            pf.discover_book_candidates(http, target, annas_search_html=annas_search_page("Another Book"))

    def test_saved_page_without_exact_matches_does_not_complete_the_search(self):
        # Anna's shows "No files found" (sometimes spuriously, when its search is slow)
        # above partial matches; neither the empty nor the partial list is exhaustive.
        target = {**BOOK_TARGET, "isbn": ""}
        http = FakeHttp([("libgen.li/index.php", html(libgen_search_page([])))])
        for cards in ("<div>No files found.</div>", "<div>No files found.</div>" + ANNAS_RESULT_CARD):
            with self.subTest(cards=len(cards)):
                result = pf.discover_book_candidates(http, target, annas_search_html=annas_search_page(
                    "An Example Book Smith, Alice", cards=cards))
                self.assertFalse(result["search_complete"])
                self.assertIn("slow-search", result["attempts"][-1]["incomplete"])
                self.assertEqual(result["status"], "needs-review" if "md5" in cards else "unknown")

    def test_candidate_parser_preserves_unknown_size_without_claiming_fidelity(self):
        page = (f'<a href="/md5/{MD5_A}">scan.pdf</a><div>An Example Book</div><div>Alice Smith</div>'
                '<div>English [en] · PDF · unknown · 1960 · Book</div>')
        candidates = pf.parse_annas_book_results(page)
        self.assertEqual(candidates[0]["title"], "An Example Book")
        self.assertEqual(candidates[0]["year"], "1960")
        self.assertEqual(candidates[0]["format"], "pdf")
        self.assertIsNone(candidates[0]["size_bytes"])
        self.assertNotIn("physical_pages", candidates[0])
        for size in ("5MB", "120KB", "2GB", "5.2MB"):
            parsed = pf.parse_annas_book_results(page.replace("unknown", size))[0]
            self.assertEqual(parsed["format"], "pdf")
            self.assertIsInstance(parsed["size_bytes"], int)

    def test_filename_and_filesize_never_approve_a_candidate(self):
        manifest = {"version": 1, "target": BOOK_TARGET, "candidates": [
            {"md5": MD5_A, "format": "pdf", "filename": "publisher-scan.pdf", "size_bytes": 5000000},
        ]}
        result = pf.select_book_candidate(manifest, {"version": 1, "target": BOOK_TARGET, "candidates": {}})
        self.assertEqual(result["status"], "needs-review")
        self.assertIsNone(result["selected"])
        self.assertEqual(result["pending"][0]["checks"], list(pf.BOOK_REVIEW_CHECKS))

    def test_selects_by_measured_bytes_after_review_even_for_small_or_ebook_named_pdf(self):
        with tempfile.TemporaryDirectory() as tmp:
            smaller = pdf_bytes("An Example Book")
            larger = smaller + b" " * 5000
            small_path, big_path = Path(tmp) / "ebook.pdf", Path(tmp) / "scan.pdf"
            small_path.write_bytes(smaller)
            big_path.write_bytes(larger)
            small_md5, big_md5 = hashlib.md5(smaller).hexdigest(), hashlib.md5(larger).hexdigest()
            manifest = {"version": 1, "target": BOOK_TARGET, "candidates": [
                {"md5": big_md5, "format": "pdf", "size_bytes": None},
                {"md5": small_md5, "format": "pdf", "filename": "ebook.pdf", "size_bytes": 9000000},
            ]}
            reviews = {"version": 1, "target": BOOK_TARGET, "candidates": {
                small_md5: reviewed_candidate(small_path, smaller), big_md5: reviewed_candidate(big_path, larger)}}
            result = pf.select_book_candidate(manifest, reviews)
            self.assertEqual(result["status"], "ok")
            self.assertFalse(result["search_complete"])
            self.assertEqual(result["selected"]["md5"], small_md5)
            self.assertEqual(result["selected"]["size_bytes"], len(smaller))
            # A wrong-edition PDF is ineligible, however small.
            reviews["candidates"][small_md5]["edition"] = {"status": "rejected", "evidence": "Second edition."}
            result = pf.select_book_candidate(manifest, reviews)
            self.assertEqual(result["selected"]["md5"], big_md5)
            self.assertEqual(result["rejected"][0]["reason"], "edition")

    def test_review_must_match_target_and_current_file_bytes(self):
        with tempfile.TemporaryDirectory() as tmp:
            content = pdf_bytes()
            path = Path(tmp) / "candidate.pdf"
            path.write_bytes(content)
            md5 = hashlib.md5(content).hexdigest()
            manifest = {"version": 1, "target": BOOK_TARGET, "candidates": [{"md5": md5, "format": "pdf"}]}
            reviews = {"version": 1, "target": BOOK_TARGET, "candidates": {md5: reviewed_candidate(path, content)}}
            path.write_bytes(content + b"changed")
            result = pf.select_book_candidate(manifest, reviews)
            self.assertEqual(result["status"], "needs-review")
            self.assertIn("bytes changed", result["pending"][0]["reason"])
            reviews["target"] = {**BOOK_TARGET, "year": "1970"}
            with self.assertRaises(pf.PaperFetchError):
                pf.select_book_candidate(manifest, reviews)

    def test_empty_evidence_and_unrecognized_statuses_are_not_approval(self):
        review = {name: {"status": "verified", "evidence": " "} for name in pf.BOOK_REVIEW_CHECKS}
        review["physical_pages"] = {"status": True, "evidence": "Looks plausible."}
        manifest = {"version": 1, "target": BOOK_TARGET, "candidates": [{"md5": MD5_A, "format": "pdf"}]}
        result = pf.select_book_candidate(manifest, {"version": 1, "target": BOOK_TARGET, "candidates": {MD5_A: review}})
        self.assertIsNone(result["selected"])

    def test_local_staging_is_explicit_preserves_source_and_checks_md5(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "download.pdf"
            content = pdf_bytes()
            source.write_bytes(content)
            md5 = hashlib.md5(content).hexdigest()
            manifest = {"version": 1, "target": BOOK_TARGET, "candidates": [{"md5": md5, "format": "pdf"}]}
            result = pf.stage_book_candidate(manifest, md5, Path(tmp) / "stage", local_file=source)
            self.assertEqual(result["status"], "needs-review")
            self.assertEqual(Path(result["file"]).read_bytes(), content)
            self.assertEqual(source.read_bytes(), content)
            with self.assertRaises(pf.PaperFetchError):
                pf.stage_book_candidate(manifest, MD5_A, Path(tmp) / "other", local_file=source)
            source.write_bytes(content + b"wrong")
            with self.assertRaises(pf.PaperFetchError):
                pf.stage_book_candidate(manifest, md5, Path(tmp) / "other", local_file=source)
            self.assertFalse((Path(tmp) / "other").exists())

    def test_remote_staging_uses_existing_fetcher_and_rejects_wrong_download(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            downloaded = directory / "wrong.pdf"
            downloaded.write_bytes(pdf_bytes())
            fetcher = mock.Mock(out_dir=directory)
            fetcher.fetch.return_value = pf.Outcome("ok", None, file=str(downloaded))
            manifest = {"version": 1, "target": BOOK_TARGET, "candidates": [{"md5": MD5_A, "format": "pdf"}]}
            with self.assertRaises(pf.PaperFetchError):
                pf.stage_book_candidate(manifest, MD5_A, directory, fetcher=fetcher)
            fetcher.fetch.assert_called_once_with(MD5_A, name=MD5_A)
            self.assertFalse(downloaded.exists())

    def test_book_staging_does_not_reject_a_valid_tiny_pdf_by_filesize(self):
        content = pdf_bytes().rstrip()
        self.assertLess(len(content), 2000)
        md5 = hashlib.md5(content).hexdigest()
        http = FakeHttp([
            ("fast_download", html(json.dumps({"download_url": "https://partner.example/book.pdf"}))),
            ("partner.example", pf.Response(200, content, "https://partner.example/book.pdf", "application/pdf")),
        ])
        with tempfile.TemporaryDirectory() as tmp:
            out = Path(tmp) / "stage"
            fetcher = pf.Fetcher(http, out, Path(tmp) / "jobs", annas_host="annas-archive.gl", key_reader=lambda: "fixture")
            manifest = {"version": 1, "target": BOOK_TARGET, "candidates": [{"md5": md5, "format": "pdf"}]}
            result = pf.stage_book_candidate(manifest, md5, out, fetcher=fetcher)
            self.assertEqual(result["status"], "needs-review")
            self.assertEqual(Path(result["file"]).read_bytes(), content)

    def test_cli_local_inspection_and_selection_need_no_network_or_credentials(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            source = directory / "example.pdf"
            content = pdf_bytes("An Example Book Alice Smith first edition 1960")
            source.write_bytes(content)
            md5 = hashlib.md5(content).hexdigest()
            manifest = directory / "candidates.json"
            manifest.write_text(json.dumps({"version": 1, "target": BOOK_TARGET, "candidates": []}))
            env = {**os.environ, "PYTHONDONTWRITEBYTECODE": "1"}
            registered_path = directory / "registered.json"
            registered = subprocess.run([sys.executable, str(CLI), "book-register", str(manifest),
                                         "--file", str(source), "--source", "https://publisher.example/book",
                                         "--out", str(registered_path)], capture_output=True, text=True, env=env, timeout=30)
            self.assertEqual(registered.returncode, 5, registered.stderr)
            self.assertEqual(json.loads(manifest.read_text())["candidates"], [])
            manifest = registered_path
            staged = subprocess.run([sys.executable, str(CLI), "book-stage", str(manifest),
                                     "--md5", md5, "--out", str(directory / "stage")],
                                    capture_output=True, text=True, env=env, timeout=30)
            self.assertEqual(staged.returncode, 5, staged.stderr)
            staged_path = Path(json.loads(staged.stdout)["file"])
            inspected = subprocess.run([sys.executable, str(CLI), "book-inspect", str(staged_path),
                                        "--out", str(directory / "inspection")],
                                       capture_output=True, text=True, env=env, timeout=60)
            self.assertEqual(inspected.returncode, 5, inspected.stderr)
            inspection = json.loads(inspected.stdout)
            self.assertEqual(inspection["pages"], 1)
            self.assertTrue(Path(inspection["images"][0]["file"]).read_bytes().startswith(b"\x89PNG"))
            self.assertIn("An Example Book", Path(inspection["text_file"]).read_text())
            reviews = directory / "reviews.json"
            reviews.write_text(json.dumps({"version": 1, "target": BOOK_TARGET,
                                           "candidates": {md5: reviewed_candidate(staged_path, content)}}))
            selected = subprocess.run([sys.executable, str(CLI), "book-select", str(manifest),
                                       "--reviews", str(reviews)], capture_output=True, text=True, env=env, timeout=30)
            self.assertEqual(selected.returncode, 0, selected.stderr)
            self.assertEqual(json.loads(selected.stdout)["selected"]["file"], str(staged_path))


class FetcherTests(unittest.TestCase):
    def _fetcher(self, http, tmp, key="k"):
        return pf.Fetcher(http, Path(tmp) / "out", Path(tmp) / "jobs", annas_host="annas-archive.gl",
                          key_reader=lambda: key, downloads_dir=Path(tmp) / "dl")

    def crossref(self):
        return html(json.dumps({"message": {"DOI": "10.1111/phpr.12684", "title": ["Rational Moral Ignorance"],
                                            "author": [{"family": "Barnett", "given": "Zach"}],
                                            "issued": {"date-parts": [[2021]]}, "URL": "https://doi.org/10.1111/phpr.12684"}}))

    def test_url_identity_ignores_reference_dois(self):
        url = "https://ojs.aamusicologia.ar/index.php/ram/en/article/view/472"
        title = "Italianidad, criollismo y tango en la revista Armonía de 1905"
        page = f"""<meta name='citation_title' content='{title}'>
        <meta content='María de la Paz Tagliabue' name='citation_author'/>
        <meta name='citation_date' content='2024/10/15'/>
        <meta name='citation_reference' content='https://doi.org/10.24201/hm.v72i3.4586'>
        <a href='https://doi.org/10.24201/hm.v72i3.4586'>Reference</a>
        <script>{{"doi":"10.24201/hm.v72i3.4586"}}</script>"""
        http = FakeHttp([(url, html(page))])
        with tempfile.TemporaryDirectory() as tmp:
            work, urls, _ = self._fetcher(http, tmp).resolve_work(url)
        self.assertEqual(work.title, title)
        self.assertEqual(work.authors, ["María de la Paz Tagliabue"])
        self.assertEqual(work.year, "2024")
        self.assertEqual(work.doi, "")
        self.assertEqual(urls, [url])
        self.assertEqual(len(http.calls), 1)

    def test_url_explicit_doi_handles_attribute_order_and_case(self):
        url = "https://publisher.example/article"
        for name in ("citation_doi", "DC.Identifier", "DC.Identifier.DOI"):
            with self.subTest(name=name), tempfile.TemporaryDirectory() as tmp:
                http = FakeHttp([(url, html(f"<meta content='10.1111/phpr.12684' name='{name}'>")),
                                 ("api.crossref.org", self.crossref())])
                work, _, _ = self._fetcher(http, tmp).resolve_work(url)
                self.assertEqual(work.title, "Rational Moral Ignorance")
                self.assertEqual(work.url, url)

    def test_url_rejects_registry_title_conflict(self):
        url = "https://publisher.example/article"
        http = FakeHttp([(url, html("<meta name='citation_title' content='An entirely different paper'>"
                                   "<meta name='citation_doi' content='10.1111/phpr.12684'>")),
                         ("api.crossref.org", self.crossref())])
        with tempfile.TemporaryDirectory() as tmp:
            work, _, _ = self._fetcher(http, tmp).resolve_work(url)
        self.assertEqual(work.title, "An entirely different paper")
        self.assertEqual(work.source, "url")
        self.assertEqual(work.doi, "")

    def test_paywalled_doi_goes_libgen_then_fast_download(self):
        http = FakeHttp([
            ("api.crossref.org", self.crossref()),
            ("unpaywall", html("{}", 404)),
            ("openalex", html("{}", 404)),
            ("doi.org/10.1111", html("<title>Just a moment...</title>", 403)),
            ("libgen", html(json.dumps({"1": {"doi": "10.1111/phpr.12684", "files": {"9": {"md5": MD5_A}}}}))),
            ("fast_download", html(json.dumps({"download_url": "https://partner.example/f.pdf"}))),
            ("partner.example", pf.Response(200, pdf_bytes(), "https://partner.example/f.pdf", "application/pdf")),
        ])
        with tempfile.TemporaryDirectory() as tmp, \
             mock.patch.object(pf, "pdf_text", side_effect=lambda p, pages=3: p.read_bytes().decode("latin1")), \
             mock.patch.object(pf, "pdf_pages", return_value=20):
            outcome = self._fetcher(http, tmp).fetch("10.1111/phpr.12684", name="Barnett2021RationalMoralIgnorance")
            self.assertEqual(outcome.status, "ok")
            self.assertEqual(outcome.route, "annas-fast-download")
            self.assertTrue(outcome.file.endswith("Barnett2021RationalMoralIgnorance.pdf"))
            self.assertEqual(outcome.verification.verdict, "verified")
            key_params = [p for u, p in http.calls if "fast_download" in u]
            self.assertEqual(key_params[0]["key"], "k")
            self.assertTrue(all("annas-archive" in u for u, p in http.calls if p.get("key")))

    def test_not_member_is_reported_with_md5_and_stops(self):
        http = FakeHttp([
            ("api.crossref.org", self.crossref()),
            ("doi.org/10.1111", html("<title>Just a moment...</title>", 403)),
            ("libgen", html(json.dumps({"1": {"doi": "10.1111/phpr.12684", "files": {"9": {"md5": MD5_A}}}}))),
            ("fast_download", html(json.dumps({"download_url": None, "error": "Not a member"}), 403)),
        ])
        with tempfile.TemporaryDirectory() as tmp:
            outcome = self._fetcher(http, tmp).fetch("10.1111/phpr.12684")
        self.assertEqual(outcome.status, "not-member")
        self.assertIn(MD5_A, outcome.message)
        self.assertEqual(sum(1 for u, _ in http.calls if "fast_download" in u), 1)

    def test_challenged_only_sources_produce_a_browser_job(self):
        http = FakeHttp([
            ("api.crossref.org", self.crossref()),
            ("unpaywall", html(json.dumps({"best_oa_location": {"url_for_pdf": "https://philpapers.org/archive/BARRMI.pdf"}}))),
            ("doi.org/10.1111", html("<html>landing, no pdf</html>")),
            ("libgen", html("{}")),
            ("scidb", html("<title>DDoS-Guard</title>", 403)),
        ])
        with tempfile.TemporaryDirectory() as tmp:
            fetcher = self._fetcher(http, tmp)
            outcome = fetcher.fetch("10.1111/phpr.12684")
            self.assertEqual(outcome.status, "needs-browser")
            job = outcome.browser_job
            origins = {g["origin"] for g in job.origins}
            self.assertEqual(origins, {"https://philpapers.org", "https://annas-archive.gl"})
            self.assertFalse(any("philpapers.org" in u for u, _ in http.calls), "challenged host must not be fetched from the CLI")
            snippet = Path(next(g["snippet"] for g in job.origins if "philpapers" in g["origin"])).read_text()
            self.assertIn("paperfetch-" + job.token + "-0.pdf", snippet)
            # collect: verified download is installed, no downloads reported clearly
            dl = Path(job.downloads_dir); dl.mkdir()
            self.assertEqual(pf.collect_browser_downloads(job, Path(tmp) / "out")["status"], "no-downloads")
            (dl / f"paperfetch-{job.token}-0.pdf").write_bytes(pdf_bytes())
            with mock.patch.object(pf, "pdf_text", side_effect=lambda p, pages=3: p.read_bytes().decode("latin1")), \
                 mock.patch.object(pf, "pdf_pages", return_value=1):
                result = pf.collect_browser_downloads(pf.load_browser_job(Path(job.path)), Path(tmp) / "out")
            self.assertEqual(result["status"], "ok")
            self.assertTrue(Path(result["file"]).exists())
            self.assertFalse((dl / f"paperfetch-{job.token}-0.pdf").exists(), "installed file is moved out of Downloads")


class ZlibIndexTests(unittest.TestCase):
    def build(self, tmp, records, name=DUMP_NAME):
        index = Path(tmp) / "index.sqlite"
        meta = zlib.build_index(write_dump(tmp, records, name), index, listed="2026-07-07")
        return index, meta

    def target(self, **fields):
        return pf.book_target({"title": "Games and Decisions", "author": "Luce, R. Duncan and Raiffa, Howard",
                               "year": "1957", "edition": "first", "language": "english", **fields})

    def test_build_merges_records_per_id_and_keeps_only_book_search_fields(self):
        records = [
            zlib_book(10, MD5_A, "Games and decisions", "Luce, R. Duncan", isbns=["0-486-65943-7"]),
            {"zlibrary_id": 11, "missing": 1},
            annabookinfo(10, MD5_A, storage="internet-archive", title="Games and decisions : introduction",
                         identifier="9780486659435,0486659437"),
            # A later crawl with an empty title must not erase the known one.
            zlib_book(10, MD5_A, "", "Luce, R. Duncan", year="1957"),
            # An annabookinfo-only record carries all the metadata there is.
            annabookinfo(12, MD5_B, title="Hermits", author="France, Peter", year="1996", extension="epub"),
            zlib_book(13, MD5_C, "A Paper on Proteins", "Lab, A.", isbns=[]),
            annabookinfo(13, MD5_C, storage="scimag"),
            zlib_book(14, "", "No file", "Nobody"),
            # A PDF that a later record reveals as DMCA-deleted, and one whose
            # DMCA deletion a later lookup lifted.
            zlib_book(15, MD5_D, "Taken down", "X"),
            annabookinfo(15, MD5_D, deleted="DMCA"),
            annabookinfo(16, MD5_E, deleted="DMCA", title="Restored", extension="pdf"),
            annabookinfo(16, MD5_E, deleted="", title="Restored", extension="pdf"),
        ]
        with tempfile.TemporaryDirectory() as tmp:
            index, meta = self.build(tmp, records)
            self.assertEqual((meta["snapshot"], meta["listed"], meta["books"]), ("2026-07-06T19:31:43Z", "2026-07-07", "2"))
            self.assertEqual(meta["dump"], DUMP_NAME)
            import sqlite3
            connection = sqlite3.connect(index)
            rows = {row[0]: row for row in connection.execute(
                "SELECT zlibrary_id, md5, title, year, isbns, extension, filesize FROM books")}
            self.assertEqual(set(rows), {10, 16}, "missing, non-PDF, scimag, DMCA and md5-less records are dropped")
            # Each field keeps its latest non-empty value, whichever record supplied it.
            self.assertEqual(rows[10][1:], (MD5_A, "Games and decisions : introduction", "1957",
                                            "9780486659435 0486659437", "pdf", 1234567))
            self.assertEqual(rows[16][2], "Restored")
            self.assertEqual(connection.execute("SELECT isbn FROM isbn").fetchall(), [("9780486659435",)])
            columns = {row[1] for row in connection.execute("PRAGMA table_info(books)")}
            self.assertEqual(columns, {"zlibrary_id", "md5", "title", "author", "publisher", "year", "language",
                                       "extension", "filesize", "isbns", "deleted"})
            self.assertEqual(dict(connection.execute("SELECT key, value FROM meta"))["schema"], pf.ZLIB_INDEX_SCHEMA)
            connection.close()

    def test_search_matches_isbn_forms_then_title_and_surname_and_skips_dmca(self):
        records = [
            zlib_book(20, MD5_A, "Games and decisions : introduction and critical survey",
                      "Luce, R. Duncan (Robert Duncan), Raiffa, Howard"),
            zlib_book(21, MD5_B, "Games & Decisions", "R. Duncan Luce", isbns=["0486659437"]),
            zlib_book(25, "f" * 32, "Games and decisions", "Luce", extension="djvu"),
            zlib_book(22, MD5_C, "Games and decisions", "Luce, R. Duncan"),
            annabookinfo(22, MD5_C, deleted="DMCA"),
            zlib_book(23, MD5_D, "Games and decisions", "Luce", removed=1,
                      removalReason="On your request nothing has been found"),
            annabookinfo(23, MD5_D, deleted="spam"),
            zlib_book(24, MD5_E, "Games People Play", "Berne, Eric"),
        ]
        with tempfile.TemporaryDirectory() as tmp:
            index, _ = self.build(tmp, records)
            candidates, attempt = pf.search_zlib_index(self.target(isbn="978-0-486-65943-5"), index)
        self.assertEqual(attempt["status"], "ok")
        self.assertEqual(attempt["snapshot"], "2026-07-06T19:31:43Z")
        self.assertEqual([(c["md5"], c["zlib_match"]) for c in candidates],
                         [(MD5_B, "isbn"), (MD5_A, "title-author"), (MD5_D, "title-author")])
        first = candidates[1]
        self.assertEqual((first["format"], first["size_bytes"], first["source"], first["zlibrary_id"]),
                         ("pdf", 1234567, "zlib", 20))
        self.assertEqual(candidates[2]["zlib_deleted"], "spam")
        self.assertNotIn("zlib_deleted", first)

    def test_search_folds_diacritics_and_finds_the_surname_in_the_title(self):
        records = [
            # Decomposed accent, as some Z-Library titles store it.
            zlib_book(30, MD5_A, "El he\u0301roe de las mujeres", "Bioy Casares, Adolfo", year="1978"),
            # A compiled volume credits its compiler; the subject's name is in the title.
            zlib_book(31, MD5_B, "ABC de Adolfo Bioy Casares : reflexiones", "Martino, Daniel, 1961-"),
            zlib_book(32, MD5_C, "Gentlemen : a novel", "Östergren, Klas, 1955-"),
        ]
        with tempfile.TemporaryDirectory() as tmp:
            index, _ = self.build(tmp, records)
            hero = pf.search_zlib_index(self.target(title="El héroe de las mujeres", author="Bioy Casares, Adolfo"), index)
            abc = pf.search_zlib_index(self.target(title="{ABC} de Adolfo Bioy Casares: reflexiones y observaciones",
                                                   author="Bioy Casares, Adolfo"), index)
            gentlemen = pf.search_zlib_index(self.target(title="Gentlemen", author="Ostergren, Klas and Nunnally, Tiina"),
                                             index)
        self.assertEqual([c["md5"] for c in hero[0]], [MD5_A])
        self.assertEqual([c["md5"] for c in abc[0]], [MD5_B])
        self.assertEqual([c["md5"] for c in gentlemen[0]], [MD5_C])

    def test_missing_index_is_a_recorded_attempt_not_a_crash(self):
        with tempfile.TemporaryDirectory() as tmp:
            candidates, attempt = pf.search_zlib_index(self.target(), Path(tmp) / "absent.sqlite")
        self.assertEqual(candidates, [])
        self.assertEqual(attempt["status"], "index-missing")
        target = {**BOOK_TARGET, "isbn": ""}
        http = FakeHttp([("libgen.li/index.php", html(libgen_search_page([]))),
                         ("/search", html("<title>DDoS-Guard</title>", 403))])
        with mock.patch.dict(os.environ, {pf.ZLIB_INDEX_ENV: "/nonexistent/index.sqlite"}):
            result = pf.discover_book_candidates(http, target, annas_host="annas-archive.gl")
        self.assertEqual([a["status"] for a in result["attempts"]], ["ok", "index-missing", "needs-browser"])
        self.assertEqual(result["status"], "needs-browser")
        self.assertFalse(result["search_complete"])

    def test_zlib_pdf_hit_feeds_review_and_avoids_the_browser_search(self):
        target = {**BOOK_TARGET, "isbn": "", "title": "Games and Decisions", "author": "Luce, R. Duncan"}
        http = FakeHttp([("libgen.li/index.php", html(libgen_search_page([]))),
                         ("/search", html("<title>DDoS-Guard</title>", 403))])
        with tempfile.TemporaryDirectory() as tmp:
            index, _ = self.build(tmp, [zlib_book(40, MD5_A, "Games and decisions", "Luce, R. Duncan")])
            with mock.patch.object(pf, "annas_secret_key", side_effect=AssertionError("credentials not permitted")):
                result = pf.discover_book_candidates(http, target, annas_host="annas-archive.gl", zlib_index=index)
            epub_index = Path(tmp) / "epub"
            epub_index.mkdir()
            epub_index, _ = self.build(epub_index, [
                zlib_book(41, MD5_B, "Games and decisions", "Luce, R. Duncan", extension="epub"),
                zlib_book(42, MD5_C, "Games and decisions", "Luce, R. Duncan"),
                annabookinfo(42, MD5_C, deleted="spam")])
            epub_only = pf.discover_book_candidates(http, target, annas_host="annas-archive.gl", zlib_index=epub_index)
        self.assertEqual(result["status"], "needs-review")
        self.assertNotIn("annas_browser_search", result)
        self.assertFalse(any("/search" in url for url, _ in http.calls[:2]))
        zlib_attempt, annas_attempt = result["attempts"][1:]
        self.assertEqual((zlib_attempt["route"], zlib_attempt["snapshot"]), ("zlib-index", "2026-07-06T19:31:43Z"))
        self.assertEqual(annas_attempt["status"], "skipped")
        self.assertIn("snippet", annas_attempt["fallback"], "the skipped browser step stays on record")
        self.assertFalse(result["search_complete"])
        candidate = result["candidates"][0]
        self.assertEqual((candidate["md5"], candidate["format"]), (MD5_A, "pdf"))
        self.assertEqual(candidate["observations"][0]["source"], "zlib")
        # The manifest is a normal inventory: the review path reads it and stages by md5.
        with tempfile.TemporaryDirectory() as tmp:
            manifest = Path(tmp) / "candidates.json"
            manifest.write_text(json.dumps(result))
            self.assertEqual(pf.read_book_candidates(manifest)["candidates"][0]["md5"], MD5_A)
        # EPUBs are not indexed, and a PDF Z-Library flagged as spam does not
        # replace Anna's search (it is still offered for review).
        self.assertEqual([c["md5"] for c in epub_only["candidates"]], [MD5_C])
        self.assertEqual(epub_only["status"], "needs-review")
        self.assertEqual(epub_only["attempts"][-1]["status"], "needs-browser")
        self.assertIn("annas_browser_search", epub_only)

    def test_failed_build_keeps_the_previous_index(self):
        with tempfile.TemporaryDirectory() as tmp:
            index, _ = self.build(tmp, [zlib_book(50, MD5_A, "Games and decisions", "Luce")])
            before = index.read_bytes()
            bad = Path(tmp) / "bad"
            bad.mkdir()
            dump = write_dump(bad, [zlib_book(51, MD5_B, "Other", "X")])
            dump.write_bytes(dump.read_bytes()[:-8])  # truncated download
            with self.assertRaises(zlib.ZlibError):
                zlib.build_index(dump, index)
            self.assertEqual(index.read_bytes(), before)
            self.assertEqual(sorted(p.name for p in Path(tmp).iterdir()), sorted([DUMP_NAME, "bad", "index.sqlite"]))
            with self.assertRaises(zlib.ZlibError):
                zlib.build_index(Path(tmp) / DUMP_NAME, Path.home() / "My Drive" / "index.sqlite")

    def test_refresh_picks_the_newest_seeded_snapshot(self):
        def entry(name, seeders, listed):
            return {"display_name": name + ".torrent", "btih": "0" * 40, "seeders": seeders,
                    "url": f"https://annas-archive.gl/dyn/small_file/torrents/{name}.torrent",
                    "added_to_torrents_list_at": listed, "data_size": 1}
        old = "annas_archive_meta__aacid__zlib3_records__20240809T171652Z--20260626T054841Z.jsonl.seekable.zst"
        seeded = DUMP_NAME
        newer = "annas_archive_meta__aacid__zlib3_records__20240809T171652Z--20260821T041731Z.jsonl.seekable.zst"
        newest = "annas_archive_meta__aacid__zlib3_records__20260913T213218Z--20260916T162622Z.jsonl.seekable.zst"
        entries = [entry(old, 3, "2026-06-26"), entry(newest, 0, "2026-09-16"), entry(seeded, 43, "2026-07-07"),
                   entry(newer, 0, "2026-08-22"), {"display_name": "annas_archive_data__aacid__other.torrent"}]
        chosen, skipped = zlib.choose_snapshot(entries)
        self.assertEqual(chosen["file_name"], seeded)
        self.assertEqual([item["file_name"] for item in skipped], [newest, newer])
        with self.assertRaises(zlib.ZlibError):
            zlib.choose_snapshot([entry(newest, 0, "2026-09-16")])
        # A dump listed twice must not break the ordering.
        self.assertEqual(zlib.choose_snapshot([entry(seeded, 1, "x"), entry(seeded, 2, "x")])[0]["file_name"], seeded)

    def test_refresh_never_steps_back_and_deletes_only_older_dumps(self):
        older = "annas_archive_meta__aacid__zlib3_records__20240809T171652Z--20260626T054841Z.jsonl.seekable.zst"
        newer = "annas_archive_meta__aacid__zlib3_records__20240809T171652Z--20260821T041731Z.jsonl.seekable.zst"
        torrents = [{"display_name": older + ".torrent", "btih": "0" * 40, "seeders": 5, "data_size": 1,
                     "url": "https://annas-archive.gl/x.torrent", "added_to_torrents_list_at": "2026-06-26"}]
        http = FakeHttp([("torrents.json", html(json.dumps(torrents)))])
        with tempfile.TemporaryDirectory() as tmp:
            self.build(tmp, [zlib_book(70, MD5_A, "Games and decisions", "Luce")])  # index on DUMP_NAME (July)
            with mock.patch.object(zlib, "transmission", side_effect=AssertionError("no download expected")):
                self.assertEqual(zlib.refresh(http, Path(tmp))["status"], "up-to-date")
            for name in (older, newer):
                (Path(tmp) / name).write_bytes(b"x")
            with mock.patch.object(zlib, "transmission", return_value={"torrents": []}):
                removed = zlib._remove_superseded(Path(tmp), Path(tmp) / DUMP_NAME, lambda _m: None)
            self.assertEqual(removed, [older])
            self.assertTrue((Path(tmp) / newer).exists() and (Path(tmp) / DUMP_NAME).exists())

    def test_refresh_rebuilds_an_index_with_an_old_schema(self):
        import sqlite3
        torrents = [{"display_name": DUMP_NAME + ".torrent", "btih": "0" * 40, "seeders": 5, "data_size": 1,
                     "url": "https://annas-archive.gl/x.torrent", "added_to_torrents_list_at": "2026-07-07"}]
        http = FakeHttp([("torrents.json", html(json.dumps(torrents)))])
        with tempfile.TemporaryDirectory() as tmp:
            index, _ = self.build(tmp, [zlib_book(80, MD5_A, "Games and decisions", "Luce")])
            connection = sqlite3.connect(index)
            connection.execute("UPDATE meta SET value = '0' WHERE key = 'schema'")
            connection.commit()
            connection.close()
            self.assertEqual(zlib.refresh(http, Path(tmp), dry_run=True)["status"], "dry-run")

    def test_query_keeps_indic_words_whole(self):
        query = pf.zlib_title_author_query({"title": "हिन्दी साहित्य का इतिहास", "author": "शुक्ल, रामचन्द्र"})
        self.assertEqual(query, 'title : ("हिन्दी" "साहित्य" "का" "इतिहास") AND {title author} : ("शुक्ल")')

    def test_refresh_is_a_no_op_when_the_index_already_has_the_chosen_snapshot(self):
        torrents = [{"display_name": DUMP_NAME + ".torrent", "btih": "0" * 40, "seeders": 5, "data_size": 1,
                     "url": "https://annas-archive.gl/x.torrent", "added_to_torrents_list_at": "2026-07-07"}]
        http = FakeHttp([("torrents.json", html(json.dumps(torrents)))])
        with tempfile.TemporaryDirectory() as tmp:
            self.build(tmp, [zlib_book(60, MD5_A, "Games and decisions", "Luce")])
            with mock.patch.object(zlib, "transmission", side_effect=AssertionError("no download expected")):
                result = zlib.refresh(http, Path(tmp))
        self.assertEqual(result["status"], "up-to-date")
        self.assertTrue(all("annas-archive" in url or "wikipedia" in url for url, _ in http.calls))


class CliTests(unittest.TestCase):
    def test_cli_loads_shared_library_and_maps_exit_codes(self):
        spec = importlib.util.spec_from_loader("paper_fetch_cli", loader=None)
        module = importlib.util.module_from_spec(spec)
        module.__file__ = str(CLI)
        code = CLI.read_text()
        exec(compile(code, "paper-fetch", "exec"), module.__dict__)
        self.assertIs(module.pf, sys.modules["paper_fetch"])
        self.assertEqual(module.EXIT_CODES["needs-browser"], 2)
        self.assertEqual(module.EXIT_CODES["not-member"], 3)


if __name__ == "__main__":
    unittest.main()
