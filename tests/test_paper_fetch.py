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

MD5_A = "a" * 32
MD5_B = "b" * 32


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
        self.assertEqual([a["status"] for a in result["attempts"]], ["ok", "ok", "needs-browser"])
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
        for payload in ({"error": "temporarily unavailable"}, {"1": {"title": "Book"}},
                        {"1": {"files": {"9": {}}}}):
            with self.subTest(payload=payload), self.assertRaises(pf.PaperFetchError):
                pf.libgen_isbn_files(FakeHttp([("libgen", html(json.dumps(payload)))]), "9780262033848", strict=True)
        http = FakeHttp([("libgen", lambda url, params: html(json.dumps(
            {} if params.get("object") == "f" else {"1": {"files": {"9": {"f_id": "9"}}}})))])
        with self.assertRaises(pf.PaperFetchError):
            pf.libgen_isbn_files(http, "9780262033848", strict=True)

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
