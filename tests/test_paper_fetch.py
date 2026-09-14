"""Regression tests for lib/python/paper_fetch.py (no network)."""

from __future__ import annotations

import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
LIB = ROOT / "lib" / "python" / "paper_fetch.py"
_SPEC = importlib.util.spec_from_file_location("paper_fetch", LIB)
pf = importlib.util.module_from_spec(_SPEC)
sys.modules["paper_fetch"] = pf
_SPEC.loader.exec_module(pf)

MD5_A = "a" * 32
MD5_B = "b" * 32


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
        module.__file__ = str(ROOT / "bin" / "paper-fetch")
        code = (ROOT / "bin" / "paper-fetch").read_text()
        exec(compile(code, "paper-fetch", "exec"), module.__dict__)
        self.assertIs(module.pf, sys.modules["paper_fetch"])
        self.assertEqual(module.EXIT_CODES["needs-browser"], 2)
        self.assertEqual(module.EXIT_CODES["not-member"], 3)


if __name__ == "__main__":
    unittest.main()
