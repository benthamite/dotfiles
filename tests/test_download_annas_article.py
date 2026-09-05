"""Downloader tests: provider/pass calls mocked, all writable paths disposable."""
import contextlib
import hashlib
import importlib.util
import io
import json
import os
import stat
import subprocess
import tempfile
import traceback
import types
import unittest
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path
from unittest import mock


REPO = Path(__file__).resolve().parents[1]
SCRIPT = REPO / "codex/skills/add-bib-entry/scripts/download_annas_article.py"
MIRROR = REPO / "claude/skills/add-bib-entry/scripts/download_annas_article.py"
SENTINEL = "fixture-only/not-a-real-account-value&other=0"
PDF = b"%PDF-1.7\nfixture payload\n%%EOF\n"
DIGEST = hashlib.md5(PDF).hexdigest()
DOI = "10.1234/example"
ENTRY = "@article{Example,\n  title = {A {nested} title},\n  doi = {10.1234/example}\n}\n"
DOWNLOAD_URL = "https://files.example.invalid/paper.pdf?signature=fixture-only"


class DownloaderTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="annas-downloader-tests-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        spec = importlib.util.spec_from_file_location("isolated_annas", SCRIPT)
        self.script = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.script)
        self.script.LOCK_ROOT = self.root / "locks"
        self.bib = self.root / "bibliography.bib"
        self.bib.write_text(ENTRY)
        self.library = self.root / "custom library"
        self.destination = self.library / "Example.pdf"
        self.real_fetch = self.script.fetch_bytes
        self.real_secret = self.script.get_secret_key
        self.secret = self.patch(self.script, "get_secret_key", return_value=SENTINEL)
        self.fetch = self.patch(self.script, "fetch_bytes", side_effect=AssertionError("unconfigured fixture request"))
        self.pass_process = self.patch(self.script.subprocess, "run", side_effect=AssertionError("real credential process forbidden"))
        self.patch(urllib.request.OpenerDirector, "open", side_effect=AssertionError("real network forbidden"))

    def patch(self, target, name, **kwargs):
        patcher = mock.patch.object(target, name, **kwargs)
        self.addCleanup(patcher.stop)
        return patcher.start()

    def provider(self, payload=PDF, md5=DIGEST, during_download=None):
        def response(url, **kwargs):
            parsed = urllib.parse.urlsplit(url)
            if parsed.netloc == "annas-archive.pk" and parsed.path.startswith("/scidb/"):
                return f'<a href="/md5/{md5}">file</a>'.encode()
            if parsed.netloc == "annas-archive.pk" and parsed.path == "/dyn/api/fast_download.json":
                self.assertEqual(urllib.parse.parse_qs(parsed.query)["key"], [SENTINEL])
                return json.dumps({"download_url": DOWNLOAD_URL}).encode()
            if url == DOWNLOAD_URL:
                if during_download:
                    during_download()
                return payload
            raise AssertionError("unexpected fixture request")
        self.fetch.side_effect = response

    def attach(self, **kwargs):
        arguments = dict(doi=DOI, key="Example", bibfile=self.bib,
                         base_url=self.script.BASE_URL, library_dir=self.library)
        arguments.update(kwargs)
        return self.script.attach_article(**arguments)

    def assert_preflight_refusal(self, **kwargs):
        original = self.bib.read_bytes()
        with self.assertRaises(self.script.AttachmentError):
            self.attach(**kwargs)
        self.fetch.assert_not_called()
        self.secret.assert_not_called()
        self.pass_process.assert_not_called()
        self.assertEqual(self.bib.read_bytes(), original)

    def assert_no_staging(self):
        self.assertEqual(list(self.root.rglob(".annas-attachment-*")), [])

    def test_paired_scripts_match(self):
        self.assertEqual(SCRIPT.read_bytes(), MIRROR.read_bytes())

    def test_unapproved_origins_refused_before_credentials_or_network(self):
        for origin in ("https://elsewhere.invalid/", "http://annas-archive.pk/",
                       "https://annas-archive.pk.evil.invalid/", "https://name@annas-archive.pk/",
                       "https://annas-archive.pk:8443/", "https://annas-archive.pk/?x=1",
                       "https://annas-archive.pk/other/"):
            with self.subTest(origin=origin):
                self.assert_preflight_refusal(base_url=origin)

    def test_redirect_handler_never_opens_credential_redirect_target(self):
        request = urllib.request.Request(self.script.BASE_URL + "dyn/api/fast_download.json?key=" + urllib.parse.quote(SENTINEL))
        for status_code in (301, 302, 303, 307, 308):
            handler = self.script.NoRedirect()
            handler.parent = mock.Mock()
            with self.subTest(code=status_code), self.assertRaises(self.script.AttachmentError) as raised:
                handler.http_error_302(request, io.BytesIO(), status_code, "redirect",
                                       {"location": "https://outside.invalid/?key=" + SENTINEL})
            handler.parent.open.assert_not_called()
            self.assertNotIn(SENTINEL, str(raised.exception))

    def test_fetch_installs_no_redirect_handler_and_redacts_transport_errors(self):
        url = self.script.BASE_URL + "dyn/api/fast_download.json?key=" + urllib.parse.quote(SENTINEL)
        opener = mock.Mock()
        error = urllib.error.HTTPError(url, 403, SENTINEL, {}, io.BytesIO())
        opener.open.side_effect = error
        with mock.patch.object(self.script.urllib.request, "build_opener", return_value=opener) as build:
            with self.assertRaises(self.script.AttachmentError) as raised:
                self.real_fetch(url)
        self.assertIsInstance(build.call_args.args[0], self.script.NoRedirect)
        rendered = "".join(traceback.format_exception(raised.exception))
        self.assertNotIn(SENTINEL, rendered)
        self.assertNotIn(url, rendered)
        self.assertTrue(error.closed)

    def test_api_errors_bad_schemas_and_reflected_keys_are_sanitized(self):
        cases = [{"error": SENTINEL + DOWNLOAD_URL}, [], "invalid", {},
                 {"download_url": "http://files.example.invalid/file"},
                 {"download_url": "https://files.example.invalid/?key=" + urllib.parse.quote(SENTINEL, safe="")},
                 {"download_url": "https://user:password@files.example.invalid/file"}]
        for data in cases:
            self.fetch.side_effect = None
            self.fetch.return_value = json.dumps(data).encode()
            with self.subTest(data=data), self.assertRaises(self.script.AttachmentError) as raised:
                self.script.fast_download_url(DIGEST, SENTINEL, self.script.BASE_URL)
            self.assertNotIn(SENTINEL, str(raised.exception))
            self.assertNotIn(DOWNLOAD_URL, str(raised.exception))

    def test_pass_failure_and_empty_output_do_not_expose_details(self):
        cases = [subprocess.CalledProcessError(1, ["fixture"], output=SENTINEL, stderr=SENTINEL),
                 subprocess.TimeoutExpired(["fixture"], 30, output=SENTINEL),
                 types.SimpleNamespace(stdout=""), types.SimpleNamespace(stdout="\n")]
        for result in cases:
            self.pass_process.side_effect = result if isinstance(result, BaseException) else None
            self.pass_process.return_value = result
            with self.subTest(result=type(result).__name__), self.assertRaises(self.script.AttachmentError) as raised:
                self.real_secret()
            self.assertNotIn(SENTINEL, "".join(traceback.format_exception(raised.exception)))
        self.assertEqual(self.pass_process.call_args.args[0], ["pass", "show", "tlon/core/annas-archive"])
        self.assertTrue(self.pass_process.call_args.kwargs["capture_output"])

    def test_doi_normalization_and_path_quoting(self):
        for value in (DOI, " DOI: 10.1234/EXAMPLE ", "https://doi.org/10.1234/EXAMPLE",
                      "http://dx.doi.org/10.1234%2FEXAMPLE"):
            self.assertEqual(self.script.normalize_doi(value), DOI)
        self.fetch.side_effect = None
        self.fetch.return_value = f'<a href="/md5/{DIGEST}">file</a>'.encode()
        special = "10.1234/a?x=1#fragment"
        self.script.find_md5_for_doi(special, self.script.BASE_URL)
        parsed = urllib.parse.urlsplit(self.fetch.call_args.args[0])
        self.assertEqual(parsed.query, "")
        self.assertEqual(parsed.fragment, "")
        self.assertEqual(parsed.path, "/scidb/" + urllib.parse.quote(special, safe=""))
        for invalid in ("not a DOI", "https://elsewhere.invalid/10.1234/example",
                        "https://doi.org/10.1234/example?another=work"):
            with self.assertRaises(self.script.AttachmentError):
                self.script.normalize_doi(invalid)

    def test_md5_candidates_are_unique_exact_same_origin_links(self):
        for html in (f'<a href="/md5/{DIGEST}">one</a><a href="/md5/{"a" * 32}">two</a>',
                     f'<script>"/md5/{DIGEST}"</script>',
                     f'<a href="https://elsewhere.invalid/md5/{DIGEST}">external</a>',
                     f'<a href="/md5/{DIGEST}extra">longer</a>'):
            self.fetch.side_effect = None
            self.fetch.return_value = html.encode()
            with self.subTest(html=html), self.assertRaises(self.script.AttachmentError):
                self.script.download_content(DOI, self.script.BASE_URL)
            self.secret.assert_not_called()
        self.fetch.return_value = (f'<a href="/md5/{DIGEST}">one</a>'
                                   f'<a href="{self.script.BASE_URL}md5/{DIGEST.upper()}/">same</a>').encode()
        self.assertEqual(self.script.find_md5_for_doi(DOI, self.script.BASE_URL), DIGEST)

    def test_missing_duplicate_or_wrong_doi_entries_fail_before_download(self):
        for entry in (ENTRY.replace("Example,", "Another,"), ENTRY + ENTRY,
                      ENTRY.replace(DOI, "10.1234/different"), "@article{Example, title={No DOI}}"):
            with self.subTest(entry=entry):
                self.bib.write_text(entry)
                self.assert_preflight_refusal()

    def test_existing_file_associations_are_never_silently_skipped(self):
        for field in ("file={/stale/path.pdf}", 'FILE = "quoted.pdf"', "file = {}"):
            with self.subTest(field=field):
                self.bib.write_text(f"@article{{Example, doi={{{DOI}}}, {field}}}")
                self.assert_preflight_refusal()

    def test_unsupported_bibliography_syntax_fails_before_download(self):
        for entry in ("@string{journal = {Title}}\n" + ENTRY,
                      f"@article(Example, doi={{{DOI}}})", f"@article{{Example, doi={{{DOI}}}, title=macro}}",
                      f"@article{{Example, doi={{{DOI}}}, title={{A}} # {{B}}}}",
                      f"@article{{Example, doi={{{DOI}}}, DOI={{{DOI}}}}}",
                      f"@article{{Example, doi={{{DOI}}}, title={{unbalanced}}",
                      ENTRY.replace("title =", "title :")):
            with self.subTest(entry=entry):
                self.bib.write_text(entry)
                self.assert_preflight_refusal()

    def test_nested_multiline_values_quotes_and_comments_preserve_other_entries(self):
        target = ('% @article{Example, fake={commented entry}}\n'
                  '@article{Example,\n title = {Outer\n{inner}\n}\n,\n'
                  ' note = "quoted {brace} and \\"literal\\"",\n'
                  f' doi = {{{DOI}}} % retain this comment\n}}\n')
        other = '\n@book{Other, title = {Leave every byte here}, year=2001}\n'
        self.bib.write_text(target + other)
        before = self.script.parse_entries(target + other)[0]["fields"]
        self.provider()
        self.attach()
        changed = self.bib.read_text()
        after = self.script.parse_entries(changed)[0]["fields"]
        self.assertTrue(changed.endswith(other))
        self.assertTrue(changed.startswith(target[:target.index(" doi")]))
        self.assertIn("% retain this comment", changed)
        for name, value in before.items():
            self.assertEqual(after[name], value)
        self.assertEqual(after["file"], str(self.destination))

    def test_unsafe_keys_and_attachment_delimiters_fail_before_download(self):
        for key in ("../escape", "/absolute", "a/b", "a\\b", ".", "..", "Key\nmore", ""):
            with self.subTest(key=key):
                self.assert_preflight_refusal(key=key)
        for directory in ("bad;second-path", "bad{brace}", "bad\\slash", "bad\nline"):
            with self.subTest(directory=directory):
                self.assert_preflight_refusal(library_dir=self.root / directory)

    def test_existing_pdf_and_symlink_destinations_are_untouched(self):
        self.library.mkdir()
        self.destination.write_bytes(b"existing user PDF")
        self.assert_preflight_refusal()
        self.assertEqual(self.destination.read_bytes(), b"existing user PDF")
        self.destination.unlink()
        target = self.root / "user-file.pdf"
        target.write_bytes(b"unrelated target")
        self.destination.symlink_to(target)
        self.assert_preflight_refusal()
        self.assertTrue(self.destination.is_symlink())
        self.assertEqual(target.read_bytes(), b"unrelated target")

    def test_empty_nonpdf_truncated_and_wrong_checksum_payloads_do_not_publish(self):
        for payload, md5 in [(b"", hashlib.md5(b"").hexdigest()),
                             (b"<html>error</html>", hashlib.md5(b"<html>error</html>").hexdigest()),
                             (b'{"error":1}', hashlib.md5(b'{"error":1}').hexdigest()),
                             (b"%PDF-1.7\ntruncated", hashlib.md5(b"%PDF-1.7\ntruncated").hexdigest()),
                             (PDF, "a" * 32)]:
            self.provider(payload, md5)
            with self.subTest(payload=payload), self.assertRaises(self.script.AttachmentError):
                self.attach()
            self.assertEqual(self.bib.read_text(), ENTRY)
            self.assertFalse(self.destination.exists())
            self.assert_no_staging()

    def test_valid_pdf_and_custom_outside_home_directory_are_associated(self):
        self.provider()
        result = self.attach(doi="https://doi.org/10.1234/EXAMPLE", base_url=self.script.BASE_URL.rstrip("/"))
        self.assertEqual(self.destination.read_bytes(), PDF)
        fields = self.script.parse_entries(self.bib.read_text())[0]["fields"]
        self.assertEqual(fields["file"], str(self.destination))
        self.assertEqual(result, {"key": "Example", "file": str(self.destination), "md5": DIGEST, "bib_updated": True})
        self.assertEqual(self.fetch.call_count, 3)
        self.secret.assert_called_once()
        self.pass_process.assert_not_called()
        self.assert_no_staging()

    def test_bibliography_symlink_and_file_mode_are_preserved(self):
        linked = self.root / "linked.bib"
        linked.symlink_to(self.bib.name)
        self.bib.chmod(0o640)
        self.provider()
        self.attach(bibfile=linked)
        self.assertTrue(linked.is_symlink())
        self.assertEqual(stat.S_IMODE(self.bib.stat().st_mode), 0o640)
        self.assertIn("file =", self.bib.read_text())

    def test_nonregular_bibliography_is_rejected_without_reading_or_download(self):
        pipe = self.root / "pipe.bib"
        os.mkfifo(pipe)
        with self.assertRaisesRegex(self.script.AttachmentError, "regular file"):
            self.attach(bibfile=pipe)
        self.fetch.assert_not_called()
        self.secret.assert_not_called()

    def test_library_symlink_is_canonicalized_and_retarget_during_download_refused(self):
        actual = self.root / "actual library"
        actual.mkdir()
        self.library.symlink_to(actual, target_is_directory=True)
        unrelated = self.root / "unrelated library"
        unrelated.mkdir()

        def retarget():
            self.library.unlink()
            self.library.symlink_to(unrelated, target_is_directory=True)

        self.provider(during_download=retarget)
        with self.assertRaisesRegex(self.script.AttachmentError, "library directory changed"):
            self.attach()
        self.assertEqual(list(actual.iterdir()), [])
        self.assertEqual(list(unrelated.iterdir()), [])
        self.assertEqual(self.bib.read_text(), ENTRY)

    def test_concurrent_bibliography_write_or_replacement_is_preserved(self):
        changed = ENTRY + "\n% concurrent edit\n"
        for replacement in (False, True):
            self.bib.write_text(ENTRY)

            def edit():
                if replacement:
                    staged = self.root / "newer.bib"
                    staged.write_text(changed)
                    staged.replace(self.bib)
                else:
                    self.bib.write_text(changed)

            self.provider(during_download=edit)
            with self.subTest(replacement=replacement), self.assertRaisesRegex(self.script.AttachmentError, "bibliography changed"):
                self.attach()
            self.assertEqual(self.bib.read_text(), changed)
            self.assertFalse(self.destination.exists())
            self.assert_no_staging()

    def test_later_contender_wins_without_lost_update_or_pdf_deletion(self):
        nested = False

        def contend(doi, base):
            nonlocal nested
            if not nested:
                nested = True
                self.attach()
            return PDF, DIGEST

        with mock.patch.object(self.script, "download_content", side_effect=contend):
            with self.assertRaisesRegex(self.script.AttachmentError, "bibliography changed"):
                self.attach()
        self.assertEqual(self.destination.read_bytes(), PDF)
        self.assertEqual(self.bib.read_text().count("file ="), 1)
        self.assert_no_staging()

    def test_target_created_during_download_is_never_overwritten(self):
        def create_target():
            self.library.mkdir()
            self.destination.write_bytes(b"another download won")

        self.provider(during_download=create_target)
        with self.assertRaises(FileExistsError):
            self.attach()
        self.assertEqual(self.destination.read_bytes(), b"another download won")
        self.assertEqual(self.bib.read_text(), ENTRY)
        self.assert_no_staging()

    def test_fsync_link_and_bibliography_replace_failures_roll_back_owned_files(self):
        self.provider()
        for name in ("fsync", "link", "replace"):
            with self.subTest(name=name), mock.patch.object(self.script.os, name, side_effect=OSError("fixture write failure")):
                with self.assertRaises(OSError):
                    self.attach()
            self.assertEqual(self.bib.read_text(), ENTRY)
            self.assertFalse(self.destination.exists())
            self.assert_no_staging()

    def test_bibliography_staging_failure_cleans_staged_pdf(self):
        self.provider()
        real_stage = self.script.stage_bytes

        def stage(path, content, mode=0o600):
            if path == self.bib:
                raise OSError("fixture bibliography staging failure")
            return real_stage(path, content, mode)

        with mock.patch.object(self.script, "stage_bytes", side_effect=stage):
            with self.assertRaises(OSError):
                self.attach()
        self.assertFalse(self.destination.exists())
        self.assertEqual(self.bib.read_text(), ENTRY)
        self.assert_no_staging()

    def test_replace_performs_then_raises_never_deletes_associated_pdf(self):
        real_replace = self.script.os.replace
        for failure in (OSError("fixture late failure"), KeyboardInterrupt()):
            self.bib.write_text(ENTRY)
            if self.destination.exists():
                self.destination.unlink()
            self.provider()

            def replace(source, destination):
                real_replace(source, destination)
                raise failure

            with self.subTest(failure=type(failure).__name__), mock.patch.object(self.script.os, "replace", side_effect=replace):
                with self.assertRaisesRegex(self.script.AttachmentError, "outcome uncertain"):
                    self.attach()
            self.assertEqual(self.destination.read_bytes(), PDF)
            self.assertIn("file =", self.bib.read_text())
            self.assert_no_staging()

    def test_link_performs_then_raises_rolls_back_only_its_own_pdf(self):
        real_link = self.script.os.link
        self.provider()
        for failure in (OSError("fixture late link failure"), KeyboardInterrupt()):
            def link(source, destination):
                real_link(source, destination)
                raise failure

            with self.subTest(failure=type(failure).__name__), mock.patch.object(self.script.os, "link", side_effect=link):
                with self.assertRaises(type(failure)):
                    self.attach()
            self.assertFalse(self.destination.exists())
            self.assertEqual(self.bib.read_text(), ENTRY)
            self.assert_no_staging()

    def test_failed_link_does_not_remove_a_concurrent_replacement(self):
        real_link = self.script.os.link
        self.provider()

        def link(source, destination):
            real_link(source, destination)
            self.destination.unlink()
            self.destination.write_bytes(b"replacement after link")
            raise OSError("fixture late link failure")

        with mock.patch.object(self.script.os, "link", side_effect=link):
            with self.assertRaises(OSError):
                self.attach()
        self.assertEqual(self.destination.read_bytes(), b"replacement after link")
        self.assertEqual(self.bib.read_text(), ENTRY)
        self.assert_no_staging()

    def test_failed_link_preserves_same_inode_edit_even_with_restored_mtime(self):
        real_link = self.script.os.link
        self.provider()
        edited = b"x" * len(PDF)

        def link(source, destination):
            original = Path(source).stat()
            real_link(source, destination)
            self.destination.write_bytes(edited)
            os.utime(self.destination, ns=(original.st_atime_ns, original.st_mtime_ns))
            raise OSError("fixture late link failure")

        with mock.patch.object(self.script.os, "link", side_effect=link):
            with self.assertRaises(OSError):
                self.attach()
        self.assertEqual(self.destination.read_bytes(), edited)
        self.assertEqual(self.bib.read_text(), ENTRY)
        self.assert_no_staging()

    def test_rollback_preserves_unrelated_replacement_at_pdf_path(self):
        self.provider()

        def replace(source, destination):
            self.destination.unlink()
            self.destination.write_bytes(b"unrelated replacement")
            raise OSError("fixture replacement failure")

        with mock.patch.object(self.script.os, "replace", side_effect=replace):
            with self.assertRaises(OSError):
                self.attach()
        self.assertEqual(self.destination.read_bytes(), b"unrelated replacement")
        self.assertEqual(self.bib.read_text(), ENTRY)
        self.assert_no_staging()

    def test_cli_redacts_unknown_external_errors_and_reports_success_only_after_publication(self):
        args = [DOI, "Example", str(self.bib), "--library-dir", str(self.library)]
        stdout, stderr = io.StringIO(), io.StringIO()
        with mock.patch.object(self.script, "download_content", side_effect=RuntimeError(SENTINEL + DOWNLOAD_URL)):
            with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
                result = self.script.main(args)
        self.assertEqual(result, 1)
        self.assertEqual(stdout.getvalue(), "")
        self.assertNotIn(SENTINEL, stderr.getvalue())
        self.assertNotIn(DOWNLOAD_URL, stderr.getvalue())
        self.assertFalse(self.destination.exists())
        self.provider()
        with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
            result = self.script.main(args)
        self.assertEqual(result, 0)
        self.assertTrue(json.loads(stdout.getvalue())["bib_updated"])
        self.assertEqual(self.destination.read_bytes(), PDF)
        self.assertIn("file =", self.bib.read_text())


if __name__ == "__main__":
    unittest.main()
