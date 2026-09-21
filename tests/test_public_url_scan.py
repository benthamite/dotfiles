"""Public routing classification must not classify request payloads as URLs."""
import importlib.util
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "claude/hooks/lib-public-url-scan.py"
spec = importlib.util.spec_from_file_location("public_url_scan", SOURCE)
scan = importlib.util.module_from_spec(spec)
spec.loader.exec_module(scan)


class PublicURLScanTests(unittest.TestCase):
    URL = "https://myweb.sabanciuniv.edu/ozgurkibris/files/2008/10/kibris-sertel-scw06.pdf"
    TOKEN = "Synthetic9Opaque_" * 3

    def test_output_file_role_with_quoted_query_punctuation(self):
        url = ('https://books.google.com/books?id=P9ViAAAAMAAJ'
               '&jscmd=SearchWithinVolume2&q=impression')
        destination = ('/Users/pablostafforini/.local/state/bibliography-cleanup/'
                       'research-woolf-steinbeck-20260921/woolf-1959-impression.json')
        for command in (
            f"curl --fail --location --max-time 30 --silent --show-error '{url}' -o '{destination}'",
            f"curl --output='{destination}' '{url}'",
            f"wget --output-document '{destination}' '{url}'",
        ):
            with self.subTest(command=command):
                self.assertIsNone(scan.finding(command))
        for command in (
            f"curl '{url}' -H 'X-Token: {destination}'",
            f"curl '{url}' -d '{destination}'",
            f"curl '{url}&token={self.TOKEN}' -o '{destination}'",
            f"curl '{url}' -H 'X-Token: {self.TOKEN}' -o '{destination}'",
            f"curl '{url}' -d '{self.TOKEN}' -o '{destination}'",
            f"curl '{url}' -d '-o' '{destination}'",
            f"curl --unknown '{url}' -o '{destination}'",
            f"env curl '{url}' -o '{destination}'",
        ):
            with self.subTest(command=command):
                self.assertIsNotNone(scan.finding(command))

    def test_bsb_page_resource_routes_preserve_payloads(self):
        root = 'https://api.digitale-sammlungen.de'
        image_url = root + '/iiif/image/v2/bsb11252077_00025/full/full/0/default.jpg'
        ocr_url = root + '/ocr/bsb11252077/25'
        for url in (image_url, image_url.replace('_00025/', '_00026/'),
                    ocr_url, root + '/ocr/bsb00000001/00010'):
            self.assertIsNone(scan.finding(f"curl '{url}'"))
        for url in (image_url,):
            for changed in (
                url.replace('https://', 'http://'),
                url.replace('api.digitale-sammlungen.de', 'api.digitale-sammlungen.de.example.org'),
                url.replace('bsb11252077', 'bsb11252077' + self.TOKEN),
                url + 'extra', url + '/', url + '/' + self.TOKEN,
                url + '?token=' + self.TOKEN, url + '#' + self.TOKEN,
            ):
                with self.subTest(url=changed):
                    self.assertIsNotNone(scan.finding(f"curl '{changed}'"))
            for option in ('-H', '-d'):
                self.assertIsNotNone(scan.finding(f"curl '{url}' {option} '{self.TOKEN}'"))
                self.assertIsNotNone(scan.finding(f"curl {option} '{url}' https://example.org"))
        for changed in (
            image_url.replace('_00025/', '_000250/'),
            image_url.replace('/full/full/', '/private/full/'),
            image_url.replace('/default.jpg', '/other.jpg'),
            ocr_url.replace('/25', '/' + self.TOKEN),
        ):
            self.assertIsNotNone(scan.finding(f"curl '{changed}'"))

    def test_archive_metadata_prefix_retains_identifier_and_payloads(self):
        # https://archive.org/developers/md-read.html documents the fixed
        # route and partial reads. Identifiers themselves are never exempt.
        base = 'https://archive.org/metadata/'
        for suffix in ('isbn_9780312108298', 'in.ernet.dli.2015.101543',
                       'xfetch/files/0?start=1&count=5'):
            self.assertIsNone(scan.finding(f"curl '{base}{suffix}'"))
        url = base + 'isbn_9780312108298'
        for changed in (
            url.replace('https://', 'http://'),
            url.replace('archive.org', 'archive.org.example.org'),
            url.replace('archive.org', 'archive.org@example.org'),
            url.replace('/metadata/', '/private/'),
            url.replace('/metadata/', '/metadata-lookalike/'),
            base + self.TOKEN, base + 'isbn_' + self.TOKEN,
            url + '/' + self.TOKEN, url + '?token=' + self.TOKEN,
            url + '#' + self.TOKEN,
            url + '?token=' + ''.join(f'%{ord(c):02x}' for c in self.TOKEN),
            'https://example.org/?next=' + url,
        ):
            with self.subTest(url=changed):
                self.assertIsNotNone(scan.finding(f"curl '{changed}'"))
        for option in ('-H', '-d'):
            self.assertIsNotNone(scan.finding(f"curl '{url}' {option} '{self.TOKEN}'"))
            self.assertIsNotNone(scan.finding(f"curl {option} '{url}' https://example.org"))

    def test_bsb_manifest_identifiers_and_retained_payloads(self):
        # https://www.digitale-sammlungen.de/en/interfaces documents this
        # endpoint and both the basic and suffixed object identifier forms.
        base = 'https://api.digitale-sammlungen.de/iiif/presentation/v2/'
        for identifier in ('bsb11252077', 'bsb00012345', 'bsb00130380_00157_u001'):
            self.assertIsNone(scan.finding(f"curl '{base}{identifier}/manifest'"))
        url = base + 'bsb11252077/manifest'
        for changed in (
            url.replace('https://', 'http://'),
            url.replace('api.digitale-sammlungen.de', 'api.digitale-sammlungen.de.example.org'),
            url.replace('api.digitale-sammlungen.de', 'api.digitale-sammlungen.de@example.org'),
            url.replace('/presentation/', '/private/'),
            url.replace('/v2/', '/v3/'),
            url.replace('bsb11252077', 'bsb1125207'),
            url.replace('bsb11252077', 'bsb11252077_' + self.TOKEN),
            url.replace('/manifest', '/other'),
            url + '/', url + 'extra', url + '/' + self.TOKEN,
            url + '?token=' + self.TOKEN, url + '#' + self.TOKEN,
            url + '?token=' + ''.join(f'%{ord(c):02x}' for c in self.TOKEN),
            'https://example.org/?next=' + url,
        ):
            with self.subTest(url=changed):
                self.assertIsNotNone(scan.finding(f"curl '{changed}'"))
        for option in ('-H', '-d'):
            self.assertIsNotNone(scan.finding(f"curl '{url}' {option} '{self.TOKEN}'"))
            self.assertIsNotNone(scan.finding(f"curl {option} '{url}' https://example.org"))

    def test_kb_cahiers_requires_exact_page_and_preserves_payloads(self):
        url = ('https://collecties.kb.nl/en/collections/'
               'collection-anny-antoine-louis-koopman/1951-1960/cahiers')
        self.assertIsNone(scan.finding(f"curl -L --fail --max-time 30 -s '{url}' -o /tmp/kb-cahiers.html"))
        self.assertIsNone(scan.finding(f"wget -q '{url}'"))
        for changed in (
            url.replace('https://', 'http://'),
            url.replace('collecties.kb.nl', 'collecties.kb.nl.example.org'),
            url.replace('collecties.kb.nl', 'collecties.kb.nl@example.org'),
            url.replace('/collections/', '/private/'),
            url.replace('/1951-1960/', '/1961-1975/'),
            url.replace('/cahiers', '/other-title'),
            url + 'extra', url + '/', url + '/' + self.TOKEN,
            url + '?token=' + self.TOKEN, url + '#' + self.TOKEN,
            url + '?token=' + ''.join(f'%{ord(c):02x}' for c in self.TOKEN),
            'https://example.org/?next=' + url,
        ):
            with self.subTest(url=changed):
                self.assertIsNotNone(scan.finding(f"curl '{changed}'"))
        for option in ('-H', '-d'):
            self.assertIsNotNone(scan.finding(f"curl '{url}' {option} '{self.TOKEN}'"))
            self.assertIsNotNone(scan.finding(f"curl {option} '{url}' https://example.org"))

    def test_paired_source(self):
        self.assertEqual(SOURCE.read_bytes(),
                         (ROOT / "codex/hooks/lib-public-url-scan.py").read_bytes())

    def test_literal_url_operands(self):
        for command in (f"curl -fL '{self.URL}' -o /tmp/paper.pdf",
                        f"curl --url='{self.URL}'", f"wget -q '{self.URL}'"):
            with self.subTest(command=command):
                self.assertIsNone(scan.finding(command))

    def test_url_in_header_body_or_option_value_is_not_exempt(self):
        for option in ("-H", "--header", "-d", "--data", "--referer", "--output"):
            with self.subTest(option=option):
                self.assertIsNotNone(scan.finding(f"curl {option} '{self.URL}' https://example.org"))

    def test_retained_fields_and_sanitized_labels(self):
        for suffix, field in (("?token=", "query"), ("#", "fragment"), ("/", "path")):
            for token in (self.TOKEN, ''.join(f'%{ord(c):02x}' for c in self.TOKEN)):
                result = scan.finding(f"curl '{self.URL}{suffix}{token}'")
                self.assertIn(field, result)
                self.assertNotIn(self.TOKEN, result)
                self.assertNotIn(token, result)
        for option, field in (("-H", "header"), ("-d", "body")):
            self.assertIn(field, scan.finding(f"curl {option} '{self.TOKEN}' '{self.URL}'"))

    def test_url_reconstruction_does_not_add_a_slash(self):
        # The host suffix plus path is exactly 29 characters, below the rule.
        url = "https://example.org/" + "a" * 24 + "9"
        self.assertFalse(scan.opaque(url))
        self.assertIsNone(scan.finding(f"curl '{url}'"))

    def test_split_path_credential_remains_detected(self):
        token = '/'.join(["Opaque9Chunk"] * 4)
        self.assertIsNotNone(scan.finding(f"curl 'https://example.org/{token}'"))

    def test_authority_and_schema_boundaries(self):
        for url in (self.URL.replace("sabanciuniv.edu", "sabanciuniv.edu.example.org"),
                    self.URL.replace("sabanciuniv.edu", "sabanciuniv.edu@example.org"),
                    self.URL.replace("myweb.", self.TOKEN + "@myweb."),
                    self.URL.replace("/files/", "/private/"),
                    self.URL.replace("/10/", "/99/"),
                    self.URL.replace("/ozgurkibris/", "/otherauthor/"),
                    'https://example.org/?next=' + self.URL):
            with self.subTest(url=url):
                self.assertIsNotNone(scan.finding(f"curl '{url}'"))

    def test_unknown_commands_receive_no_exemption(self):
        for command in (f"curl --unknown '{self.URL}'", f"env curl '{self.URL}'",
                        f"curl '{self.URL}' > /tmp/output",
                        f"curl '{self.URL}' $EXTRA", f"curl '{self.URL}"):
            with self.subTest(command=command):
                self.assertIsNotNone(scan.finding(command))

    def test_literal_pipeline_and_quoted_payload_boundaries(self):
        self.assertIsNone(scan.finding(f"sed -n '1,2p' source.py; curl '{self.URL}' | head -c 300"))
        self.assertIsNone(scan.finding(f"wget -qO- '{self.URL}'"))
        self.assertIsNotNone(scan.finding(f"curl -d '; curl {self.URL}' https://example.org"))
        self.assertIsNotNone(scan.finding(f"curl --request ';' -d '{self.TOKEN}' '{self.URL}'"))

    def test_full_identifier_route_retains_extra_bytes(self):
        url = "https://digital.library.adelaide.edu.au/bitstreams/403aefae-ade4-4d86-98f0-32b0d6b0e62d/download"
        self.assertIsNone(scan.finding(f"curl '{url}'"))
        for tail in ("extra", "/", "?token=" + self.TOKEN):
            self.assertIsNotNone(scan.finding(f"curl '{url}{tail}'"))

    def test_adelaide_rest_bitstream_content(self):
        url = "https://digital.library.adelaide.edu.au/server/api/core/bitstreams/403aefae-ade4-4d86-98f0-32b0d6b0e62d/content"
        self.assertIsNone(scan.finding(f"curl --fail --location --max-time 60 '{url}' --output /tmp/paper.pdf"))
        for changed in (url.replace('/content', '/download'), url + 'extra',
                        url.replace('adelaide.edu.au', 'adelaide.edu.au.example.org'),
                        url.replace('adelaide.edu.au', 'adelaide.edu.au@example.org'),
                        url + '?token=' + self.TOKEN, url + '#' + self.TOKEN):
            self.assertIsNotNone(scan.finding(f"curl '{changed}'"))

    def test_zora_public_eprint_file_keeps_filename_and_payload(self):
        url = "https://www.zora.uzh.ch/id/eprint/174318/1/Riedener_constructivism.pdf"
        self.assertIsNone(scan.finding(f"curl --fail --location --max-time 60 '{url}' --output /tmp/paper.pdf"))
        for changed in (url.replace('/eprint/', '/private/'),
                        url.replace('zora.uzh.ch', 'zora.uzh.ch.example.org'),
                        url.replace('zora.uzh.ch', 'zora.uzh.ch@example.org'),
                        url.rsplit('/', 1)[0] + '/' + self.TOKEN + '.pdf',
                        url + '/' + self.TOKEN, url + '?token=' + self.TOKEN,
                        url + '#' + self.TOKEN):
            self.assertIsNotNone(scan.finding(f"curl '{changed}'"))

    def test_ergo_public_galley_download(self):
        url = "https://journals.publishing.umich.edu/ergo/article/7303/galley/4678/download/"
        self.assertIsNone(scan.finding(f"curl -L --fail --max-time 45 '{url}' --output /tmp/paper.pdf"))
        for changed in (url.replace('/galley/', '/private/'), url + self.TOKEN,
                        url.replace('umich.edu', 'umich.edu.example.org'),
                        url.replace('umich.edu', 'umich.edu@example.org'),
                        url + '?token=' + self.TOKEN, url + '#' + self.TOKEN):
            self.assertIsNotNone(scan.finding(f"curl '{changed}'"))

    def test_brown_review_and_jagiellonian_metadata_routes(self):
        urls = (
            "https://www.brown.edu/Departments/Philosophy/bears/0301ridg.html",
            "https://ruj.uj.edu.pl/entities/publication/c32ca7c6-ecdc-49e1-ba7d-e6c10816b5f6",
        )
        for url in urls:
            with self.subTest(url=url):
                self.assertIsNone(scan.finding(f"curl '{url}'"))
                host = url.split('/')[2]
                for changed in (
                    url.replace(host, host + '.example.org'),
                    url.replace(host, host + '@example.org'),
                    url.replace('https://' + host + '/', 'https://' + host + '/private/'),
                    url.rsplit('/', 1)[0] + '/' + self.TOKEN + '.html',
                    url + '?token=' + self.TOKEN,
                    url + '#' + self.TOKEN,
                    url + '/' + self.TOKEN,
                ):
                    self.assertIsNotNone(scan.finding(f"curl '{changed}'"))
                for option in ('-H', '-d'):
                    self.assertIsNotNone(scan.finding(f"curl '{url}' {option} '{self.TOKEN}'"))
        self.assertIsNotNone(scan.finding(f"curl '{urls[0].replace('/bears/', '/private/')}'"))
        self.assertIsNotNone(scan.finding(f"curl '{urls[1].replace('/publication/', '/account/')}'"))

    def test_bibliography_public_routes_keep_payloads(self):
        urls = (
            "https://www.bobbeddor.com/uploads/3/2/0/3/32037343/fallibility_for_expressivists_final.pdf",
            "https://discovery.ucl.ac.uk/id/eprint/10086797/9/Knox_10086797_Thesis.pdf",
            "https://www.jacobbarrett.org/uploads/1/2/3/6/123631127/barrett_and_schmidt_moral_uncertainty_and_public_justification.pdf",
            "https://eprints.lse.ac.uk/110362/1/Makins_attitudinal_ambivalence_published.pdf",
            "https://jesp.org/index.php/jesp/article/download/1117/433",
            "https://www.frontiersin.org/journals/artificial-intelligence/articles/10.3389/frai.2026.1754973/pdf",
            "https://80000hours.org/wp-content/uploads/2017/06/MacAskill-Normative-Uncertainty.pdf",
            "https://www.cambridge.org/core/services/aop-cambridge-core/content/view/9DAA9A1E7577A374A1C31FFD9740DCC4/S0045509124000341a.pdf/supererogation_suberogation_and_maximizing_expected_choiceworthiness.pdf",
            'https://blogs.kent.ac.uk/futureofnormativity/files/2018/05/Risberg-ethics-and-the-question-of-what-to-do.pdf',
            'https://www.jesp.org/pdf/36346f73-90e0-8193-bf02-c929ee3765e5',
        )
        for url in urls:
            with self.subTest(url=url):
                self.assertIsNone(scan.finding(
                    f"curl --fail --location --max-time 45 '{url}' --output /tmp/paper.pdf"))
                host = url.split('/')[2]
                for changed in (
                    url.replace(host, host + '.example.org'),
                    url.replace(host, host + '@example.org'),
                    url.replace(host, self.TOKEN + '@' + host),
                    url.replace('https://' + host + '/', 'https://' + host + '/private/'),
                    url.rsplit('/', 1)[0] + '/' + self.TOKEN + '.pdf',
                    url + '?token=' + self.TOKEN, url + '#' + self.TOKEN,
                    url + '?token=' + ''.join(f'%{ord(c):02x}' for c in self.TOKEN),
                ):
                    self.assertIsNotNone(scan.finding(f"curl '{changed}'"))
                for option in ('-H', '-d'):
                    self.assertIsNotNone(scan.finding(f"curl '{url}' {option} '{self.TOKEN}'"))
        self.assertIsNotNone(scan.finding(f"curl '{urls[-2].replace('/futureofnormativity/', '/otherproject/')}'"))
        self.assertIsNotNone(scan.finding(f"curl '{urls[-1]}/{self.TOKEN}'"))
        self.assertIsNotNone(scan.finding(f"curl '{urls[0].replace('32037343', '32037344')}'"))
        self.assertIsNotNone(scan.finding(f"curl '{urls[2].replace('123631127', '123631128')}'"))


if __name__ == "__main__":
    unittest.main()
