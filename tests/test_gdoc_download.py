"""Runs under the installed gdoc environment's Python; no credentials required.

Under any other interpreter the module delegates to that one (see
tests/interpreter_delegation.py).
"""
from contextlib import redirect_stderr, redirect_stdout
import hashlib
import importlib.util
import io
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import MagicMock, patch

sys.path.insert(0, str(Path(__file__).resolve().parent))
from interpreter_delegation import delegated_case  # noqa: E402

SOURCE = Path(__file__).resolve().parents[1] / 'claude/bin/gdoc_download.py'
GDOC_PYTHON = Path.home() / '.local/share/uv/tools/gdoc/bin/python'
OWN_INTERPRETER = importlib.util.find_spec('gdoc') is not None
if OWN_INTERPRETER:
    spec = importlib.util.spec_from_file_location('gdoc_download', SOURCE)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

DelegatedRun = unittest.skipIf(OWN_INTERPRETER, 'already under the gdoc interpreter')(
    delegated_case('test_gdoc_download', GDOC_PYTHON))


@unittest.skipUnless(OWN_INTERPRETER, 'runs through DelegatedRun under the gdoc interpreter')
class DownloadTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.output = Path(self.directory.name) / 'paper.pdf'
        self.data = b'%PDF-1.7\noriginal bytes\x00\xff'
        self.metadata = dict(name='paper.pdf', mimeType='application/pdf',
                             size=str(len(self.data)), capabilities={'canDownload': True},
                             md5Checksum=hashlib.md5(self.data).hexdigest())
        self.service = MagicMock()
        self.service.files.return_value.get.return_value.execute.return_value = self.metadata
        self.service_patch = patch.object(module, 'get_drive_service', return_value=self.service)
        self.service_patch.start()
        self.addCleanup(self.service_patch.stop)

    def download(self, fail=False, race=False):
        def factory(stream, request):
            class Downloader:
                def next_chunk(inner):
                    stream.write(self.data)
                    if race:
                        self.output.write_bytes(b'other owner')
                    if fail:
                        raise OSError('interrupted')
                    return None, True
            return Downloader()
        with patch.object(module, 'MediaIoBaseDownload', side_effect=factory):
            return module.download_file('file-id', self.output)

    def test_preserves_binary_bytes(self):
        self.download()
        self.assertEqual(self.output.read_bytes(), self.data)
        self.service.files.return_value.get_media.assert_called_once_with(
            fileId='file-id', supportsAllDrives=True)
        self.service.files.return_value.export_media.assert_not_called()

    def test_existing_destination_is_never_touched(self):
        self.output.write_bytes(b'owned')
        with self.assertRaises(module.GdocError):
            self.download()
        self.assertEqual(self.output.read_bytes(), b'owned')
        self.service.files.assert_not_called()

    def test_racing_destination_is_never_replaced(self):
        with self.assertRaises(module.GdocError):
            self.download(race=True)
        self.assertEqual(self.output.read_bytes(), b'other owner')
        self.assertEqual(list(self.output.parent.iterdir()), [self.output])

    def test_native_document_and_denied_download_rejected(self):
        for update in [{'mimeType': 'application/vnd.google-apps.document'},
                       {'mimeType': 'application/pdf', 'capabilities': {'canDownload': False}}]:
            self.metadata.update(update)
            with self.assertRaises(module.GdocError):
                self.download()
            self.service.files.return_value.get_media.assert_not_called()
            self.assertFalse(self.output.exists())

    def test_failed_or_changed_download_leaves_no_artifact(self):
        for change in ['interrupted', 'size', 'checksum']:
            with self.subTest(change=change):
                if change == 'size':
                    self.metadata['size'] = '999'
                if change == 'checksum':
                    self.metadata['size'] = str(len(self.data))
                    self.metadata['md5Checksum'] = 'wrong'
                with self.assertRaises(module.GdocError):
                    self.download(fail=change == 'interrupted')
                self.assertEqual(list(self.output.parent.iterdir()), [])

    def test_cli_existing_commands_and_allowlist_preserved(self):
        parser = module.build_parser()
        self.assertEqual(parser.parse_args(['download', 'id', '--output', 'x', '--account', 'personal']).account, 'personal')
        self.assertEqual(parser.parse_args(['export', 'id', '--out', 'x.pdf']).func, module.cli.cmd_export)
        with patch.object(module.cli, 'build_parser', module.build_parser):
            self.assertEqual(module.cli.run_argv(['--allow-commands', 'cat', 'download', 'id', '--out', 'x'], check_updates=False), 3)
        self.service.files.assert_not_called()


@unittest.skipUnless(OWN_INTERPRETER, 'runs through DelegatedRun under the gdoc interpreter')
class EmptyWriteGuardTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.upstream = {name: MagicMock(return_value=0) for name in ('cmd_write', 'cmd_push')}
        for name, mock in self.upstream.items():
            patcher = patch.object(module.cli, name, mock)
            patcher.start()
            self.addCleanup(patcher.stop)
        self.parser = module.build_parser()

    def run_upload(self, content, *extra):
        source = Path(self.directory.name) / 'doc.md'
        source.write_text(content, encoding='utf-8')
        results = {}
        for command, argv in [('cmd_write', ['write', 'doc-id', str(source)]),
                              ('cmd_push', ['push', str(source)])]:
            args = self.parser.parse_args(argv + list(extra))
            try:
                results[command] = args.func(args)
            except module.GdocError as error:
                results[command] = error
        return results

    def test_empty_or_frontmatter_only_file_is_refused(self):
        for content in ['', '  \n\n', '---\ngdoc: doc-id\ntitle: T\n---\n\n']:
            with self.subTest(content=content):
                for command, result in self.run_upload(content).items():
                    self.assertIsInstance(result, module.GdocError)
                    self.upstream[command].assert_not_called()

    def test_content_and_explicit_clear_reach_upstream(self):
        for content, extra in [('# Title\n\nText.\n', ()), ('', ('--allow-empty',))]:
            with self.subTest(content=content):
                for command, result in self.run_upload(content, *extra).items():
                    self.assertEqual(result, 0)
                    self.upstream[command].assert_called_once()
                    self.upstream[command].reset_mock()


def thread(cid, quote, content='note'):
    return {'id': cid, 'content': content, 'author': {'displayName': 'Ann'},
            'createdTime': '2026-10-14T09:00:00Z', 'resolved': False,
            'quotedFileContent': {'value': quote} if quote is not None else {},
            'replies': [{'author': {'displayName': 'Bo'}, 'content': 'reply'}]}


@unittest.skipUnless(OWN_INTERPRETER, 'runs through DelegatedRun under the gdoc interpreter')
class CatCommentsTests(unittest.TestCase):
    TABS = ['Talks\nTom on acausal catastrophe risk.\nIt is here.',
            'Logistics\nRooms: Manor Hall & Byre. It is here too.']
    COMMENTS = [thread('a', 'acausal catastrophe risk'), thread('b', 'Manor Hall &amp; Byre'),
                thread('c', 'here'), thread('d', 'deleted text'), thread('e', None),
                thread('f', 'is h')]

    def setUp(self):
        self.upstream = MagicMock(side_effect=lambda args: print('BODY') or 0)
        patcher = patch.object(module.cli, 'cmd_cat', self.upstream)
        patcher.start()
        self.addCleanup(patcher.stop)
        for name, value in [('_resolve_doc_id', lambda doc: doc)]:
            p = patch.object(module.cli, name, value)
            p.start()
            self.addCleanup(p.stop)
        p = patch('gdoc.api.comments.list_comments', return_value=self.COMMENTS)
        self.list_comments = p.start()
        self.addCleanup(p.stop)
        p = patch.object(module, '_tab_texts', return_value=(self.TABS, 0))
        self.tab_texts = p.start()
        self.addCleanup(p.stop)
        self.parser = module.build_parser()

    def run_cat(self, *argv):
        args = self.parser.parse_args(['cat', 'doc-id', *argv])
        out, err = io.StringIO(), io.StringIO()
        with redirect_stdout(out), redirect_stderr(err):
            status = args.func(args)
        return status, out.getvalue(), err.getvalue()

    def test_whole_doc_appends_every_thread(self):
        status, out, _ = self.run_cat()
        self.assertEqual(status, 0)
        self.assertTrue(out.startswith('BODY\n'))
        self.assertIn('## Comments (6 threads, open and resolved)', out)
        self.assertIn('-> Bo: "reply"', out)
        self.assertIn('on "Manor Hall & Byre"', out)
        self.tab_texts.assert_not_called()

    def test_tab_shows_its_threads_and_counts_the_rest(self):
        _, out, _ = self.run_cat('--tab', 'Talks')
        self.assertIn('## Comments (2 threads, open and resolved; not shown: 1 on other tabs '
                      'and 3 whose quoted text is no longer in the doc', out)
        self.assertIn('#a ', out)
        self.assertIn('on "here" (this text is also on other tabs)', out)
        self.assertNotIn('#b ', out)

    def test_short_quotes_match_whole_words_only(self):
        self.assertFalse(module._contains('this', module._folded('this'), 'is h'))
        self.assertTrue(module._contains('It is here', module._folded('It is here'), 'is  here'))

    def test_json_carries_the_threads(self):
        self.upstream.side_effect = lambda args: print(json.dumps({'ok': True, 'content': 'BODY'})) or 0
        _, out, _ = self.run_cat('--json')
        data = json.loads(out)
        self.assertEqual(len(data['comments']), 6)
        self.assertIn('Suggestion threads', data['comments_note'])

    def test_opt_outs_and_plain_leave_upstream_output_alone(self):
        for argv in (['--no-comments'], ['--comments'], ['--plain']):
            with self.subTest(argv=argv):
                _, out, err = self.run_cat(*argv)
                self.assertEqual(out, 'BODY\n')
        self.assertIn('comments not shown with --plain', err)

    def test_failed_comment_read_is_reported_and_body_kept(self):
        self.list_comments.side_effect = RuntimeError('quota')
        status, out, err = self.run_cat()
        self.assertEqual((status, out), (0, 'BODY\n'))
        self.assertIn('comments could not be read (quota)', err)


if __name__ == '__main__':
    unittest.main()
