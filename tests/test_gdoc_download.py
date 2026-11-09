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


@unittest.skipUnless(OWN_INTERPRETER, 'runs through DelegatedRun under the gdoc interpreter')
class StarBulletsTests(unittest.TestCase):
    """Drive's Markdown import keeps `-` and `+` as the bullet glyph; `*` gives round bullets."""

    def test_dash_and_plus_markers_become_stars_at_every_level(self):
        self.assertEqual(module.star_bullets('- a\n  - b\n    + c\n* d'),
                         '* a\n  * b\n    * c\n* d')

    def test_inline_formatting_and_blockquotes_keep_their_text(self):
        self.assertEqual(module.star_bullets('- **Fork:** x - y\n> - quoted'),
                         '* **Fork:** x - y\n> * quoted')

    def test_code_fences_thematic_breaks_and_tables_are_left_alone(self):
        text = '```\n- in code\n```\n---\n- - -\n| - | x |\n~~~\n+ also code\n~~~'
        self.assertEqual(module.star_bullets(text), text)

    def test_hyphens_that_are_not_list_markers_are_left_alone(self):
        text = '-1 degrees\n--flag\nnot - a list\n-'
        self.assertEqual(module.star_bullets(text), text)

    def test_indented_code_maths_pre_and_front_matter_are_left_alone(self):
        for text in ('para\n\n    - code', '$$\na\n- b\n+ c\n$$', '<pre>\n- x\n</pre>',
                     '---\ntags:\n  - a\n---\nbody'):
            self.assertEqual(module.star_bullets(text), text)

    def test_nested_items_indented_four_spaces_are_still_list_items(self):
        self.assertEqual(module.star_bullets('- a\n    - b'), '* a\n    * b')

    def test_a_longer_fence_is_not_closed_by_a_shorter_one(self):
        text = '````\n```\n- in code\n```\n````'
        self.assertEqual(module.star_bullets(text), text)

    def test_uploads_receive_the_rewritten_markdown(self):
        upload = MagicMock(return_value='result', __name__='update_doc_content', __doc__='doc')
        wrapped = module.with_star_bullets(upload)
        self.assertEqual(wrapped('doc-id', '- a', folder_id='f'), 'result')
        upload.assert_called_once_with('doc-id', '* a', folder_id='f')

    def test_cli_wraps_both_drive_markdown_uploads(self):
        source = SOURCE.read_text()
        for name in ('create_doc_from_markdown', 'update_doc_content'):
            self.assertRegex(source, rf'drive\.{name} = with_fitted_tables\(\s*'
                                     rf'with_star_bullets\(drive\.{name}\)')


class _UnitMetrics:
    """One point per character, so piece widths are character counts."""

    def width(self, text, size):
        return float(len(text))


def _cell(*paragraphs, bold=False):
    return {'content': [{'paragraph': {'elements': [
        {'textRun': {'content': text + '\n', 'textStyle': {'bold': True} if bold else {}}}]}}
        for text in paragraphs]}


def _table(rows, index, fixed=False):
    columns = len(rows[0])
    width = {'widthType': 'FIXED_WIDTH', 'width': {'magnitude': 50, 'unit': 'PT'}} if fixed \
        else {'widthType': 'EVENLY_DISTRIBUTED'}
    return {'startIndex': index, 'table': {
        'rows': len(rows), 'columns': columns,
        'tableStyle': {'tableColumnProperties': [dict(width) for _ in range(columns)]},
        'tableRows': [{'tableCells': [_cell(text) for text in row]} for row in rows]}}


def _document(*tables):
    return {'tabs': [{'tabProperties': {'tabId': 't.0'}, 'documentTab': {
        'documentStyle': {'pageSize': {'width': {'magnitude': 612, 'unit': 'PT'}},
                          'marginLeft': {'magnitude': 72, 'unit': 'PT'},
                          'marginRight': {'magnitude': 72, 'unit': 'PT'}},
        'namedStyles': {'styles': [{'namedStyleType': 'NORMAL_TEXT', 'textStyle': {
            'fontSize': {'magnitude': 11, 'unit': 'PT'},
            'weightedFontFamily': {'fontFamily': 'Arial', 'weight': 400}}}]},
        'body': {'content': [{'paragraph': {}}, *tables]}}}]}


@unittest.skipUnless(OWN_INTERPRETER, 'runs through DelegatedRun under the gdoc interpreter')
class TableWidthTests(unittest.TestCase):
    """Drive's Markdown import makes every column the same width; fitted
    widths minimise each table's height (Pablo, 9 Nov 2026)."""

    def unit_fonts(self):
        fonts = patch.object(module, 'font_metrics', return_value=_UnitMetrics())
        fonts.start()
        self.addCleanup(fonts.stop)

    @unittest.skipUnless(Path('/System/Library/Fonts/Supplemental/Arial.ttf').is_file(), 'no Arial')
    def test_arial_advances_come_from_the_font_file(self):
        arial = module.FontMetrics('/System/Library/Fonts/Supplemental/Arial.ttf')
        self.assertAlmostEqual(arial.width('a', 11), 1139 * 11 / 2048)
        self.assertAlmostEqual(arial.width('aaaa', 11), 4 * arial.width('a', 11))
        self.assertLess(arial.width('iii', 11), arial.width('MMM', 11))
        self.assertGreater(arial.width('ñ', 11), 0)

    @unittest.skipUnless(Path('/System/Library/Fonts/Supplemental/Arial.ttf').is_file(), 'no Arial')
    def test_missing_fonts_are_approximated_openly(self):
        module.font_metrics.cache_clear()
        self.addCleanup(module.font_metrics.cache_clear)
        self.assertAlmostEqual(module.font_metrics('Nowhere Mono', False).width('abc', 10), 18)
        with redirect_stderr(io.StringIO()) as err:
            metrics = module.font_metrics('Nowhere Sans', True)
        self.assertIs(metrics, module.font_metrics('Arial', True))
        self.assertIn('Nowhere Sans', err.getvalue())
        module.font_metrics.cache_clear()
        with patch.object(module, 'FONT_DIRECTORIES', ()), self.assertRaises(module.GdocError):
            module.font_metrics('Arial', False)

    def test_pieces_break_at_spaces_and_after_hyphens(self):
        self.unit_fonts()
        characters = [(c, ('Arial', False, 11)) for c in 'ab  cd-ef']
        self.assertEqual(module._pieces(characters), [(2, 0), (3, 2), (2, 0)])

    def test_lines_wrap_greedily_and_split_overlong_words(self):
        self.assertEqual(module.line_count([[(10, 0), (10, 5), (10, 5)]], 25), 2)
        self.assertEqual(module.line_count([[(10, 0), (10, 5), (10, 5)]], 40), 1)
        self.assertEqual(module.line_count([[(50, 0)]], 20), 3)
        self.assertEqual(module.line_count([[], [(5, 0)]], 20), 2)
        self.assertEqual(module.split_count([[(50, 0), (5, 1)]], 20), 1)

    def test_fitted_widths_beat_an_even_split_and_respect_the_bounds(self):
        short = [[(8, 0)]]
        long = [[(8, 3)] * 40]
        cells = [[short, long], [short, long], [short, short]]
        padding = [10, 10]
        widths = module.fit_widths(cells, 200, padding)
        self.assertEqual(sum(widths), 200)
        self.assertGreaterEqual(min(widths), module.MIN_COLUMN_PT)
        self.assertGreater(widths[1], widths[0])

        def height(ws):
            return sum(max(module.line_count(cell, w - 10 - module.TEXT_SAFETY_PT)
                           for cell, w in zip(row, ws)) for row in cells)
        self.assertLess(height(widths), height([100, 100]))

    def test_a_word_is_broken_only_when_that_saves_lines(self):
        word = [[(60, 0)]]
        text = [[(8, 3)] * 6]
        widths = module.fit_widths([[word, text]], 200, [10, 10])
        self.assertGreaterEqual(widths[0] - 10 - module.TEXT_SAFETY_PT, 60)

    def test_plan_fits_even_tables_and_keeps_hand_set_widths(self):
        self.unit_fonts()
        rows = [['#', 'Long description ' * 6], ['1', 'Short']]
        plans = module.plan_table_widths(_document(_table(rows, 5), _table(rows, 90, fixed=True)))
        (fitted, requests), (kept, none) = plans
        self.assertEqual(kept['skipped'], 'has fixed widths')
        self.assertEqual(none, [])
        self.assertEqual(sum(fitted['widths']), 468)
        self.assertLessEqual(fitted['lines_after'], fitted['lines_before'])
        self.assertEqual([r['updateTableColumnProperties']['columnIndices'] for r in requests],
                         [[0], [1]])
        location = requests[0]['updateTableColumnProperties']['tableStartLocation']
        self.assertEqual(location, {'index': 5, 'tabId': 't.0'})
        refit = module.plan_table_widths(_document(_table(rows, 90, fixed=True)), refit=True)
        self.assertEqual(len(refit[0][1]), 2)

    def test_merged_cells_are_left_alone(self):
        self.unit_fonts()
        table = _table([['a', 'b'], ['c', 'd']], 5)
        table['table']['tableRows'][1]['tableCells'] = [
            dict(_cell('cd'), tableCellStyle={'columnSpan': 2})]
        (report, requests), = module.plan_table_widths(_document(table))
        self.assertEqual((report['skipped'], requests), ('has merged cells', []))

    def test_uploads_report_the_version_after_fitting(self):
        with patch.object(module, 'fit_tables', return_value=[{'widths': [1]}]) as fit, \
                patch.object(module, '_current_version', return_value=7):
            create = module.with_fitted_tables(
                MagicMock(return_value={'id': 'new', 'version': 5}, __name__='c', __doc__=''),
                lambda args, kwargs, result: result['id'])
            self.assertEqual(create('title', 'md'), {'id': 'new', 'version': 7})
            fit.assert_called_once_with('new')
            update = module.with_fitted_tables(
                MagicMock(return_value=5, __name__='u', __doc__=''),
                lambda args, kwargs, result: args[0])
            self.assertEqual(update('doc', 'md'), 7)

    def test_a_failed_fit_keeps_the_upload_and_says_so(self):
        with patch.object(module, 'fit_tables', side_effect=module.GdocError('no Arial')), \
                redirect_stderr(io.StringIO()) as err:
            update = module.with_fitted_tables(
                MagicMock(return_value=5, __name__='u', __doc__=''),
                lambda args, kwargs, result: args[0])
            self.assertEqual(update('doc', 'md'), 5)
        self.assertIn('table column widths not fitted: no Arial', err.getvalue())


@unittest.skipUnless(OWN_INTERPRETER, 'runs through DelegatedRun under the gdoc interpreter')
class TokenWriteTests(unittest.TestCase):
    """Parallel runs refreshing one token must all save it."""

    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.path = Path(self.directory.name) / 'accounts/forethought/token.json'

    def test_parallel_writes_all_succeed(self):
        import multiprocessing
        context = multiprocessing.get_context('spawn')
        with context.Manager() as manager:
            start = manager.Event()
            with context.Pool(6) as pool:
                results = [pool.apply_async(_race_write, (str(SOURCE), str(self.path), i, start))
                           for i in range(6)]
                start.set()
                outcomes = [r.get(timeout=60) for r in results]
        self.assertEqual(outcomes, ['ok'] * 6)
        self.assertIn(json.loads(self.path.read_text())['writer'], range(6))
        self.assertEqual([p.name for p in self.path.parent.iterdir()], ['token.json'])

    def test_mode_is_private(self):
        self.path.parent.mkdir(parents=True)
        self.path.write_text('{}')
        self.path.chmod(0o644)
        module.write_private(self.path, '{"writer": 0}')
        self.assertEqual(self.path.stat().st_mode & 0o777, 0o600)

    def test_cli_saves_tokens_through_it(self):
        self.assertIn('auth._write_private = write_private', SOURCE.read_text())


def _race_write(source, path, writer, start):
    """Load the wrapper in a fresh process and write repeatedly once START is set."""
    spec = importlib.util.spec_from_file_location('gdoc_download', source)
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    start.wait()
    try:
        for _ in range(20):
            loaded.write_private(path, json.dumps({'writer': writer}))
        return 'ok'
    except Exception as error:  # report it, so a failure shows what collided
        return f'{type(error).__name__}: {error}'


if __name__ == '__main__':
    unittest.main()
