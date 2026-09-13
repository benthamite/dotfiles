"""Run with the installed gdoc environment's Python; no credentials required."""
import hashlib
import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest.mock import MagicMock, patch

SOURCE = Path(__file__).resolve().parents[1] / 'claude/bin/gdoc_download.py'
spec = importlib.util.spec_from_file_location('gdoc_download', SOURCE)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


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


if __name__ == '__main__':
    unittest.main()
