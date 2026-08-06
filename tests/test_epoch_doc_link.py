import importlib.machinery
import importlib.util
import os
import pathlib
import stat
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "epoch-doc-link"


def load_module():
    loader = importlib.machinery.SourceFileLoader("epoch_doc_link", str(SCRIPT))
    spec = importlib.util.spec_from_loader("epoch_doc_link", loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules["epoch_doc_link"] = module
    spec.loader.exec_module(module)
    return module


class EpochDocLinkTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()

    def test_create_writes_exact_shortcut_and_preserves_id_case(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            self.mod.create_link("AbC_19-xYz", destination)
            contents = destination.read_bytes()

        self.assertEqual(
            contents,
            b"[InternetShortcut]\nURL=epoch-doc:///document/AbC_19-xYz\n",
        )

    def test_create_is_atomic_idempotent_and_mode_0644(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            with mock.patch.object(
                self.mod.os, "link", wraps=self.mod.os.link
            ) as link:
                self.mod.create_link("Doc_1", destination)
            first_inode = destination.stat().st_ino
            self.mod.create_link("Doc_1", destination)

            self.assertEqual(stat.S_IMODE(destination.stat().st_mode), 0o644)
            self.assertEqual(destination.stat().st_ino, first_inode)
            self.assertEqual(link.call_count, 1)
            self.assertEqual(list(pathlib.Path(temp_dir).iterdir()), [destination])

    def test_create_refuses_different_regular_file(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            destination.write_text("keep me")
            with self.assertRaisesRegex(
                self.mod.LinkError, "different regular file"
            ):
                self.mod.create_link("Doc_1", destination)
            self.assertEqual(destination.read_text(), "keep me")

    def test_create_refuses_crlf_shortcut_as_different_exact_bytes(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            original = (
                b"[InternetShortcut]\r\n"
                b"URL=epoch-doc:///document/Doc_1\r\n"
            )
            destination.write_bytes(original)
            destination.chmod(0o644)

            with self.assertRaisesRegex(
                self.mod.LinkError, "different regular file"
            ):
                self.mod.create_link("Doc_1", destination)

            self.assertEqual(destination.read_bytes(), original)

    def test_create_refuses_file_that_appears_during_atomic_publish(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            real_link = self.mod.os.link

            def collide(source, target):
                pathlib.Path(target).write_text("concurrent contents")
                return real_link(source, target)

            with mock.patch.object(self.mod.os, "link", side_effect=collide):
                with self.assertRaisesRegex(self.mod.LinkError, "appeared"):
                    self.mod.create_link("Doc_1", destination)
            self.assertEqual(destination.read_text(), "concurrent contents")

    def test_create_refuses_symlink_even_if_target_matches(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = pathlib.Path(temp_dir)
            target = root / "target"
            target.write_text(
                "[InternetShortcut]\nURL=epoch-doc:///document/Doc_1\n"
            )
            destination = root / "Paper.url"
            destination.symlink_to(target)
            with self.assertRaisesRegex(self.mod.LinkError, "symlink"):
                self.mod.create_link("Doc_1", destination)

    def test_create_refuses_non_regular_destination(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            destination.mkdir()
            with self.assertRaisesRegex(self.mod.LinkError, "non-file"):
                self.mod.create_link("Doc_1", destination)

    def test_create_requires_existing_parent_and_url_suffix(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = pathlib.Path(temp_dir)
            with self.assertRaisesRegex(self.mod.LinkError, "does not exist"):
                self.mod.create_link("Doc_1", root / "missing" / "Paper.url")
            with self.assertRaisesRegex(self.mod.LinkError, "\\.url"):
                self.mod.create_link("Doc_1", root / "Paper.gdoc")

    def test_doc_id_validation_is_strict(self):
        for doc_id in ("", "with space", "slash/id", "dot.id", "é"):
            with self.subTest(doc_id=doc_id):
                with self.assertRaises(self.mod.LinkError):
                    self.mod.validate_doc_id(doc_id)

    def test_url_validation_is_exact_and_preserves_case(self):
        self.assertEqual(
            self.mod.doc_id_from_url("epoch-doc:///document/AbC_19-xYz"),
            "AbC_19-xYz",
        )
        invalid = (
            "EPOCH-DOC:///document/Doc_1",
            "epoch-doc://document/Doc_1",
            "epoch-doc:///Document/Doc_1",
            "epoch-doc:///document/Doc_1/edit",
            "epoch-doc:///document/Doc_1?x=1",
            "https://docs.google.com/document/d/Doc_1/edit",
        )
        for url in invalid:
            with self.subTest(url=url):
                with self.assertRaises(self.mod.LinkError):
                    self.mod.doc_id_from_url(url)

    def test_open_uses_exact_opener_argv_and_propagates_failure(self):
        completed = subprocess.CompletedProcess([], 7)
        with mock.patch.object(
            self.mod.subprocess, "run", return_value=completed
        ) as run:
            status = self.mod.open_link("epoch-doc:///document/AbC_19-xYz")

        self.assertEqual(status, 7)
        run.assert_called_once_with(
            [
                str(pathlib.Path.home() / "bin" / "chrome-profile-open"),
                "epoch",
                "https://docs.google.com/document/d/AbC_19-xYz/edit",
            ],
            check=False,
        )

    def test_invalid_url_never_launches(self):
        with mock.patch.object(self.mod.subprocess, "run") as run:
            with self.assertRaises(self.mod.LinkError):
                self.mod.open_link("epoch-doc:///document/not/valid")
        run.assert_not_called()

    def test_malformed_cli_input_exits_status_2_visibly(self):
        result = subprocess.run(
            [str(SCRIPT), "open", "epoch-doc:///wrong/Doc_1"],
            text=True,
            capture_output=True,
        )
        self.assertEqual(result.returncode, 2)
        self.assertTrue(result.stderr.strip())


if __name__ == "__main__":
    unittest.main()
