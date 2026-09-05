import importlib.machinery
import importlib.util
import io
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

            def collide(source, target, **kwargs):
                destination.write_text("concurrent contents")
                return real_link(source, target, **kwargs)

            with mock.patch.object(self.mod.os, "link", side_effect=collide):
                with self.assertRaisesRegex(self.mod.LinkError, "appeared"):
                    self.mod.create_link("Doc_1", destination)
            self.assertEqual(destination.read_text(), "concurrent contents")

    def test_idempotent_read_rejects_destination_replaced_by_symlink(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = pathlib.Path(temp_dir)
            destination = root / "Paper.url"
            target = root / "target"
            content = self.mod.shortcut_text("Doc_1")
            for path in (destination, target):
                path.write_text(content)
                path.chmod(0o644)
            real_stat = self.mod.os.stat
            replaced = False

            def replace_then_stat(path, *args, **kwargs):
                nonlocal replaced
                if not replaced and str(path) in (str(destination), destination.name):
                    replaced = True
                    destination.unlink()
                    destination.symlink_to(target)
                return real_stat(path, *args, **kwargs)

            with mock.patch.object(self.mod.os, "stat", side_effect=replace_then_stat):
                with self.assertRaises(self.mod.LinkError):
                    self.mod.create_link("Doc_1", destination)
            self.assertTrue(replaced)
            self.assertTrue(destination.is_symlink())
            self.assertEqual(target.read_text(), content)

    def test_parent_replacement_cannot_redirect_creation_or_cleanup(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = pathlib.Path(temp_dir)
            parent = root / "intended"
            moved = root / "original"
            other = root / "other"
            parent.mkdir()
            other.mkdir()
            destination = parent / "Paper.url"
            real_open = self.mod.os.open
            real_mkstemp = tempfile.mkstemp
            replaced = False

            def replace_parent():
                nonlocal replaced
                if not replaced:
                    replaced = True
                    parent.rename(moved)
                    parent.symlink_to(other, target_is_directory=True)

            def replace_before_open(path, flags, *args, **kwargs):
                if flags & os.O_CREAT:
                    replace_parent()
                return real_open(path, flags, *args, **kwargs)

            def replace_before_mkstemp(*args, **kwargs):
                replace_parent()
                return real_mkstemp(*args, **kwargs)

            with mock.patch.object(self.mod.os, "open", side_effect=replace_before_open), \
                 mock.patch.object(tempfile, "mkstemp", side_effect=replace_before_mkstemp):
                with self.assertRaises(self.mod.LinkError):
                    self.mod.create_link("Doc_1", destination)
            self.assertTrue(replaced)
            self.assertEqual(list(other.iterdir()), [])
            self.assertEqual(list(moved.iterdir()), [])

    def test_creation_oserror_has_controlled_cli_failure_and_cleans_temp(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            stderr = io.StringIO()
            with mock.patch.object(self.mod.os, "fsync", side_effect=PermissionError("fixture denied")), \
                 mock.patch.object(sys, "stderr", stderr):
                status = self.mod.main(["create", "Doc_1", str(destination)])
            self.assertEqual(status, 2)
            self.assertIn("epoch-doc-link:", stderr.getvalue())
            self.assertNotIn("Traceback", stderr.getvalue())
            self.assertEqual(list(pathlib.Path(temp_dir).iterdir()), [])

    def test_published_destination_is_read_back_before_success(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            real_link = self.mod.os.link

            def replace_published_file(source, target, **kwargs):
                real_link(source, target, **kwargs)
                destination.unlink()
                destination.write_text("concurrent contents")

            with mock.patch.object(self.mod.os, "link", side_effect=replace_published_file):
                with self.assertRaises(self.mod.LinkError):
                    self.mod.create_link("Doc_1", destination)
            self.assertEqual(destination.read_text(), "concurrent contents")
            self.assertEqual(list(pathlib.Path(temp_dir).iterdir()), [destination])

    def test_published_same_content_replacement_is_not_mistaken_for_created_file(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            real_link = self.mod.os.link

            def replace_published_file(source, target, **kwargs):
                real_link(source, target, **kwargs)
                destination.unlink()
                destination.write_text(self.mod.shortcut_text("Doc_1"))
                destination.chmod(0o644)

            with mock.patch.object(self.mod.os, "link", side_effect=replace_published_file):
                with self.assertRaisesRegex(self.mod.LinkError, "changed"):
                    self.mod.create_link("Doc_1", destination)
            self.assertEqual(destination.read_text(), self.mod.shortcut_text("Doc_1"))
            self.assertEqual(list(pathlib.Path(temp_dir).iterdir()), [destination])

    def test_replaced_temporary_file_is_preserved_before_or_during_publish(self):
        for boundary in ("fchmod", "link"):
            with self.subTest(boundary=boundary), tempfile.TemporaryDirectory() as temp_dir:
                root = pathlib.Path(temp_dir)
                destination = root / "Paper.url"
                replacement = None
                real_operation = getattr(self.mod.os, boundary)

                def replace_temporary(*args, **kwargs):
                    nonlocal replacement
                    replacement, = root.iterdir()
                    replacement.unlink()
                    replacement.write_text("concurrent temporary contents")
                    replacement.chmod(0o600)
                    return real_operation(*args, **kwargs)

                with mock.patch.object(self.mod.os, boundary, side_effect=replace_temporary):
                    with self.assertRaises(self.mod.LinkError):
                        self.mod.create_link("Doc_1", destination)
                self.assertIsNotNone(replacement)
                self.assertTrue(replacement.exists(), "cleanup must preserve a replaced temp name")
                self.assertEqual(replacement.read_text(), "concurrent temporary contents")
                self.assertEqual(stat.S_IMODE(replacement.stat().st_mode), 0o600)
                if boundary == "fchmod":
                    self.assertFalse(destination.exists())

    def test_missing_temporary_name_is_a_controlled_failure(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            real_link = self.mod.os.link

            def remove_temporary_after_publish(source, target, **kwargs):
                real_link(source, target, **kwargs)
                os.unlink(source, dir_fd=kwargs["src_dir_fd"])

            stderr = io.StringIO()
            with mock.patch.object(self.mod.os, "link", side_effect=remove_temporary_after_publish), \
                 mock.patch.object(sys, "stderr", stderr):
                status = self.mod.main(["create", "Doc_1", str(destination)])
            self.assertEqual(status, 2)
            self.assertIn("Temporary shortcut disappeared", stderr.getvalue())
            self.assertEqual(destination.read_text(), self.mod.shortcut_text("Doc_1"))
            self.assertEqual(list(pathlib.Path(temp_dir).iterdir()), [destination])

    def test_parent_replaced_at_publication_does_not_delete_concurrent_content(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = pathlib.Path(temp_dir)
            parent = root / "intended"
            moved = root / "original"
            other = root / "other"
            parent.mkdir()
            other.mkdir()
            destination = parent / "Paper.url"
            concurrent = other / destination.name
            concurrent.write_text("concurrent contents")
            real_link = self.mod.os.link

            def replace_parent_before_publish(source, target, **kwargs):
                parent.rename(moved)
                parent.symlink_to(other, target_is_directory=True)
                return real_link(source, target, **kwargs)

            with mock.patch.object(self.mod.os, "link", side_effect=replace_parent_before_publish):
                with self.assertRaisesRegex(self.mod.LinkError, "directory changed"):
                    self.mod.create_link("Doc_1", destination)
            self.assertEqual(concurrent.read_text(), "concurrent contents")
            self.assertEqual((moved / destination.name).read_text(), self.mod.shortcut_text("Doc_1"))
            self.assertEqual(list(moved.iterdir()), [moved / destination.name])

    def test_stable_parent_symlink_alias_is_supported(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = pathlib.Path(temp_dir)
            parent = root / "original"
            alias = root / "alias"
            parent.mkdir()
            alias.symlink_to(parent, target_is_directory=True)
            self.mod.create_link("Doc_1", alias / "Paper.url")
            self.mod.create_link("Doc_1", alias / "Paper.url")
            self.assertEqual((parent / "Paper.url").read_text(), self.mod.shortcut_text("Doc_1"))

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

    def test_create_refuses_fifo_without_waiting_for_writer(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            os.mkfifo(destination)
            result = subprocess.run(
                [str(SCRIPT), "create", "Doc_1", str(destination)],
                text=True, capture_output=True, timeout=5,
            )
            self.assertEqual(result.returncode, 2)
            self.assertIn("non-file", result.stderr)
            self.assertTrue(stat.S_ISFIFO(destination.stat().st_mode))

    def test_idempotent_create_refuses_wrong_mode_without_changing_file(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            destination.write_text(self.mod.shortcut_text("Doc_1"))
            destination.chmod(0o600)
            before = destination.stat()
            with self.assertRaisesRegex(self.mod.LinkError, "unexpected mode"):
                self.mod.create_link("Doc_1", destination)
            self.assertEqual(destination.stat().st_ino, before.st_ino)
            self.assertEqual(stat.S_IMODE(destination.stat().st_mode), 0o600)
            self.assertEqual(destination.read_text(), self.mod.shortcut_text("Doc_1"))

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

    def test_create_cli_is_exact_idempotent_and_no_overwrite(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = pathlib.Path(temp_dir) / "Paper.url"
            command = [str(SCRIPT), "create", "Doc_1", str(destination)]
            first = subprocess.run(command, text=True, capture_output=True)
            self.assertEqual(first.returncode, 0, first.stderr)
            original_inode = destination.stat().st_ino
            repeated = subprocess.run(command, text=True, capture_output=True)
            self.assertEqual(repeated.returncode, 0, repeated.stderr)
            self.assertEqual(destination.stat().st_ino, original_inode)
            self.assertEqual(stat.S_IMODE(destination.stat().st_mode), 0o644)
            self.assertEqual(destination.read_bytes(), self.mod.shortcut_text("Doc_1").encode("ascii"))
            different = subprocess.run(
                [str(SCRIPT), "create", "Doc_2", str(destination)],
                text=True, capture_output=True,
            )
            self.assertEqual(different.returncode, 2)
            self.assertEqual(destination.stat().st_ino, original_inode)
            self.assertEqual(destination.read_text(), self.mod.shortcut_text("Doc_1"))


if __name__ == "__main__":
    unittest.main()
