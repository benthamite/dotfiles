from __future__ import annotations

import importlib.machinery
import importlib.util
import os
import plistlib
import stat
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "install-epoch-doc-handler"


def load_module():
    loader = importlib.machinery.SourceFileLoader(
        "install_epoch_doc_handler", str(SCRIPT)
    )
    spec = importlib.util.spec_from_loader("install_epoch_doc_handler", loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules["install_epoch_doc_handler"] = module
    loader.exec_module(module)
    return module


class InstallEpochDocHandlerTests(unittest.TestCase):
    def setUp(self):
        self.module = load_module()
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.applications = self.root / "Applications"
        self.drive = self.root / "My Drive"
        self.drive.mkdir()
        self.toolchain = self.module.Toolchain(
            swiftc=Path("/fake/swiftc"),
            codesign=Path("/fake/codesign"),
            plutil=Path("/fake/plutil"),
            lsregister=Path("/fake/lsregister"),
            trash=Path("/fake/trash"),
        )
        self.calls = []
        self.registration_failures = 0
        self.trashed = []

    def runner(self, command):
        self.calls.append(command)
        if command[0] == str(self.toolchain.swiftc):
            output = Path(command[command.index("-o") + 1])
            output.write_text("binary", encoding="utf-8")
            output.chmod(0o755)
        if (
            command[0] == str(self.toolchain.lsregister)
            and self.registration_failures
        ):
            self.registration_failures -= 1
            raise self.module.InstallError("registration failed")

    def trash(self, path, _toolchain):
        self.trashed.append(path)
        count = len(list(self.root.glob("trashed-*")))
        os.replace(path, self.root / f"trashed-{count}")

    def install(self):
        return self.module.install(
            self.applications,
            self.drive,
            self.toolchain,
            runner=self.runner,
            trash_func=self.trash,
        )

    def write_application(self, application, identifier, marker):
        contents = application / "Contents"
        contents.mkdir(parents=True)
        with (contents / "Info.plist").open("wb") as handle:
            plistlib.dump({"CFBundleIdentifier": identifier}, handle)
        (application / "marker").write_text(marker, encoding="utf-8")

    def test_first_install_builds_expected_bundle_and_registers(self):
        installed = self.install()
        info = installed / "Contents" / "Info.plist"
        executable = installed / "Contents" / "MacOS" / "epoch-doc-handler"
        with info.open("rb") as handle:
            payload = plistlib.load(handle)
        self.assertEqual(BUNDLE_ID, payload["CFBundleIdentifier"])
        self.assertEqual(
            ["epoch-doc"], payload["CFBundleURLTypes"][0]["CFBundleURLSchemes"]
        )
        self.assertEqual("Viewer", payload["CFBundleURLTypes"][0]["CFBundleTypeRole"])
        self.assertTrue(executable.is_file())
        self.assertTrue(executable.stat().st_mode & stat.S_IXUSR)
        self.assertEqual(
            [str(self.toolchain.lsregister), "-f", str(installed)],
            self.calls[-1],
        )

    def test_reinstall_replaces_only_matching_generated_bundle(self):
        first = self.install()
        (first / "marker").write_text("old", encoding="utf-8")
        second = self.install()
        self.assertEqual(first, second)
        self.assertFalse((second / "marker").exists())

    def test_refuses_existing_application_with_another_bundle_id(self):
        installed = self.applications / self.module.APP_NAME
        contents = installed / "Contents"
        contents.mkdir(parents=True)
        with (contents / "Info.plist").open("wb") as handle:
            plistlib.dump({"CFBundleIdentifier": "com.example.other"}, handle)
        with self.assertRaisesRegex(self.module.InstallError, "another bundle ID"):
            self.install()
        self.assertEqual([], self.calls)

    def test_cli_refuses_non_dictionary_plist_without_traceback_or_mutation(self):
        for label, payload in (("array", []), ("scalar", "not a dictionary")):
            with self.subTest(label=label):
                root = self.root / label
                applications = root / "Applications"
                drive = root / "My Drive"
                installed = applications / self.module.APP_NAME
                contents = installed / "Contents"
                contents.mkdir(parents=True)
                drive.mkdir()
                info = contents / "Info.plist"
                with info.open("wb") as handle:
                    plistlib.dump(payload, handle)
                original = info.read_bytes()

                result = subprocess.run(
                    [
                        sys.executable,
                        str(SCRIPT),
                        "--applications-root",
                        str(applications),
                        "--drive-root",
                        str(drive),
                    ],
                    capture_output=True,
                    text=True,
                )

                self.assertEqual(1, result.returncode)
                self.assertIn("application metadata", result.stderr)
                self.assertNotIn("Traceback", result.stderr)
                self.assertEqual(original, info.read_bytes())
                self.assertEqual([installed], list(applications.iterdir()))

    def test_refuses_existing_destination_symlink(self):
        installed = self.install()
        external = self.root / "external.app"
        os.replace(installed, external)
        installed.symlink_to(external, target_is_directory=True)
        self.calls.clear()

        with self.assertRaisesRegex(
            self.module.InstallError, "non-application path"
        ):
            self.install()

        self.assertTrue(installed.is_symlink())
        self.assertEqual([], self.calls)

    def test_refuses_destination_inside_drive(self):
        with self.assertRaisesRegex(
            self.module.InstallError, "inside Google Drive"
        ):
            self.module.install(
                self.drive / "Applications",
                self.drive,
                self.toolchain,
                runner=self.runner,
                trash_func=self.trash,
            )
        self.assertEqual([], self.calls)

    def test_refuses_applications_symlink_resolving_inside_drive(self):
        drive_applications = self.drive / "Applications"
        drive_applications.mkdir()
        applications_link = self.root / "Applications-link"
        applications_link.symlink_to(
            drive_applications, target_is_directory=True
        )

        with self.assertRaisesRegex(
            self.module.InstallError, "inside Google Drive"
        ):
            self.module.install(
                applications_link,
                self.drive,
                self.toolchain,
                runner=self.runner,
                trash_func=self.trash,
            )

        self.assertEqual([], self.calls)

    def test_registration_failure_restores_previous_bundle(self):
        installed = self.install()
        (installed / "marker").write_text("preserve", encoding="utf-8")
        self.registration_failures = 1
        with self.assertRaisesRegex(self.module.InstallError, "registration failed"):
            self.install()
        restored = self.applications / self.module.APP_NAME
        self.assertEqual("preserve", (restored / "marker").read_text())
        register = [str(self.toolchain.lsregister), "-f", str(restored)]
        self.assertEqual([register, register], self.calls[-2:])

    def test_success_cleans_staged_build_root_through_trash(self):
        self.install()

        self.assertEqual(1, len(self.trashed))
        self.assertEqual(self.applications, self.trashed[0].parent)
        self.assertTrue(self.trashed[0].name.startswith(".epoch-doc-handler-"))
        self.assertFalse(self.trashed[0].exists())

    def test_reinstall_aborts_if_destination_is_swapped_during_build(self):
        installed = self.install()
        displaced = self.root / "displaced-generated.app"
        swapped = False

        def swapping_runner(command):
            nonlocal swapped
            self.runner(command)
            if (
                command[0] == str(self.toolchain.codesign)
                and "--verify" in command
                and not swapped
            ):
                swapped = True
                os.replace(installed, displaced)
                self.write_application(
                    installed, "com.example.concurrent", "unrelated"
                )

        with self.assertRaisesRegex(self.module.InstallError, "changed during build"):
            self.module.install(
                self.applications,
                self.drive,
                self.toolchain,
                runner=swapping_runner,
                trash_func=self.trash,
            )

        self.assertEqual("unrelated", (installed / "marker").read_text())
        self.assertEqual(
            "com.example.concurrent", self.module.bundle_identifier(installed)
        )
        self.assertTrue(displaced.is_dir())

    def test_first_install_aborts_if_destination_appears_during_build(self):
        installed = self.applications / self.module.APP_NAME
        appeared = False

        def appearing_runner(command):
            nonlocal appeared
            self.runner(command)
            if (
                command[0] == str(self.toolchain.codesign)
                and "--verify" in command
                and not appeared
            ):
                appeared = True
                self.write_application(
                    installed, "com.example.concurrent", "unrelated"
                )

        with self.assertRaisesRegex(self.module.InstallError, "changed during build"):
            self.module.install(
                self.applications,
                self.drive,
                self.toolchain,
                runner=appearing_runner,
                trash_func=self.trash,
            )

        self.assertEqual("unrelated", (installed / "marker").read_text())
        self.assertEqual(
            "com.example.concurrent", self.module.bundle_identifier(installed)
        )

    def test_reinstall_restores_app_swapped_during_the_move(self):
        installed = self.install()
        displaced = self.root / "displaced-generated.app"
        real_replace = self.module.os.replace
        swapped = False

        def swapping_replace(source, destination):
            nonlocal swapped
            source = Path(source)
            destination = Path(destination)
            if (
                source == installed
                and destination.name == "previous.app"
                and not swapped
            ):
                swapped = True
                real_replace(installed, displaced)
                self.write_application(
                    installed, "com.example.concurrent", "unrelated"
                )
            return real_replace(source, destination)

        with mock.patch.object(
            self.module.os, "replace", side_effect=swapping_replace
        ):
            with self.assertRaisesRegex(
                self.module.InstallError, "changed while being moved"
            ):
                self.module.install(
                    self.applications,
                    self.drive,
                    self.toolchain,
                    runner=self.runner,
                    trash_func=self.trash,
                )

        self.assertEqual("unrelated", (installed / "marker").read_text())
        self.assertEqual(
            "com.example.concurrent", self.module.bundle_identifier(installed)
        )
        self.assertTrue(displaced.is_dir())

    def test_successful_registration_swap_aborts_without_touching_foreign_app(self):
        installed = self.install()
        (installed / "previous-marker").write_text("previous", encoding="utf-8")
        displaced_generated = self.root / "displaced-generated.app"
        swapped = False
        trash_count = len(self.trashed)

        def registration_runner(command):
            nonlocal swapped
            self.runner(command)
            if command[0] == str(self.toolchain.lsregister) and not swapped:
                swapped = True
                os.replace(installed, displaced_generated)
                self.write_application(
                    installed, "com.example.concurrent", "foreign"
                )

        with self.assertRaisesRegex(
            self.module.InstallError, "changed during registration"
        ):
            self.module.install(
                self.applications,
                self.drive,
                self.toolchain,
                runner=registration_runner,
                trash_func=self.trash,
            )

        self.assertEqual("foreign", (installed / "marker").read_text())
        self.assertEqual(
            "com.example.concurrent", self.module.bundle_identifier(installed)
        )
        self.assertTrue(displaced_generated.is_dir())
        self.assertEqual(trash_count, len(self.trashed))
        build_roots = list(self.applications.glob(".epoch-doc-handler-*"))
        self.assertEqual(1, len(build_roots))
        self.assertEqual(
            "previous",
            (build_roots[0] / "previous.app" / "previous-marker").read_text(),
        )

    def test_failed_registration_swap_does_not_move_or_trash_foreign_app(self):
        installed = self.install()
        (installed / "previous-marker").write_text("previous", encoding="utf-8")
        displaced_generated = self.root / "displaced-generated.app"
        swapped = False
        trash_count = len(self.trashed)

        def failing_registration_runner(command):
            nonlocal swapped
            if command[0] == str(self.toolchain.lsregister) and not swapped:
                self.calls.append(command)
                swapped = True
                os.replace(installed, displaced_generated)
                self.write_application(
                    installed, "com.example.concurrent", "foreign"
                )
                raise self.module.InstallError("registration failed")
            self.runner(command)

        with self.assertRaisesRegex(
            self.module.InstallError, "changed during registration"
        ):
            self.module.install(
                self.applications,
                self.drive,
                self.toolchain,
                runner=failing_registration_runner,
                trash_func=self.trash,
            )

        self.assertEqual("foreign", (installed / "marker").read_text())
        self.assertEqual(
            "com.example.concurrent", self.module.bundle_identifier(installed)
        )
        self.assertTrue(displaced_generated.is_dir())
        self.assertEqual(trash_count, len(self.trashed))
        build_roots = list(self.applications.glob(".epoch-doc-handler-*"))
        self.assertEqual(1, len(build_roots))
        self.assertEqual(
            "previous",
            (build_roots[0] / "previous.app" / "previous-marker").read_text(),
        )


BUNDLE_ID = "com.stafforini.epoch-doc-handler"


class EpochDocHandlerSourceTests(unittest.TestCase):
    def test_stderr_is_drained_before_waiting_for_helper_exit(self):
        source = (ROOT / "macos/epoch-doc-handler/main.swift").read_text()
        drain = source.index("readDataToEndOfFile()")
        wait = source.index("process.waitUntilExit()")

        self.assertLess(drain, wait)


@unittest.skipUnless(sys.platform == "darwin", "requires macOS")
class EpochDocHandlerCompileTests(unittest.TestCase):
    def test_real_source_compiles_and_signs(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            applications = root / "Applications"
            drive = root / "My Drive"
            drive.mkdir()
            environment = dict(os.environ)
            environment["EPOCH_DOC_LSREGISTER"] = "/usr/bin/true"
            environment["EPOCH_DOC_TRASH"] = "/usr/bin/true"
            result = subprocess.run(
                [
                    sys.executable,
                    str(SCRIPT),
                    "--applications-root",
                    str(applications),
                    "--drive-root",
                    str(drive),
                ],
                env=environment,
                capture_output=True,
                text=True,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            application = applications / "Epoch Document Link.app"
            self.assertTrue(
                application.joinpath(
                    "Contents", "MacOS", "epoch-doc-handler"
                ).is_file()
            )
            verify = subprocess.run(
                [
                    "/usr/bin/codesign",
                    "--verify",
                    "--deep",
                    "--strict",
                    str(application),
                ],
                capture_output=True,
                text=True,
            )
            self.assertEqual(verify.returncode, 0, verify.stderr)


if __name__ == "__main__":
    unittest.main()
