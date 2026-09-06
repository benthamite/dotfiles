"""Static scanner regressions using only owned synthetic bundles/processes."""
import contextlib
import hashlib
import importlib.util
import io
import json
import os
from pathlib import Path
import plistlib
import shutil
import signal
import stat
import subprocess
import sys
import tempfile
import time
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
SKILLS = [ROOT / "macos" / runtime / "skills/audit-mac-app" for runtime in (".codex", ".claude")]
SPEC = importlib.util.spec_from_file_location("mac_app_scanner", SKILLS[0] / "scripts/audit_mac_app.py")
SCANNER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(SCANNER)


def result(status="ok", stdout=b"", stderr=b""):
    return {"status": status, "stdout": stdout, "stderr": stderr,
            "returncode": 0 if status == "ok" else 1}


class StaticScannerTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="mac-app-scanner-", dir=str(Path("/tmp").resolve()))
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name).resolve()
        self.app = self.root / "Owned.app"
        (self.app / "Contents/Resources").mkdir(parents=True)
        (self.app / "Contents/Info.plist").write_bytes(plistlib.dumps({"CFBundleIdentifier": "synthetic.fixture"}))
        self.calls = []

    def runner(self, argv, **kwargs):
        self.calls.append((argv, kwargs))
        return result(stdout=plistlib.dumps({})) if "--entitlements" in argv else result()

    def scan(self, *args, runner=None):
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            code = SCANNER.main([str(self.app), *map(str, args)], runner=runner or self.runner)
        self.assertEqual(code, 0, output.getvalue())
        return json.loads(output.getvalue()), output.getvalue()

    def source(self, name, data):
        path = self.app / "Contents/Resources" / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(data)
        return path

    def test_paired_scanner_and_launchers(self):
        for name in ("audit_mac_app.py", "audit-mac-app.sh"):
            self.assertEqual((SKILLS[0] / "scripts" / name).read_bytes(), (SKILLS[1] / "scripts" / name).read_bytes())

    def test_nonzero_diagnostics_cannot_become_success(self):
        def failed(argv, **kwargs):
            return result("failed", stderr=b"accepted\nvalid on disk\nAuthority=Developer ID Application\nflags=0x10000(runtime)\n")
        report, text = self.scan("--assess-gatekeeper", runner=failed)
        for name in ("signature_details", "signature_verification", "entitlements", "gatekeeper_assessment"):
            self.assertEqual(report["checks"][name]["status"], "failed")
        self.assertNotIn("hardened_runtime_flag", report["checks"]["signature_details"])
        self.assertEqual(report["checks"]["gatekeeper_assessment"]["assessment"], "not_established")
        self.assertNotIn("Authority=", text)

    def test_optional_assessment_is_not_notarization(self):
        report, _text = self.scan()
        self.assertEqual(report["checks"]["gatekeeper_assessment"]["status"], "not_checked")
        self.assertFalse(any("spctl" in call[0][0] for call in self.calls))
        report, _text = self.scan("--assess-gatekeeper")
        self.assertEqual(report["checks"]["gatekeeper_assessment"]["assessment"], "accepted")
        self.assertEqual(report["checks"]["notarization"]["status"], "not_checked")

    def test_typed_entitlements_and_combined_runtime_flags(self):
        values = {"com.apple.security.app-sandbox": False,
                  "com.apple.security.cs.disable-library-validation": False,
                  "com.apple.security.device.microphone": True,
                  "com.apple.security.device.audio-input": "true"}
        def supplied(argv, **kwargs):
            return result(stdout=plistlib.dumps(values)) if "--entitlements" in argv else result(stderr=b"CodeDirectory v=20500 size=400 flags=0x10001(runtime,host) hashes=2+7 location=embedded\n")
        report, _text = self.scan(runner=supplied)
        actual = report["checks"]["entitlements"]["values"]
        self.assertIs(actual["com.apple.security.app-sandbox"], False)
        self.assertIs(actual["com.apple.security.cs.disable-library-validation"], False)
        self.assertIs(actual["com.apple.security.device.microphone"], True)
        self.assertEqual(actual["com.apple.security.device.audio-input"], "non_boolean")
        self.assertIs(report["checks"]["signature_details"]["hardened_runtime_flag"], True)
        self.assertEqual(report["checks"]["tcc_grants"]["status"], "not_checked")

    def test_signature_metadata_requires_unambiguous_structural_records(self):
        actual = b"CodeDirectory v=20500 size=400 flags=0x2(adhoc) hashes=2+7 location=embedded\nSignature=adhoc\n"
        cases = (
            (b"Executable=/fixture/flags=0x10000/Owned.app\n" + actual, False, "ad_hoc"),
            (b"Identifier=fixture\nCodeDirectory v=20500 size=400 flags=0x10000(runtime)\n" + actual, "unknown", "displayed_not_identity_verified"),
            (b"Executable=/fixture/Signature=adhoc.app\nCodeDirectory v=20500 size=400 flags=0x10000(runtime)\n", True, "displayed_not_identity_verified"),
        )
        for displayed, hardened, kind in cases:
            def supplied(argv, **kwargs):
                return result(stdout=plistlib.dumps({})) if "--entitlements" in argv else result(stderr=displayed)
            report, _text = self.scan(runner=supplied)
            details = report["checks"]["signature_details"]
            self.assertEqual(details["hardened_runtime_flag"], hardened)
            self.assertEqual(details["signature_kind"], kind)

    def test_empty_and_malformed_entitlements(self):
        for output, expected in ((b"", "ok"), (b"<plist>broken", "parse_error"), (plistlib.dumps(["not-dict"]), "parse_error")):
            with self.subTest(expected=expected):
                def supplied(argv, **kwargs):
                    return result(stdout=output) if "--entitlements" in argv else result()
                report, _text = self.scan(runner=supplied)
                self.assertEqual(report["checks"]["entitlements"]["status"], expected)

    def test_usage_descriptions_are_not_grants_or_raw_prose(self):
        (self.app / "Contents/Info.plist").write_bytes(plistlib.dumps({"NSCameraUsageDescription": "private fixture description"}))
        report, text = self.scan()
        self.assertEqual(report["checks"]["bundle_info"]["usage_descriptions"], {"NSCameraUsageDescription": "string"})
        self.assertNotIn("private fixture description", text)
        self.assertEqual(report["checks"]["tcc_grants"]["status"], "not_checked")

    def test_dependencies_unpacked_and_loose_code_have_contextual_signals(self):
        content = b'// hypothetical commented setting\nnodeIntegration: true; eval("fixture");'
        for name in ("app/node_modules/vendor/index.js", "app.asar.unpacked/vendor.js", "app/main.js"):
            self.source(name, content)
        report, text = self.scan()
        self.assertEqual(sum(row["kind"] == "node_integration_true_literal" for row in report["signals"]), 3)
        for row in report["signals"]:
            self.assertEqual(row["content_prefix_sha256"], hashlib.sha256(content).hexdigest())
            self.assertEqual(row["prefix_bytes"], len(content))
            self.assertIs(row["whole_file_read"], True)
        self.assertTrue(report["coverage"]["app_asar_unpacked_observed"])
        self.assertNotIn("CRITICAL", text)
        self.assertNotIn("VERDICT", text)

    def test_no_match_does_not_establish_security(self):
        self.source("empty.js", b"const ordinary = 1;")
        report, text = self.scan()
        self.assertEqual(report["signals"], [])
        self.assertNotIn("contextIsolation appears enabled", text)
        self.assertNotIn("LOW RISK", text)
        self.assertEqual(report["checks"]["native_link_inventory"]["status"], "not_checked")

    def test_urls_filenames_and_control_characters_are_not_echoed(self):
        self.source("escape-\x1b[31m-private-filename.js",
                    b'"https://fixture-user:fixture-password@api.openai.com/private-path?token=fixture#hidden" '
                    b'"https://api.openai.com.evil.test/path" "http://localhost/private"')
        report, text = self.scan()
        for forbidden in ("fixture-password", "private-path", "token=fixture", "hidden", "private-filename", "\x1b"):
            self.assertNotIn(forbidden, text)
        hosts = {row["host"]: row["category"] for row in report["endpoint_hosts"]}
        self.assertEqual(hosts["api.openai.com"], "named_service_host_literal")
        self.assertEqual(hosts["api.openai.com.evil.test"], "unclassified_host_literal")
        self.assertEqual(hosts["localhost"], "loopback_literal")

    def test_symlinks_fifo_and_parent_links_are_not_followed(self):
        outside = self.root / "outside.js"
        outside.write_bytes(b"nodeIntegration: true;")
        resources = self.app / "Contents/Resources"
        (resources / "linked.js").symlink_to(outside)
        os.mkfifo(resources / "named-pipe.js")
        report, _text = self.scan()
        self.assertEqual(report["signals"], [])
        self.assertEqual(report["coverage"]["gaps"]["symlink_skipped"], 1)
        self.assertEqual(report["coverage"]["gaps"]["special_file_skipped"], 1)
        alias = self.root / "parent-alias"
        alias.symlink_to(self.root, target_is_directory=True)
        with self.assertRaises(SCANNER.InputError):
            SCANNER.root_directory(alias / "Owned.app")
        with self.assertRaises(SCANNER.InputError):
            SCANNER.root_directory(str(alias / ".." / "Owned.app"))

    def test_extraction_staging_ignores_ambient_tmpdir(self):
        self.source("app.asar", b"synthetic archive")
        temporary = self.root / "safe-staging-fixture"
        temporary.mkdir()
        with mock.patch.dict(os.environ, {"TMPDIR": str(self.root / "My Drive")}), \
                mock.patch.object(SCANNER.tempfile, "mkdtemp", return_value=str(temporary)) as created:
            report, _text = self.scan("--extract-asar", runner=lambda *_args, **_kwargs: result("failed"))
        self.assertEqual(created.call_args.kwargs["dir"], "/private/tmp" if sys.platform == "darwin" else "/tmp")
        self.assertEqual(report["checks"]["asar_extraction"]["retained_directory"], str(temporary))

    def test_unreadable_and_truncated_inputs_are_gaps(self):
        self.source("denied.js", b"nodeIntegration: true;")
        original = SCANNER.os.open
        def opened(path, *args, **kwargs):
            if path == "denied.js":
                raise PermissionError("private error")
            return original(path, *args, **kwargs)
        with mock.patch.object(SCANNER.os, "open", side_effect=opened):
            report, text = self.scan()
        self.assertEqual(report["coverage"]["gaps"]["unreadable_or_changed_entry"], 1)
        self.assertNotIn("private error", text)
        self.source("large.js", b" " * 1000 + b"nodeIntegration: true;")
        with mock.patch.object(SCANNER, "MAX_FILE_BYTES", 100):
            report, _text = self.scan()
        self.assertGreater(report["coverage"]["gaps"]["file_prefix_only"], 0)

    def test_output_and_inventory_limits_are_explicit(self):
        self.source("many.js", b'nodeIntegration: true; eval("x"); "https://one.test/" "https://two.test/"')
        with mock.patch.object(SCANNER, "MAX_SIGNALS", 1), mock.patch.object(SCANNER, "MAX_HOSTS", 1):
            report, _text = self.scan()
        self.assertEqual(len(report["signals"]), 1)
        self.assertEqual(len(report["endpoint_hosts"]), 1)
        self.assertGreater(report["coverage"]["gaps"]["signal_output_limit"], 0)
        self.assertGreaterEqual(report["coverage"]["gaps"]["endpoint_output_limit"], 1)
        with mock.patch.object(SCANNER, "MAX_FILES", 1):
            report, _text = self.scan()
        self.assertGreater(report["coverage"]["gaps"]["file_limit"], 0)

    def test_native_inspector_receives_opened_regular_file(self):
        self.source("helper.bin", b"\xcf\xfa\xed\xfe" + b"LaunchAgent http://native.test/")
        def supplied(argv, **kwargs):
            if argv[0] == "/usr/bin/otool":
                descriptor, = kwargs["pass_fds"]
                self.assertTrue(stat.S_ISREG(os.fstat(descriptor).st_mode))
                self.assertEqual(argv[-1], "/dev/fd/" + str(descriptor))
                return result("failed", stdout=b"/PrivateFrameworks/private")
            return self.runner(argv, **kwargs)
        report, _text = self.scan(runner=supplied)
        native = report["checks"]["native_link_inventory"]
        self.assertEqual(native["status"], "partial")
        self.assertEqual(native["files"][0]["status"], "failed")
        self.assertNotIn("private_framework_reference", native["files"][0])

    def test_asar_opt_in_failure_has_no_host_fallback(self):
        self.source("app.asar", b"synthetic invalid archive")
        report, _text = self.scan()
        self.assertEqual(report["checks"]["asar_extraction"]["status"], "not_checked")
        self.assertFalse(any("extract-asar" in call[0][0] for call in self.calls))
        temporary = self.root / "owned-extract-temp"
        temporary.mkdir()
        def supplied(argv, **kwargs):
            if argv[0].endswith("extract-asar.sh"):
                self.calls.append((argv, kwargs))
                return result("failed", stderr=b"private extraction failure")
            return self.runner(argv, **kwargs)
        with mock.patch.object(SCANNER.tempfile, "mkdtemp", return_value=str(temporary)), \
                mock.patch.object(SCANNER.shutil, "which", return_value="/owned-fixture-trash") as trash_lookup:
            report, text = self.scan("--extract-asar", runner=supplied)
        trash_lookup.assert_not_called()
        self.assertEqual(report["checks"]["asar_extraction"]["status"], "failed")
        self.assertEqual(report["checks"]["asar_extraction"]["retained_directory"], str(temporary))
        self.assertNotIn("temporary_cleanup", report["checks"])
        self.assertTrue(temporary.is_dir())
        self.assertNotIn("private extraction failure", text)
        self.assertEqual(sum(call[0][0].endswith("extract-asar.sh") for call in self.calls), 1)

    def test_cleanup_requires_owned_path_absence(self):
        self.source("app.asar", b"synthetic archive")
        temporary = self.root / "retained-after-zero-cleanup"
        temporary.mkdir()
        def supplied(argv, **kwargs):
            if argv[0].endswith("extract-asar.sh"):
                Path(argv[-1]).mkdir()
                return result()
            return self.runner(argv, **kwargs)
        with mock.patch.object(SCANNER.tempfile, "mkdtemp", return_value=str(temporary)), \
                mock.patch.object(SCANNER.shutil, "which", return_value="/owned-fixture-trash"):
            report, _text = self.scan("--extract-asar", runner=supplied)
        cleanup = report["checks"]["temporary_cleanup"]
        self.assertEqual(cleanup["returncode"], 0)
        self.assertEqual(cleanup["status"], "cleanup_unconfirmed")
        self.assertEqual(cleanup["retained_directory"], str(temporary))
        self.assertTrue(temporary.is_dir())

    def test_native_trash_removes_only_owned_extraction_root(self):
        trash = shutil.which("trash")
        if not trash:
            self.skipTest("native trash utility unavailable")
        self.source("app.asar", b"synthetic archive")
        temporary = self.root / "native-trash-owned-root"
        temporary.mkdir()
        neighbor = self.root / "retained-neighbor"
        neighbor.mkdir()
        def supplied(argv, **kwargs):
            if argv[0].endswith("extract-asar.sh"):
                Path(argv[-1]).mkdir()
                return result()
            if argv[0] == trash:
                self.assertEqual(argv, [trash, str(temporary)])
                return SCANNER.tool(argv, **kwargs)
            return self.runner(argv, **kwargs)
        with mock.patch.object(SCANNER.tempfile, "mkdtemp", return_value=str(temporary)):
            report, _text = self.scan("--extract-asar", runner=supplied)
        self.assertEqual(report["checks"]["temporary_cleanup"], {"status": "ok", "returncode": 0})
        self.assertFalse(os.path.lexists(temporary))
        self.assertTrue(neighbor.is_dir())

    def test_supplied_source_is_unbound_and_preserved(self):
        extracted = self.root / "supplied"
        extracted.mkdir()
        source = extracted / "source.js"
        source.write_bytes(b"nodeIntegration: true;")
        report, _text = self.scan("--extracted-root", extracted)
        self.assertEqual(report["checks"]["asar_extraction"]["status"], "supplied_unbound")
        self.assertEqual(report["signals"][0]["source"], "supplied_source")
        self.assertEqual(source.read_bytes(), b"nodeIntegration: true;")

    def test_public_cli_refuses_symlink_input(self):
        alias = self.root / "Alias.app"
        alias.symlink_to(self.app, target_is_directory=True)
        for skill in SKILLS:
            checked = subprocess.run([str(skill / "scripts/audit-mac-app.sh"), str(alias)], capture_output=True, text=True, timeout=10)
            self.assertEqual(checked.returncode, 2, checked.stderr)
            self.assertEqual(json.loads(checked.stdout)["status"], "input_error")

    def test_trusted_tmp_alias_resolves_without_accepting_artifact_links(self):
        alias = Path("/tmp") / self.root.relative_to(Path("/tmp").resolve()) / "Owned.app"
        _path, descriptor = SCANNER.root_directory(alias)
        try:
            self.assertEqual(os.fstat(descriptor).st_ino, self.app.stat().st_ino)
        finally:
            os.close(descriptor)

    def test_extraction_gets_new_time_window_and_retains_uncertain_output(self):
        self.source("app.asar", b"synthetic invalid archive")
        for status in ("ok", "timeout"):
            with self.subTest(status=status):
                temporary = self.root / ("owned-extraction-" + status)
                temporary.mkdir()
                clock = [0]
                def supplied(argv, **kwargs):
                    if argv[0].endswith("extract-asar.sh"):
                        self.assertEqual(kwargs, {"seconds": 360, "grace": 45})
                        clock[0] = 200
                        output = Path(argv[-1])
                        output.mkdir()
                        (output / "owned.js").write_bytes(b"nodeIntegration: true;")
                        return result(status)
                    return self.runner(argv, **kwargs)
                with mock.patch.object(SCANNER.tempfile, "mkdtemp", return_value=str(temporary)), \
                        mock.patch.object(SCANNER.shutil, "which", return_value=None), \
                        mock.patch.object(SCANNER.time, "monotonic", side_effect=lambda: clock[0]):
                    report, _text = self.scan("--extract-asar", runner=supplied)
                if status == "ok":
                    self.assertTrue(any(row["source"] == "isolated_extraction" for row in report["signals"]))
                    self.assertNotIn("inventory_limit", report["coverage"]["gaps"])
                else:
                    self.assertEqual(report["checks"]["asar_extraction"]["retained_directory"], str(temporary))
                    self.assertNotIn("temporary_cleanup", report["checks"])
                self.assertTrue(temporary.is_dir())

    def test_interrupted_extraction_reports_retained_directory(self):
        self.source("app.asar", b"synthetic archive")
        temporary = self.root / "interrupted-extraction"
        temporary.mkdir()
        def supplied(argv, **kwargs):
            if argv[0].endswith("extract-asar.sh"):
                raise OSError("private interrupted operation")
            return self.runner(argv, **kwargs)
        output = io.StringIO()
        with mock.patch.object(SCANNER.tempfile, "mkdtemp", return_value=str(temporary)), contextlib.redirect_stdout(output):
            code = SCANNER.main([str(self.app), "--extract-asar"], runner=supplied)
        self.assertEqual(code, 2)
        self.assertEqual(json.loads(output.getvalue())["retained_directory"], str(temporary))
        self.assertNotIn("private interrupted operation", output.getvalue())

    def test_missing_bundle_structure_refuses_before_inspection(self):
        invalid = self.root / "Not-a-bundle.app"
        invalid.mkdir()
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            code = SCANNER.main([str(invalid)], runner=self.runner)
        self.assertEqual(code, 2)
        self.assertEqual(self.calls, [])

    def test_public_entry_sigterm_cleans_owned_child_and_reports_retained_staging(self):
        trash = shutil.which("trash")
        if not trash:
            self.skipTest("native trash utility unavailable")
        self.source("app.asar", b"synthetic archive")
        scripts = self.root / "owned-public-entry"
        scripts.mkdir()
        for name in ("audit-mac-app.sh", "audit_mac_app.py"):
            (scripts / name).write_bytes((SKILLS[0] / "scripts" / name).read_bytes())
        wrapper = scripts / "audit-mac-app.sh"
        wrapper.chmod(0o700)
        marker = self.root / "owned-extractor-ready.json"
        stopped = self.root / "owned-extractor-stopped"
        helper = scripts / "extract-asar.sh"
        helper.write_text('#!/bin/bash\nexec /usr/bin/python3 -I -B "$0.py" "$@"\n')
        helper.chmod(0o700)
        (scripts / "extract-asar.sh.py").write_text(
            "import json,os,signal,sys,time\nfrom pathlib import Path\n"
            "def stop(*_args):\n"
            f"    Path({str(stopped)!r}).write_text('terminated')\n"
            "    raise SystemExit(130)\n"
            "signal.signal(signal.SIGTERM,stop)\n"
            "output=Path(sys.argv[-1]); output.mkdir()\n"
            f"Path({str(marker)!r}).write_text(json.dumps({{'pid':os.getpid(),'staging':str(output.parent)}}))\n"
            "time.sleep(30)\n")
        neighbor = subprocess.Popen([sys.executable, "-I", "-B", "-c", "import time; time.sleep(30)"])
        running = subprocess.Popen([str(wrapper), str(self.app), "--extract-asar"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, start_new_session=True)
        try:
            deadline = time.monotonic() + 10
            while not marker.exists() and running.poll() is None and time.monotonic() < deadline:
                time.sleep(0.02)
            self.assertTrue(marker.is_file(), "owned extractor never reached readiness")
            ready = json.loads(marker.read_text())
            running.send_signal(signal.SIGTERM)
            stdout, stderr = running.communicate(timeout=10)
            self.assertEqual(running.returncode, 130, stderr)
            report = json.loads(stdout)
            self.assertEqual(report["status"], "interrupted")
            self.assertEqual(report["retained_directory"], ready["staging"])
            self.assertEqual(stopped.read_text(), "terminated")
            with self.assertRaises(ProcessLookupError):
                os.kill(ready["pid"], 0)
            self.assertIsNone(neighbor.poll())
        finally:
            if running.poll() is None:
                running.terminate()
            running.communicate(timeout=10)
            neighbor.terminate()
            neighbor.wait(timeout=5)
            if marker.is_file():
                owned_staging = Path(json.loads(marker.read_text())["staging"])
                self.assertEqual(owned_staging.parent, Path("/private/tmp" if sys.platform == "darwin" else "/tmp"))
                self.assertTrue(owned_staging.name.startswith("audit-mac-app-"))
                cleanup = SCANNER.tool([trash, str(owned_staging)])
                self.assertEqual(cleanup["status"], "ok")
                self.assertFalse(os.path.lexists(owned_staging))


class ToolLifetimeTests(unittest.TestCase):
    def test_graceful_timeout_allows_owned_cleanup_and_reports_failed_grace(self):
        command = "import os,signal,time; signal.signal(signal.SIGTERM,lambda *_: exit(77)); print(os.getpid(),flush=True); time.sleep(5)"
        checked = SCANNER.tool([sys.executable, "-B", "-c", command], seconds=0.3, grace=1)
        self.assertEqual(checked["status"], "timeout")
        self.assertEqual(checked["returncode"], 77)
        with self.assertRaises(ProcessLookupError):
            os.kill(int(checked["stdout"].strip()), 0)
        command = "import os,signal,time; signal.signal(signal.SIGTERM,signal.SIG_IGN); print(os.getpid(),flush=True); time.sleep(5)"
        checked = SCANNER.tool([sys.executable, "-B", "-c", command], seconds=0.3, grace=0.1)
        self.assertEqual(checked["status"], "cleanup_unconfirmed")
        with self.assertRaises(ProcessLookupError):
            os.kill(int(checked["stdout"].strip()), 0)

    def test_native_status_and_bounded_output(self):
        checked = SCANNER.tool([sys.executable, "-B", "-c", "print('accepted'); raise SystemExit(7)"])
        self.assertEqual(checked["status"], "failed")
        self.assertEqual(checked["returncode"], 7)
        self.assertEqual(checked["stdout"], b"accepted\n")
        with mock.patch.object(SCANNER, "MAX_TOOL_BYTES", 100):
            checked = SCANNER.tool([sys.executable, "-B", "-c", "import os,time; os.write(1,b'x'*10000); time.sleep(5)"])
        self.assertEqual(checked["status"], "output_limit")
        self.assertEqual(len(checked["stdout"]), 100)

    def test_native_deadline_reaps_new_child(self):
        started = time.monotonic()
        checked = SCANNER.tool([sys.executable, "-B", "-c", "import os,time; print(os.getpid(),flush=True); time.sleep(5)"], seconds=0.3)
        self.assertEqual(checked["status"], "timeout")
        self.assertLess(time.monotonic() - started, 3)
        with self.assertRaises(ProcessLookupError):
            os.kill(int(checked["stdout"].strip()), 0)

    def test_missing_tool_is_unavailable(self):
        checked = SCANNER.tool(["/nonexistent/owned-fixture-tool"])
        self.assertEqual(checked["status"], "unavailable")
        self.assertIsNone(checked["returncode"])


if __name__ == "__main__":
    unittest.main()
