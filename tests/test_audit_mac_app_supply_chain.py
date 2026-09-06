"""Synthetic extractor checks; live Docker acceptance is explicitly opt-in.

AUDIT_MAC_APP_LIVE_TESTS=1 enables the real archive/VM test. It requires an
already provisioned pinned image and a matching existing extractor cache: the
test never starts Docker, pulls an image, or downloads dependencies.
"""

from __future__ import annotations

import contextlib
import errno
import fcntl
import filecmp
import hashlib
import importlib.util
import io
import json
import os
from pathlib import Path
import struct
import subprocess
import sys
import tempfile
import time
import unittest
from unittest import mock


DOTFILES = Path(__file__).resolve().parents[1]
SKILL_DIRS = tuple(DOTFILES / f"macos/.{runtime}/skills/audit-mac-app" for runtime in ("claude", "codex"))


def extractor(skill):
    spec = importlib.util.spec_from_file_location("test_asar_helper", skill / "scripts/extract-asar.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def relative_files(root):
    return {path.relative_to(root) for path in root.rglob("*")
            if path.is_file() and "node_modules" not in path.parts and "__pycache__" not in path.parts}


def fake_runtime(root, body=None):
    runtime = root / "runtime"
    runtime.mkdir()
    node = runtime / "node"
    node.write_text("#!/bin/sh\nprintf 'node\\n' >>\"$ASAR_TEST_LOG\"\nexit 0\n")
    node.chmod(0o700)
    npm = runtime / "npm"
    npm.write_text("#!/opt/homebrew/bin/python3 -I\n" + (body or '''
import json, os, pathlib, sys
with open(os.environ["ASAR_TEST_LOG"], "a") as log:
    log.write(json.dumps({"argv": sys.argv[1:], "cwd": os.getcwd()}) + "\\n")
assert sys.argv[1:5] == ["ci", "--ignore-scripts", "--no-audit", "--no-fund"]
assert sys.argv[5] == "--cache"
assert pathlib.Path("package.json").is_file()
assert pathlib.Path("package-lock.json").is_file()
module = pathlib.Path("node_modules/@electron/asar")
(module / "bin").mkdir(parents=True)
(module / "package.json").write_text('{"version":"4.2.1"}')
(module / "bin/asar.mjs").write_text('import{writeFileSync}from"node:fs";writeFileSync("/workspace/package.json","{}");')
'''))
    npm.chmod(0o700)
    return {"AUDIT_MAC_APP_NODE": str(node), "AUDIT_MAC_APP_CACHE_DIR": str(root / "cache"),
            "ASAR_TEST_LOG": str(root / "bootstrap.log")}


class AuditMacAppSupplyChainTests(unittest.TestCase):
    def test_preflight_refuses_all_bad_grants_before_bootstrap(self):
        for skill in SKILL_DIRS:
            with self.subTest(skill=skill), tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
                root = Path(temp)
                archive = root / "app.asar"
                archive.touch()
                ordinary = root / "ordinary"
                ordinary.mkdir()
                (ordinary / "app.asar").touch()
                link = root / "parent-link"
                link.symlink_to(ordinary)
                root_link = root / "archive-link"
                root_link.symlink_to(archive)
                dangling = root / "dangling"
                dangling.symlink_to(root / "missing")
                fifo = root / "fifo"
                os.mkfifo(fifo)
                environment = dict(os.environ, **fake_runtime(root))
                default_cache = root / "cache"
                cases = [(root_link, root / "output", default_cache),
                         (link / "app.asar", root / "output", default_cache),
                         (ordinary, root / "output", default_cache),
                         (fifo, root / "output", default_cache),
                         (archive, root / "missing/output", default_cache),
                         (archive, ordinary, default_cache),
                         (archive, dangling, default_cache),
                         (archive, link / "output", default_cache),
                         (archive, root / "output", link / "cache"),
                         (archive, root / "output", root / ".codex/cache"),
                         (archive, root / "output,comma", default_cache),
                         (archive, root / "output\nnewline", default_cache),
                         (ordinary / "../app.asar", root / "output", default_cache)]
                for source, destination, cache in cases:
                    with self.subTest(source=source, destination=destination, cache=cache):
                        environment["AUDIT_MAC_APP_CACHE_DIR"] = str(cache)
                        result = subprocess.run([str(skill / "scripts/extract-asar.sh"), str(source), str(destination)],
                                                env=environment, capture_output=True, text=True, timeout=10)
                        self.assertEqual(result.returncode, 2, result.stderr)
                        self.assertFalse((root / "bootstrap.log").exists(), result.stderr)
                        self.assertFalse(default_cache.exists(), result.stderr)
                for kind in ("symlink", "file", "fifo"):
                    unpacked = Path(str(archive) + ".unpacked")
                    if kind == "symlink":
                        unpacked.symlink_to(ordinary)
                    elif kind == "fifo":
                        os.mkfifo(unpacked)
                    else:
                        unpacked.touch()
                    environment["AUDIT_MAC_APP_CACHE_DIR"] = str(default_cache)
                    result = subprocess.run([str(skill / "scripts/extract-asar.sh"), str(archive), str(root / "output")],
                                            env=environment, capture_output=True, text=True, timeout=10)
                    self.assertEqual(result.returncode, 2, result.stderr)
                    self.assertFalse((root / "bootstrap.log").exists())
                    unpacked.unlink()

    def test_drive_grants_are_refused_in_read_only_preflight(self):
        # Exercise only preflight here: a regressed implementation must not be
        # allowed to create a real Drive cache even when a regression test fails.
        for skill in SKILL_DIRS:
            with self.subTest(skill=skill), tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
                root = Path(temp)
                archive = root / "app.asar"
                archive.touch()
                module = extractor(skill)
                drive = Path.home() / "My Drive"
                variants = [drive]
                case_alias = drive.with_name("my drive")
                if case_alias.exists() and case_alias.samefile(drive):
                    variants.append(case_alias)
                for variant in variants:
                    with mock.patch.dict(os.environ, {"AUDIT_MAC_APP_CACHE_DIR": str(root / "cache")}):
                        with self.assertRaisesRegex(ValueError, "outside Google Drive"):
                            module.preflight(str(archive), str(variant / "asar-audit-must-not-exist"))
                    with mock.patch.dict(os.environ, {"AUDIT_MAC_APP_CACHE_DIR": str(variant / "asar-cache-must-not-exist")}):
                        with self.assertRaisesRegex(ValueError, "outside Google Drive"):
                            module.preflight(str(archive), str(root / "output"))

    def test_cold_cache_install_uses_staging_and_vm_argv_then_reuses_cache(self):
        for skill in SKILL_DIRS:
            with self.subTest(skill=skill), tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
                root = Path(temp)
                archive = root / "app.asar"
                archive.touch()
                module = extractor(skill)
                environment = fake_runtime(root)
                with mock.patch.dict(os.environ, environment), mock.patch.object(module.os, "execv") as launch:
                    module.run(str(archive), str(root / "output"))
                    called, argv = launch.call_args.args
                    self.assertEqual(called, str(DOTFILES / "bin/untrusted-run"))
                    self.assertEqual(argv[-5:], ["node", "/inputs/dependencies/" + str(module.CLI),
                                                "extract", "/inputs/archive.asar", "/workspace"])
                    self.assertEqual(argv[1:3], ["--workspace", str(root / "output")])
                    before = (root / "bootstrap.log").read_bytes()
                    module.run(str(archive), str(root / "output-again"))
                    self.assertEqual((root / "bootstrap.log").read_bytes(), before)
                record = json.loads(before.decode().splitlines()[1])
                self.assertEqual(record["argv"], ["ci", "--ignore-scripts", "--no-audit", "--no-fund", "--cache", str(root / "cache/npm-cache")])
                self.assertTrue(Path(record["cwd"]).name.startswith(".install."))
                self.assertEqual([path for path in (root / "cache").glob(".install.*")
                                  if path.name != ".install.lock"], [])
                install = root / "cache" / hashlib.sha256((skill / "package-lock.json").read_bytes()).hexdigest()
                self.assertTrue((install / module.CLI).is_file())
                self.assertFalse((root / "output").exists(), "only the real isolator may create the output")

    def test_cache_symlink_incomplete_mode_and_lock_refusals(self):
        for skill in SKILL_DIRS:
            for kind in ("install-symlink", "incomplete", "public", "lock-symlink", "lock-fifo", "busy",
                         "metadata-list", "metadata-null", "metadata-string", "wrong-version", "mismatched-lock",
                         "cli-symlink", "npm-cache-symlink", "lock-hardlink"):
                with self.subTest(skill=skill, kind=kind), tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
                    root = Path(temp)
                    archive = root / "app.asar"
                    archive.touch()
                    environment = dict(os.environ, **fake_runtime(root))
                    cache = root / "cache"
                    cache.mkdir(mode=0o700)
                    install = cache / hashlib.sha256((skill / "package-lock.json").read_bytes()).hexdigest()
                    lock = None
                    if kind == "install-symlink":
                        install.symlink_to(root)
                    elif kind == "incomplete":
                        install.mkdir(mode=0o700)
                    elif kind == "public":
                        cache.chmod(0o755)
                    elif kind == "lock-symlink":
                        (cache / ".install.lock").symlink_to(archive)
                    elif kind == "lock-fifo":
                        os.mkfifo(cache / ".install.lock")
                    elif kind == "lock-hardlink":
                        os.link(archive, cache / ".install.lock")
                    elif kind == "npm-cache-symlink":
                        (cache / "npm-cache").symlink_to(root)
                    elif kind == "busy":
                        lock = (cache / ".install.lock").open("w")
                        (cache / ".install.lock").chmod(0o600)
                        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
                    else:
                        install.mkdir(mode=0o700)
                        for name in ("package.json", "package-lock.json"):
                            (install / name).write_bytes((skill / name).read_bytes())
                        package = install / "node_modules/@electron/asar"
                        (package / "bin").mkdir(parents=True)
                        executable = package / "bin/asar.mjs"
                        executable.touch()
                        metadata = {"metadata-list": [], "metadata-null": None,
                                    "metadata-string": "4.2.1", "wrong-version": {"version": "0.0.0"}}.get(kind, {"version": "4.2.1"})
                        (package / "package.json").write_text(json.dumps(metadata))
                        if kind == "cli-symlink":
                            executable.unlink()
                            executable.symlink_to(archive)
                        elif kind == "mismatched-lock":
                            (install / "package-lock.json").write_text("{}")
                    try:
                        result = subprocess.run([str(skill / "scripts/extract-asar.sh"), str(archive), str(root / "output")],
                                                env=environment, capture_output=True, text=True, timeout=10)
                        self.assertEqual(result.returncode, 2, result.stderr)
                        self.assertFalse((root / "bootstrap.log").exists())
                        self.assertFalse((root / "output").exists())
                    finally:
                        if lock:
                            lock.close()

    def test_input_changes_during_bootstrap_prevent_vm_dispatch(self):
        for skill in SKILL_DIRS:
            for change in ("archive", "unpacked-added", "unpacked-replaced", "unpacked-removed"):
                with self.subTest(skill=skill, change=change), tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
                    root = Path(temp)
                    archive = root / "app.asar"
                    archive.write_bytes(b"before")
                    unpacked = root / "app.asar.unpacked"
                    if change in ("unpacked-replaced", "unpacked-removed"):
                        unpacked.mkdir()
                    environment = fake_runtime(root)
                    npm = root / "runtime/npm"
                    actions = {"archive": "archive.write_bytes(b'changed')",
                               "unpacked-added": "unpacked.mkdir()",
                               "unpacked-replaced": "unpacked.rename(unpacked.with_name('old-unpacked')); unpacked.mkdir()",
                               "unpacked-removed": "unpacked.rmdir()"}
                    npm.write_text(npm.read_text() + f"\narchive = pathlib.Path({str(archive)!r})\nunpacked = pathlib.Path({str(unpacked)!r})\n" + actions[change] + "\n")
                    module = extractor(skill)
                    with mock.patch.dict(os.environ, environment), mock.patch.object(module.os, "execv") as launch:
                        with self.assertRaisesRegex(ValueError, "changed during preparation"):
                            module.run(str(archive), str(root / "output"))
                        launch.assert_not_called()
                    self.assertFalse((root / "output").exists())

    def test_atomic_publish_never_overwrites_or_nests_in_concurrent_destination(self):
        module = extractor(SKILL_DIRS[0])
        for occupied in (False, True):
            with self.subTest(occupied=occupied), tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
                root = Path(temp)
                staging = root / "staging"
                staging.mkdir()
                (staging / "owned").write_text("owned")
                destination = root / "install"
                destination.mkdir()
                if occupied:
                    (destination / "foreign").write_text("foreign")
                with self.assertRaises(OSError) as raised:
                    module.publish(staging, destination)
                self.assertEqual(raised.exception.errno, errno.EEXIST)
                self.assertEqual((staging / "owned").read_text(), "owned")
                self.assertEqual(sorted(path.name for path in destination.iterdir()), ["foreign"] if occupied else [])
                fresh = root / "fresh"
                module.publish(staging, fresh)
                self.assertFalse(staging.exists())
                self.assertEqual((fresh / "owned").read_text(), "owned")

    def test_failed_bootstrap_uses_trash_and_never_creates_destination(self):
        module = extractor(SKILL_DIRS[0])
        with tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
            root = Path(temp)
            archive = root / "app.asar"
            archive.touch()
            environment = fake_runtime(root, "import sys\nsys.exit(9)\n")
            with mock.patch.dict(os.environ, environment), mock.patch.object(module.subprocess, "run", wraps=subprocess.run) as calls:
                with self.assertRaisesRegex(ValueError, "npm status 9"):
                    module.run(str(archive), str(root / "output"))
                trash_calls = [call.args[0] for call in calls.call_args_list if Path(call.args[0][0]).name == "trash"]
                self.assertEqual(len(trash_calls), 1)
                self.assertFalse(Path(trash_calls[0][1]).exists())
            self.assertFalse((root / "output").exists())

    def test_missing_and_failed_trash_are_not_replaced_by_recursive_deletion(self):
        module = extractor(SKILL_DIRS[0])
        with tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
            root = Path(temp)
            archive = root / "app.asar"
            archive.touch()
            environment = fake_runtime(root, "import sys\nsys.exit(9)\n")
            with mock.patch.dict(os.environ, environment), mock.patch.object(module.shutil, "which", return_value=None):
                with self.assertRaisesRegex(ValueError, "trash executable"):
                    module.run(str(archive), str(root / "output"))
                self.assertFalse((root / "bootstrap.log").exists())
            actual_run = subprocess.run
            def failed_trash(argv, **kwargs):
                if Path(argv[0]).name == "trash":
                    return subprocess.CompletedProcess(argv, 1)
                return actual_run(argv, **kwargs)
            diagnostics = io.StringIO()
            with mock.patch.dict(os.environ, environment), mock.patch.object(module.subprocess, "run", side_effect=failed_trash), contextlib.redirect_stderr(diagnostics):
                with self.assertRaisesRegex(ValueError, "npm status 9"):
                    module.run(str(archive), str(root / "output"))
            retained = [path for path in (root / "cache").glob(".install.*") if path.is_dir()]
            self.assertEqual(len(retained), 1)
            self.assertTrue((retained[0] / "package-lock.json").is_file())
            self.assertIn("staging cleanup unconfirmed; retained path " + str(retained[0]), diagnostics.getvalue())

    def test_bootstrap_timeout_kills_and_reaps_owned_process(self):
        module = extractor(SKILL_DIRS[0])
        with tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
            root = Path(temp)
            body = "import os, pathlib, time\npathlib.Path(os.environ['ASAR_TEST_LOG']).write_text(str(os.getpid()))\ntime.sleep(30)\n"
            environment = fake_runtime(root, body)
            staging = root / "staging"
            staging.mkdir(mode=0o700)
            cache = root / "cache"
            cache.mkdir(mode=0o700)
            started = time.monotonic()
            with mock.patch.dict(os.environ, environment), mock.patch.object(module, "BOOTSTRAP_TIMEOUT", 0.15):
                with self.assertRaises(subprocess.TimeoutExpired):
                    module.bootstrap(Path(environment["AUDIT_MAC_APP_NODE"]), staging, cache)
            self.assertLess(time.monotonic() - started, 3)
            pid = int((root / "bootstrap.log").read_text())
            with self.assertRaises(ProcessLookupError):
                os.kill(pid, 0)

    def test_concurrent_installer_fails_busy_without_second_bootstrap(self):
        for skill in SKILL_DIRS:
            with self.subTest(skill=skill), tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
                root = Path(temp)
                archive = root / "app.asar"
                archive.touch()
                environment = dict(os.environ, **fake_runtime(root))
                npm = root / "runtime/npm"
                npm.write_text(npm.read_text().replace("assert sys.argv[1:5]", "pathlib.Path(os.environ['ASAR_TEST_LOG'] + '.ready').touch()\n"
                    "import time\nwhile not pathlib.Path(os.environ['ASAR_TEST_LOG'] + '.release').exists(): time.sleep(0.01)\n"
                    "assert sys.argv[1:5]"))
                # Only the final VM dispatch is replaced. The bootstrap and
                # contender are independent real OS processes with real locks.
                launch = "import os,runpy,sys; ns=runpy.run_path(sys.argv.pop(1)); os.execv=lambda *args: None; sys.exit(ns['main']())"
                first = subprocess.Popen([sys.executable, "-B", "-c", launch, str(skill / "scripts/extract-asar.py"), str(archive), str(root / "output")],
                                         env=environment, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
                try:
                    deadline = time.monotonic() + 5
                    while not (root / "bootstrap.log.ready").exists() and time.monotonic() < deadline:
                        time.sleep(0.01)
                    self.assertTrue((root / "bootstrap.log.ready").exists())
                    contender = subprocess.run([str(skill / "scripts/extract-asar.sh"), str(archive), str(root / "output-2")],
                                               env=environment, capture_output=True, text=True, timeout=5)
                    self.assertEqual(contender.returncode, 2, contender.stderr)
                    self.assertIn("installation is busy", contender.stderr)
                finally:
                    (root / "bootstrap.log.release").touch()
                    stdout, stderr = first.communicate(timeout=10)
                self.assertEqual(first.returncode, 0, stderr)
                self.assertEqual(len((root / "bootstrap.log").read_text().splitlines()), 2)
                self.assertFalse((root / "output").exists())
                self.assertFalse((root / "output-2").exists())

    def test_term_cleans_owned_bootstrap_and_leaves_neighbor_alive(self):
        for skill in SKILL_DIRS:
            with self.subTest(skill=skill), tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
                root = Path(temp)
                archive = root / "app.asar"
                archive.touch()
                body = "import os, pathlib, time\npathlib.Path(os.environ['ASAR_TEST_LOG']).write_text(str(os.getpid()))\ntime.sleep(30)\n"
                environment = dict(os.environ, **fake_runtime(root, body))
                neighbor = subprocess.Popen([sys.executable, "-I", "-c", "import time; time.sleep(30)"])
                worker = subprocess.Popen([str(skill / "scripts/extract-asar.sh"), str(archive), str(root / "output")],
                                          env=environment, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
                try:
                    deadline = time.monotonic() + 5
                    while time.monotonic() < deadline:
                        marker = root / "bootstrap.log"
                        if marker.exists() and marker.read_text().isdigit():
                            break
                        time.sleep(0.01)
                    self.assertTrue(marker.read_text().isdigit())
                    pid = int(marker.read_text())
                    worker.terminate()
                    stdout, stderr = worker.communicate(timeout=10)
                    self.assertEqual(worker.returncode, 130, stderr)
                    with self.assertRaises(ProcessLookupError):
                        os.kill(pid, 0)
                    self.assertIsNone(neighbor.poll())
                    self.assertEqual([path for path in (root / "cache").glob(".install.*") if path.is_dir()], [])
                    self.assertFalse((root / "output").exists())
                finally:
                    if worker.poll() is None:
                        worker.terminate()
                    worker.communicate(timeout=10)
                    neighbor.terminate()
                    neighbor.wait(timeout=5)

    @unittest.skipUnless(os.environ.get("AUDIT_MAC_APP_LIVE_TESTS") == "1", "explicit live Docker acceptance only")
    def test_real_archive_relative_paths_and_unpacked_inputs(self):
        for skill in SKILL_DIRS:
            lock_hash = hashlib.sha256((skill / "package-lock.json").read_bytes()).hexdigest()
            install = Path.home() / ".cache/audit-mac-app/asar" / lock_hash
            extractor(skill).valid_install(install)
        with tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
            root = Path(temp)
            contents = b'{"name":"synthetic-isolation-fixture"}\n'
            unpacked_contents = b"synthetic unpacked asset\n"
            header = json.dumps({"files": {"package.json": {"size": len(contents), "offset": "0"},
                                           "asset.txt": {"size": len(unpacked_contents), "unpacked": True}}}).encode()
            padded = header + b"\0" * (-len(header) % 4)
            (root / "app.asar").write_bytes(struct.pack("<IIII", 4, 8 + len(padded), 4 + len(padded), len(header)) + padded + contents)
            unpacked = root / "app.asar.unpacked"
            unpacked.mkdir()
            (unpacked / "asset.txt").write_bytes(unpacked_contents)
            for index, skill in enumerate(SKILL_DIRS):
                result = subprocess.run([str(skill / "scripts/extract-asar.sh"), "app.asar", f"output-{index}"],
                                        cwd=root, capture_output=True, text=True, timeout=180)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual((root / f"output-{index}/package.json").read_bytes(), contents)
                self.assertEqual((root / f"output-{index}/asset.txt").read_bytes(), unpacked_contents)
            self.assertEqual((unpacked / "asset.txt").read_bytes(), unpacked_contents)

    def test_mirrored_skill_trees_are_byte_identical(self):
        claude, codex = SKILL_DIRS
        self.assertEqual(relative_files(claude), relative_files(codex))
        for relative in sorted(relative_files(claude)):
            with self.subTest(path=relative):
                self.assertTrue(filecmp.cmp(claude / relative, codex / relative, shallow=False))

    def test_extractor_is_exactly_locked_and_never_uses_npx(self):
        for skill in SKILL_DIRS:
            package = json.loads((skill / "package.json").read_text())
            self.assertEqual(package["dependencies"], {"@electron/asar": "4.2.1"})
            lock = json.loads((skill / "package-lock.json").read_text())
            self.assertEqual(lock["packages"]["node_modules/@electron/asar"]["version"], "4.2.1")
            self.assertIn("integrity", lock["packages"]["node_modules/@electron/asar"])
            self.assertTrue((skill / "scripts/extract-asar.sh").stat().st_mode & 0o100)
            for relative in relative_files(skill):
                if relative.suffix in {".md", ".sh", ".py"}:
                    self.assertNotRegex((skill / relative).read_text(), r"\bnpx\b[^\n]*\basar\b")

    def test_brace_expansion_has_dos_fix(self):
        for skill in SKILL_DIRS:
            lock = json.loads((skill / "package-lock.json").read_text())
            version = lock["packages"]["node_modules/brace-expansion"]["version"]
            self.assertGreaterEqual(tuple(map(int, version.split("."))), (5, 0, 9))


if __name__ == "__main__":
    unittest.main()
