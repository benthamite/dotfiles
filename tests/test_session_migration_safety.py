"""Offline migration preservation tests; every input and writer is disposable."""
import importlib.util
import json
import os
from pathlib import Path
import stat
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
HELPER = ROOT / "codex/skills/move-session-log/scripts/migration_safety.py"


class MigrationSafetyTests(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location("migration_safety_fixture", HELPER)
        self.mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.mod)
        self.temporary = tempfile.TemporaryDirectory(prefix="migration-safety-", dir="/tmp")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.source = self.root / "session.jsonl"
        self.source.write_bytes(b'{"synthetic":"original"}\n')
        self.backup = self.root / "recovery"
        self.home = mock.patch.object(self.mod.Path, "home", return_value=self.root)
        self.home.start()
        self.addCleanup(self.home.stop)

    def plan(self, path=None, data=b'{"synthetic":"changed"}\n'):
        plan = self.mod.MigrationPlan()
        plan.read(path or self.source)
        plan.rewrite(path or self.source, data)
        return plan

    def apply(self, plan):
        with mock.patch.object(self.mod, "_check_writers"):
            return plan.run(offline=True, backup_dir=self.backup)

    def manifest(self):
        return json.loads((self.backup / "manifest.json").read_bytes())

    def test_paired_helpers_are_identical(self):
        mirror = ROOT / "claude/skills/move-session-log/scripts/migration_safety.py"
        self.assertEqual(HELPER.read_bytes(), mirror.read_bytes())

    def test_dry_run_validates_without_original_writes_backups_or_callbacks(self):
        plan = self.plan()
        action = mock.Mock()
        plan.add_action("synthetic action", action)
        before = (self.source.stat(), self.source.read_bytes(), set(self.root.iterdir()))
        with mock.patch.object(self.mod, "_check_writers") as writers:
            self.assertIsNone(plan.run(dry_run=True, backup_dir=self.backup))
        self.assertEqual((self.source.stat(), self.source.read_bytes(), set(self.root.iterdir())), before)
        writers.assert_called_once()
        action.assert_not_called()

    def test_apply_requires_offline_and_a_new_absolute_outside_drive_backup(self):
        drive = self.root / "My Drive"
        drive.mkdir()
        existing = self.root / "existing"
        existing.mkdir()
        for options in ({}, {"offline": True}, {"backup_dir": self.backup},
                        {"offline": True, "backup_dir": Path("relative")},
                        {"offline": True, "backup_dir": existing},
                        {"offline": True, "backup_dir": drive / "backup"}):
            with self.subTest(options=options), mock.patch.object(self.mod, "_check_writers"):
                with self.assertRaises(self.mod.MigrationError):
                    self.plan().run(**options)
        self.assertEqual(self.source.read_bytes(), b'{"synthetic":"original"}\n')
        self.assertFalse(self.backup.exists())

    def test_private_backups_manifest_and_original_mode_are_preserved(self):
        self.source.chmod(0o640)
        original = self.source.read_bytes()
        self.assertEqual(self.apply(self.plan()), self.backup.resolve())
        self.assertEqual(stat.S_IMODE(self.backup.stat().st_mode), 0o700)
        manifest = self.manifest()
        entry = manifest["inputs"][0]
        self.assertEqual(entry["originalPath"], str(self.source))
        self.assertEqual(entry["mode"], 0o640)
        self.assertEqual((self.backup / entry["backup"]).read_bytes(), original)
        for path in self.backup.iterdir():
            self.assertEqual(stat.S_IMODE(path.stat().st_mode), 0o600)
        self.assertEqual(stat.S_IMODE(self.source.stat().st_mode), 0o640)
        self.assertEqual(manifest["journal"][0]["status"], "completed")

    def test_shared_history_symlink_stays_intact_and_retargeting_refuses(self):
        alias = self.root / "history.jsonl"
        alias.symlink_to(self.source.name)
        identity = alias.lstat().st_ino
        self.apply(self.plan(alias))
        self.assertTrue(alias.is_symlink())
        self.assertEqual(alias.lstat().st_ino, identity)
        self.assertIn(b"changed", self.source.read_bytes())
        plan = self.plan(alias, b"next")
        other = self.root / "other.jsonl"
        other.write_bytes(alias.read_bytes())
        alias.unlink()
        alias.symlink_to(other.name)
        with self.assertRaises(self.mod.MigrationError):
            plan.verify(alias)
        self.assertIn(b"changed", other.read_bytes())

    def test_nonregular_inputs_are_rejected_without_reading_fifo(self):
        fifo = self.root / "pipe"
        os.mkfifo(fifo)
        for path in (fifo, self.root):
            with self.subTest(path=path), self.assertRaises(self.mod.MigrationError):
                self.mod.MigrationPlan().read(path)

    def test_move_rejects_all_destination_collisions_and_source_symlinks(self):
        for kind in ("file", "directory", "dangling"):
            destination = self.root / kind
            if kind == "file":
                destination.write_bytes(b"foreign")
            elif kind == "directory":
                destination.mkdir()
            else:
                destination.symlink_to("missing")
            with self.subTest(kind=kind), self.assertRaises(self.mod.MigrationError):
                self.mod.MigrationPlan().move(self.source, destination)
        bucket = self.root / "bucket"
        bucket.mkdir()
        (bucket / "link").symlink_to(self.source)
        alias = self.root / "alias"
        alias.symlink_to(bucket, target_is_directory=True)
        for path in (bucket, alias):
            with self.assertRaises(self.mod.MigrationError):
                self.mod.MigrationPlan().move(path, self.root / "new")

    def test_tree_membership_drift_refuses_before_any_backup(self):
        bucket = self.root / "bucket"
        bucket.mkdir()
        transcript = bucket / "session.jsonl"
        transcript.write_bytes(b"original")
        plan = self.mod.MigrationPlan()
        plan.move(bucket, self.root / "moved")
        (bucket / "new-session.jsonl").write_bytes(b"concurrent")
        with self.assertRaises(self.mod.MigrationError):
            self.apply(plan)
        self.assertFalse(self.backup.exists())
        self.assertTrue(transcript.exists())

    def test_watch_tree_detects_new_entries_without_a_move(self):
        plan = self.mod.MigrationPlan()
        plan.watch_tree(self.root)
        (self.root / "new-session.jsonl").write_bytes(b"concurrent")
        with mock.patch.object(self.mod, "_check_writers"), self.assertRaises(self.mod.MigrationError):
            plan.run(dry_run=True)

    def test_supplied_dry_run_backup_is_validated_without_creation(self):
        for backup in (self.source, Path("relative")):
            with mock.patch.object(self.mod, "_check_writers"), self.assertRaises(self.mod.MigrationError):
                self.plan().run(dry_run=True, backup_dir=backup)
        plan = self.plan()
        plan.watch_tree(self.root)
        with mock.patch.object(self.mod, "_check_writers"), self.assertRaises(self.mod.MigrationError):
            plan.run(dry_run=True, backup_dir=self.backup)
        self.assertFalse(self.backup.exists())

    def test_absent_sidecar_tracks_moved_parent(self):
        bucket = self.root / "bucket"
        bucket.mkdir()
        transcript = bucket / "session.jsonl"
        transcript.write_bytes(b"original")
        plan = self.mod.MigrationPlan()
        plan.expect_absent(bucket / "missing-sidecar")
        plan.move(bucket, self.root / "moved")
        self.apply(plan)
        with mock.patch.object(self.mod, "_check_writers"):
            plan.validate()
        (self.root / "moved/missing-sidecar").mkdir()
        with mock.patch.object(self.mod, "_check_writers"), self.assertRaises(self.mod.MigrationError):
            plan.validate()

    def test_rewrite_then_directory_move_accepts_only_owned_snapshot_changes(self):
        bucket = self.root / "bucket"
        bucket.mkdir()
        transcript = bucket / "session.jsonl"
        transcript.write_bytes(b"original")
        destination = self.root / "moved"
        plan = self.mod.MigrationPlan()
        plan.move(bucket, destination)
        plan.rewrite(transcript, b"changed")
        try:
            self.apply(plan)
        except self.mod.MigrationError as error:
            self.fail(str(error.__context__))
        self.assertFalse(bucket.exists())
        self.assertEqual((destination / transcript.name).read_bytes(), b"changed")
        self.assertEqual([e["kind"] for e in self.manifest()["journal"]], ["rewrite", "move"])

    def test_explicit_new_bucket_supports_multiple_moves_without_implicit_parents(self):
        second = self.root / "sidecar"
        second.mkdir()
        (second / "child.jsonl").write_bytes(b"child")
        bucket = self.root / "new-project"
        plan = self.plan()
        plan.mkdir(bucket)
        plan.move(self.source, bucket / self.source.name)
        plan.move(second, bucket / second.name)
        try:
            self.apply(plan)
        except self.mod.MigrationError as error:
            self.fail(str(error.__context__))
        self.assertEqual(stat.S_IMODE(bucket.stat().st_mode), 0o700)
        self.assertIn(b"changed", (bucket / self.source.name).read_bytes())
        self.assertEqual((bucket / "sidecar/child.jsonl").read_bytes(), b"child")
        with self.assertRaises(self.mod.MigrationError):
            self.mod.MigrationPlan().mkdir(self.root / "missing/child")

    def test_changes_after_backup_are_not_overwritten(self):
        plan = self.plan()
        backup = plan._backup

        def changed(directory):
            backup(directory)
            self.source.write_bytes(b"concurrent replacement")

        with mock.patch.object(plan, "_backup", side_effect=changed):
            with self.assertRaises(self.mod.MigrationError) as caught:
                self.apply(plan)
        self.assertEqual(self.source.read_bytes(), b"concurrent replacement")
        self.assertEqual(caught.exception.backup_dir, self.backup.resolve())
        self.assertIn(b"original", (self.backup / self.manifest()["inputs"][0]["backup"]).read_bytes())

    def test_late_destination_collision_never_overwrites_foreign_file(self):
        destination = self.root / "destination.jsonl"
        plan = self.mod.MigrationPlan()
        plan.move(self.source, destination)
        rename = self.mod._move_no_replace

        def collision(source, target):
            target.write_bytes(b"foreign")
            rename(source, target)

        with mock.patch.object(self.mod, "_move_no_replace", side_effect=collision):
            with self.assertRaises(self.mod.MigrationError):
                self.apply(plan)
        self.assertEqual(destination.read_bytes(), b"foreign")
        self.assertIn(b"original", self.source.read_bytes())
        self.assertEqual(self.manifest()["journal"][-1]["status"], "failed-or-uncertain")

    def test_replace_performs_then_raises_retains_changed_file_without_rollback(self):
        replace = self.mod.os.replace

        def uncertain(source, destination):
            replace(source, destination)
            if Path(destination) == self.source.resolve():
                raise OSError("untrusted synthetic error text")

        with mock.patch.object(self.mod.os, "replace", side_effect=uncertain):
            with self.assertRaises(self.mod.MigrationError) as caught:
                self.apply(self.plan())
        self.assertIn(b"changed", self.source.read_bytes())
        self.assertNotIn("untrusted", str(caught.exception))
        self.assertEqual(self.manifest()["journal"][-1]["status"], "failed-or-uncertain")

    def test_callback_failure_reports_partial_state_and_never_runs_later_action(self):
        plan = self.plan()
        later = mock.Mock()

        def fail():
            self.source.write_bytes(b"callback wrote before failure")
            raise RuntimeError("sensitive fixture content must not appear")

        plan.add_action("synthetic database", fail)
        plan.add_action("later database", later)
        with self.assertRaises(self.mod.MigrationError) as caught:
            self.apply(plan)
        self.assertEqual(self.source.read_bytes(), b"callback wrote before failure")
        self.assertNotIn("sensitive", str(caught.exception))
        later.assert_not_called()
        self.assertEqual([e["status"] for e in self.manifest()["journal"]],
                         ["completed", "failed-or-uncertain"])

    def test_independent_callbacks_verify_inputs_without_recapturing_prior_db_writes(self):
        second = self.root / "second.db"
        second.write_bytes(b"second")
        plan = self.mod.MigrationPlan()
        plan.read(self.source)
        plan.read(second)

        def action(path):
            plan.verify(path)
            path.write_bytes(b"owned database update")

        plan.add_action("first", lambda: action(self.source))
        plan.add_action("second", lambda: action(second))
        self.apply(plan)
        self.assertEqual(second.read_bytes(), b"owned database update")
        self.assertEqual([e["status"] for e in self.manifest()["journal"]], ["completed", "completed"])

    def test_inventory_checks_run_in_dry_run_and_sanitize_failures(self):
        for mode in ("false", "exception"):
            plan = self.plan()

            def check():
                if mode == "exception":
                    raise OSError("private fixture inventory details")
                return False

            plan.add_check("inventory", check)
            with mock.patch.object(self.mod, "_check_writers"), self.assertRaises(self.mod.MigrationError) as caught:
                plan.run(dry_run=True)
            self.assertNotIn("private", str(caught.exception))
            self.assertFalse(self.backup.exists())

    def test_inventory_check_repeats_after_staging_before_replacement(self):
        plan = self.plan()
        changed = False
        plan.add_check("membership", lambda: not changed)
        mkstemp = self.mod.tempfile.mkstemp

        def stage(**options):
            nonlocal changed
            result = mkstemp(**options)
            if options.get("prefix") == ".session-migration-":
                changed = True
            return result

        with mock.patch.object(self.mod.tempfile, "mkstemp", side_effect=stage):
            with self.assertRaises(self.mod.MigrationError) as caught:
                self.apply(plan)
        self.assertIn("Additional inventory validation failed", str(caught.exception))
        self.assertIn("partial changes may remain", str(caught.exception))
        self.assertIn(b"original", self.source.read_bytes())
        self.assertEqual(self.manifest()["journal"][-1]["status"], "failed-or-uncertain")
        self.assertFalse(list(self.root.glob(".session-migration-*")))

    def test_inventory_checks_repeat_before_mkdir_and_move(self):
        for kind in ("mkdir", "move"):
            plan = self.mod.MigrationPlan()
            target = self.root / ("new-" + kind)
            if kind == "mkdir":
                plan.mkdir(target)
            else:
                plan.move(self.source, target)
            changed = False
            plan.add_check("membership", lambda: not changed)
            save = plan._save_manifest

            def journal():
                nonlocal changed
                save()
                if plan.journal:
                    changed = True

            with mock.patch.object(plan, "_save_manifest", side_effect=journal):
                with mock.patch.object(self.mod, "_check_writers"), self.assertRaises(self.mod.MigrationError):
                    plan.run(offline=True, backup_dir=self.root / ("recovery-" + kind))
            self.assertFalse(target.exists())
            self.assertTrue(self.source.exists())

    def test_inventory_checks_repeat_before_each_action_without_recapturing(self):
        plan = self.mod.MigrationPlan()
        plan.read(self.source)
        changed = False
        plan.add_check("membership", lambda: not changed)

        def first():
            nonlocal changed
            changed = True

        later = mock.Mock()
        plan.add_action("first", first)
        plan.add_action("later", later)
        with self.assertRaises(self.mod.MigrationError):
            self.apply(plan)
        later.assert_not_called()
        self.assertEqual([e["status"] for e in self.manifest()["journal"]],
                         ["completed", "failed-or-uncertain"])

    def test_mount_diagnostics_require_exact_complete_matching_groups(self):
        raw = "/Volumes/fixture’s disk".encode()
        escaped = b"/Volumes/fixture\\xe2\\x80\\x99s disk"
        group = (b"lsof: avoiding readlink(" + escaped + b"): -b was specified.\n"
                 b"lsof: avoiding stat(" + raw + b"): -b was specified.\n"
                 b"lsof: WARNING: can't stat() apfs file system " + escaped + b"\n"
                 b"      Output information may be incomplete.\n"
                 b'      assuming "dev=123ab" from mount table\n'
                 b"lsof: avoiding readlink(/dev/fixture): -b was specified.\n"
                 b"lsof: avoiding stat(/dev/fixture): -b was specified.\n")
        self.mod._check_mount_diagnostics(group)
        self.mod._check_mount_diagnostics(b"")
        self.assertEqual(self.mod._lsof_safe_path(b"/Volumes/back\\slash"), b"/Volumes/back\\\\slash")
        for bad in (group + b"private unknown diagnostic\n", group.rsplit(b"\n", 2)[0],
                    group.replace(raw, b"/Volumes/different", 1),
                    group.replace(b"assuming", b"guessed"),
                    group.replace(b"dev=123ab", b"dev=not-hex"),
                    group.replace(raw, b"/Volumes/control\tcharacter", 1)):
            with self.subTest(kind=len(bad)), self.assertRaises(self.mod.MigrationError) as caught:
                self.mod._check_mount_diagnostics(bad)
            self.assertNotIn("private", str(caught.exception))

    def test_kernel_records_match_both_device_and_inode_and_refuse_unknowns(self):
        selected = {(0x12, 34)}
        process = b"p123\0\n"
        for record in (b"f3\0ar\0tREG\0D0x12\0i34\0\n",
                       b"f3\0aw\0tREG\0D0x13\0i34\0\n",
                       b"f3\0aw\0tREG\0D0x12\0i35\0\n",
                       b"f3\0au\0tPIPE\0\n"):
            self.mod._check_kernel_records(process + record, selected)
        for kind in (b"rte", b"ndrv", b"key", b"ppp", b"vsock", b"vsockp"):
            self.mod._check_kernel_records(process + b"f3\0au\0t" + kind + b"\0\n", selected)
        for record in (b"f3\0aw\0tREG\0D0x12\0i34\0\n",
                       b"f3\0au\0tREG\0D0x12\0i34\0\n",
                       b"f3\0a?\0tREG\0D0x12\0i34\0\n",
                       b"ftxt\0tREG\0D0x12\0i34\0\n",
                       b"ftxt\0tREG\0\n", b"ferr\0tREG\0\n",
                       b"f3\0ar\0tREG\0D0x12\0\n", b"f3\0tUNKNOWN\0\n",
                       b"f3\0t0000\0\n", b"f3\0tREG\0Dnothex\0i34\0\n",
                       b"f3\0tREG\0D0x12\0iNaN\0\n",
                       b"f3\0tREG\0D0x12\0i34\0ar\0ar\0\n", b"f3\0\n"):
            with self.subTest(record=record), self.assertRaises(self.mod.MigrationError):
                self.mod._check_kernel_records(process + record, selected)

    def test_only_exact_vanished_numeric_descriptor_can_omit_identity(self):
        process = b"p123\0\n"
        vanished = b"f3\0a \0nvnode: FD unavailable\0\n"
        self.mod._check_kernel_records(process + vanished, {(0x12, 34)})
        self.mod._check_kernel_records(process + vanished.replace(b"vnode:", b"socket:"), {(0x12, 34)})
        for record in (vanished.replace(b"f3", b"ftxt"),
                       vanished.replace(b"f3", b"ferr"),
                       vanished.replace(b"f3", b"f-1"),
                       vanished.replace(b"a \0", b""),
                       vanished.replace(b"a \0", b"a\0"),
                       vanished.replace(b"a \0", b"ar\0"),
                       vanished.replace(b"a \0", b"aw\0"),
                       vanished.replace(b"a \0", b"au\0"),
                       vanished.replace(b"a \0", b"a  \0"),
                       vanished.replace(b"f3\0", b"f3\0tREG\0"),
                       vanished.replace(b"f3\0", b"f3\0D0x12\0"),
                       vanished.replace(b"f3\0", b"f3\0i34\0"),
                       vanished.replace(b"FD unavailable", b"FD unavailable extra"),
                       vanished.replace(b"FD unavailable", b"process unavailable"),
                       b"f3\0nno more information\0\n", b"f3\0n(revoked)\0\n",
                       b"f3\0nprivate unknown error text\0\n"):
            for variant in (record, record.replace(b"vnode:", b"socket:")):
                with self.subTest(record=variant), self.assertRaises(self.mod.MigrationError) as caught:
                    self.mod._check_kernel_records(process + variant, {(0x12, 34)})
                self.assertNotIn("private", str(caught.exception))

    def test_incomplete_diagnostics_expose_only_fixed_categories(self):
        for message, category in ((b"vnode: process unavailable", "vnode-ESRCH"),
                                  (b"no more information", "vnode-EPERM"),
                                  (b"(revoked)", "vnode-revoked"),
                                  (b"socket: process unavailable", "socket-ESRCH"),
                                  (b"vnode: " + os.strerror(13).encode(), "vnode-EACCES"),
                                  (b"/private/personal/unknown path", "unknown-redacted")):
            with self.assertRaises(self.mod.MigrationError) as caught:
                self.mod._check_kernel_records(b"p987654\0\nf87654\0a \0n" + message + b"\0\n", set())
            text = str(caught.exception)
            self.assertIn("fields=afn; access=blank; reason=" + category, text)
            self.assertIn("descriptor=decimal", text)
            self.assertNotIn("987654", text)
            self.assertNotIn("87654", text)
            self.assertNotIn("personal", text)
        for value, expected in ((None, "absent"), (b"", "empty"), (b"raw private", "other")):
            record = {b"f": b"3", b"n": b"unknown private text"}
            if value is not None:
                record[b"a"] = value
            text = self.mod._record_diagnostic(record)
            self.assertIn("access=" + expected, text)
            self.assertNotIn("private", text)
        for value, expected in ((b"*123", "star-three-digits"), (b"txt", "mapped"),
                                (b"*1234", "other"), (b"private descriptor", "other")):
            text = self.mod._record_diagnostic({b"f": value})
            self.assertIn("descriptor=" + expected, text)
            self.assertNotIn("private", text)

    def test_lsof_errors_and_unknown_access_fail_closed_without_raw_output(self):
        version = subprocess.CompletedProcess([], 0, b"", b"  revision: 4.91\n  configuration info: libproc-based\n")
        outputs = [(2, b"", b""), (1, b"", b"untrusted sensitive stderr"),
                   (0, b"", b""), (0, b"p1\0\nf3\0\n", b""),
                   (0, b"p1\0\nf3\0au\0\n", b""), (0, b"invalid", b"")]
        for status, stdout, stderr in outputs:
            with self.subTest(status=status, stdout=stdout, stderr=stderr):
                result = subprocess.CompletedProcess([], status, stdout, stderr)
                with mock.patch.object(self.mod.subprocess, "run", side_effect=[version, result]):
                    with self.assertRaises(self.mod.MigrationError) as caught:
                        self.mod._check_writers([self.source])
                self.assertNotIn("sensitive", str(caught.exception))
        with mock.patch.object(self.mod.subprocess, "run", side_effect=FileNotFoundError):
            with self.assertRaises(self.mod.MigrationError):
                self.mod._check_writers([self.source])
        wrong_version = subprocess.CompletedProcess([], 0, b"revision: 5.0\n", b"")
        with mock.patch.object(self.mod.subprocess, "run", return_value=wrong_version) as run:
            with self.assertRaises(self.mod.MigrationError):
                self.mod._check_writers([self.source])
            run.assert_called_once()

    def test_native_lsof_distinguishes_closed_readonly_and_open_writer(self):
        self.mod._check_writers([self.source])
        with self.source.open("rb"):
            self.mod._check_writers([self.source])
        with self.source.open("ab"):
            with self.assertRaises(self.mod.MigrationError):
                self.mod._check_writers([self.source])

    def test_native_owned_descriptor_churn_preserves_writer_detection(self):
        child_code = ("import os,sys,time,socket\n"
                      "print('ready',flush=True)\n"
                      "end=time.monotonic()+8\n"
                      "while time.monotonic()<end:\n"
                      " fds=[os.open(sys.argv[1],os.O_RDONLY) for _ in range(32)]\n"
                      " for fd in fds: os.close(fd)\n"
                      " pairs=[socket.socketpair() for _ in range(16)]\n"
                      " for pair in pairs:\n"
                      "  for sock in pair: sock.close()\n")
        child = subprocess.Popen([sys.executable, "-c", child_code, str(self.source)],
                                 stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True)
        try:
            self.assertEqual(child.stdout.readline().strip(), "ready")
            # Native acceptance is bounded, not a retry-until-pass loop.  Every
            # scan must pass; the synthetic record test ensures exact EBADF
            # coverage even when scheduling yields no vanished descriptor.
            for _ in range(6):
                self.mod._check_writers([self.source])
            with self.source.open("ab"):
                with self.assertRaisesRegex(self.mod.MigrationError, "open writer"):
                    self.mod._check_writers([self.source])
        finally:
            if child.poll() is None:
                child.terminate()
            child.wait(timeout=3)
            child.stdout.close()

if __name__ == "__main__":
    unittest.main(verbosity=2)
