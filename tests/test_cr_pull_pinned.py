"""Isolated CLI acceptance checks: no real engine, auth, or network."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parents[1] / "bin/cr-pull-pinned"
JOB = "11111111-1111-4111-8111-111111111111"
ENV = "fbf70736-91c1-40ab-963f-a75187b340c6"


class PullPinnedTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="cr-pinned-test-")
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.wt = self.root / "wt"
        self.task = self.wt / "tasks/sample"
        self.task.mkdir(parents=True)
        self.old = self.task / "taiga/old.txt"
        self.old.parent.mkdir()
        self.old.write_text("original")
        self.info = {"id": "sample", "taigaLink": f"https://taiga.ant.dev/transcripts/?environmentId={ENV}&id={JOB}&problemId=sample&referrer=job"}
        engine = self.root / "Trajectory/reasoning-tasks/main/tooling/engine/src/reasoning_engine"
        engine.mkdir(parents=True)
        (engine / "__init__.py").write_text("")
        (engine / "runs.py").write_text('''import json, os, time
from pathlib import Path
async def fetch_runs(task, **kwargs):
    mode = os.environ.get("MOCK_MODE", "ok")
    if mode == "empty": return
    if mode == "wait":
        deadline = time.monotonic() + 5
        while not Path(os.environ["MOCK_RELEASE"]).exists():
            if time.monotonic() > deadline: raise RuntimeError("mock wait expired")
            time.sleep(0.01)
    dest = Path.cwd() / "tasks" / task / "taiga/job/1"
    dest.mkdir(parents=True)
    if mode == "fifo":
        os.mkfifo(dest / "run.json")
        return
    if mode in ("list", "null"):
        (dest / "run.json").write_text("[]" if mode == "list" else "null")
        return
    (dest / "run.json").write_text(json.dumps({"job_id": kwargs["job_id"] if mode != "wrong-job" else "wrong", "problem_id": kwargs["problem_id"] if mode != "wrong-problem" else "wrong"}))
    if mode == "fail": raise RuntimeError("mock fetch failure")
''')
        bindir = self.root / "bin"
        bindir.mkdir()
        for name, body in {
            "uv": 'import os, sys\na=sys.argv; i=a.index("python"); os.execv(sys.executable,[sys.executable,*a[i+1:]])',
            "trash": 'import os,pathlib,sys\nif os.environ.get("MOCK_MODE") == "trash-fail": sys.exit(1)\np=pathlib.Path(sys.argv[1]); p.rename(p.parent / "trashed-old")',
        }.items():
            p = bindir / name
            p.write_text("#!/usr/bin/env python3\n" + body + "\n")
            p.chmod(0o755)
        self.env = {**os.environ, "HOME": str(self.root), "PATH": str(bindir) + os.pathsep + os.environ["PATH"], "PYTHONDONTWRITEBYTECODE": "1"}

    def invoke(self, slug="sample", mode="ok", write_info=True):
        if write_info:
            (self.task / "basic_info.json").write_text(json.dumps(self.info))
        return subprocess.run([str(SCRIPT), slug, "--worktree", str(self.wt)], env={**self.env, "MOCK_MODE": mode}, capture_output=True, text=True, timeout=10)

    def test_query_order_and_verified_replacement(self):
        result = self.invoke()
        self.assertEqual(result.returncode, 0, result.stderr)
        run = json.loads((self.task / "taiga/job/1/run.json").read_text())
        self.assertEqual(run["job_id"], JOB)
        self.assertEqual((self.task / "trashed-old/old.txt").read_text(), "original")

    def test_failed_or_wrong_identity_preserves_old(self):
        for mode in ("fail", "wrong-job", "wrong-problem", "empty", "trash-fail"):
            with self.subTest(mode=mode):
                result = self.invoke(mode=mode)
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(self.old.read_text(), "original")

    def test_unverified_run_url_or_environment_rejected(self):
        for link in (f"https://taiga.ant.dev/transcripts/?id={JOB}&environmentId={ENV}", f"https://taiga.ant.dev/jobs/{JOB}", f"https://taiga.ant.dev/jobs/{JOB}?environmentId=unknown"):
            with self.subTest(link=link):
                self.info["taigaLink"] = link
                self.assertNotEqual(self.invoke().returncode, 0)
                self.assertEqual(self.old.read_text(), "original")

    def test_existing_unknown_or_dead_lock_preserved(self):
        lock = self.task / ".taiga-pull-lock"
        lock.mkdir()
        for pid in (None, "999999999", str(os.getpid())):
            with self.subTest(pid=pid):
                if pid: (lock / "pid").write_text(pid)
                self.assertNotEqual(self.invoke().returncode, 0)
                self.assertTrue(lock.is_dir())
                self.assertEqual(self.old.read_text(), "original")

    def test_traversal_rejected(self):
        self.assertNotEqual(self.invoke("../sample").returncode, 0)
        self.assertEqual(self.old.read_text(), "original")

    def test_job_path_and_distinct_problem_id(self):
        self.info = {"id": "upstream-problem", "taigaLink": f"https://taiga.ant.dev/jobs/{JOB}?environmentId={ENV}"}
        result = self.invoke()
        self.assertEqual(result.returncode, 0, result.stderr)
        run = json.loads((self.task / "taiga/job/1/run.json").read_text())
        self.assertEqual(run["problem_id"], "upstream-problem")

    def test_singular_job_url(self):
        self.info["taigaLink"] = f"https://taiga.ant.dev/job?id={JOB}&environmentId={ENV}"
        result = self.invoke()
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_malformed_metadata_rejected_without_traceback(self):
        valid = self.info
        for value in ([], None, {**valid, "id": "../sample"}, {**valid, "id": "/sample"}, {**valid, "taigaLink": None}):
            with self.subTest(value=value):
                self.info = value
                result = self.invoke()
                self.assertNotEqual(result.returncode, 0)
                self.assertNotIn("Traceback", result.stderr)
                self.assertEqual(self.old.read_text(), "original")

    def test_url_authority_oddities_rejected(self):
        for host in ("user@taiga.ant.dev", "taiga.ant.dev:443", "taiga.ant.dev:bad"):
            with self.subTest(host=host):
                self.info["taigaLink"] = f"https://{host}/job?id={JOB}&environmentId={ENV}"
                result = self.invoke()
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(self.old.read_text(), "original")

    def test_invalid_run_json_or_fifo_preserves_old(self):
        for mode in ("list", "null", "fifo"):
            with self.subTest(mode=mode):
                result = self.invoke(mode=mode)
                self.assertNotEqual(result.returncode, 0)
                self.assertNotIn("Traceback", result.stderr)
                self.assertEqual(self.old.read_text(), "original")

    def test_basic_info_fifo_rejected_without_blocking(self):
        os.mkfifo(self.task / "basic_info.json")
        result = self.invoke(write_info=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("Traceback", result.stderr)
        self.assertEqual(self.old.read_text(), "original")

    def test_concurrent_caller_waits_without_touching_owner(self):
        (self.task / "basic_info.json").write_text(json.dumps(self.info))
        release = self.root / "release"
        proc = subprocess.Popen([str(SCRIPT), "sample", "--worktree", str(self.wt)],
                                env={**self.env, "MOCK_MODE": "wait", "MOCK_RELEASE": str(release)},
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        try:
            self.assertIn("pinned job:", proc.stdout.readline())
            result = self.invoke()
            self.assertNotEqual(result.returncode, 0)
            self.assertIsNone(proc.poll())
            self.assertEqual(self.old.read_text(), "original")
        finally:
            release.touch()
            stdout, stderr = proc.communicate(timeout=10)
        self.assertEqual(proc.returncode, 0, stdout + stderr)
        self.assertFalse((self.task / ".taiga-pull-lock").exists())


if __name__ == "__main__":
    unittest.main()
