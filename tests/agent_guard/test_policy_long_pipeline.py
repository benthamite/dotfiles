import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, Call  # noqa: E402
from agent_guard.policies import long_pipeline  # noqa: E402

GIT_ENV = {"GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t", "GIT_COMMITTER_NAME": "t",
           "GIT_COMMITTER_EMAIL": "t@t", "GIT_CONFIG_GLOBAL": os.devnull, "GIT_CONFIG_NOSYSTEM": "1"}


class LongPipelineTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.env = {**os.environ, **GIT_ENV}
        self.repos = {}
        for name in ("consensus-trader", "other"):
            repo = Path(self.tmp.name, name)
            (repo / "scripts").mkdir(parents=True)
            (repo / "scripts/setup_db.py").write_text("x\n")
            for args in (["init", "-q"], ["add", "-A"], ["commit", "-qm", "init"]):
                subprocess.run(["git", *args], cwd=repo, env=self.env, check=True)
            self.repos[name] = repo

    def tearDown(self):
        self.tmp.cleanup()

    def decide(self, command, repo="consensus-trader"):
        payload = {"tool_name": "Bash", "tool_input": {"command": command}, "cwd": str(self.repos[repo])}
        return long_pipeline.decide(Call.from_payload(CLAUDE, payload, env=self.env))

    def test_prepared_launch_allows(self):
        self.assertEqual(self.decide("PREFLIGHT=ok python3 scripts/setup_db.py --full").decision, "allow")

    def test_unprepared_launch_lists_failing_gates(self):
        (self.repos["consensus-trader"] / "scripts/setup_db.py").write_text("changed\n")
        result = self.decide("PIPELINE_CHECKPOINT=0 bash scripts/refresh_data.sh")
        self.assertEqual(result.decision, "deny")
        self.assertTrue(result.reason.startswith(
            "BLOCKED: long pipeline run is not pre-flight-ready.\n  [1] Uncommitted pipeline changes — commit "
            "before a multi-hour run:\n       M scripts/setup_db.py\n  [2] PIPELINE_CHECKPOINT=0"))
        self.assertIn("\n  [3] Missing PREFLIGHT=ok.", result.reason)
        self.assertNotIn("\\n", result.reason)
        self.assertTrue(result.reason.endswith("re-run with PREFLIGHT=ok prefixed."))

    def test_scope_and_launch_detection(self):
        self.assertEqual(self.decide("python3 scripts/setup_db.py --full", repo="other").decision, "allow")
        for command in ("cat scripts/refresh_data.sh", "python3 scripts/setup_db.py | grep --full",
                        "python3 scripts/setup_db.py\n--full"):
            self.assertEqual(self.decide(command).decision, "allow", command)
        self.assertEqual(self.decide("xPREFLIGHT=ok sh refresh_data.sh").decision, "deny")

    def test_long_command_with_early_match_is_denied(self):
        # Intended difference 15: the hook's launch grep lost this to SIGPIPE and allowed.
        result = self.decide("sh scripts/refresh_data.sh\n" + "x\n" * 200_001)
        self.assertEqual(result.decision, "deny")
        self.assertIn("[3] Missing PREFLIGHT=ok.", result.reason)

    def test_cd_target_decides_the_repository(self):
        other = self.repos["other"]
        target = self.repos["consensus-trader"]
        payload = {"tool_name": "Bash", "tool_input": {"command": f"cd {target} && sh refresh_data.sh"},
                   "cwd": str(other)}
        self.assertEqual(long_pipeline.decide(Call.from_payload(CLAUDE, payload, env=self.env)).decision, "deny")


class GitFailureTests(unittest.TestCase):
    """`DIRTY=$(git -C ... status ... 2>/dev/null || true)`: a failing git skips gate 1 only."""

    def test_non_repository_named_consensus_trader(self):
        with tempfile.TemporaryDirectory() as tmp:
            plain = Path(tmp, "consensus-trader")
            plain.mkdir()
            old = os.getcwd()
            os.chdir(plain)  # REPO_ROOT is empty, so the hook fell back to $PWD
            try:
                env = {**os.environ, **GIT_ENV, "PWD": str(plain), "GIT_CEILING_DIRECTORIES": tmp}
                payload = {"tool_name": "Bash", "tool_input": {"command": "sh refresh_data.sh"}}
                result = long_pipeline.decide(Call.from_payload(CLAUDE, payload, env=env))
            finally:
                os.chdir(old)
        self.assertEqual(result.decision, "deny")
        self.assertNotIn("[1]", result.reason)
        self.assertIn("[3] Missing PREFLIGHT=ok.", result.reason)


if __name__ == "__main__":
    unittest.main()
