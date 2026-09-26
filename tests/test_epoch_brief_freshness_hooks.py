"""Tests for the paired epoch-brief-freshness.sh PostToolUse wrappers.

The wrappers filter cheaply and dispatch to the Epoch helper, which owns
detection, the audit and dedup (tested in the Epoch repository). These tests
cover only the wrapper contract: fast exit, silent no-op when the Epoch tree is
absent, and verbatim pass-through of the helper's stdin and stdout.
"""
from __future__ import annotations

import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path

DOTFILES = Path(__file__).resolve().parents[1]
WRAPPERS = (
    DOTFILES / "claude/hooks/epoch-brief-freshness.sh",
    DOTFILES / "codex/hooks/epoch-brief-freshness.sh",
)

STUB_HELPER = """\
import json, os, sys
payload = sys.stdin.read()
with open(os.environ["STUB_RECORD"], "w") as handle:
    handle.write(payload)
print(json.dumps({"hookSpecificOutput": {"hookEventName": "PostToolUse",
                                         "additionalContext": "stub notice"}}))
"""


class EpochBriefFreshnessWrapperTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.record = self.root / "record.json"

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def install_helper(self) -> Path:
        epoch = self.root / "epoch"
        helper = epoch / "projects/shared/scripts/brief_freshness_notice.py"
        helper.parent.mkdir(parents=True)
        helper.write_text(STUB_HELPER, encoding="utf-8")
        return epoch

    def run_wrapper(self, wrapper: Path, payload: str, epoch: Path) -> subprocess.CompletedProcess:
        environment = dict(os.environ, EPOCH_ROOT=str(epoch), STUB_RECORD=str(self.record),
                           PYTHONDONTWRITEBYTECODE="1")
        return subprocess.run(["/bin/bash", str(wrapper)], input=payload, text=True,
                              capture_output=True, env=environment, timeout=30, check=False)

    def test_payload_without_org_exits_before_the_helper(self) -> None:
        epoch = self.install_helper()
        payload = json.dumps({"tool_name": "Bash", "tool_input": {"command": "ls projects/"}})
        for wrapper in WRAPPERS:
            with self.subTest(wrapper=wrapper.parent.parent.name):
                result = self.run_wrapper(wrapper, payload, epoch)
                self.assertEqual((result.returncode, result.stdout), (0, ""))
                self.assertFalse(self.record.exists())

    def test_missing_epoch_helper_is_a_silent_no_op(self) -> None:
        payload = json.dumps({"tool_name": "Read", "tool_input": {"file_path": "/x/y.org"}})
        for wrapper in WRAPPERS:
            with self.subTest(wrapper=wrapper.parent.parent.name):
                result = self.run_wrapper(wrapper, payload, self.root / "absent")
                self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "", ""))

    def test_org_payload_is_passed_through_to_the_helper(self) -> None:
        epoch = self.install_helper()
        payload = json.dumps({"tool_name": "Read", "session_id": "s",
                              "tool_input": {"file_path": "/e/projects/p/p.org"}})
        for wrapper in WRAPPERS:
            with self.subTest(wrapper=wrapper.parent.parent.name):
                result = self.run_wrapper(wrapper, payload, epoch)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(self.record.read_text(encoding="utf-8"), payload)
                output = json.loads(result.stdout)
                self.assertEqual(output["hookSpecificOutput"]["hookEventName"], "PostToolUse")
                self.assertEqual(output["hookSpecificOutput"]["additionalContext"], "stub notice")
                self.record.unlink()


if __name__ == "__main__":
    unittest.main()
