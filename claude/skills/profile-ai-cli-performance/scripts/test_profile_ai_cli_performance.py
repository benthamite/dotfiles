#!/usr/bin/env python3

import importlib.util
import os
import sys
import tempfile
import time
import unittest
from pathlib import Path


SCRIPT = Path(__file__).with_name("profile_ai_cli_performance.py")
SPEC = importlib.util.spec_from_file_location("profile_ai_cli_performance", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class BenchmarkTests(unittest.TestCase):
    def condition(self, code):
        return {
            "name": "test",
            "command": [sys.executable, "-c", code],
            "cwd": tempfile.gettempdir(),
            "env": dict(os.environ),
        }

    def test_silent_process_honors_timeout(self):
        started = time.monotonic()
        result = MODULE.execute(self.condition("import time; time.sleep(30)"), 0.2, "OK")
        self.assertEqual(result["status"], "timeout")
        self.assertLess(time.monotonic() - started, 4)

    def test_stderr_is_drained_without_deadlock(self):
        code = "import sys; sys.stderr.write('x' * 200000); print('{\\\"type\\\":\\\"item.completed\\\",\\\"item\\\":{\\\"type\\\":\\\"agent_message\\\",\\\"text\\\":\\\"OK\\\"}}')"
        result = MODULE.execute(self.condition(code), 3, "OK")
        self.assertEqual(result["status"], "success")

    def test_structured_error_is_provider_rejection(self):
        status = MODULE.classify("", ['{"type":"error","message":"unavailable"}'], 1, False)
        self.assertEqual(status, "provider_rejection")

    def test_summary_uses_paired_blocks(self):
        rows = []
        for iteration, clean, configured in ((1, 1.0, 4.0), (2, 10.0, 11.0)):
            for condition, value in (("claude_clean", clean), ("claude_configured", configured)):
                rows.append({
                    "condition": condition,
                    "iteration": iteration,
                    "status": "success",
                    "completed_message_s": value,
                    "total_s": value + 1,
                })
        summary = MODULE.summarize(rows)
        effect = summary["claude_configuration_association"]
        self.assertEqual(effect["paired_blocks"], 2)
        self.assertEqual(effect["paired_completed_message_delta_median_s"], 2)


if __name__ == "__main__":
    unittest.main()
