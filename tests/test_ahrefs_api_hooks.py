import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from guard_harness import run_guard as run_hook  # noqa: E402


class AhrefsApiHookTest(unittest.TestCase):
    def test_claude_hook_blocks_raw_paid_ahrefs_api_call(self):
        result = run_hook(
            "claude/hooks/block-unguarded-ahrefs-api.sh",
            {
                "tool_name": "Bash",
                "tool_input": {
                    "command": "curl https://api.ahrefs.com/v3/site-explorer/all-backlinks"
                },
            },
        )

        self.assertEqual(result.returncode, 0)
        self.assertIn("raw Ahrefs API call detected", result.stdout)

    def test_claude_hook_allows_free_usage_probe(self):
        result = run_hook(
            "claude/hooks/block-unguarded-ahrefs-api.sh",
            {
                "tool_name": "Bash",
                "tool_input": {
                    "command": "curl https://api.ahrefs.com/v3/subscription-info/limits-and-usage"
                },
            },
        )

        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")

    def test_codex_hook_blocks_raw_paid_ahrefs_api_call(self):
        result = run_hook(
            "codex/hooks/block-unguarded-ahrefs-api.sh",
            {
                "tool_name": "functions.exec_command",
                "tool_input": {
                    "cmd": "curl https://api.ahrefs.com/v3/site-explorer/all-backlinks"
                },
            },
        )

        self.assertEqual(result.returncode, 0)
        self.assertIn("raw Ahrefs API call detected", result.stdout)

    def test_codex_hook_allows_guard_wrapper(self):
        result = run_hook(
            "codex/hooks/block-unguarded-ahrefs-api.sh",
            {
                "tool_name": "functions.exec_command",
                "tool_input": {
                    "cmd": "ahrefs-api-guard request /site-explorer/all-backlinks"
                },
            },
        )

        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
