import json
import subprocess
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def run_hook(script, payload):
    return subprocess.run(
        [str(ROOT / "claude" / "hooks" / script)],
        input=json.dumps(payload),
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )


class ClaudeBashPreToolUseTest(unittest.TestCase):
    def assert_rewrite_preserves_bash_input_fields(self, script):
        payload = {
            "tool_name": "Bash",
            "tool_input": {
                "command": "printenv",
                "timeout": 600000,
                "run_in_background": True,
                "description": "long local eval",
            },
        }
        result = run_hook(script, payload)

        self.assertEqual(result.returncode, 0, result.stderr)
        output = json.loads(result.stdout)
        updated_input = output["hookSpecificOutput"]["updatedInput"]

        self.assertIn("redact-secrets.sh", updated_input["command"])
        self.assertEqual(updated_input["timeout"], 600000)
        self.assertIs(updated_input["run_in_background"], True)
        self.assertEqual(updated_input["description"], "long local eval")

    def test_command_rewrite_preserves_bash_input_fields(self):
        self.assert_rewrite_preserves_bash_input_fields("pretooluse-bash.sh")

    def test_standalone_wrap_rewrite_preserves_bash_input_fields(self):
        self.assert_rewrite_preserves_bash_input_fields("wrap-bash-output.sh")

    def assert_benign_command_is_not_rewritten(self, script):
        payload = {
            "tool_name": "Bash",
            "tool_input": {"command": "git status --short"},
        }
        result = run_hook(script, payload)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")

    def test_dispatcher_leaves_benign_command_unchanged(self):
        self.assert_benign_command_is_not_rewritten("pretooluse-bash.sh")

    def test_standalone_wrap_leaves_benign_command_unchanged(self):
        self.assert_benign_command_is_not_rewritten("wrap-bash-output.sh")


if __name__ == "__main__":
    unittest.main()
