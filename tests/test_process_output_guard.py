"""Exercise process-output prevention at each registered secret-guard entry."""

import json
from pathlib import Path
import subprocess
import shutil
import tempfile
import unittest
from test_process_output_policy import PATH_READ_COMMAND

ROOT = Path(__file__).resolve().parents[1]


class ProcessOutputGuardTests(unittest.TestCase):
    def test_classifier_failure_denies(self):
        for provider in ("claude", "codex"):
            with tempfile.TemporaryDirectory(prefix="process-guard-fixture-") as directory:
                hooks = Path(directory) / "hooks"
                shutil.copytree(ROOT / provider / "hooks", hooks)
                helper = hooks / "lib-process-output-policy.py"
                for source in ("raise SystemExit(1)\n", "print('not-json')\n"):
                    helper.write_text(source)
                    payload = {"tool_name": "Bash", "tool_input": {"command": "ps -axo pid,comm"}}
                    result = subprocess.run(
                        ["bash", str(hooks / "block-secret-leak.sh")],
                        input=json.dumps(payload), capture_output=True, text=True,
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(json.loads(result.stdout)["hookSpecificOutput"]["permissionDecision"], "deny")

    def test_guard_entry_points(self):
        cases = {
            PATH_READ_COMMAND: "allow",
            "pgrep -fl 'synthetic-process-fixture'": "deny",
            "ps -axo pid,etime,command | rg synthetic": "deny",
            "ps aux > /tmp/synthetic-process-list": "deny",
            "pgrep -f synthetic-process-fixture": "allow",
            "ps -axo pid,ppid,comm": "allow",
            "rg -n 'pgrep|ps' README.md": "allow",
        }
        routes = [
            ("claude/hooks/block-secret-leak.sh", "Bash"),
            ("claude/hooks/pretooluse-bash.sh", "Bash"),
            ("codex/hooks/block-secret-leak.sh", "functions.exec_command"),
            ("codex/hooks/block-secret-leak.sh", "functions.exec"),
        ]
        for path, tool in routes:
            for command, expected in cases.items():
                with self.subTest(path=path, tool=tool, command=command):
                    if tool == "functions.exec":
                        tool_input = {"input": "text(await tools.exec_command("
                                      + json.dumps({"cmd": command}) + "));"}
                    else:
                        tool_input = {"command" if tool == "Bash" else "cmd": command}
                    payload = {"tool_name": tool, "tool_input": tool_input, "cwd": str(ROOT)}
                    result = subprocess.run(
                        ["bash", str(ROOT / path)], input=json.dumps(payload),
                        capture_output=True, text=True, cwd=ROOT,
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)
                    output = json.loads(result.stdout) if result.stdout.strip() else {}
                    actual = output.get("hookSpecificOutput", {}).get("permissionDecision", "allow")
                    self.assertEqual(actual, expected, result.stdout)


if __name__ == "__main__":
    unittest.main()
