"""The core.hooksPath guard denies overrides and lets plain reads through."""

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from guard_harness import run_guard  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
HOOKS = (
    Path("claude/hooks/pretooluse-bash.sh"),
    Path("codex/hooks/block-hookspath-override.sh"),
)
KEY = "core." + "hooksPath"


def denied(hook, command):
    payload = {"tool_name": "Bash", "tool_input": {"command": command}, "cwd": str(ROOT)}
    result = run_guard(hook, payload, cwd=ROOT)
    return "may not override " + KEY in result.stdout + result.stderr


class HooksPathGuardTests(unittest.TestCase):
    def assert_all(self, command, expected):
        for hook in HOOKS:
            with self.subTest(hook=hook.name, command=command):
                self.assertEqual(expected, denied(hook, command))

    def test_reads_are_allowed(self):
        for command in (
            f"git config --get {KEY}",
            f"git config --global --get {KEY}",
            f"git config --show-origin --get-all {KEY}",
            f"git config {KEY}",
            f"git -C /tmp/x config {KEY} | cat",
        ):
            self.assert_all(command, False)

    def test_overrides_are_denied(self):
        for command in (
            f"git -c {KEY}=/dev/null commit -m x",
            f"git config {KEY} /dev/null",
            f"git config --global {KEY} /tmp/hooks",
            f"git config --unset {KEY}",
            f"git config --add {KEY} /tmp/hooks",
            f"git config --get {KEY}; git config {KEY} /dev/null",
        ):
            self.assert_all(command, True)


if __name__ == "__main__":
    unittest.main()
