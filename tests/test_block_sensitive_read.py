"""Bash-branch cases for the sensitive-file read guard, in all three copies.

`op-automations run --env-file F -- <program>` is the documented way to give a
program Epoch runtime secrets. The allowance around it must accept the shapes
agents actually type — a leading `cd DIR &&`, `VAR="value with spaces"` — and
keep denying anything that could print the file.
"""

import json
import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
GUARDS = [
    ROOT / "claude" / "hooks" / "block-sensitive-read.sh",
    ROOT / "codex" / "hooks" / "block-sensitive-read.sh",
    ROOT / "claude" / "hooks" / "pretooluse-bash.sh",
]

ALLOW = [
    "op-automations run --env-file .env.op -- python3 script.py --check",
    'EPOCH_SHARED_DIR="/Users/x/My Drive/Epoch/projects/shared" KEY=/tmp/k.json '
    "op-automations run --env-file /Users/x/repo/.env.op -- python3 /Users/x/repo/refresh.py --check --diff",
    "DIR='/Users/x/My Drive' op-automations run --env-file .env.op -- make test",
    'ROOT="$HOME/repos/x" op-automations run --env-file "$ROOT/.env.op" -- python3 "$ROOT/bridge.py"',
    "cd ~/repos/x && op-automations run --env-file .env.op -- make test",
    'cd "/Users/x/My Drive/repo" && op-automations run --env-file .env.op -- python3 run.py',
    "cd ~/repos/x && D=/tmp op-automations run --env-file .env.op -- python3 run.py > /tmp/out.txt 2> /tmp/err.txt",
    # index metadata, with or without -C
    "git ls-files .env.op",
    "git -C /Users/x/repo ls-files .env.op",
    "git -C /Users/x/repo check-ignore -v .env.op",
    # the same loader shapes over a real environment file
    "op-automations run --env-file .env.local -- python3 script.py --check",
    "cd ~/repos/x && op-automations run --env-file .env.local -- make test",
    "git -C /Users/x/repo ls-files .env.local",
]

# Since 2026-09-18 `.env.op` itself is not a secrets file (it holds op://
# references only; see tests/test_env_op_template_policy.py), so the loader
# shapes are exercised against a real environment file here.
DENY = [
    "cat .env.local",
    "cd ~/repos/x && cat .env.local",
    "cd ~/repos/x && op-automations run --env-file .env.local -- true; cat .env.local",
    'X="$(cat .env.local)" op-automations run --env-file .env.local -- true',
    "OP_RUN_NO_MASKING=1 op-automations run --env-file .env.local -- true",
    'cd "$(cat .env.local)" && op-automations run --env-file .env.local -- true',
    "cd ~/repos/x || cat .env.local && op-automations run --env-file .env.local -- true",
    # a revision or option separator in front of the name is still the file
    "git -C /Users/x/repo show HEAD:.env.local",
    "git show HEAD:.env",
    "python3 dump.py --env-file=.env.local",
    "git -C /Users/x/repo ls-files .env.local | xargs cat",
    # op-automations cached reads are plaintext 1Password values
    "cat ~/.local/state/op-automations/cache/0123abcd",
    'head -c 40 "$XDG_STATE_HOME/op-automations/cache/"*',
]


# The standalone sensitive-read guard trusts anything that *starts* with a
# broker; refusing environment dumpers behind `--` is the broker output
# policy's job (block-secret-leak.sh), which only the dispatcher composes in.
DENY_DISPATCHER_ONLY = [
    "op-automations run --env-file .env.op -- env",
    "op-automations run --env-file .env.op -- bash -c 'echo $SECRET'",
    "op-automations run --env-file .env.local -- env",
]


def decision(guard: Path, command: str) -> str | None:
    payload = json.dumps({"tool_name": "Bash", "tool_input": {"command": command}})
    result = subprocess.run(["bash", str(guard)], input=payload, text=True,
                            capture_output=True, check=False)
    assert result.returncode == 0, result.stderr
    if not result.stdout.strip():
        return None
    return json.loads(result.stdout)["hookSpecificOutput"].get("permissionDecision")


class SensitiveReadBashTests(unittest.TestCase):
    def test_env_file_loader_shapes_are_allowed(self):
        for guard in GUARDS:
            for command in ALLOW:
                with self.subTest(guard=guard.parent.parent.name + "/" + guard.name, command=command):
                    self.assertIn(decision(guard, command), (None, "allow"))

    def test_env_file_reads_stay_denied(self):
        for guard in GUARDS:
            for command in DENY:
                with self.subTest(guard=guard.parent.parent.name + "/" + guard.name, command=command):
                    self.assertEqual(decision(guard, command), "deny")

    def test_dispatcher_denies_environment_dumpers_behind_the_loader(self):
        dispatcher = ROOT / "claude" / "hooks" / "pretooluse-bash.sh"
        for command in DENY_DISPATCHER_ONLY:
            with self.subTest(command=command):
                self.assertEqual(decision(dispatcher, command), "deny")


if __name__ == "__main__":
    unittest.main()
