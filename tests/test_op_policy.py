"""Case table for the 1Password broker output policy (claude/hooks/lib-op-policy.py).

Both hook copies of the module must agree. Every row here is a command an
agent could type into a Bash tool call; the expected decision follows the
allowlist in docs/superpowers/plans/2026-09-02-secret-guard-op-output-policy.md.
"""

from __future__ import annotations

import importlib.util
import json
import subprocess
import sys
import unittest
from pathlib import Path

DOTFILES = Path("/Users/pablostafforini/My Drive/dotfiles")
MODULES = {
    "claude": DOTFILES / "claude" / "hooks" / "lib-op-policy.py",
    "codex": DOTFILES / "codex" / "hooks" / "lib-op-policy.py",
}


def load(path: Path):
    spec = importlib.util.spec_from_file_location(path.stem.replace("-", "_") + path.parent.parent.name, path)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module  # dataclasses resolve the defining module by name
    spec.loader.exec_module(module)
    return module


ALLOW = [
    # run: masked provisioning of a process
    "op-automations run --env-file=.env.op -- true",
    "op-automations run --env-file .env.op -- python3 script.py --flag",
    "DATA_DIR=/tmp/d op-automations run --env-file .env.op -- /usr/bin/python3 bridge.py --redraft 2> /tmp/err.txt",
    "op-automations run --env-file /Users/x/repo/.env.op -- /usr/bin/python3 /Users/x/repo/bridge.py --redraft --actor 'claude[bot]'",
    "op-automations --account acct run --env-file .env.op -- true",
    "cd ~/repos/x && op-automations run --env-file .env.op -- make test",
    'op-automations run --env-file .env.op -- python3 bridge.py > "$S/out.txt" 2> "$S/err.txt"',
    # read: captured, filed, or consumed
    "op-automations read op://Automations/Example/credential > /dev/null",
    "op-desktop read op://Employee/Example/credential > /tmp/token.txt",
    "op-automations read op://Automations/Example/credential --out-file /tmp/token.txt",
    'X=$(op-automations read op://Automations/X/credential); curl -H "Authorization: Bearer $X" https://api.example',
    'TOKEN=$(op-automations read op://Automations/X/credential)\ncurl -H "Authorization: Bearer $TOKEN" https://api.example',
    'curl -H "Authorization: Bearer $(op-automations read op://Automations/X/credential)" https://api.example',
    "op-automations read op://Automations/X/credential | pbcopy",
    "op-automations read op://Automations/X/credential | gh secret set MY_SECRET --repo o/r",
    "op-automations read op://Automations/X/credential | wrangler secret put TOKEN",
    "op-automations read op://Automations/X/credential | docker login ghcr.io -u me --password-stdin",
    # item output limited to metadata
    "op-desktop item list --format=json | jq '[.[] | {id,title}]'",
    "op-desktop item get abc --format=json | jq '[.fields[] | {label,purpose,type}]'",
    "op-desktop item get abc --format=json | jq -r '.title'",
    "op-desktop item get abc --format=json > /tmp/item.json",
    "op-automations item list --vault Automations --format=json | jq -r '.[].title'",
    # documents and inject to files
    "op-desktop document get abc --out-file /tmp/key.json",
    "op-desktop document get abc > /tmp/key.json",
    "op-automations inject --in-file=.env.op --out-file=/tmp/env",
    "op-desktop document create /tmp/key.json --title Example --vault Automations",
    # writes without --format
    "op-desktop item create --vault Automations --title Example --category login",
    "op-desktop item edit abc 'credential[password]=x'",
    "echo '{}' | op-desktop item create --vault Automations",
    "op-desktop item delete abc",
    # metadata
    "op-desktop --status",
    "op-desktop --stop",
    "op-automations whoami",
    "op-desktop vault list",
    "op-desktop user list",
    "op-desktop item template list",
    "op-desktop document list --vault Automations",
    # inert documentation
    "rg 'pbpaste' docs",
    "rg 'op-automations' docs",
    "git commit -m 'docs: op-desktop read and pbpaste'",
    "git commit -m 'guard: deny OP_RUN_NO_MASKING overrides on op-automations run'",
    "rg OP_RUN_NO_MASKING claude/hooks",
    "bash -c 'echo op-automations read'",
    "sudo echo op-automations read",
    "/bin/echo op-automations read",
    "FOO=x echo op-automations read",
    "echo op-automations read",
    "python3 -c 'print(\"op-automations\")'",
    "make sync-models",
    "case x in *) true ;; esac",
    "copy-slack-draft --file \"$TMPFILE\"\nrc=$?\nrm -f \"$TMPFILE\"\nexit $rc",
]

DENY = [
    # bare or effectively bare reads
    "op-automations read op://Automations/Example/credential",
    "op-automations read op://Automations/Example/credential 2>/dev/null",
    "op-automations read op://Automations/X/credential && echo done",
    "op-automations --account acct read op://Automations/X/credential",
    "op-automations read op://Automations/X/credential > /dev/stdout",
    "op-automations read op://Automations/X/credential >&2",
    "op-automations read op://Automations/X/credential --out-file /dev/stderr",
    "op-automations read op://Automations/X/credential | cat",
    "op-automations read op://Automations/X/credential | tee /tmp/x",
    "op-automations read op://Automations/X/credential | head -c 4",
    "op-automations read op://Automations/X/credential | base64",
    # position tricks
    "true | op-automations read op://Automations/X/credential",
    "{ op-automations read op://Automations/X/credential; }",
    "if true; then op-automations read op://Automations/X/credential; fi",
    "! op-automations read op://Automations/X/credential",
    "cat <(op-automations read op://Automations/X/credential)",
    "diff <(op-automations read op://A/x/c) /tmp/f",
    "find . -exec op-automations read op://A/x/c ;",
    "xargs op-automations",
    # captured then printed in the same call
    "echo $(op-automations read op://Automations/X/credential)",
    "echo `op-desktop read op://Employee/X/credential`",
    "echo $(op-desktop item list)",
    "X=$(op-automations read op://Automations/X/credential); printf '%s\\n' \"$X\"",
    "X=$(op-automations read op://Automations/X/credential)\necho \"$X\"",
    "X=$(op-automations read op://Automations/X/credential) && cat <<< \"$X\"",
    "X=$(op-automations read op://Automations/X/credential); python3 -c \"print('$X')\"",
    "X=$(op-automations read op://Automations/X/credential); echo hi > \"$X\"",
    "X=$(op-automations read op://Automations/X/credential); Y=X; echo \"${!Y}\"",
    "X=$(op-automations read op://Automations/X/credential | tee /tmp/x)",
    # run that can print its environment or disable masking
    "op-automations run --env-file=.env.op -- printenv SECRET",
    "op-automations run --env-file=.env.op -- env",
    "op-automations run --env-file=.env.op -- bash -c 'echo $SECRET'",
    "op-automations run --no-masking --env-file=.env.op -- true",
    "op-automations --cache=false run -- printenv SECRET",
    "OP_RUN_NO_MASKING=1 op-automations run --env-file=.env.op -- true",
    "OP_RUN_NO_MASKING=1 make sync-models",
    "export OP_RUN_NO_MASKING=true; op-automations run --env-file=.env.op -- true",
    "env OP_RUN_NO_MASKING=1 op-automations run --env-file=.env.op -- true",
    "OP_SERVICE_ACCOUNT_TOKEN= op-automations run --env-file=.env.op -- true",
    "op-automations run --env-file=.env.op",
    "X=$(op-automations run --env-file=.env.op -- ./print-token)",
    # subcommands that print credentials, and anything unclassified
    "op-automations inject --in-file=.env.op",
    "op-automations inject --in-file=.env.op --out-file=/dev/stdout",
    "op-desktop document get abc",
    "op-desktop --no-color document get abc",
    "op-desktop item share abc",
    "op-desktop signin --raw",
    "op-desktop signin",
    "op-desktop environment read blgexucrwfr2dtsxe2q4uu7dp4",
    "op-desktop service-account create Example --raw",
    "op-desktop events-api create Example",
    "op-desktop connect token create server",
    "op-desktop connect server create server",
    "op-desktop frobnicate",
    "op-automations",
    "op-desktop item list --format=json",
    "op-desktop item get abc --vault Finance --format=json",
    "op-automations item list --vault Automations --format=json",
    "op-automations item list --vault Automations",
    "op-desktop --format=json item get abc",
    "op-desktop item list --format=json | jq .",
    "op-desktop item list --format=json | jq '.[]'",
    "op-desktop item get abc --format=json | jq '.fields[].value'",
    "op-desktop item get abc --format=json | jq 'to_entries'",
    "op-desktop item get abc --format=json | jq '{id,title}' > /dev/stderr",
    "op-desktop item get abc --fields label=password",
    "op-desktop item create --vault Automations --title Example --format=json",
    "op-desktop item edit abc --format json",
    "op-automations item get abc --reveal",
    "op-automations item get abc --reveal 2>/dev/null",
    "op-desktop item edit abc --reveal",
    # indirection and interpreters
    "/opt/homebrew/bin/bash -c 'op-automations read op://Automations/X/credential'",
    "env bash -c 'op-automations read op://Automations/X/credential'",
    "sudo bash -c 'op-automations run --env-file .env.op -- true'",
    "bash -c 'f(){ op-automations read op://Automations/X/credential; }; f'",
    "bash -c 'echo > >(op-automations read op://Automations/X/credential)'",
    "eval 'op-automations read op://Automations/X/credential'",
    'OP=op-automations; "$OP" read op://Automations/X/credential',
    "OP='op-automations'; \"$OP\" read op://Automations/X/credential",
    "OP=$(which op-automations); \"$OP\" read op://Automations/X/credential",
    "$(command -v op-automations) read op://Automations/X/credential",
    '"$(type -P op-automations)" read op://Automations/X/credential',
    "/Users/pablostafforini/bin/op-automations read op://Automations/X/credential",
    "op-'automations' read op://Automations/X/credential",
    "op\\-automations read op://Automations/X/credential",
    "env -u echo op-automations read op://Automations/X/credential",
    "op-automations read op://Automations/$REF/credential > /tmp/x",
]


class OpPolicyTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.modules = {name: load(path) for name, path in MODULES.items()}

    def check(self, command: str, expected: str) -> None:
        for name, module in self.modules.items():
            with self.subTest(copy=name, command=command):
                result = module.decide(command)
                self.assertEqual(result["decision"], expected, result.get("reason"))

    def test_allowed_shapes(self):
        for command in ALLOW:
            self.check(command, "allow")

    def test_denied_shapes(self):
        for command in DENY:
            self.check(command, "deny")

    def test_copies_are_identical(self):
        self.assertEqual(MODULES["claude"].read_bytes(), MODULES["codex"].read_bytes())

    def test_cli_contract(self):
        for path in MODULES.values():
            result = subprocess.run(
                ["python3", str(path)],
                input="op-automations read op://Automations/X/credential",
                capture_output=True,
                text=True,
                check=True,
            )
            payload = json.loads(result.stdout)
            self.assertEqual(payload["decision"], "deny")
            self.assertIn("read", payload["reason"])
            result = subprocess.run(["python3", str(path)], input="ls", capture_output=True, text=True, check=True)
            self.assertEqual(json.loads(result.stdout), {"decision": "allow"})


if __name__ == "__main__":
    unittest.main()
