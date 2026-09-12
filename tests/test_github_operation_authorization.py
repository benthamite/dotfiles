"""Exercise scoped grants through both real hook entrypoints, without GitHub writes."""
from datetime import datetime, timedelta, timezone
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
FORK = "gh repo fork external/package --clone=false"


class OperationGrants(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        for runtime in ("claude", "codex"):
            target = self.root / runtime / "hooks"
            target.mkdir(parents=True)
            for source in (ROOT / runtime / "hooks").glob("*"):
                if source.is_file() and (source.name.startswith("lib-") or source.name == "block-github-write-command.sh"):
                    shutil.copy2(source, target / source.name)
        (self.root / "bin").mkdir()
        shutil.copy2(ROOT / "bin/github-operation-authorization", self.root / "bin")
        (self.root / "agents").mkdir()
        (self.root / "agents/github-write-allowlist.txt").write_text("benthamite/*\n")
        self.manifest = self.root / "agents/github-operation-authorizations.json"
        self.body = self.root / "body.md"
        self.body.write_text("Reviewed public PR description\n")
        self.pr = ('gh pr create --repo external/package --base master '
                   '--head benthamite:fix --title "Fix outline actions" '
                   f'--body-file {self.body}')
        fakebin = self.root / "fakebin"
        fakebin.mkdir()
        gh = fakebin / "gh"
        gh.write_text('#!/bin/sh\n[ "$*" = "api --hostname github.com user --jq .login" ] || exit 90\nprintf "%s\\n" "${TEST_LOGIN:-benthamite}"\n')
        gh.chmod(0o755)
        self.env = dict(os.environ, PATH=str(fakebin) + os.pathsep + os.environ["PATH"], GH_HOST="github.com")
        self.git("init", "-q")
        self.git("config", "user.name", "Fixture")
        self.git("config", "user.email", "fixture@example.invalid")
        self.write_grants([])
        self.git("add", ".")
        self.commit()

    def git(self, *args):
        return subprocess.run(["git", "-C", str(self.root), "-c", "core.hooksPath=/dev/null", *args],
                              check=True, capture_output=True, text=True)

    def commit(self):
        self.git("commit", "-qm", "Fixture grant", "--no-gpg-sign")

    def record(self, command, **changes):
        now = datetime.now(timezone.utc)
        record = dict(command=command, account="benthamite", authorization="Explicit fixture authorization",
                      created_at=(now - timedelta(minutes=1)).isoformat(),
                      expires_at=(now + timedelta(hours=1)).isoformat(),
                      body_sha256=hashlib.sha256(self.body.read_bytes()).hexdigest() if command == self.pr else None)
        record.update(changes)
        return record

    def write_grants(self, grants, commit=False):
        self.manifest.write_text(json.dumps(dict(version=1, grants=grants)))
        if commit:
            self.git("add", "agents/github-operation-authorizations.json")
            self.commit()

    def check(self, command, expected, nested=False):
        for runtime in ("claude", "codex"):
            if nested and runtime == "claude":
                continue
            with self.subTest(runtime=runtime, command=command):
                if nested:
                    payload = dict(tool_name="functions.exec", tool_input=dict(input=
                        "await tools.exec_command(" + json.dumps(dict(cmd=command, workdir=str(self.root))) + ");"))
                else:
                    payload = dict(tool_name="Bash" if runtime == "claude" else "functions.exec_command",
                                   tool_input={"command" if runtime == "claude" else "cmd": command})
                result = subprocess.run(["bash", str(self.root / runtime / "hooks/block-github-write-command.sh")],
                                        input=json.dumps(payload), text=True, capture_output=True,
                                        cwd=self.root, env=self.env)
                allowed = result.returncode == 0 and not result.stdout.strip()
                self.assertEqual(allowed, expected, (result.stdout, result.stderr))

    def test_no_grant_and_working_tree_grant_do_not_authorize(self):
        self.check(FORK, False)
        self.write_grants([self.record(FORK)])
        self.check(FORK, False)
        self.git("add", "agents/github-operation-authorizations.json")
        self.check(FORK, False)

    def test_committed_grants_allow_exact_operations_in_both_entrypoints(self):
        self.write_grants([self.record(FORK), self.record(self.pr)], commit=True)
        for command in (FORK, self.pr):
            self.check(command, True)
            self.check(command, True, nested=True)

    def test_other_targets_actions_and_shell_are_denied(self):
        self.write_grants([self.record(FORK), self.record(self.pr)], commit=True)
        for command in (FORK.replace("external/package", "external/other"),
                        FORK + " --org other", self.pr.replace("--base master", "--base other"),
                        "gh pr merge --repo external/package 1", "gh issue create --repo external/package --title T --body B",
                        self.pr + "; touch /tmp/extra", self.pr + " && gh pr merge --repo external/package 1",
                        self.pr.replace("Fix outline actions", "$(touch /tmp/extra)")):
            self.check(command, False)

    def test_body_mutation_account_and_host_mismatch_deny(self):
        self.write_grants([self.record(FORK), self.record(self.pr)], commit=True)
        self.body.write_text("Different unreviewed content")
        self.check(self.pr, False)
        self.env["TEST_LOGIN"] = "other"
        self.check(FORK, False)
        self.env["TEST_LOGIN"] = "benthamite"
        self.env["GH_HOST"] = "example.invalid"
        self.check(FORK, False)

    def test_expired_overlong_and_future_grants_deny(self):
        now = datetime.now(timezone.utc)
        for created, expires in ((now - timedelta(hours=2), now - timedelta(hours=1)),
                                 (now - timedelta(hours=1), now + timedelta(days=2)),
                                 (now + timedelta(hours=1), now + timedelta(hours=2))):
            self.write_grants([self.record(FORK, created_at=created.isoformat(), expires_at=expires.isoformat())], commit=True)
            self.check(FORK, False)

    def test_unsupported_commands_cannot_gain_authority_even_in_manifest(self):
        for command in ("gh pr merge --repo external/package 1", FORK + " --org other", self.pr + " --repo benthamite/other"):
            self.write_grants([self.record(command)], commit=True)
            self.check(command, False)

    def test_removing_committed_grant_revokes_access(self):
        self.write_grants([self.record(FORK)], commit=True)
        self.check(FORK, True)
        self.write_grants([], commit=True)
        self.check(FORK, False)

    def test_invalid_matching_record_fails_closed_for_allowlisted_repo(self):
        command = self.pr.replace("external/package", "benthamite/package")
        record = self.record(command, authorization=42)
        self.write_grants([record], commit=True)
        self.check(command, False)

    def test_prepare_is_inert_and_binds_body(self):
        command_file = self.root / "command.txt"
        command_file.write_text(self.pr + "\n")
        result = subprocess.run([str(self.root / "bin/github-operation-authorization"), "prepare",
                                 "--command-file", str(command_file), "--account", "benthamite",
                                 "--authorization", "Explicit fixture authorization"],
                                capture_output=True, text=True, check=True)
        record = json.loads(result.stdout)
        self.assertEqual(record["body_sha256"], hashlib.sha256(self.body.read_bytes()).hexdigest())
        self.assertEqual(json.loads(self.manifest.read_text())["grants"], [])
        self.check(self.pr, False)


if __name__ == "__main__":
    unittest.main()
