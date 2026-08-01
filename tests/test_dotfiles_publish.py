"""Integration tests for guarded dotfiles publication.

Every test is offline: the "remote" is a local bare repository and the secret
scanner is a test double.  The literal below is the only secret-shaped value
used anywhere in the suite, so the redaction tests can assert that it never
reaches standard output, standard error, or persisted state.
"""

from __future__ import annotations

import json
import os
import re
import shutil
import stat
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
PUBLISH = REPO_ROOT / "bin" / "dotfiles-publish"
PRE_PUSH = REPO_ROOT / "bin" / "dotfiles-pre-push"
BLOCKED_TRANSPORT = REPO_ROOT / "bin" / "git-remote-dotfiles-blocked"

TEST_SECRET = "DOTFILES_TEST_SECRET_7b4c1fa9e62d"
BLOCKED_PUSH_MESSAGE = "Push blocked: use publish-dotfiles so outgoing history is reviewed."

FIXED_NOW = 1754000000
DAY = 86400

GIT = shutil.which("git") or "/usr/bin/git"


FAKE_GITLEAKS = r'''"""Test double for gitleaks.

It accepts the production command line, really inspects the requested commit
range or directory, and writes a gitleaks-shaped JSON report.  It deliberately
leaves the raw matched value in the report so the tests can prove that
dotfiles-publish redacts scanner output on its own instead of trusting the
scanner's own --redact flag.
"""

import json
import os
import re
import subprocess
import sys

VALUE_OPTIONS = {"--config", "-c", "--report-path", "-r", "--report-format", "-f", "--log-opts"}


def option(argv, name):
    prefix = name + "="
    for index, arg in enumerate(argv):
        if arg.startswith(prefix):
            return arg[len(prefix):]
        if arg == name and index + 1 < len(argv):
            return argv[index + 1]
    return None


def patterns():
    raw = os.environ.get(
        "DOTFILES_FAKE_GITLEAKS_PATTERNS", "DOTFILES_TEST_SECRET_[0-9a-f]{12}"
    )
    return [re.compile(item) for item in raw.split("|||") if item]


def record(value, path, commit, line_number, line):
    column = line.find(value) + 1
    return {
        "RuleID": "fake-generic-api-key",
        "Description": "fake generic api key",
        "File": path,
        "Commit": commit,
        "StartLine": line_number,
        "EndLine": line_number,
        "StartColumn": column,
        "EndColumn": column + len(value) - 1,
        "Match": line.strip(),
        "Secret": value,
        "Entropy": 4.2,
        "Fingerprint": "%s:%s:fake-generic-api-key:%d" % (commit, path, line_number),
    }


def scan_git(repo, log_opts, compiled):
    rev_args = ["--all"] if log_opts in (None, "", "--all") else [log_opts]
    listing = subprocess.run(
        ["git", "-C", repo, "rev-list", "--reverse", *rev_args],
        capture_output=True,
        text=True,
        check=True,
    )
    findings = []
    for commit in listing.stdout.split():
        patch = subprocess.run(
            ["git", "-C", repo, "show", "--format=", "--patch", "--root", commit],
            capture_output=True,
            text=True,
        ).stdout
        path = ""
        line_number = 0
        for line in patch.splitlines():
            if line.startswith("+++ "):
                path = line[4:]
                if path.startswith("b/"):
                    path = path[2:]
                continue
            hunk = re.match(r"^@@ -\d+(?:,\d+)? \+(\d+)", line)
            if hunk:
                line_number = int(hunk.group(1)) - 1
                continue
            if line.startswith("-"):
                continue
            if line.startswith("+") or line.startswith(" "):
                line_number += 1
            if not line.startswith("+"):
                continue
            for pattern in compiled:
                match = pattern.search(line)
                if match:
                    findings.append(
                        record(match.group(0), path, commit, line_number, line[1:])
                    )
    return findings


def scan_dir(root, compiled):
    findings = []
    for base, directories, files in os.walk(root):
        if ".git" in directories:
            directories.remove(".git")
        for name in sorted(files):
            full = os.path.join(base, name)
            relative = os.path.relpath(full, root)
            try:
                with open(full, "r", encoding="utf-8", errors="replace") as handle:
                    text = handle.read()
            except OSError:
                continue
            for number, line in enumerate(text.splitlines(), start=1):
                for pattern in compiled:
                    match = pattern.search(line)
                    if match:
                        findings.append(record(match.group(0), relative, "", number, line))
    return findings


def main():
    argv = sys.argv[1:]
    log_path = os.environ.get("DOTFILES_FAKE_GITLEAKS_LOG")
    if log_path:
        with open(log_path, "a", encoding="utf-8") as handle:
            handle.write(json.dumps(argv) + "\n")
    if argv and argv[0] == "version":
        sys.stdout.write("8.30.0\n")
        return 0
    forced = os.environ.get("DOTFILES_FAKE_GITLEAKS_FAIL")
    if forced:
        sys.stderr.write(os.environ.get("DOTFILES_FAKE_GITLEAKS_FAIL_TEXT", "fake scanner crash") + "\n")
        return int(forced)
    report = option(argv, "--report-path")
    mode = argv[0] if argv else ""
    positional = []
    skip = False
    for arg in argv[1:]:
        if skip:
            skip = False
            continue
        if arg.startswith("-"):
            if arg in VALUE_OPTIONS:
                skip = True
            continue
        positional.append(arg)
    target = positional[-1] if positional else "."
    compiled = patterns()
    if mode == "dir":
        findings = scan_dir(target, compiled)
    else:
        findings = scan_git(target, option(argv, "--log-opts"), compiled)
    if report:
        with open(report, "w", encoding="utf-8") as handle:
            json.dump(findings, handle)
    return 1 if findings else 0


if __name__ == "__main__":
    sys.exit(main())
'''


class PublicationFixture(unittest.TestCase):
    """Disposable source repository, bare remote, and scanner double."""

    def setUp(self):
        self.workspace = tempfile.TemporaryDirectory(prefix="dotfiles-publish-")
        self.addCleanup(self.workspace.cleanup)
        self.base = Path(self.workspace.name)
        self.home = self.base / "home"
        self.tools = self.base / "tools"
        self.home.mkdir()
        self.tools.mkdir()
        self.now = FIXED_NOW
        self.scanner = self.tools / "gitleaks-double"
        self.scanner.write_text("#!%s\n%s" % (sys.executable, FAKE_GITLEAKS))
        self.scanner.chmod(0o755)
        self.scanner_log = self.base / "gitleaks-argv.log"
        self.remote = self.base / "remote.git"
        subprocess.run([GIT, "init", "--quiet", "--bare", str(self.remote)], check=True)
        self.repo = self.base / "source"
        subprocess.run(
            [GIT, "init", "--quiet", "--initial-branch=master", str(self.repo)],
            check=True,
            env=self.git_env(),
        )
        for name, value in (
            ("user.email", "test@example.invalid"),
            ("user.name", "Publication Test"),
            ("commit.gpgsign", "false"),
            ("tag.gpgsign", "false"),
            ("core.autocrlf", "false"),
        ):
            self.git("config", name, value)

    # -- process helpers ------------------------------------------------

    def path_value(self):
        return os.pathsep.join(
            [
                str(self.tools),
                str(REPO_ROOT / "bin"),
                str(Path(GIT).parent),
                "/usr/bin",
                "/bin",
            ]
        )

    def git_env(self, **extra):
        env = {
            "PATH": self.path_value(),
            "HOME": str(self.home),
            "GIT_CONFIG_GLOBAL": "/dev/null",
            "GIT_CONFIG_SYSTEM": "/dev/null",
            "GIT_TERMINAL_PROMPT": "0",
            "GIT_AUTHOR_DATE": "2026-01-01T00:00:00+00:00",
            "GIT_COMMITTER_DATE": "2026-01-01T00:00:00+00:00",
            "LANG": "C",
        }
        env.update(extra)
        return env

    def git(self, *args, cwd=None, check=True, env=None):
        return subprocess.run(
            [GIT, *args],
            cwd=str(cwd or self.repo),
            capture_output=True,
            text=True,
            check=check,
            env=env or self.git_env(),
        )

    def env(self, **extra):
        env = self.git_env()
        env.update(
            {
                "DOTFILES_PUBLISH_TESTING": "1",
                "DOTFILES_PUBLISH_NOW": str(self.now),
                "DOTFILES_PUBLISH_GITLEAKS": str(self.scanner),
                "DOTFILES_FAKE_GITLEAKS_LOG": str(self.scanner_log),
            }
        )
        env.update(extra)
        return env

    def cli(self, *args, cwd=None, stdin=None, env=None):
        self.assertTrue(
            PUBLISH.exists(), "bin/dotfiles-publish does not exist yet"
        )
        return subprocess.run(
            [sys.executable, str(PUBLISH), *args],
            cwd=str(cwd or self.repo),
            input=stdin,
            capture_output=True,
            text=True,
            env=env or self.env(),
        )

    # -- repository helpers ---------------------------------------------

    def commit(self, message, files=None, remove=()):
        for path, content in (files or {}).items():
            target = self.repo / path
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(content)
        for path in remove:
            (self.repo / path).unlink()
        self.git("add", "-A")
        self.git("commit", "--quiet", "-m", message)
        return self.head()

    def head(self):
        return self.git("rev-parse", "HEAD").stdout.strip()

    def publish_base(self):
        """Create the published base commit and track it on the bare remote."""
        self.commit("public base", {"README.md": "public dotfiles\n"})
        self.git(
            "-c",
            "core.hooksPath=/dev/null",
            "push",
            "--quiet",
            "--set-upstream",
            str(self.remote),
            "master:refs/heads/master",
        )
        self.git("remote", "add", "origin", str(self.remote))
        self.git("fetch", "--quiet", "origin")
        self.git("branch", "--set-upstream-to=origin/master", "master")
        return self.head()

    def remote_tip(self, ref="refs/heads/master"):
        listing = self.git("ls-remote", str(self.remote), ref)
        if not listing.stdout.strip():
            return None
        return listing.stdout.split()[0]

    def state_dir(self):
        return self.repo / ".git" / "dotfiles-publish"

    def run_dir(self, run_id):
        return self.state_dir() / "runs" / run_id

    def read_json(self, path):
        return json.loads(Path(path).read_text())

    # -- workflow helpers -----------------------------------------------

    def scan(self, *extra, env=None):
        proc = self.cli("scan", *extra, env=env)
        return proc, self.run_id_of(proc)

    def run_id_of(self, proc):
        match = re.search(r"^run: (\S+)$", proc.stdout, re.MULTILINE)
        self.assertIsNotNone(
            match, "scan did not report a run id:\n%s\n%s" % (proc.stdout, proc.stderr)
        )
        return match.group(1)

    def unit_ids(self, run_id):
        listing = self.cli("review-list", "--run", run_id)
        self.assertEqual(0, listing.returncode, listing.stderr)
        return [line.split()[0] for line in listing.stdout.splitlines() if line.strip()]

    def review_all(self, run_id, skip=()):
        recorded = []
        for unit_id in self.unit_ids(run_id):
            if unit_id in skip:
                continue
            proc = self.cli(
                "review-record",
                "--run",
                run_id,
                "--unit",
                unit_id,
                "--verdict",
                "clean",
            )
            self.assertEqual(0, proc.returncode, proc.stderr)
            recorded.append(unit_id)
        return recorded

    def write_full_audit_receipt(self, run_id, completed_epoch=None):
        """Record a clean full audit without running one, for publication tests."""
        run = self.read_json(self.run_dir(run_id) / "run.json")
        receipt = {
            "schema": 1,
            "completed_epoch": completed_epoch if completed_epoch is not None else self.now,
            "completed_at": "2026-01-01T00:00:00Z",
            "ruleset_id": run["ruleset_id"],
            "run_id": run_id,
            "public_refs": {},
            "incident_flag": False,
        }
        path = self.state_dir() / "full-audit.json"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(receipt))
        path.chmod(0o600)
        return receipt

    def prepare_authorized_run(self):
        """Clean scan, exhaustive review, and a fresh full-audit receipt."""
        self.publish_base()
        self.commit("add helper", {"docs/helper.md": "helper notes\n"})
        proc, run_id = self.scan()
        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        self.write_full_audit_receipt(run_id)
        self.review_all(run_id)
        return run_id

    def state_files(self):
        for path in sorted(self.state_dir().rglob("*")):
            if path.is_file():
                yield path


class DotfilesPublishScanTests(PublicationFixture):
    def test_scan_covers_secret_added_then_deleted_before_head(self):
        self.publish_base()
        leaking = self.commit(
            "add configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )
        self.commit("clean configuration", {"config/service.conf": "token = from-op\n"})
        candidate = self.head()

        proc, run_id = self.scan()

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        commits = {finding["commit"] for finding in findings}
        self.assertIn(
            leaking,
            commits,
            "the intermediate commit that introduced the secret was not scanned",
        )
        invocations = [
            json.loads(line)
            for line in self.scanner_log.read_text().splitlines()
            if line.strip()
        ]
        ranges = [
            argument
            for argv in invocations
            for argument in argv
            if argument.startswith("--log-opts=")
        ]
        self.assertIn(
            "--log-opts=%s..%s" % (self.remote_tip(), candidate),
            ranges,
            "the scanner was not pointed at the exact outgoing range",
        )

    def test_git_crypt_path_with_plaintext_historical_blob_is_rejected(self):
        self.publish_base()
        self.commit(
            "encrypt secrets",
            {".gitattributes": "secrets/service.env filter=git-crypt diff=git-crypt\n"},
        )
        plaintext = self.commit(
            "add unencrypted secrets file",
            {"secrets/service.env": "SERVICE_TOKEN=plaintext-not-encrypted\n"},
        )

        proc, run_id = self.scan()

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        matching = [
            finding
            for finding in findings
            if finding["path"] == "secrets/service.env"
            and "git-crypt" in finding["rule_id"]
        ]
        self.assertTrue(
            matching,
            "a git-crypt path stored as plaintext was not reported: %s" % findings,
        )
        self.assertIn(plaintext, {finding["commit"] for finding in matching})


class DotfilesPublishRedactionTests(PublicationFixture):
    def test_scan_redacts_value_from_stdout_stderr_and_persisted_state(self):
        self.publish_base()
        self.commit(
            "add configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )

        proc, run_id = self.scan()

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        self.assertNotIn(TEST_SECRET, proc.stdout)
        self.assertNotIn(TEST_SECRET, proc.stderr)
        leaked = [
            str(path)
            for path in self.state_files()
            if TEST_SECRET.encode() in path.read_bytes()
        ]
        self.assertEqual([], leaked, "the secret value reached persisted state")
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        self.assertIn("[REDACTED:", json.dumps(manifest))


class DotfilesPublishManifestTests(PublicationFixture):
    def test_manifest_has_every_commit_path_patch_and_full_new_file(self):
        self.publish_base()
        first = self.commit(
            "extend readme and add a temporary file",
            {"README.md": "public dotfiles\nsecond line\n", "scratch.txt": "temporary\n"},
        )
        second = self.commit(
            "add a module and drop the temporary file",
            {"module/new.py": 'print("hello")\n', "deploy.log": "started\n"},
            remove=("scratch.txt",),
        )

        proc, run_id = self.scan()

        self.assertIn(proc.returncode, (0, 2), proc.stdout + proc.stderr)
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        units = manifest["units"]
        commit_units = {unit["commit"] for unit in units if unit["kind"] == "commit"}
        self.assertEqual({first, second}, commit_units)
        path_units = {
            (unit["commit"], unit["path"]) for unit in units if unit["kind"] == "path"
        }
        self.assertEqual(
            {
                (first, "README.md"),
                (first, "scratch.txt"),
                (second, "module/new.py"),
                (second, "deploy.log"),
                (second, "scratch.txt"),
            },
            path_units,
        )
        self.assertTrue([unit for unit in units if unit["kind"] == "patch"])
        new_files = {
            unit["path"]: unit["content"] for unit in units if unit["kind"] == "new-file"
        }
        self.assertEqual('print("hello")\n', new_files["module/new.py"])
        self.assertIn("scratch.txt", new_files)
        risky = {unit["path"] for unit in units if unit["kind"] == "high-risk-file"}
        self.assertIn("deploy.log", risky)
        self.assertEqual(manifest["counts"]["commit"], len(commit_units))

    def test_authorize_refuses_incomplete_llm_review(self):
        run_id = self.prepare_authorized_run()
        units = self.unit_ids(run_id)
        omitted = units[-1]

        review = self.read_json(self.run_dir(run_id) / "review.json")
        del review["entries"][omitted]
        (self.run_dir(run_id) / "review.json").write_text(json.dumps(review))

        refused = self.cli("authorize", "--run", run_id)
        self.assertNotEqual(0, refused.returncode)
        self.assertIn(omitted, refused.stdout + refused.stderr)
        self.assertFalse((self.state_dir() / "authorization.json").exists())

        review["entries"][omitted] = {"verdict": "clean"}
        review["entries"]["forged-unit-id"] = {"verdict": "clean"}
        (self.run_dir(run_id) / "review.json").write_text(json.dumps(review))

        forged = self.cli("authorize", "--run", run_id)
        self.assertNotEqual(0, forged.returncode)
        self.assertIn("forged-unit-id", forged.stdout + forged.stderr)
        self.assertFalse((self.state_dir() / "authorization.json").exists())


class DotfilesPublishRepairTests(PublicationFixture):
    def test_repair_ref_survives_failure_and_is_removed_after_verified_push(self):
        self.publish_base()
        self.commit(
            "add configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )
        proc, run_id = self.scan()
        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)

        started = self.cli("repair-start", "--run", run_id)
        self.assertEqual(0, started.returncode, started.stderr)
        recovery = "refs/dotfiles-publish/recovery/%s" % run_id
        self.assertTrue(self.git("rev-parse", "--verify", recovery, check=False).returncode == 0)

        self.git("reset", "--quiet", "--hard", self.remote_tip())
        self.commit("add configuration", {"config/service.conf": "token = from-op\n"})
        verified = self.cli(
            "repair-verify", "--run", run_id, "--allowed-path", "config/service.conf"
        )
        self.assertEqual(0, verified.returncode, verified.stdout + verified.stderr)
        self.assertEqual(
            0,
            self.git("rev-parse", "--verify", recovery, check=False).returncode,
            "the recovery ref must survive until a verified push",
        )

        clean, new_run = self.scan()
        self.assertEqual(0, clean.returncode, clean.stdout + clean.stderr)
        self.write_full_audit_receipt(new_run)
        self.review_all(new_run)

        failed = self.cli(
            "push", "--run", new_run, env=self.env(DOTFILES_PUBLISH_FORCE_PUSH_FAILURE="1")
        )
        self.assertNotEqual(0, failed.returncode)
        self.assertEqual(
            0,
            self.git("rev-parse", "--verify", recovery, check=False).returncode,
            "a failed push must keep the recovery ref",
        )

        pushed = self.cli("push", "--run", new_run)
        self.assertEqual(0, pushed.returncode, pushed.stdout + pushed.stderr)
        self.assertNotEqual(
            0,
            self.git("rev-parse", "--verify", recovery, check=False).returncode,
            "a verified push must remove the recovery ref",
        )


class DotfilesPublishAuthorizationTests(PublicationFixture):
    def hook_input(self, run_id):
        run = self.read_json(self.run_dir(run_id) / "run.json")
        return "%s %s %s %s\n" % (
            run["candidate_oid"],
            run["candidate_oid"],
            run["remote_ref"],
            run["remote_oid"],
        )

    def test_authorization_is_exact_short_lived_and_single_use(self):
        run_id = self.prepare_authorized_run()

        authorized = self.cli("authorize", "--run", run_id)
        self.assertEqual(0, authorized.returncode, authorized.stdout + authorized.stderr)
        record = self.read_json(self.state_dir() / "authorization.json")
        self.assertEqual(run_id, record["run_id"])
        self.assertEqual(1, len(record["updates"]))
        self.assertEqual(
            0o600, stat.S_IMODE((self.state_dir() / "authorization.json").stat().st_mode)
        )

        accepted = self.cli(
            "hook", "origin", str(self.remote), stdin=self.hook_input(run_id)
        )
        self.assertEqual(0, accepted.returncode, accepted.stdout + accepted.stderr)
        self.assertFalse(
            (self.state_dir() / "authorization.json").exists(),
            "authorization must be consumed by the hook",
        )

        replayed = self.cli(
            "hook", "origin", str(self.remote), stdin=self.hook_input(run_id)
        )
        self.assertNotEqual(0, replayed.returncode)

        self.cli("authorize", "--run", run_id)
        expired = self.cli(
            "hook",
            "origin",
            str(self.remote),
            stdin=self.hook_input(run_id),
            env=self.env(DOTFILES_PUBLISH_NOW=str(self.now + 301)),
        )
        self.assertNotEqual(0, expired.returncode)
        self.assertIn("expired", (expired.stdout + expired.stderr).lower())

    def test_changed_head_remote_ref_remote_tip_or_ruleset_invalidates_authorization(self):
        run_id = self.prepare_authorized_run()
        self.assertEqual(0, self.cli("authorize", "--run", run_id).returncode)

        moved = self.commit("later work", {"docs/later.md": "later\n"})
        proposed = "%s %s refs/heads/master %s" % (
            moved,
            moved,
            self.remote_tip(),
        )
        rejected = self.cli(
            "hook", "origin", str(self.remote), stdin=proposed + "\n"
        )
        self.assertNotEqual(0, rejected.returncode)

        self.git("reset", "--quiet", "--hard", "HEAD~1")
        (self.repo / ".gitleaks.toml").write_text("[extend]\nuseDefault = true\n")
        stale_ruleset = self.cli("authorize", "--run", run_id)
        self.assertNotEqual(0, stale_ruleset.returncode)
        self.assertIn("ruleset", (stale_ruleset.stdout + stale_ruleset.stderr).lower())
        (self.repo / ".gitleaks.toml").unlink()

        other = self.base / "other"
        subprocess.run(
            [GIT, "clone", "--quiet", str(self.remote), str(other)],
            check=True,
            env=self.git_env(),
        )
        for name, value in (("user.email", "o@example.invalid"), ("user.name", "Other")):
            self.git("config", name, value, cwd=other)
        (other / "README.md").write_text("public dotfiles\nremote work\n")
        self.git("add", "-A", cwd=other)
        self.git("commit", "--quiet", "-m", "remote work", cwd=other)
        self.git("-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "master", cwd=other)

        stale_remote = self.cli("authorize", "--run", run_id)
        self.assertNotEqual(0, stale_remote.returncode)

    def test_pre_push_rejects_without_authorization(self):
        run_id = self.prepare_authorized_run()
        self.assertFalse((self.state_dir() / "authorization.json").exists())

        rejected = self.cli(
            "hook", "origin", str(self.remote), stdin=self.hook_input(run_id)
        )
        self.assertNotEqual(0, rejected.returncode)
        self.assertIn("publish-dotfiles", rejected.stdout + rejected.stderr)

        self.assertTrue(PRE_PUSH.exists(), "bin/dotfiles-pre-push does not exist yet")
        wrapper = subprocess.run(
            [str(PRE_PUSH), "origin", str(self.remote)],
            cwd=str(self.repo),
            input=self.hook_input(run_id),
            capture_output=True,
            text=True,
            env=self.env(),
        )
        self.assertNotEqual(0, wrapper.returncode)

    def test_refusing_transport_blocks_push_even_with_no_verify(self):
        self.publish_base()
        before = self.remote_tip()
        self.commit("unreviewed work", {"docs/unreviewed.md": "unreviewed\n"})
        self.assertTrue(
            BLOCKED_TRANSPORT.exists(), "bin/git-remote-dotfiles-blocked does not exist yet"
        )
        self.git("config", "remote.origin.pushurl", "dotfiles-blocked::origin")

        blocked = self.git(
            "push", "--no-verify", "origin", "master", check=False
        )
        self.assertNotEqual(0, blocked.returncode)
        self.assertIn(BLOCKED_PUSH_MESSAGE, blocked.stderr)
        self.assertEqual(before, self.remote_tip(), "the remote must not advance")

    def test_publish_pushes_only_candidate_to_configured_branch(self):
        run_id = self.prepare_authorized_run()
        candidate = self.head()
        self.git("tag", "v99.99.99")

        pushed = self.cli("push", "--run", run_id)

        self.assertEqual(0, pushed.returncode, pushed.stdout + pushed.stderr)
        self.assertEqual(candidate, self.remote_tip())
        listing = self.git("ls-remote", str(self.remote))
        self.assertNotIn("refs/tags/", listing.stdout)
        self.assertEqual(
            ["refs/heads/master"],
            sorted({line.split()[1] for line in listing.stdout.splitlines() if line.strip()}),
        )


class DotfilesPublishFullAuditTests(PublicationFixture):
    def test_full_audit_due_after_thirty_days_or_ruleset_change(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})

        first, run_id = self.scan()
        self.assertIn("full-audit-due: true", first.stdout)

        self.write_full_audit_receipt(run_id)

        fresh, _ = self.scan(env=self.env(DOTFILES_PUBLISH_NOW=str(self.now + 29 * DAY)))
        self.assertIn("full-audit-due: false", fresh.stdout)

        stale, _ = self.scan(env=self.env(DOTFILES_PUBLISH_NOW=str(self.now + 31 * DAY)))
        self.assertIn("full-audit-due: true", stale.stdout)

        (self.repo / ".gitleaks.toml").write_text("[extend]\nuseDefault = true\n")
        changed, _ = self.scan()
        self.assertIn("full-audit-due: true", changed.stdout)

    def test_full_audit_finding_on_public_ref_is_classified_as_incident(self):
        self.publish_base()
        self.commit(
            "publish configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )
        self.git(
            "-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "master"
        )
        self.git("fetch", "--quiet", "origin")

        proc, run_id = self.scan("--mode", "full-audit")

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        self.assertNotIn(TEST_SECRET, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        classes = {finding.get("classification") for finding in findings}
        self.assertIn("public-incident", classes)
        self.assertIn("gitguardian-triage", proc.stdout)


if __name__ == "__main__":
    unittest.main()
