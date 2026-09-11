"""Integration tests for guarded dotfiles publication.

Every test is offline: the "remote" is a local bare repository and the secret
scanner is a test double except for the synthetic local-scanner regression.
The literal below is the only secret-shaped value
used anywhere in the suite, so the redaction tests can assert that it never
reaches standard output, standard error, or persisted state.
"""

from __future__ import annotations

import copy
import concurrent.futures
import hashlib
import json
import os
import re
import runpy
import shutil
import stat
import subprocess
import sys
import tempfile
import time
import threading
import unittest
from pathlib import Path
from types import SimpleNamespace

REPO_ROOT = Path(__file__).resolve().parent.parent
PUBLISH = REPO_ROOT / "bin" / "dotfiles-publish"
PRE_PUSH = REPO_ROOT / "bin" / "dotfiles-pre-push"
BLOCKED_TRANSPORT = REPO_ROOT / "bin" / "git-remote-dotfiles-blocked"

TEST_SECRET = "DOTFILES_TEST_SECRET_7b4c1fa9e62d"
BLOCKED_PUSH_MESSAGE = "Push blocked: use publish-dotfiles so outgoing history is reviewed."

FIXED_NOW = 1754000000
DAY = 86400

GIT = shutil.which("git") or "/usr/bin/git"


def option_from_argv(argv, name):
    prefix = name + "="
    for index, argument in enumerate(argv):
        if argument.startswith(prefix):
            return argument[len(prefix) :]
        if argument == name and index + 1 < len(argv):
            return argv[index + 1]
    return None


FAKE_GITLEAKS = r'''"""Test double for gitleaks.

It accepts the production command line, really inspects the requested commit
range or directory, and writes a gitleaks-shaped JSON report.  It honors the
scanner's --redact flag so tests fail if dotfiles-publish prevents its own
redactor from learning the raw value.
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
    character_offset = line.find(value)
    column = len(line[:character_offset].encode("utf-8")) + 1
    value_length = len(value.encode("utf-8"))
    return {
        "RuleID": "fake-generic-api-key",
        "Description": "fake generic api key",
        "File": path,
        "Commit": commit,
        "StartLine": line_number,
        "EndLine": line_number,
        "StartColumn": column,
        "EndColumn": column + value_length - 1,
        "Line": line,
        "Match": line.strip(),
        "Secret": value,
        "Entropy": 4.2,
        "Fingerprint": "%s:%s:fake-generic-api-key:%d" % (commit, path, line_number),
    }


def scan_git(repo, log_opts, compiled):
    options = log_opts.split() if log_opts else []
    patch_options = [arg for arg in options if arg in ("--no-textconv", "--no-ext-diff")]
    rev_args = [arg for arg in options if arg not in patch_options] or ["--all"]
    git_environment = os.environ.copy()
    git_environment["DOTFILES_FAKE_GITLEAKS_CHILD"] = "1"
    listing = subprocess.run(
        ["git", "-C", repo, "rev-list", "--reverse", *rev_args],
        capture_output=True,
        text=True,
        check=True,
        env=git_environment,
    )
    findings = []
    for commit in listing.stdout.split():
        patch = subprocess.run(
            ["git", "-C", repo, "show", "--format=", "--patch", "--root", *patch_options, commit],
            capture_output=True,
            text=True,
            env=git_environment,
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
            try:
                with open(full, "r", encoding="utf-8", errors="replace") as handle:
                    text = handle.read()
            except OSError:
                continue
            for number, line in enumerate(text.splitlines(), start=1):
                for pattern in compiled:
                    match = pattern.search(line)
                    if match:
                        # Real directory scans put the absolute export path
                        # in both File and Fingerprint.
                        findings.append(record(match.group(0), full, "", number, line))
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
    required_mode = os.environ.get("DOTFILES_FAKE_GITLEAKS_REQUIRE_REPORT_MODE")
    if report and required_mode:
        try:
            actual_mode = os.stat(report).st_mode & 0o777
        except OSError:
            actual_mode = -1
        if actual_mode != int(required_mode, 8):
            sys.stderr.write("report path is not pre-created with the required mode\n")
            return 8
    required_directory_mode = os.environ.get(
        "DOTFILES_FAKE_GITLEAKS_REQUIRE_REPORT_DIRECTORY_MODE"
    )
    if report and required_directory_mode:
        actual_directory_mode = os.stat(os.path.dirname(report)).st_mode & 0o777
        if actual_directory_mode != int(required_directory_mode, 8):
            sys.stderr.write("report directory does not have the required mode\n")
            return 8
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
    reported_secret = os.environ.get("DOTFILES_FAKE_GITLEAKS_REPORTED_SECRET")
    if reported_secret:
        for finding in findings:
            source_value = finding["Secret"]
            finding["Secret"] = reported_secret
            finding["Match"] = finding["Match"].replace(source_value, reported_secret)
    if os.environ.get("DOTFILES_FAKE_GITLEAKS_INVALID_COLUMNS"):
        for finding in findings:
            finding["StartColumn"] = 999999
            finding["EndColumn"] = 1000000
    if mode != "dir" and os.environ.get(
        "DOTFILES_FAKE_GITLEAKS_INVALID_GIT_COORDINATES"
    ):
        for finding in findings:
            finding["StartLine"] = 999999
            finding["EndLine"] = 999999
            finding["StartColumn"] = 999999
            finding["EndColumn"] = 1000000
    if mode != "dir" and os.environ.get("DOTFILES_FAKE_GITLEAKS_OMIT_GIT_LINE"):
        for finding in findings:
            finding.pop("Line", None)
            finding["Tags"] = ["decoded:hex", "decode-depth:1"]
    if option(argv, "--redact"):
        for finding in findings:
            value = finding["Secret"]
            finding["Secret"] = "REDACTED"
            finding["Match"] = finding["Match"].replace(value, "REDACTED")
    attack = os.environ.get("DOTFILES_FAKE_GITLEAKS_REPORT_ATTACK")
    if report and attack in ("replace", "permissive", "symlink"):
        os.unlink(report)
        if attack == "symlink":
            os.symlink(os.devnull, report)
    if report:
        with open(report, "w", encoding="utf-8") as handle:
            json.dump(findings, handle)
        if attack in ("replace", "permissive"):
            os.chmod(report, 0o644 if attack == "permissive" else 0o600)
    if os.environ.get("DOTFILES_FAKE_GITLEAKS_PARTIAL_FAILURE"):
        return 1
    return int(option(argv, "--exit-code") or 1) if findings else 0


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
                "DOTFILES_FAKE_GITLEAKS_REQUIRE_REPORT_MODE": "600",
                "DOTFILES_FAKE_GITLEAKS_REQUIRE_REPORT_DIRECTORY_MODE": "700",
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
            if isinstance(content, bytes):
                target.write_bytes(content)
            else:
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

    def scan_from(self, directory, *extra, env=None):
        Path(directory).mkdir(parents=True, exist_ok=True)
        proc = self.cli("scan", *extra, cwd=directory, env=env)
        return proc, self.run_id_of(proc)

    def advance_remote_elsewhere(self, message="remote work"):
        """Move the bare remote forward from a separate clone."""
        clone = self.base / ("clone-%d" % len(list(self.base.glob("clone-*"))))
        subprocess.run(
            [GIT, "clone", "--quiet", str(self.remote), str(clone)],
            check=True,
            env=self.git_env(),
        )
        for name, value in (
            ("user.email", "other@example.invalid"),
            ("user.name", "Other Author"),
            ("commit.gpgsign", "false"),
        ):
            self.git("config", name, value, cwd=clone)
        (clone / "REMOTE.md").write_text(message + "\n")
        self.git("add", "-A", cwd=clone)
        self.git("commit", "--quiet", "-m", message, cwd=clone)
        self.git(
            "-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "HEAD:refs/heads/master", cwd=clone
        )
        return self.remote_tip()

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
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        audit_id = "fixture-audit-" + run_id
        completed = completed_epoch if completed_epoch is not None else self.now
        # The shortcut certifies a separate synthetic audit, never the outgoing
        # publication run: production receipts require a stored full-audit scan.
        run.update(
            run_id=audit_id, mode="full-audit", scanned_epoch=completed,
            scan_id="fixture-" + run["scan_id"],
        )
        manifest["run_id"] = audit_id
        audit_dir = self.run_dir(audit_id)
        audit_dir.mkdir(parents=True, mode=0o700, exist_ok=True)
        for name, payload in (("run.json", run), ("manifest.json", manifest)):
            path = audit_dir / name
            path.write_text(json.dumps(payload))
            path.chmod(0o600)
        receipt = {
            "schema": 4,
            "completed_epoch": completed,
            "completed_at": "2026-01-01T00:00:00Z",
            "ruleset_id": run["ruleset_id"],
            "run_id": audit_id,
            "scan_id": run["scan_id"],
            "manifest_id": manifest["manifest_id"],
            "public_refs": {},
            "incident_flag": False,
            "incident_digest": run["incident_digest"],
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

    def set_remote_writable(self, writable):
        """Make the bare remote reject writes, so a push fails like a network error."""
        for directory in [self.remote, *[path for path in self.remote.rglob("*") if path.is_dir()]]:
            directory.chmod(0o755 if writable else 0o555)

    def state_files(self):
        for path in sorted(self.state_dir().rglob("*")):
            if path.is_file():
                yield path

    def assert_run_redacts(self, proc, run_id, value, fingerprint):
        marker = "[REDACTED:%s]" % fingerprint
        self.assertNotIn(value, proc.stdout + proc.stderr)
        persisted = []
        for path in sorted(self.run_dir(run_id).rglob("*")):
            if not path.is_file():
                continue
            content = path.read_text(encoding="utf-8", errors="replace")
            self.assertNotIn(value, content, str(path))
            persisted.append(content)
        self.assertIn(marker, "\n".join(persisted))

        shown_units = []
        for unit_id in self.unit_ids(run_id):
            shown = self.cli("review-show", "--run", run_id, "--unit", unit_id)
            self.assertEqual(0, shown.returncode, shown.stderr)
            self.assertNotIn(value, shown.stdout)
            shown_units.append(shown.stdout)
        self.assertIn(marker, "\n".join(shown_units))

        invocations = [
            json.loads(line)
            for line in self.scanner_log.read_text().splitlines()
            if line.strip()
        ]
        self.assertFalse(
            [argument for argv in invocations for argument in argv if argument.startswith("--redact")]
        )
        for argv in invocations:
            report = option_from_argv(argv, "--report-path")
            self.assertIsNotNone(report)
            self.assertFalse(Path(report).exists(), "raw scanner report survived cleanup")


class DotfilesPublishDocumentationTests(unittest.TestCase):
    """The workflow is only usable if it is findable and its paths are real."""

    def test_the_tracked_executables_and_hook_exist_and_are_executable(self):
        for helper in (PUBLISH, PRE_PUSH, BLOCKED_TRANSPORT):
            self.assertTrue(helper.is_file(), helper)
            self.assertTrue(os.access(str(helper), os.X_OK), "%s is not executable" % helper)

    def test_shared_configuration_guide_names_the_project_local_skill_pair(self):
        overview = (REPO_ROOT / "agents" / "configuration.org").read_text(encoding="utf-8")
        self.assertIn("=.claude/skills/publish-dotfiles/=", overview)
        self.assertIn("=.codex/skills/publish-dotfiles/=", overview)
        self.assertIn("Guarded dotfiles publication", overview)

        root_readme = (REPO_ROOT / "README.org").read_text(encoding="utf-8")
        self.assertIn("guarded dotfiles workflow", root_readme)
        self.assertIn("agents/configuration.org", root_readme)
        landing = (REPO_ROOT / "agents" / "README.org").read_text(encoding="utf-8")
        self.assertIn("[[file:configuration.org]", landing)

    def test_the_documented_paths_are_the_real_ones(self):
        overview = (REPO_ROOT / "agents" / "configuration.org").read_text(encoding="utf-8")
        for path in ("bin/dotfiles-publish", "bin/dotfiles-pre-push", "bin/git-remote-dotfiles-blocked"):
            self.assertIn(path, overview, "the overview does not document %s" % path)
            self.assertTrue((REPO_ROOT / path).is_file(), "%s does not exist" % path)

    def test_the_skill_pair_and_its_evaluations_are_present(self):
        for side in (".claude", ".codex"):
            skill = REPO_ROOT / side / "skills" / "publish-dotfiles"
            self.assertTrue((skill / "SKILL.md").is_file(), skill)
            self.assertTrue((skill / "evals" / "scenarios.md").is_file(), skill)


class DotfilesPublishQuotedRedactionTests(unittest.TestCase):
    def setUp(self):
        self.functions = runpy.run_path(str(PUBLISH), run_name='isolated_quoted_redaction')

    def test_quoted_separators_and_escaped_quotes_keep_only_surrounding_text(self):
        for quote in ("'", '"'):
            for delimiter in (';', '#', ',', ')', ' ', '\\' + quote, '\\\\'):
                with self.subTest(quote=quote, delimiter=delimiter):
                    literal = quote + 'SYNTHETIC_PREFIX_123' + delimiter + 'SYNTHETIC_SUFFIX_456' + quote
                    source = 'Config(password=' + literal + ", username='fixture@example.invalid')"
                    self.assertEqual("Config(password=" + quote + '[REDACTED:structural]' + quote + ", username='fixture@example.invalid')", self.functions['structural_redaction'](source))

    def test_partial_scanner_match_cannot_erase_assignment_before_redaction(self):
        prefix = 'SYNTHETIC_PASSWORD_PREFIX_123'
        suffix = 'SYNTHETIC_SUFFIX_456'
        source = "Config(password='" + prefix + ';' + suffix + "', username='fixture@example.invalid')"
        redactor = self.functions['Redactor'](b'synthetic-only-key')
        report = [{'Secret': prefix, 'Match': "password='" + prefix,
                   'RuleID': 'dotfiles-credential-assignment', 'File': 'fixture.log',
                   'Commit': 'a' * 40, 'StartLine': 1, 'EndLine': 1,
                   'StartColumn': 18, 'EndColumn': 44}]
        records = self.functions['convert_gitleaks_results'](report, b'synthetic-only-key', redactor, 'gitleaks-history', None)
        marker = '[REDACTED:' + records[0]['fingerprint'] + ']'
        result = redactor.scrub(source)
        self.assertEqual("Config(password='" + marker + "', username='fixture@example.invalid')", result)
        self.assertEqual(result, redactor.scrub(result))

    def test_unquoted_shell_separators_do_not_hide_later_commands(self):
        for separator in ('; echo visible', ' # visible comment', ', other=visible', ') visible'):
            source = 'password=SYNTHETIC_PASSWORD_123' + separator
            self.assertEqual('password=[REDACTED:structural]' + separator, self.functions['structural_redaction'](source))

    def test_arrow_assignment_redacts_the_complete_literal(self):
        source = "password => 'SYNTHETIC_PREFIX_123;SYNTHETIC_SUFFIX_456'"
        self.assertEqual("password => '[REDACTED:structural]'", self.functions['structural_redaction'](source))

    def test_json_quoted_key_is_redacted(self):
        source = '{"password": "SYNTHETIC_PREFIX_123;SYNTHETIC_SUFFIX_456", "other": "visible"}'
        self.assertEqual('{"password": "[REDACTED:structural]", "other": "visible"}', self.functions['structural_redaction'](source))

    def test_old_findings_and_manifest_are_refused_without_changing_incidents(self):
        with tempfile.TemporaryDirectory(prefix='dotfiles-redaction-version-') as directory:
            state = Path(directory)
            repo = SimpleNamespace(state_dir=state)
            run_id = 'a' * 32
            run_dir = state / 'runs' / run_id
            run_dir.mkdir(parents=True)
            digest = self.functions['hashlib'].sha256(b'dotfiles-publish-manifest\x00').hexdigest()
            run = {'schema': self.functions['SCHEMA'], 'manifest_id': digest, 'mode': 'full-audit'}
            (run_dir / 'run.json').write_text(json.dumps(run))
            manifest = {'schema': self.functions['SCHEMA'], 'run_id': run_id, 'manifest_id': digest, 'units': []}
            findings = {'schema': self.functions['SCHEMA'], 'run_id': run_id, 'findings': []}
            incidents = {'schema': self.functions['SCHEMA'], 'incidents': {'synthetic-fingerprint': {'resolution': 'revoked'}}}
            incident_path = state / 'incidents.json'
            incident_path.write_text(json.dumps(incidents))
            before = incident_path.read_bytes()
            for name, data, loader in (('manifest.json', manifest, 'load_manifest'), ('findings.json', findings, 'load_findings')):
                path = run_dir / name
                path.write_text(json.dumps(data))
                with self.assertRaisesRegex(self.functions['PublishError'], 'predates safe quoted-value redaction'):
                    self.functions[loader](repo, run_id)
                data['redaction_version'] = self.functions['REDACTION_VERSION']
                path.write_text(json.dumps(data))
                self.assertEqual(data, self.functions[loader](repo, run_id))
            self.assertEqual(incidents, self.functions['load_incidents'](repo))
            self.assertEqual(before, incident_path.read_bytes())


class DotfilesPublishFingerprintTests(unittest.TestCase):
    def setUp(self):
        self.functions = runpy.run_path(str(PUBLISH))
        temporary = tempfile.TemporaryDirectory(prefix="dotfiles-fingerprint-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.key = b"synthetic-fingerprint-test-key"
        self.commit = "a" * 40

    def report(self, path, line=1, rule="fixture-token", scanner_fingerprint="scanner-id"):
        return {
            "RuleID": rule, "File": str(path), "StartLine": line, "EndLine": line,
            "StartColumn": 1, "EndColumn": len(TEST_SECRET),
            "Line": TEST_SECRET, "Match": TEST_SECRET, "Secret": TEST_SECRET,
            "Fingerprint": scanner_fingerprint,
        }

    def convert(self, report, directory=None, commit=None, source="gitleaks-tree"):
        redactor = self.functions["Redactor"](self.key)
        records = self.functions["convert_gitleaks_results"](
            [copy.deepcopy(report)], self.key, redactor, source, None,
            default_commit=commit or self.commit, strip_prefix=directory,
            directory=directory,
        )
        self.assertNotIn(TEST_SECRET, json.dumps(records))
        return records[0]

    def test_directory_identity_uses_each_repository_coordinate(self):
        for name in ("one", "two"):
            directory = self.root / name
            directory.mkdir()
            for filename in ("fixture.txt", "other.txt"):
                (directory / filename).write_text((TEST_SECRET + "\n") * 2)
        one, two = self.root / "one", self.root / "two"
        original = self.report(one / "fixture.txt")
        first = self.convert(original, one)
        second = self.convert(
            self.report(two / "fixture.txt", scanner_fingerprint="different-export-id"), two
        )
        self.assertEqual("fixture.txt", first["path"])
        self.assertEqual(first["fingerprint"], second["fingerprint"])
        controls = [
            self.convert(original, one, commit="b" * 40),
            self.convert(self.report(one / "other.txt"), one),
            self.convert(self.report(one / "fixture.txt", rule="other-rule"), one),
            self.convert(self.report(one / "fixture.txt", line=2), one),
        ]
        fingerprints = {first["fingerprint"], *(record["fingerprint"] for record in controls)}
        self.assertEqual(5, len(fingerprints))

    def test_git_identity_keeps_original_scanner_fingerprint(self):
        original = self.report("fixture.txt", scanner_fingerprint="original-git-location")
        record = self.convert(original, source="gitleaks-history")
        expected = self.functions["fingerprint"](
            self.key, "gitleaks", "original-git-location", self.commit,
            "fixture.txt", "fixture-token", "1",
        )
        self.assertEqual(expected, record["fingerprint"])
        changed = dict(original, Fingerprint="different-git-location")
        self.assertNotEqual(record["fingerprint"], self.convert(changed, source="gitleaks-history")["fingerprint"])

    def test_tree_representatives_are_stable_across_hash_seeds(self):
        script = '''
import json, runpy, sys
from types import SimpleNamespace
functions = runpy.run_path(sys.argv[1])
refs = {"1" * 40: "a" * 40, "2" * 40: "a" * 40, "3" * 40: "b" * 40}
def run_git(root, command, *args, **kwargs):
    if command == "for-each-ref":
        return SimpleNamespace(text="\\n".join(["3" * 40, "2" * 40, "1" * 40, "2" * 40]), code=0)
    assert command == "rev-parse"
    return SimpleNamespace(text=refs[args[-1].removesuffix("^{tree}")], code=0)
functions["audit_tip_trees"].__globals__["run_git"] = run_git
print(json.dumps(functions["audit_tip_trees"](SimpleNamespace(root="fixture")), sort_keys=True))
'''
        expected = {"a" * 40: "1" * 40, "b" * 40: "3" * 40}
        for seed in range(1, 9):
            with self.subTest(seed=seed):
                proc = subprocess.run(
                    [sys.executable, "-c", script, str(PUBLISH)],
                    env={**os.environ, "PYTHONHASHSEED": str(seed), "PYTHONDONTWRITEBYTECODE": "1"},
                    text=True, capture_output=True, check=True,
                )
                self.assertEqual(expected, json.loads(proc.stdout))

    @unittest.skipUnless(shutil.which("gitleaks"), "gitleaks is not installed")
    def test_live_directory_scanner_and_converter_keep_identity_across_exports(self):
        config = self.root / ".gitleaks.toml"
        config.write_text(
            'title = "Synthetic fingerprint regression"\n'
            '[[rules]]\nid = "fixture-token"\n'
            'description = "Synthetic test token"\n'
            'regex = "DOTFILES_TEST_SECRET_[0-9a-f]{12}"\n'
        )
        self.functions["run_gitleaks"].__globals__["gitleaks_executable"] = lambda: shutil.which("gitleaks")
        scanner_prints = []
        records = []
        for name in ("one", "two"):
            directory = self.root / name
            directory.mkdir()
            (directory / "fixture.txt").write_text(TEST_SECRET + "\n")
            results = self.functions["run_gitleaks"](
                SimpleNamespace(root=self.root), self.root, directory=directory
            )
            self.assertEqual(1, len(results))
            scanner_prints.append(results[0]["Fingerprint"])
            records.append(self.convert(results[0], directory))
            self.assertFalse((self.root / "gitleaks-report.json").exists())
        self.assertNotEqual(scanner_prints[0], scanner_prints[1])
        self.assertEqual("fixture.txt", records[0]["path"])
        self.assertEqual(records[0]["fingerprint"], records[1]["fingerprint"])


class DotfilesPublishAtomicStateTests(unittest.TestCase):
    def test_concurrent_atomic_writes_preserve_a_complete_generation(self):
        loaded = runpy.run_path(str(PUBLISH), run_name="isolated_atomic_tests")
        functions = loaded["write_json"].__globals__
        barrier = threading.Barrier(2)
        original_dump = json.dump

        def simultaneous_dump(payload, handle, **kwargs):
            barrier.wait(timeout=5)
            original_dump(payload, handle, **kwargs)

        functions["json"] = SimpleNamespace(dump=simultaneous_dump)
        with tempfile.TemporaryDirectory(prefix="dotfiles-atomic-") as directory:
            path = Path(directory) / "full-audit-generation.json"
            payloads = [{"run_id": "a" * 32}, {"run_id": "b" * 32}]
            with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
                futures = [pool.submit(loaded["write_json"], path, payload) for payload in payloads]
                for future in futures:
                    future.result(timeout=10)
            self.assertIn(json.loads(path.read_text()), payloads)
            self.assertEqual([path], list(Path(directory).iterdir()))
            self.assertEqual(0o600, stat.S_IMODE(path.stat().st_mode))


class DotfilesPublishStateTests(PublicationFixture):
    def test_readiness_preserves_stale_tracking_refs_and_fetch_head(self):
        base = self.publish_base()
        published = self.commit("published work", {"docs/public.md": "public\n"})
        self.git(
            "-c", "core.hooksPath=/dev/null", "push", "--quiet",
            str(self.remote), "HEAD:refs/heads/master",
        )
        self.commit("candidate work", {"docs/candidate.md": "candidate\n"})
        self.assertEqual(base, self.git("rev-parse", "origin/master").stdout.strip())
        refs = self.git("for-each-ref").stdout
        fetch_head = (self.repo / ".git/FETCH_HEAD").read_bytes()

        proc, run_id = self.scan("--mode", "readiness")

        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        run = self.read_json(self.run_dir(run_id) / "run.json")
        self.assertEqual("readiness", run["mode"])
        self.assertEqual(published, run["remote_oid"])
        self.assertEqual(refs, self.git("for-each-ref").stdout)
        self.assertEqual(fetch_head, (self.repo / ".git/FETCH_HEAD").read_bytes())
        self.assertFalse((self.state_dir() / "full-audit.json").exists())
        self.assertFalse((self.state_dir() / "authorization.json").exists())

    def test_readiness_fetches_missing_tip_without_updating_refs_on_refusal(self):
        self.publish_base()
        remote_tip = self.advance_remote_elsewhere()
        self.assertNotEqual(
            0, self.git("cat-file", "-e", remote_tip, check=False).returncode
        )
        refs = self.git("for-each-ref").stdout
        fetch_head = (self.repo / ".git/FETCH_HEAD").read_bytes()

        proc = self.cli("scan", "--mode", "readiness")

        self.assertNotEqual(0, proc.returncode)
        self.assertIn("not an ancestor", proc.stderr)
        self.assertEqual(0, self.git("cat-file", "-e", remote_tip).returncode)
        self.assertEqual(refs, self.git("for-each-ref").stdout)
        self.assertEqual(fetch_head, (self.repo / ".git/FETCH_HEAD").read_bytes())

    def test_readiness_cannot_authorize_push_or_start_repair(self):
        self.publish_base()
        self.commit("candidate work", {"docs/candidate.md": "candidate\n"})
        proc, run_id = self.scan("--mode", "readiness")
        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        self.review_all(run_id)
        self.write_full_audit_receipt(run_id)
        self.advance_remote_elsewhere()
        refs = self.git("for-each-ref").stdout
        fetch_head = (self.repo / ".git/FETCH_HEAD").read_bytes()
        remote_before = self.remote_tip()

        for command in ("authorize", "push", "repair-start"):
            with self.subTest(command=command):
                refused = self.cli(command, "--run", run_id)
                self.assertNotEqual(0, refused.returncode)
                self.assertIn("readiness run", refused.stderr)
                self.assertEqual(refs, self.git("for-each-ref").stdout)
                self.assertEqual(fetch_head, (self.repo / ".git/FETCH_HEAD").read_bytes())
                self.assertFalse((self.state_dir() / "authorization.json").exists())
                self.assertFalse((self.state_dir() / "repairs.json").exists())
                self.assertEqual(remote_before, self.remote_tip())

    def test_scan_discovers_repository_from_a_subdirectory(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})

        proc, run_id = self.scan_from(self.repo / "docs")

        record = self.read_json(self.run_dir(run_id) / "run.json")
        self.assertEqual(os.path.realpath(self.repo), os.path.realpath(record["repository"]))
        self.assertIn("repository: ", proc.stdout)

    def test_scan_resolves_the_configured_upstream_branch(self):
        self.publish_base()
        self.git(
            "-c",
            "core.hooksPath=/dev/null",
            "push",
            "--quiet",
            "origin",
            "master:refs/heads/publication",
        )
        self.git("fetch", "--quiet", "origin")
        self.git("branch", "--set-upstream-to=origin/publication", "master")
        self.commit("add notes", {"docs/notes.md": "notes\n"})

        _, run_id = self.scan()

        record = self.read_json(self.run_dir(run_id) / "run.json")
        self.assertEqual("origin", record["remote_name"])
        self.assertEqual("refs/heads/publication", record["remote_ref"])
        self.assertEqual(str(self.remote), record["remote_url"])
        self.assertEqual(self.remote_tip("refs/heads/publication"), record["remote_oid"])
        self.assertEqual(self.head(), record["candidate_oid"])

    def test_scan_rejects_a_non_fast_forward_candidate(self):
        self.publish_base()
        self.commit("local work", {"docs/local.md": "local\n"})
        self.advance_remote_elsewhere()

        proc = self.cli("scan")

        self.assertNotEqual(0, proc.returncode)
        self.assertIn("ancestor", proc.stderr)
        self.assertNotIn("force", proc.stdout)

    def test_state_directory_and_files_are_owner_only(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})

        _, run_id = self.scan()

        self.assertEqual(0o700, stat.S_IMODE(self.state_dir().stat().st_mode))
        self.assertEqual(
            0o600, stat.S_IMODE((self.state_dir() / "fingerprint.key").stat().st_mode)
        )
        self.assertEqual(
            0o600, stat.S_IMODE((self.run_dir(run_id) / "run.json").stat().st_mode)
        )
        self.assertEqual(0o700, stat.S_IMODE(self.run_dir(run_id).stat().st_mode))

    def test_ruleset_identity_is_deterministic_and_policy_sensitive(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})

        first, first_run = self.scan()
        second, second_run = self.scan()
        self.assertNotEqual(first_run, second_run)
        self.assertEqual(self.read_json(self.run_dir(first_run) / "run.json")["ruleset_id"],
                         self.read_json(self.run_dir(second_run) / "run.json")["ruleset_id"])
        first_ruleset = self.read_json(self.run_dir(first_run) / "run.json")["ruleset_id"]

        (self.repo / ".gitleaks.toml").write_text("[extend]\nuseDefault = true\n")
        _, third_run = self.scan()
        third_ruleset = self.read_json(self.run_dir(third_run) / "run.json")["ruleset_id"]

        self.assertNotEqual(first_ruleset, third_ruleset)
        self.assertNotEqual(first_run, third_run)

    def test_repeated_scan_preserves_original_snapshot_and_review(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})
        _, run_id = self.scan()
        self.review_all(run_id)
        before = {p.name: p.read_bytes() for p in self.run_dir(run_id).iterdir() if p.is_file()}
        _, repeated = self.scan()
        self.assertNotEqual(run_id, repeated)
        self.assertEqual(before, {p.name: p.read_bytes() for p in self.run_dir(run_id).iterdir() if p.is_file()})
        self.assertFalse((self.run_dir(repeated) / "review.json").exists())
        self.assertNotEqual(0, self.cli("review-status", "--run", repeated).returncode)

    def test_existing_run_directory_cannot_be_reused(self):
        self.publish_base()
        _, run_id = self.scan()
        loaded = runpy.run_path(str(PUBLISH), run_name="isolated_storage_tests")
        repo = SimpleNamespace(state_dir=self.state_dir())
        record = self.read_json(self.run_dir(run_id) / "run.json")
        before = (self.run_dir(run_id) / "run.json").read_bytes()
        with self.assertRaisesRegex(loaded["PublishError"], "refusing to replace"):
            loaded["store_run"](repo, record)
        self.assertEqual(before, (self.run_dir(run_id) / "run.json").read_bytes())

    def test_review_record_refuses_mismatched_boundary_without_rebinding(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})
        _, run_id = self.scan()
        units = self.review_all(run_id)
        path = self.run_dir(run_id) / "review.json"
        original = self.read_json(path)
        for field in ("manifest_id", "run_id", "schema"):
            with self.subTest(field=field):
                review = dict(original, **{field: "different"})
                path.write_text(json.dumps(review))
                before = path.read_bytes()
                result = self.cli("review-record", "--run", run_id, "--unit", units[0], "--verdict", "clean")
                self.assertNotEqual(0, result.returncode)
                self.assertIn("different manifest or run", result.stderr)
                self.assertEqual(before, path.read_bytes())

    def test_review_record_refuses_mutated_manifest(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})
        _, run_id = self.scan()
        unit = self.unit_ids(run_id)[0]
        path = self.run_dir(run_id) / "manifest.json"
        manifest = self.read_json(path)
        manifest["units"][0]["content"] += " changed"
        path.write_text(json.dumps(manifest))
        result = self.cli("review-record", "--run", run_id, "--unit", unit, "--verdict", "clean")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("manifest content changed", result.stderr)
        self.assertFalse((self.run_dir(run_id) / "review.json").exists())

    def test_full_audit_metadata_is_bound_to_manifest_digest(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})
        _, run_id = self.scan("--mode", "full-audit")
        path = self.run_dir(run_id) / "manifest.json"
        manifest = self.read_json(path)
        unit = manifest["units"][0]["unit_id"]
        manifest["units"][0]["path"] = "different-source"
        path.write_text(json.dumps(manifest))
        result = self.cli("review-record", "--run", run_id, "--unit", unit, "--verdict", "clean")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("manifest content changed", result.stderr)
        self.assertFalse((self.run_dir(run_id) / "review.json").exists())

    def test_fingerprint_key_is_created_once_and_permission_checked(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})

        self.scan()
        key_path = self.state_dir() / "fingerprint.key"
        original = key_path.read_bytes()
        self.assertEqual(32, len(original))

        self.scan()
        self.assertEqual(original, key_path.read_bytes())

        key_path.chmod(0o644)
        proc = self.cli("scan")
        self.assertNotEqual(0, proc.returncode)
        self.assertIn("fingerprint key", proc.stderr)

    def test_clock_override_requires_the_testing_flag(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})

        environment = self.env()
        del environment["DOTFILES_PUBLISH_TESTING"]
        proc = self.cli("scan", env=environment)
        run_id = self.run_id_of(proc)

        record = self.read_json(self.run_dir(run_id) / "run.json")
        self.assertNotEqual("2025-08-01T00:53:20Z", record["created_at"])
        self.assertGreater(record["created_at"], "2026-")


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
            "--log-opts=--no-textconv --no-ext-diff %s..%s" % (self.remote_tip(), candidate),
            ranges,
            "the scanner was not pointed at the exact outgoing range",
        )

    def test_candidate_tree_scan_finds_a_secret_inherited_from_published_history(self):
        self.commit(
            "public base with an inherited secret",
            {
                "README.md": "public dotfiles\n",
                "config/legacy.conf": "token = %s\n" % TEST_SECRET,
            },
        )
        self.git(
            "-c",
            "core.hooksPath=/dev/null",
            "push",
            "--quiet",
            str(self.remote),
            "master:refs/heads/master",
        )
        self.git("remote", "add", "origin", str(self.remote))
        self.git("fetch", "--quiet", "origin")
        self.git("branch", "--set-upstream-to=origin/master", "master")
        self.commit("unrelated work", {"docs/notes.md": "notes\n"})

        proc, run_id = self.scan()

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        sources = {finding["source"] for finding in findings}
        self.assertIn(
            "gitleaks-tree",
            sources,
            "the candidate tree was not scanned for inherited secrets",
        )
        self.assertNotIn(TEST_SECRET, proc.stdout)
        inherited = [
            finding for finding in findings if finding["source"] == "gitleaks-tree"
        ]
        self.assertTrue(inherited)
        for finding in inherited:
            self.assertEqual(
                "public-incident",
                finding["classification"],
                "an already-published secret was labelled as outgoing, which points "
                "at a useless rewrite of unpublished commits",
            )
        self.assert_run_redacts(proc, run_id, TEST_SECRET, inherited[0]["fingerprint"])

    def test_tree_finding_fingerprints_are_stable_across_runs(self):
        # A fingerprint keyed on the temporary extraction path would change
        # every run, so no incident receipt could ever match it.
        self.publish_base()
        self.commit(
            "add configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )

        _, first_run = self.scan()
        first = self.read_json(self.run_dir(first_run) / "findings.json")["findings"]
        (self.run_dir(first_run) / "findings.json").rename(self.base / "first-findings.json")
        _, second_run = self.scan()
        second = self.read_json(self.run_dir(second_run) / "findings.json")["findings"]

        def tree_prints(records):
            return {
                (record["path"], record["fingerprint"])
                for record in records
                if record["source"] == "gitleaks-tree"
            }

        self.assertTrue(tree_prints(first))
        self.assertEqual(tree_prints(first), tree_prints(second))
        for path, _ in tree_prints(first):
            self.assertFalse(path.startswith("/"), "a tree finding kept an absolute path")
            self.assertNotIn("dotfiles-publish/tmp", path)

    def test_tree_scan_reads_committed_bytes_not_smudged_content(self):
        # git archive applies the smudge filter, so it would hand the scanner
        # the plaintext of an encrypted path instead of the published bytes.
        self.publish_base()
        self.git("config", "filter.probe.clean", "sed s/%s/ENCRYPTED-PLACEHOLDER/" % TEST_SECRET)
        self.git("config", "filter.probe.smudge", "sed s/ENCRYPTED-PLACEHOLDER/%s/" % TEST_SECRET)
        self.commit(
            "declare the filter", {".gitattributes": "secrets/service.conf filter=probe\n"}
        )
        self.commit(
            "add the encrypted configuration",
            {"secrets/service.conf": "token = %s\n" % TEST_SECRET},
        )

        committed = self.git("cat-file", "blob", "HEAD:secrets/service.conf").stdout
        self.assertIn("ENCRYPTED-PLACEHOLDER", committed, "the clean filter did not run")

        proc, run_id = self.scan()

        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        tree_findings = [
            finding for finding in findings if finding["source"] == "gitleaks-tree"
        ]
        self.assertEqual(
            [],
            tree_findings,
            "the tree scan read smudged plaintext instead of the committed bytes",
        )

    def test_deterministic_detectors_report_risky_names_symlinks_and_large_blobs(self):
        self.publish_base()
        os.symlink("/etc/passwd", self.repo / "passwd-link")
        self.commit(
            "add objects that need inspection",
            {
                "deploy.log": "started\n",
                "keys/service.pem": "not really a key\n",
                "assets/blob.bin": bytes(range(256)) * 8,
                "assets/huge.dat": b"a" * (2 * 1024 * 1024 + 1),
            },
        )

        proc, run_id = self.scan()

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        rules = {(finding["rule_id"], finding["path"]) for finding in findings}
        self.assertIn(("risky-log-file", "deploy.log"), rules)
        self.assertIn(("risky-private-key-file", "keys/service.pem"), rules)
        self.assertIn(("binary-file", "assets/blob.bin"), rules)
        self.assertIn(("large-file", "assets/huge.dat"), rules)
        self.assertIn(("symlink-in-outgoing-history", "passwd-link"), rules)

    def test_a_large_text_blob_does_not_stall_redaction(self):
        # Structural redaction once had a quadratic rule that spun for minutes
        # on a long run of ordinary characters.
        self.publish_base()
        self.commit("add generated data", {"assets/generated.txt": "a" * 400000})

        started = time.monotonic()
        proc, _ = self.scan()
        elapsed = time.monotonic() - started

        self.assertIn(proc.returncode, (0, 2), proc.stdout + proc.stderr)
        self.assertLess(elapsed, 60, "scanning a large text blob took %.1fs" % elapsed)

    def test_scanner_failure_is_reported_as_a_closed_failure(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})

        proc = self.cli(
            "scan",
            env=self.env(
                DOTFILES_FAKE_GITLEAKS_FAIL="7",
                DOTFILES_FAKE_GITLEAKS_FAIL_TEXT="scanner crashed while reading token = %s"
                % TEST_SECRET,
            ),
        )

        self.assertEqual(3, proc.returncode, proc.stdout + proc.stderr)
        self.assertIn("gitleaks failed", proc.stderr)
        self.assertNotIn(TEST_SECRET, proc.stdout + proc.stderr)

    @unittest.skipUnless(shutil.which("gitleaks"), "gitleaks is not installed")
    def test_live_gitleaks_config_fires_default_and_repository_rules(self):
        # Values are assembled at run time so no secret-shaped literal is
        # committed to this file.
        sample = "\n".join(
            [
                "aws_key = " + "AKIA" + "Z2Q7X4M1V9K3T5B8",
                "db = postgres://admin:" + "hunter2hunter2" + "@db.example.invalid/app",
            ]
        )
        self.commit("public base", {"README.md": "public dotfiles\n"})
        self.commit("add sample", {"sample.conf": sample + "\n"})
        report = self.base / "live-report.json"

        completed = subprocess.run(
            [
                "gitleaks",
                "git",
                "--config",
                str(REPO_ROOT / ".gitleaks.toml"),
                "--log-opts=--all",
                "--no-banner",
                "--redact=100",
                "--report-format",
                "json",
                "--report-path",
                str(report),
                str(self.repo),
            ],
            capture_output=True,
            text=True,
            env=self.git_env(),
        )

        self.assertIn(completed.returncode, (0, 1), completed.stderr)
        results = json.loads(report.read_text() or "[]")
        rules = {result["RuleID"] for result in results}
        self.assertTrue(
            [rule for rule in rules if not rule.startswith("dotfiles-")],
            "no upstream default detector fired: %s" % rules,
        )
        self.assertIn("dotfiles-credential-url", rules)

    @unittest.skipUnless(shutil.which("gitleaks"), "gitleaks is not installed")
    def test_inline_command_rule_ignores_long_flags_that_contain_dash_p(self):
        # The short-flag branch once matched the -p inside --paginate and
        # captured "aginate" as the secret.
        sample = "\n".join(
            [
                "run `gh api repos/OWNER/REPO/issues --jq .title` or `gh api --paginate`.",
                "psql -p 5432 -h localhost",
                "mysql -u root -p" + "SuperSecretPassword1",
                "curl --password " + "hunter2hunter2 https://example.invalid",
            ]
        )
        (self.base / "sample.md").write_text(sample + "\n")
        report = self.base / "flags-report.json"

        completed = subprocess.run(
            [
                "gitleaks", "dir",
                "--config", str(REPO_ROOT / ".gitleaks.toml"),
                "--no-banner", "--redact=100",
                "--report-format", "json",
                "--report-path", str(report),
                str(self.base),
            ],
            capture_output=True,
            text=True,
            env=self.git_env(),
        )

        self.assertIn(completed.returncode, (0, 1), completed.stderr)
        results = json.loads(report.read_text() or "[]")
        lines = {result["StartLine"] for result in results}
        self.assertNotIn(1, lines, "a long flag containing -p was read as a password")
        self.assertNotIn(2, lines, "a port number after -p was read as a password")
        self.assertEqual({3, 4}, lines, "the real inline credentials must still fire")

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
    @unittest.skipUnless(shutil.which('gitleaks'), 'gitleaks is not installed')
    def test_live_truncated_match_hides_complete_quoted_value(self):
        self.publish_base()
        config = """title = "synthetic quoted assignment"
[[rules]]
id = "synthetic-truncated-assignment"
description = "synthetic password prefix"
regex = '''password='(DOTFILES_TEST_SECRET_[0-9a-f]{12})'''
secretGroup = 1
"""
        secret = TEST_SECRET
        suffix = 'SYNTHETIC_SUFFIX_456'
        self.commit('add synthetic assignment', {'.gitleaks.toml': config, 'fixture.log': "Config(password='" + secret + ';' + suffix + "', username='fixture@example.invalid')\n"})
        for mode in ('publish', 'full-audit'):
            with self.subTest(mode=mode):
                proc, run_id = self.scan('--mode', mode, env=self.env(DOTFILES_PUBLISH_GITLEAKS=shutil.which('gitleaks')))
                self.assertEqual(2, proc.returncode, proc.stderr)
                self.assertNotIn(suffix, proc.stdout + proc.stderr)
                self.assertNotIn(secret, proc.stdout + proc.stderr)
                for path in self.run_dir(run_id).iterdir():
                    if path.is_file():
                        self.assertNotIn(suffix, path.read_text())
                        self.assertNotIn(secret, path.read_text())
                for unit_id in self.unit_ids(run_id):
                    result = self.cli('review-show', '--run', run_id, '--unit', unit_id)
                    self.assertEqual(0, result.returncode, result.stderr)
                    self.assertNotIn(suffix, result.stdout)
                    self.assertNotIn(secret, result.stdout)

    @unittest.skipUnless(shutil.which("gitleaks"), "gitleaks is not installed")
    def test_live_gitleaks_report_is_read_privately_and_redacted_by_the_wrapper(self):
        self.publish_base()
        config = """title = "dotfiles publication test"
[extend]
useDefault = true

[[rules]]
id = "dotfiles-test-secret"
description = "synthetic test secret"
regex = '''DOTFILES_TEST_SECRET_[0-9a-f]{12}'''
"""
        self.commit(
            "add live scanner fixture",
            {
                ".gitleaks.toml": config,
                "config/service.conf": "π historical sample %s\n" % TEST_SECRET,
            },
        )

        proc, run_id = self.scan(
            env=self.env(DOTFILES_PUBLISH_GITLEAKS=shutil.which("gitleaks"))
        )

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        matching = [
            finding for finding in findings if finding["rule_id"] == "dotfiles-test-secret"
        ]
        self.assertTrue(matching, findings)
        marker = "[REDACTED:%s]" % matching[0]["fingerprint"]
        persisted = "\n".join(
            path.read_text(encoding="utf-8", errors="replace")
            for path in self.run_dir(run_id).rglob("*")
            if path.is_file()
        )
        self.assertNotIn(TEST_SECRET, persisted)
        self.assertIn(marker, persisted)
        for unit_id in self.unit_ids(run_id):
            shown = self.cli("review-show", "--run", run_id, "--unit", unit_id)
            self.assertEqual(0, shown.returncode, shown.stderr)
            self.assertNotIn(TEST_SECRET, shown.stdout)

    def _check_encrypted_history_stays_opaque(self, scanner):
        self.publish_base()
        marker = self.base / "textconv-ran"
        driver = self.base / "decrypt-fixture.py"
        driver.write_text(
            "from pathlib import Path\n"
            f"Path({str(marker)!r}).touch()\n"
            f"print({TEST_SECRET!r})\n"
        )
        self.git("config", "diff.git-crypt.textconv", f"{sys.executable} {driver}")
        ciphertext = b"\0GITCRYPT\0synthetic-ciphertext\xff"
        self.commit("add encrypted fixture", {
            ".gitattributes": "config/encrypted.env filter=git-crypt diff=git-crypt\n",
            ".gitleaks.toml": (
                '[[rules]]\nid="synthetic-only"\ndescription="synthetic fixture"\n'
                'regex="""DOTFILES_TEST_SECRET_[0-9a-f]{12}"""\n'
            ),
            "config/encrypted.env": ciphertext,
        })
        # Outgoing scans and public-history scans must both keep ciphertext raw.
        for mode in ("publish", "full-audit"):
            with self.subTest(mode=mode):
                if mode == "full-audit":
                    self.git("-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "master")
                    self.git("fetch", "--quiet", "origin")
                proc, run_id = self.scan("--mode", mode, env=self.env(DOTFILES_PUBLISH_GITLEAKS=scanner))
                self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
                self.assertFalse(marker.exists(), "a private decryption driver ran")
                findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
                # The existing opaque-binary review obligation remains in force.
                self.assertEqual({"binary-file"}, {finding["rule_id"] for finding in findings})
                self.assertEqual({"objects"}, {finding["source"] for finding in findings})
                for path in self.run_dir(run_id).rglob("*"):
                    if path.is_file():
                        self.assertNotIn(TEST_SECRET, path.read_text(errors="replace"))
                self.assertIn("binary", (self.run_dir(run_id) / "manifest.json").read_text().lower())
        self.assertEqual(ciphertext, (self.repo / "config/encrypted.env").read_bytes())

    def test_encrypted_history_stays_opaque_with_scanner_double(self):
        self._check_encrypted_history_stays_opaque(str(self.scanner))

    @unittest.skipUnless(shutil.which("gitleaks"), "gitleaks is not installed")
    def test_live_gitleaks_never_decrypts_encrypted_history(self):
        self._check_encrypted_history_stays_opaque(shutil.which("gitleaks"))

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
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        self.assertTrue(findings)
        self.assertIn("[REDACTED:", json.dumps(findings))
        self.assert_run_redacts(proc, run_id, TEST_SECRET, findings[0]["fingerprint"])

    def test_manifest_units_are_redacted_before_they_are_stored(self):
        self.publish_base()
        self.commit(
            "add configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )

        _, run_id = self.scan()

        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        serialized = json.dumps(manifest)
        self.assertNotIn(TEST_SECRET, serialized)
        self.assertIn("[REDACTED:", serialized)

    def test_private_atomically_replaced_scanner_report_is_accepted(self):
        self.publish_base()
        self.commit(
            "add configuration",
            {"config/service.conf": "historical sample %s\n" % TEST_SECRET},
        )

        proc, run_id = self.scan(
            env=self.env(DOTFILES_FAKE_GITLEAKS_REPORT_ATTACK="replace")
        )

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        finding = self.read_json(self.run_dir(run_id) / "findings.json")["findings"][0]
        self.assert_run_redacts(proc, run_id, TEST_SECRET, finding["fingerprint"])

    def test_unsafe_scanner_report_fails_closed_and_is_removed(self):
        self.publish_base()
        self.commit(
            "add configuration",
            {"config/service.conf": "historical sample %s\n" % TEST_SECRET},
        )

        proc = self.cli(
            "scan",
            env=self.env(DOTFILES_FAKE_GITLEAKS_REPORT_ATTACK="symlink"),
        )

        self.assertEqual(3, proc.returncode, proc.stdout + proc.stderr)
        self.assertNotIn(TEST_SECRET, proc.stdout + proc.stderr)
        invocation = json.loads(self.scanner_log.read_text().splitlines()[-1])
        report = option_from_argv(invocation, "--report-path")
        self.assertFalse(Path(report).exists())

    def test_permissive_scanner_report_fails_closed_and_is_removed(self):
        self.publish_base()
        self.commit(
            "add configuration",
            {"config/service.conf": "historical sample %s\n" % TEST_SECRET},
        )

        proc = self.cli(
            "scan",
            env=self.env(DOTFILES_FAKE_GITLEAKS_REPORT_ATTACK="permissive"),
        )

        self.assertEqual(3, proc.returncode, proc.stdout + proc.stderr)
        self.assertIn("private regular file", proc.stderr)
        invocation = json.loads(self.scanner_log.read_text().splitlines()[-1])
        self.assertFalse(
            Path(option_from_argv(invocation, "--report-path")).exists()
        )

    def test_partial_report_with_error_status_is_not_accepted_or_persisted(self):
        self.publish_base()
        self.commit(
            "add configuration",
            {"config/service.conf": "historical sample %s\n" % TEST_SECRET},
        )

        proc = self.cli(
            "scan",
            env=self.env(DOTFILES_FAKE_GITLEAKS_PARTIAL_FAILURE="1"),
        )

        self.assertEqual(3, proc.returncode, proc.stdout + proc.stderr)
        self.assertNotIn(TEST_SECRET, proc.stdout + proc.stderr)
        self.assertFalse((self.state_dir() / "runs").exists())
        invocation = json.loads(self.scanner_log.read_text().splitlines()[-1])
        self.assertFalse(Path(option_from_argv(invocation, "--report-path")).exists())

    def test_scanner_exec_failure_removes_private_workspace(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})
        temporary_root = self.base / "temporary"
        temporary_root.mkdir(mode=0o700)

        proc = self.cli(
            "scan",
            env=self.env(
                DOTFILES_PUBLISH_GITLEAKS=str(self.base / "missing-gitleaks"),
                TMPDIR=str(temporary_root),
            ),
        )

        self.assertEqual(3, proc.returncode, proc.stdout + proc.stderr)
        self.assertIn("could not execute gitleaks", proc.stderr)
        self.assertEqual([], list(temporary_root.glob("dotfiles-publish-scan-*")))


class DotfilesPublishManifestTests(PublicationFixture):
    def test_legacy_run_refuses_all_review_commands_before_reading_manifest(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "notes\n"})
        _, run_id = self.scan()
        run_path = self.run_dir(run_id) / "run.json"
        record = self.read_json(run_path)
        record["schema"] = 1
        run_path.write_text(json.dumps(record))
        canary = "LEGACY_MANIFEST_CANARY_MUST_NOT_BE_PRINTED"
        (self.run_dir(run_id) / "manifest.json").write_text(canary)

        commands = (
            ("review-list", "--run", run_id),
            ("review-show", "--run", run_id, "--unit", "0" * 32),
            ("review-status", "--run", run_id),
        )
        for command in commands:
            with self.subTest(command=command[0]):
                proc = self.cli(*command)
                self.assertEqual(1, proc.returncode, proc.stdout + proc.stderr)
                self.assertIn("predates safe scanner redaction", proc.stderr)
                self.assertNotIn(canary, proc.stdout + proc.stderr)

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

    def test_review_list_is_content_free_and_show_returns_one_unit(self):
        self.publish_base()
        self.commit("add notes", {"docs/notes.md": "sensitive-looking notes\n"})
        _, run_id = self.scan()

        listing = self.cli("review-list", "--run", run_id)
        self.assertEqual(0, listing.returncode, listing.stderr)
        self.assertNotIn("sensitive-looking notes", listing.stdout)

        unit_id = listing.stdout.splitlines()[0].split()[0]
        shown = self.cli("review-show", "--run", run_id, "--unit", unit_id)
        self.assertEqual(0, shown.returncode, shown.stderr)
        unit = json.loads(shown.stdout)
        self.assertEqual(unit_id, unit["unit_id"])

        missing = self.cli("review-show", "--run", run_id, "--unit", "0" * 32)
        self.assertNotEqual(0, missing.returncode)

    def test_review_show_resolves_a_path_unit_to_its_actual_bytes(self):
        # Capture the redacted snapshot while scanner values are still in
        # memory; resolving the raw object later would lose that protection.
        self.publish_base()
        self.commit("add a helper", {"shell/helper.sh": "alias gs='git status'\n"})
        _, run_id = self.scan()

        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        path_unit = next(
            unit
            for unit in manifest["units"]
            if unit["kind"] == "path" and unit["path"] == "shell/helper.sh"
        )
        self.assertIn("alias gs='git status'", path_unit["blob"])

        shown = self.cli("review-show", "--run", run_id, "--unit", path_unit["unit_id"])
        self.assertEqual(0, shown.returncode, shown.stderr)
        self.assertIn("alias gs='git status'", json.loads(shown.stdout)["blob"])

    def test_review_show_refuses_legacy_path_units_without_a_redacted_snapshot(self):
        self.publish_base()
        self.commit("add a helper", {"shell/helper.sh": "alias gs='git status'\n"})
        _, run_id = self.scan()
        manifest_path = self.run_dir(run_id) / "manifest.json"
        manifest = self.read_json(manifest_path)
        path_unit = next(unit for unit in manifest["units"] if unit["kind"] == "path")
        path_unit.pop("blob", None)
        manifest_path.write_text(json.dumps(manifest))

        shown = self.cli("review-show", "--run", run_id, "--unit", path_unit["unit_id"])

        self.assertEqual(1, shown.returncode)
        self.assertIn("manifest content changed", shown.stderr)

    def test_concurrent_review_records_do_not_overwrite_each_other(self):
        self.publish_base()
        self.commit(
            "add concurrent review fixtures",
            {"docs/item-%02d.md" % index: "item %d\n" % index for index in range(8)},
        )
        _, run_id = self.scan()
        unit_ids = self.unit_ids(run_id)[:12]
        environment = self.env(DOTFILES_PUBLISH_TEST_REVIEW_DELAY="0.05")

        processes = [
            subprocess.Popen(
                [
                    sys.executable,
                    str(PUBLISH),
                    "review-record",
                    "--run",
                    run_id,
                    "--unit",
                    unit_id,
                    "--verdict",
                    "clean",
                ],
                cwd=str(self.repo),
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                env=environment,
            )
            for unit_id in unit_ids
        ]
        completed = [process.communicate(timeout=10) for process in processes]

        for process, (stdout, stderr) in zip(processes, completed):
            self.assertEqual(0, process.returncode, stdout + stderr)
        review = self.read_json(self.run_dir(run_id) / "review.json")
        self.assertEqual(set(unit_ids), set(review["entries"]))

    def test_review_record_rejects_value_fields_and_scanner_known_values(self):
        self.publish_base()
        self.commit(
            "add configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )
        _, run_id = self.scan()
        unit_id = self.unit_ids(run_id)[0]

        with_value = self.base / "with-value.json"
        with_value.write_text(
            json.dumps([{"rule_id": "manual", "context": "redacted", "value": "anything"}])
        )
        rejected = self.cli(
            "review-record",
            "--run",
            run_id,
            "--unit",
            unit_id,
            "--verdict",
            "finding",
            "--findings",
            str(with_value),
        )
        self.assertNotEqual(0, rejected.returncode)
        self.assertIn("value field", rejected.stderr)

        with_secret = self.base / "with-secret.json"
        with_secret.write_text(
            json.dumps([{"rule_id": "manual", "context": "saw %s here" % TEST_SECRET}])
        )
        leaking = self.cli(
            "review-record",
            "--run",
            run_id,
            "--unit",
            unit_id,
            "--verdict",
            "finding",
            "--findings",
            str(with_secret),
        )
        self.assertNotEqual(0, leaking.returncode)
        self.assertIn("scanner detected", leaking.stderr)
        self.assertNotIn(TEST_SECRET, leaking.stdout + leaking.stderr)

    def test_review_record_rejects_punctuation_and_multiline_scanner_values(self):
        reported = "a!β@c#d$e%f^g&h*i\nsecond!line"
        self.publish_base()
        self.commit(
            "add configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )
        _, run_id = self.scan(
            env=self.env(DOTFILES_FAKE_GITLEAKS_REPORTED_SECRET=reported)
        )
        unit_id = self.unit_ids(run_id)[0]
        for index, context in enumerate((reported, "saw %s here" % reported)):
            findings_file = self.base / ("punctuation-multiline-%d.json" % index)
            findings_file.write_text(
                json.dumps([{"rule_id": "manual", "context": context}])
            )
            rejected = self.cli(
                "review-record",
                "--run",
                run_id,
                "--unit",
                unit_id,
                "--verdict",
                "finding",
                "--findings",
                str(findings_file),
            )
            self.assertEqual(1, rejected.returncode, rejected.stdout + rejected.stderr)
            self.assertIn("scanner detected", rejected.stderr)
        review_path = self.run_dir(run_id) / "review.json"
        if review_path.exists():
            self.assertNotIn(reported, review_path.read_text())
        for path in self.run_dir(run_id).rglob("*"):
            if path.is_file():
                self.assertNotIn(reported, path.read_text(errors="replace"), str(path))

    def test_review_record_refuses_legacy_value_hmac_state(self):
        self.publish_base()
        self.commit(
            "add configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )
        _, run_id = self.scan()
        findings_path = self.run_dir(run_id) / "findings.json"
        findings = self.read_json(findings_path)
        findings["schema"] = 2
        findings["value_hmacs"] = [
            signature["hmac"] for signature in findings.pop("value_signatures")
        ]
        findings_path.write_text(json.dumps(findings))
        findings_file = self.base / "legacy-review.json"
        findings_file.write_text(
            json.dumps([{"rule_id": "manual", "context": "redacted evidence"}])
        )

        rejected = self.cli(
            "review-record",
            "--run",
            run_id,
            "--unit",
            self.unit_ids(run_id)[0],
            "--verdict",
            "finding",
            "--findings",
            str(findings_file),
        )

        self.assertEqual(1, rejected.returncode, rejected.stdout + rejected.stderr)
        self.assertIn("predates safe scanner redaction", rejected.stderr)

    def test_review_record_bounds_reviewer_controlled_input(self):
        self.publish_base()
        self.commit(
            "add configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )
        _, run_id = self.scan()
        unit_id = self.unit_ids(run_id)[0]

        cases = (
            ("oversized", [{"context": "x" * (64 * 1024)}], "file is too large"),
            ("too-many", [{} for _ in range(257)], "too many records"),
            ("long-field", [{"context": "x" * 4097}], "field context is too long"),
        )
        for name, payload, expected in cases:
            findings_file = self.base / (name + ".json")
            findings_file.write_text(json.dumps(payload))
            rejected = self.cli(
                "review-record",
                "--run",
                run_id,
                "--unit",
                unit_id,
                "--verdict",
                "finding",
                "--findings",
                str(findings_file),
            )
            self.assertEqual(1, rejected.returncode, rejected.stdout + rejected.stderr)
            self.assertIn(expected, rejected.stderr)
        self.assertFalse((self.run_dir(run_id) / "review.json").exists())

    def test_review_record_bounds_secret_comparison_work_before_hashing(self):
        self.publish_base()
        self.commit(
            "add configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )
        _, run_id = self.scan()
        findings_path = self.run_dir(run_id) / "findings.json"
        findings = self.read_json(findings_path)
        findings_file = self.base / "comparison-budget.json"
        findings_file.write_text(json.dumps([{"context": "x" * 4096}]))
        unit_id = self.unit_ids(run_id)[0]

        findings["value_signatures"] = [{"length": 500, "hmac": "0" * 64}]
        findings_path.write_text(json.dumps(findings))
        accepted = self.cli(
            "review-record",
            "--run",
            run_id,
            "--unit",
            unit_id,
            "--verdict",
            "finding",
            "--findings",
            str(findings_file),
        )
        self.assertEqual(0, accepted.returncode, accepted.stdout + accepted.stderr)
        before = (self.run_dir(run_id) / "review.json").read_text()

        findings_file.write_text(
            json.dumps([{"context": "x" * 4096}, {"context": "y" * 4096}])
        )
        rejected_total = self.cli(
            "review-record",
            "--run",
            run_id,
            "--unit",
            unit_id,
            "--verdict",
            "finding",
            "--findings",
            str(findings_file),
        )
        self.assertEqual(
            1, rejected_total.returncode, rejected_total.stdout + rejected_total.stderr
        )
        self.assertIn("too much secret-comparison work", rejected_total.stderr)

        findings["value_signatures"] = [{"length": 1000, "hmac": "0" * 64}]
        findings_path.write_text(json.dumps(findings))
        findings_file.write_text(json.dumps([{"context": "x" * 4096}]))
        rejected = self.cli(
            "review-record",
            "--run",
            run_id,
            "--unit",
            unit_id,
            "--verdict",
            "finding",
            "--findings",
            str(findings_file),
        )
        self.assertEqual(1, rejected.returncode, rejected.stdout + rejected.stderr)
        self.assertIn("too much secret-comparison work", rejected.stderr)
        self.assertEqual(before, (self.run_dir(run_id) / "review.json").read_text())

    def test_review_record_is_idempotent_only_for_an_identical_verdict(self):
        run_id = self.prepare_authorized_run()
        unit_id = self.unit_ids(run_id)[0]

        again = self.cli(
            "review-record", "--run", run_id, "--unit", unit_id, "--verdict", "clean"
        )
        self.assertEqual(0, again.returncode, again.stderr)

        findings = self.base / "manual.json"
        findings.write_text(json.dumps([{"rule_id": "manual", "context": "worth a look"}]))
        changed = self.cli(
            "review-record",
            "--run",
            run_id,
            "--unit",
            unit_id,
            "--verdict",
            "finding",
            "--findings",
            str(findings),
        )
        self.assertNotEqual(0, changed.returncode)
        self.assertIn("different verdict", changed.stderr)

    def test_a_scanner_finding_judged_clean_by_the_review_stops_blocking(self):
        # Every deterministic finding gets a unit so the review can adjudicate
        # it. Ignoring that verdict would make the second layer decorative and
        # leave a detector that fires on documentation blocking forever.
        self.publish_base()
        self.commit(
            "document the credential format",
            {"docs/format.md": "token = %s\n" % TEST_SECRET},
        )
        proc, run_id = self.scan()
        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        self.write_full_audit_receipt(run_id)

        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        scanner_units = [
            unit["unit_id"]
            for unit in manifest["units"]
            if unit["kind"] == "deterministic-finding"
        ]
        self.assertTrue(scanner_units)

        self.review_all(run_id)
        status = self.cli("review-status", "--run", run_id)
        self.assertEqual(0, status.returncode, status.stdout + status.stderr)
        self.assertIn("clean: yes", status.stdout)
        self.assertEqual(
            0, self.cli("authorize", "--run", run_id).returncode, "a judged run must authorize"
        )

    def test_a_scanner_finding_judged_unsafe_keeps_blocking(self):
        self.publish_base()
        self.commit(
            "add a credential", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )
        _, run_id = self.scan()
        self.write_full_audit_receipt(run_id)

        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        scanner_units = {
            unit["unit_id"]
            for unit in manifest["units"]
            if unit["kind"] == "deterministic-finding"
        }
        findings = self.base / "real.json"
        findings.write_text(
            json.dumps([{"rule_id": "review-credential", "context": "a real credential"}])
        )
        for unit_id in self.unit_ids(run_id):
            if unit_id in scanner_units:
                arguments = ["--verdict", "finding", "--findings", str(findings)]
            else:
                arguments = ["--verdict", "clean"]
            self.assertEqual(
                0,
                self.cli("review-record", "--run", run_id, "--unit", unit_id, *arguments).returncode,
            )

        status = self.cli("review-status", "--run", run_id)
        self.assertNotEqual(0, status.returncode)
        self.assertIn("clean: no", status.stdout)
        self.assertNotEqual(0, self.cli("authorize", "--run", run_id).returncode)

    def test_review_status_reports_completeness_without_content(self):
        run_id = self.prepare_authorized_run()

        status = self.cli("review-status", "--run", run_id)
        self.assertEqual(0, status.returncode, status.stderr)
        self.assertIn("clean: yes", status.stdout)

        review = self.read_json(self.run_dir(run_id) / "review.json")
        omitted = sorted(review["entries"])[0]
        del review["entries"][omitted]
        (self.run_dir(run_id) / "review.json").write_text(json.dumps(review))

        incomplete = self.cli("review-status", "--run", run_id)
        self.assertNotEqual(0, incomplete.returncode)
        self.assertIn("missing-unit: %s" % omitted, incomplete.stdout)
        self.assertIn("clean: no", incomplete.stdout)

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
    def start_repairable_run(self):
        self.publish_base()
        self.commit(
            "add configuration", {"config/service.conf": "token = %s\n" % TEST_SECRET}
        )
        proc, run_id = self.scan()
        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        started = self.cli("repair-start", "--run", run_id)
        self.assertEqual(0, started.returncode, started.stderr)
        return run_id, started

    def rewrite_history(self, content="token = from-op\n"):
        self.git("reset", "--quiet", "--hard", self.remote_tip())
        return self.commit("add configuration", {"config/service.conf": content})

    def test_repair_start_requires_a_blocking_finding(self):
        run_id = self.prepare_authorized_run()

        refused = self.cli("repair-start", "--run", run_id)

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("nothing to repair", refused.stderr)
        self.assertNotEqual(
            0,
            self.git(
                "rev-parse", "--verify", "refs/dotfiles-publish/recovery/%s" % run_id, check=False
            ).returncode,
        )

    def test_repair_start_records_targets_and_refuses_to_move_the_recovery_ref(self):
        run_id, started = self.start_repairable_run()
        self.assertIn("recovery-ref: refs/dotfiles-publish/recovery/%s" % run_id, started.stdout)
        self.assertIn("repair-target: ", started.stdout)
        self.assertNotIn(TEST_SECRET, started.stdout)

        again = self.cli("repair-start", "--run", run_id)
        self.assertEqual(0, again.returncode, again.stderr)

        self.git(
            "update-ref", "refs/dotfiles-publish/recovery/%s" % run_id, self.remote_tip()
        )
        moved = self.cli("repair-start", "--run", run_id)
        self.assertNotEqual(0, moved.returncode)
        self.assertIn("already protects", moved.stderr)

    def test_repair_verify_enforces_the_allowed_path_list(self):
        run_id, _ = self.start_repairable_run()
        self.git("reset", "--quiet", "--hard", self.remote_tip())
        self.commit(
            "add configuration",
            {"config/service.conf": "token = from-op\n", "docs/extra.md": "extra\n"},
        )

        refused = self.cli(
            "repair-verify", "--run", run_id, "--allowed-path", "config/service.conf"
        )
        self.assertNotEqual(0, refused.returncode)
        self.assertIn("docs/extra.md", refused.stderr)

        accepted = self.cli(
            "repair-verify",
            "--run",
            run_id,
            "--allowed-path",
            "config/service.conf",
            "--allowed-path",
            "docs/extra.md",
        )
        self.assertEqual(0, accepted.returncode, accepted.stdout + accepted.stderr)
        self.assertIn("next: scan", accepted.stdout)
        self.assertIn("review-invalidated: true", accepted.stdout)

    def test_repair_verify_requires_at_least_one_allowed_path(self):
        run_id, _ = self.start_repairable_run()
        self.rewrite_history()

        refused = self.cli("repair-verify", "--run", run_id)

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("--allowed-path", refused.stderr)

    def test_repair_verify_refuses_when_the_remote_advanced(self):
        run_id, _ = self.start_repairable_run()
        self.rewrite_history()
        self.advance_remote_elsewhere()

        refused = self.cli(
            "repair-verify", "--run", run_id, "--allowed-path", "config/service.conf"
        )

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("advanced during repair", refused.stderr)

    def test_repair_verify_requires_the_original_recovery_ref(self):
        run_id, _ = self.start_repairable_run()
        self.rewrite_history()
        self.git(
            "update-ref",
            "refs/dotfiles-publish/recovery/%s" % run_id,
            self.remote_tip(),
        )

        refused = self.cli(
            "repair-verify",
            "--run",
            run_id,
            "--allowed-path",
            "config/service.conf",
        )

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("no longer protects", refused.stderr)

    def test_repaired_history_needs_a_new_run_and_a_new_review(self):
        run_id, _ = self.start_repairable_run()
        self.rewrite_history()
        verified = self.cli(
            "repair-verify", "--run", run_id, "--allowed-path", "config/service.conf"
        )
        self.assertEqual(0, verified.returncode, verified.stderr)

        clean, new_run = self.scan()
        self.assertEqual(0, clean.returncode, clean.stdout + clean.stderr)
        self.assertNotEqual(run_id, new_run)
        self.assertFalse((self.run_dir(new_run) / "review.json").exists())

        self.write_full_audit_receipt(new_run)
        refused = self.cli("authorize", "--run", new_run)
        self.assertNotEqual(0, refused.returncode)
        self.assertIn("missing-unit", refused.stderr)

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

        self.set_remote_writable(False)
        failed = self.cli("push", "--run", new_run)
        self.set_remote_writable(True)
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

    def test_new_audit_blocks_already_issued_push_authorization(self):
        run_id = self.prepare_authorized_run()
        authorized = self.cli("authorize", "--run", run_id)
        self.assertEqual(0, authorized.returncode, authorized.stderr)
        self.scan("--mode", "full-audit")
        refused = self.cli("hook", "origin", str(self.remote), stdin=self.hook_input(run_id))
        self.assertNotEqual(0, refused.returncode)
        self.assertIn("pending review", refused.stderr)
        self.assertFalse((self.state_dir() / "authorization.json").exists())

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
        published = {
            line.split()[1]
            for line in listing.stdout.splitlines()
            if line.strip() and line.split()[1] != "HEAD"
        }
        self.assertEqual({"refs/heads/master"}, published)


    def test_release_publishes_the_branch_and_exactly_one_tag(self):
        self.publish_base()
        self.commit("prepare release", {"docs/changes.md": "changes\n"})
        notes = self.base / "release-notes.md"
        notes.write_text("Adds the changes document.\n")
        self.git("tag", "v1.2.3")

        proc, run_id = self.scan(
            "--mode", "release", "--tag", "v1.2.3", "--release-notes", str(notes)
        )
        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        public = [unit for unit in manifest["units"] if unit["kind"] == "public-text"]
        self.assertEqual(2, len(public), "release notes and tag text must be reviewed")
        self.write_full_audit_receipt(run_id)
        self.review_all(run_id)

        pushed = self.cli("push", "--run", run_id, "--tag", "v1.2.3")

        self.assertEqual(0, pushed.returncode, pushed.stdout + pushed.stderr)
        self.assertEqual(self.head(), self.remote_tip())
        self.assertEqual(self.head(), self.remote_tip("refs/tags/v1.2.3"))
        listing = self.git("ls-remote", "--tags", str(self.remote))
        self.assertEqual(1, len([line for line in listing.stdout.splitlines() if line.strip()]))

    def test_ordinary_publication_refuses_a_tag(self):
        run_id = self.prepare_authorized_run()
        self.git("tag", "v2.0.0")

        refused = self.cli("push", "--run", run_id, "--tag", "v2.0.0")

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("release run", refused.stderr)
        self.assertNotIn("refs/tags/", self.git("ls-remote", str(self.remote)).stdout)

    def test_release_refuses_a_substituted_or_omitted_reviewed_tag(self):
        self.publish_base()
        self.commit("release candidate", {"docs/release.md": "release\n"})
        self.git("tag", "v1.0.0")
        self.git("tag", "unreviewed-tag")
        proc, run_id = self.scan("--mode", "release", "--tag", "v1.0.0")
        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        self.review_all(run_id)
        self.write_full_audit_receipt(run_id)
        remote_before = self.remote_tip()

        for command in ("authorize", "push"):
            for tag_arguments in (("--tag", "unreviewed-tag"), ()):
                with self.subTest(command=command, tag_arguments=tag_arguments):
                    refused = self.cli(command, "--run", run_id, *tag_arguments)
                    self.assertNotEqual(0, refused.returncode)
                    self.assertIn("reviewed release tag", refused.stderr)
                    self.assertFalse((self.state_dir() / "authorization.json").exists())
                    self.assertEqual(remote_before, self.remote_tip())
                    self.assertEqual("", self.git("ls-remote", "--tags", str(self.remote)).stdout)

    def test_release_refuses_adding_a_tag_that_was_not_scanned(self):
        self.publish_base()
        self.commit("release candidate", {"docs/release.md": "release\n"})
        proc, run_id = self.scan("--mode", "release")
        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        self.review_all(run_id)
        self.write_full_audit_receipt(run_id)
        self.git("tag", "unreviewed-tag")

        refused = self.cli("push", "--run", run_id, "--tag", "unreviewed-tag")

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("reviewed release tag", refused.stderr)
        self.assertFalse((self.state_dir() / "authorization.json").exists())

    def test_hook_rejects_extra_refs_and_deletions(self):
        run_id = self.prepare_authorized_run()
        run = self.read_json(self.run_dir(run_id) / "run.json")
        authorized = "%s %s %s %s" % (
            run["candidate_oid"],
            run["candidate_oid"],
            run["remote_ref"],
            run["remote_oid"],
        )

        self.assertEqual(0, self.cli("authorize", "--run", run_id).returncode)
        extra = self.cli(
            "hook",
            "origin",
            str(self.remote),
            stdin=authorized
            + "\n%s %s refs/heads/extra %s\n"
            % (run["candidate_oid"], run["candidate_oid"], "0" * 40),
        )
        self.assertNotEqual(0, extra.returncode)
        self.assertIn("does not match the authorization", extra.stderr)

        self.assertEqual(0, self.cli("authorize", "--run", run_id).returncode)
        deletion = self.cli(
            "hook",
            "origin",
            str(self.remote),
            stdin="(delete) %s %s %s\n" % ("0" * 40, run["remote_ref"], run["remote_oid"]),
        )
        self.assertNotEqual(0, deletion.returncode)
        self.assertIn("deletion", deletion.stderr)


class DotfilesPublishInstallTests(PublicationFixture):
    BOOTSTRAP_PRE_PUSH = (
        "#!/bin/sh\n\n"
        "printf '%s\\n' \\\n"
        "  'Push blocked: guarded dotfiles publication is not installed yet."
        " Use publish-dotfiles after implementation.' \\\n"
        "  >&2\n"
        "exit 1\n"
    )

    def install_env(self, **extra):
        return self.env(DOTFILES_PUBLISH_EXPECTED_REMOTE=str(self.remote), **extra)

    def add_helpers(self):
        """Copy the tracked helpers into the disposable repository."""
        target = self.repo / "bin"
        target.mkdir(parents=True, exist_ok=True)
        for name in ("dotfiles-publish", "dotfiles-pre-push", "git-remote-dotfiles-blocked"):
            shutil.copy2(REPO_ROOT / "bin" / name, target / name)
            (target / name).chmod(0o755)
        self.git("add", "-A")
        self.git("commit", "--quiet", "-m", "add publication helpers")

    def hooks_dir(self):
        return self.repo / ".git" / "hooks"

    def write_unrelated_hooks(self):
        self.hooks_dir().mkdir(parents=True, exist_ok=True)
        for name in ("post-commit", "post-rewrite"):
            path = self.hooks_dir() / name
            path.write_text("#!/bin/sh\necho %s\n" % name)
            path.chmod(0o755)
        return {
            name: (self.hooks_dir() / name).read_bytes()
            for name in ("post-commit", "post-rewrite")
        }

    def test_install_replaces_the_bootstrap_blocker_and_leaves_other_hooks_alone(self):
        self.publish_base()
        self.add_helpers()
        untouched = self.write_unrelated_hooks()
        bootstrap = self.hooks_dir() / "pre-push"
        bootstrap.write_text(self.BOOTSTRAP_PRE_PUSH)
        bootstrap.chmod(0o755)

        installed = self.cli("install", env=self.install_env())

        self.assertEqual(0, installed.returncode, installed.stdout + installed.stderr)
        hook = self.hooks_dir() / "pre-push"
        self.assertTrue(hook.is_symlink())
        self.assertEqual(
            os.path.realpath(self.repo / "bin" / "dotfiles-pre-push"),
            os.path.realpath(hook),
        )
        for name, content in untouched.items():
            self.assertEqual(content, (self.hooks_dir() / name).read_bytes())
        self.assertEqual(
            str(self.remote),
            self.git("config", "--get", "remote.origin.url").stdout.strip(),
        )
        self.assertEqual(
            "dotfiles-blocked::origin",
            self.git("config", "--get", "remote.origin.pushurl").stdout.strip(),
        )

    def test_install_refuses_an_unrelated_pre_push_hook(self):
        self.publish_base()
        self.add_helpers()
        self.hooks_dir().mkdir(parents=True, exist_ok=True)
        hook = self.hooks_dir() / "pre-push"
        hook.write_text("#!/bin/sh\nexec ./scripts/lint.sh\n")
        hook.chmod(0o755)

        refused = self.cli("install", env=self.install_env())

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("merge it with bin/dotfiles-pre-push", refused.stderr)
        self.assertEqual("#!/bin/sh\nexec ./scripts/lint.sh\n", hook.read_text())
        self.assertFalse(hook.is_symlink())

    def test_install_is_idempotent(self):
        self.publish_base()
        self.add_helpers()

        first = self.cli("install", env=self.install_env())
        self.assertEqual(0, first.returncode, first.stderr)
        self.assertIn("hook: installed", first.stdout)

        second = self.cli("install", env=self.install_env())
        self.assertEqual(0, second.returncode, second.stderr)
        self.assertIn("hook: unchanged", second.stdout)
        self.assertIn("pushurl: unchanged", second.stdout)

    def test_installed_remote_blocks_a_no_verify_push(self):
        self.publish_base()
        self.add_helpers()
        self.assertEqual(0, self.cli("install", env=self.install_env()).returncode)
        before = self.remote_tip()
        self.commit("unreviewed work", {"docs/unreviewed.md": "unreviewed\n"})

        blocked = self.git("push", "--no-verify", "origin", "master", check=False)

        self.assertNotEqual(0, blocked.returncode)
        self.assertIn(BLOCKED_PUSH_MESSAGE, blocked.stderr)
        self.assertEqual(before, self.remote_tip())

    def test_authorized_url_push_passes_through_the_tracked_hook(self):
        self.publish_base()
        self.add_helpers()
        self.assertEqual(0, self.cli("install", env=self.install_env()).returncode)
        self.commit("add helper", {"docs/helper.md": "helper notes\n"})
        proc, run_id = self.scan()
        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        self.write_full_audit_receipt(run_id)
        self.review_all(run_id)

        pushed = self.cli("push", "--run", run_id)

        self.assertEqual(0, pushed.returncode, pushed.stdout + pushed.stderr)
        self.assertIn(
            "push authorized for run %s" % run_id,
            pushed.stdout,
            "the tracked pre-push hook did not run for the authorized URL push",
        )
        self.assertEqual(self.head(), self.remote_tip())


class DotfilesPublishFullAuditSnapshotTests(unittest.TestCase):
    """Exercise actual receipt/authorization logic with deterministic interleavings."""

    def setUp(self):
        loaded = runpy.run_path(str(PUBLISH), run_name="isolated_receipt_tests")
        self.functions = loaded["record_full_audit"].__globals__
        self.repo = SimpleNamespace(
            root=Path("/synthetic-repo"), state_dir=Path("/synthetic-state")
        )
        self.audit_id = "same-audit-run"
        self.publish_id = "outgoing-publish-run"
        self.records = {
            self.audit_id: {
                "mode": "full-audit", "scan_id": "scan-A",
                "manifest_id": "manifest-A", "scanned_epoch": FIXED_NOW,
                "ruleset_id": "policy", "incident_digest": "unchanged-incidents",
                "public_refs": ["refs/heads/master old-public-tip"],
            },
            self.publish_id: {
                "mode": "publish", "scan_id": "publish-scan",
                "manifest_id": "publish-manifest", "ruleset_id": "policy",
                "candidate_oid": "b" * 40, "remote_oid": "a" * 40,
                "remote_ref": "refs/heads/master", "remote_url": "synthetic-no-network",
            },
        }
        self.manifests = {
            self.audit_id: {
                "manifest_id": "manifest-A", "units": [{"unit_id": "A", "kind": "patch"}],
            },
            self.publish_id: {
                "manifest_id": "publish-manifest", "units": [{"unit_id": "P", "kind": "patch"}],
            },
        }
        self.reviews = {
            self.audit_id: {"manifest_id": "manifest-A", "entries": {"A": {"verdict": "clean"}}},
            self.publish_id: {"manifest_id": "publish-manifest", "entries": {"P": {"verdict": "clean"}}},
        }
        self.writes = {}
        self.receipt_path = self.repo.state_dir / "full-audit.json"
        self.authorization_path = self.repo.state_dir / "authorization.json"

        def write_json(path, payload):
            self.writes[path] = copy.deepcopy(payload)

        self.functions.update({
            "load_run": lambda repo, run_id: copy.deepcopy(self.records[run_id]),
            "load_manifest": lambda repo, run_id: copy.deepcopy(self.manifests[run_id]),
            "load_review": lambda repo, run_id, manifest: copy.deepcopy(self.reviews[run_id]),
            "load_findings": lambda repo, run_id: {"findings": []},
            "resolved_fingerprints": lambda repo: frozenset(),
            "incident_state_digest": lambda repo: "unchanged-incidents",
            "ruleset_identity": lambda root: "policy",
            "now_epoch": lambda: FIXED_NOW,
            "full_audit_receipt": lambda repo: copy.deepcopy(self.writes.get(self.receipt_path)),
            "write_json": write_json,
            "revalidate_run": lambda repo, run_id: (copy.deepcopy(self.records[run_id]), None),
        })

    def replace_scan(self, changed_manifest=True):
        # Both scans intentionally have the same boundary and wall-clock second.
        self.records[self.audit_id]["scan_id"] = "scan-B"
        if changed_manifest:
            self.records[self.audit_id].update(
                manifest_id="manifest-B",
                public_refs=["refs/heads/master old-public-tip", "refs/heads/other new-public-tip"],
            )
            self.manifests[self.audit_id] = {
                "manifest_id": "manifest-B",
                "units": [{"unit_id": "A", "kind": "patch"}, {"unit_id": "B-unreviewed", "kind": "patch"}],
            }

    def test_clean_status_cannot_certify_a_replacement_manifest(self):
        status = self.functions["review_status"](self.repo, self.audit_id)
        self.assertTrue(status.clean)
        self.replace_scan()

        with self.assertRaisesRegex(self.functions["PublishError"], "changed after review"):
            self.functions["record_full_audit"](self.repo, self.audit_id, status)

        self.assertNotIn(self.receipt_path, self.writes)
        with self.assertRaisesRegex(self.functions["PublishError"], "different manifest"):
            self.functions["review_status"](self.repo, self.audit_id)
        with self.assertRaisesRegex(self.functions["PublishError"], "full audit required"):
            self.functions["authorize_run"](self.repo, self.publish_id, None, FIXED_NOW)
        self.assertNotIn(self.authorization_path, self.writes)

    def test_same_second_identical_rescan_requires_status_for_current_scan(self):
        status = self.functions["review_status"](self.repo, self.audit_id)
        self.replace_scan(changed_manifest=False)

        with self.assertRaisesRegex(self.functions["PublishError"], "changed after review"):
            self.functions["record_full_audit"](self.repo, self.audit_id, status)

        # Existing verdicts remain safe for a byte-identical manifest, but the
        # status that certifies the newly completed scan must be computed anew.
        current = self.functions["review_status"](self.repo, self.audit_id)
        receipt = self.functions["record_full_audit"](self.repo, self.audit_id, current)
        self.assertEqual("scan-B", receipt["scan_id"])
        self.assertIsNone(self.functions["full_audit_staleness"](self.repo, "policy", FIXED_NOW))

    def test_replacement_during_receipt_write_cannot_authorize_publication(self):
        status = self.functions["review_status"](self.repo, self.audit_id)
        original_write = self.functions["write_json"]

        def replace_before_write(path, payload):
            self.replace_scan()
            original_write(path, payload)

        self.functions["write_json"] = replace_before_write
        receipt = self.functions["record_full_audit"](self.repo, self.audit_id, status)

        self.assertEqual("scan-A", receipt["scan_id"])
        self.assertEqual("manifest-A", receipt["manifest_id"])
        self.assertEqual(["refs/heads/master old-public-tip"], receipt["public_refs"])
        self.assertIn("replaced", self.functions["full_audit_staleness"](self.repo, "policy", FIXED_NOW))
        with self.assertRaisesRegex(self.functions["PublishError"], "full audit required.*replaced"):
            self.functions["authorize_run"](self.repo, self.publish_id, None, FIXED_NOW)
        self.assertNotIn(self.authorization_path, self.writes)

    def test_review_status_rejects_mixed_run_and_manifest_snapshots(self):
        self.records[self.audit_id]["manifest_id"] = "manifest-B"

        with self.assertRaisesRegex(self.functions["PublishError"], "changed during review"):
            self.functions["review_status"](self.repo, self.audit_id)

    def test_receipt_revalidation_checks_the_stored_manifest(self):
        status = self.functions["review_status"](self.repo, self.audit_id)
        self.functions["record_full_audit"](self.repo, self.audit_id, status)
        self.manifests[self.audit_id]["manifest_id"] = "manifest-B"

        self.assertIn("replaced", self.functions["full_audit_staleness"](self.repo, "policy", FIXED_NOW))


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

    def publish_a_secret(self):
        """Put a secret on the public branch, the way a real leak arrives."""
        self.publish_base()
        self.commit(
            "publish configuration",
            {"config/service.conf": "verification token = %s\n" % TEST_SECRET},
        )
        self.git("-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "master")
        self.git("fetch", "--quiet", "origin")

    def public_incident(self, run_id):
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        incidents = [
            finding
            for finding in findings
            if finding.get("classification") == "public-incident"
        ]
        self.assertTrue(incidents, "no finding was classified as a public incident")
        return incidents[0]

    def write_evidence(self, fingerprint, **extra):
        evidence = {
            "fingerprint": fingerprint,
            "action_time": "2026-01-02T03:04:05Z",
            "verification": "the credential was rotated and the old one now fails",
        }
        evidence.update(extra)
        path = self.base / ("evidence-%s.json" % fingerprint[:8])
        path.write_text(json.dumps(evidence))
        return path

    def test_full_audit_finding_on_public_ref_is_classified_as_incident(self):
        self.publish_a_secret()

        proc, run_id = self.scan("--mode", "full-audit")

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        self.assertNotIn(TEST_SECRET, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        classes = {finding.get("classification") for finding in findings}
        self.assertIn("public-incident", classes)
        self.assertIn("gitguardian-triage", proc.stdout)
        self.assertIn("rotate or revoke", proc.stdout)
        self.assertFalse((self.state_dir() / "authorization.json").exists())

    def test_full_audit_reviews_bounded_complete_history_patches(self):
        self.publish_base()
        first = self.commit(
            "first historical change",
            {"docs/changes.md": "first historical line\n"},
        )
        second = self.commit(
            "second historical change",
            {"docs/changes.md": "first historical line\nsecond historical line\n"},
        )

        proc, run_id = self.scan("--mode", "full-audit")

        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        self.assertFalse(
            [unit for unit in manifest["units"] if unit["kind"] in ("commit", "path")]
        )
        patches = [unit for unit in manifest["units"] if unit["kind"] == "patch"]
        self.assertTrue(patches)
        serialized = "\n".join(unit["content"] for unit in patches)
        for expected in (
            first,
            second,
            "first historical change",
            "second historical change",
            "+first historical line",
            "+second historical line",
        ):
            self.assertIn(expected, serialized)
        for unit in patches:
            self.assertLessEqual(len(unit["content"].encode("utf-8")), 128 * 1024)

    def test_full_audit_finding_includes_redacted_source_context(self):
        self.publish_base()
        self.commit(
            "historical configuration",
            {
                "config/service.conf": (
                    "service = example\n"
                    "token = %s\n"
                    "purpose = regression test\n" % TEST_SECRET
                )
            },
        )

        proc, run_id = self.scan("--mode", "full-audit")

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        serialized = json.dumps(manifest)
        self.assertNotIn(TEST_SECRET, serialized)
        findings = [
            unit for unit in manifest["units"] if unit["kind"] == "deterministic-finding"
        ]
        self.assertTrue(findings)
        source_context = "\n".join(unit["content"] for unit in findings)
        self.assertIn("source excerpt:", source_context)
        history_context = "\n".join(
            unit["content"]
            for unit in findings
            if '"source": "gitleaks-history"' in unit["content"]
        )
        self.assertIn("Git patch coordinates are not blob coordinates", history_context)
        self.assertNotIn("service = example", history_context)
        self.assertNotIn("purpose = regression test", history_context)
        self.assertIn("service = example", source_context)
        self.assertIn("purpose = regression test", source_context)
        self.assertIn("[REDACTED:", source_context)

    def test_full_audit_redacts_a_secret_present_only_in_public_history(self):
        self.publish_base()
        leaking = self.commit(
            "add historical fixture",
            {
                "config/historical.conf": (
                    "historical sample %s\nordinary trailing line\n" % TEST_SECRET
                )
            },
        )
        self.commit("remove historical fixture", remove=("config/historical.conf",))
        self.git("-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "master")
        self.git("fetch", "--quiet", "origin")

        proc, run_id = self.scan("--mode", "full-audit")

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        historical = [
            finding
            for finding in findings
            if finding["source"] == "gitleaks-history"
            and finding["commit"] == leaking
            and finding["path"] == "config/historical.conf"
        ]
        self.assertTrue(historical, findings)
        self.assertEqual("public-incident", historical[0]["classification"])
        self.assert_run_redacts(
            proc, run_id, TEST_SECRET, historical[0]["fingerprint"]
        )

    def test_full_audit_redacts_encoded_source_when_scanner_reports_decoded_value(self):
        encoded = TEST_SECRET.encode("utf-8").hex()
        self.publish_base()
        leaking = self.commit(
            "add encoded historical fixture",
            {"config/encoded.conf": "historical sample %s\n" % encoded},
        )
        self.commit("remove encoded historical fixture", remove=("config/encoded.conf",))
        self.git("-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "master")
        self.git("fetch", "--quiet", "origin")

        proc, run_id = self.scan(
            "--mode",
            "full-audit",
            env=self.env(
                DOTFILES_FAKE_GITLEAKS_PATTERNS=re.escape(encoded),
                DOTFILES_FAKE_GITLEAKS_REPORTED_SECRET=TEST_SECRET,
            ),
        )

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        historical = [
            finding
            for finding in findings
            if finding["source"] == "gitleaks-history"
            and finding["commit"] == leaking
            and finding["path"] == "config/encoded.conf"
        ]
        self.assertTrue(historical, findings)
        self.assert_run_redacts(proc, run_id, encoded, historical[0]["fingerprint"])
        for path in self.run_dir(run_id).rglob("*"):
            if path.is_file():
                self.assertNotIn(TEST_SECRET, path.read_text(errors="replace"))

        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        unit = next(
            unit
            for unit in manifest["units"]
            if unit["kind"] == "deterministic-finding"
            and unit["detail"]["fingerprint"] == historical[0]["fingerprint"]
        )
        findings_file = self.base / "encoded-review-finding.json"
        findings_file.write_text(
            json.dumps([{"rule_id": "manual", "context": encoded}])
        )
        recorded = self.cli(
            "review-record",
            "--run",
            run_id,
            "--unit",
            unit["unit_id"],
            "--verdict",
            "finding",
            "--findings",
            str(findings_file),
        )
        self.assertEqual(1, recorded.returncode, recorded.stdout + recorded.stderr)
        self.assertIn("contains a value the scanner detected", recorded.stderr)
        review_path = self.run_dir(run_id) / "review.json"
        if review_path.exists():
            self.assertNotIn(encoded, review_path.read_text())

    def test_full_audit_recovers_decoded_source_from_the_git_patch_stream(self):
        encoded = TEST_SECRET.encode("utf-8").hex()
        target = ":(glob)*.enc"
        textconv = self.base / "prefix-lines.sh"
        textconv.write_text("#!/bin/sh\nprintf '\\n\\n\\n\\n'\n/bin/cat \"$1\"\n")
        textconv.chmod(0o700)
        self.publish_base()
        self.git("config", "diff.audit-fixture.textconv", str(textconv))
        leaking = self.commit(
            "add textconv historical fixture",
            {
                ".gitattributes": "*.enc diff=audit-fixture\n",
                target: encoded + "\n",
                "decoy.enc": "DECOY-CANARY\n",
            },
        )
        self.commit("remove textconv fixture", remove=(target,))
        self.git("-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "master")
        self.git("fetch", "--quiet", "origin")

        proc, run_id = self.scan(
            "--mode",
            "full-audit",
            env=self.env(
                DOTFILES_FAKE_GITLEAKS_PATTERNS=re.escape(encoded),
                DOTFILES_FAKE_GITLEAKS_REPORTED_SECRET=TEST_SECRET,
                DOTFILES_FAKE_GITLEAKS_OMIT_GIT_LINE="1",
            ),
        )

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        historical = next(
            finding
            for finding in findings
            if finding["source"] == "gitleaks-history"
            and finding["commit"] == leaking
            and finding["path"] == target
        )
        self.assertEqual(1, historical["line"])
        self.assert_run_redacts(proc, run_id, encoded, historical["fingerprint"])
        manifest = (self.run_dir(run_id) / "manifest.json").read_text()
        self.assertIn("DECOY-CANARY", manifest)

    def test_full_audit_source_recovery_never_runs_failing_textconv(self):
        encoded = TEST_SECRET.encode("utf-8").hex()
        canary = "TEXTCONV-SECRET-CANARY"
        marker = self.base / "failing-textconv-ran"
        textconv = self.base / "failing-textconv.sh"
        textconv.write_text(
            "#!/bin/sh\n"
            f"touch {marker}\n"
            "printf '%s\\n' \"%s\" >&2\n"
            "exit 9\n" % ("%s", canary)
        )
        textconv.chmod(0o700)
        self.publish_base()
        self.git("config", "diff.audit-fixture.textconv", str(textconv))
        self.commit(
            "add failing textconv fixture",
            {
                ".gitattributes": "*.enc diff=audit-fixture\n",
                "failure.enc": encoded + "\n",
            },
        )
        self.commit("remove failing textconv fixture", remove=("failure.enc",))
        self.git("-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "master")
        self.git("fetch", "--quiet", "origin")

        proc = self.cli(
            "scan",
            "--mode",
            "full-audit",
            env=self.env(
                DOTFILES_FAKE_GITLEAKS_PATTERNS=re.escape(encoded),
                DOTFILES_FAKE_GITLEAKS_REPORTED_SECRET=TEST_SECRET,
                DOTFILES_FAKE_GITLEAKS_OMIT_GIT_LINE="1",
            ),
        )

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        self.assertFalse(marker.exists())
        for value in (canary, encoded, TEST_SECRET):
            self.assertNotIn(value, proc.stdout + proc.stderr)
            for path in self.state_files():
                self.assertNotIn(value, path.read_text(errors="replace"), str(path))

    def test_full_audit_uses_lines_when_scanner_columns_are_impossible(self):
        encoded = TEST_SECRET.encode("utf-8").hex()
        line_prefix = "LINE-FIRST-CANARY"
        self.publish_base()
        leaking = self.commit(
            "add line-first fixture",
            {
                "config/line-first.conf": "%s configuration %s TRAILING-CANARY\n"
                % (line_prefix, encoded),
                "docs/benign.md": "configuration remains visible\n",
            },
        )
        self.commit("remove line-first fixture", remove=("config/line-first.conf",))
        self.git("-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "master")
        self.git("fetch", "--quiet", "origin")

        proc, run_id = self.scan(
            "--mode",
            "full-audit",
            env=self.env(
                DOTFILES_FAKE_GITLEAKS_PATTERNS=re.escape(encoded),
                DOTFILES_FAKE_GITLEAKS_REPORTED_SECRET=TEST_SECRET,
                DOTFILES_FAKE_GITLEAKS_INVALID_COLUMNS="1",
            ),
        )

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        historical = next(
            finding
            for finding in findings
            if finding["source"] == "gitleaks-history"
            and finding["commit"] == leaking
            and finding["path"] == "config/line-first.conf"
        )
        self.assert_run_redacts(proc, run_id, encoded, historical["fingerprint"])

        persisted = "\n".join(
            path.read_text(encoding="utf-8", errors="replace")
            for path in self.run_dir(run_id).rglob("*")
            if path.is_file()
        )
        self.assertNotIn(line_prefix, persisted)
        shown_units = []
        for unit_id in self.unit_ids(run_id):
            shown = self.cli("review-show", "--run", run_id, "--unit", unit_id)
            self.assertEqual(0, shown.returncode, shown.stderr)
            shown_units.append(shown.stdout)
        shown_text = "\n".join(shown_units)
        self.assertNotIn(line_prefix, shown_text)
        self.assertIn("configuration remains visible", shown_text)

        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        target = next(
            unit
            for unit in manifest["units"]
            if unit["kind"] == "deterministic-finding"
            and unit["detail"]["fingerprint"] == historical["fingerprint"]
        )
        safe_unit = next(
            unit for unit in manifest["units"] if unit["unit_id"] != target["unit_id"]
        )
        clean = self.cli(
            "review-record",
            "--run",
            run_id,
            "--unit",
            safe_unit["unit_id"],
            "--verdict",
            "clean",
        )
        self.assertEqual(0, clean.returncode, clean.stderr)
        findings_file = self.base / "invalid-column-review.json"
        findings_file.write_text(
            json.dumps([{"rule_id": "manual", "context": encoded}])
        )
        rejected = self.cli(
            "review-record",
            "--run",
            run_id,
            "--unit",
            target["unit_id"],
            "--verdict",
            "finding",
            "--findings",
            str(findings_file),
        )
        self.assertEqual(1, rejected.returncode, rejected.stdout + rejected.stderr)
        self.assertIn("contains a value the scanner detected", rejected.stderr)
        review = (self.run_dir(run_id) / "review.json").read_text()
        self.assertNotIn(encoded, review)
        self.assertNotIn(line_prefix, review)

    def test_full_audit_patch_bundles_include_merge_resolution_content(self):
        self.publish_base()
        self.commit("add merge fixture", {"docs/merge.md": "base\n"})
        self.git("switch", "--quiet", "-c", "side")
        self.commit("side change", {"docs/merge.md": "side\n"})
        self.git("switch", "--quiet", "master")
        self.commit("main change", {"docs/merge.md": "main\n"})
        merged = self.git("merge", "--no-ff", "side", check=False)
        self.assertNotEqual(0, merged.returncode)
        merge_path = self.repo / "docs/merge.md"
        merge_path.write_text("manual merge resolution\n")
        self.git("add", "docs/merge.md")
        self.git("commit", "--quiet", "-m", "merge resolved manually")
        merge_commit = self.head()

        proc, run_id = self.scan("--mode", "full-audit")

        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        serialized = "\n".join(
            unit["content"] for unit in manifest["units"] if unit["kind"] == "patch"
        )
        self.assertIn(merge_commit, serialized)
        self.assertIn("merge resolved manually", serialized)
        self.assertIn("manual merge resolution", serialized)

    def test_full_audit_patch_bundles_show_a_merge_equal_to_one_parent(self):
        self.publish_base()
        self.commit("add decision fixture", {"docs/decision.md": "base decision\n"})
        self.git("switch", "--quiet", "-c", "side")
        self.commit("side decision", {"docs/decision.md": "side decision\n"})
        self.git("switch", "--quiet", "master")
        self.commit("main decision", {"docs/decision.md": "main decision\n"})
        merged = self.git("merge", "--no-ff", "side", check=False)
        self.assertNotEqual(0, merged.returncode)
        decision_path = self.repo / "docs/decision.md"
        decision_path.write_text("main decision\n")
        self.git("add", "docs/decision.md")
        self.git("commit", "--quiet", "-m", "keep main decision at merge")
        merge_commit = self.head()

        proc, run_id = self.scan("--mode", "full-audit")

        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        serialized = "\n".join(
            unit["content"] for unit in manifest["units"] if unit["kind"] == "patch"
        )
        marker = "=== AUDIT COMMIT %s PART 1/1 ===" % merge_commit
        merge_frame = serialized.split(marker, 1)[1].split("=== AUDIT COMMIT", 1)[0]
        self.assertIn("keep main decision at merge", merge_frame)
        self.assertIn("diff --git a/docs/decision.md b/docs/decision.md", merge_frame)
        self.assertIn("-side decision", merge_frame)
        self.assertIn("+main decision", merge_frame)

    def test_full_audit_detects_a_risky_file_created_only_by_a_merge(self):
        self.publish_base()
        self.commit("add merge base", {"docs/base.md": "base\n"})
        self.git("switch", "--quiet", "-c", "side")
        self.commit("side work", {"docs/side.md": "side\n"})
        self.git("switch", "--quiet", "master")
        self.commit("main work", {"docs/main.md": "main\n"})
        merged = self.git("merge", "--no-ff", "--no-commit", "side")
        self.assertEqual(0, merged.returncode, merged.stdout + merged.stderr)
        merge_only = self.repo / "config/.env.merge-only"
        merge_only.parent.mkdir(parents=True, exist_ok=True)
        merge_only.write_text("EXAMPLE=placeholder\n")
        self.git("add", "config/.env.merge-only")
        self.git("commit", "--quiet", "-m", "add merge-only fixture")

        proc, run_id = self.scan("--mode", "full-audit")

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        self.assertIn("config/.env.merge-only", {finding["path"] for finding in findings})
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        high_risk = {
            unit["path"] for unit in manifest["units"] if unit["kind"] == "high-risk-file"
        }
        self.assertIn("config/.env.merge-only", high_risk)

    def test_full_audit_chunking_preserves_a_long_multibyte_line(self):
        self.publish_base()
        payload = "é" * 70000
        self.commit("add long line", {"docs/long.txt": payload + "\n"})

        proc, run_id = self.scan("--mode", "full-audit")

        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        patches = [unit for unit in manifest["units"] if unit["kind"] == "patch"]
        self.assertGreater(len(patches), 1)
        self.assertEqual(70000, sum(unit["content"].count("é") for unit in patches))
        for unit in patches:
            self.assertLessEqual(len(unit["content"].encode("utf-8")), 128 * 1024)

    def test_full_audit_object_reader_does_not_deadlock_on_full_pipes(self):
        self.publish_base()
        long_path = "/".join(["x" * 50] * 8) + "/large.bin"
        self.commit(
            "add audit objects",
            {"large.bin": b"x" * (128 * 1024), long_path: b"x"},
        )
        large_request = "HEAD:large.bin"
        repeated_request = "HEAD:%s" % long_path
        probe = """
import runpy
import sys
from pathlib import Path

module = runpy.run_path(sys.argv[1], run_name="dotfiles_publish_probe")
repo = type("Repo", (), {"root": Path(sys.argv[2])})()
module["cat_file_batch"](repo, [sys.argv[3]] + [sys.argv[4]] * 5000)
"""
        try:
            completed = subprocess.run(
                [
                    sys.executable,
                    "-c",
                    probe,
                    str(PUBLISH),
                    str(self.repo),
                    large_request,
                    repeated_request,
                ],
                capture_output=True,
                text=True,
                timeout=5,
            )
        except subprocess.TimeoutExpired:
            self.fail("git cat-file batch protocol deadlocked on full pipes")
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)

    def test_full_audit_covers_pull_request_heads_and_removes_its_refs(self):
        self.publish_base()
        self.commit(
            "work in a pull request", {"config/pr.conf": "token = %s\n" % TEST_SECRET}
        )
        pull_head = self.head()
        self.git(
            "-c",
            "core.hooksPath=/dev/null",
            "push",
            "--quiet",
            "origin",
            "HEAD:refs/pull/7/head",
        )
        self.git("reset", "--quiet", "--hard", self.remote_tip())

        proc, run_id = self.scan("--mode", "full-audit")

        self.assertEqual(2, proc.returncode, proc.stdout + proc.stderr)
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        self.assertIn(
            pull_head,
            {finding["commit"] for finding in findings},
            "the pull-request head was not audited",
        )
        self.assertEqual("public-incident", self.public_incident(run_id)["classification"])
        remaining = self.git(
            "for-each-ref", "--format=%(refname)", "refs/dotfiles-publish/audit/"
        ).stdout.strip()
        self.assertEqual("", remaining, "temporary audit refs were left behind")

    def test_audit_refs_are_removed_after_a_scanner_crash(self):
        self.publish_base()
        self.git(
            "-c",
            "core.hooksPath=/dev/null",
            "push",
            "--quiet",
            "origin",
            "HEAD:refs/pull/9/head",
        )

        proc = self.cli(
            "scan",
            "--mode",
            "full-audit",
            env=self.env(DOTFILES_FAKE_GITLEAKS_FAIL="9"),
        )

        self.assertEqual(3, proc.returncode, proc.stdout + proc.stderr)
        remaining = self.git(
            "for-each-ref", "--format=%(refname)", "refs/dotfiles-publish/audit/"
        ).stdout.strip()
        self.assertEqual("", remaining, "a scanner crash left temporary audit refs behind")

    def test_incident_record_resolves_exactly_one_fingerprint(self):
        self.publish_a_secret()
        proc, run_id = self.scan("--mode", "full-audit")
        finding = self.public_incident(run_id)
        legacy_fingerprint = "a" * 20
        legacy_incidents = {
            "schema": 1,
            "incidents": {
                legacy_fingerprint: {
                    "resolution": "rotated",
                    "resolved_at": "2025-01-01T00:00:00Z",
                }
            },
        }
        incidents_path = self.state_dir() / "incidents.json"
        incidents_path.write_text(json.dumps(legacy_incidents))
        incidents_path.chmod(0o600)

        recorded = self.cli(
            "incident-record",
            "--run",
            run_id,
            "--fingerprint",
            finding["fingerprint"],
            "--resolution",
            "rotated",
            "--evidence",
            str(self.write_evidence(finding["fingerprint"])),
        )

        self.assertEqual(0, recorded.returncode, recorded.stdout + recorded.stderr)
        incidents = self.read_json(self.state_dir() / "incidents.json")["incidents"]
        self.assertEqual(legacy_incidents["incidents"][legacy_fingerprint], incidents[legacy_fingerprint])
        self.assertEqual("rotated", incidents[finding["fingerprint"]]["resolution"])
        self.assertNotIn("value", json.dumps(incidents))
        self.assertIn("evidence_sha256", incidents[finding["fingerprint"]])

        status = self.cli("review-status", "--run", run_id)
        self.assertNotIn(
            "deterministic-finding: %s" % finding["fingerprint"], status.stdout
        )

    def manual_public_fixture(self):
        self.publish_base()
        commit = self.commit("public meeting configuration", {"docs/room.txt": "synthetic meeting access setting\n"})
        self.git("-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "master")
        self.git("fetch", "--quiet", "origin")
        private = self.commit("local configuration", {"docs/private.txt": "unpublished setting\n"})
        _, run_id = self.scan("--mode", "full-audit")
        self.assertEqual([], self.read_json(self.run_dir(run_id) / "findings.json")["findings"])
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        unit = next(u for u in manifest["units"] if commit in (u.get("detail") or {}).get("commits", []))
        finding = {"rule_id": "manual-meeting-secret", "classification": "public-incident",
                   "commit": commit, "path": "docs/room.txt", "context": "synthetic historical access credential"}
        return run_id, unit, finding, private

    def record_manual_public(self, run_id, unit, findings):
        path = self.base / "manual-public.json"
        path.write_text(json.dumps(findings))
        return self.cli("review-record", "--run", run_id, "--unit", unit["unit_id"],
                        "--verdict", "finding", "--findings", str(path))

    def test_manual_public_incident_exact_resolution_and_fresh_audit(self):
        run_id, unit, finding, private = self.manual_public_fixture()
        records = [finding, dict(finding, rule_id="manual-second-access-setting")]
        recorded = self.record_manual_public(run_id, unit, records)
        self.assertEqual(0, recorded.returncode, recorded.stderr)
        self.review_all(run_id, skip=(unit["unit_id"],))
        stored = self.read_json(self.run_dir(run_id) / "review.json")["entries"][unit["unit_id"]]["findings"]
        printed = re.findall(r"^manual-public-incident: ([0-9a-f]+)$", recorded.stdout, re.MULTILINE)
        self.assertEqual([r["fingerprint"] for r in stored], printed)
        self.assertEqual(2, len({r["fingerprint"] for r in stored}))
        for r in stored:
            self.assertEqual("manual-public-incident", r["source"])
            self.assertEqual(self.git("rev-parse", finding["commit"] + ":docs/room.txt").stdout.strip(), r["object"])
        for index, r in enumerate(stored):
            resolution = "rotated" if index == 0 else "accepted-risk"
            evidence = self.write_evidence(r["fingerprint"], rationale="Synthetic exact exception.",
                                           authorization="Owner authorized this exact synthetic incident.")
            proc = self.cli("incident-record", "--run", run_id, "--fingerprint", r["fingerprint"],
                            "--resolution", resolution, "--evidence", str(evidence))
            self.assertEqual(0, proc.returncode, proc.stderr)
            if index == 0:
                self.assertNotEqual(0, self.cli("review-status", "--run", run_id).returncode)
                self.assertNotIn(stored[1]["fingerprint"], self.read_json(self.state_dir() / "incidents.json")["incidents"])
        stale = self.cli("review-status", "--run", run_id)
        self.assertNotEqual(0, stale.returncode)
        self.assertIn("incident state changed", stale.stderr)
        _, fresh = self.scan("--mode", "full-audit")
        fresh_manifest = self.read_json(self.run_dir(fresh) / "manifest.json")
        fresh_unit = next(u for u in fresh_manifest["units"] if finding["commit"] in (u.get("detail") or {}).get("commits", []))
        borrowed = dict(finding, fingerprint=stored[0]["fingerprint"], commit=private,
                        path="docs/private.txt", source="review", classification="outgoing")
        rejected = self.record_manual_public(fresh, fresh_unit, [borrowed])
        self.assertNotEqual(0, rejected.returncode)
        self.assertIn("review fingerprint differs", rejected.stderr)
        self.assertEqual(0, self.record_manual_public(fresh, fresh_unit, records).returncode)
        fresh_records = self.read_json(self.run_dir(fresh) / "review.json")["entries"][fresh_unit["unit_id"]]["findings"]
        self.assertEqual(stored, fresh_records)
        self.review_all(fresh, skip=(fresh_unit["unit_id"],))
        status = self.cli("review-status", "--run", fresh)
        self.assertEqual(0, status.returncode, status.stderr)
        self.assertIn("full-audit-recorded:", status.stdout)

    def test_manual_public_incident_rejects_forged_source_identity(self):
        run_id, unit, finding, private = self.manual_public_fixture()
        # A later public ref must not retroactively alter the captured boundary.
        self.git("-c", "core.hooksPath=/dev/null", "push", "--quiet", "origin", "master")
        self.git("fetch", "--quiet", "origin")
        cases = [dict(finding, object="0" * 40), dict(finding, fingerprint="a" * 20),
                 dict(finding, path="docs/missing.txt"), dict(finding, path="docs/*.txt"),
                 dict(finding, commit="0" * 40), dict(finding, commit=private, path="docs/private.txt"),
                 dict(finding, line=-1), dict(finding, line=999999), dict(finding, line=1, column=999999),
                 dict(finding, source="manual-public-incident", classification="outgoing")]
        for record in cases:
            with self.subTest(record=record):
                proc = self.record_manual_public(run_id, unit, [record])
                self.assertNotEqual(0, proc.returncode)
                self.assertIn("manual public incident", proc.stderr)
        self.assertFalse((self.run_dir(run_id) / "review.json").exists())
        self.assertFalse((self.state_dir() / "incidents.json").exists())

    def test_manual_public_incident_revalidates_record_and_rejects_unknown(self):
        run_id, unit, finding, _ = self.manual_public_fixture()
        unknown = "a" * 20
        proc = self.cli("incident-record", "--run", run_id, "--fingerprint", unknown, "--resolution", "rotated",
                        "--evidence", str(self.write_evidence(unknown)))
        self.assertNotEqual(0, proc.returncode)
        self.assertEqual(0, self.record_manual_public(run_id, unit, [finding]).returncode)
        path = self.run_dir(run_id) / "review.json"
        original = self.read_json(path)
        token = original["entries"][unit["unit_id"]]["findings"][0]["fingerprint"]
        for tamper in ("digest", "object", "verdict", "classification"):
            review = copy.deepcopy(original)
            entry = review["entries"][unit["unit_id"]]
            if tamper == "digest": entry["digest"] = "0" * 64
            elif tamper == "verdict": entry["verdict"] = "clean"
            else:
                entry["findings"][0][tamper] = "0" * 40 if tamper == "object" else "outgoing"
                entry["digest"] = hashlib.sha256(json.dumps(entry["findings"], sort_keys=True).encode()).hexdigest()
            path.write_text(json.dumps(review))
            proc = self.cli("incident-record", "--run", run_id, "--fingerprint", token, "--resolution", "rotated",
                            "--evidence", str(self.write_evidence(token)))
            self.assertNotEqual(0, proc.returncode, tamper)
            self.assertFalse((self.state_dir() / "incidents.json").exists())

    def test_manual_public_incident_cannot_borrow_resolved_scanner_identity(self):
        self.publish_a_secret()
        private = self.commit("local access configuration", {"docs/private.txt": "synthetic local access setting\n"})
        _, run_id = self.scan("--mode", "full-audit")
        scanned = self.public_incident(run_id)
        resolved = self.cli("incident-record", "--run", run_id, "--fingerprint", scanned["fingerprint"],
                            "--resolution", "rotated", "--evidence", str(self.write_evidence(scanned["fingerprint"])))
        self.assertEqual(0, resolved.returncode, resolved.stderr)
        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        unit = next(u for u in manifest["units"] if private in (u.get("detail") or {}).get("commits", []))
        claimed = dict(scanned, commit=private, path="docs/private.txt")
        proc = self.record_manual_public(run_id, unit, [claimed])
        self.assertNotEqual(0, proc.returncode)
        self.assertIn("different source provenance", proc.stderr)
        scanner_unit = next(u for u in manifest["units"] if (u.get("detail") or {}).get("fingerprint") == scanned["fingerprint"])
        for override in ({"line": (scanned.get("line") or 1) + 1},
                         {"column": (scanned.get("column") or 1) + 1}, {"object": "0" * 40}):
            proc = self.record_manual_public(run_id, scanner_unit, [dict(scanned, **override)])
            self.assertNotEqual(0, proc.returncode, override)
            self.assertIn("scanner-linked", proc.stderr)

    def test_a_recorded_incident_unblocks_its_own_review_verdict(self):
        self.check_recorded_incident_unblocks_review("rotated")

    def test_accepted_risk_unblocks_audit_and_guarded_push(self):
        self.check_recorded_incident_unblocks_review("accepted-risk")

    def check_recorded_incident_unblocks_review(self, resolution):
        # The run id is a pure function of the boundary and verdicts are
        # immutable, so a public incident must be clearable without inventing
        # a new run.
        self.publish_a_secret()
        if resolution == "accepted-risk":
            DotfilesPublishInstallTests.add_helpers(self)
            installed = self.cli(
                "install", env=self.env(DOTFILES_PUBLISH_EXPECTED_REMOTE=str(self.remote))
            )
            self.assertEqual(0, installed.returncode, installed.stdout + installed.stderr)
        _, run_id = self.scan("--mode", "full-audit")
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        incidents = [
            finding
            for finding in findings
            if finding.get("classification") == "public-incident"
        ]
        self.assertTrue(incidents)
        exposed = {finding["fingerprint"] for finding in incidents}

        manifest = self.read_json(self.run_dir(run_id) / "manifest.json")
        blocking = {
            unit["unit_id"]: (unit.get("detail") or {}).get("fingerprint")
            for unit in manifest["units"]
            if unit["kind"] == "deterministic-finding"
            and (unit.get("detail") or {}).get("fingerprint") in exposed
        }
        self.assertEqual(len(exposed), len(blocking))

        for unit_id in self.unit_ids(run_id):
            if unit_id in blocking:
                evidence = self.base / ("unit-%s.json" % unit_id[:8])
                evidence.write_text(
                    json.dumps(
                        [
                            {
                                "rule_id": "review-public-credential",
                                "fingerprint": blocking[unit_id],
                                "context": "already published, needs rotation",
                            }
                        ]
                    )
                )
                arguments = [
                    "review-record", "--run", run_id, "--unit", unit_id,
                    "--verdict", "finding", "--findings", str(evidence),
                ]
            else:
                arguments = [
                    "review-record", "--run", run_id, "--unit", unit_id, "--verdict", "clean"
                ]
            self.assertEqual(0, self.cli(*arguments).returncode)

        before = self.cli("review-status", "--run", run_id)
        self.assertNotEqual(0, before.returncode)
        self.assertFalse((self.state_dir() / "full-audit.json").exists())

        for index, fingerprint in enumerate(sorted(exposed)):
            recorded = self.cli(
                "incident-record",
                "--run",
                run_id,
                "--fingerprint",
                fingerprint,
                "--resolution",
                resolution,
                "--evidence",
                str(self.write_evidence(
                    fingerprint,
                    verification="Provider validity is unverified; owner accepts continued exposure."
                    if resolution == "accepted-risk" else "Old credential is rejected.",
                    rationale="Low-value historical credential with no important access.",
                    authorization="Owner explicitly requested this exact exception.",
                )),
            )
            self.assertEqual(0, recorded.returncode, recorded.stderr)
            if index == 0 and len(exposed) > 1:
                partial = self.cli("review-status", "--run", run_id)
                self.assertNotEqual(0, partial.returncode)
                self.assertNotIn("deterministic-finding: %s" % fingerprint, partial.stdout)
                for other in exposed - {fingerprint}:
                    self.assertIn("deterministic-finding: %s" % other, partial.stdout)

        after = self.cli("review-status", "--run", run_id)
        self.assertNotEqual(0, after.returncode)
        self.assertIn("clean: yes", after.stdout)
        self.assertIn("incident state changed", after.stderr)
        self.assertFalse((self.state_dir() / "full-audit.json").exists())

        _, rescanned_run = self.scan("--mode", "full-audit")
        self.assertNotEqual(run_id, rescanned_run)
        self.review_all(rescanned_run)
        completed = self.cli("review-status", "--run", rescanned_run)
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertIn("full-audit-recorded: ", completed.stdout)

        if resolution == "accepted-risk":
            self.commit(
                "remove obsolete credential and continue ordinary work",
                {"docs/follow-up.md": "ordinary change\n"},
                remove=("config/service.conf",),
            )
            scanned, publish_run = self.scan()
            self.assertEqual(0, scanned.returncode, scanned.stdout + scanned.stderr)
            self.assertIn("full-audit-due: false", scanned.stdout)
            self.review_all(publish_run)
            pushed = self.cli("push", "--run", publish_run)
            self.assertEqual(0, pushed.returncode, pushed.stdout + pushed.stderr)
            self.assertIn("push authorized for run %s" % publish_run, pushed.stdout)
            self.assertEqual(self.head(), self.remote_tip())

    def test_accepted_risk_requires_explicit_rationale_and_authorization(self):
        self.publish_a_secret()
        _, run_id = self.scan("--mode", "full-audit")
        finding = self.public_incident(run_id)
        valid = {
            "verification": "Continued validity is unverified.",
            "rationale": "Low-value public credential.",
            "authorization": "Owner explicitly accepts this exposure.",
        }
        for field in ("rationale", "authorization"):
            for value in (None, "", "   ", True, 42, ["approved"]):
                with self.subTest(field=field, value=value):
                    evidence = dict(valid)
                    if value is None:
                        evidence.pop(field)
                    else:
                        evidence[field] = value
                    refused = self.cli(
                        "incident-record", "--run", run_id,
                        "--fingerprint", finding["fingerprint"],
                        "--resolution", "accepted-risk", "--evidence",
                        str(self.write_evidence(finding["fingerprint"], **evidence)),
                    )
                    self.assertNotEqual(0, refused.returncode)
                    self.assertIn(field, refused.stderr)
                    self.assertFalse((self.state_dir() / "incidents.json").exists())
        evidence_path = self.write_evidence(finding["fingerprint"], **valid)
        accepted = self.cli(
            "incident-record", "--run", run_id,
            "--fingerprint", finding["fingerprint"],
            "--resolution", "accepted-risk", "--evidence", str(evidence_path),
        )
        self.assertEqual(0, accepted.returncode, accepted.stdout + accepted.stderr)
        entry = self.read_json(self.state_dir() / "incidents.json")["incidents"][finding["fingerprint"]]
        self.assertEqual("accepted-risk", entry["resolution"])
        self.assertEqual(self.read_json(evidence_path), entry["evidence"])
        self.assertIn("evidence_sha256", entry)

    def test_unknown_incident_resolution_does_not_clear_a_finding(self):
        self.publish_a_secret()
        _, run_id = self.scan("--mode", "full-audit")
        finding = self.public_incident(run_id)
        path = self.state_dir() / "incidents.json"
        path.write_text(json.dumps({
            "schema": 1,
            "incidents": {finding["fingerprint"]: {"resolution": "pending"}},
        }))
        path.chmod(0o600)
        status = self.cli("review-status", "--run", run_id)
        self.assertNotEqual(0, status.returncode)
        self.assertIn("deterministic-finding: %s" % finding["fingerprint"], status.stdout)

    def test_incident_record_rejects_broad_or_value_bearing_evidence(self):
        self.publish_a_secret()
        _, run_id = self.scan("--mode", "full-audit")
        finding = self.public_incident(run_id)

        with_value = self.write_evidence(finding["fingerprint"], value="anything")
        rejected = self.cli(
            "incident-record",
            "--run",
            run_id,
            "--fingerprint",
            finding["fingerprint"],
            "--resolution",
            "rotated",
            "--evidence",
            str(with_value),
        )
        self.assertNotEqual(0, rejected.returncode)
        self.assertIn("value field", rejected.stderr)

        broad = self.write_evidence(finding["fingerprint"], path="*")
        wide = self.cli(
            "incident-record",
            "--run",
            run_id,
            "--fingerprint",
            finding["fingerprint"],
            "--resolution",
            "rotated",
            "--evidence",
            str(broad),
        )
        self.assertNotEqual(0, wide.returncode)
        self.assertIn("forbidden", wide.stderr)

        leaking = self.write_evidence(
            finding["fingerprint"], verification="the value %s was rotated" % TEST_SECRET
        )
        detected = self.cli(
            "incident-record",
            "--run",
            run_id,
            "--fingerprint",
            finding["fingerprint"],
            "--resolution",
            "rotated",
            "--evidence",
            str(leaking),
        )
        self.assertNotEqual(0, detected.returncode)
        self.assertIn("scanner detected", detected.stderr)

        self.assertFalse((self.state_dir() / "incidents.json").exists())

    def test_incident_record_rejects_punctuation_and_multiline_scanner_values(self):
        reported = "a!β@c#d$e%f^g&h*i\nsecond!line"
        self.publish_a_secret()
        _, run_id = self.scan(
            "--mode",
            "full-audit",
            env=self.env(DOTFILES_FAKE_GITLEAKS_REPORTED_SECRET=reported),
        )
        finding = self.public_incident(run_id)
        evidence = self.write_evidence(
            finding["fingerprint"],
            verification="rotation probe rejected %s as expected" % reported,
        )

        rejected = self.cli(
            "incident-record",
            "--run",
            run_id,
            "--fingerprint",
            finding["fingerprint"],
            "--resolution",
            "rotated",
            "--evidence",
            str(evidence),
        )

        self.assertEqual(1, rejected.returncode, rejected.stdout + rejected.stderr)
        self.assertIn("scanner detected", rejected.stderr)
        self.assertFalse((self.state_dir() / "incidents.json").exists())

    def test_incident_record_refuses_an_unpublished_finding(self):
        self.publish_base()
        self.commit(
            "unpublished configuration", {"config/local.conf": "token = %s\n" % TEST_SECRET}
        )
        _, run_id = self.scan("--mode", "full-audit")
        findings = self.read_json(self.run_dir(run_id) / "findings.json")["findings"]
        unpublished = [
            finding
            for finding in findings
            if finding.get("classification") == "unpublished" and finding["commit"]
        ]
        self.assertTrue(unpublished)

        refused = self.cli(
            "incident-record",
            "--run",
            run_id,
            "--fingerprint",
            unpublished[0]["fingerprint"],
            "--resolution",
            "rotated",
            "--evidence",
            str(self.write_evidence(unpublished[0]["fingerprint"])),
        )

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("remove it from history", refused.stderr)

    def test_receipt_is_written_only_after_a_clean_audit_review(self):
        self.publish_base()
        self.commit("ordinary work", {"docs/notes.md": "notes\n"})

        proc, run_id = self.scan("--mode", "full-audit")
        self.assertEqual(0, proc.returncode, proc.stdout + proc.stderr)
        receipt_path = self.state_dir() / "full-audit.json"

        incomplete = self.cli("review-status", "--run", run_id)
        self.assertNotEqual(0, incomplete.returncode)
        self.assertFalse(receipt_path.exists())

        self.review_all(run_id)
        complete = self.cli("review-status", "--run", run_id)
        self.assertEqual(0, complete.returncode, complete.stderr)
        self.assertIn("full-audit-recorded: ", complete.stdout)
        receipt = self.read_json(receipt_path)
        self.assertFalse(receipt["incident_flag"])

        advanced, publish_run = self.scan()
        self.assertIn("full-audit-due: false", advanced.stdout)

    def test_review_status_cannot_refresh_an_old_audit_without_rescanning(self):
        self.publish_base()
        self.commit("ordinary work", {"docs/notes.md": "notes\n"})
        _, run_id = self.scan("--mode", "full-audit")
        self.review_all(run_id)
        self.assertEqual(0, self.cli("review-status", "--run", run_id).returncode)
        receipt_path = self.state_dir() / "full-audit.json"
        receipt = receipt_path.read_bytes()
        scanner_calls = self.scanner_log.read_bytes()

        self.now += 10 * DAY
        unchanged = self.cli("review-status", "--run", run_id)
        self.assertEqual(0, unchanged.returncode, unchanged.stdout + unchanged.stderr)
        self.assertEqual(receipt, receipt_path.read_bytes())
        self.now += 21 * DAY
        expired = self.cli("review-status", "--run", run_id)
        self.assertNotEqual(0, expired.returncode)
        self.assertIn("recent scan time", expired.stderr)
        self.assertEqual(receipt, receipt_path.read_bytes())
        self.assertEqual(scanner_calls, self.scanner_log.read_bytes())
        overdue, _ = self.scan()
        self.assertIn("full-audit-due: true", overdue.stdout)

        _, rescanned_run = self.scan("--mode", "full-audit")
        self.assertNotEqual(run_id, rescanned_run)
        self.review_all(rescanned_run)
        complete = self.cli("review-status", "--run", rescanned_run)
        self.assertEqual(0, complete.returncode, complete.stdout + complete.stderr)
        self.assertEqual(self.now, self.read_json(receipt_path)["completed_epoch"])

    def test_same_boundary_rescan_invalidates_receipt_until_current_status(self):
        self.publish_base()
        self.commit("ordinary work", {"docs/notes.md": "notes\n"})
        _, audit_id = self.scan("--mode", "full-audit")
        self.review_all(audit_id)
        self.assertEqual(0, self.cli("review-status", "--run", audit_id).returncode)
        receipt_path = self.state_dir() / "full-audit.json"
        receipt = self.read_json(receipt_path)
        _, publish_id = self.scan()
        self.review_all(publish_id)

        rescanned, repeated_id = self.scan("--mode", "full-audit")

        self.assertEqual(0, rescanned.returncode, rescanned.stdout + rescanned.stderr)
        self.assertNotEqual(audit_id, repeated_id)
        self.assertIn("full-audit-due: true", rescanned.stdout)
        self.assertIn("pending review", rescanned.stdout)
        current = self.read_json(self.run_dir(repeated_id) / "run.json")
        self.assertTrue(current["full_audit_due"])
        self.assertEqual(receipt["completed_epoch"], current["scanned_epoch"])
        self.assertEqual(receipt["manifest_id"], current["manifest_id"])
        self.assertNotEqual(receipt["scan_id"], current["scan_id"])
        refused = self.cli("authorize", "--run", publish_id)
        self.assertNotEqual(0, refused.returncode)
        self.assertIn("pending review", refused.stderr)
        self.assertFalse((self.state_dir() / "authorization.json").exists())

        old_review = self.cli("review-status", "--run", audit_id)
        self.assertNotEqual(0, old_review.returncode)
        self.assertIn("newer full-audit", old_review.stderr)
        self.assertEqual(receipt, self.read_json(receipt_path))
        self.assertNotEqual(0, self.cli("review-status", "--run", repeated_id).returncode)
        self.review_all(repeated_id)
        reviewed = self.cli("review-status", "--run", repeated_id)
        self.assertEqual(0, reviewed.returncode, reviewed.stdout + reviewed.stderr)
        self.assertEqual(current["scan_id"], self.read_json(receipt_path)["scan_id"])
        authorized = self.cli("authorize", "--run", publish_id)
        self.assertEqual(0, authorized.returncode, authorized.stdout + authorized.stderr)

    def test_failed_new_audit_still_blocks_old_receipt(self):
        self.publish_base()
        self.commit("ordinary work", {"docs/notes.md": "notes\n"})
        _, audit_id = self.scan("--mode", "full-audit")
        self.review_all(audit_id)
        self.assertEqual(0, self.cli("review-status", "--run", audit_id).returncode)
        receipt_path = self.state_dir() / "full-audit.json"
        original = receipt_path.read_bytes()
        _, publish_id = self.scan()
        self.review_all(publish_id)
        failed = self.cli("scan", "--mode", "full-audit", env=self.env(DOTFILES_PUBLISH_GITLEAKS=str(self.base / "missing-scanner")))
        self.assertNotEqual(0, failed.returncode)
        self.assertEqual(original, receipt_path.read_bytes())
        refused = self.cli("authorize", "--run", publish_id)
        self.assertNotEqual(0, refused.returncode)
        self.assertIn("pending review", refused.stderr)
        old = self.cli("review-status", "--run", audit_id)
        self.assertNotEqual(0, old.returncode)
        self.assertEqual(original, receipt_path.read_bytes())

    def test_review_status_cannot_clear_later_incident_invalidation(self):
        self.publish_a_secret()
        _, run_id = self.scan("--mode", "full-audit")
        self.review_all(run_id)
        self.assertEqual(0, self.cli("review-status", "--run", run_id).returncode)
        finding = self.public_incident(run_id)
        recorded = self.cli(
            "incident-record", "--run", run_id,
            "--fingerprint", finding["fingerprint"], "--resolution", "revoked",
            "--evidence", str(self.write_evidence(finding["fingerprint"])),
        )
        self.assertEqual(0, recorded.returncode, recorded.stderr)
        receipt_path = self.state_dir() / "full-audit.json"
        invalidated = receipt_path.read_bytes()

        refused = self.cli("review-status", "--run", run_id)

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("incident state changed", refused.stderr)
        self.assertEqual(invalidated, receipt_path.read_bytes())
        self.assertTrue(self.read_json(receipt_path)["incident_flag"])

        # Even a stale concurrent receipt write that clears the flag cannot
        # erase the independently checked change in incident state.
        stale_receipt = self.read_json(receipt_path)
        stale_receipt["incident_flag"] = False
        receipt_path.write_text(json.dumps(stale_receipt))
        due, _ = self.scan()
        self.assertIn("full-audit-due: true", due.stdout)
        self.assertIn("incident state changed", due.stdout)

    def test_review_status_cannot_issue_receipt_after_ruleset_changes(self):
        self.publish_base()
        self.commit("ordinary work", {"docs/notes.md": "notes\n"})
        _, run_id = self.scan("--mode", "full-audit")
        self.review_all(run_id)
        (self.repo / ".gitignore").write_text("temporary-output/\n")

        refused = self.cli("review-status", "--run", run_id)

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("ruleset changed", refused.stderr)
        self.assertFalse((self.state_dir() / "full-audit.json").exists())

    def test_full_audit_refuses_advertised_tips_excluded_by_fetch_refspec(self):
        self.publish_base()
        self.git(
            "config", "remote.origin.fetch",
            "+refs/heads/master:refs/remotes/origin/master",
        )
        self.advance_remote_elsewhere("other public branch")
        clone = sorted(self.base.glob("clone-*"))[-1]
        other_tip = self.git("rev-parse", "HEAD", cwd=clone).stdout.strip()
        self.git(
            "-c", "core.hooksPath=/dev/null", "push", "--quiet",
            str(self.remote), "HEAD:refs/heads/other", cwd=clone,
        )
        base_tip = self.head()
        self.git("update-ref", "refs/heads/master", base_tip, cwd=self.remote)
        self.assertNotEqual(
            0, self.git("cat-file", "-e", other_tip, check=False).returncode
        )

        refused = self.cli("scan", "--mode", "full-audit")

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("advertised public tip objects are missing", refused.stderr)
        self.assertNotIn("run: ", refused.stdout)
        self.assertFalse((self.state_dir() / "full-audit.json").exists())
        self.assertEqual(
            "", self.git("for-each-ref", "--format=%(refname)", "refs/dotfiles-publish/audit/").stdout
        )

        self.git(
            "fetch", "--quiet", "--no-write-fetch-head", "--no-tags", "--refmap=",
            str(self.remote), "refs/heads/other",
        )
        self.assertEqual(0, self.git("cat-file", "-e", other_tip).returncode)
        self.assertEqual("", self.git("for-each-ref", "--contains", other_tip).stdout)

        unreferenced = self.cli("scan", "--mode", "full-audit")

        self.assertNotEqual(0, unreferenced.returncode)
        self.assertIn("public commits are outside the audited ref surface", unreferenced.stderr)
        self.assertNotIn("run: ", unreferenced.stdout)
        self.assertFalse((self.state_dir() / "full-audit.json").exists())

    def test_full_audit_refuses_partial_public_history_from_failed_rev_list(self):
        base = self.publish_base()
        self.commit("ordinary work", {"docs/notes.md": "notes\n"})
        wrapper = self.tools / "git"
        wrapper.write_text(
            "#!%s\nimport os, sys\n" % sys.executable
            + "args = sys.argv[1:]\n"
            + "if 'rev-list' in args and args[args.index('rev-list') + 1:] == [%r]:\n" % base
            + "    print(%r)\n    sys.exit(9)\n" % base
            + "os.execv(%r, [%r, *args])\n" % (GIT, GIT)
        )
        wrapper.chmod(0o700)

        refused = self.cli("scan", "--mode", "full-audit")

        self.assertEqual(3, refused.returncode, refused.stdout + refused.stderr)
        self.assertIn("rev-list", refused.stderr)
        self.assertIn("failed (9)", refused.stderr)
        self.assertNotIn("run: ", refused.stdout)
        self.assertFalse((self.state_dir() / "full-audit.json").exists())

    def test_an_incident_receipt_invalidates_the_full_audit(self):
        self.publish_a_secret()
        _, run_id = self.scan("--mode", "full-audit")
        finding = self.public_incident(run_id)
        self.write_full_audit_receipt(run_id)

        self.cli(
            "incident-record",
            "--run",
            run_id,
            "--fingerprint",
            finding["fingerprint"],
            "--resolution",
            "revoked",
            "--evidence",
            str(self.write_evidence(finding["fingerprint"])),
        )

        after, _ = self.scan()
        self.assertIn("full-audit-due: true", after.stdout)
        self.assertIn("incident", after.stdout)

    def test_a_reviewed_branch_advance_keeps_the_receipt_valid(self):
        self.publish_base()
        self.commit("first change", {"docs/first.md": "first\n"})
        _, audit_run = self.scan("--mode", "full-audit")
        self.review_all(audit_run)
        self.assertEqual(0, self.cli("review-status", "--run", audit_run).returncode)

        self.commit("second change", {"docs/second.md": "second\n"})
        advanced, publish_run = self.scan()

        self.assertIn("full-audit-due: false", advanced.stdout)
        self.review_all(publish_run)
        authorized = self.cli("authorize", "--run", publish_run)
        self.assertEqual(0, authorized.returncode, authorized.stdout + authorized.stderr)

    def test_full_audit_never_authorizes_a_push(self):
        self.publish_base()
        self.commit("ordinary work", {"docs/notes.md": "notes\n"})
        _, run_id = self.scan("--mode", "full-audit")
        self.review_all(run_id)
        self.cli("review-status", "--run", run_id)

        refused = self.cli("authorize", "--run", run_id)

        self.assertNotEqual(0, refused.returncode)
        self.assertIn("never publishes", refused.stderr)
        self.assertFalse((self.state_dir() / "authorization.json").exists())
        self.assertEqual(
            self.read_json(self.run_dir(run_id) / "run.json")["remote_oid"],
            self.remote_tip(),
        )


if __name__ == "__main__":
    unittest.main()
