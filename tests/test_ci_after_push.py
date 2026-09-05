"""Exercise the public CLI with owned fake gh/git commands; no network or pushes."""
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
HELPER = ROOT / "bin/ci-after-push"
SHA = "a" * 40
REPO = "fixture-owner/fixture-repo"

FAKE_COMMAND = r'''
import json, os, sys, time
from pathlib import Path
root = Path(os.environ["CI_FIXTURE_DIR"])
args = sys.argv[1:]
command = Path(sys.argv[0]).name
with (root / "calls.jsonl").open("a") as stream:
    stream.write(json.dumps([command, *args]) + "\n")
config = json.loads((root / "config.json").read_text())
if config.get("delay"):
    time.sleep(config["delay"])
if command == "git":
    if args == ["rev-parse", "--show-toplevel"]:
        print(root)
    elif args == ["rev-parse", "HEAD"]:
        print("a" * 40)
    elif args == ["rev-parse", "--abbrev-ref", "HEAD"]:
        print("main")
    elif args != ["push"]:
        sys.exit(8)
    sys.exit(0)
if args[:2] == ["repo", "view"]:
    print("fixture-owner/fixture-repo")
elif args[:2] == ["run", "list"]:
    if config.get("list_fail"):
        print("UNTRUSTED_PRIVATE_ERROR", file=sys.stderr)
        sys.exit(7)
    counter = root / "list-count"
    count = int(counter.read_text()) if counter.exists() else 0
    counter.write_text(str(count + 1))
    snapshots = config["snapshots"]
    value = snapshots[min(count, len(snapshots) - 1)]
    if isinstance(value, dict) and "raw" in value:
        print(value["raw"])
    else:
        print(json.dumps(value))
elif args[:2] == ["run", "view"]:
    if "--log-failed" in args:
        if config.get("log_fail"):
            print("UNTRUSTED_PRIVATE_ERROR", file=sys.stderr)
            sys.exit(7)
        print("Synthetic failed-step log")
    elif config.get("view_fail"):
        print("UNTRUSTED_PRIVATE_ERROR", file=sys.stderr)
        sys.exit(7)
    else:
        value = config["details"][args[2]]
        print(value["raw"] if isinstance(value, dict) and "raw" in value else json.dumps(value))
else:
    sys.exit(9)
'''


def run_record(run_id=101, **changes):
    record = {"databaseId": run_id, "attempt": 2, "status": "completed", "conclusion": "success",
              "name": "Checks", "workflowName": "CI", "workflowDatabaseId": 7,
              "event": "push", "headBranch": "main", "headSha": SHA,
              "url": f"https://github.com/{REPO}/actions/runs/{run_id}",
              "createdAt": "2026-09-05T12:00:00Z", "updatedAt": "2026-09-05T12:01:00Z"}
    record.update(changes)
    return record


def run_detail(record, conclusion="success"):
    # gh GetRun appends /attempts/N to the exported run URL for --attempt N.
    return {**record, "url": record["url"] + f"/attempts/{record['attempt']}",
            "jobs": [{"databaseId": record["databaseId"] * 10,
                                "name": "Unit tests", "status": "completed", "conclusion": conclusion,
                                "url": record["url"] + f"/job/{record['databaseId'] * 10}", "steps": []}]}


class CIAfterPushTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="ci-observer-fixture-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        commands = self.root / "bin"
        commands.mkdir()
        for name in ("gh", "git"):
            path = commands / name
            path.write_text(f"#!{sys.executable}\n" + FAKE_COMMAND)
            path.chmod(0o700)
        self.environment = {**os.environ, "PATH": str(commands) + os.pathsep + os.environ["PATH"],
                            "CI_FIXTURE_DIR": str(self.root), "PYTHONDONTWRITEBYTECODE": "1"}
        record = run_record()
        self.config = {"snapshots": [[record]], "details": {"101": run_detail(record)}}

    def invoke(self, *extra, explicit=True):
        (self.root / "config.json").write_text(json.dumps(self.config))
        args = [str(HELPER)]
        if explicit:
            args += ["--repo", REPO, "--commit", SHA, "--branch", "main"]
        return subprocess.run([*args, "--timeout", "0", "--interval", "1", *extra],
                              env=self.environment, cwd=self.root, capture_output=True, text=True, timeout=8)

    def calls(self):
        path = self.root / "calls.jsonl"
        return [json.loads(line) for line in path.read_text().splitlines()] if path.exists() else []

    def test_default_never_pushes_or_queries_unrelated_git_state(self):
        result = self.invoke()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(any(call[0] == "git" for call in self.calls()))
        receipt = json.loads(result.stdout)
        self.assertEqual(receipt["status"], "observed_success")
        self.assertEqual(receipt["coverage"], "observed_snapshot_only")
        self.assertIn("delayed", receipt["coverageNote"])
        self.assertEqual(receipt["target"], {"repository": REPO, "commit": SHA,
                                           "branch": "main", "event": "push"})
        self.assertEqual(len(receipt["runs"][0]["jobs"]), 1)
        self.assertEqual(receipt["runs"][0]["url"], run_record()["url"] + "/attempts/2")
        for call in self.calls():
            self.assertEqual(call[call.index("--repo") + 1], "github.com/" + REPO)
            if call[1:3] == ["run", "list"]:
                self.assertEqual(call[call.index("--branch") + 1], "main")
                self.assertEqual(call[call.index("--commit") + 1], SHA)
                self.assertEqual(call[call.index("--event") + 1], "push")
            elif call[1:3] == ["run", "view"]:
                self.assertEqual(call[call.index("--attempt") + 1], "2")

    def test_missing_explicit_target_refuses_without_any_service_call(self):
        result = self.invoke(explicit=False)
        self.assertEqual(result.returncode, 2)
        self.assertEqual(self.calls(), [])

    def test_run_from_other_branch_never_satisfies_selected_branch(self):
        self.config["snapshots"] = [[run_record(headBranch="other")]]
        self.assertEqual(self.invoke("--no-push").returncode, 2)

    def test_limit_saturation_never_claims_green(self):
        self.config["snapshots"] = [[run_record(run_id=n) for n in range(100, 120)]]
        self.assertEqual(self.invoke("--no-push").returncode, 2)

    def test_skipped_or_neutral_run_is_not_executed_success(self):
        for conclusion in ("skipped", "neutral"):
            with self.subTest(conclusion=conclusion):
                self.config["snapshots"] = [[run_record(conclusion=conclusion)]]
                self.config["details"]["101"] = run_detail(self.config["snapshots"][0][0], conclusion)
                self.assertEqual(self.invoke("--no-push").returncode, 2)

    def test_appearing_late_run_prevents_first_subset_success(self):
        first = run_record()
        later = run_record(run_id=102, status="queued", conclusion="")
        self.config["snapshots"] = [[first], [first, later]]
        result = self.invoke("--no-push")
        self.assertNotEqual(result.returncode, 0)
        self.assertGreaterEqual(int((self.root / "list-count").read_text()), 2)
        self.assertIn(102, [run["databaseId"] for run in json.loads(result.stdout)["runs"]])

    def test_job_failure_cannot_be_hidden_by_successful_run_summary(self):
        self.config["details"]["101"] = run_detail(run_record(), "failure")
        result = self.invoke("--no-push")
        self.assertEqual(result.returncode, 1)
        receipt = json.loads(result.stdout)
        self.assertEqual(receipt["runs"][0]["conclusion"], "success")
        self.assertIn("continue-on-error", receipt["reason"])

    def test_failed_log_fetch_is_explicit_and_pinned_to_attempt(self):
        failed = run_record(conclusion="failure")
        self.config.update(snapshots=[[failed]], details={"101": run_detail(failed, "failure")}, log_fail=True)
        result = self.invoke("--no-push")
        self.assertEqual(result.returncode, 1)
        self.assertIn("log", result.stdout.lower())
        self.assertNotIn("UNTRUSTED_PRIVATE_ERROR", result.stdout + result.stderr)
        log_call = next(call for call in self.calls() if "--log-failed" in call)
        self.assertIn("--attempt", log_call)
        self.assertEqual(log_call[log_call.index("--attempt") + 1], "2")
        self.assertEqual(json.loads(result.stdout)["failedLogs"][0]["status"], "unavailable")

    def test_conditional_skipped_and_neutral_jobs_preserve_success_with_caveat(self):
        for conclusion in ("skipped", "neutral"):
            with self.subTest(conclusion=conclusion):
                self.config["details"]["101"] = run_detail(run_record(), conclusion)
                result = self.invoke("--no-push")
                self.assertEqual(result.returncode, 0, result.stdout)
                self.assertEqual(json.loads(result.stdout)["jobCaveats"], [
                    {"runId": 101, "attempt": 2, "jobId": 1010, "conclusion": conclusion}])

    def test_successful_failed_log_fetch_retains_exact_attempt_evidence(self):
        failed = run_record(conclusion="failure")
        self.config.update(snapshots=[[failed]], details={"101": run_detail(failed, "failure")})
        result = self.invoke()
        self.assertEqual(result.returncode, 1)
        self.assertEqual(json.loads(result.stdout)["failedLogs"], [
            {"runId": 101, "attempt": 2, "status": "fetched", "text": "Synthetic failed-step log\n"}])

    def test_every_failed_run_is_inspected_and_logs_fetched(self):
        first, second = run_record(conclusion="failure"), run_record(102, conclusion="timed_out")
        self.config.update(snapshots=[[first, second]], details={
            "101": run_detail(first, "failure"), "102": run_detail(second, "timed_out")})
        result = self.invoke()
        self.assertEqual(result.returncode, 1)
        self.assertEqual([entry["runId"] for entry in json.loads(result.stdout)["failedLogs"]], [101, 102])

    def test_empty_and_pending_runs_timeout_without_green(self):
        for runs in ([], [run_record(status="in_progress", conclusion=None)]):
            with self.subTest(runs=runs):
                self.config["snapshots"] = [runs]
                result = self.invoke()
                self.assertEqual(result.returncode, 124)
                self.assertEqual(json.loads(result.stdout)["status"], "pending_or_changed")

    def test_completed_summary_with_pending_job_does_not_pass(self):
        self.config["details"]["101"]["jobs"][0].update(status="in_progress", conclusion=None)
        self.assertEqual(self.invoke().returncode, 124)

    def test_wait_can_observe_pending_then_completed(self):
        self.config["snapshots"] = [[run_record(status="queued", conclusion="")], [run_record()]]
        result = self.invoke("--timeout", "4")
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertEqual(int((self.root / "list-count").read_text()), 3)

    def test_run_rerun_after_job_read_does_not_accept_old_attempt(self):
        self.config["snapshots"] = [[run_record()], [run_record(attempt=3)]]
        result = self.invoke()
        self.assertEqual(result.returncode, 124)
        self.assertEqual(json.loads(result.stdout)["runs"][0]["attempt"], 3)

    def test_malformed_run_json_is_never_clean_or_echoed(self):
        for raw in ('UNTRUSTED_PRIVATE_ERROR', '{}', '[null]', '[NaN]', '[1e999]',
                    '[{"status":"completed","status":"queued"}]'):
            with self.subTest(raw=raw):
                self.config["snapshots"] = [{"raw": raw}]
                result = self.invoke()
                self.assertEqual(result.returncode, 2)
                self.assertNotIn("UNTRUSTED_PRIVATE_ERROR", result.stdout + result.stderr)
                json.loads(result.stdout)

    def test_invalid_missing_or_duplicate_run_fields_are_incomplete(self):
        records = [run_record(databaseId=True), run_record(attempt=0), run_record(status="unknown"),
                   run_record(conclusion=None), run_record(headSha="b" * 40), run_record(event="pull_request"),
                   run_record(url="https://github.com/other/repository/actions/runs/101")]
        missing = run_record()
        del missing["attempt"]
        for runs in [[record] for record in [*records, missing]] + [[run_record(), run_record()]]:
            with self.subTest(runs=runs):
                self.config["snapshots"] = [runs]
                self.assertEqual(self.invoke().returncode, 2)

    def test_detail_mismatch_or_malformed_response_is_incomplete(self):
        for record in (run_detail(run_record(attempt=3)), run_detail(run_record(headBranch="other")),
                       {"raw": "UNTRUSTED_PRIVATE_ERROR"}):
            with self.subTest(record=record):
                self.config["details"]["101"] = record
                result = self.invoke()
                self.assertEqual(result.returncode, 2)
                self.assertNotIn("UNTRUSTED_PRIVATE_ERROR", result.stdout + result.stderr)

    def test_detail_url_requires_exact_requested_attempt_and_repository(self):
        base = run_record()["url"]
        for url in (base, base + "/attempts/3", base + "/attempts/02", base + "/attempts/2/extra",
                    base + "/attempts/2?other=1", base + "/attempts/2#other",
                    base + "/unrelated/2", base + "/attempts/2/../3",
                    "https://github.com/other/repo/actions/runs/101/attempts/2",
                    "https://github.com/fixture-owner/fixture-repo/actions/runs/102/attempts/2"):
            with self.subTest(url=url):
                self.config["details"]["101"] = {**run_detail(run_record()), "url": url}
                result = self.invoke()
                self.assertEqual(result.returncode, 2, result.stdout)

    def test_list_url_does_not_accept_detail_attempt_suffix(self):
        self.config["snapshots"] = [[run_record(url=run_record()["url"] + "/attempts/2")]]
        self.assertEqual(self.invoke().returncode, 2)

    def test_job_fields_are_required_and_duplicate_jobs_refused(self):
        original = run_detail(run_record())["jobs"][0]
        missing = dict(original)
        del missing["conclusion"]
        missing_pending = {**missing, "status": "queued"}
        for jobs in (None, [], [None], [missing], [missing_pending], [{**original, "databaseId": True}],
                     [{**original, "status": "unknown"}], [original, original]):
            with self.subTest(jobs=jobs):
                self.config["details"]["101"]["jobs"] = jobs
                result = self.invoke()
                self.assertEqual(result.returncode, 2)
                json.loads(result.stdout)

    def test_subprocess_failure_reports_unknown_status_without_raw_stderr(self):
        for field in ("list_fail", "view_fail"):
            with self.subTest(field=field):
                self.config.pop("list_fail", None)
                self.config.pop("view_fail", None)
                self.config[field] = True
                result = self.invoke()
                self.assertEqual(result.returncode, 2)
                self.assertNotIn("UNTRUSTED_PRIVATE_ERROR", result.stdout + result.stderr)
                self.assertIn("7", json.loads(result.stdout)["reason"])

    def test_subprocess_is_bounded_by_total_timeout(self):
        self.config["delay"] = 3
        result = self.invoke("--timeout", "1")
        self.assertEqual(result.returncode, 124)
        self.assertIn("deadline", json.loads(result.stdout)["reason"])

    def test_invalid_limits_and_target_arguments_make_no_requests(self):
        for args in (("--limit", "1001"), ("--limit", "0"), ("--interval", "0"),
                     ("--interval", "61"), ("--timeout", "-1"), ("--commit", "abc"),
                     ("--repo", "https://github.com/owner/repo"), ("--branch", ""),
                     ("--repo", "owner/.."), ("--repo", "owner/.")):
            with self.subTest(args=args):
                self.assertEqual(self.invoke(*args).returncode, 2)
                self.assertEqual(self.calls(), [])

    def test_implicit_github_host_is_not_overridden_by_environment(self):
        self.environment["GH_HOST"] = "unrelated.example"
        result = self.invoke()
        self.assertEqual(result.returncode, 0, result.stdout)
        for call in self.calls():
            self.assertEqual(call[call.index("--repo") + 1], "github.com/" + REPO)

    def test_repository_names_can_start_with_dot_or_underscore(self):
        for name in (".github", "_fixture"):
            with self.subTest(name=name):
                repo = "fixture-owner/" + name
                record = run_record(url=f"https://github.com/{repo}/actions/runs/101")
                self.config.update(snapshots=[[record]], details={"101": run_detail(record)})
                result = self.invoke("--repo", repo)
                self.assertEqual(result.returncode, 0, result.stdout)

    def test_enterprise_repository_and_literal_branch_use_exact_argv(self):
        branch = "--odd-'quoted-$(not-a-command)"
        enterprise = "forge.example/fixture-owner/fixture-repo"
        record = run_record(headBranch=branch, url="https://forge.example/fixture-owner/fixture-repo/actions/runs/101")
        self.config.update(snapshots=[[record]], details={"101": run_detail(record)})
        result = self.invoke("--repo", enterprise, "--branch=" + branch)
        self.assertEqual(result.returncode, 0, result.stdout)
        for call in self.calls():
            self.assertEqual(call[call.index("--repo") + 1], enterprise)
            if call[1:3] == ["run", "list"]:
                self.assertEqual(call[call.index("--branch") + 1], branch)


if __name__ == "__main__":
    unittest.main()
