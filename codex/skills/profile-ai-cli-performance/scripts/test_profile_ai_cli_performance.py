#!/usr/bin/env python3

import importlib.util
import argparse
import contextlib
import io
import json
import os
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from unittest import mock
from pathlib import Path


SCRIPT = Path(__file__).with_name("profile_ai_cli_performance.py")
SPEC = importlib.util.spec_from_file_location("profile_ai_cli_performance", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class BenchmarkTests(unittest.TestCase):
    def condition(self, code):
        return {
            "name": "test",
            "command": [sys.executable, "-c", code],
            "cwd": tempfile.gettempdir(),
            "env": dict(os.environ),
            "client": "codex",
        }

    def test_silent_process_honors_timeout(self):
        started = time.monotonic()
        result = MODULE.execute(self.condition("import time; time.sleep(30)"), 0.2, "OK")
        self.assertEqual(result["status"], "timeout")
        self.assertLess(time.monotonic() - started, 4)
        self.assertIsNone(result["total_s"])

    def test_stderr_is_drained_without_deadlock(self):
        code = "import sys; sys.stderr.write('x' * 200000); print('{\\\"type\\\":\\\"item.completed\\\",\\\"item\\\":{\\\"type\\\":\\\"agent_message\\\",\\\"text\\\":\\\"OK\\\"}}')"
        code += "\n" + self.events({"type": "turn.completed"})
        result = MODULE.execute(self.condition(code), 3, "OK")
        self.assertEqual(result["status"], "success")

    def test_structured_error_is_provider_rejection(self):
        status = MODULE.classify("", ['{"type":"error","message":"unavailable"}'], 1, False)
        self.assertEqual(status, "provider_rejection")

    def test_summary_uses_paired_blocks(self):
        rows = []
        for iteration, clean, configured in ((1, 1.0, 4.0), (2, 10.0, 11.0)):
            for condition, value in (("claude_clean", clean), ("claude_configured", configured)):
                rows.append({
                    "condition": condition,
                    "iteration": iteration,
                    "status": "success",
                    "completed_message_s": value,
                    "total_s": value + 1,
                })
        summary = MODULE.summarize(rows)
        effect = summary["claude_configuration_association"]
        self.assertEqual(effect["paired_blocks"], 2)
        self.assertEqual(effect["paired_completed_message_delta_median_s"], 2)

    def events(self, *events):
        return "\n".join("print(" + repr(json.dumps(event)) + ", flush=True)" for event in events)

    def answer_event(self, text="OK"):
        return {"type": "item.completed", "item": {"type": "agent_message", "text": text}}

    def test_json_nonobject_and_nested_malformed_are_rejected_without_crash(self):
        for event in ([], None, {"type": "item.completed", "item": []},
                      {"type": "assistant", "message": {"content": [None]}}):
            with self.subTest(event=event):
                code = self.events(event, self.answer_event(), {"type": "turn.completed"})
                row = MODULE.execute(self.condition(code), 3, "OK")
                self.assertNotEqual(row["status"], "success")

    def test_provider_error_cannot_be_erased_by_later_expected_answer(self):
        code = self.events({"type": "error", "message": "UNTRUSTED_PRIVATE_TEXT"},
                           self.answer_event(), {"type": "turn.completed"})
        row = MODULE.execute(self.condition(code), 3, "OK")
        self.assertEqual(row["status"], "provider_rejection")
        self.assertNotIn("UNTRUSTED_PRIVATE_TEXT", json.dumps(row))

    def test_result_only_does_not_claim_a_completed_message_metric(self):
        condition = self.condition(self.events(
            {"type": "result", "subtype": "success", "is_error": False, "result": "OK"}))
        condition["client"] = "claude"
        row = MODULE.execute(condition, 3, "OK")
        self.assertNotEqual(row["status"], "success")

    def test_expected_answer_without_terminal_event_is_not_success(self):
        row = MODULE.execute(self.condition(self.events(self.answer_event())), 3, "OK")
        self.assertNotEqual(row["status"], "success")

    def test_eof_before_normal_process_exit_does_not_trigger_termination(self):
        code = "import os, time\n" + self.events(self.answer_event(), {"type": "turn.completed"})
        code += "\nos.close(1)\nos.close(2)\ntime.sleep(0.08)"
        row = MODULE.execute(self.condition(code), 3, "OK")
        self.assertEqual(row["status"], "success")
        self.assertEqual(row["exit_code"], 0)

    def test_completion_time_belongs_to_last_answer_not_first_message(self):
        code = "import time\n" + self.events(self.answer_event("earlier"))
        code += "\ntime.sleep(0.08)\n" + self.events(self.answer_event(), {"type": "turn.completed"})
        row = MODULE.execute(self.condition(code), 3, "OK")
        self.assertEqual(row["status"], "success")
        self.assertGreater(row["completed_message_s"] - row["first_stdout_line_s"], 0.05)

    def test_missing_metrics_and_unpaired_successes_are_explicit_gaps(self):
        rows = [
            {"condition": "claude_clean", "iteration": 1, "status": "success",
             "completed_message_s": None, "total_s": 1.0},
            {"condition": "claude_configured", "iteration": 2, "status": "success",
             "completed_message_s": 2.0, "total_s": 3.0},
        ]
        effect = MODULE.summarize(rows)["claude_configuration_association"]
        self.assertIsNone(effect["median_completed_message_delta_s"])
        self.assertEqual(effect["paired_blocks"], 0)
        self.assertIsNone(effect["paired_total_delta_median_s"])

    def test_excess_output_is_bounded_failure_not_silent_truncation(self):
        for code in ("print('x' * 300000)", "import sys\nsys.stderr.write('x' * 300000)",
                     "for _ in range(30000): print('x' * 100)"):
            with self.subTest(code=code):
                row = MODULE.execute(self.condition(code), 3, "OK")
                self.assertEqual(row["status"], "output_limit")
                self.assertLess(len(json.dumps(row)), 2000)

    def test_partial_mcp_activity_is_an_isolation_contradiction(self):
        for kind in ("item.started", "item.updated", "item.completed"):
            with self.subTest(kind=kind):
                condition = self.condition(self.events(
                    {"type": kind, "item": {"type": "mcp_tool_call"}},
                    self.answer_event(), {"type": "turn.completed"}))
                condition["require_no_tools"] = True
                self.assertEqual(MODULE.execute(condition, 3, "OK")["status"], "isolation_failure")

    def test_claude_original_prompt_terminal_and_partial_tool_contract(self):
        init = {"type": "system", "subtype": "init", "mcp_servers": [], "tools": []}
        answer = {"type": "assistant", "message": {"content": [{"type": "text", "text": "OK"}]}}
        result = {"type": "result", "subtype": "success", "is_error": False,
                  "result": "OK", "origin": {"kind": "human"}, "terminal_reason": "completed"}
        for changes in ({}, {"is_error": True}, {"terminal_reason": "hook_stopped"},
                        {"api_error_status": 429}, {"deferred_tool_use": {}},
                        {"origin": {"kind": "task-notification"}}, {"origin": "main"}):
            with self.subTest(changes=changes):
                condition = self.condition(self.events(init, answer, {**result, **changes}))
                condition.update(client="claude", require_no_tools=True)
                row = MODULE.execute(condition, 3, "OK")
                self.assertEqual(row["status"] == "success", not changes)
        partial = {"type": "stream_event", "event": {"type": "content_block_start",
                   "content_block": {"type": "tool_use", "name": "mcp__fixture__tool"}}}
        condition = self.condition(self.events(init, partial, answer, result))
        condition.update(client="claude", require_no_tools=True)
        self.assertEqual(MODULE.execute(condition, 3, "OK")["status"], "isolation_failure")

    def test_native_claude_availability_fields_keep_specific_failure(self):
        answer = {"type": "assistant", "message": {"content": [{"type": "text", "text": "OK"}]}}
        result = {"type": "result", "subtype": "success", "is_error": False, "result": "OK"}
        for error, expected in (("rate_limit", "rate_limit"), ("authentication_failed", "auth_failure"),
                                ("oauth_org_not_allowed", "auth_failure")):
            with self.subTest(error=error):
                condition = self.condition(self.events({**answer, "error": error}, result))
                condition["client"] = "claude"
                self.assertEqual(MODULE.execute(condition, 3, "OK")["status"], expected)
        condition = self.condition(self.events(answer, {**result, "api_error_status": 429}))
        condition["client"] = "claude"
        self.assertEqual(MODULE.execute(condition, 3, "OK")["status"], "rate_limit")

    def test_cleanup_failure_preserves_attempt_without_raising(self):
        code = self.events(self.answer_event(), {"type": "turn.completed"})
        original = MODULE.terminate_process_group
        def fail_after_cleanup(proc):
            original(proc)
            raise subprocess.TimeoutExpired("owned-fixture", 3)
        with mock.patch.object(MODULE, "terminate_process_group", side_effect=fail_after_cleanup):
            row = MODULE.execute(self.condition(code), 3, "OK")
        self.assertEqual(row["status"], "cleanup_failure")
        self.assertFalse(row["cleanup_complete"])

    def test_termination_reaches_descendant_after_leader_exits(self):
        with tempfile.TemporaryDirectory(prefix="ai-group-fixture-") as name:
            marker = Path(name) / "alive"
            code = ("import os, signal, time\n"
                    "child = os.fork()\n"
                    "if child == 0:\n"
                    " signal.signal(signal.SIGTERM, signal.SIG_IGN)\n"
                    " print('ready', flush=True)\n"
                    " time.sleep(0.5)\n"
                    f" open({str(marker)!r}, 'w').write('survived')\n"
                    " time.sleep(2)\n"
                    "else:\n"
                    " time.sleep(5)\n")
            proc = subprocess.Popen([sys.executable, "-c", code], stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE, text=True, start_new_session=True)
            try:
                self.assertEqual(proc.stdout.readline(), "ready\n")
                MODULE.terminate_process_group(proc)
                # EOF proves the descendant's inherited pipe also closed.
                stdout, stderr = proc.communicate(timeout=0.3)
                self.assertEqual((stdout, stderr), ("", ""))
                self.assertFalse(marker.exists())
            finally:
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                proc.wait(timeout=3)
                proc.stdout.close()
                proc.stderr.close()

    def test_escaped_child_pipe_does_not_block_cleanup_reporting(self):
        with tempfile.TemporaryDirectory(prefix="ai-escaped-fixture-") as name:
            completed = Path(name) / "child-completed"
            code = ("import os, time\n"
                    "child = os.fork()\n"
                    "if child == 0:\n"
                    " os.setsid()\n"
                    " time.sleep(1.5)\n"
                    f" with open({str(completed)!r}, 'w') as stream: stream.write('completed')\n"
                    "else:\n"
                    " time.sleep(5)\n")
            started = time.monotonic()
            row = MODULE.execute(self.condition(code), 0.1, "OK")
            self.assertEqual(row["status"], "cleanup_failure")
            self.assertFalse(row["cleanup_complete"])
            self.assertLess(time.monotonic() - started, 3)
            # The owned escaped child exits naturally before the second reader join.
            self.assertEqual(completed.read_text(), "completed")


FAKE_CLI = r'''
import json, os, sys, time
from pathlib import Path
root = Path(FIXTURE_ROOT)
config = json.loads((root / "control.json").read_text())
client = Path(sys.argv[0]).name
with (root / "calls.jsonl").open("a") as stream:
    stream.write(json.dumps({"client": client, "argv": sys.argv[1:],
                             "key_present": bool(os.environ.get("CODEX_API_KEY")),
                             "key_matches_fixture": os.environ.get("CODEX_API_KEY") == "owned-nonsecret-fixture-value"}) + "\n")
if "--version" in sys.argv:
    if config.get("late_output"):
        Path(config["late_output"]).write_text("concurrent fixture report")
    if config.get("version_hang"):
        time.sleep(5)
    if config.get("version_flood"):
        print("x" * 300000)
        sys.exit(0)
    print("1.0.0")
    sys.exit(0)
counter = root / (client + "-count")
count = int(counter.read_text()) if counter.exists() else 0
counter.write_text(str(count + 1))
dirty = config.get("mcp_after", 1000) <= count
if client == "claude":
    print(json.dumps({"type": "system", "subtype": "init", "mcp_servers": ([{"name":"fixture", "status":"connected"}] if dirty else []), "tools": []}))
    print(json.dumps({"type":"assistant", "message":{"content":[{"type":"text", "text":"OK"}]}}))
    print(json.dumps({"type":"result", "subtype":"success", "is_error":False, "result":"OK", "origin":{"kind":"human"}}))
else:
    print(json.dumps({"type":"item.completed", "item":{"type":"agent_message", "text":"OK"}}))
    print(json.dumps({"type":"turn.completed"}))
if config.get("provider_error") or config.get("provider_error_after", 1000) <= count:
    print(json.dumps({"type":"error", "message":config.get("error_message", "UNTRUSTED_PRIVATE_TEXT")}))
    sys.exit(config.get("error_exit", 0))
'''


class PublicCLITests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="ai-cli-fixture-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.config = {}
        for client in ("claude", "codex"):
            path = self.root / client
            path.write_text(f"#!{sys.executable}\nFIXTURE_ROOT={str(self.root)!r}\n" + FAKE_CLI)
            path.chmod(0o700)
        # No real service credentials or account-selector values are needed.
        self.env = {"PATH": os.environ["PATH"], "PYTHONDONTWRITEBYTECODE": "1"}

    def invoke(self, *extra, selection="claude_clean,claude_configured"):
        (self.root / "control.json").write_text(json.dumps(self.config))
        command = [sys.executable, str(SCRIPT), "--project-dir", str(self.root), "--runs", "1",
                   "--timeout", "1", "--only", selection]
        if "claude" in selection:
            command += ["--claude-bin", str(self.root / "claude"), "--claude-model", "fixture",
                        "--claude-effort", "low"]
        if "codex" in selection:
            command += ["--codex-bin", str(self.root / "codex"), "--codex-model", "fixture",
                        "--codex-effort", "ultra"]
        result = subprocess.run([*command, *extra], env=self.env, capture_output=True,
                                text=True, cwd=self.root, timeout=10)
        report = json.loads(result.stdout.removeprefix("FINAL_JSON="))
        return result, report

    def calls(self):
        path = self.root / "calls.jsonl"
        return [json.loads(line) for line in path.read_text().splitlines()] if path.exists() else []

    def test_claude_only_does_not_require_codex_flags_binary_or_auth(self):
        result, report = self.invoke()
        self.assertEqual(result.returncode, 0, report)
        self.assertEqual(report["metadata"]["conditions"], ["claude_clean", "claude_configured"])
        self.assertEqual(len(report["attempts"]), 2)
        self.assertTrue(all(call["client"] == "claude" for call in self.calls()))

    def test_dry_run_does_not_spawn_or_require_auth(self):
        result, report = self.invoke("--dry-run", selection="codex_clean,codex_configured")
        self.assertEqual(result.returncode, 0, report)
        self.assertEqual(report["status"], "dry_run")
        self.assertFalse(report["auth_route_checked"])
        self.assertEqual(self.calls(), [])

    def test_dry_run_never_reads_auth_bearing_environment_values(self):
        class GuardedEnvironment(dict):
            def get(self, key, default=None):
                if key == "CODEX_API_KEY":
                    raise AssertionError("preview read auth")
                return super().get(key, default)
            def __getitem__(self, key):
                if key == "CODEX_API_KEY":
                    raise AssertionError("preview copied auth")
                return super().__getitem__(key)
            def items(self):
                raise AssertionError("preview enumerated auth environment")
            def keys(self):
                raise AssertionError("preview copied auth environment")
        args = argparse.Namespace(project_dir=self.root, runs=1, timeout=1, seed=1,
                                  only=["codex_clean"], codex_bin=self.root / "codex",
                                  codex_model="fixture", codex_effort="ultra", claude_bin=None,
                                  claude_model=None, claude_effort=None, expected="OK", prompt=None,
                                  output=None, dry_run=True, claude_mcp_ablation=False)
        env = GuardedEnvironment({"HOME": str(self.root), "CODEX_API_KEY": "unread-fixture-value"})
        with mock.patch.object(MODULE, "parse_args", return_value=args), \
             mock.patch.object(MODULE.os, "environ", env), \
             mock.patch.object(MODULE.subprocess, "Popen", side_effect=AssertionError("preview spawned")), \
             contextlib.redirect_stdout(io.StringIO()) as stdout:
            self.assertEqual(MODULE.main(), 0)
        self.assertNotIn("unread-fixture-value", stdout.getvalue())

    def test_empty_selection_and_nonfinite_timeout_refuse_before_processes(self):
        for selection, extra in (("", ()), (",", ()), ("claude_clean", ("--timeout", "nan")),
                                 ("claude_clean", ("--timeout", "inf")),
                                 ("claude_clean", ("--timeout", "0"))):
            with self.subTest(selection=selection, extra=extra):
                result, _report = self.invoke(*extra, selection=selection)
                self.assertEqual(result.returncode, 2)
                self.assertEqual(self.calls(), [])

    def test_existing_or_symlink_output_is_preserved_without_processes(self):
        existing = self.root / "existing.json"
        existing.write_text("owned prior report")
        link = self.root / "linked.json"
        link.symlink_to(existing)
        for target in (existing, link):
            with self.subTest(target=target):
                result, _report = self.invoke("--output", str(target))
                self.assertEqual(result.returncode, 2)
                self.assertEqual(existing.read_text(), "owned prior report")
                self.assertEqual(self.calls(), [])

    def test_concurrent_output_appearance_is_not_clobbered(self):
        output = self.root / "late.json"
        self.config["late_output"] = str(output)
        result, report = self.invoke("--output", str(output))
        self.assertEqual(result.returncode, 2)
        self.assertIn("output_error", report)
        self.assertEqual(output.read_text(), "concurrent fixture report")
        self.assertEqual(list(self.root.glob(".ai-cli-report-*")), [])

    def test_preflight_failure_is_written_privately_without_raw_error_text(self):
        self.config["provider_error"] = True
        output = self.root / "failure.json"
        result, report = self.invoke("--output", str(output))
        self.assertEqual(result.returncode, 2)
        self.assertEqual(json.loads(output.read_text()), report)
        self.assertEqual(output.stat().st_mode & 0o777, 0o600)
        self.assertTrue(report["preflight"])
        self.assertEqual(report["attempts"], [])
        self.assertNotIn("UNTRUSTED_PRIVATE_TEXT", result.stdout + result.stderr)

    def test_failed_first_preflight_does_not_launch_remaining_conditions(self):
        self.config.update(provider_error=True, error_message="quota exceeded", error_exit=3)
        result, report = self.invoke(selection="claude_clean,claude_configured")
        self.assertEqual(result.returncode, 2)
        self.assertEqual(report["status"], "unavailable")
        self.assertEqual(report["metadata"]["conditions"], ["claude_clean", "claude_configured"])
        self.assertEqual([row["status"] for row in report["preflight"]], ["rate_limit"])
        self.assertEqual(report["attempts"], [])
        requests = [call for call in self.calls() if "--version" not in call["argv"]]
        self.assertEqual(len(requests), 1)
        self.assertIn("--safe-mode", requests[0]["argv"])

    def test_measured_isolation_drift_is_not_hidden_by_preflight(self):
        self.config["mcp_after"] = 2
        result, report = self.invoke("--runs", "3", selection="claude_clean")
        self.assertEqual(result.returncode, 2)
        self.assertEqual(report["preflight"][0]["status"], "success")
        self.assertEqual([row["status"] for row in report["attempts"]], ["success", "isolation_failure"])

    def test_measured_structured_quota_error_stops_remaining_calls(self):
        self.config.update(provider_error_after=1, error_message="quota exceeded", error_exit=3)
        result, report = self.invoke("--runs", "3", selection="claude_clean")
        self.assertEqual(result.returncode, 2)
        self.assertEqual(report["preflight"][0]["status"], "success")
        self.assertEqual([row["status"] for row in report["attempts"]], ["rate_limit"])
        self.assertEqual(len([call for call in self.calls() if "--version" not in call["argv"]]), 2)

    def test_codex_clean_requires_explicit_api_mode_without_auth_file_access(self):
        result, report = self.invoke(selection="codex_clean")
        self.assertEqual(result.returncode, 2)
        self.assertIn("CODEX_API_KEY", report["reason"])
        self.assertEqual(self.calls(), [])

    def test_codex_configured_only_does_not_require_clean_auth_mode(self):
        result, report = self.invoke(selection="codex_configured")
        self.assertEqual(result.returncode, 0, report)
        self.assertEqual(report["metadata"]["conditions"], ["codex_configured"])

    def test_conflicting_codex_auth_selector_refuses_before_processes(self):
        self.env.update(CODEX_API_KEY="owned-nonsecret-fixture-value", OPENAI_API_KEY="other-fixture")
        result, report = self.invoke(selection="codex_clean")
        self.assertEqual(result.returncode, 2)
        self.assertEqual(self.calls(), [])
        self.assertNotIn("other-fixture", json.dumps(report))

    def test_existing_api_key_is_only_forwarded_to_codex_exec_children(self):
        self.env["CODEX_API_KEY"] = "owned-nonsecret-fixture-value"
        result, report = self.invoke(selection="claude_clean,claude_configured,codex_clean,codex_configured")
        self.assertEqual(result.returncode, 0, report)
        self.assertNotIn("owned-nonsecret-fixture-value", result.stdout + result.stderr)
        for call in self.calls():
            self.assertEqual(call["key_present"], call["client"] == "codex" and "--version" not in call["argv"])
            self.assertEqual(call["key_matches_fixture"], call["key_present"])
        codex_rows = [row for row in report["attempts"] if row["condition"].startswith("codex_")]
        self.assertTrue(all(row["isolation"]["runtime_evidence"] == "unobserved" for row in codex_rows))

    def test_custom_claude_account_selector_refuses_clean_comparison(self):
        self.env["CLAUDE_CONFIG_DIR"] = str(self.root / "unread-account")
        result, _report = self.invoke()
        self.assertEqual(result.returncode, 2)
        self.assertEqual(self.calls(), [])

    def test_hanging_version_probe_is_bounded_and_output_is_retained(self):
        self.config["version_hang"] = True
        output = self.root / "version-failure.json"
        result, report = self.invoke("--timeout", "0.1", "--output", str(output))
        self.assertEqual(result.returncode, 2)
        self.assertIn("version", report["reason"])
        self.assertEqual(json.loads(output.read_text()), report)

    def test_version_output_is_bounded_and_not_echoed(self):
        self.config["version_flood"] = True
        result, report = self.invoke()
        self.assertEqual(result.returncode, 2)
        self.assertIn("version", report["reason"])
        self.assertLess(len(result.stdout), 4000)

    def test_leading_dash_prompt_remains_literal_after_end_of_options(self):
        result, report = self.invoke("--prompt=--help", selection="claude_configured")
        self.assertEqual(result.returncode, 0, report)
        for call in self.calls():
            if "--version" not in call["argv"]:
                self.assertEqual(call["argv"][-2:], ["--", "--help"])


if __name__ == "__main__":
    unittest.main()
