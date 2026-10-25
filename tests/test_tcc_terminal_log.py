"""Tests for bin/tcc-terminal-log's parsing, filtering, storage and summary."""

import importlib.machinery
import importlib.util
import json
import os
import pathlib
import stat
import tempfile
import unittest
from unittest import mock


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "tcc-terminal-log"
TERMINAL = "/System/Applications/Utilities/Terminal.app/Contents/MacOS/Terminal"
PYTHON = "/opt/homebrew/bin/python3.15"


def load_module():
    loader = importlib.machinery.SourceFileLoader("tcc_terminal_log", str(SCRIPT))
    spec = importlib.util.spec_from_loader("tcc_terminal_log", loader)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def process(identifier, binary, responsible_path=None):
    fields = [f"identifier={identifier}", "pid=101", "auid=501", "euid=501"]
    if responsible_path:
        fields.append(f"responsible_path={responsible_path}")
    fields.append(f"binary_path={binary}")
    return "{TCCDProcess: " + ", ".join(fields) + "}"


def events(msg_id, service, preflight, roles, auth_value=2, auth_reason=4, request=None):
    """The tccd log lines for one request, as `log stream --style ndjson` emits them."""
    attribution = ", ".join(f"{role}={body}" for role, body in roles.items())
    lines = [
        f"AUTHREQ_CTX: msgID={msg_id}, function=<private>, service={service}, "
        f"preflight={'yes' if preflight else 'no'}, query=1, client_dict=(null), daemon_dict=<private>",
        f"AUTHREQ_ATTRIBUTION: msgID={msg_id}, attribution={{{attribution}, }},",
        f"AUTHREQ_SUBJECT: msgID={msg_id}, subject=/x,",
        f"AUTHREQ_RESULT: msgID={msg_id}, authValue={auth_value}, authReason={auth_reason}, "
        "authVersion=1, desired_auth=0, error=(null),",
    ]
    if request is not None:
        lines.insert(0, f"REQUEST_MSG: msgID={msg_id}, msg={request}")
    return [{"timestamp": "2026-10-25 09:00:00.000000+0100", "processID": 448,
             "eventMessage": line} for line in lines]


class ParsingTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()

    def feed_all(self, items):
        joiner = self.mod.Joiner()
        return [r for r in (joiner.feed(e, now=0) for e in items) if r is not None]

    def test_terminal_responsible_preflight_is_recorded_with_metadata_only(self):
        [record] = self.feed_all(events("1.1", "kTCCServiceAccessibility", True, {
            "responsible": process("com.apple.Terminal", TERMINAL, TERMINAL),
            "accessing": process("python.exe", PYTHON),
        }))
        self.assertEqual(record, {
            "time": "2026-10-25 09:00:00.000000+0100", "service": "kTCCServiceAccessibility",
            "preflight": True, "accessing": PYTHON, "responsible": TERMINAL,
            "requesting": None, "target": None, "decision": "allowed", "reason": "system set",
        })

    def test_real_request_and_denial_are_distinguished(self):
        [record] = self.feed_all(events("1.2", "kTCCServiceSystemPolicyAllFiles", False, {
            "responsible": process("com.apple.Terminal", TERMINAL, TERMINAL),
            "accessing": process("trash", "/opt/homebrew/bin/trash"),
        }, auth_value=0, auth_reason=5))
        self.assertFalse(record["preflight"])
        self.assertEqual((record["decision"], record["reason"]), ("denied", "service policy"))

    def test_screen_capture_preflight_without_accessing_names_the_requester(self):
        [record] = self.feed_all(events("1.3", "kTCCServiceScreenCapture", True, {
            "responsible": process("com.apple.Terminal", TERMINAL, TERMINAL),
            "requesting": process("python.exe", PYTHON),
        }))
        self.assertEqual(record["accessing"], PYTHON)

    def test_terminal_itself_without_responsible_role_is_recorded(self):
        [record] = self.feed_all(events("1.4", "kTCCServiceDeveloperTool", True, {
            "accessing": process("com.apple.Terminal", TERMINAL),
            "requesting": process("com.apple.syspolicyd", "/usr/libexec/syspolicyd"),
        }))
        self.assertEqual(record["requesting"], "/usr/libexec/syspolicyd")

    def test_terminal_only_requesting_about_another_app_is_not_recorded(self):
        app = "/Applications/Other.app/Contents/MacOS/Other"
        self.assertEqual(self.feed_all(events("1.5", "kTCCServiceAccessibility", True, {
            "accessing": process("com.example.other", app),
            "requesting": process("com.apple.Terminal", TERMINAL),
        })), [])

    def test_other_responsible_apps_and_unlisted_services_are_not_recorded(self):
        app = "/Applications/Other App.app/Contents/MacOS/Other App"
        other = events("1.6", "kTCCServiceAccessibility", True,
                       {"responsible": process("com.example.other", app, app)})
        unlisted = events("1.7", "kTCCServiceMicrophone", False, {
            "responsible": process("com.apple.Terminal", TERMINAL, TERMINAL)})
        self.assertEqual(self.feed_all(other + unlisted), [])

    def test_apple_events_target_is_taken_from_the_request(self):
        [record] = self.feed_all(events("1.8", "kTCCServiceAppleEvents", False, {
            "responsible": process("com.apple.Terminal", TERMINAL, TERMINAL),
            "accessing": process("com.apple.osascript", "/usr/bin/osascript"),
        }, request='{\n\tservice="kTCCServiceAppleEvents"\n'
                    '\tindirect_object_identifier="com.apple.finder"\n}'))
        self.assertEqual(record["target"], "com.apple.finder")

    def test_interleaved_requests_from_two_tccd_processes_are_kept_apart(self):
        first = events("9.1", "kTCCServiceAccessibility", True, {
            "responsible": process("com.apple.Terminal", TERMINAL, TERMINAL)})
        second = [e | {"processID": 766} for e in events("9.1", "kTCCServiceScreenCapture", True, {
            "responsible": process("com.example.x", "/x", "/x")})]
        mixed = [item for pair in zip(first, second) for item in pair]
        records = self.feed_all(mixed)
        self.assertEqual([r["service"] for r in records], ["kTCCServiceAccessibility"])

    def test_role_paths_with_spaces_and_commas_parse(self):
        roles = self.mod.parse_attribution(
            "AUTHREQ_ATTRIBUTION: msgID=1.1, attribution={responsible="
            + process("x", "/Applications/A, B.app/x", "/Applications/A, B.app/x") + ", }")
        self.assertEqual(roles["responsible"]["binary"], "/Applications/A, B.app/x")

    def test_stale_partial_requests_expire(self):
        joiner = self.mod.Joiner(ttl=10)
        joiner.feed(events("2.1", "kTCCServiceAccessibility", True, {})[0], now=0)
        joiner.feed(events("2.2", "kTCCServiceAccessibility", True, {})[0], now=11)
        self.assertEqual(len(joiner.pending), 1)


class StoreAndSummaryTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()
        self.directory = tempfile.TemporaryDirectory()
        self.base = pathlib.Path(self.directory.name)

    def tearDown(self):
        self.directory.cleanup()

    def record(self, **changes):
        return {"time": "t1", "service": "kTCCServiceSystemPolicyAllFiles", "preflight": False,
                "accessing": "/opt/homebrew/bin/trash", "responsible": TERMINAL,
                "requesting": "/usr/libexec/sandboxd", "target": None,
                "decision": "allowed", "reason": "user set"} | changes

    def test_store_is_private_and_bounded_but_counts_stay_complete(self):
        store = self.mod.Store(self.base / "state", max_bytes=1)
        store.add(self.record())
        store.add(self.record(time="t2"))
        store.flush()
        self.assertEqual(stat.S_IMODE(os.stat(self.base / "state").st_mode), 0o700)
        self.assertEqual(stat.S_IMODE(os.stat(store.records).st_mode), 0o600)
        self.assertEqual(len(store.records.read_text().splitlines()), 1)
        self.assertTrue(store.state()["records_truncated_at"])
        [entry] = json.loads(store.counts_file.read_text()).values()
        self.assertEqual((entry["count"], entry["first"], entry["last"]), (2, "t1", "t2"))

    def test_summary_groups_per_service_and_separates_preflight(self):
        store = self.mod.Store(self.base / "state")
        store.add(self.record())
        store.add(self.record(preflight=True, decision="allowed"))
        store.add(self.record(service="kTCCServiceAppleEvents", target="com.apple.finder",
                              accessing="/usr/bin/osascript"))
        text = self.mod.summarize(store.counts, {"started": "s", "ends": "e",
                                                 "events": ["x started", "y stopped by signal 15"]})
        self.assertIn("## Full Disk Access (`kTCCServiceSystemPolicyAllFiles`)", text)
        self.assertIn("2 decisions: 1 preflight, 1 real requests; 2 allowed.", text)
        self.assertIn("`com.apple.finder`", text)
        self.assertIn("y stopped by signal 15", text)
        self.assertIn("Services with no recorded decision:", text)
        self.assertIn("Screen Recording", text.split("Services with no recorded decision:")[1])
        self.assertIn("not shown to be unused", text)

    def test_finish_writes_summary_alerts_and_marks_done(self):
        store = self.mod.Store(self.base / "state")
        store.update_state(started="s", ends="e")
        store.add(self.record())
        args = mock.Mock(name="args", summary_dir=self.base, notes_git_dir=None,
                         notes_root=None, plan="/plan.md")
        args.name = "tcc-terminal-log"
        with mock.patch.object(self.mod, "send_alert", return_value=None) as alert:
            self.mod.finish(args, store)
        [summary] = self.base.glob("tcc-terminal-log-*.md")
        self.assertIn("Full Disk Access", summary.read_text())
        text = alert.call_args.args[1]
        self.assertIn(str(summary), text)
        self.assertIn("/plan.md", text)
        self.assertTrue(store.state()["done"])

    def test_finish_raises_when_the_completion_alert_fails(self):
        store = self.mod.Store(self.base / "state")
        args = mock.Mock(summary_dir=self.base, notes_git_dir=None, notes_root=None, plan=None)
        with mock.patch.object(self.mod, "send_alert", return_value="502"):
            with self.assertRaises(RuntimeError):
                self.mod.finish(args, store)

    def test_finished_run_exits_without_streaming(self):
        store = self.mod.Store(self.base / "state")
        store.update_state(done=True)
        with mock.patch.object(self.mod.subprocess, "Popen") as popen:
            code = self.mod.main(["run", "--state-dir", str(self.base / "state")])
        self.assertEqual(code, 0)
        popen.assert_not_called()


if __name__ == "__main__":
    unittest.main()
