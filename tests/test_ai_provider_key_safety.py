"""Synthetic public-entry and persistence regressions; never contact a provider/broker."""

import contextlib
import copy
import importlib.machinery
import importlib.util
import io
import http.client
import json
import os
import socket
import subprocess
import sys
import tempfile
import unittest
import urllib.error
from pathlib import Path
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "claude/bin/ai-provider-key"
ITEM_ID = "a" * 26
SECRET = "sk-ant-" + "SYNTHETIC" * 5
OPENAI_SECRET = "sk-" + "SYNTHETIC" * 5
TITLE = "Anthropic - synthetic"
TEMPLATE = {"category": "API_CREDENTIAL", "fields": [{"id": "credential", "type": "CONCEALED", "value": ""}]}
_run = subprocess.run


class ProviderSafetyTest(unittest.TestCase):
    def setUp(self):
        loader = importlib.machinery.SourceFileLoader("provider_safety_test", str(SCRIPT))
        spec = importlib.util.spec_from_loader(loader.name, loader)
        self.m = importlib.util.module_from_spec(spec)
        sys.modules[loader.name] = self.m
        loader.exec_module(self.m)
        self.scratch = Path(tempfile.mkdtemp(prefix="provider-key-test-", dir="/private/tmp"))
        self.addCleanup(self.cleanup)
        self.addCleanup(patch.stopall)
        patch.object(socket, "socket", side_effect=AssertionError("network forbidden")).start()
        patch.object(subprocess, "run", side_effect=AssertionError("external processes must be explicitly faked")).start()
        self.m.run = lambda *a, **k: self.fail("unexpected broker operation")

    def cleanup(self):
        result = _run(["/opt/homebrew/bin/trash", str(self.scratch)], capture_output=True, text=True)
        self.assertEqual(result.returncode, 0)
        self.assertFalse(os.path.lexists(self.scratch))

    def args(self, *more, provider="anthropic"):
        return ["create-automation", "--provider", provider, "--project", "synthetic", *more]

    def invoke(self, args, stdin=""):
        stdout, stderr = io.StringIO(), io.StringIO()
        with patch.object(sys, "argv", [str(SCRIPT), *args]), patch.object(sys, "stdin", io.StringIO(stdin)), \
             contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr), patch.dict(os.environ, {}, clear=True):
            try:
                rc = self.m.main()
            except SystemExit as exc:
                rc = exc.code
        return rc, stdout.getvalue(), stderr.getvalue()

    def credential(self):
        return self.m.RuntimeCredential("anthropic", TITLE, "SYNTHETIC_ANTHROPIC_API_KEY",
                                       f"op://Automations/{TITLE}/credential", "wrkspc_synthetic", SECRET)

    def install_store(self, existing=False, readback=SECRET):
        state = {"item": {"id": ITEM_ID, "title": TITLE, **copy.deepcopy(TEMPLATE), "notesPlain": "preserve", "tags": ["keep"]}
                 if existing else None, "writes": [], "calls": []}
        if state["item"]:
            state["item"]["fields"].append({"id": "unrelated", "type": "STRING", "value": "preserve-me"})

        def fake(argv, *, input_text=None):
            state["calls"].append((argv, input_text))
            if argv[1:3] == ["item", "list"]:
                result = [{"id": ITEM_ID, "title": state["item"]["title"]}] if state["item"] else []
            elif argv[1:4] == ["item", "template", "get"]:
                result = TEMPLATE
            elif argv[1:3] == ["item", "get"]:
                self.assertEqual(argv[3], ITEM_ID)
                result = state["item"]
            elif argv[1:3] in (["item", "create"], ["item", "edit"]):
                self.assertEqual(argv[0], "op-desktop")
                self.assertNotIn(SECRET, repr(argv))
                state["writes"].append(argv)
                state["item"] = {**json.loads(input_text), "id": ITEM_ID}
                result = {}
            elif argv[1] == "read":
                self.assertEqual(argv, ["op-automations", "read", f"op://Automations/{ITEM_ID}/credential"])
                return subprocess.CompletedProcess(argv, 0, readback + "\n", "")
            elif argv[1] == "whoami":
                self.assertEqual(argv[0], "op-desktop")
                result = {}
            else:
                self.fail("unsupported fake boundary: " + repr(argv[:3]))
            return subprocess.CompletedProcess(argv, 0, json.dumps(result), "")
        self.m.run = fake
        return state

    def test_anthropic_public_import_persists_exact_reference_and_preserves_other_lines(self):
        state = self.install_store()
        env = self.scratch / ".env.op"
        env.write_bytes(b"# synthetic comment\r\nOTHER=op://Automations/Other/credential\r\n")
        before = env.read_bytes()
        rc, out, err = self.invoke(self.args("--secret-stdin", "--repo", str(self.scratch), "--verified-monthly-spend-cap-usd", "1500"), SECRET)
        self.assertEqual((rc, err), (0, ""))
        self.assertEqual(len(state["writes"]), 1)
        self.assertTrue(env.read_bytes().startswith(before))
        self.assertIn(f"SYNTHETIC_ANTHROPIC_API_KEY=op://Automations/{ITEM_ID}/credential", env.read_text())
        self.assertIn(ITEM_ID, out)
        self.assertNotIn(SECRET, out + err + repr([c[0] for c in state["calls"]]))
        self.assertFalse(any(c[0][1] == "read" and "Employee" in repr(c[0]) for c in state["calls"]))

    def test_public_new_env_is_created_exclusively_and_verified(self):
        self.install_store()
        rc, _, err = self.invoke(self.args("--secret-stdin", "--repo", str(self.scratch), "--verified-monthly-spend-cap-usd", "1500"), SECRET)
        self.assertEqual((rc, err), (0, ""))
        self.assertEqual((self.scratch / ".env.op").read_text(), f"SYNTHETIC_ANTHROPIC_API_KEY=op://Automations/{ITEM_ID}/credential\n")
        self.assertEqual(list(self.scratch.iterdir()), [self.scratch / ".env.op"])

    def test_openai_public_mint_uses_admin_broker_and_validated_wire_shape(self):
        state = self.install_store(readback=OPENAI_SECRET)
        store_run = self.m.run
        def broker(argv, **kwargs):
            if argv[:2] == ["op-desktop", "read"]:
                state["calls"].append((argv, None))
                return subprocess.CompletedProcess(argv, 0, "SYNTHETIC_ADMIN", "")
            return store_run(argv, **kwargs)
        self.m.run = broker
        wire = {"id": "acct_synthetic", "api_key": {"value": OPENAI_SECRET}}
        with patch.object(self.m, "request_json", return_value=wire) as request:
            rc, out, err = self.invoke(self.args("--verified-monthly-spend-cap-usd", "500", provider="openai"))
        self.assertEqual((rc, err), (0, ""))
        self.assertEqual(request.call_count, 1)
        self.assertEqual(request.call_args.kwargs["method"], "POST")
        self.assertTrue(request.call_args.args[0].endswith("/service_accounts"))
        self.assertIn(ITEM_ID, out)
        self.assertNotIn(OPENAI_SECRET, out + err + repr([c[0] for c in state["calls"]]))

    def test_force_edit_uses_captured_id_and_preserves_unmanaged_metadata(self):
        state = self.install_store(existing=True)
        before = copy.deepcopy(state["item"])
        reference = self.m.create_or_update_automation_item(self.credential(), "Automations", force=True)
        self.assertEqual(reference, f"op://Automations/{ITEM_ID}/credential")
        self.assertEqual(state["writes"][0][:4], ["op-desktop", "item", "edit", ITEM_ID])
        self.assertEqual(state["item"]["notesPlain"], before["notesPlain"])
        self.assertEqual(state["item"]["tags"], before["tags"])
        self.assertIn(before["fields"][-1], state["item"]["fields"])

    def test_forced_public_edit_replaces_only_selected_env_line_with_id_reference(self):
        state = self.install_store(existing=True)
        env = self.scratch / ".env.op"
        prefix, suffix = b"# preserve\r\nOTHER=kept\r\n", b"AFTER=also-kept\r\n"
        env.write_bytes(prefix + b"SYNTHETIC_ANTHROPIC_API_KEY=op://Automations/Old/credential\r\n" + suffix)
        rc, out, err = self.invoke(self.args("--secret-stdin", "--repo", str(self.scratch), "--force", "--verified-monthly-spend-cap-usd", "1500"), SECRET)
        self.assertEqual((rc, err), (0, ""))
        self.assertEqual(env.read_bytes(), prefix + f"SYNTHETIC_ANTHROPIC_API_KEY=op://Automations/{ITEM_ID}/credential\n".encode() + suffix)
        self.assertEqual(state["writes"][0][3], ITEM_ID)
        self.assertNotIn(SECRET, out + err)

    def test_unsupported_template_fields_refuse_before_mint_or_write(self):
        for altered in [{"category": "API_CREDENTIAL", "fields": []},
                        {**copy.deepcopy(TEMPLATE), "files": [{"id": "synthetic"}]},
                        {"category": "API_CREDENTIAL", "fields": [{"id": "custom", "label": "credential", "type": "CONCEALED", "section": {"id": "x"}}]},
                        {"category": "API_CREDENTIAL", "fields": TEMPLATE["fields"] * 2}]:
            with self.assertRaises(self.m.ProvisionError):
                self.m.validate_template(altered)

    def test_exact_readback_mismatch_is_failure_even_with_successful_exit(self):
        state = self.install_store(readback="SYNTHETIC_WRONG_VALUE")
        rc, out, err = self.invoke(self.args("--secret-stdin", "--verified-monthly-spend-cap-usd", "1500"), SECRET)
        self.assertEqual(rc, 1)
        self.assertEqual(len(state["writes"]), 1)
        self.assertEqual(out, "")
        self.assertIn("read-back", err)
        self.assertIn("Reconcile", err)
        self.assertNotIn(SECRET, err)

    def test_all_lookup_errors_refuse_instead_of_inventing_absence(self):
        for rc, output in [(1, ""), (0, "{}"), (0, "null"), (0, "not-json"),
                           (0, '[{"title":"unknown"}]')]:
            self.m.run = lambda *a, **k: subprocess.CompletedProcess([], rc, output, SECRET)
            with self.assertRaises(self.m.ProvisionError) as caught:
                self.m.lookup_item(TITLE, "Automations")
            self.assertNotIn(SECRET, str(caught.exception))

    def test_duplicate_title_never_selects_first(self):
        self.m.run = lambda *a, **k: subprocess.CompletedProcess([], 0, json.dumps([
            {"id": "a" * 26, "title": TITLE}, {"id": "b" * 26, "title": TITLE}]), "")
        with self.assertRaises(self.m.ProvisionError):
            self.m.lookup_item(TITLE, "Automations")

    def test_item_and_env_conflicts_are_checked_before_remote_mint(self):
        for env_conflict in (False, True):
            state = self.install_store(existing=not env_conflict)
            if state["item"]:
                state["item"]["title"] = "OpenAI - synthetic"
            if env_conflict:
                (self.scratch / ".env.op").write_text("SYNTHETIC_OPENAI_API_KEY=op://Automations/Other/credential\n")
            with patch.object(self.m, "create_openai", side_effect=AssertionError("must not mint")):
                rc, _, _ = self.invoke(self.args("--repo", str(self.scratch), "--verified-monthly-spend-cap-usd", "500", provider="openai"))
            self.assertEqual(rc, 1)
            self.assertEqual(state["writes"], [])

    def test_invalid_inputs_refuse_before_any_secret_or_account_access(self):
        for more in [
            ["--verified-monthly-spend-cap-usd", "nan"], ["--verified-monthly-spend-cap-usd", "inf"],
            ["--verified-monthly-spend-cap-usd", "-1"], ["--verified-monthly-spend-cap-usd", "1501"],
            ["--project", "bad\nslug"], ["--env-name", "TOKEN\nINJECTED"],
            ["--provider-resource-id", "wrkspc_x/other"], ["--vault", "Other"],
            ["--admin-key-ref", "https://example.invalid"],
            ["--secret-file", "a", "--secret-stdin"],
            ["--provider-project-name", "x", "--provider-resource-id", "wrkspc_x"],
            ["--provider", "openai"], ["--repo", "bad\npath"],
            ["--env-name", ""], ["--provider-resource-id", ""], ["--admin-key-ref", ""],
        ]:
            with self.subTest(more=more):
                rc, out, _ = self.invoke(self.args(*more, "--dry-run"))
                self.assertNotEqual(rc, 0)
                self.assertEqual(out, "")

    def test_dry_run_is_pure_even_with_nonexistent_repo_and_secret_file(self):
        for provider, ceiling in (("openai", 500), ("anthropic", 1500)):
            flags = ["--repo", str(self.scratch / "absent"), "--dry-run"]
            if provider == "anthropic":
                flags += ["--secret-file", str(self.scratch / "missing")]
            rc, out, err = self.invoke(self.args(*flags, provider=provider))
            self.assertEqual((rc, err), (0, ""))
            plan = json.loads(out)
            self.assertEqual(plan["max_autonomous_monthly_spend_usd"], ceiling)
            self.assertFalse(plan["will_create_provider_secret"])
            self.assertFalse(plan["will_store_secret"])
            self.assertIn("caller assertion", plan["spend_control_basis"])
        self.assertEqual(list(self.scratch.iterdir()), [])

    def test_openai_import_is_refused_without_reading_it(self):
        rc, _, err = self.invoke(self.args("--secret-stdin", "--verified-monthly-spend-cap-usd", "500", provider="openai"), OPENAI_SECRET)
        self.assertEqual(rc, 1)
        self.assertIn("mint-only", err)
        self.assertNotIn(OPENAI_SECRET, err)

    def test_absent_anthropic_import_never_reads_admin(self):
        rc, out, err = self.invoke(self.args("--verified-monthly-spend-cap-usd", "1500"))
        self.assertEqual(rc, 1)
        self.assertEqual(out, "")
        self.assertIn("--secret-file or --secret-stdin", err)

    def test_secret_file_rejects_symlinks_fifo_oversize_and_bad_shape(self):
        secret_path = self.scratch / "secret-fixture"
        secret_path.write_text(SECRET)
        link = self.scratch / "link"
        link.symlink_to(secret_path)
        fifo = self.scratch / "pipe"
        os.mkfifo(fifo)
        big = self.scratch / "large"
        big.write_bytes(b"x" * 4097)
        for path in (link, fifo, big):
            rc, out, _ = self.invoke(self.args("--secret-file", str(path), "--verified-monthly-spend-cap-usd", "1500"))
            self.assertEqual(rc, 1)
            self.assertEqual(out, "")
        for value in ("", SECRET + "\n\n", SECRET + "\r", SECRET + " a"):
            rc, _, _ = self.invoke(self.args("--secret-stdin", "--verified-monthly-spend-cap-usd", "1500"), value)
            self.assertEqual(rc, 1)

    def test_env_duplicate_export_symlink_and_drift_refuse(self):
        path = self.scratch / ".env.op"
        ref = "op://Automations/Example/credential"
        path.write_text("export TARGET=old\nTARGET=other\n")
        with self.assertRaises(self.m.ProvisionError):
            self.m.prepare_env(self.scratch, "TARGET", ref, force=True)
        path.write_text("OTHER=keep\n")
        planned = self.m.prepare_env(self.scratch, "TARGET", ref, force=False)
        path.write_text("OTHER=changed\n")
        with self.assertRaises(self.m.ProvisionError):
            self.m.write_env_op(self.scratch, "TARGET", ref, force=False, prepared=planned)
        self.assertEqual(path.read_text(), "OTHER=changed\n")
        target = self.scratch / "target"
        target.write_text("untouched")
        path.unlink()
        path.symlink_to(target)
        with self.assertRaises(OSError):
            self.m.prepare_env(self.scratch, "TARGET", ref, force=True)
        self.assertEqual(target.read_text(), "untouched")

    def test_remote_item_drift_refuses_before_write(self):
        state = self.install_store(existing=True)
        planned = self.m.prepare_item(TITLE, "Automations", force=True)
        state["item"]["notesPlain"] = "concurrent user change"
        with self.assertRaises(self.m.ProvisionError):
            self.m.create_or_update_automation_item(self.credential(), "Automations", force=True, prepared=planned)
        self.assertEqual(state["writes"], [])

    def test_provider_errors_do_not_echo_headers_body_or_redirect_credentials(self):
        headers = {"Authorization": "Bearer " + OPENAI_SECRET}
        error = urllib.error.HTTPError("https://api.openai.com/x", 403, SECRET, {}, io.BytesIO(SECRET.encode()))
        class Opener:
            def open(self, *a, **k):
                raise error
        with patch.object(self.m.urllib.request, "build_opener", return_value=Opener()):
            with self.assertRaises(self.m.ProvisionError) as caught:
                self.m.request_json("https://api.openai.com/x", headers=headers)
        self.assertNotIn(SECRET, str(caught.exception))
        self.assertIn("403", str(caught.exception))
        with self.assertRaises(self.m.ProvisionError):
            self.m.NoRedirect().redirect_request(None, None, 302, "", {}, "https://other.invalid")
        with self.assertRaises(self.m.ProvisionError):
            self.m.request_json("https://api.openai.com@other.invalid/x", headers=headers)
        error = http.client.BadStatusLine(SECRET)
        with patch.object(self.m.urllib.request, "build_opener", return_value=Opener()):
            with self.assertRaises(self.m.ProvisionError) as malformed:
                self.m.request_json("https://api.openai.com/x", headers=headers)
        self.assertNotIn(SECRET, str(malformed.exception))

    def test_public_error_output_suppresses_raw_broker_stderr(self):
        self.m.run = lambda *a, **k: subprocess.CompletedProcess([], 1, SECRET, SECRET + "\x1b[2J")
        rc, out, err = self.invoke(self.args("--secret-stdin", "--verified-monthly-spend-cap-usd", "1500"), SECRET)
        self.assertEqual(rc, 1)
        self.assertNotIn(SECRET, out + err)
        self.assertNotIn("\x1b", out + err)

    def test_provider_name_lookup_consumes_pages_and_refuses_incomplete_inventory(self):
        pages = [{"data": [{"id": "proj_first", "name": "other"}], "has_more": True, "last_id": "proj_first"},
                 {"data": [{"id": "proj_second", "name": "selected"}], "has_more": False}]
        calls = []
        def request(url, **kwargs):
            calls.append((url, kwargs))
            return pages.pop(0)
        with patch.object(self.m, "request_json", side_effect=request):
            result = self.m.get_or_create_openai_project("SYNTHETIC_ADMIN", "selected")
        self.assertEqual(result, ("proj_second", False))
        self.assertIn("after=proj_first", calls[1][0])
        self.assertFalse(any(c[1].get("method") == "POST" for c in calls))
        for response in ({}, {"data": [], "has_more": True}, {"data": [], "has_more": "false"}):
            with patch.object(self.m, "request_json", return_value=response):
                with self.assertRaises(self.m.ProvisionError):
                    self.m.get_or_create_openai_project("SYNTHETIC_ADMIN", "selected")


if __name__ == "__main__":
    unittest.main()
