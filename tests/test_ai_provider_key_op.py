import importlib.machinery
import importlib.util
import json
import socket
import subprocess
import sys
import unittest
from pathlib import Path
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "claude" / "bin" / "ai-provider-key"


def load_module():
    loader = importlib.machinery.SourceFileLoader("ai_provider_key", str(SCRIPT))
    spec = importlib.util.spec_from_loader("ai_provider_key", loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    loader.exec_module(module)
    return module


class AiProviderKeyOpTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()
        self.calls = []
        self.inputs = []
        self.addCleanup(patch.stopall)
        patch.object(socket, "socket", side_effect=AssertionError("offline only")).start()
        patch.object(subprocess, "run", side_effect=AssertionError("all broker calls must be faked")).start()

    def fake_run(self, responses):
        def run(args, **kwargs):
            self.calls.append(args)
            self.inputs.append(kwargs.get("input_text"))
            return responses.pop(0)

        self.mod.run = run

    def test_desktop_session_uses_broker_and_never_falls_back_to_signin(self):
        self.fake_run(
            [
                subprocess.CompletedProcess([], 1, "", "not signed in"),
            ]
        )

        with self.assertRaises(self.mod.ProvisionError):
            self.mod.ensure_desktop_op_session()

        self.assertEqual(
            self.calls,
            [
                ["op-desktop", "whoami"],
            ],
        )

    def test_automations_read_uses_promptless_wrapper(self):
        self.fake_run([subprocess.CompletedProcess([], 0, "value\n", "")])

        value = self.mod.op_read("op://Automations/Example/credential")

        self.assertEqual(value, "value")
        self.assertEqual(
            self.calls,
            [["op-automations", "read", "op://Automations/Example/credential"]],
        )

    def test_non_automations_read_requires_explicit_desktop_session(self):
        with self.assertRaises(self.mod.ProvisionError):
            self.mod.op_read("op://Employee/Example/credential")

        self.assertEqual(self.calls, [])

    def test_automation_item_lookup_uses_promptless_wrapper(self):
        self.fake_run([subprocess.CompletedProcess([], 0, json.dumps([{"id": "a" * 26, "title": "Example"}]), "")])

        self.assertTrue(self.mod.op_item_exists("Example", "Automations"))
        self.assertEqual(
            self.calls,
            [["op-automations", "item", "list", "--vault", "Automations", "--format", "json"]],
        )

    def test_existing_item_without_force_never_starts_desktop_auth(self):
        credential = self.mod.RuntimeCredential(
            provider="openai",
            item_title="OpenAI - example",
            env_name="OPENAI_API_KEY",
            op_ref="op://Automations/OpenAI - example/credential",
            project_or_workspace_id="proj_example",
            secret="not-a-real-secret",
        )
        self.fake_run([subprocess.CompletedProcess([], 0, json.dumps([{"id": "a" * 26, "title": credential.item_title}]), "")])

        with self.assertRaises(self.mod.ProvisionError):
            self.mod.create_or_update_automation_item(credential, "Automations", force=False)

        self.assertEqual(
            self.calls,
            [["op-automations", "item", "list", "--vault", "Automations", "--format", "json"]],
        )

    def test_create_uses_stdin_template_and_verifies_exact_value_by_captured_id(self):
        credential = self.mod.RuntimeCredential(
            provider="openai",
            item_title="OpenAI - example",
            env_name="OPENAI_API_KEY",
            op_ref="op://Automations/OpenAI - example/credential",
            project_or_workspace_id="proj_example",
            secret="not-a-real-secret",
        )
        self.fake_run(
            [
                subprocess.CompletedProcess([], 0, "[]", ""),
                subprocess.CompletedProcess([], 0, '{"category":"API_CREDENTIAL","fields":[{"id":"credential","type":"CONCEALED","value":""}]}', ""),
                subprocess.CompletedProcess([], 0, "[]", ""),
                subprocess.CompletedProcess([], 0, "", ""),
                subprocess.CompletedProcess([], 0, json.dumps([{"id": "a" * 26, "title": credential.item_title}]), ""),
                subprocess.CompletedProcess([], 0, credential.secret + "\n", ""),
            ]
        )

        self.mod.create_or_update_automation_item(credential, "Automations", force=False)

        self.assertEqual(self.calls[0][0], "op-automations")
        self.assertEqual(self.calls[1], ["op-desktop", "item", "template", "get", "API Credential", "--format", "json"])
        self.assertEqual(self.calls[3], ["op-desktop", "item", "create", "-", "--vault", "Automations"])
        self.assertNotIn(credential.secret, repr(self.calls))
        self.assertIn(credential.secret, self.inputs[3])
        self.assertEqual(
            self.calls[5],
            ["op-automations", "read", "op://Automations/" + "a" * 26 + "/credential"],
        )


if __name__ == "__main__":
    unittest.main()
