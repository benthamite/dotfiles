import importlib.machinery
import importlib.util
import subprocess
import sys
import unittest
from pathlib import Path


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

    def fake_run(self, responses):
        def run(args, **_kwargs):
            self.calls.append(args)
            return responses.pop(0)

        self.mod.run = run

    def test_desktop_session_reuses_whoami_then_signs_in_without_force(self):
        self.fake_run(
            [
                subprocess.CompletedProcess([], 1, "", "not signed in"),
                subprocess.CompletedProcess([], 0, "", ""),
            ]
        )

        self.mod.ensure_desktop_op_session()

        self.assertEqual(
            self.calls,
            [
                ["env", "-u", "OP_SERVICE_ACCOUNT_TOKEN", "op", "whoami"],
                ["env", "-u", "OP_SERVICE_ACCOUNT_TOKEN", "op", "signin"],
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
        self.fake_run([subprocess.CompletedProcess([], 0, "{}", "")])

        self.assertTrue(self.mod.op_item_exists("Example", "Automations"))
        self.assertEqual(
            self.calls,
            [["op-automations", "item", "get", "Example", "--vault", "Automations", "--format", "json"]],
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
        self.fake_run([subprocess.CompletedProcess([], 0, "{}", "")])

        with self.assertRaises(self.mod.ProvisionError):
            self.mod.create_or_update_automation_item(credential, "Automations", force=False)

        self.assertEqual(
            self.calls,
            [["op-automations", "item", "get", "OpenAI - example", "--vault", "Automations", "--format", "json"]],
        )

    def test_create_authenticates_only_after_lookup_and_verifies_promptlessly(self):
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
                subprocess.CompletedProcess([], 1, "", "not found"),
                subprocess.CompletedProcess([], 0, "", ""),
                subprocess.CompletedProcess([], 0, "", ""),
                subprocess.CompletedProcess([], 0, "value", ""),
            ]
        )

        self.mod.create_or_update_automation_item(credential, "Automations", force=False)

        self.assertEqual(self.calls[0][0], "op-automations")
        self.assertEqual(self.calls[1], ["env", "-u", "OP_SERVICE_ACCOUNT_TOKEN", "op", "whoami"])
        self.assertEqual(self.calls[2][0:5], ["env", "-u", "OP_SERVICE_ACCOUNT_TOKEN", "op", "item"])
        self.assertEqual(
            self.calls[3],
            ["op-automations", "read", "op://Automations/OpenAI - example/credential"],
        )


if __name__ == "__main__":
    unittest.main()
