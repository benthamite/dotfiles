from __future__ import annotations

import os
import shlex
import subprocess
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path("/Users/pablostafforini/My Drive/dotfiles")
GUARD = DOTFILES / "shell" / "zsh-history-security.zsh"


class ZshHistorySecretGuardTests(unittest.TestCase):
    def test_credential_shaped_command_executes_without_persisting(self):
        fake_credentials = {
            "AWS access key": "AK" + "IA" + ("A" * 16),
            "GitHub classic token": "gh" + "p_" + ("A" * 36),
            "GitHub fine-grained token": "github_" + "pat_" + ("A" * 22),
            "Slack token": "xo" + "xb-" + ("A" * 20),
            "Anthropic key": "sk-" + "ant-" + ("A" * 40),
            "generic API key": "sk-" + ("A" * 24),
            "Stripe key": "sk_" + "live_" + ("A" * 20),
            "Linear key": "lin_" + "api_" + ("A" * 40),
            "GitLab token": "gl" + "pat-" + ("A" * 20),
            "Google API key": "AI" + "za" + ("A" * 35),
        }

        with tempfile.TemporaryDirectory() as temp_dir:
            history = Path(temp_dir) / "history"
            command_lines = [
                f"HISTFILE={history}",
                "HISTSIZE=100",
                "SAVEHIST=100",
                "setopt APPEND_HISTORY",
                f"source {shlex.quote(str(GUARD))}",
                "true safe-history-entry",
            ]
            command_lines.extend(
                f"true {credential}" for credential in fake_credentials.values()
            )
            command_lines.extend(['fc -W "$HISTFILE"', "exit", ""])
            commands = "\n".join(command_lines)
            env = os.environ.copy()
            env["ZDOTDIR"] = temp_dir

            result = subprocess.run(
                ["zsh", "-dfi"],
                input=commands,
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                env=env,
                check=False,
            )

            self.assertEqual(result.returncode, 0, result.stderr)
            saved = history.read_text(encoding="utf-8")
            self.assertIn("safe-history-entry", saved)
            for label, credential in fake_credentials.items():
                with self.subTest(label=label):
                    self.assertFalse(
                        credential in saved,
                        f"{label} command was persisted",
                    )
            self.assertIn("credential-shaped command omitted from history", result.stderr)


if __name__ == "__main__":
    unittest.main()
