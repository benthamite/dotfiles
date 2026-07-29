from __future__ import annotations

import json
import subprocess
import tomllib
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "clean-codex-config"


def run_cleaner(config: Path, mode: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(SCRIPT), "--config", str(config), mode],
        text=True,
        capture_output=True,
    )


def test_cleaner_reports_then_removes_only_stale_generated_tables(
    tmp_path: Path,
) -> None:
    existing_project = tmp_path / "existing-project"
    existing_project.mkdir()
    missing_project = tmp_path / "missing-project"
    hooks_path = tmp_path / "hooks.json"
    hooks_path.write_text(
        json.dumps(
            {
                "hooks": {
                    "PreToolUse": [
                        {
                            "matcher": "Bash",
                            "hooks": [{"type": "command", "command": "true"}],
                        }
                    ]
                }
            }
        ),
        encoding="utf-8",
    )
    missing_hooks_path = tmp_path / "missing-hooks.json"
    config = tmp_path / "config.toml"
    original = f"""model = "gpt-test"

[projects.{json.dumps(str(existing_project))}]
trust_level = "trusted"

[projects.{json.dumps(str(missing_project))}]
trust_level = "trusted"

[tui]
theme = "dark"

[hooks.state]

[hooks.state.{json.dumps(f"{hooks_path}:pre_tool_use:0:0")}]
trusted_hash = "sha256:live"

[hooks.state.{json.dumps(f"{hooks_path}:post_tool_use:0:0")}]
trusted_hash = "sha256:stale-event"

[hooks.state.{json.dumps(f"{hooks_path}:pre_tool_use:0:1")}]
trusted_hash = "sha256:stale-index"

[hooks.state.{json.dumps(f"{missing_hooks_path}:pre_tool_use:0:0")}]
trusted_hash = "sha256:missing-file"

[notice]
hide_rate_limit_model_nudge = true
"""
    config.write_text(original, encoding="utf-8")

    check = run_cleaner(config, "--check")

    assert check.returncode == 1
    assert "1 stale project trust entry" in check.stdout
    assert "3 stale hook trust entries" in check.stdout
    assert config.read_text(encoding="utf-8") == original

    write = run_cleaner(config, "--write")

    assert write.returncode == 0, write.stderr
    assert "Removed 1 stale project trust entry" in write.stdout
    assert "3 stale hook trust entries" in write.stdout
    parsed = tomllib.loads(config.read_text(encoding="utf-8"))
    assert parsed["projects"] == {
        str(existing_project): {"trust_level": "trusted"}
    }
    assert parsed["tui"] == {"theme": "dark"}
    assert parsed["notice"] == {"hide_rate_limit_model_nudge": True}
    assert parsed["hooks"]["state"] == {
        f"{hooks_path}:pre_tool_use:0:0": {"trusted_hash": "sha256:live"}
    }

    clean = run_cleaner(config, "--check")
    assert clean.returncode == 0, clean.stderr
    assert "No stale Codex config entries found" in clean.stdout
