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


def test_cleaner_writes_through_config_symlink(tmp_path: Path) -> None:
    missing_project = tmp_path / "missing-project"
    target = tmp_path / "tracked-config.toml"
    target.write_text(
        f"""model = "gpt-test"

[projects.{json.dumps(str(missing_project))}]
trust_level = "trusted"
""",
        encoding="utf-8",
    )
    link = tmp_path / "config.toml"
    link.symlink_to(target)

    result = run_cleaner(link, "--write")

    assert result.returncode == 0, result.stderr
    assert link.is_symlink()
    assert tomllib.loads(target.read_text(encoding="utf-8")) == {
        "model": "gpt-test"
    }


def test_cleaner_removes_semantically_equivalent_table_headers(
    tmp_path: Path,
) -> None:
    missing = [tmp_path / f"missing-{index}" for index in range(3)]
    config = tmp_path / "config.toml"
    config.write_text(
        f"""model = "gpt-test"

 [ projects . {json.dumps(str(missing[0]))} ] # spaced header
trust_level = "trusted"

[projects.'{missing[1]}']
trust_level = "trusted"

[ projects.{json.dumps(str(missing[2]))}]# compact comment
trust_level = "trusted"
""",
        encoding="utf-8",
    )

    result = run_cleaner(config, "--write")

    assert result.returncode == 0, result.stderr
    assert tomllib.loads(config.read_text(encoding="utf-8")) == {
        "model": "gpt-test"
    }
    assert run_cleaner(config, "--check").returncode == 0


def test_cleaner_reports_invalid_hook_json_shape_without_traceback(
    tmp_path: Path,
) -> None:
    hooks_path = tmp_path / "hooks.json"
    hooks_path.write_text("[]\n", encoding="utf-8")
    config = tmp_path / "config.toml"
    config.write_text(
        f"""[hooks.state]

[hooks.state.{json.dumps(f"{hooks_path}:pre_tool_use:0:0")}]
trusted_hash = "sha256:value"
""",
        encoding="utf-8",
    )

    result = run_cleaner(config, "--check")

    assert result.returncode == 2
    assert "top-level JSON value is not an object" in result.stderr
    assert "Traceback" not in result.stderr


def test_cleaner_preserves_bracket_prefixed_multiline_array_values(
    tmp_path: Path,
) -> None:
    missing_project = tmp_path / "missing-project"
    config = tmp_path / "config.toml"
    config.write_text(
        f"""[settings]
matrix = [
  [1, 2],
  [3, 4],
]

[projects.{json.dumps(str(missing_project))}]
trust_level = "trusted"
""",
        encoding="utf-8",
    )

    result = run_cleaner(config, "--write")

    assert result.returncode == 0, result.stderr
    assert tomllib.loads(config.read_text(encoding="utf-8")) == {
        "settings": {"matrix": [[1, 2], [3, 4]]}
    }
