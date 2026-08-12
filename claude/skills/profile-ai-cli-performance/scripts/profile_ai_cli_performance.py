#!/usr/bin/env python3
"""Benchmark clean and configured Claude Code and Codex processes."""

from __future__ import annotations

import argparse
import json
import os
import random
import queue
import signal
import shutil
import statistics
import subprocess
import sys
import tempfile
import threading
import time
from collections import Counter
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Compare clean and configured Claude Code and Codex latency."
    )
    parser.add_argument("--project-dir", required=True, type=Path)
    parser.add_argument("--claude-model", required=True)
    parser.add_argument("--claude-effort", required=True)
    parser.add_argument("--codex-model", required=True)
    parser.add_argument("--codex-effort", required=True)
    parser.add_argument("--runs", type=int, default=10)
    parser.add_argument("--timeout", type=float, default=180)
    parser.add_argument("--seed", type=int, default=481516)
    parser.add_argument("--expected", default="OK")
    parser.add_argument("--prompt")
    parser.add_argument("--claude-bin", type=Path)
    parser.add_argument("--codex-bin", type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument(
        "--claude-mcp-ablation",
        action="store_true",
        help="Add configured Claude with only hosted/local MCP loading excluded.",
    )
    parser.add_argument(
        "--dry-run", action="store_true", help="Print sanitized commands only."
    )
    return parser.parse_args()


def native_binary(explicit: Path | None, name: str) -> str:
    if explicit:
        path = explicit.expanduser().resolve()
    else:
        candidates = [Path(f"/opt/homebrew/bin/{name}"), Path(f"/usr/local/bin/{name}")]
        found = shutil.which(name)
        if found:
            candidates.append(Path(found))
        path = next((item.resolve() for item in candidates if item.is_file()), None)
        if path is None:
            raise SystemExit(f"Cannot find {name}")
    if not path.is_file():
        raise SystemExit(f"Binary does not exist: {path}")
    return str(path)


def percentile(values: list[float], fraction: float) -> float:
    ordered = sorted(values)
    position = (len(ordered) - 1) * fraction
    low = int(position)
    high = min(low + 1, len(ordered) - 1)
    return ordered[low] + (ordered[high] - ordered[low]) * (position - low)


def classify(stderr: str, lines: list[str], code: int | None, timed_out: bool) -> str:
    text = (stderr + "\n" + "\n".join(lines)).lower()
    if timed_out:
        return "timeout"
    if any(term in text for term in ("usage limit", "rate limit", "quota", "retry-after")):
        return "rate_limit"
    if any(term in text for term in ("not logged in", "authentication", "unauthorized", "login required")):
        return "auth_failure"
    if any(term in text for term in ('"type":"error"', '"type": "error"', "turn.failed", "provider error")):
        return "provider_rejection"
    if code:
        return "process_error"
    return "bad_output"


def terminate_process_group(proc: subprocess.Popen) -> None:
    try:
        os.killpg(proc.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass
    try:
        proc.wait(timeout=3)
    except subprocess.TimeoutExpired:
        try:
            os.killpg(proc.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        proc.wait()


def execute(condition: dict, timeout: float, expected: str) -> dict:
    started = time.monotonic()
    first_line = None
    first_partial = None
    completed_message = None
    answer = None
    init = None
    lines: list[str] = []
    proc = subprocess.Popen(
        condition["command"],
        cwd=condition["cwd"],
        env=condition["env"],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1,
        start_new_session=True,
    )
    events: queue.Queue = queue.Queue()

    def read_stream(name: str, stream) -> None:
        try:
            for raw_line in stream:
                events.put((name, time.monotonic(), raw_line))
        finally:
            events.put((name, time.monotonic(), None))

    readers = [
        threading.Thread(target=read_stream, args=("stdout", proc.stdout), daemon=True),
        threading.Thread(target=read_stream, args=("stderr", proc.stderr), daemon=True),
    ]
    for reader in readers:
        reader.start()
    timed_out = False
    stderr_parts = []
    closed_streams = set()
    try:
        deadline = started + timeout
        while len(closed_streams) < 2:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                timed_out = True
                terminate_process_group(proc)
                break
            try:
                stream_name, now, raw = events.get(timeout=min(0.25, remaining))
            except queue.Empty:
                if proc.poll() is not None and all(not reader.is_alive() for reader in readers):
                    break
                continue
            if raw is None:
                closed_streams.add(stream_name)
                continue
            if stream_name == "stderr":
                stderr_parts.append(raw)
                continue
            first_line = first_line or now
            line = raw.strip()
            if line:
                if not line:
                    continue
                lines.append(line)
                try:
                    event = json.loads(line)
                except json.JSONDecodeError:
                    continue
                event_type = event.get("type")
                if event_type == "system" and event.get("subtype") == "init":
                    init = event
                if event_type == "stream_event":
                    nested = event.get("event", {})
                    delta = nested.get("delta", {})
                    if delta.get("type") == "text_delta" and delta.get("text"):
                        first_partial = first_partial or now
                if event_type == "assistant":
                    content = event.get("message", {}).get("content", [])
                    text = "".join(item.get("text", "") for item in content if item.get("type") == "text")
                    if text:
                        completed_message = completed_message or now
                        answer = text
                if event_type == "item.completed":
                    item = event.get("item", {})
                    if item.get("type") == "agent_message":
                        completed_message = completed_message or now
                        answer = item.get("text")
                if event_type == "result" and event.get("result") is not None:
                    answer = event["result"]
    except BaseException:
        terminate_process_group(proc)
        raise
    finally:
        if proc.poll() is None:
            terminate_process_group(proc)
        for reader in readers:
            reader.join(timeout=1)
        proc.stdout.close()
        proc.stderr.close()
    stderr = "".join(stderr_parts)
    code = proc.returncode
    ended = time.monotonic()
    valid = code == 0 and answer == expected and not timed_out
    return {
        "condition": condition["name"],
        "status": "success" if valid else classify(stderr, lines, code, timed_out),
        "exit_code": code,
        "answer_valid": answer == expected,
        "first_stdout_line_s": first_line - started if first_line else None,
        "first_partial_content_s": first_partial - started if first_partial else None,
        "completed_message_s": completed_message - started if completed_message else None,
        "total_s": ended - started,
        "isolation": {
            "mcp_servers": init.get("mcp_servers") if init else None,
            "hosted_tool_count": sum(
                name.startswith("mcp__claude_ai_") for name in (init or {}).get("tools", [])
            ),
        },
        "error_excerpt": (stderr + "\n" + "\n".join(lines[-5:]))[-1000:] if not valid else "",
    }


def summarize(rows: list[dict]) -> dict:
    result = {}
    for name in sorted({row["condition"] for row in rows}):
        matching = [row for row in rows if row["condition"] == name]
        successes = [row for row in matching if row["status"] == "success"]
        completed = [row["completed_message_s"] for row in successes if row["completed_message_s"] is not None]
        totals = [row["total_s"] for row in successes]
        result[name] = {
            "attempts": len(matching),
            "successes": len(successes),
            "failures": dict(Counter(row["status"] for row in matching if row["status"] != "success")),
            "completed_message_observations": len(completed),
            "completed_message_median_s": statistics.median(completed) if completed else None,
            "completed_message_p95_s": percentile(completed, 0.95) if completed else None,
            "total_median_s": statistics.median(totals) if totals else None,
            "total_p95_s": percentile(totals, 0.95) if totals else None,
        }
    for client in ("claude", "codex"):
        clean = result.get(f"{client}_clean", {}).get("total_median_s")
        configured = result.get(f"{client}_configured", {}).get("total_median_s")
        if clean is not None and configured is not None:
            clean_completed = result[f"{client}_clean"]["completed_message_median_s"]
            configured_completed = result[f"{client}_configured"]["completed_message_median_s"]
            result[f"{client}_configuration_association"] = {
                "median_completed_message_delta_s": configured_completed - clean_completed,
                "median_total_delta_s": configured - clean,
                "median_total_ratio": configured / clean if clean else None,
            }
            by_iteration = {}
            for row in rows:
                if row["condition"] in {f"{client}_clean", f"{client}_configured"}:
                    by_iteration.setdefault(row["iteration"], {})[row["condition"]] = row
            pairs = [
                pair for pair in by_iteration.values()
                if f"{client}_clean" in pair and f"{client}_configured" in pair
                and pair[f"{client}_clean"]["status"] == "success"
                and pair[f"{client}_configured"]["status"] == "success"
            ]
            completed_deltas = [
                pair[f"{client}_configured"]["completed_message_s"]
                - pair[f"{client}_clean"]["completed_message_s"]
                for pair in pairs
            ]
            total_deltas = [
                pair[f"{client}_configured"]["total_s"]
                - pair[f"{client}_clean"]["total_s"]
                for pair in pairs
            ]
            result[f"{client}_configuration_association"].update({
                "paired_blocks": len(pairs),
                "paired_completed_message_delta_median_s": statistics.median(completed_deltas),
                "paired_total_delta_median_s": statistics.median(total_deltas),
            })
    return result


def main() -> int:
    args = parse_args()
    if args.runs < 1:
        raise SystemExit("--runs must be positive")
    if args.prompt is None:
        args.prompt = f"Reply with exactly {args.expected} and nothing else."
    project = args.project_dir.expanduser().resolve()
    if not project.is_dir():
        raise SystemExit(f"Project directory does not exist: {project}")
    claude = native_binary(args.claude_bin, "claude")
    codex = native_binary(args.codex_bin, "codex")
    real_home = Path.home()
    active_codex_home = Path(os.environ.get("CODEX_HOME", real_home / ".codex")).expanduser().resolve()
    auth = active_codex_home / "auth.json"
    if not auth.is_file():
        raise SystemExit(f"Active Codex auth file is missing: {auth}")

    with tempfile.TemporaryDirectory(prefix="ai-cli-profile-", dir="/tmp") as temp_name:
        root = Path(temp_name)
        root.chmod(0o700)
        work = root / "work"
        clean_home = root / "home"
        clean_codex = clean_home / ".codex"
        work.mkdir()
        clean_codex.mkdir(parents=True)
        clean_auth = clean_codex / "auth.json"
        shutil.copyfile(auth, clean_auth)
        clean_auth.chmod(0o600)

        base_env = {
            "HOME": str(real_home),
            "USER": os.environ.get("USER", ""),
            "LOGNAME": os.environ.get("LOGNAME", ""),
            "SHELL": "/bin/zsh",
            "PATH": "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin",
            "TMPDIR": "/tmp",
            "LANG": os.environ.get("LANG", "C.UTF-8"),
        }
        for name, value in os.environ.items():
            if name.upper().endswith("_PROXY") or name in {
                "NO_PROXY", "SSL_CERT_FILE", "SSL_CERT_DIR", "REQUESTS_CA_BUNDLE",
            }:
                base_env[name] = value
        configured_env = dict(os.environ)
        configured_env.pop("ANTHROPIC_API_KEY", None)
        configured_env.pop("ANTHROPIC_AUTH_TOKEN", None)
        for name in (
            "OPENAI_API_KEY", "OPENAI_AUTH_TOKEN", "CODEX_API_KEY",
            "CODEX_ACCESS_TOKEN",
        ):
            configured_env.pop(name, None)
        codex_configured_env = dict(configured_env)
        claude_common = [
            claude, "--no-session-persistence", "--prompt-suggestions", "false",
            "--model", args.claude_model, "--effort", args.claude_effort,
            "--verbose", "-p", "--output-format", "stream-json",
            "--include-partial-messages", args.prompt,
        ]
        codex_common = [
            codex, "exec", "--ephemeral", "-s", "read-only",
            "-c", 'approval_policy="never"', "-m", args.codex_model,
            "-c", f'model_reasoning_effort="{args.codex_effort}"', "--json", args.prompt,
        ]
        conditions = {
            "claude_clean": {
                "name": "claude_clean", "cwd": str(work), "env": base_env,
                "command": claude_common[:1] + ["--safe-mode", "--strict-mcp-config", "--no-chrome"] + claude_common[1:],
            },
            "claude_configured": {
                "name": "claude_configured", "cwd": str(project), "env": configured_env,
                "command": claude_common,
            },
            "codex_clean": {
                "name": "codex_clean", "cwd": str(work),
                "env": {**base_env, "HOME": str(clean_home), "CODEX_HOME": str(clean_codex)},
                "command": codex_common[:2] + [
                    "--ignore-user-config", "--ignore-rules", "--skip-git-repo-check",
                    "--disable", "hooks", "--disable", "plugins", "--disable", "apps",
                    "--disable", "multi_agent", "--disable", "workspace_dependencies",
                    "--disable", "skill_search",
                ] + codex_common[2:],
            },
            "codex_configured": {
                "name": "codex_configured", "cwd": str(project), "env": codex_configured_env,
                "command": codex_common,
            },
        }
        if args.claude_mcp_ablation:
            conditions["claude_configured_no_mcp"] = {
                "name": "claude_configured_no_mcp", "cwd": str(project), "env": configured_env,
                "command": claude_common[:1] + ["--strict-mcp-config"] + claude_common[1:],
            }

        metadata = {
            "claude_binary": claude,
            "codex_binary": codex,
            "active_codex_home": str(active_codex_home),
            "project_dir": str(project),
            "runs": args.runs,
            "timeout_s": args.timeout,
            "seed": args.seed,
            "prompt": args.prompt,
            "expected": args.expected,
            "models": {"claude": args.claude_model, "codex": args.codex_model},
            "effort": {"claude": args.claude_effort, "codex": args.codex_effort},
        }
        if args.dry_run:
            print(json.dumps({"metadata": metadata, "commands": {name: value["command"] for name, value in conditions.items()}}, indent=2))
            return 0

        metadata["versions"] = {
            "claude": subprocess.check_output([claude, "--version"], text=True).strip(),
            "codex": subprocess.check_output([codex, "--version"], text=True).strip(),
        }
        preflight = []
        for condition in conditions.values():
            row = execute(condition, args.timeout, args.expected)
            row["phase"] = "preflight"
            preflight.append(row)
        failed = [row for row in preflight if row["status"] != "success"]
        clean_claude = next(row for row in preflight if row["condition"] == "claude_clean")
        if clean_claude["isolation"]["mcp_servers"] != []:
            failed.append({"condition": "claude_clean", "status": "isolation_failure"})
        if clean_claude["isolation"]["hosted_tool_count"]:
            failed.append({"condition": "claude_clean", "status": "isolation_failure"})
        if args.claude_mcp_ablation:
            ablation = next(
                row for row in preflight
                if row["condition"] == "claude_configured_no_mcp"
            )
            if (
                ablation["isolation"]["mcp_servers"] != []
                or ablation["isolation"]["hosted_tool_count"]
            ):
                failed.append({
                    "condition": "claude_configured_no_mcp",
                    "status": "isolation_failure",
                })
        codex_mcp = subprocess.run(
            [codex, "mcp", "list"], env=conditions["codex_clean"]["env"],
            cwd=work, text=True, capture_output=True, timeout=30,
        )
        if codex_mcp.returncode or "No MCP servers configured" not in codex_mcp.stdout:
            failed.append({"condition": "codex_clean", "status": "isolation_failure"})
        if failed:
            print(json.dumps({"metadata": metadata, "preflight": preflight, "status": "unavailable"}, indent=2))
            return 2

        rng = random.Random(args.seed)
        rows = []
        names = list(conditions)
        for iteration in range(1, args.runs + 1):
            order = names[:]
            rng.shuffle(order)
            for name in order:
                row = execute(conditions[name], args.timeout, args.expected)
                row.update({"phase": "measured", "iteration": iteration})
                rows.append(row)
                print(json.dumps({"progress": f"{iteration}/{args.runs}", "condition": name, "status": row["status"], "total_s": row["total_s"]}), flush=True)
                if row["status"] in {"auth_failure", "rate_limit"}:
                    report = {"metadata": metadata, "preflight": preflight, "attempts": rows, "status": "unavailable"}
                    if args.output:
                        args.output.write_text(json.dumps(report, indent=2) + "\n")
                    print("FINAL_JSON=" + json.dumps(report))
                    return 2

        all_successful = all(row["status"] == "success" for row in rows)
        report = {
            "metadata": metadata,
            "preflight": preflight,
            "attempts": rows,
            "summary": summarize(rows),
            "status": "complete" if all_successful else "inconclusive",
        }
        if args.output:
            args.output.write_text(json.dumps(report, indent=2) + "\n")
            args.output.chmod(0o600)
        print("FINAL_JSON=" + json.dumps(report))
        return 0 if all_successful else 2


if __name__ == "__main__":
    sys.exit(main())
