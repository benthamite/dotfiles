#!/usr/bin/env python3
"""Benchmark clean and configured Claude Code and Codex processes."""

from __future__ import annotations

import argparse
import json
import math
import os
import random
import re
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


class BenchmarkError(Exception):
    """A fixed, credential-free preflight or reporting failure."""


MAX_OUTPUT_CHARS = 2 * 1024 * 1024
MAX_LINE_CHARS = 256 * 1024


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Compare clean and configured Claude Code and Codex latency."
    )
    parser.add_argument("--project-dir", required=True, type=Path)
    parser.add_argument("--claude-model")
    parser.add_argument("--claude-effort")
    parser.add_argument("--codex-model")
    parser.add_argument("--codex-effort")
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
        "--only",
        action="append",
        default=[],
        help=(
            "Restrict the run to these conditions (repeatable or comma separated). "
            "Use when one client is unavailable and the other within-client "
            "comparison is still valid. Cross-client claims stay unavailable."
        ),
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
            raise BenchmarkError(f"Cannot find selected {name} binary")
    if not path.is_file() or not os.access(path, os.X_OK):
        raise BenchmarkError(f"Selected {name} binary is not an executable file")
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
    if any(term in text for term in ("usage limit", "rate limit", "rate_limit", "quota", "retry-after")):
        return "rate_limit"
    if any(term in text for term in ("not logged in", "authentication", "unauthorized", "login required", "oauth_org_not_allowed")):
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
        proc.wait(timeout=0.2)
    except subprocess.TimeoutExpired:
        pass
    # The leader may exit on TERM while an owned descendant ignores it.
    try:
        os.killpg(proc.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    proc.wait(timeout=3)


def strict_event(line: str):
    def pairs(items):
        value = {}
        for key, item in items:
            if key in value:
                raise ValueError("duplicate event field")
            value[key] = item
        return value

    def invalid_constant(_value):
        raise ValueError("nonfinite event value")

    value = json.loads(line, object_pairs_hook=pairs, parse_constant=invalid_constant)
    if not isinstance(value, dict) or not isinstance(value.get("type"), str):
        raise ValueError("event is not a typed object")
    return value


def execute(condition: dict, timeout: float, expected: str, *, version_probe=False) -> dict:
    started = time.monotonic()
    first_line = first_partial = completed_message = terminal_time = None
    answer = None
    terminal = False
    malformed = rejected = False
    availability_failure = None
    init_seen = False
    mcp_count = hosted_count = None
    observed_tool = False
    observed_mcp = False
    lines, stderr_parts = [], []
    client = condition.get("client", condition["name"].split("_", 1)[0])
    proc = None
    code = None
    timed_out = False
    exit_observed = False
    events = queue.Queue(maxsize=64)
    stop_reading = threading.Event()
    readers = []
    output_limit = False
    cleanup_ok = True

    def publish(event):
        while not stop_reading.is_set():
            try:
                events.put(event, timeout=0.05)
                return
            except queue.Full:
                continue

    def read_stream(name, stream):
        count = 0
        try:
            while not stop_reading.is_set():
                raw_line = stream.readline(MAX_LINE_CHARS + 1)
                if not raw_line:
                    break
                count += len(raw_line)
                if count > MAX_OUTPUT_CHARS or len(raw_line) > MAX_LINE_CHARS:
                    publish(("output_limit", time.monotonic(), None))
                    return
                publish((name, time.monotonic(), raw_line))
        except (OSError, UnicodeError):
            publish(("invalid", time.monotonic(), None))
        finally:
            # A reader that outlives the bounded join still owns its eventual close.
            stream.close()
            publish((name, time.monotonic(), None))

    try:
        proc = subprocess.Popen(
            condition["command"], cwd=condition["cwd"], env=condition["env"],
            stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            text=True, bufsize=1, start_new_session=True)
        readers = [
            threading.Thread(target=read_stream, args=("stdout", proc.stdout), daemon=True),
            threading.Thread(target=read_stream, args=("stderr", proc.stderr), daemon=True)]
        for reader in readers:
            reader.start()
        closed_streams = set()
        deadline = started + timeout
        while len(closed_streams) < 2 or proc.poll() is None:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                timed_out = True
                break
            try:
                stream_name, now, raw = events.get(timeout=min(0.05, remaining))
            except queue.Empty:
                continue
            if stream_name == "output_limit":
                output_limit = True
                break
            if stream_name == "invalid":
                malformed = True
                continue
            if raw is None:
                closed_streams.add(stream_name)
                continue
            if stream_name == "stderr":
                stderr_parts.append(raw)
                continue
            if not raw.strip():
                continue
            first_line = first_line or now
            lines.append(raw)
            if version_probe:
                continue
            try:
                event = strict_event(raw)
                kind = event["type"]
                if event.get("error") == "rate_limit" or (kind == "result" and event.get("api_error_status") == 429):
                    availability_failure = "rate_limit"
                elif event.get("error") in ("authentication_failed", "oauth_org_not_allowed"):
                    availability_failure = "auth_failure"
                if kind in {"error", "turn.failed"} or event.get("error"):
                    rejected = True
                if kind == "system" and event.get("subtype") == "init":
                    servers, tools = event.get("mcp_servers"), event.get("tools")
                    if (not isinstance(servers, list) or not isinstance(tools, list)
                            or any(not isinstance(item, dict) for item in servers)
                            or any(not isinstance(item, str) for item in tools)):
                        raise ValueError("malformed init")
                    init_seen = True
                    mcp_count = len(servers)
                    hosted_count = sum(name.startswith("mcp__") for name in tools)
                    if mcp_count or hosted_count:
                        observed_mcp = True
                if kind == "stream_event":
                    nested = event.get("event")
                    if not isinstance(nested, dict):
                        raise ValueError("malformed stream event")
                    if nested.get("type") == "content_block_start":
                        block = nested.get("content_block")
                        if not isinstance(block, dict):
                            raise ValueError("malformed partial content block")
                        if block.get("type") == "tool_use":
                            if not isinstance(block.get("name"), str):
                                raise ValueError("malformed partial tool name")
                            observed_tool = True
                            observed_mcp |= block["name"].startswith("mcp__")
                    delta = nested.get("delta", {})
                    if not isinstance(delta, dict):
                        raise ValueError("malformed delta")
                    if delta.get("type") == "text_delta":
                        if not isinstance(delta.get("text"), str):
                            raise ValueError("malformed text delta")
                        if delta["text"]:
                            first_partial = first_partial or now
                if kind == "assistant":
                    message = event.get("message")
                    if not isinstance(message, dict) or not isinstance(message.get("content"), list):
                        raise ValueError("malformed assistant")
                    content = message["content"]
                    if any(not isinstance(item, dict) for item in content):
                        raise ValueError("malformed content")
                    if any(item.get("type") == "tool_use" for item in content):
                        observed_tool = True
                        for item in content:
                            if item.get("type") == "tool_use":
                                if not isinstance(item.get("name"), str):
                                    raise ValueError("malformed tool name")
                                observed_mcp |= item["name"].startswith("mcp__")
                    text_parts = [item.get("text") for item in content if item.get("type") == "text"]
                    if any(not isinstance(text, str) for text in text_parts):
                        raise ValueError("malformed text")
                    # Child-agent messages are not this request's final answer.
                    if text_parts and event.get("parent_tool_use_id") is None:
                        if terminal:
                            raise ValueError("assistant after terminal result")
                        answer, completed_message = "".join(text_parts), now
                if kind in {"item.started", "item.updated", "item.completed"}:
                    item = event.get("item")
                    if not isinstance(item, dict):
                        raise ValueError("malformed item")
                    if item.get("type") in {"mcp_tool_call", "command_execution", "web_search", "file_change"}:
                        observed_tool = True
                    if item.get("type") == "mcp_tool_call":
                        observed_mcp = True
                    if kind == "item.completed" and item.get("type") == "agent_message":
                        if not isinstance(item.get("text"), str) or terminal:
                            raise ValueError("malformed agent message")
                        answer, completed_message = item["text"], now
                if kind == "result":
                    if client != "claude":
                        raise ValueError("wrong client terminal")
                    origin = event.get("origin")
                    if origin is not None and (not isinstance(origin, dict) or origin.get("kind") != "human"):
                        raise ValueError("result is not for the original user prompt")
                    good = (event.get("subtype") == "success" and event.get("is_error") is False
                            and event.get("terminal_reason", "completed") == "completed"
                            and event.get("api_error_status") is None
                            and event.get("deferred_tool_use") is None)
                    if not good:
                        rejected = True
                    if not isinstance(event.get("result"), str) or event["result"] != answer or terminal:
                        raise ValueError("result does not match completed message")
                    terminal, terminal_time = good, now
                if kind == "turn.completed":
                    if client != "codex" or terminal:
                        raise ValueError("wrong client or repeated terminal")
                    terminal, terminal_time = True, now
            except (ValueError, TypeError, RecursionError):
                malformed = True
        code = proc.poll()
        exit_observed = code is not None
        ended = time.monotonic()
    except OSError:
        ended = time.monotonic()
        code = None
    finally:
        if proc is not None:
            try:
                terminate_process_group(proc)
            except (OSError, subprocess.SubprocessError):
                cleanup_ok = False
            stop_reading.set()
            code = proc.returncode
            for reader, stream in zip(readers, (proc.stdout, proc.stderr)):
                reader.join(timeout=1)
                if reader.is_alive():
                    cleanup_ok = False
                else:
                    stream.close()
            # An earlier reader can finish while the later reader is joined.
            for reader, stream in zip(readers, (proc.stdout, proc.stderr)):
                if not reader.is_alive() and not stream.closed:
                    stream.close()
    stderr = "".join(stderr_parts)
    exposure = "unobserved"
    isolation_required = condition.get("require_no_tools", False)
    if init_seen:
        exposure = "contradicted" if observed_mcp else "observed_no_mcp"
    elif observed_mcp:
        exposure = "contradicted"
    isolation_failure = isolation_required and (
        observed_mcp or (client == "claude" and not init_seen))
    valid = (code == 0 and (version_probe or (answer == expected and completed_message is not None and terminal))
             and not timed_out and not rejected and not malformed
             and not isolation_failure and not output_limit and cleanup_ok)
    failure = classify(stderr, lines, code, timed_out)
    status = ("success" if valid else "cleanup_failure" if not cleanup_ok else
              "output_limit" if output_limit else "timeout" if timed_out else
              "isolation_failure" if isolation_failure else
              availability_failure if availability_failure else
              failure if failure in {"auth_failure", "rate_limit"} else
              "provider_rejection" if rejected else "bad_output" if malformed else failure)
    result = {
        "condition": condition["name"], "status": status, "exit_code": code,
        "answer_valid": answer == expected, "terminal_success": terminal,
        "first_stdout_line_s": first_line - started if first_line is not None else None,
        "first_partial_content_s": first_partial - started if first_partial is not None else None,
        "completed_message_s": completed_message - started if completed_message is not None else None,
        "terminal_success_s": terminal_time - started if terminal_time is not None else None,
        "total_s": ended - started if exit_observed else None,
        "elapsed_until_stop_s": ended - started,
        "isolation": {"runtime_evidence": exposure, "init_observed": init_seen,
                      "mcp_server_count": mcp_count, "mcp_tool_count": hosted_count,
                      "tool_use_observed": observed_tool},
        "diagnostic": None if valid else status,
        "cleanup_complete": cleanup_ok,
    }
    if version_probe:
        result["version_text"] = "".join(lines)
    return result


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
                "median_completed_message_delta_s": (
                    configured_completed - clean_completed
                    if configured_completed is not None and clean_completed is not None else None),
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
                if pair[f"{client}_configured"]["completed_message_s"] is not None
                and pair[f"{client}_clean"]["completed_message_s"] is not None
            ]
            total_deltas = [
                pair[f"{client}_configured"]["total_s"]
                - pair[f"{client}_clean"]["total_s"]
                for pair in pairs
            ]
            result[f"{client}_configuration_association"].update({
                "paired_blocks": len(pairs),
                "paired_completed_message_blocks": len(completed_deltas),
                "paired_completed_message_delta_median_s": statistics.median(completed_deltas) if completed_deltas else None,
                "paired_total_delta_median_s": statistics.median(total_deltas) if total_deltas else None,
            })
    return result


def selected_conditions(args):
    names = ["claude_clean", "claude_configured", "codex_clean", "codex_configured"]
    if args.claude_mcp_ablation:
        names.append("claude_configured_no_mcp")
    if args.only:
        requested = [name.strip() for item in args.only for name in item.split(",")]
        if any(not name or name not in names for name in requested):
            raise BenchmarkError("Unknown or empty selected condition")
        names = [name for name in names if name in requested]
    if args.runs < 1 or not math.isfinite(args.timeout) or args.timeout <= 0:
        raise BenchmarkError("Require positive run count and finite positive timeout")
    if not args.expected:
        raise BenchmarkError("Expected answer must be nonempty")
    for client in {name.split("_", 1)[0] for name in names}:
        for field in ("model", "effort"):
            value = getattr(args, client + "_" + field)
            if not value or any(ord(char) < 32 or ord(char) == 127 for char in value):
                raise BenchmarkError("Require nonempty control-free model and effort for each selected client")
    if "claude" in {name.split("_", 1)[0] for name in names}:
        if args.claude_effort not in {"low", "medium", "high", "xhigh", "max"}:
            raise BenchmarkError("Unsupported Claude effort value")
    return names


def check_auth_route(names):
    # Inspect presence only. Never serialize, hash, copy, or fetch credential values.
    if "claude_clean" in names:
        incompatible = {
            "ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN", "ANTHROPIC_BASE_URL",
            "ANTHROPIC_PROFILE", "CLAUDE_CONFIG_DIR", "CLAUDE_CODE_OAUTH_TOKEN",
            "CLAUDE_CODE_USE_BEDROCK", "CLAUDE_CODE_USE_VERTEX", "CLAUDE_CODE_USE_FOUNDRY",
        }
        if any(os.environ.get(name) for name in incompatible):
            raise BenchmarkError("Claude clean cannot preserve the selected ambient auth/provider route")
    if "codex_clean" in names:
        if not os.environ.get("CODEX_API_KEY"):
            raise BenchmarkError("Codex clean requires an already-supplied CODEX_API_KEY; auth-file copying is unsupported")
        if any(os.environ.get(name) for name in (
                "OPENAI_API_KEY", "OPENAI_AUTH_TOKEN", "OPENAI_BASE_URL", "CODEX_ACCESS_TOKEN",
                "OPENAI_ORG_ID", "OPENAI_PROJECT_ID")):
            raise BenchmarkError("Codex clean has conflicting ambient auth/provider selectors")


def build_conditions(args, project, root, binaries, names, *, preview=False):
    work, clean_home = root / "work", root / "home"
    base_env = {
        "HOME": str(Path.home()), "USER": os.environ.get("USER", ""),
        "LOGNAME": os.environ.get("LOGNAME", ""), "SHELL": "/bin/zsh",
        "PATH": "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin", "TMPDIR": "/tmp",
        "LANG": os.environ.get("LANG", "C.UTF-8"),
    }
    for name, value in (() if preview else os.environ.items()):
        if name.upper().endswith("_PROXY") or name in {
                "NO_PROXY", "SSL_CERT_FILE", "SSL_CERT_DIR", "REQUESTS_CA_BUNDLE"}:
            base_env[name] = value
    configured_env = {} if preview else dict(os.environ)
    conditions = {}
    for name in names:
        client = name.split("_", 1)[0]
        env = dict(configured_env)
        cwd = str(project)
        no_tools = name.endswith("_clean") or name == "claude_configured_no_mcp"
        if client == "claude":
            env.pop("CODEX_API_KEY", None)
            command = [binaries[client]]
            if name == "claude_clean":
                command += ["--safe-mode", "--strict-mcp-config", "--no-chrome"]
                env, cwd = dict(base_env), str(work)
            elif name == "claude_configured_no_mcp":
                command += ["--strict-mcp-config"]
            command += [
                "--no-session-persistence", "--prompt-suggestions", "false",
                "--model", args.claude_model, "--effort", args.claude_effort,
                "--verbose", "-p", "--output-format", "stream-json",
                "--include-partial-messages", "--", args.prompt]
        else:
            command = [binaries[client], "exec"]
            if name == "codex_clean":
                command += [
                    "--ignore-user-config", "--ignore-rules", "--skip-git-repo-check",
                    "--disable", "hooks", "--disable", "plugins", "--disable", "apps",
                    "--disable", "multi_agent", "--disable", "workspace_dependencies",
                    "--disable", "skill_search"]
                env = {**base_env, "HOME": str(clean_home), "CODEX_HOME": str(clean_home / ".codex")}
                # Official single-invocation exec auth; never copied into metadata.
                if not preview and os.environ.get("CODEX_API_KEY"):
                    env["CODEX_API_KEY"] = os.environ["CODEX_API_KEY"]
                cwd = str(work)
            command += [
                "--ephemeral", "-s", "read-only", "-c", 'approval_policy="never"',
                "-m", args.codex_model, "-c", "model_reasoning_effort=" + json.dumps(args.codex_effort),
                "--json", "--", args.prompt]
        conditions[name] = {"name": name, "client": client, "cwd": cwd, "env": env,
                            "command": command, "require_no_tools": no_tools}
    return conditions


def version(binary, client, condition, timeout):
    metadata_env = dict(condition["env"])
    metadata_env.pop("CODEX_API_KEY", None)
    row = execute({"name": client + "_version", "client": client, "cwd": condition["cwd"],
                   "env": metadata_env, "command": [binary, "--version"]},
                  min(5, timeout), "", version_probe=True)
    if row["status"] != "success":
        raise BenchmarkError("Selected " + client + " version probe did not complete")
    value = row["version_text"].strip()
    if not re.fullmatch(r"(?:codex-cli )?\d+\.\d+\.\d+(?:[-+][A-Za-z0-9.-]+)?(?: \(Claude Code\))?", value):
        raise BenchmarkError("Selected " + client + " version format is unrecognized")
    return value


def output_target(path):
    if path is None:
        return None
    if not path.is_absolute():
        raise BenchmarkError("Output must be a new absolute path outside Drive")
    parent = path.parent.resolve(strict=True)
    target = parent / path.name
    drive = (Path.home() / "My Drive").resolve()
    if target == drive or drive in target.parents or os.path.lexists(target):
        raise BenchmarkError("Output must be a new path outside Drive; existing paths are preserved")
    if not parent.is_dir():
        raise BenchmarkError("Output parent must be an existing directory")
    return target


def write_report(path, report):
    if path is None:
        return
    descriptor, temporary = tempfile.mkstemp(prefix=".ai-cli-report-", dir=path.parent)
    staged = Path(temporary)
    identity = os.fstat(descriptor)
    try:
        with os.fdopen(descriptor, "w") as stream:
            json.dump(report, stream, indent=2)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        # Atomic no-clobber publication; never follow or replace an existing target.
        os.link(staged, path)
    finally:
        current = staged.lstat()
        if (current.st_dev, current.st_ino) == (identity.st_dev, identity.st_ino):
            staged.unlink()


def main() -> int:
    args = parse_args()
    report = {"schema_version": 2, "status": "unavailable", "metadata": {},
              "preflight": [], "attempts": [], "summary": {}}
    target = None
    try:
        target = output_target(args.output)
        names = selected_conditions(args)
        project = args.project_dir.expanduser().resolve()
        if not project.is_dir():
            raise BenchmarkError("Configured project directory does not exist")
        if args.prompt is None:
            args.prompt = f"Reply with exactly {args.expected} and nothing else."
        clients = sorted({name.split("_", 1)[0] for name in names})
        binaries = {client: native_binary(getattr(args, client + "_bin"), client) for client in clients}
        metadata = {
            "binaries": binaries, "conditions": names, "project_dir": str(project),
            "runs": args.runs, "timeout_s": args.timeout, "seed": args.seed,
            "prompt": args.prompt, "expected": args.expected,
            "models": {client: getattr(args, client + "_model") for client in clients},
            "effort": {client: getattr(args, client + "_effort") for client in clients},
            "identity_provider_tier_verified": False,
            "limitations": [
                "Timing streams do not verify account, provider, model revision or service tier.",
                "Clean and configured arms also differ in cwd, environment and local state.",
                "Codex JSON does not expose a complete runtime tool inventory.",
                "Success-only summaries exclude failures; all attempted samples remain recorded.",
                "Fresh processes are not proof of cold caches or causal configuration effects.",
                "total_s observes exit after pipe delivery/parsing, excludes cleanup, may include lag, and is null if exit was not observed before cleanup.",
            ],
        }
        report["metadata"] = metadata
        if args.dry_run:
            conditions = build_conditions(args, project, Path("/tmp/ai-cli-profile-PLANNED"), binaries, names, preview=True)
            report.update(status="dry_run", auth_route_checked=False,
                          commands={name: condition["command"] for name, condition in conditions.items()})
        else:
            check_auth_route(names)
            with tempfile.TemporaryDirectory(prefix="ai-cli-profile-", dir="/tmp") as temp_name:
                root = Path(temp_name)
                root.chmod(0o700)
                (root / "work").mkdir()
                (root / "home" / ".codex").mkdir(parents=True, mode=0o700)
                conditions = build_conditions(args, project, root, binaries, names)
                metadata["versions"] = {
                    client: version(binaries[client], client, next(
                        condition for condition in conditions.values() if condition["client"] == client),
                        args.timeout) for client in clients}
                for condition in conditions.values():
                    row = execute(condition, args.timeout, args.expected)
                    row["phase"] = "preflight"
                    report["preflight"].append(row)
                    if row["status"] != "success":
                        break
                if any(row["status"] != "success" for row in report["preflight"]):
                    report["reason"] = "Selected condition preflight failed; no measured samples started"
                else:
                    rng = random.Random(args.seed)
                    stop = False
                    for iteration in range(1, args.runs + 1):
                        order = names[:]
                        rng.shuffle(order)
                        for name in order:
                            row = execute(conditions[name], args.timeout, args.expected)
                            row.update(phase="measured", iteration=iteration)
                            report["attempts"].append(row)
                            if row["status"] in {"auth_failure", "rate_limit", "isolation_failure", "cleanup_failure"}:
                                stop = True
                                break
                        if stop:
                            break
                    rows = report["attempts"]
                    report["summary"] = summarize(rows)
                    report["status"] = ("complete" if len(rows) == args.runs * len(names)
                                        and all(row["status"] == "success" for row in rows)
                                        else "inconclusive")
                    if stop:
                        report.update(status="unavailable", reason="Measured comparison stopped after an availability/isolation failure")
    except BenchmarkError as error:
        report.update(status="unavailable", reason=str(error))
    except (OSError, ValueError, subprocess.SubprocessError):
        report.update(status="unavailable", reason="Local preflight, process cleanup or report preparation failed")
    try:
        write_report(target, report)
    except (OSError, ValueError):
        report.update(status="unavailable", output_error="Private output publication failed; existing targets were preserved")
    print("FINAL_JSON=" + json.dumps(report))
    return 0 if report["status"] in {"complete", "dry_run"} else 2


if __name__ == "__main__":
    sys.exit(main())
