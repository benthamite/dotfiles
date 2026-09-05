#!/usr/bin/env python3
"""Audit evidence of Codex skill invocations from rollout JSONL files.

This collector reconstructs evidence from supported rollout reads and assistant
announcements, rather than native invocation telemetry. A confirmed invocation
needs a matching announcement, a read-like tool call for the skill's main
``SKILL.md``, and returned frontmatter for that skill. Weaker evidence is reported
separately and never added to the confirmed counts.
"""

from __future__ import annotations

import argparse
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime, timezone
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys
from typing import Any, Iterable


SCHEMA_VERSION = 1
REPO_ROOT = Path(__file__).resolve().parents[4]
DEFAULT_ROLLOUT_ROOTS = (
    Path.home() / ".codex" / "sessions",
    Path.home() / ".codex" / "archived_sessions",
)
DEFAULT_SKILL_ROOTS = (
    REPO_ROOT / ".codex" / "skills",
    REPO_ROOT / "codex" / "skills",
)
HUMAN_AMBIGUOUS_LIMIT = 25
SKILL_NAME_END = r"(?![A-Za-z0-9_@+-]|[.:](?=[A-Za-z0-9_.:@+-]))"
SKILL_PATH_RE = re.compile(
    r"(?:^|[./])skills/(?P<system>\.system/)?"
    r"(?P<skill>[A-Za-z0-9_.:@+-]+)/SKILL\.md"
)
PLUGIN_PATH_RE = re.compile(
    r"plugins/cache/[^/\s\"'`]+/"
    r"(?P<plugin>[^/\s\"'`]+)/[^/\s\"'`]+/skills/"
    r"(?P<skill>[A-Za-z0-9_.:@+-]+)/SKILL\.md"
)
AGENT_SKILL_CAT_RE = re.compile(
    r"(?:^|[/\s])agent-skill(?:['\"])?\s+cat\s+"
    r"(?P<skill>[A-Za-z0-9_.:@+-]+)"
)
READ_COMMAND_RE = re.compile(
    r"\b(?:cat|sed|awk|head|tail|bat|less|perl|ruby)(?:\s|$)"
)
FRONTMATTER_NAME_RE = re.compile(
    r"^---[ \t]*\r?\nname:[ \t]*['\"]?"
    r"(?P<name>[A-Za-z0-9_.:@+-]+)['\"]?[ \t]*$",
    re.MULTILINE,
)
NONZERO_EXIT_RE = re.compile(
    r"^(?:Process exited with code|exit(?:ed)?(?: with)?(?: status| code)?)\s+"
    r"(?P<code>[1-9][0-9]*)[ \t]*$",
    re.IGNORECASE | re.MULTILINE,
)
ROLLOUT_CALL_PATTERN = (
    r'(?:"type"\s*:\s*"(?:function_call|custom_tool_call)"[^\n]*'
    r'(?:skills/(?:\.system/)?[A-Za-z0-9_.:@+-]+/SKILL\.md|'
    r'agent-skill[^\n]*\s+cat\s+)'
    r'|(?:skills/(?:\.system/)?[A-Za-z0-9_.:@+-]+/SKILL\.md|'
    r'agent-skill[^\n]*\s+cat\s+)'
    r'[^\n]*"type"\s*:\s*"(?:function_call|custom_tool_call)")'
)

NEGATED_INVOCATION_RE = re.compile(
    r"\b(?:do|does|did|will|would|should|must|can|could)\s+not\b|"
    r"\b(?:don|doesn|didn|won|wouldn|shouldn|mustn|can|couldn)['’]t\b|"
    r"\b(?:never|without|against|avoid|avoiding)\b|"
    r"\bunder\s+no\s+circumstances\b|"
    r"\bno\s+(?:need|reason)\s+to\s+(?:use|invoke|apply|run)\b|"
    r"\brefrain(?:ing)?\s+from\b|\bstop(?:ping)?\b",
    re.IGNORECASE,
)
INFORMATION_QUESTION_RE = re.compile(
    r"^\s*(?:what|why|when|where|which|how|does|do|did|is|are|was|were|"
    r"should|can|could|would|will)\b",
    re.IGNORECASE,
)
POSITIVE_NEGATED_IDIOM_RE = re.compile(
    r"\b(?:do\s+not|don['’]t)\s+(?:forget|hesitate)\b",
    re.IGNORECASE,
)


@dataclass(frozen=True)
class SkillCandidate:
    """A skill main-file reference found in a tool call."""

    basename: str
    requested_name: str
    plugin: str | None
    path_hint: str


@dataclass(frozen=True)
class TimedText:
    """A message associated with one rollout turn."""

    timestamp: datetime
    text: str


@dataclass(frozen=True)
class ToolCall:
    """The parts of a rollout tool call used by this collector."""

    call_id: str
    name: str
    text: str
    timestamp: datetime
    timestamp_text: str
    turn_id: str


@dataclass(frozen=True)
class ToolOutput:
    """Text evidence and failure status retained from a tool result envelope."""

    text: str
    failed: bool = False


def parse_timestamp(value: str) -> datetime:
    """Parse an ISO-8601 timestamp and return an aware UTC datetime."""

    normalized = value.strip()
    if normalized.endswith("Z"):
        normalized = f"{normalized[:-1]}+00:00"
    parsed = datetime.fromisoformat(normalized)
    if parsed.tzinfo is None:
        raise ValueError("timestamp must include a timezone")
    return parsed.astimezone(timezone.utc)


def format_timestamp(value: datetime) -> str:
    """Return a stable ISO-8601 UTC timestamp."""

    return value.astimezone(timezone.utc).isoformat().replace("+00:00", "Z")


def content_text(content: Any) -> str:
    """Flatten text-bearing rollout content without exposing it in reports."""

    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return "\n".join(content_text(item) for item in content)
    if isinstance(content, dict):
        if isinstance(content.get("text"), str):
            return content["text"]
        if "output" in content:
            return content_text(content["output"])
    return ""


def call_text(payload: dict[str, Any]) -> str:
    """Extract the serialized input of either Codex tool-call generation."""

    value = payload.get("arguments")
    if value is None:
        value = payload.get("input")
    if isinstance(value, str):
        return value
    if value is None:
        return ""
    return json.dumps(value, sort_keys=True, separators=(",", ":"))


def tool_output(value: Any) -> ToolOutput:
    """Unpack text blocks and JSON execution envelopes without losing status."""

    if isinstance(value, str):
        if value.lstrip().startswith(("{", "[")):
            try:
                decoded = json.loads(value)
            except json.JSONDecodeError:
                decoded = None
            if isinstance(decoded, (dict, list)):
                return tool_output(decoded)
        # Legacy transports print status before their Output: delimiter or the
        # file's frontmatter. Exit examples inside the skill are content, not
        # transport status.
        header = re.split(r"(?m)^(?:Output:|---)[ \t]*\r?$", value, maxsplit=1)[0]
        return ToolOutput(value, bool(NONZERO_EXIT_RE.search(header)))
    if isinstance(value, list):
        parts = [tool_output(item) for item in value]
        return ToolOutput(
            "\n".join(part.text for part in parts),
            any(part.failed for part in parts),
        )
    if isinstance(value, dict):
        status = value.get("exit_code")
        failed = bool(value.get("isError")) or status not in (None, 0, "0")
        for key in ("output", "text", "content"):
            if key in value:
                nested = tool_output(value[key])
                return ToolOutput(nested.text, failed or nested.failed)
        return ToolOutput("", failed)
    return ToolOutput("")


def payload_turn_id(payload: dict[str, Any], fallback: str | None) -> str | None:
    """Resolve the turn identifier used across Codex rollout generations."""

    direct = payload.get("turn_id")
    if isinstance(direct, str) and direct:
        return direct
    metadata = payload.get("internal_chat_message_metadata_passthrough")
    if isinstance(metadata, dict):
        nested = metadata.get("turn_id")
        if isinstance(nested, str) and nested:
            return nested
    return fallback


def find_candidates(text: str) -> list[SkillCandidate]:
    """Find canonical skill main-file reads named in a tool call."""

    plugin_paths = list(PLUGIN_PATH_RE.finditer(text))

    candidates: dict[tuple[str, str | None, str], SkillCandidate] = {}
    for match in SKILL_PATH_RE.finditer(text):
        basename = match.group("skill")
        system = bool(match.group("system"))
        plugin = next(
            (
                path.group("plugin")
                for path in plugin_paths
                if path.start() <= match.start() and match.end() <= path.end()
            ),
            None,
        )
        requested = f"{plugin}:{basename}" if plugin else basename
        path_hint = (
            f"plugins/{plugin}/skills/{basename}/SKILL.md"
            if plugin
            else (
                f"skills/.system/{basename}/SKILL.md"
                if system
                else f"skills/{basename}/SKILL.md"
            )
        )
        candidate = SkillCandidate(
            basename=basename,
            requested_name=requested,
            plugin=plugin,
            path_hint=path_hint,
        )
        candidates[(basename, plugin, path_hint)] = candidate

    for match in AGENT_SKILL_CAT_RE.finditer(text):
        requested = match.group("skill")
        basename = requested.rsplit(":", 1)[-1]
        candidate = SkillCandidate(
            basename=basename,
            requested_name=requested,
            plugin=requested.split(":", 1)[0] if ":" in requested else None,
            path_hint=f"agent-skill:{requested}",
        )
        candidates[(basename, candidate.plugin, candidate.path_hint)] = candidate

    return sorted(
        candidates.values(),
        key=lambda item: (item.requested_name, item.path_hint),
    )


def is_read_like(text: str) -> bool:
    """Return whether a tool call is intended to return file contents."""

    return bool(AGENT_SKILL_CAT_RE.search(text) or READ_COMMAND_RE.search(text))


def frontmatter_names(output: str) -> set[str]:
    """Return skill names whose YAML frontmatter is present in tool output."""

    return {match.group("name") for match in FRONTMATTER_NAME_RE.finditer(output)}


def name_aliases(candidate: SkillCandidate, output_name: str) -> set[str]:
    """Return qualified and unqualified aliases for one skill."""

    aliases = {candidate.basename, candidate.requested_name, output_name}
    if candidate.plugin:
        aliases.add(f"{candidate.plugin}:{output_name}")
    return aliases


def skill_token(name: str) -> str:
    """Return a regex fragment for a named skill token."""

    # A final period or colon can be punctuation; '.extra' and ':extra' extend
    # the skill name instead.
    return rf"`?[$/]?{re.escape(name)}{SKILL_NAME_END}`?(?:\s+skill\b)?"


def user_invocation_phrase(name: str) -> re.Pattern[str]:
    """Build a pattern for a direct user request or command marker."""

    token = skill_token(name)
    boundary = r"(?:^|[.;,!?\n]\s*|\b(?:and|then)\s+)"
    action = r"(?:use|invoke|apply|run)"
    request = (
        rf"{boundary}(?:(?:please[\s,]+)?(?:explicitly\s+)?{action}\s+"
        rf"(?:the\s+)?{token}|"
        rf"(?:can|could|would|will)\s+you\s+(?:please\s+)?{action}\s+"
        rf"(?:the\s+)?{token}|"
        rf"(?:do\s+not|don['’]t)\s+(?:forget|hesitate)"
        rf"(?:\s+to|,\s*)\s*{action}\s+(?:the\s+)?{token}|"
        rf"i(?:'d|\s+would)?\s+(?:like|want|need)\s+you\s+to\s+{action}\s+"
        rf"(?:the\s+)?{token})"
    )
    marker = (
        rf"(?P<marker>[`]?[$/]{re.escape(name)}{SKILL_NAME_END}`?)"
        rf"(?=$|\s|[.,;:!?])"
    )
    return re.compile(rf"(?P<request>{request})|{marker}", re.IGNORECASE)


def assistant_invocation_phrase(name: str) -> re.Pattern[str]:
    """Build a pattern for a first-person assistant execution announcement."""

    token = skill_token(name)
    present = r"(?:\s+am|['’]m)\s+(?:using|invoking|applying|running)"
    future = (
        r"(?:(?:\s+will|['’]ll)\s+(?:use|invoke|apply|run)|"
        r"(?:\s+am|['’]m)\s+going\s+to\s+(?:use|invoke|apply|run))"
    )
    return re.compile(
        rf"(?:\bi(?:{present}|{future})\s+(?:the\s+)?{token}|"
        rf"(?:^|[.;,!?\n]\s*)(?:using|invoking|applying|running)\s+"
        rf"(?:the\s+)?{token})",
        re.IGNORECASE,
    )


def positive_user_match(text: str, match: re.Match[str]) -> bool:
    """Reject a request in a negative or information-only sentence."""

    clause_start = max(
        text.rfind(boundary, 0, match.start()) for boundary in ".;!?:\n"
    ) + 1
    following_boundaries = [
        position
        for boundary in ".;!?:\n"
        if (position := text.find(boundary, match.end())) != -1
    ]
    clause_end = min(following_boundaries) + 1 if following_boundaries else len(text)
    clause = text[clause_start:clause_end]
    prefix = text[clause_start:match.start()]
    if POSITIVE_NEGATED_IDIOM_RE.search(match.group(0)):
        return True
    if NEGATED_INVOCATION_RE.search(prefix):
        return False
    return not (
        match.lastgroup == "marker"
        and clause.rstrip().endswith("?")
        and INFORMATION_QUESTION_RE.search(clause)
    )


def matching_alias(
    messages: Iterable[TimedText],
    aliases: set[str],
    before: datetime,
    *,
    speaker: str,
) -> str | None:
    """Return the first alias explicitly invoked before a tool call."""

    for message in sorted(messages, key=lambda item: item.timestamp):
        if message.timestamp > before:
            continue
        for alias in sorted(aliases, key=lambda item: (-len(item), item)):
            pattern = (
                user_invocation_phrase(alias)
                if speaker == "user"
                else assistant_invocation_phrase(alias)
            )
            for match in pattern.finditer(message.text):
                if speaker != "user" or positive_user_match(message.text, match):
                    return alias
    return None


def mentioned_alias(
    messages: Iterable[TimedText], aliases: set[str], before: datetime
) -> str | None:
    """Return an alias named in a direct prompt without inferring its polarity."""

    for message in sorted(messages, key=lambda item: item.timestamp):
        if message.timestamp > before:
            continue
        for alias in sorted(aliases, key=lambda item: (-len(item), item)):
            escaped = re.escape(alias)
            if re.search(
                rf"(?<![A-Za-z0-9_.:@+-]){escaped}(?=$|\s|[.,;!?`])",
                message.text,
                re.IGNORECASE,
            ):
                return alias
    return None


def matching_output_name(
    candidate: SkillCandidate, names: set[str]
) -> str | None:
    """Match returned frontmatter to the folder or resolver name read."""

    matches = sorted(
        name
        for name in names
        if name == candidate.requested_name
        or name == candidate.basename
        or name.rsplit(":", 1)[-1] == candidate.basename
    )
    return matches[0] if matches else None


def catalog_skill_names(skill_roots: Iterable[Path]) -> set[str]:
    """Read immediate skill names from explicit personal catalog roots."""

    names: set[str] = set()
    for root in skill_roots:
        if not root.is_dir():
            raise FileNotFoundError(f"skill root does not exist: {root}")
        for skill_file in sorted(root.glob("*/SKILL.md"), key=str):
            names.add(skill_file.parent.name)
            try:
                header = skill_file.read_text(encoding="utf-8")[:4096]
            except OSError as error:
                raise OSError(f"cannot read skill catalog entry {skill_file}: {error}")
            matched = FRONTMATTER_NAME_RE.search(header)
            if matched:
                names.add(matched.group("name"))
    return names


def canonical_name(
    candidate: SkillCandidate, output_name: str, announced_alias: str | None
) -> str:
    """Prefer the qualified name that the assistant actually announced."""

    if announced_alias and ":" in announced_alias:
        return announced_alias
    if ":" in candidate.requested_name:
        return candidate.requested_name
    if candidate.plugin:
        return f"{candidate.plugin}:{output_name}"
    return output_name


def classify_invocation(
    user_messages: Iterable[TimedText],
    aliases: set[str],
    before: datetime,
) -> tuple[str, str, str]:
    """Classify explicit versus autonomous use without retaining user text."""

    messages = [message for message in user_messages if message.timestamp <= before]
    matched = matching_alias(messages, aliases, before, speaker="user")
    if matched:
        return "explicit", "high", f"direct user invocation marker for {matched}"
    mentioned = mentioned_alias(messages, aliases, before)
    if mentioned:
        return (
            "unknown",
            "low",
            f"direct user message named {mentioned} without a positive invocation marker",
        )
    if messages:
        return (
            "autonomous",
            "medium",
            "no explicit invocation marker in a direct user message",
        )
    return "unknown", "low", "no direct user message was available for this turn"


def discover_rollouts(inputs: Iterable[Path]) -> list[Path]:
    """Resolve explicit fixture files or rollout directories deterministically."""

    discovered: set[Path] = set()
    for source in inputs:
        if source.is_file():
            discovered.add(source.resolve())
        elif source.is_dir():
            discovered.update(path.resolve() for path in source.rglob("*.jsonl"))
        else:
            raise FileNotFoundError(f"rollout input does not exist: {source}")
    return sorted(discovered, key=str)


def candidate_rollouts(inputs: Iterable[Path]) -> list[Path]:
    """Use ripgrep to select rollouts that contain a candidate tool call.

    Historical rollout roots can be many gigabytes.  Selecting call-bearing files
    before JSON decoding avoids parsing developer skill catalogs in every session.
    Ripgrep is an explicit dependency because it performs this selection much
    faster than a Python fallback over the same data.
    """

    executable = shutil.which("rg")
    if executable is None:
        raise RuntimeError("ripgrep (rg) is required to scan rollout roots")
    command = [
        executable,
        "--no-config",
        "--hidden",
        "--no-ignore",
        "--files-with-matches",
        "--glob",
        "*.jsonl",
        "--regexp",
        ROLLOUT_CALL_PATTERN,
        "--",
        *(str(path.resolve()) for path in inputs),
    ]
    completed = subprocess.run(
        command,
        check=False,
        capture_output=True,
        text=True,
    )
    if completed.returncode not in {0, 1}:
        detail = completed.stderr.strip() or f"exit status {completed.returncode}"
        raise RuntimeError(f"ripgrep rollout selection failed: {detail}")
    return sorted(
        {Path(line).resolve() for line in completed.stdout.splitlines() if line},
        key=str,
    )


def scan_rollout(path: Path, cutoff: datetime) -> dict[str, Any]:
    """Scan one rollout file and retain only evidence before the cutoff."""

    session_id = path.stem
    calls: dict[str, ToolCall] = {}
    outputs: dict[str, ToolOutput] = {}
    user_messages: dict[str, list[TimedText]] = defaultdict(list)
    event_user_messages: dict[str, list[TimedText]] = defaultdict(list)
    assistant_messages: dict[str, list[TimedText]] = defaultdict(list)
    malformed_lines = 0
    missing_timestamps = 0

    # First select the few tool calls that can be invocation evidence.  Avoid
    # decoding every reasoning event and large tool result in multi-GB archives.
    with path.open(encoding="utf-8") as stream:
        for raw_line in stream:
            stripped = raw_line.lstrip()
            if not stripped.startswith("{"):
                malformed_lines += 1
                continue
            if '"timestamp"' not in raw_line:
                missing_timestamps += 1
                continue
            if "SKILL.md" not in raw_line and "agent-skill" not in raw_line:
                continue
            try:
                record = json.loads(raw_line)
            except json.JSONDecodeError:
                malformed_lines += 1
                continue
            timestamp_text = record.get("timestamp")
            if not isinstance(timestamp_text, str):
                missing_timestamps += 1
                continue
            try:
                timestamp = parse_timestamp(timestamp_text)
            except (TypeError, ValueError):
                malformed_lines += 1
                continue
            if timestamp >= cutoff:
                continue

            payload = record.get("payload")
            if not isinstance(payload, dict):
                payload = {}
            if record.get("type") != "response_item":
                continue
            payload_type = payload.get("type")
            if payload_type not in {"function_call", "custom_tool_call"}:
                continue
            call_id = payload.get("call_id")
            if not isinstance(call_id, str) or not call_id:
                continue
            text = call_text(payload)
            if not find_candidates(text):
                continue
            turn_id = payload_turn_id(payload, None)
            if not turn_id:
                # Legacy records normally carry metadata.  The second pass can
                # recover a missing ID from the active turn context.
                turn_id = ""
            name = payload.get("name")
            calls[call_id] = ToolCall(
                call_id=call_id,
                name=name if isinstance(name, str) else "",
                text=text,
                timestamp=timestamp,
                timestamp_text=format_timestamp(timestamp),
                turn_id=turn_id,
            )

    if not calls:
        return {
            "session_id": session_id,
            "calls": calls,
            "outputs": outputs,
            "user_messages": user_messages,
            "assistant_messages": assistant_messages,
            "malformed_lines": malformed_lines,
            "missing_timestamps": missing_timestamps,
        }

    call_ids = set(calls)
    call_id_pattern = re.compile(
        "|".join(re.escape(call_id) for call_id in sorted(call_ids))
    )
    current_turn: str | None = None
    with path.open(encoding="utf-8") as stream:
        for raw_line in stream:
            relevant = (
                "session_meta" in raw_line
                or "turn_context" in raw_line
                or "task_started" in raw_line
                or "user_message" in raw_line
                or ('"role"' in raw_line and ("user" in raw_line or "assistant" in raw_line))
                or call_id_pattern.search(raw_line)
            )
            if not relevant:
                continue
            try:
                record = json.loads(raw_line)
                timestamp_text = record.get("timestamp")
                if not isinstance(timestamp_text, str):
                    continue
                timestamp = parse_timestamp(timestamp_text)
            except (json.JSONDecodeError, TypeError, ValueError):
                continue
            if timestamp >= cutoff:
                continue
            payload = record.get("payload")
            if not isinstance(payload, dict):
                payload = {}
            record_type = record.get("type")
            if record_type == "session_meta":
                possible_id = payload.get("id")
                if isinstance(possible_id, str) and possible_id:
                    session_id = possible_id
                continue
            if record_type == "turn_context":
                possible_turn = payload.get("turn_id")
                if isinstance(possible_turn, str) and possible_turn:
                    current_turn = possible_turn
                continue
            if record_type == "event_msg":
                payload_type = payload.get("type")
                possible_turn = payload.get("turn_id")
                if payload_type == "task_started" and isinstance(possible_turn, str):
                    current_turn = possible_turn
                elif payload_type == "user_message" and current_turn:
                    text = payload.get("message")
                    if isinstance(text, str) and text:
                        event_user_messages[current_turn].append(
                            TimedText(timestamp, text)
                        )
                continue
            if record_type != "response_item":
                continue

            payload_type = payload.get("type")
            turn_id = payload_turn_id(payload, current_turn)
            if payload_type == "message" and turn_id:
                role = payload.get("role")
                text = content_text(payload.get("content"))
                if role == "user" and text:
                    user_messages[turn_id].append(TimedText(timestamp, text))
                elif role == "assistant" and text:
                    assistant_messages[turn_id].append(TimedText(timestamp, text))
                continue

            call_id = payload.get("call_id")
            if not isinstance(call_id, str) or call_id not in call_ids:
                continue
            if payload_type in {"function_call", "custom_tool_call"}:
                call = calls[call_id]
                if not call.turn_id and turn_id:
                    calls[call_id] = ToolCall(
                        call_id=call.call_id,
                        name=call.name,
                        text=call.text,
                        timestamp=call.timestamp,
                        timestamp_text=call.timestamp_text,
                        turn_id=turn_id,
                    )
            elif payload_type in {
                "function_call_output",
                "custom_tool_call_output",
            }:
                outputs[call_id] = tool_output(payload.get("output"))

    calls = {call_id: call for call_id, call in calls.items() if call.turn_id}
    for turn_id, messages in event_user_messages.items():
        # Event messages represent the direct prompt.  Role-user response items
        # can also contain injected AGENTS.md and environment blocks.
        if messages:
            user_messages[turn_id] = messages

    return {
        "session_id": session_id,
        "calls": calls,
        "outputs": outputs,
        "user_messages": user_messages,
        "assistant_messages": assistant_messages,
        "malformed_lines": malformed_lines,
        "missing_timestamps": missing_timestamps,
    }


def collect(
    inputs: Iterable[Path],
    cutoff: datetime,
    *,
    allowed_skills: set[str] | None = None,
    scope_label: str = "all skills",
) -> dict[str, Any]:
    """Collect confirmed invocations and ambiguous read evidence."""

    input_paths = [path.resolve() for path in inputs]
    discovered_paths = discover_rollouts(input_paths)
    rollout_paths = candidate_rollouts(input_paths)
    confirmed: dict[tuple[str, str], dict[str, Any]] = {}
    ambiguous: dict[tuple[str, str, str], dict[str, Any]] = {}
    malformed_lines = 0
    missing_timestamps = 0
    candidate_reads_seen = 0
    eligible_candidate_reads = 0
    ignored_candidate_reads = 0
    ignored_skills: set[str] = set()

    for path in rollout_paths:
        scan = scan_rollout(path, cutoff)
        malformed_lines += scan["malformed_lines"]
        missing_timestamps += scan["missing_timestamps"]
        session_id = scan["session_id"]
        for call in sorted(
            scan["calls"].values(), key=lambda item: (item.timestamp, item.call_id)
        ):
            candidates = find_candidates(call.text)
            if not candidates:
                continue
            result = scan["outputs"].get(call.call_id, ToolOutput(""))
            output = result.text
            names = frontmatter_names(output)
            read_like = is_read_like(call.text)
            failed = result.failed

            for candidate in candidates:
                candidate_reads_seen += 1
                output_name = matching_output_name(candidate, names)
                initial_aliases = {candidate.basename, candidate.requested_name}
                if output_name:
                    initial_aliases.update(name_aliases(candidate, output_name))
                announced = matching_alias(
                    scan["assistant_messages"].get(call.turn_id, []),
                    initial_aliases,
                    call.timestamp,
                    speaker="assistant",
                )
                if output_name:
                    aliases = name_aliases(candidate, output_name)
                else:
                    aliases = {candidate.basename, candidate.requested_name}
                if announced:
                    aliases.add(announced)
                skill = canonical_name(
                    candidate,
                    output_name or candidate.basename,
                    announced,
                )
                eligible_names = {
                    skill,
                    candidate.basename,
                    candidate.requested_name,
                    *(aliases or set()),
                }
                if candidate.plugin:
                    # A plugin can share its basename with a personal skill.
                    # Only an explicitly qualified catalog entry admits it.
                    eligible_names = {
                        name for name in eligible_names if ":" in name
                    }
                if allowed_skills is not None and not (eligible_names & allowed_skills):
                    ignored_candidate_reads += 1
                    ignored_skills.add(skill)
                    continue
                eligible_candidate_reads += 1

                if read_like and output_name and announced and not failed:
                    classification, class_confidence, class_evidence = (
                        classify_invocation(
                            scan["user_messages"].get(call.turn_id, []),
                            aliases,
                            call.timestamp,
                        )
                    )
                    key = (call.turn_id, skill)
                    evidence = confirmed.get(key)
                    if evidence is None:
                        evidence = {
                            "skill": skill,
                            "session_id": session_id,
                            "session_ids": [],
                            "turn_id": call.turn_id,
                            "timestamp": call.timestamp_text,
                            "classification": classification,
                            "classification_confidence": class_confidence,
                            "classification_evidence": class_evidence,
                            "invocation_confidence": "high",
                            "evidence_paths": [],
                            "call_ids": [],
                        }
                        confirmed[key] = evidence
                    evidence["evidence_paths"].append(candidate.path_hint)
                    evidence["call_ids"].append(call.call_id)
                    evidence["session_ids"].append(session_id)
                    evidence["session_id"] = min(evidence["session_ids"])
                    classification_rank = {
                        "unknown": 0,
                        "autonomous": 1,
                        "explicit": 2,
                    }
                    if classification_rank[classification] > classification_rank[
                        evidence["classification"]
                    ]:
                        evidence["classification"] = classification
                        evidence["classification_confidence"] = class_confidence
                        evidence["classification_evidence"] = class_evidence
                    if call.timestamp < parse_timestamp(evidence["timestamp"]):
                        evidence["timestamp"] = call.timestamp_text
                    continue

                if not read_like:
                    confidence = "low"
                    reason = "main SKILL.md reference was not in a read-like call"
                elif failed:
                    confidence = "low"
                    reason = "main SKILL.md read returned a nonzero exit status"
                elif not output:
                    confidence = "low"
                    reason = "main SKILL.md read had no pre-cutoff tool output"
                elif not output_name:
                    confidence = "medium"
                    reason = "tool output did not contain matching skill frontmatter"
                else:
                    confidence = "medium"
                    reason = (
                        "successful main SKILL.md read lacked a matching assistant "
                        "invocation announcement"
                    )
                key = (call.turn_id, skill, reason)
                evidence = ambiguous.get(key)
                if evidence is None:
                    evidence = {
                        "skill": skill,
                        "session_id": session_id,
                        "session_ids": [],
                        "turn_id": call.turn_id,
                        "timestamp": call.timestamp_text,
                        "confidence": confidence,
                        "reason": reason,
                        "evidence_paths": [],
                        "call_ids": [],
                    }
                    ambiguous[key] = evidence
                evidence["evidence_paths"].append(candidate.path_hint)
                evidence["call_ids"].append(call.call_id)
                evidence["session_ids"].append(session_id)
                evidence["session_id"] = min(evidence["session_ids"])
                if call.timestamp < parse_timestamp(evidence["timestamp"]):
                    evidence["timestamp"] = call.timestamp_text

    invocations = sorted(
        confirmed.values(),
        key=lambda item: (parse_timestamp(item["timestamp"]), item["session_id"], item["skill"]),
    )
    ambiguous_reads = sorted(
        ambiguous.values(),
        key=lambda item: (
            parse_timestamp(item["timestamp"]),
            item["session_id"],
            item["skill"],
            item["reason"],
        ),
    )
    for item in [*invocations, *ambiguous_reads]:
        item["evidence_paths"] = sorted(set(item["evidence_paths"]))
        item["call_ids"] = sorted(set(item["call_ids"]))
        item["session_ids"] = sorted(set(item["session_ids"]))

    totals: dict[str, dict[str, int]] = defaultdict(
        lambda: {"total": 0, "explicit": 0, "autonomous": 0, "unknown": 0}
    )
    for invocation in invocations:
        bucket = totals[invocation["skill"]]
        bucket["total"] += 1
        bucket[invocation["classification"]] += 1
    by_skill = [
        {"skill": skill, **totals[skill]}
        for skill in sorted(totals, key=lambda name: (-totals[name]["total"], name))
    ]

    return {
        "schema_version": SCHEMA_VERSION,
        "cutoff_exclusive": format_timestamp(cutoff),
        "scope": {
            "label": scope_label,
            "catalog_skill_count": (
                None if allowed_skills is None else len(allowed_skills)
            ),
            "ignored_candidate_reads": ignored_candidate_reads,
            "ignored_skills": sorted(ignored_skills),
        },
        "inputs": [str(path) for path in input_paths],
        "sources": [str(path) for path in rollout_paths],
        "summary": {
            "rollout_files_considered": len(discovered_paths),
            "rollout_files_scanned": len(rollout_paths),
            "confirmed_invocations": len(invocations),
            "ambiguous_reads": len(ambiguous_reads),
            "malformed_lines": malformed_lines,
            "records_without_timestamps": missing_timestamps,
            "candidate_reads_seen": candidate_reads_seen,
            "eligible_candidate_reads": eligible_candidate_reads,
            "ignored_candidate_reads": ignored_candidate_reads,
            "by_skill": by_skill,
        },
        "invocations": invocations,
        "ambiguous_reads": ambiguous_reads,
    }


def render_human(report: dict[str, Any]) -> str:
    """Render a compact deterministic report without transcript text."""

    summary = report["summary"]
    lines = [
        f"Cutoff (exclusive): {report['cutoff_exclusive']}",
        f"Scope: {report['scope']['label']}",
        f"Rollout files considered: {summary['rollout_files_considered']}",
        f"Rollout files scanned: {summary['rollout_files_scanned']}",
        f"Confirmed invocations: {summary['confirmed_invocations']}",
        f"Ambiguous reads: {summary['ambiguous_reads']}",
        f"Ignored out-of-scope candidate reads: {summary['ignored_candidate_reads']}",
        "",
        "Confirmed counts by skill:",
    ]
    if summary["by_skill"]:
        for item in summary["by_skill"]:
            lines.append(
                f"  {item['skill']}: {item['total']} "
                f"(explicit {item['explicit']}, autonomous {item['autonomous']}, "
                f"unknown {item['unknown']})"
            )
    else:
        lines.append("  none")

    if report["ambiguous_reads"]:
        lines.extend(("", "Ambiguous evidence:"))
        for item in report["ambiguous_reads"][:HUMAN_AMBIGUOUS_LIMIT]:
            lines.append(
                f"  {item['timestamp']} {item['skill']} [{item['confidence']}]: "
                f"{item['reason']}"
            )
        omitted = len(report["ambiguous_reads"]) - HUMAN_AMBIGUOUS_LIMIT
        if omitted > 0:
            lines.append(
                f"  ... {omitted} more; use --format json for complete evidence"
            )
    if summary["malformed_lines"] or summary["records_without_timestamps"]:
        lines.extend(
            (
                "",
                "Input diagnostics:",
                f"  malformed lines: {summary['malformed_lines']}",
                "  records without timestamps: "
                f"{summary['records_without_timestamps']}",
            )
        )
    return "\n".join(lines) + "\n"


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--cutoff",
        required=True,
        help="exclusive ISO-8601 cutoff; record this before starting the audit",
    )
    parser.add_argument(
        "--skill-root",
        action="append",
        type=Path,
        dest="skill_roots",
        help=(
            "personal skill catalog root; repeat as needed (default: the "
            "dotfiles .codex/skills and codex/skills roots)"
        ),
    )
    parser.add_argument(
        "--skill",
        action="append",
        dest="skills",
        help="exact skill name to include; repeat to replace catalog-root scope",
    )
    parser.add_argument(
        "--include-all",
        action="store_true",
        help="include plugin and system skills instead of the personal catalog",
    )
    parser.add_argument(
        "--input",
        action="append",
        type=Path,
        dest="inputs",
        help=(
            "rollout JSONL file or directory; repeat as needed "
            "(default: active and archived Codex rollout roots)"
        ),
    )
    parser.add_argument(
        "--format",
        choices=("human", "json"),
        default="human",
        help="output format (default: human)",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    """Run the read-only collector CLI."""

    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        cutoff = parse_timestamp(args.cutoff)
    except (TypeError, ValueError) as error:
        parser.error(f"invalid --cutoff: {error}")
    inputs = args.inputs if args.inputs is not None else list(DEFAULT_ROLLOUT_ROOTS)
    try:
        if args.include_all:
            allowed_skills = None
            scope_label = "all skills"
        elif args.skills:
            allowed_skills = set(args.skills)
            scope_label = f"explicit skill list ({len(allowed_skills)} names)"
        else:
            skill_roots = (
                args.skill_roots
                if args.skill_roots is not None
                else list(DEFAULT_SKILL_ROOTS)
            )
            allowed_skills = catalog_skill_names(skill_roots)
            scope_label = f"personal catalog ({len(allowed_skills)} names)"
        report = collect(
            inputs,
            cutoff,
            allowed_skills=allowed_skills,
            scope_label=scope_label,
        )
    except (OSError, RuntimeError) as error:
        parser.error(str(error))
    if args.format == "json":
        json.dump(report, sys.stdout, indent=2, sort_keys=True)
        sys.stdout.write("\n")
    else:
        sys.stdout.write(render_human(report))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
