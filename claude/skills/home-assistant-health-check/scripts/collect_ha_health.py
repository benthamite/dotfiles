#!/usr/bin/env python3
"""Collect Home Assistant health signals without mutating the instance."""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import os
import re
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Any


DEFAULT_HA_URL = "http://homeassistant.local:8123"
DEFAULT_STATE_DIR = Path.home() / ".local/state/ha-health-check"
DEFAULT_CRITICAL_ENTITIES = [
    "water_heater.peugeot_electric_pour_over_kettle_kettle",
    "switch.peugeot_electric_pour_over_kettle_keep_warm",
    "switch.coffee_grinder_plug",
    "switch.interruptor_smart_wifi_2_switch_1",
    "switch.interruptor_smart_baw_3_switch_1",
    "climate.thermostat",
    "climate.library_air_conditioner",
    "media_player.tx_rz70",
    "media_player.tx_rz70_zone_2",
    "camera.doorbell_front",
    "camera.smart_doorbell_ip09",
]

LOG_PATTERNS = {
    "template_missing_action": r"payload_json\.action|has no attribute 'action'",
    "missing_referenced_entity": r"Referenced entities .* missing or not currently available",
    "smartthings_subscription_limit": r"Reached limit of subscriptions|Unable to create SinkFilter",
    "camera_stream_404": r"Not Found error opening stream|Server returned 404 Not Found",
    "midea_unable_to_connect": r"\bmidealocal\.device\b.*Unable to connect",
    "tuya_local": r"\btuya_local\b",
    "water_heater_error": r"water_heater|Temperature is not a number",
    "config_entry_error": r"Config entry|setup_retry|migration_error",
}

SIGNED_URL_RE = re.compile(r"(signInfo=)[A-Za-z0-9_.~%+-]+")
RTSP_AUTH_RE = re.compile(r"(rtsps?://)[^/@\s]+@")
ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")


def now_utc() -> str:
    return dt.datetime.now(dt.UTC).isoformat()


def get_token() -> str:
    try:
        return subprocess.check_output(
            ["pass", "show", "env/home-assistant-token"],
            text=True,
            stderr=subprocess.DEVNULL,
        ).strip()
    except subprocess.CalledProcessError as exc:
        raise SystemExit(f"failed to read Home Assistant token from pass: {exc}") from exc


def api_get(base_url: str, token: str, path: str, timeout: int = 20) -> Any:
    url = urllib.parse.urljoin(base_url.rstrip("/") + "/", path.lstrip("/"))
    req = urllib.request.Request(url, headers={"Authorization": f"Bearer {token}"})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as response:
            data = response.read().decode("utf-8", "replace")
            ctype = response.headers.get("content-type", "")
    except urllib.error.HTTPError as exc:
        body = exc.read().decode("utf-8", "replace")
        return {"_error": f"HTTP {exc.code}", "body": body}
    except urllib.error.URLError as exc:
        return {"_error": str(exc)}

    if "application/json" in ctype:
        try:
            return json.loads(data)
        except json.JSONDecodeError:
            return {"_error": "invalid json", "body": data}
    return data


def redact_log(line: str) -> str:
    line = ANSI_RE.sub("", line)
    line = SIGNED_URL_RE.sub(r"\1<redacted>", line)
    line = RTSP_AUTH_RE.sub(r"\1<redacted>@", line)
    return line


def load_known_issues(path: Path | None) -> list[str]:
    if not path or not path.exists():
        return []
    issues: list[str] = []
    for raw in path.read_text().splitlines():
        line = raw.strip()
        if line.startswith("- "):
            issues.append(line[2:].strip())
    return issues


def summarize_logs(log_text: str, max_lines: int) -> dict[str, Any]:
    lines = [redact_log(line) for line in log_text.splitlines()]
    recent = lines[-max_lines:]
    warning_error = [
        line for line in recent if " WARNING " in line or " ERROR " in line or " CRITICAL " in line
    ]
    patterns: dict[str, dict[str, Any]] = {}
    for name, pattern in LOG_PATTERNS.items():
        rx = re.compile(pattern, re.I)
        matches = [line for line in warning_error if rx.search(line)]
        if matches:
            patterns[name] = {"count": len(matches), "examples": matches[-3:]}
    return {
        "tail_line_count": len(recent),
        "warning_error_count": len(warning_error),
        "patterns": patterns,
        "recent_warning_error_examples": warning_error[-20:],
    }


def entity_summary(entity: dict[str, Any]) -> dict[str, Any]:
    attrs = entity.get("attributes") or {}
    return {
        "entity_id": entity.get("entity_id"),
        "state": entity.get("state"),
        "friendly_name": attrs.get("friendly_name"),
        "device_class": attrs.get("device_class"),
        "last_changed": entity.get("last_changed"),
        "last_updated": entity.get("last_updated"),
    }


def fingerprint(item: Any) -> str:
    return hashlib.sha256(json.dumps(item, sort_keys=True, default=str).encode()).hexdigest()


def load_previous(state_file: Path) -> dict[str, Any] | None:
    if not state_file.exists():
        return None
    try:
        return json.loads(state_file.read_text())
    except json.JSONDecodeError:
        return None


def ids(items: list[dict[str, Any]], key: str) -> set[str]:
    return {str(item.get(key)) for item in items if item.get(key)}


def compute_delta(current: dict[str, Any], previous: dict[str, Any] | None) -> dict[str, Any]:
    if not previous:
        return {"first_run": True}
    prev_entries = ids(previous.get("config_entries_not_loaded", []), "entry_id")
    cur_entries = ids(current.get("config_entries_not_loaded", []), "entry_id")
    prev_unavailable = ids(previous.get("unavailable_or_unknown_entities", []), "entity_id")
    cur_unavailable = ids(current.get("unavailable_or_unknown_entities", []), "entity_id")
    prev_critical = ids(previous.get("critical_entities_problem", []), "entity_id")
    cur_critical = ids(current.get("critical_entities_problem", []), "entity_id")

    prev_patterns = set((previous.get("logs") or {}).get("patterns", {}).keys())
    cur_patterns = set((current.get("logs") or {}).get("patterns", {}).keys())
    return {
        "first_run": False,
        "new_config_entry_failures": sorted(cur_entries - prev_entries),
        "resolved_config_entry_failures": sorted(prev_entries - cur_entries),
        "new_unavailable_or_unknown_entities": sorted(cur_unavailable - prev_unavailable),
        "resolved_unavailable_or_unknown_entities": sorted(prev_unavailable - cur_unavailable),
        "new_critical_entity_problems": sorted(cur_critical - prev_critical),
        "resolved_critical_entity_problems": sorted(prev_critical - cur_critical),
        "new_log_patterns": sorted(cur_patterns - prev_patterns),
        "resolved_log_patterns": sorted(prev_patterns - cur_patterns),
        "previous_run_at": previous.get("collected_at"),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ha-url", default=os.environ.get("HA_URL", DEFAULT_HA_URL))
    parser.add_argument("--home-root", default="/Users/pablostafforini/My Drive/home")
    parser.add_argument("--known-issues")
    parser.add_argument("--state-dir", default=str(DEFAULT_STATE_DIR))
    parser.add_argument("--log-tail-lines", type=int, default=500)
    parser.add_argument("--max-unavailable-entities", type=int, default=40)
    parser.add_argument("--include-all-entities", action="store_true")
    parser.add_argument("--no-save-state", action="store_true")
    args = parser.parse_args()

    token = get_token()
    base = args.ha_url

    api_status = api_get(base, token, "/api/")
    states = api_get(base, token, "/api/states")
    entries = api_get(base, token, "/api/config/config_entries/entry")
    logs = api_get(base, token, "/api/hassio/core/logs")

    if isinstance(states, dict) and states.get("_error"):
        raise SystemExit(f"states query failed: {states}")
    if isinstance(entries, dict) and entries.get("_error"):
        raise SystemExit(f"config entries query failed: {entries}")

    states_by_id = {entity.get("entity_id"): entity for entity in states if isinstance(entity, dict)}

    unavailable = [
        entity_summary(entity)
        for entity in states
        if isinstance(entity, dict) and entity.get("state") in {"unavailable", "unknown"}
    ]
    unavailable.sort(key=lambda item: item["entity_id"] or "")

    critical = [
        entity_summary(states_by_id[entity_id])
        for entity_id in DEFAULT_CRITICAL_ENTITIES
        if entity_id in states_by_id and states_by_id[entity_id].get("state") in {"unavailable", "unknown"}
    ]
    missing_critical = [
        {"entity_id": entity_id, "state": "missing"}
        for entity_id in DEFAULT_CRITICAL_ENTITIES
        if entity_id not in states_by_id
    ]
    critical.extend(missing_critical)

    not_loaded = [
        {
            "entry_id": entry.get("entry_id"),
            "domain": entry.get("domain"),
            "title": entry.get("title"),
            "state": entry.get("state"),
            "reason": entry.get("reason"),
            "disabled_by": entry.get("disabled_by"),
        }
        for entry in entries
        if isinstance(entry, dict) and entry.get("state") != "loaded" and entry.get("disabled_by") is None
    ]
    not_loaded.sort(key=lambda item: (item.get("domain") or "", item.get("title") or ""))

    result: dict[str, Any] = {
        "collected_at": now_utc(),
        "ha_url": base,
        "api_status": api_status,
        "home_root": args.home_root,
        "known_issues": load_known_issues(Path(args.known_issues) if args.known_issues else None),
        "config_entries_not_loaded": not_loaded,
        "critical_entities_problem": critical,
        "unavailable_or_unknown_count": len(unavailable),
        "unavailable_or_unknown_entities": unavailable,
        "logs": summarize_logs(logs if isinstance(logs, str) else json.dumps(logs), args.log_tail_lines),
    }

    state_dir = Path(args.state_dir).expanduser()
    state_file = state_dir / "last-run.json"
    previous = load_previous(state_file)
    result["delta"] = compute_delta(result, previous)
    result["snapshot_fingerprint"] = fingerprint(
        {
            "config_entries_not_loaded": result["config_entries_not_loaded"],
            "critical_entities_problem": result["critical_entities_problem"],
            "unavailable_or_unknown_entities": result["unavailable_or_unknown_entities"],
            "log_patterns": sorted(result["logs"]["patterns"].keys()),
        }
    )

    if not args.no_save_state:
        state_dir.mkdir(parents=True, exist_ok=True)
        state_file.write_text(json.dumps(result, indent=2, sort_keys=True))

    output = dict(result)
    if not args.include_all_entities:
        output["unavailable_or_unknown_entities"] = unavailable[: args.max_unavailable_entities]
        output["unavailable_or_unknown_entities_truncated"] = max(
            0, len(unavailable) - args.max_unavailable_entities
        )

    print(json.dumps(output, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    sys.exit(main())
