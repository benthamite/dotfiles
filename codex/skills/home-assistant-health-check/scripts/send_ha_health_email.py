#!/usr/bin/env python3
"""Run the HA health collector and email a concise report."""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any


DEFAULT_HOME_ROOT = "/Users/pablostafforini/My Drive/home"
DEFAULT_KNOWN_ISSUES = f"{DEFAULT_HOME_ROOT}/context/ha-health-known-issues.md"
DEFAULT_GMAIL = "/Users/pablostafforini/My Drive/dotfiles/claude/bin/gmail.py"
DEFAULT_TO = "pablo.stafforini@gmail.com"


def run_json(command: list[str]) -> dict[str, Any]:
    proc = subprocess.run(command, check=True, text=True, capture_output=True)
    return json.loads(proc.stdout)


def known_match(text: str, known_issues: list[str]) -> bool:
    haystack = "\n".join(known_issues).lower()
    return bool(text) and text.lower() in haystack


def config_entry_line(entry: dict[str, Any]) -> str:
    title = entry.get("title") or "(untitled)"
    domain = entry.get("domain") or "unknown"
    state = entry.get("state") or "unknown"
    reason = entry.get("reason")
    suffix = f": {reason}" if reason else ""
    return f"{title} ({domain}, {state}){suffix}"


def entity_line(entity: dict[str, Any]) -> str:
    name = entity.get("friendly_name") or entity.get("entity_id")
    return f"{name}: {entity.get('entity_id')} is {entity.get('state')}"


def is_known_entry(entry: dict[str, Any], known_issues: list[str]) -> bool:
    domain = str(entry.get("domain") or "").lower()
    known_text = "\n".join(known_issues).lower()
    if domain == "hue_ble" and "hue ble" in known_text:
        return True
    return any(
        known_match(str(entry.get(key) or ""), known_issues)
        for key in ("title", "domain", "reason")
    )


def is_known_entity(entity: dict[str, Any], known_issues: list[str]) -> bool:
    return any(
        known_match(str(entity.get(key) or ""), known_issues)
        for key in ("entity_id", "friendly_name")
    )


def summarize(result: dict[str, Any]) -> tuple[str, str]:
    known_issues = result.get("known_issues") or []
    delta = result.get("delta") or {}
    new_items: list[str] = []
    needs_action: list[str] = []
    known: list[str] = []
    known_config_counts: dict[str, int] = {}

    for key, label in (
        ("new_config_entry_failures", "new config entry failure"),
        ("new_critical_entity_problems", "new critical entity problem"),
        ("new_log_patterns", "new log pattern"),
        ("new_unavailable_or_unknown_entities", "new unavailable/unknown entity"),
    ):
        for item in delta.get(key) or []:
            new_items.append(f"{label}: {item}")

    for entry in result.get("config_entries_not_loaded") or []:
        line = config_entry_line(entry)
        if is_known_entry(entry, known_issues):
            domain = entry.get("domain") or "unknown"
            state = entry.get("state") or "unknown"
            reason = entry.get("reason")
            key = f"{domain}: config entries {state}"
            if reason and domain in {"smartthings"}:
                key = f"{key}: {reason}"
            known_config_counts[key] = known_config_counts.get(key, 0) + 1
        else:
            needs_action.append(line)

    for entity in result.get("critical_entities_problem") or []:
        line = entity_line(entity)
        if is_known_entity(entity, known_issues):
            known.append(line)
        else:
            needs_action.append(line)

    patterns = (result.get("logs") or {}).get("patterns") or {}
    for name, info in patterns.items():
        text = f"{name}: {info.get('count', 0)} recent matches"
        if name in {"camera_stream_404", "midea_unable_to_connect"}:
            known.append(text)
        elif name == "missing_referenced_entity" and any(
            "interruptor_smart_wifi_2_switch_1" in example
            for example in info.get("examples", [])
        ):
            known.append(text)
        else:
            needs_action.append(text)

    known = [f"{key} ({count})" for key, count in sorted(known_config_counts.items())] + known

    subject_status = "Needs attention" if new_items or needs_action else "OK"
    subject = f"Home Assistant health: {subject_status}"
    lines = [
        "**New**",
        *(f"- {item}" for item in new_items[:20]),
        *([] if new_items else ["- None"]),
        "",
        "**Needs Action**",
        *(f"- {item}" for item in needs_action[:20]),
        *([] if needs_action else ["- None"]),
        "",
        "**Known / Still Present**",
        *(f"- {item}" for item in known[:30]),
        *([] if known else ["- None"]),
        "",
        "**Verification**",
        f"- Collector run: {result.get('collected_at')}",
        f"- HA API: {(result.get('api_status') or {}).get('message', 'unknown')}",
        f"- Unavailable/unknown entities: {result.get('unavailable_or_unknown_count')}",
        f"- Snapshot fingerprint: {result.get('snapshot_fingerprint')}",
    ]
    return subject, "\n".join(lines) + "\n"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--home-root", default=DEFAULT_HOME_ROOT)
    parser.add_argument("--known-issues", default=DEFAULT_KNOWN_ISSUES)
    parser.add_argument("--to", default=DEFAULT_TO)
    parser.add_argument("--gmail-account", default="personal", choices=("epoch", "personal"))
    parser.add_argument("--gmail-bin", default=DEFAULT_GMAIL)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()

    script_dir = Path(__file__).resolve().parent
    collector = script_dir / "collect_ha_health.py"
    result = run_json(
        [
            sys.executable,
            str(collector),
            "--home-root",
            args.home_root,
            "--known-issues",
            args.known_issues,
            "--max-unavailable-entities",
            "5",
        ]
    )
    subject, body = summarize(result)

    if args.dry_run:
        print(f"Subject: {subject}\n")
        print(body)
        return 0

    with tempfile.NamedTemporaryFile("w", delete=False, encoding="utf-8") as tmp:
        tmp.write(body)
        body_file = tmp.name
    try:
        subprocess.run(
            [
                sys.executable,
                args.gmail_bin,
                "--account",
                args.gmail_account,
                "send",
                "--to",
                args.to,
                "--subject",
                subject,
                "--body-file",
                body_file,
            ],
            check=True,
        )
    finally:
        Path(body_file).unlink(missing_ok=True)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
