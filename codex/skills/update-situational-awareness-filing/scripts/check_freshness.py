#!/usr/bin/env python3
"""Fail when SA LP filing artifacts do not identify the target filing."""

from __future__ import annotations

import argparse
import json
import re
import sys
from collections import Counter
from pathlib import Path


DEFAULT_NOTE = Path(
    "/Users/pablostafforini/My Drive/notes/public/situational-awareness-lp.org"
)
DEFAULT_SITE = Path("/Users/pablostafforini/repos/stafforini.com")


def result_block(text: str, name: str) -> str:
    match = re.search(
        rf"(?m)^#\+RESULTS: {re.escape(name)}[^\n]*\n", text
    )
    if not match:
        raise ValueError(f"missing #+RESULTS: {name}")

    tail = text[match.end() :]
    lines = tail.splitlines()
    while lines and not lines[0].strip():
        lines.pop(0)
    if not lines:
        return ""

    first = lines[0]
    if first.startswith("#+begin_"):
        end_marker = first.replace("#+begin_", "#+end_", 1)
        captured = [first]
        for line in lines[1:]:
            captured.append(line)
            if line == end_marker:
                break
        return "\n".join(captured)

    if first.startswith(":"):
        captured = []
        for line in lines:
            if not line.startswith(":"):
                break
            captured.append(line)
        return "\n".join(captured)

    if first.startswith("|"):
        captured = []
        for line in lines:
            if line.startswith("|") or line.startswith("#+TBLFM:"):
                captured.append(line)
            else:
                break
        return "\n".join(captured)

    captured = []
    for line in lines:
        if not line.strip():
            break
        captured.append(line)
    return "\n".join(captured)


def require(conditions: list[str], failures: list[str], condition: bool, message: str) -> None:
    if condition:
        conditions.append(message)
    else:
        failures.append(message)


def parse_data(block: str) -> dict:
    payload = "\n".join(line.removeprefix(": ") for line in block.splitlines())
    return json.loads(payload)


def parse_calculator_rows(text: str) -> list[dict]:
    match = re.search(r"equity_only:\s*(\[.*?\]),\s*\n\s*full:", text, re.DOTALL)
    if not match:
        raise ValueError("calculator equity_only data is missing")
    return json.loads(match.group(1))


def holdings_signature(rows: list[dict], value_key: str) -> Counter:
    return Counter(
        (row["ticker"], row["type"], int(round(row[value_key]))) for row in rows
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--quarter", required=True, help="For example Q2_2026")
    parser.add_argument("--filing-date", required=True)
    parser.add_argument(
        "--effective-date",
        help="Backtest boundary date; defaults to the official filing date",
    )
    parser.add_argument("--accession", required=True)
    parser.add_argument("--holding-count", required=True, type=int)
    parser.add_argument("--reported-total", required=True, type=int)
    parser.add_argument("--note", type=Path, default=DEFAULT_NOTE)
    parser.add_argument("--site-repo", type=Path, default=DEFAULT_SITE)
    args = parser.parse_args()
    effective_date = args.effective_date or args.filing_date

    note_text = args.note.read_text()
    quarter_display = args.quarter.replace("_", " ")
    conditions: list[str] = []
    failures: list[str] = []

    try:
        data = result_block(note_text, "sa-data")
        perf = result_block(note_text, "sa-perf")
        sensitivity = result_block(note_text, "sa-sensitivity")
        delay = result_block(note_text, "sa-delay")
    except ValueError as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1

    require(
        conditions,
        failures,
        args.accession in data,
        "sa-data contains the target accession",
    )
    try:
        filings = parse_data(data)["filings"]
        target = next(
            filing
            for filing in filings
            if filing.get("accession") == args.accession
        )
    except (json.JSONDecodeError, KeyError, StopIteration) as error:
        print(f"FAIL: cannot parse target filing from sa-data: {error}", file=sys.stderr)
        return 1

    holdings = target.get("holdings", [])
    require(
        conditions,
        failures,
        target.get("quarter") == args.quarter,
        "sa-data target accession has the target quarter",
    )
    require(
        conditions,
        failures,
        filings[-1].get("accession") == args.accession,
        "sa-data target accession is the latest full filing",
    )
    require(
        conditions,
        failures,
        len(holdings) == args.holding_count,
        f"sa-data target has {args.holding_count} holdings",
    )
    require(
        conditions,
        failures,
        sum(int(row["value"]) for row in holdings) == args.reported_total,
        f"sa-data target totals {args.reported_total}",
    )
    require(
        conditions,
        failures,
        re.search(
            rf"(?m)^{re.escape(args.quarter)}(?:\s+†)?\s+"
            rf"{re.escape(effective_date)}\s+to\s+",
            perf,
        )
        is not None,
        "sa-perf contains the target quarter at the effective-date boundary",
    )
    require(
        conditions,
        failures,
        re.search(
            rf"(?m)^Window: .*\s+to\s+{re.escape(effective_date)}\s+\(",
            delay,
        )
        is not None,
        "sa-delay window ends at the target effective date",
    )
    require(
        conditions,
        failures,
        bool(sensitivity.strip()),
        "sa-sensitivity has a result",
    )
    require(
        conditions,
        failures,
        re.search(r"\berr\b", sensitivity, re.IGNORECASE) is None,
        "sa-sensitivity contains no err cells",
    )

    assets = {
        "returns chart": args.site_repo / "static/images/sa-lp-returns.html",
        "AIS chart": args.site_repo / "static/images/sa-lp-returns-ais.html",
        "calculator": args.site_repo / "static/images/sa-lp-calculator.html",
    }
    asset_text: dict[str, str] = {}
    for label, path in assets.items():
        if not path.exists():
            failures.append(f"{label} exists")
            continue
        asset_text[label] = path.read_text()
        conditions.append(f"{label} exists")

    for label in ("returns chart", "AIS chart"):
        text = asset_text.get(label, "")
        marker = (
            f'"x0":"{args.filing_date}",'
            f'"x1":"{args.filing_date}"'
        )
        require(
            conditions,
            failures,
            marker in text,
            f"{label} contains the target filing-date marker",
        )
        require(
            conditions,
            failures,
            args.filing_date in text,
            f"{label} contains the target filing date",
        )

    calculator = asset_text.get("calculator", "")
    expected_label = (
        f"Latest disclosed portfolio: {quarter_display} 13F filed "
        f"{args.filing_date}"
    )
    require(
        conditions,
        failures,
        expected_label in calculator,
        "calculator contains the target current-portfolio label",
    )
    require(
        conditions,
        failures,
        args.filing_date in calculator,
        "calculator contains the target filing date",
    )
    try:
        calculator_rows = parse_calculator_rows(calculator)
        calculator_matches = holdings_signature(
            calculator_rows, "reported_value"
        ) == holdings_signature(holdings, "value")
    except (ValueError, json.JSONDecodeError, KeyError, TypeError):
        calculator_matches = False
    require(
        conditions,
        failures,
        calculator_matches,
        "calculator holdings exactly match the target sa-data holdings",
    )

    for item in conditions:
        print(f"PASS: {item}")
    for item in failures:
        print(f"FAIL: {item}", file=sys.stderr)
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
