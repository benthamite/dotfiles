#!/usr/bin/env python3
"""Classify org-roam TODOs from a JSON dump and write a triage report.

The skill body describes the workflow. This script encodes the title-pattern
heuristics so they can be tuned in one place. Edit the BLOCKER_PATTERNS,
SCOPE_DEMOTIONS, and CANDIDATE_PATTERNS lists when patterns mis-classify.

Usage:
  triage.py --input <dump.json> --output <report.md> [--mode dry-run|act]
            [--max-tasks N] [--classifications-out <classifications.json>]
"""
from __future__ import annotations

import argparse
import json
import re
import sys
from collections import defaultdict
from datetime import datetime
from pathlib import Path

# A title that starts with "Ask Claude" / "Ask AI" / "Ask myself" is not a
# user-bound blocker. The negative lookahead keeps those out of ease 1.
NOT_ASK_USER = r"(?!claude|ai|chatgpt|myself|gpt)"

# (regex, ease, reason). First match wins. re.IGNORECASE applied at search.
BLOCKER_PATTERNS: list[tuple[str, int, str]] = [
    (rf"^\s*ask {NOT_ASK_USER}\w", 1, "Needs your direct input"),
    (r"^\s*(check with|confirm with|consult|coordinate with|nudge|ping|follow up with)\b", 1, "Needs your direct input"),
    (r"^\s*(meet with|meeting with|call with|reunirme con|llamar a)\b", 1, "Requires you to attend a meeting/call"),
    (r"^\s*(decide|decidir|pick|elegir|choose between|choose what|select which)\b", 2, "Needs your decision"),
    (r"\b(decide whether|decidir si)\b", 2, "Needs your decision"),
    (r"^\s*(buy|purchase|order|comprar|adquirir|renew|renovar|subscribe|suscribirse|pay for)\b", 2, "Needs your purchasing/financial decision"),
    (r"^\s*(go to|visit|attend|ir a|asistir|visitar)\b", 2, "Requires your physical presence"),
    (r"^\s*(install|set ?up|instalar)\b", 2, "Requires your hands-on setup or decision"),
    (r"^\s*(hire|contract|contratar)\b", 2, "Hiring/contracting decision"),
    (r"^\s*open (a|an|new)?\s*\w*\s*account\b", 2, "Requires you to open an account"),
    (r"^\s*(apply for|apply to|aplicar para)\b", 2, "Application requires your personal details"),
    (r"^\s*(schedule|book|reservar|programar)\b", 2, "Scheduling/booking requires your availability"),
    (r"^\s*(sell|vender)\b", 2, "Selling requires your decision and externally visible action"),
    (r"^\s*(email|message|reply to|respond to|write to|escribirle|responder a|contactar|invite|invitar)\b", 3, "Needs your voice for outbound communication"),
    (r"^\s*(tell|notify|inform|let .* know|avisar|informar)\b", 3, "Needs your voice for outbound communication"),
    (r"^\s*(contact|reach out to|get in touch with)\b", 3, "Needs your voice for outbound communication"),
    (r"^\s*(announce|publish|publicar|anunciar)\b", 3, "Externally visible communication needs your sign-off"),
    (r"^\s*plan\b", 3, "Planning is usually user-bound"),
    (r"^\s*take photo\b", 2, "Requires you in person"),
    (r"^\s*take .* (course|class|exam|test|appointment|leave)\b", 2, "Personal scheduling/commitment"),
    (r"^\s*(abstain|stop|commit to|comprometer)\b", 4, "Behavioral commitment"),
    (r"^\s*(reflect on|think about|consider|meditate on|reflexionar|pensar en|pensar sobre)\b", 4, "Personal cognitive work"),
    (r"^\s*plan\s+(future|life|career|next|los próximos|el futuro|trip|vacation)\b", 4, "Personal planning needs your input"),
    (r"^\s*look at .* (health|condition|situation|finances?|portfolio)\b", 4, "Personal context only you have"),
    (r"\b(long.?term|goals? for|strategic plan|vision for|life plan|career direction|metas a largo plazo)\b", 5, "Deep strategic context"),
]

# Verbs that LOOK candidate-like but become blockers when paired with a scope
# qualifier (course/book/website/recurring/etc.). Checked before candidates.
SCOPE_DEMOTIONS: list[tuple[str, int, str]] = [
    (r"^\s*translate\b.*(course|book|website|site|essay|article|paper|manual|treatise|/[^/]+/)", 2, "Long-form translation scope; needs sub-task split or queue assignment"),
    (r"^\s*refactor\b", 2, "Refactor scope typically exceeds a single subagent session"),
    (r"^\s*(rewrite|migrate|restructure|reorganise|reorganize)\b", 2, "Scope typically exceeds a single subagent session"),
    (r"^\s*organi[sz]e .*(meetings?|movie nights?|weekly|bi-weekly|monthly|EAGx|conference|things to do|trip|event)\b", 3, "Recurring/social coordination needs your voice"),
    (r"^\s*(compile|make) (a |the )?long\b", 4, "Open-ended cognitive work"),
    (r"^\s*(read|leer) (book|libro|/[^/]+/|the .* manual|entire|all of)\b", 3, "Long-form reading; output requires your synthesis"),
    (r"^\s*find treatment\b", 3, "Medical decisions require your context"),
    (r"^\s*build\b", 3, "Build implies multi-step engineering not scoped to one session"),
    (r"^\s*implement\b", 3, "Implementation tasks usually require design decisions"),
    (r"^\s*write\s+(book|libro|essay|paper|article)\b", 4, "Long-form writing is multi-day"),
    (r"^\s*create .*(radio|station|channel|business|company|website)\b", 3, "Creating a system/service is multi-step"),
    (r"^\s*create .*(pull request|\bPR\b|issue|ticket|asana|jira)\b", 2, "Externally visible action; draft instead"),
]

CANDIDATE_PATTERNS: list[str] = [
    r"^\s*(read|leer)\b",
    r"^\s*(summari[sz]e|resumir|extract|annotate|highlight)\b",
    r"^\s*(find|look up|research|search for|investigate|encontrar|buscar)\b",
    r"^\s*(update|actualizar|append|insert|agregar)\b",
    r"^\s*(add\b|añadir)\b",
    r"^\s*(fix|correct|repair|patch|arreglar|corregir)\b",
    r"^\s*(generate|draft|write up|compile|escribir|redactar|preparar)\b",
    r"^\s*(clean up|organi[sz]e|sort|tidy|ordenar|organizar)\b",
    r"^\s*(download|fetch|pull|sync|descargar|bajar)\b",
    r"^\s*(rename|move|copy|renombrar|mover|copiar)\b",
    r"^\s*(check|verify|validate|test|verificar|comprobar|revisar)\b",
    r"^\s*(create|crear)\b",
    r"^\s*(convert|transform|translate|traducir|convertir)\b",
    r"^\s*(tag|categori[sz]e|label|etiquetar)\b",
    r"^\s*(link|unify|unificar|enlazar)\b",
    r"^\s*(remove|delete|borrar|eliminar)\b",
    r"^\s*(format|reformat|formatear)\b",
    r"^\s*(extract|extraer)\b",
]

EASE_LABELS = {
    1: "missing fact / direct input",
    2: "yes-no decision",
    3: "outbound voice / framing",
    4: "personal cognitive work",
    5: "deep strategic context",
}


def classify(record: dict) -> tuple[str, int | None, str | None]:
    title = record.get("title") or ""
    for pattern, ease, reason in BLOCKER_PATTERNS:
        if re.search(pattern, title, re.IGNORECASE):
            return ("blocked", ease, reason)
    for pattern, ease, reason in SCOPE_DEMOTIONS:
        if re.search(pattern, title, re.IGNORECASE):
            return ("blocked", ease, reason)
    for pattern in CANDIDATE_PATTERNS:
        if re.search(pattern, title, re.IGNORECASE):
            return ("candidate", None, None)
    return ("investigate", None, None)


def priority_value(record: dict) -> int:
    p = record.get("priority")
    if p is None:
        return 5
    try:
        return int(p)
    except (ValueError, TypeError):
        return 5


def difficulty(record: dict) -> int:
    effort = record.get("effort")
    if effort:
        m = re.match(r"^(\d+):(\d+)$", effort)
        if m:
            mins = int(m.group(1)) * 60 + int(m.group(2))
        else:
            m = re.match(r"^(\d+)m$", effort)
            mins = int(m.group(1)) if m else None
        if mins is not None:
            if mins <= 15:
                return 1
            if mins <= 30:
                return 2
            if mins <= 60:
                return 3
            if mins <= 150:
                return 4
            return 5
    title = (record.get("title") or "").lower()
    if re.search(r"\b(typo|link|date|tag|name|path|url)\b", title):
        return 1
    if re.search(r"\b(refactor|rewrite|migrate|consolidate|reorganize)\b", title):
        return 4
    return 3


def fmt_priority(record: dict) -> str:
    p = record.get("priority")
    return f"[#{p}]" if p else "[#-]"


def fmt_file(record: dict, home: str) -> str:
    f = record.get("file") or ""
    return ("~" + f[len(home):]) if f.startswith(home) else f


def build_report(todos, buckets, mode, max_tasks, now) -> str:
    lines = []
    suffix = " (max-tasks=0, no subagents dispatched)" if (mode == "dry-run" and max_tasks == 0) else ""
    lines.append(f"# Overnight TODO {mode} — {now.strftime('%Y-%m-%d %H:%M')}{suffix}")
    lines.append("")
    lines.append("## Summary")
    lines.append("")
    lines.append(f"- **{len(todos)}** total active TODOs (no scheduled/deadline date)")
    lines.append(f"- **{len(buckets['blocked'])}** would be blockers (skipped subagent)")
    lines.append(f"- **{len(buckets['candidate'])}** would be dispatched as autonomous candidates")
    lines.append(f"- **{len(buckets['investigate'])}** would be dispatched as ambiguous (subagent reads body)")
    lines.append("")
    lines.append("Blockers by ease:")
    lines.append("")
    counts = defaultdict(int)
    for r in buckets["blocked"]:
        counts[r["_ease"]] += 1
    for e in sorted(counts):
        lines.append(f"- **ease {e}** ({EASE_LABELS[e]}): {counts[e]}")
    lines.append("")
    lines.append("Candidate count by org priority:")
    lines.append("")
    pri = defaultdict(int)
    for r in buckets["candidate"]:
        pri[r["_priority"]] += 1
    for p in sorted(pri):
        lines.append(f"- priority {p}: {pri[p]}")
    home = str(Path.home())
    lines.append("")
    lines.append("## Blockers — ranked by ease (1 easiest), then org priority")
    lines.append("")
    for r in buckets["blocked"]:
        lines.append(f"- **ease {r['_ease']}** {fmt_priority(r)} {r['title']}")
        lines.append(f"  - {r['_reason']}")
        lines.append(f"  - {fmt_file(r, home)}")
    lines.append("")
    lines.append("## Candidates (would dispatch in act mode) — by combined score")
    lines.append("")
    lines.append("Score = org priority + difficulty estimate. Lower score = higher dispatch priority.")
    lines.append("")
    for r in buckets["candidate"]:
        score = r["_priority"] + r["_difficulty"]
        effort_str = f" effort={r['effort']}" if r.get("effort") else ""
        lines.append(f"- score={score} {fmt_priority(r)} diff={r['_difficulty']}{effort_str} — {r['title']}")
        lines.append(f"  - {fmt_file(r, home)}")
    lines.append("")
    lines.append("## Investigate (would dispatch in act mode) — by org priority")
    lines.append("")
    lines.append("Ambiguous from the title alone; a subagent would read the heading body before deciding act-vs-block.")
    lines.append("")
    for r in buckets["investigate"]:
        effort_str = f" effort={r['effort']}" if r.get("effort") else ""
        lines.append(f"- {fmt_priority(r)}{effort_str} — {r['title']}")
        lines.append(f"  - {fmt_file(r, home)}")
    return "\n".join(lines) + "\n"


def main(argv):
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--input", required=True, help="Path to the JSON dump from org-roam-extras-dump-actionable-todos")
    p.add_argument("--output", required=True, help="Path to write the triage report (Markdown)")
    p.add_argument("--mode", choices=["dry-run", "act"], default="dry-run")
    p.add_argument("--max-tasks", type=int, default=0)
    p.add_argument("--classifications-out", help="Optional path to dump the classified records as JSON for the orchestrator")
    args = p.parse_args(argv)
    todos = json.load(open(args.input))
    buckets = {"blocked": [], "candidate": [], "investigate": []}
    for r in todos:
        cls, ease, reason = classify(r)
        rec = dict(r)
        rec["_class"] = cls
        rec["_ease"] = ease
        rec["_reason"] = reason
        rec["_priority"] = priority_value(r)
        rec["_difficulty"] = difficulty(r)
        buckets[cls].append(rec)
    buckets["blocked"].sort(key=lambda x: (x["_ease"], x["_priority"], (x.get("title") or "").lower()))
    buckets["candidate"].sort(key=lambda x: (x["_priority"] + x["_difficulty"], x["_priority"]))
    buckets["investigate"].sort(key=lambda x: (x["_priority"], (x.get("title") or "").lower()))
    now = datetime.now()
    Path(args.output).parent.mkdir(parents=True, exist_ok=True)
    Path(args.output).write_text(build_report(todos, buckets, args.mode, args.max_tasks, now))
    if args.classifications_out:
        Path(args.classifications_out).write_text(
            json.dumps({"blocked": buckets["blocked"], "candidate": buckets["candidate"], "investigate": buckets["investigate"]}, ensure_ascii=False)
        )
    print(f"TOTAL: {len(todos)}")
    print(f"BLOCKED: {len(buckets['blocked'])}")
    print(f"CANDIDATE: {len(buckets['candidate'])}")
    print(f"INVESTIGATE: {len(buckets['investigate'])}")
    print(f"REPORT: {args.output}")


if __name__ == "__main__":
    main(sys.argv[1:])
