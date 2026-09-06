#!/usr/bin/env python3
"""Describe contact merge candidates for review; never generate executable edits.

Matching keys are evidence to inspect, not proof that two records are one person.
The loaded projections are not full contact backups. This helper cannot safely
copy every contact field or bind a later deletion to the reviewed live state.
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import importlib.util
import json
import math
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

HERE = Path(__file__).resolve().parent
MAX_TEXT_GROUPS = 20
MAX_TEXT_MEMBERS = 5
MAX_LABEL = 120


class PlanInputError(ValueError):
    """An invalid projection; messages contain only field names and row indexes."""


def load_reconciler():
    """Import the sibling reader without invoking its CLI or selecting stores."""
    spec = importlib.util.spec_from_file_location("reconcile_contacts", HERE / "reconcile-contacts.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def validate_records(records, store):
    if not isinstance(records, list) or not records:
        raise PlanInputError(f"{store}: a nonempty, completely read record list is required")
    identifiers = set()
    for index, record in enumerate(records):
        if not isinstance(record, dict):
            raise PlanInputError(f"{store}: row {index} is not a record object")
        uid = record.get("uuid")
        if not isinstance(uid, str) or not uid.strip() or uid != uid.strip() or uid in identifiers:
            raise PlanInputError(f"{store}: missing, invalid or duplicate record ID at row {index}")
        identifiers.add(uid)
        for field in ("first", "mid", "last", "maiden", "nick", "sfx"):
            if not isinstance(record.get(field), str):
                raise PlanInputError(f"{store}: missing or invalid {field} at row {index}")
        for field in ("org", "aka", "mail", "urls"):
            values = record.get(field)
            if not isinstance(values, list) or any(not isinstance(value, str) for value in values):
                raise PlanInputError(f"{store}: missing or invalid {field} at row {index}")
        phones = record.get("phones")
        if not isinstance(phones, list) or any(not isinstance(phone, (list, tuple)) or len(phone) != 2
                                             or any(not isinstance(value, str) for value in phone) for phone in phones):
            raise PlanInputError(f"{store}: missing or invalid phones at row {index}")
        if "birthday" not in record:
            raise PlanInputError(f"{store}: missing birthday field at row {index}")
        birthday = record["birthday"]
        if birthday is not None and (isinstance(birthday, bool) or not isinstance(birthday, (str, int, float))
                                     or isinstance(birthday, float) and not math.isfinite(birthday)):
            raise PlanInputError(f"{store}: invalid birthday field at row {index}")
    # Preserve all provided fields, including future reader additions, but do
    # not silently drop values that cannot be represented in the review output.
    json.dumps(records, ensure_ascii=True, sort_keys=True, allow_nan=False)


def projection_digest(records):
    encoded = json.dumps(records, ensure_ascii=True, sort_keys=True, separators=(",", ":"), allow_nan=False).encode()
    return hashlib.sha256(encoded).hexdigest()


def build_plan(rc, bbdb, contacts):
    """Return review evidence from explicit in-memory projections; no I/O.

    Multiple Contacts rows with exactly one common candidate in BBDB form a
    review group, not a merge instruction. Multi-target rows remain separately
    visible as ambiguous evidence; no survivor or field-normalization is chosen.
    """
    validate_records(bbdb, "BBDB")
    validate_records(contacts, "Contacts")
    bbdb_index = rc.build_index(bbdb)
    claimed = defaultdict(list)
    ambiguous = []
    unmatched = 0
    for index, record in enumerate(contacts):
        evidence = defaultdict(list)
        for key in sorted(rc.match_keys(record)):
            for target in sorted(bbdb_index.get(key, ())):
                evidence[target].append(key)
        item = {"row": index, "record": copy.deepcopy(record)}
        if len(evidence) == 1:
            target, keys = next(iter(evidence.items()))
            claimed[target].append(dict(item, matching_keys=keys))
        elif evidence:
            ambiguous.append(dict(item, candidate_bbdb_records=[
                {"row": target, "record": copy.deepcopy(bbdb[target]), "matching_keys": keys}
                for target, keys in sorted(evidence.items())]))
        else:
            unmatched += 1
    groups = [{"bbdb": {"row": target, "record": copy.deepcopy(bbdb[target])},
               "contacts": members,
               "reason": "multiple Contacts rows share one heuristic BBDB candidate; identity is unverified"}
              for target, members in sorted(claimed.items()) if len(members) > 1]
    return {
        "schema_version": 1,
        "mode": "review_only",
        "executable_mutations": False,
        "counts": {"bbdb_records": len(bbdb), "contacts_records": len(contacts),
                   "candidate_groups": len(groups), "candidate_contacts": sum(len(group["contacts"]) for group in groups),
                   "ambiguous_contacts": len(ambiguous), "unmatched_contacts": unmatched},
        "projection_digests": {"bbdb_sha256": projection_digest(bbdb), "contacts_sha256": projection_digest(contacts),
                               "scope": "loaded projected values and order only; not complete database bytes or a future live-state guard"},
        "candidate_groups": groups,
        "ambiguous_contacts": ambiguous,
        "limitations": [
            "Name, organization, email and profile keys can be shared; a unique candidate is not verified identity.",
            "Original projected values are preserved, not merged, renamed, normalized or deduplicated.",
            "These projections omit contact fields such as postal addresses, notes, photos and group membership; they are not full backups.",
            "Saved evidence is not authorization or a state-bound plan for later live edits or deletions.",
            "No candidate group does not mean the stores are reconciled or contain no duplicates.",
        ],
    }


def emit_applescript(_plan, _out):
    """Refuse the former unsafe executable-generation API without writing output."""
    raise ValueError("Executable contact merges are not supported; use review-only candidate evidence and a separately authorized full-record workflow")


def label(record):
    value = " ".join(record[field] for field in ("first", "mid", "last", "sfx") if record[field])
    if not value:
        value = "; ".join(record["org"]) or "(unnamed)"
    # repr prevents record-controlled terminal escapes/newlines; truncate the
    # escaped representation as well as bounding groups and members.
    return ascii(value)[:MAX_LABEL]


def emit_summary(plan, out):
    counts = plan["counts"]
    print("Review only. No contact changes are generated or authorized.", file=out)
    print(f"{counts['candidate_groups']} candidate group(s); {counts['ambiguous_contacts']} ambiguous Contacts row(s).", file=out)
    for group in plan["candidate_groups"][:MAX_TEXT_GROUPS]:
        print(f"  BBDB row {group['bbdb']['row']}: {label(group['bbdb']['record'])}", file=out)
        for member in group["contacts"][:MAX_TEXT_MEMBERS]:
            kinds = sorted({key.split(":", 1)[0] for key in member["matching_keys"]})
            print(f"    Contacts row {member['row']}: {label(member['record'])}; key kinds: {', '.join(kinds)}", file=out)
        omitted = len(group["contacts"]) - MAX_TEXT_MEMBERS
        if omitted > 0:
            print(f"    {omitted} additional row(s) omitted from this summary.", file=out)
    if counts["candidate_groups"] > MAX_TEXT_GROUPS:
        print(f"{counts['candidate_groups'] - MAX_TEXT_GROUPS} additional group(s) omitted from this summary.", file=out)
    print("Identity remains unverified. Full projected fields and ambiguous evidence require --json in a private location.", file=out)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bbdb-file", type=Path, required=True, help="Explicit selected BBDB file")
    parser.add_argument("--contacts-db", type=Path, required=True, help="Explicit selected Contacts source database")
    output = parser.add_mutually_exclusive_group(required=True)
    output.add_argument("--dry-run", action="store_true", help="Bounded human-readable candidate summary")
    output.add_argument("--json", action="store_true", help="Full review-only projected evidence; keep output private")
    args = parser.parse_args(argv)
    phase = "load_reader"
    rc = None
    try:
        rc = load_reconciler()
        bbdb_metadata, contacts_metadata = {}, {}
        phase = "read_bbdb"
        bbdb = rc.load_bbdb(args.bbdb_file, HERE / "dump-bbdb.el", metadata=bbdb_metadata)
        if bbdb_metadata.get("status") != "complete":
            raise PlanInputError("BBDB reader did not confirm a complete supported projection")
        phase = "read_contacts"
        contacts = rc.load_contacts(args.contacts_db, metadata=contacts_metadata)
        if contacts_metadata.get("status") != "complete":
            raise PlanInputError("Contacts reader did not confirm a complete supported projection")
        phase = "validate_and_plan"
        plan = build_plan(rc, bbdb, contacts)
        plan["sources"] = {"bbdb": {"selected_file": str(args.bbdb_file.absolute()), "read_metadata": bbdb_metadata},
                           "contacts": {"selected_file": str(args.contacts_db.absolute()), "read_metadata": contacts_metadata}}
        if args.json:
            print(json.dumps(plan, ensure_ascii=True, sort_keys=True, allow_nan=False))
        else:
            emit_summary(plan, sys.stdout)
        return 0
    except (OSError, ValueError, TypeError, subprocess.SubprocessError) as error:
        # Reader messages can contain private source data; no partial plan is
        # emitted. The typed failure is enough to distinguish invalid input.
        interrupted = rc is not None and isinstance(error, getattr(rc, "ParserInterrupted", ()))
        reason = ("Selected-source reading was interrupted; no candidate plan emitted." if interrupted else
                  str(error) if isinstance(error, PlanInputError) else
                  "Explicit sources could not be read completely or contained invalid records; no candidate plan emitted.")
        failure = {"status": "interrupted" if interrupted else "input_error", "phase": phase,
                   "error_type": type(error).__name__, "reason": reason}
        if interrupted:
            failure["signal"] = error.signum
        print(json.dumps(failure))
        return error.exit_code if interrupted else 2


if __name__ == "__main__":
    sys.exit(main())
