#!/usr/bin/env python3
"""Persist redacted credential-incident state for future security audits."""

import argparse
import datetime as dt
import json
import os
import re
import stat
import subprocess
import sys
import tempfile
from pathlib import Path


SCHEMA = 1
DIRECTORY_MODE = 0o700
FILE_MODE = 0o600
FINGERPRINT = re.compile(r"^[0-9a-f]{20}$")
COMMIT = re.compile(r"^[0-9a-f]{40}$")
INCIDENT_ID = re.compile(r"^[a-z0-9][a-z0-9-]{2,79}$")
REFERENCE_ID = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._:/-]{0,127}$")
RFC3339 = re.compile(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$")
DATE = re.compile(r"^\d{4}-\d{2}-\d{2}$")

STATUSES = {
    "active-non-revocable",
    "awaiting-provider-revocation",
    "invalid",
    "revoked",
    "superseded",
    "historical-material-absent",
}
VERIFICATION_RESULTS = {
    "accepted",
    "rejected",
    "different-from-current",
    "matches-current",
    "absent-from-current",
    "provider-confirmed-revoked",
    "not-checked",
}
VERIFICATION_METHODS = {
    "provider-api",
    "provider-confirmation",
    "local-state-comparison",
    "local-install-inventory",
    "gitguardian-incident",
    "not-applicable",
}
NEXT_ACTIONS = {
    "await-provider",
    "verify-provider-rejection",
    "purge-history",
    "retain-private-and-purge-history",
    "none",
}
REFERENCE_KINDS = {"gmail-message", "provider-ticket", "gitguardian-incident"}
RECORD_FIELDS = {
    "provider",
    "credential_type",
    "credential_fingerprint",
    "finding_fingerprints",
    "status",
    "last_verified_at",
    "last_verified_result",
    "verification_method",
    "provider_references",
    "locations",
    "next_action",
    "summary",
    "updated_at",
}
INPUT_FIELDS = RECORD_FIELDS | {"incident_id"}
SECRET_TEXT = (
    re.compile(r"AKIA[0-9A-Z]{16}"),
    re.compile(r"gh[ps]_[A-Za-z0-9_]{30,}"),
    re.compile(r"github_pat_[A-Za-z0-9_]{22,}"),
    re.compile(r"xox[bporca]-[A-Za-z0-9-]+"),
    re.compile(r"-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----"),
    re.compile(
        r"(?i)(?:api[_ -]?key|token|secret|password|authorization)\s*[:=]\s*\S+"
    ),
    re.compile(r"(?i)[?&](?:api_?key|token|secret|password)=[^&\s]+"),
    re.compile(r"(?<![A-Za-z0-9])[A-Za-z0-9_+/=]{24,}(?![A-Za-z0-9])"),
)


class RegistryError(Exception):
    pass


def utc_now():
    return dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def default_registry_path(start):
    completed = subprocess.run(
        ["git", "-C", str(start), "rev-parse", "--absolute-git-dir"],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    if completed.returncode == 0:
        return Path(completed.stdout.strip()) / "dotfiles-publish" / "credential-incidents.json"
    state_home = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local/state"))
    return state_home / "security-audit" / "credential-incidents.json"


def reject_secret_text(value, field):
    if not isinstance(value, str):
        return
    for pattern in SECRET_TEXT:
        if pattern.search(value):
            raise RegistryError(f"{field} appears to contain credential material")


def require_text(record, field, limit=240):
    value = record.get(field)
    if not isinstance(value, str) or not value or len(value) > limit:
        raise RegistryError(f"{field} must be a non-empty string of at most {limit} characters")
    reject_secret_text(value, field)
    return value


def require_choice(record, field, choices):
    value = require_text(record, field, 80)
    if value not in choices:
        raise RegistryError(f"unsupported {field}: {value}")
    return value


def require_fingerprints(value, field):
    if not isinstance(value, list) or any(
        not isinstance(item, str) or not FINGERPRINT.fullmatch(item) for item in value
    ):
        raise RegistryError(f"{field} must be a list of 20-character hexadecimal fingerprints")
    if len(value) != len(set(value)):
        raise RegistryError(f"{field} contains duplicate fingerprints")


def validate_references(value):
    if not isinstance(value, list):
        raise RegistryError("provider_references must be a list")
    for reference in value:
        if not isinstance(reference, dict) or set(reference) != {"kind", "id"}:
            raise RegistryError("provider_references entries require only kind and id")
        if reference["kind"] not in REFERENCE_KINDS:
            raise RegistryError(f"unsupported provider reference kind: {reference['kind']}")
        if not isinstance(reference["id"], str) or not REFERENCE_ID.fullmatch(reference["id"]):
            raise RegistryError("provider reference id has an unsafe format")


def validate_locations(value):
    if not isinstance(value, list) or not value:
        raise RegistryError("locations must be a non-empty list")
    for location in value:
        if not isinstance(location, dict) or not set(location).issubset(
            {"path", "commits", "finding_fingerprints"}
        ):
            raise RegistryError("locations entries contain unsupported fields")
        path = location.get("path")
        if (
            not isinstance(path, str)
            or not path
            or path.startswith("/")
            or ".." in Path(path).parts
            or len(path) > 300
        ):
            raise RegistryError("location path must be a safe repository-relative path")
        commits = location.get("commits", [])
        if not isinstance(commits, list) or any(
            not isinstance(commit, str) or not COMMIT.fullmatch(commit) for commit in commits
        ):
            raise RegistryError("location commits must be full hexadecimal commit ids")
        require_fingerprints(location.get("finding_fingerprints", []), "location finding_fingerprints")


def validate_record(incident_id, record):
    if not isinstance(incident_id, str) or not INCIDENT_ID.fullmatch(incident_id):
        raise RegistryError("incident_id must be a lowercase hyphenated identifier")
    if not isinstance(record, dict):
        raise RegistryError(f"incident {incident_id} must be an object")
    extra = set(record) - RECORD_FIELDS
    if extra:
        raise RegistryError(f"incident {incident_id} has unsupported fields: {', '.join(sorted(extra))}")
    require_text(record, "provider", 100)
    require_text(record, "credential_type", 100)
    credential_fingerprint = require_text(record, "credential_fingerprint", 20)
    if not FINGERPRINT.fullmatch(credential_fingerprint):
        raise RegistryError("credential_fingerprint must be 20 hexadecimal characters")
    require_fingerprints(record.get("finding_fingerprints"), "finding_fingerprints")
    require_choice(record, "status", STATUSES)
    verified_at = require_text(record, "last_verified_at", 20)
    if not (RFC3339.fullmatch(verified_at) or DATE.fullmatch(verified_at)):
        raise RegistryError("last_verified_at must be an ISO date or UTC RFC3339 timestamp")
    require_choice(record, "last_verified_result", VERIFICATION_RESULTS)
    require_choice(record, "verification_method", VERIFICATION_METHODS)
    validate_references(record.get("provider_references"))
    validate_locations(record.get("locations"))
    require_choice(record, "next_action", NEXT_ACTIONS)
    require_text(record, "summary", 400)
    updated_at = require_text(record, "updated_at", 20)
    if not RFC3339.fullmatch(updated_at):
        raise RegistryError("updated_at must be UTC RFC3339 without fractional seconds")


def validate_state(state):
    if not isinstance(state, dict) or set(state) != {"schema", "incidents"}:
        raise RegistryError("registry must contain only schema and incidents")
    if state["schema"] != SCHEMA or not isinstance(state["incidents"], dict):
        raise RegistryError("unsupported credential incident registry schema")
    for incident_id, record in state["incidents"].items():
        validate_record(incident_id, record)
    return state


def check_private_path(path):
    if path.is_symlink():
        raise RegistryError(f"registry is a symlink: {path}")
    if path.parent.is_symlink():
        raise RegistryError(f"registry parent is a symlink: {path.parent}")
    if path.parent.exists():
        if not path.parent.is_dir():
            raise RegistryError(f"registry parent is not a directory: {path.parent}")
        parent_mode = stat.S_IMODE(path.parent.stat().st_mode)
        if parent_mode & 0o077:
            raise RegistryError(
                f"registry directory is group- or world-accessible (mode {parent_mode:o})"
            )
    if not path.exists():
        return
    mode = stat.S_IMODE(path.stat().st_mode)
    if mode & 0o077:
        raise RegistryError(f"registry is group- or world-accessible (mode {mode:o})")


def load_state(path):
    check_private_path(path)
    if not path.exists():
        return {"schema": SCHEMA, "incidents": {}}
    try:
        return validate_state(json.loads(path.read_text(encoding="utf-8")))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise RegistryError(f"could not read registry: {error}") from error


def write_state(path, state):
    validate_state(state)
    path.parent.mkdir(parents=True, exist_ok=True)
    os.chmod(path.parent, DIRECTORY_MODE)
    check_private_path(path)
    encoded = (json.dumps(state, indent=2, sort_keys=True) + "\n").encode()
    descriptor, temporary_name = tempfile.mkstemp(prefix=path.name + ".", dir=path.parent)
    temporary = Path(temporary_name)
    try:
        os.fchmod(descriptor, FILE_MODE)
        with os.fdopen(descriptor, "wb") as handle:
            handle.write(encoded)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(temporary, path)
        os.chmod(path, FILE_MODE)
    finally:
        temporary.unlink(missing_ok=True)


def read_input(path):
    if path.is_symlink():
        raise RegistryError("record input must not be a symlink")
    try:
        mode = stat.S_IMODE(path.stat().st_mode)
    except OSError as error:
        raise RegistryError(f"could not inspect record input: {error}") from error
    if mode & 0o077:
        raise RegistryError(f"record input is group- or world-accessible (mode {mode:o})")
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise RegistryError(f"could not read record input: {error}") from error
    if not isinstance(payload, dict):
        raise RegistryError("record input must be one JSON object")
    extra = set(payload) - INPUT_FIELDS
    if extra:
        raise RegistryError(f"record input has unsupported fields: {', '.join(sorted(extra))}")
    incident_id = payload.pop("incident_id", None)
    payload["updated_at"] = utc_now()
    validate_record(incident_id, payload)
    return incident_id, payload


def record_matches(record, fingerprint):
    return fingerprint == record["credential_fingerprint"] or fingerprint in record["finding_fingerprints"]


def print_record(incident_id, record):
    print(json.dumps({"incident_id": incident_id, **record}, indent=2, sort_keys=True))


def command_list(state):
    for incident_id, record in sorted(state["incidents"].items()):
        print(
            "\t".join(
                (
                    incident_id,
                    record["provider"],
                    record["status"],
                    record["last_verified_result"],
                    record["last_verified_at"],
                    record["next_action"],
                    record["credential_fingerprint"],
                    str(len(record["finding_fingerprints"])),
                )
            )
        )


def build_parser():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--registry", type=Path)
    parser.add_argument("--start", type=Path, default=Path.cwd())
    subparsers = parser.add_subparsers(dest="command", required=True)
    subparsers.add_parser("path")
    subparsers.add_parser("validate")
    subparsers.add_parser("list")
    show = subparsers.add_parser("show")
    show.add_argument("--id", required=True)
    lookup = subparsers.add_parser("lookup")
    lookup.add_argument("--fingerprint", required=True)
    record = subparsers.add_parser("record")
    record.add_argument("--input", required=True, type=Path)
    return parser


def main():
    args = build_parser().parse_args()
    path = (args.registry or default_registry_path(args.start)).expanduser()
    if not path.is_absolute():
        path = Path.cwd() / path
    path = Path(os.path.abspath(path))
    if args.command == "path":
        print(path)
        return 0
    state = load_state(path)
    if args.command == "validate":
        print(f"valid: {path} ({len(state['incidents'])} incidents)")
        return 0
    if args.command == "list":
        command_list(state)
        return 0
    if args.command == "show":
        record = state["incidents"].get(args.id)
        if record is None:
            raise RegistryError(f"unknown incident: {args.id}")
        print_record(args.id, record)
        return 0
    if args.command == "lookup":
        if not FINGERPRINT.fullmatch(args.fingerprint):
            raise RegistryError("lookup fingerprint must be 20 hexadecimal characters")
        matches = [
            (incident_id, record)
            for incident_id, record in state["incidents"].items()
            if record_matches(record, args.fingerprint)
        ]
        if not matches:
            return 1
        for incident_id, record in sorted(matches):
            print_record(incident_id, record)
        return 0
    if args.command == "record":
        incident_id, record = read_input(args.input)
        state["incidents"][incident_id] = record
        write_state(path, state)
        print(f"recorded: {incident_id}")
        return 0
    raise RegistryError(f"unsupported command: {args.command}")


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except RegistryError as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(2)
