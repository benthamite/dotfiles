#!/usr/bin/python3 -I
"""Persist redacted credential-incident state for future security audits."""

import argparse
import contextlib
import datetime as dt
import errno
import fcntl
import json
import os
import re
import stat
import subprocess
import sys
import time
import unicodedata
import uuid
from pathlib import Path


SCHEMA = 1
DIRECTORY_MODE = 0o700
FILE_MODE = 0o600
MAX_INPUT_BYTES = 256 * 1024
MAX_REGISTRY_BYTES = 8 * 1024 * 1024
LOCK_SECONDS = 10
GIT_EXECUTABLE = "/usr/bin/git"
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
    start = absolute_path(start)
    with open_directory(start):
        pass
    environment = {"PATH": "/usr/bin:/bin:/usr/sbin:/sbin", "LC_ALL": "C",
                   "GIT_CONFIG_NOSYSTEM": "1", "GIT_CONFIG_GLOBAL": os.devnull}
    try:
        completed = subprocess.run(
            [GIT_EXECUTABLE, "-C", str(start), "rev-parse", "--absolute-git-dir"],
            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True,
            timeout=10, env=environment,
        )
    except (OSError, subprocess.TimeoutExpired, UnicodeError) as error:
        raise RegistryError("could not resolve the selected Git directory") from error
    if completed.returncode == 0:
        output = completed.stdout.rstrip("\n")
        if not output or "\n" in output or not Path(output).is_absolute():
            raise RegistryError("Git returned an invalid directory")
        git_directory = absolute_path(output)
        with open_directory(git_directory):
            pass
        # Preserve the existing per-worktree Git-directory convention.
        return git_directory / "dotfiles-publish" / "credential-incidents.json"
    # A failed Git invocation is not by itself proof of a non-repository.
    # Inspect the selected ancestry without following links or denied paths.
    for ancestor in (start, *start.parents):
        with open_directory(ancestor) as descriptor:
            for marker in (".git", "HEAD", "objects"):
                try:
                    os.stat(marker, dir_fd=descriptor, follow_symlinks=False)
                except FileNotFoundError:
                    continue
                raise RegistryError("Git resolution failed in a possible repository; select --registry explicitly")
    if completed.returncode != 128:
        raise RegistryError("Git resolution failed; select --registry explicitly")
    state_home = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local/state"))
    if not state_home.is_absolute():
        raise RegistryError("XDG_STATE_HOME must be an absolute directory")
    state_home = absolute_path(state_home)
    return state_home / "security-audit" / "credential-incidents.json"


def reject_secret_text(value, field, *, identifier=False, repository_path=False):
    if not isinstance(value, str):
        return
    if any(unicodedata.category(character) in {"Cc", "Cf", "Cs"} for character in value):
        raise RegistryError(f"{field} contains control characters")
    for pattern in SECRET_TEXT[:-1]:
        if pattern.search(value):
            raise RegistryError(f"{field} appears to contain credential material")
    # Paths contain separators and provider IDs may be hexadecimal hashes.
    # Preserve these metadata shapes without exempting recognizable credentials.
    candidates = value.split("/") if repository_path else [value]
    for candidate in candidates:
        if (identifier or repository_path) and re.fullmatch(
            r"(?:[0-9a-fA-F]{32}|[0-9a-fA-F]{40}|[0-9a-fA-F]{64})", candidate
        ):
            continue
        if SECRET_TEXT[-1].search(candidate):
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
        raise RegistryError(f"unsupported {field}")
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
        if not isinstance(reference["kind"], str) or reference["kind"] not in REFERENCE_KINDS:
            raise RegistryError("unsupported provider reference kind")
        if not isinstance(reference["id"], str) or not REFERENCE_ID.fullmatch(reference["id"]):
            raise RegistryError("provider reference id has an unsafe format")
        reject_secret_text(reference["id"], "provider reference id", identifier=True)


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
        reject_secret_text(path, "location path", repository_path=True)
        commits = location.get("commits", [])
        if not isinstance(commits, list) or any(
            not isinstance(commit, str) or not COMMIT.fullmatch(commit) for commit in commits
        ):
            raise RegistryError("location commits must be full hexadecimal commit ids")
        require_fingerprints(location.get("finding_fingerprints", []), "location finding_fingerprints")


def validate_record(incident_id, record):
    if not isinstance(incident_id, str) or not INCIDENT_ID.fullmatch(incident_id):
        raise RegistryError("incident_id must be a lowercase hyphenated identifier")
    reject_secret_text(incident_id, "incident_id")
    if not isinstance(record, dict):
        raise RegistryError("incident must be an object")
    extra = set(record) - RECORD_FIELDS
    if extra:
        raise RegistryError("incident has unsupported fields")
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
    validate_calendar(verified_at, "last_verified_at")
    require_choice(record, "last_verified_result", VERIFICATION_RESULTS)
    require_choice(record, "verification_method", VERIFICATION_METHODS)
    validate_references(record.get("provider_references"))
    validate_locations(record.get("locations"))
    require_choice(record, "next_action", NEXT_ACTIONS)
    require_text(record, "summary", 400)
    updated_at = require_text(record, "updated_at", 20)
    if not RFC3339.fullmatch(updated_at):
        raise RegistryError("updated_at must be UTC RFC3339 without fractional seconds")
    validate_calendar(updated_at, "updated_at")


def validate_calendar(value, field):
    try:
        dt.datetime.strptime(value, "%Y-%m-%d" if len(value) == 10 else "%Y-%m-%dT%H:%M:%SZ")
    except ValueError as error:
        raise RegistryError(f"{field} has an invalid calendar date or time") from error


def validate_state(state):
    if not isinstance(state, dict) or set(state) != {"schema", "incidents"}:
        raise RegistryError("registry must contain only schema and incidents")
    if type(state["schema"]) is not int or state["schema"] != SCHEMA or not isinstance(state["incidents"], dict):
        raise RegistryError("unsupported credential incident registry schema")
    for incident_id, record in state["incidents"].items():
        validate_record(incident_id, record)
    return state


def absolute_path(value):
    path = Path(value).expanduser()
    if ".." in path.parts or any(unicodedata.category(char) in {"Cc", "Cf", "Cs"} for char in str(path)):
        raise RegistryError("path contains traversal or control characters")
    path = Path(os.path.abspath(path))
    # macOS owns these fixed aliases. Artifact and other ancestor links remain
    # refused; resolving an arbitrary symlink would choose a different store.
    if sys.platform == "darwin" and len(path.parts) > 1 and path.parts[1] in {"tmp", "var"}:
        path = Path("/private").joinpath(*path.parts[1:])
    return path


@contextlib.contextmanager
def open_directory(path, *, create=False, private=False):
    path = absolute_path(path)
    descriptor = os.open("/", os.O_RDONLY | os.O_DIRECTORY)
    try:
        for component in path.parts[1:]:
            try:
                child = os.open(component, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
                                dir_fd=descriptor)
            except FileNotFoundError:
                if not create:
                    raise
                try:
                    os.mkdir(component, DIRECTORY_MODE, dir_fd=descriptor)
                except FileExistsError:
                    pass
                child = os.open(component, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
                                dir_fd=descriptor)
            except OSError as error:
                if error.errno in {errno.ELOOP, errno.ENOTDIR}:
                    raise RegistryError("path ancestor is a symlink or is not a directory") from error
                raise
            os.close(descriptor)
            descriptor = child
        if private:
            info = os.fstat(descriptor)
            if info.st_uid != os.getuid():
                raise RegistryError("registry directory is not owned by the current user")
            mode = stat.S_IMODE(info.st_mode)
            if mode & 0o077:
                raise RegistryError(f"registry directory is group- or world-accessible (mode {mode:o})")
        yield descriptor
    finally:
        os.close(descriptor)


def file_identity(info):
    return (info.st_dev, info.st_ino, info.st_mode, info.st_uid, info.st_nlink,
            info.st_size, info.st_mtime_ns, info.st_ctime_ns)


def private_file(info, label):
    if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
        raise RegistryError(f"{label} must be a regular single-link file")
    if info.st_uid != os.getuid():
        raise RegistryError(f"{label} is not owned by the current user")
    mode = stat.S_IMODE(info.st_mode)
    if mode & 0o077:
        raise RegistryError(f"{label} is group- or world-accessible (mode {mode:o})")


def read_bytes_at(directory, name, label, limit):
    try:
        descriptor = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=directory)
    except OSError as error:
        if error.errno == errno.ELOOP:
            raise RegistryError(f"{label} is a symlink") from error
        raise
    try:
        before = os.fstat(descriptor)
        private_file(before, label)
        if before.st_size > limit:
            raise RegistryError(f"{label} exceeds its byte limit")
        chunks = []
        remaining = limit + 1
        while remaining:
            chunk = os.read(descriptor, min(65536, remaining))
            if not chunk:
                break
            chunks.append(chunk)
            remaining -= len(chunk)
        content = b"".join(chunks)
        after = os.fstat(descriptor)
        try:
            current = os.stat(name, dir_fd=directory, follow_symlinks=False)
        except FileNotFoundError as error:
            raise RegistryError(f"{label} changed during its bounded read") from error
        if (len(content) > limit or len(content) != before.st_size
                or file_identity(before) != file_identity(after)
                or file_identity(after) != file_identity(current)):
            raise RegistryError(f"{label} changed during its bounded read")
        return content
    finally:
        os.close(descriptor)


def decode_json(content, label):
    def object_pairs(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise RegistryError(f"{label} has duplicate JSON fields")
            result[key] = value
        return result

    def invalid_constant(_value):
        raise RegistryError(f"{label} has an unsupported JSON number")

    try:
        return json.loads(content.decode("utf-8"), object_pairs_hook=object_pairs,
                          parse_constant=invalid_constant)
    except (UnicodeError, ValueError, RecursionError) as error:
        raise RegistryError(f"could not decode {label}") from error


def load_state(path, *, directory=None):
    path = absolute_path(path)
    if directory is None:
        try:
            with open_directory(path.parent, private=True) as opened:
                return load_state(path, directory=opened)
        except FileNotFoundError:
            return {"schema": SCHEMA, "incidents": {}}
    try:
        content = read_bytes_at(directory, path.name, "registry", MAX_REGISTRY_BYTES)
    except FileNotFoundError:
        return {"schema": SCHEMA, "incidents": {}}
    return validate_state(decode_json(content, "registry"))


@contextlib.contextmanager
def writer_lock(directory, name):
    try:
        try:
            descriptor = os.open(name, os.O_RDWR | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                                 FILE_MODE, dir_fd=directory)
        except FileExistsError:
            descriptor = os.open(name, os.O_RDWR | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=directory)
        else:
            os.fchmod(descriptor, FILE_MODE)
    except OSError as error:
        raise RegistryError("could not open the private registry lock") from error
    try:
        private_file(os.fstat(descriptor), "registry lock")
        deadline = time.monotonic() + LOCK_SECONDS
        while True:
            try:
                fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
                break
            except BlockingIOError:
                if time.monotonic() >= deadline:
                    raise RegistryError("registry writer lock timed out")
                time.sleep(0.05)
        current = os.stat(name, dir_fd=directory, follow_symlinks=False)
        if file_identity(current) != file_identity(os.fstat(descriptor)):
            raise RegistryError("registry lock changed while waiting")
        yield
    finally:
        os.close(descriptor)


def write_state(path, state, *, directory):
    validate_state(state)
    encoded = (json.dumps(state, indent=2, sort_keys=True) + "\n").encode()
    if len(encoded) > MAX_REGISTRY_BYTES:
        raise RegistryError("updated registry exceeds its byte limit")
    # A competing helper uses the same stable lock. Read-only commands never
    # create that sidecar. No locking guarantee is made for other writers.
    temporary = path.name + ".tmp-" + uuid.uuid4().hex
    descriptor = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                         FILE_MODE, dir_fd=directory)
    try:
        os.fchmod(descriptor, FILE_MODE)
        with os.fdopen(descriptor, "wb") as handle:
            handle.write(encoded)
            handle.flush()
            os.fsync(handle.fileno())
        with open_directory(path.parent, private=True) as current_parent:
            if (os.fstat(current_parent).st_dev, os.fstat(current_parent).st_ino) != (
                    os.fstat(directory).st_dev, os.fstat(directory).st_ino):
                raise RegistryError("registry directory changed before replacement")
        try:
            current = os.stat(path.name, dir_fd=directory, follow_symlinks=False)
            private_file(current, "registry")
        except FileNotFoundError:
            pass
        os.replace(temporary, path.name, src_dir_fd=directory, dst_dir_fd=directory)
        os.fsync(directory)
    finally:
        try:
            os.unlink(temporary, dir_fd=directory)
        except FileNotFoundError:
            pass


def read_input(path):
    path = absolute_path(path)
    with open_directory(path.parent) as directory:
        payload = decode_json(read_bytes_at(directory, path.name, "record input", MAX_INPUT_BYTES),
                              "record input")
    if not isinstance(payload, dict):
        raise RegistryError("record input must be one JSON object")
    extra = set(payload) - INPUT_FIELDS
    if extra:
        raise RegistryError("record input has unsupported fields")
    incident_id = payload.pop("incident_id", None)
    payload["updated_at"] = utc_now()
    validate_record(incident_id, payload)
    return incident_id, payload


def record_matches(record, fingerprint):
    return (fingerprint == record["credential_fingerprint"] or fingerprint in record["finding_fingerprints"]
            or any(fingerprint in location.get("finding_fingerprints", []) for location in record["locations"]))


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
    path = absolute_path(args.registry or default_registry_path(args.start))
    if args.command == "path":
        print(path)
        return 0
    if args.command == "record":
        incident_id, record = read_input(args.input)
        with open_directory(path.parent, create=True, private=True) as directory:
            with writer_lock(directory, path.name + ".lock"):
                state = load_state(path, directory=directory)
                existing = state["incidents"].get(incident_id)
                if existing and existing["credential_fingerprint"] != record["credential_fingerprint"]:
                    raise RegistryError("existing incident_id belongs to a different credential fingerprint")
                state["incidents"][incident_id] = record
                write_state(path, state, directory=directory)
        print(f"recorded: {incident_id}")
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
            raise RegistryError("unknown incident")
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
    raise RegistryError("unsupported command")


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except RegistryError as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(2)
    except OSError:
        print("error: registry filesystem operation failed", file=sys.stderr)
        raise SystemExit(2)
