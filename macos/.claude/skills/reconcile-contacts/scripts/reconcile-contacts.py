#!/usr/bin/env python3
"""Compare supported projections of explicitly selected BBDB and Contacts files.

Reports what is missing from each side, duplicates within each side, and any
match that is too ambiguous to trust. Read-only: it never edits either store.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import re
import signal
import sqlite3
import stat
import subprocess
import sys
import tempfile
import threading
import time
import unicodedata
from collections import Counter, defaultdict
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.parse import urlsplit

MAX_BBDB_BYTES = 16 * 1024 * 1024
MAX_CONTACT_ROWS = 100000
# ZABCDRECORD stores every Core Data record kind; only this entity is a contact
# card. Groups, the account container and the store's info row are not
# unprojected contacts and must not make an otherwise complete read incomplete.
CONTACT_ENTITY_NAMES = frozenset({"ABCDContact"})


class InputError(ValueError):
    """A selected source could not yield a complete supported projection."""


class ParserInterrupted(InputError):
    """The owned batch parser was cancelled and its group cleanup completed."""

    status = "interrupted"

    def __init__(self, message, signum=signal.SIGINT):
        super().__init__(message)
        self.signum = int(signum)
        self.exit_code = 128 + self.signum

# Facebook path segments that are not profile identifiers.
FACEBOOK_NON_PROFILE = {"pages", "groups", "events", "profile.php", "people", "sharer"}
APPLE_REFERENCE_DATE = datetime(2001, 1, 1, tzinfo=timezone.utc)


def normalize(value: str) -> str:
    """Casefold VALUE, strip accents and punctuation, collapse whitespace.

    A synthetic pair such as "José" and "Jose" illustrates an accent-only
    difference. Normalization supports comparison, not proof of identity.
    """
    if not value:
        return ""
    decomposed = unicodedata.normalize("NFKD", str(value))
    stripped = "".join(c for c in decomposed if not unicodedata.combining(c))
    return " ".join("".join(c if c.isalnum() else " " for c in stripped.casefold()).split())


def join_names(*parts: str) -> str:
    """Normalize the non-empty PARTS joined in order."""
    return normalize(" ".join(p for p in parts if p and p.strip()))


def facebook_id(url: str) -> str | None:
    """Return the profile identifier in URL, or None if it is not one.

    Shared segments like /pages/ and /groups/ would otherwise link every
    contact that happens to reference a Facebook page.
    """
    if not url:
        return None
    try:
        parsed = urlsplit(str(url))
    except ValueError:
        return None
    if (parsed.scheme not in ("http", "https")
            or parsed.hostname not in ("facebook.com", "www.facebook.com", "m.facebook.com")
            or parsed.username is not None or parsed.password is not None):
        return None
    segments = parsed.path.strip("/").split("/")
    if len(segments) != 1 or not re.fullmatch(r"[A-Za-z0-9.]+", segments[0]):
        return None
    ident = segments[0].lower()
    return None if ident in FACEBOOK_NON_PROFILE else ident


def is_person(record: dict) -> bool:
    """Return True if RECORD carries a personal name rather than only an org."""
    return any(record.get(key, "").strip() for key in ("first", "mid", "last", "maiden", "nick", "sfx"))


def match_keys(record: dict) -> set[str]:
    """Return the keys RECORD may be matched on, namespaced by kind.

    An organization is only a key for records that have no personal name.
    Employers are shared: keying people on them makes every colleague match
    every other colleague, which silently collapses distinct people into one.
    """
    keys: set[str] = set()
    for address in record.get("mail", []):
        if address and "@" in address:
            keys.add("mail:" + address.strip().lower())
    for url in record.get("urls", []):
        ident = facebook_id(url)
        if ident:
            keys.add("facebook:" + ident)
    for name in name_variants(record):
        keys.add("name:" + name)
    if not is_person(record):
        for org in record.get("org", []):
            if normalize(org):
                keys.add("name:" + normalize(org))
    return keys


def name_variants(record: dict) -> set[str]:
    """Return the name spellings RECORD might legitimately be recorded under.

    Surname-derived variants require a surname. Original single names and
    supplied aliases remain weak name-only candidates, never identity proof.
    """
    first = record.get("first", "")
    middle = record.get("mid", "")
    last = record.get("last", "")
    maiden = record.get("maiden", "")
    nickname = record.get("nick", "")
    suffix = record.get("sfx", "")
    variants = {
        join_names(first, middle, last),
        join_names(first, last),
        join_names(first, middle, last, suffix),
        join_names(first, last, suffix),
    }
    if nickname and last.strip():
        variants.add(join_names(nickname, last))
    if maiden:
        variants.add(join_names(first, maiden))
    for alias in record.get("aka", []):
        if normalize(alias):
            variants.add(normalize(alias))
    return variants - {""}


def display_name(record: dict) -> str:
    """Return a human-readable label for RECORD."""
    parts = [record.get(k, "") for k in ("first", "mid", "last", "sfx")]
    name = " ".join(p for p in parts if p).strip()
    return name or "; ".join(record.get("org", [])) or "(unnamed)"


def normalize_birthday(value: str | int | float | None) -> str | None:
    """Return VALUE as ``YYYY-MM-DD`` or a year-less ``MM-DD``.

    Interpret numeric values using the supported source convention of UTC
    seconds from 2001-01-01 and missing year 1604. This private Contacts encoding
    assumption is not a universal Apple API contract.
    """
    if value is None or value == "":
        return None
    if isinstance(value, bool):
        raise ValueError("Unsupported birthday type")
    if isinstance(value, (int, float)):
        if not math.isfinite(value):
            raise ValueError("Unsupported non-finite birthday")
        date = (APPLE_REFERENCE_DATE + timedelta(seconds=float(value))).date()
        return date.strftime("%m-%d" if date.year == 1604 else "%Y-%m-%d")
    text = str(value).strip()
    if re.fullmatch(r"\d{4}-\d{1,2}-\d{1,2}", text):
        date = datetime.strptime(text, "%Y-%m-%d").date()
        return date.strftime("%m-%d" if date.year == 1604 else "%Y-%m-%d")
    if re.fullmatch(r"\d{2}-\d{2}", text):
        datetime.strptime(f"2000-{text}", "%Y-%m-%d")
        return text
    if re.fullmatch(r"\d{2}-\d{2}-\d{4}", text):
        if int(text[:2]) <= 12 and int(text[3:5]) <= 12:
            raise ValueError("Ambiguous legacy birthday order requires source-specific format metadata")
        date = datetime.strptime(text, "%d-%m-%Y").date()
        return date.strftime("%m-%d" if date.year == 1604 else "%Y-%m-%d")
    raise ValueError(f"Unsupported birthday value: {value!r}")


def regular_source(path: Path) -> Path:
    """Resolve an explicitly selected regular local source PATH."""
    try:
        selected = path.expanduser().resolve(strict=True)
        if not stat.S_ISREG(selected.stat().st_mode):
            raise InputError("Selected input must be a regular local file")
        return selected
    except OSError as error:
        raise InputError("Selected input is unavailable") from error


def validate_ids(records: list[dict]) -> None:
    """Reject absent, malformed, or duplicate within-store record identifiers."""
    values = [record.get("uuid") for record in records]
    if any(not isinstance(value, str) or not value.strip() for value in values):
        raise InputError("Selected projection contains missing or non-string UUIDs")
    if len(set(values)) != len(values):
        raise InputError("Selected projection contains duplicate UUIDs")


def bbdb_source_bytes(selected: Path):
    """Read bounded regular SELECTED bytes through a validated descriptor."""
    descriptor = os.open(selected, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
    try:
        before = os.fstat(descriptor)
        if not stat.S_ISREG(before.st_mode) or before.st_size > MAX_BBDB_BYTES:
            raise InputError("Unsupported BBDB input type or 16 MiB limit")
        chunks = []
        remaining = MAX_BBDB_BYTES + 1
        while remaining:
            chunk = os.read(descriptor, min(65536, remaining))
            if not chunk:
                break
            chunks.append(chunk)
            remaining -= len(chunk)
        after = os.fstat(descriptor)
        identity = lambda info: (info.st_dev, info.st_ino, info.st_size, info.st_mtime_ns, info.st_ctime_ns)
        data = b"".join(chunks)
        if len(data) > MAX_BBDB_BYTES or len(data) != before.st_size or identity(before) != identity(after):
            raise InputError("BBDB changed or exceeded limits during the bounded read")
        return data, identity(after)
    finally:
        os.close(descriptor)


def run_bbdb_dump(argv, *, env=None, timeout=60):
    """Run a fresh batch parser and bound its entire newly owned process group."""
    if threading.current_thread() is not threading.main_thread():
        raise InputError("Batch parser cancellation supervision requires the main thread")
    child = None
    cancelled = 0
    stopping = False
    previous = {}

    def interrupted(signum, _frame):
        nonlocal cancelled
        if not cancelled:
            cancelled = signum
        if child is not None and not stopping:
            raise KeyboardInterrupt

    try:
        for interruption in (signal.SIGTERM, signal.SIGHUP, signal.SIGINT):
            previous[interruption] = signal.getsignal(interruption)
            signal.signal(interruption, interrupted)
        try:
            if cancelled:
                raise KeyboardInterrupt
            child = subprocess.Popen(argv, stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE, text=True, env=env, start_new_session=True)
            if cancelled:
                raise KeyboardInterrupt
            stdout, stderr = child.communicate(timeout=timeout)
        except (subprocess.TimeoutExpired, KeyboardInterrupt) as error:
            stopping = True
            if child is not None:
                try:
                    os.killpg(child.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                try:
                    child.communicate(timeout=5)
                except subprocess.TimeoutExpired:
                    raise InputError("BBDB parser stopped responding; owned-child cleanup unconfirmed") from None
            if isinstance(error, KeyboardInterrupt):
                raise ParserInterrupted("BBDB parser interrupted; no complete projection",
                                        cancelled or signal.SIGINT) from None
            raise InputError("BBDB parser exceeded the bounded batch deadline") from None
        return subprocess.CompletedProcess(argv, child.returncode, stdout, stderr)
    finally:
        stopping = True
        if child is not None:
            child.stdout.close()
            child.stderr.close()
        for interruption, handler in previous.items():
            signal.signal(interruption, handler)


def load_bbdb(bbdb_file: Path, dump_script: Path, *, metadata=None) -> list[dict]:
    """Read complete strict format-9 BBDB_FILE through a new batch Emacs."""
    selected = regular_source(bbdb_file)
    try:
        before, identity = bbdb_source_bytes(selected)
        before.decode("utf-8")
        with tempfile.TemporaryDirectory(prefix="reconcile-bbdb-", dir="/private/tmp" if sys.platform == "darwin" else "/tmp") as staging:
            snapshot = Path(staging) / "input.bbdb"
            with snapshot.open("xb") as stream:
                stream.write(before)
            snapshot.chmod(0o600)
            result = run_bbdb_dump(
                ["emacs", "-Q", "--batch", "-l", str(dump_script.resolve(strict=True))],
                env=dict(os.environ, BBDB_FILE=str(snapshot)),
            )
        if result.returncode != 0:
            raise InputError("BBDB dump failed; no complete supported projection")
        raw_records = json.loads(result.stdout)
        if not isinstance(raw_records, list) or len(raw_records) > 50000:
            raise InputError("Unsupported BBDB dump shape or record limit")
        after, after_identity = bbdb_source_bytes(selected)
        if after != before or after_identity != identity:
            raise InputError("BBDB changed while the projection was read")
        records = []
        for raw in raw_records:
            records.append({
                "first": raw["first"], "mid": "", "last": raw["last"],
                "maiden": "", "nick": "", "sfx": " ".join(raw.get("affix", [])),
                "affix": list(raw.get("affix", [])),
                "org": list(raw["org"]), "aka": list(raw["aka"]),
                "mail": list(raw["mail"]), "urls": [raw["url"]] if raw["url"] else [],
                "phones": [(p["label"], p["number"]) for p in raw["phones"]],
                "phone_components": [p["raw_components"] for p in raw["phones"]],
                "birthday": raw["birthday"], "birthday_type": raw["birthday_type"], "uuid": raw["uuid"],
            })
        validate_ids(records)
    except (OSError, UnicodeError, subprocess.SubprocessError, json.JSONDecodeError,
            KeyError, TypeError) as error:
        raise InputError("BBDB could not yield a complete supported projection") from error
    unsupported_birthdays = sum(record["birthday_type"] == "unsupported" for record in records)
    if metadata is not None:
        metadata.update(status="incomplete" if unsupported_birthdays else "complete", path=str(selected), format="BBDB file-format 9",
                        sha256=hashlib.sha256(before).hexdigest(), records=len(records),
                        unsupported_birthday_types=unsupported_birthdays,
                        limitations=["Strict raw reader of an owned private off-Drive byte snapshot, removed after parsing",
                                     "No normal BBDB loading or migration; source writers can still invalidate later use",
                                     "16 MiB and 50000 record limits; unprojected fields remain unassessed"])
    elif unsupported_birthdays:
        raise InputError("BBDB has unsupported birthday types; metadata is required to expose this gap")
    return records


def find_carddav_source() -> Path:
    """Refuse account guessing; callers must select an explicit Contacts store."""
    raise InputError("Select an explicit Contacts database; account size cannot identify Google")


def load_contacts(db_path: Path, *, metadata=None) -> list[dict]:
    """Read the supported private Contacts schema at explicitly selected DB_PATH.

    Use one read transaction across all tables. SQLite mode=ro prevents database
    SQL writes but can require/create WAL/SHM sidecars; this is not a promise of
    zero filesystem activity or a supported public Apple database API.
    """
    selected = regular_source(db_path)
    con = None
    records = {}
    row_count = 0
    orphan_rows = Counter()
    non_contact_rows = Counter()
    started = time.monotonic()
    try:
        con = sqlite3.connect(selected.as_uri() + "?mode=ro", uri=True, timeout=5)
        con.execute("PRAGMA query_only = ON")
        con.set_progress_handler(
            lambda: int(time.monotonic() - started > 30), 10000)
        con.execute("BEGIN")

        def rows(sql):
            nonlocal row_count
            for row in con.execute(sql):
                row_count += 1
                if row_count > MAX_CONTACT_ROWS:
                    raise InputError("Contacts exceeds the supported table-row limit")
                yield row

        entity_names = {}
        for entity, name in rows("select Z_ENT, Z_NAME from Z_PRIMARYKEY"):
            if not isinstance(entity, int) or not isinstance(name, str) or entity in entity_names:
                raise InputError("Unsupported Contacts entity table")
            entity_names[entity] = name
        for row in rows(
            "select Z_PK, Z_ENT, coalesce(ZFIRSTNAME,''), coalesce(ZMIDDLENAME,''), "
            "coalesce(ZLASTNAME,''), coalesce(ZMAIDENNAME,''), coalesce(ZNICKNAME,''), "
            "coalesce(ZSUFFIX,''), coalesce(ZORGANIZATION,''), ZBIRTHDAY, ZUNIQUEID "
            "from ZABCDRECORD"
        ):
            pk, entity, first, mid, last, maiden, nick, sfx, org, birthday, uid = row
            entity_name = entity_names.get(entity)
            if entity_name is None:
                raise InputError("Unsupported Contacts record entity")
            if entity_name not in CONTACT_ENTITY_NAMES:
                non_contact_rows[entity_name] += 1
                continue
            if not isinstance(pk, int) or pk in records or any(
                    not isinstance(value, str) for value in
                    (first, mid, last, maiden, nick, sfx, org)):
                raise InputError("Unsupported Contacts record field types")
            records[pk] = {
                "first": first, "mid": mid, "last": last, "maiden": maiden,
                "nick": nick, "sfx": sfx, "org": [org] if org.strip() else [],
                "aka": [], "mail": [], "urls": [], "phones": [],
                "birthday": birthday, "uuid": uid,
            }
        for owner, address in rows(
            "select ZOWNER, ZADDRESS from ZABCDEMAILADDRESS where ZADDRESS is not null"
        ):
            if owner in records:
                if not isinstance(address, str):
                    raise InputError("Unsupported Contacts email field type")
                records[owner]["mail"].append(address)
            else:
                orphan_rows["email"] += 1
        for owner, url in rows(
            "select ZOWNER, ZURL from ZABCDURLADDRESS where ZURL is not null"
        ):
            if owner in records:
                if not isinstance(url, str):
                    raise InputError("Unsupported Contacts URL field type")
                records[owner]["urls"].append(url)
            else:
                orphan_rows["url"] += 1
        for owner, label, number in rows(
            "select ZOWNER, ZLABEL, ZFULLNUMBER from ZABCDPHONENUMBER "
            "where ZFULLNUMBER is not null"
        ):
            if owner in records:
                if not isinstance(number, str) or (label is not None and not isinstance(label, str)):
                    raise InputError("Unsupported Contacts phone field type")
                records[owner]["phones"].append((label or "main", number))
            else:
                orphan_rows["phone"] += 1
        included = [record for record in records.values()
                    if any(record[key].strip() for key in ("first", "mid", "last", "maiden", "nick", "sfx"))
                    or any(value.strip() for key in ("org", "mail", "urls") for value in record[key])
                    or record["phones"] or record["birthday"] is not None]
        validate_ids(included)
    except sqlite3.Error as error:
        raise InputError("Contacts schema/read is unsupported or unavailable; no complete projection") from error
    finally:
        if con is not None:
            con.close()
    if metadata is not None:
        metadata.update(status="incomplete" if len(included) != len(records) or orphan_rows else "complete", path=str(selected),
                        schema="Private ZABCDRECORD/email/URL/phone column projection",
                        rows_read=row_count, records=len(included),
                        omitted_without_supported_fields=len(records) - len(included),
                        non_contact_rows=dict(sorted(non_contact_rows.items())),
                        orphan_related_rows=dict(orphan_rows),
                        birthday_encoding="Assumed Apple 2001 UTC seconds; year 1604 means yearless",
                        limitations=["One SQLite read transaction, not a whole-store export",
                                     "SQL read-only may still create/use WAL or SHM sidecars",
                                     "Private schema and birthday conventions require source-specific review",
                                     "Rows without supported fields may still have unprojected addresses, notes or photos",
                                     "Only ABCDContact entity rows are contacts; groups and account metadata rows are excluded and counted",
                                     "100000 total table rows and 30-second query budget"])
    elif len(included) != len(records) or orphan_rows:
        raise InputError("Contacts projection has omitted or orphan rows; metadata is required to expose these gaps")
    return included


def build_index(records: list[dict]) -> dict[str, set[int]]:
    """Return a map from match key to the indices of RECORDS carrying it."""
    index: dict[str, set[int]] = defaultdict(set)
    for i, record in enumerate(records):
        for key in match_keys(record):
            index[key].add(i)
    return index


def find_duplicates(records: list[dict]) -> list[list[dict]]:
    """Return groups of RECORDS sharing an identical normalized full name."""
    groups: dict[str, list[dict]] = defaultdict(list)
    for record in records:
        key = join_names(record["first"], record["mid"], record["last"])
        key = key or normalize("; ".join(record["org"]))
        if key:
            groups[key].append(record)
    return [g for g in groups.values() if len(g) > 1]


def reconcile(bbdb: list[dict], contacts: list[dict]) -> dict:
    """Return the differences between BBDB and CONTACTS."""
    bbdb_index = build_index(bbdb)
    contacts_index = build_index(contacts)
    birthday_errors = []
    birthday_values = {}
    identity_errors = []
    for side, records in (("bbdb", bbdb), ("contacts", contacts)):
        counts = Counter(record.get("uuid") for record in records
                         if isinstance(record.get("uuid"), str))
        for index, record in enumerate(records):
            uid = record.get("uuid")
            if not isinstance(uid, str) or not uid.strip() or counts[uid] != 1:
                identity_errors.append({"side": side, "record_index": index,
                                        "reason": "missing, non-string or duplicate UUID"})
            try:
                if record.get("birthday_type") == "unsupported":
                    raise ValueError("Unsupported source birthday type")
                birthday_values[side, index] = normalize_birthday(record.get("birthday"))
            except (ValueError, TypeError, OverflowError):
                birthday_errors.append({"side": side, "record_index": index,
                                        "name": display_name(record),
                                        "reason": "unsupported or invalid birthday encoding"})

    def missing_from(records, other_index):
        return [display_name(r) for r in records
                if not any(k in other_index for k in match_keys(r))]

    contacts_only = missing_from(contacts, bbdb_index)
    bbdb_only = missing_from(bbdb, contacts_index)

    # A key held by several records on one side cannot identify anyone. Surface
    # them: a polluted key is the usual cause of a falsely clean reconciliation.
    ambiguous = []
    for side, index, records in (("bbdb", bbdb_index, bbdb),
                                 ("contacts", contacts_index, contacts)):
        for key, owners in index.items():
            if len(owners) > 1:
                ambiguous.append({
                    "side": side, "kind": key.split(":", 1)[0],
                    "key": key.split(":", 1)[1], "count": len(owners),
                    "records": sorted(display_name(records[i]) for i in owners),
                })
    ambiguous.sort(key=lambda a: -a["count"])

    # Many-to-one key overlap is a review candidate, not evidence that the
    # records are duplicates or that any record may be deleted.
    def collapsing(records, other, other_index):
        groups = []
        seen = set()
        owners = defaultdict(list)
        for i, record in enumerate(records):
            hits = set()
            for key in match_keys(record):
                hits |= other_index.get(key, set())
            if len(hits) == 1:
                owners[next(iter(hits))].append(i)
        for target, members in owners.items():
            if len(members) > 1 and target not in seen:
                seen.add(target)
                groups.append({
                    "target": display_name(other[target]),
                    "records": sorted(display_name(records[i]) for i in members),
                })
        return sorted(groups, key=lambda g: -len(g["records"]))

    # Compare birthdays only for reciprocal one-to-one matches. A record that
    # reaches several candidates through its keys is not safe to adjudicate.
    birthday_bbdb_only = []
    birthday_contacts_only = []
    birthday_conflicts = []
    birthday_precision_differences = []
    candidate_matches = []
    ambiguous_matches = []
    birthday_not_compared = []
    compared_contacts = set()
    for bbdb_i, bbdb_record in enumerate(bbdb):
        contact_hits = set()
        for key in match_keys(bbdb_record):
            contact_hits |= contacts_index.get(key, set())
        if len(contact_hits) != 1:
            birthday_not_compared.append({"side": "bbdb", "record_index": bbdb_i,
                                           "reason": "no reciprocal unique candidate"})
            if contact_hits:
                ambiguous_matches.append({"side": "bbdb", "record_index": bbdb_i,
                                          "candidate_indices": sorted(contact_hits)})
            continue
        contacts_i = next(iter(contact_hits))
        contact_record = contacts[contacts_i]
        bbdb_hits = set()
        for key in match_keys(contact_record):
            bbdb_hits |= bbdb_index.get(key, set())
        if bbdb_hits != {bbdb_i}:
            ambiguous_matches.append({"side": "contacts", "record_index": contacts_i,
                                      "candidate_indices": sorted(bbdb_hits)})
            birthday_not_compared.append({"side": "bbdb", "record_index": bbdb_i,
                                           "reason": "no reciprocal unique candidate"})
            continue
        common = sorted(match_keys(bbdb_record) & match_keys(contact_record))
        name_only = all(key.startswith("name:") for key in common)
        candidate_matches.append({
            "bbdb_index": bbdb_i, "contacts_index": contacts_i,
            "bbdb_uuid": bbdb_record.get("uuid"), "contacts_uuid": contact_record.get("uuid"),
            "evidence": common,
            "assessment": "name_only_candidate" if name_only else "shared_contact_key_candidate",
            "requires_review": name_only or normalize(display_name(bbdb_record)) != normalize(display_name(contact_record)),
        })
        compared_contacts.add(contacts_i)
        if ("bbdb", bbdb_i) not in birthday_values or ("contacts", contacts_i) not in birthday_values:
            birthday_not_compared.append({"side": "pair", "bbdb_index": bbdb_i,
                                           "contacts_index": contacts_i, "reason": "invalid birthday encoding"})
            continue
        bbdb_birthday = birthday_values["bbdb", bbdb_i]
        contacts_birthday = birthday_values["contacts", contacts_i]
        if bbdb_birthday == contacts_birthday:
            continue
        entry = {
            "bbdb_name": display_name(bbdb_record),
            "contacts_name": display_name(contact_record),
            "bbdb_birthday": bbdb_birthday,
            "contacts_birthday": contacts_birthday,
            "bbdb_uuid": bbdb_record.get("uuid"),
            "contacts_uuid": contact_record.get("uuid"),
        }
        if bbdb_birthday and not contacts_birthday:
            birthday_bbdb_only.append(entry)
        elif contacts_birthday and not bbdb_birthday:
            birthday_contacts_only.append(entry)
        elif len(bbdb_birthday) != len(contacts_birthday) and bbdb_birthday[-5:] == contacts_birthday[-5:]:
            birthday_precision_differences.append(entry)
        else:
            birthday_conflicts.append(entry)

    for index in range(len(contacts)):
        if index not in compared_contacts:
            birthday_not_compared.append({"side": "contacts", "record_index": index,
                                           "reason": "no reciprocal unique candidate"})

    for entries in (birthday_bbdb_only, birthday_contacts_only,
                    birthday_conflicts, birthday_precision_differences):
        entries.sort(key=lambda entry: normalize(entry["bbdb_name"]))

    report = {
        "schema_version": 2,
        "bbdb_count": len(bbdb),
        "contacts_count": len(contacts),
        "contacts_collapsing": collapsing(contacts, bbdb, bbdb_index),
        "bbdb_collapsing": collapsing(bbdb, contacts, contacts_index),
        "contacts_only": sorted(contacts_only),
        "bbdb_only": sorted(bbdb_only),
        "bbdb_duplicates": [[display_name(r) for r in g]
                            for g in find_duplicates(bbdb)],
        "contacts_duplicates": [[display_name(r) for r in g]
                                for g in find_duplicates(contacts)],
        "ambiguous_keys": ambiguous,
        "birthday_bbdb_only": birthday_bbdb_only,
        "birthday_contacts_only": birthday_contacts_only,
        "birthday_conflicts": birthday_conflicts,
        "birthday_precision_differences": birthday_precision_differences,
        "birthday_errors": birthday_errors,
        "birthday_not_compared": birthday_not_compared,
        "identity_errors": identity_errors,
        "candidate_matches": candidate_matches,
        "ambiguous_matches": ambiguous_matches,
        "coverage": {
            "comparison": "Candidate overlap by names, email and profile URLs; birthdays only for reciprocal unique candidates",
            "birthday_candidate_pairs": len(compared_contacts),
            "birthday_unassessed_entries": len(birthday_not_compared),
            "limitations": ["Candidate overlap is not proof that records identify the same person",
                            "No application, cloud account, sync state, or complete field equality was checked",
                            "Names, same-name groups and collapsing groups are review candidates, never merge instructions",
                            "Contacts birthday UTC/1604 convention is a source-specific assumption"],
        },
    }
    report["status"] = ("incomplete" if birthday_errors or identity_errors or not bbdb or not contacts
                        else "review_required" if self_check(report) else "no_reported_differences")
    return report


def self_check(report: dict) -> list[str]:
    """Return unresolved findings; passing is not proof of store identity or sync."""
    problems = []
    birthday_mismatches = sum(len(report[key]) for key in (
        "birthday_bbdb_only", "birthday_contacts_only", "birthday_conflicts",
        "birthday_precision_differences"))
    if birthday_mismatches:
        problems.append(f"{birthday_mismatches} reciprocal candidate pair(s) have differing birthday data")
    for side in ("bbdb", "contacts"):
        if report[f"{side}_count"] == 0:
            problems.append(f"Loaded zero {side} records; comparison is incomplete")
    if report["bbdb_count"] != report["contacts_count"]:
        problems.append("Projected source record counts differ")
    for key in ("contacts_only", "bbdb_only", "bbdb_duplicates", "contacts_duplicates",
                "contacts_collapsing", "bbdb_collapsing", "ambiguous_keys",
                "ambiguous_matches", "birthday_errors", "birthday_not_compared", "identity_errors"):
        if report[key]:
            problems.append(f"{key}: {len(report[key])} unresolved item(s)")
    candidates = sum(entry["requires_review"] for entry in report["candidate_matches"])
    if candidates:
        problems.append(f"{candidates} name-only or differently named candidate pair(s) require identity review")
    return problems


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bbdb-file", type=Path, required=True)
    parser.add_argument("--contacts-db", type=Path, required=True,
                        help="explicit selected private Contacts SQLite store; no account discovery")
    parser.add_argument("--json", action="store_true", help="emit the private supported-projection report")
    parser.add_argument("--limit", type=int, default=40, help="positive maximum entries per text section")
    args = parser.parse_args(argv)
    if args.limit <= 0:
        parser.error("--limit must be positive")
    sources = {"bbdb": {}, "contacts": {}}
    try:
        dump_script = Path(__file__).resolve().parent / "dump-bbdb.el"
        bbdb = load_bbdb(args.bbdb_file, dump_script, metadata=sources["bbdb"])
        contacts = load_contacts(args.contacts_db, metadata=sources["contacts"])
        report = reconcile(bbdb, contacts)
    except (InputError, OSError) as error:
        failure = {"schema_version": 2, "status": getattr(error, "status", "input_error"),
                   "reason": str(error) if isinstance(error, InputError) else "Selected input is unavailable"}
        if isinstance(error, ParserInterrupted):
            failure["signal"] = error.signum
        print(json.dumps(failure, ensure_ascii=True))
        return getattr(error, "exit_code", 2)
    report["sources"] = sources
    source_problems = [f"{side}: selected-source projection has source-specific coverage gaps"
                       for side, evidence in sources.items() if evidence.get("status") != "complete"]
    if source_problems:
        report["status"] = "incomplete"
    report["coverage"]["source_problems"] = source_problems
    report["projection_sha256"] = hashlib.sha256(
        json.dumps({"bbdb": bbdb, "contacts": contacts}, sort_keys=True,
                   ensure_ascii=True, separators=(",", ":")).encode()).hexdigest()
    report["self_check_problems"] = self_check(report) + source_problems
    if args.json:
        print(json.dumps(report, indent=2, ensure_ascii=True, sort_keys=True))
    else:
        print(f"Status: {report['status']} (supported projection only, not identity or sync proof)")
        print(f"BBDB: {report['bbdb_count']}; Contacts: {report['contacts_count']}")
        for key in ("contacts_only", "bbdb_only", "bbdb_duplicates", "contacts_duplicates",
                    "contacts_collapsing", "bbdb_collapsing", "ambiguous_keys", "ambiguous_matches",
                    "birthday_bbdb_only", "birthday_contacts_only", "birthday_conflicts",
                    "birthday_precision_differences", "birthday_errors", "birthday_not_compared",
                    "identity_errors", "candidate_matches"):
            entries = report[key]
            print(f"{key}: {len(entries)}")
            for entry in entries[:args.limit]:
                rendered = json.dumps(entry, ensure_ascii=True, sort_keys=True)
                print("    " + (rendered if len(rendered) <= 1000 else rendered[:1000] + " ... [truncated; use --json]"))
            if len(entries) > args.limit:
                print(f"    ... {len(entries) - args.limit} more; full private report requires --json")
        for problem in report["self_check_problems"]:
            print("Review: " + problem)
    return 0 if report["status"] == "no_reported_differences" else 1


if __name__ == "__main__":
    sys.exit(main())
