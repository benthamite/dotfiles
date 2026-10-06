"""Local Z-Library metadata index for ``paper-fetch book-candidates``.

Anna's Archive publishes the whole Z-Library catalogue as ``zlib3_records``
AAC dumps (one JSON record per line, zstd-compressed, about 24 GB). Anna's has
no search API, so this module turns the newest dump into a compact SQLite
index that ``paper_fetch.search_zlib_index`` queries by ISBN and by
title/author. The index stores metadata only: candidates found in it are
downloaded by md5 through the existing member fast-download.

``build_index`` streams a dump once. A Z-Library id appears in several
records (later crawls, Anna's ``annabookinfo`` lookups, removal notices); the
file is in AAC id order, which is time order, so each field keeps its latest
non-empty value. paper-fetch reviews only PDFs, so the index keeps only PDF
records with an md5 that Z-Library has not deleted for DMCA, minus the Sci-Hub
articles Z-Library mirrors (storage ``scimag``). A record that rules its file
out is stored as a narrow tombstone (the deciding fields only), so a later
record can still overturn it, and the final delete removes it.

``refresh`` finds the newest seeded dump in Anna's /dyn/torrents.json,
downloads it through the running Transmission daemon with seeding disabled,
rebuilds the index atomically, and deletes superseded dumps.
"""

from __future__ import annotations

import datetime
import fcntl
import json
import os
import re
import shutil
import sqlite3
import subprocess
import tempfile
import time
import unicodedata
import urllib.parse
from contextlib import contextmanager
from pathlib import Path
from typing import Callable, Iterator

import paper_fetch as pf

# Anna's AAC file name for a full Z-Library metadata dump; group 2 is the
# time of its newest record, which is the snapshot's as-of time.
DUMP_RE = re.compile(r"^annas_archive_meta__aacid__zlib3_records__"
                     r"([0-9]{8}T[0-9]{6}Z)--([0-9]{8}T[0-9]{6}Z)\.jsonl(?:\.seekable)?\.zst$")
TORRENTS_PATH = "dyn/torrents.json"
INDEX_NAME = "index.sqlite"
BATCH = 50_000
DOWNLOAD_POLL_SECONDS = 60
DOWNLOAD_TIMEOUT_SECONDS = 12 * 3600

# books.isbns: space-separated ISBN-10/13 digits as Z-Library lists them.
# books.deleted: annabookinfo deleted_comment, '', 'spam' or 'bad file' ('DMCA' rows are dropped).
# books.storage: Z-Library collection, used during the build to drop 'scimag', then dropped.
# (No SQL comments inside the table: ALTER TABLE DROP COLUMN rewrites its text.)
SCHEMA = """
CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT NOT NULL);
CREATE TABLE books (
  zlibrary_id INTEGER PRIMARY KEY,
  md5 TEXT, title TEXT, author TEXT, publisher TEXT, year TEXT, language TEXT,
  extension TEXT, filesize INTEGER, isbns TEXT, deleted TEXT, storage TEXT
);
CREATE TABLE isbn (isbn TEXT NOT NULL, zlibrary_id INTEGER NOT NULL,
                   PRIMARY KEY (isbn, zlibrary_id)) WITHOUT ROWID;
"""
FTS = ("CREATE VIRTUAL TABLE books_fts USING fts5(title, author, content='books', "
       "content_rowid='zlibrary_id', tokenize='unicode61 remove_diacritics 2')")
FIELDS = ("md5", "title", "author", "publisher", "year", "language", "extension", "filesize", "isbns",
          "deleted", "storage")
UPSERT = (f"INSERT INTO books (zlibrary_id, {', '.join(FIELDS)}) VALUES ({', '.join('?' * (len(FIELDS) + 1))}) "
          f"ON CONFLICT (zlibrary_id) DO UPDATE SET "
          + ", ".join(f"{field} = coalesce(excluded.{field}, {field})" for field in FIELDS))


class ZlibError(pf.PaperFetchError):
    """A dump, index or refresh step failed; the previous index is untouched."""


def snapshot_of(dump: Path) -> tuple[str, str]:
    """(first, last) record times of a dump, from its AAC file name, as ISO 8601."""
    match = DUMP_RE.match(dump.name)
    if not match:
        raise ZlibError(f"not a zlib3_records dump name: {dump.name}")
    return tuple(datetime.datetime.strptime(stamp, "%Y%m%dT%H%M%SZ").strftime("%Y-%m-%dT%H:%M:%SZ")
                 for stamp in match.groups())


def _text(value) -> str | None:
    if value is None:
        return None
    value = unicodedata.normalize("NFC", str(value)).strip()
    return value or None


def excluded(extension: str | None, storage: str | None, deleted: str | None) -> bool:
    return ((extension is not None and extension != "pdf") or storage == "scimag"
            or (deleted or "").upper() == "DMCA")


def extract(line: bytes) -> tuple | None:
    """One dump line -> an upsert row, or None when the line says nothing about a file.

    None in a field means "this record does not say"; the upsert then keeps the
    earlier value. '' in deleted is a statement (not deleted).
    """
    metadata = json.loads(line)["metadata"]
    response = (metadata.get("annabookinfo") or {}).get("response") or {}
    crawled = "md5_reported" in metadata or "title" in metadata
    if not crawled and not response:
        return None  # {"missing": 1}: Z-Library had no book under this id

    def field(key: str) -> str | None:
        return _text(metadata.get(key)) or _text(response.get(key))

    md5 = (metadata.get("md5_reported") or response.get("md5") or "").strip().lower()
    size = metadata.get("filesize_reported") or response.get("filesize")
    try:
        size = int(size) if size else None
    except (TypeError, ValueError):
        size = None
    isbns: list[str] = []
    identifiers = list(metadata.get("isbns") or []) + str(response.get("identifier") or "").split(",")
    for value in identifiers:
        digits = pf.isbn_digits(value)
        if digits and digits not in isbns:
            isbns.append(digits)
    return (int(metadata["zlibrary_id"]), md5 if pf.MD5_RE.fullmatch(md5) else None,
            field("title"), field("author"), field("publisher"), field("year"), field("language"),
            (field("extension") or "").lower() or None, size, " ".join(isbns) or None,
            (response.get("deleted_comment") or "").strip() if response else None,
            _text(response.get("storage")))


def _dump_lines(dump: Path) -> Iterator[bytes]:
    zstd = shutil.which("zstd")
    if not zstd:
        raise ZlibError("building the Z-Library index needs the zstd command")
    with tempfile.TemporaryFile() as errors:
        process = subprocess.Popen([zstd, "-dcq", str(dump)], stdout=subprocess.PIPE, stderr=errors,
                                   bufsize=1 << 20)
        try:
            yield from process.stdout
        finally:
            process.stdout.close()
            status = process.wait()
        if status != 0:
            errors.seek(0)
            detail = errors.read().decode("utf-8", "replace").strip().splitlines()
            raise ZlibError(f"zstd could not decompress {dump.name} (exit {status}"
                            + (f": {detail[-1].split(' : ')[-1][:200]}" if detail else "") + ")")


def build_index(dump: Path, out: Path, *, listed: str = "", btih: str = "",
                log: Callable[[str], None] = lambda _message: None) -> dict:
    """Stream DUMP once into a new index and atomically replace OUT with it."""
    dump = dump.expanduser().resolve()
    if not dump.is_file():
        raise ZlibError(f"dump not found: {dump}")
    first, snapshot = snapshot_of(dump)
    out = out.expanduser()
    if "My Drive" in str(out.parent.resolve()):
        raise ZlibError("the Z-Library index must not live under Google Drive")
    out.parent.mkdir(parents=True, exist_ok=True)
    # A killed build (launchd timeout, reboot) cannot clean up after itself;
    # callers hold refresh_lock, so any older partial file is an orphan.
    for orphan in out.parent.glob(f"{out.name}.partial-*"):
        orphan.unlink()
    partial = out.with_name(f"{out.name}.partial-{os.getpid()}")
    started = time.monotonic()
    connection = sqlite3.connect(partial, isolation_level=None)
    try:
        for pragma in ("journal_mode = OFF", "synchronous = OFF", "locking_mode = EXCLUSIVE",
                       "cache_size = -1000000", "temp_store = FILE"):
            connection.execute(f"PRAGMA {pragma}")
        connection.executescript(SCHEMA)
        connection.execute("BEGIN")
        lines = 0
        batch: list[tuple] = []
        for lines, line in enumerate(_dump_lines(dump), 1):
            try:
                row = extract(line)
            except (ValueError, KeyError, TypeError) as exc:
                raise ZlibError(f"{dump.name} line {lines} is not a zlib3 record ({type(exc).__name__})") from None
            if row is None:
                continue
            extension, deleted, storage = row[7], row[10], row[11]
            if excluded(extension, storage, deleted):
                row = (row[0], None, None, None, None, None, None, extension, None, None, deleted, storage)
            batch.append(row)
            if len(batch) >= BATCH:
                connection.executemany(UPSERT, batch)
                batch.clear()
            if lines % 10_000_000 == 0:
                log(f"zlib index: {lines:,} lines, {time.monotonic() - started:.0f}s")
        connection.executemany(UPSERT, batch)
        if not lines:
            raise ZlibError(f"{dump.name} is empty")
        dropped = connection.execute(
            "DELETE FROM books WHERE md5 IS NULL OR extension IS NOT 'pdf' OR storage IS 'scimag' "
            "OR deleted IS 'DMCA'").rowcount
        connection.execute("ALTER TABLE books DROP COLUMN storage")
        listed_isbns = connection.execute("SELECT zlibrary_id, isbns FROM books WHERE isbns IS NOT NULL")
        connection.executemany("INSERT OR IGNORE INTO isbn VALUES (?, ?)",
                               ((pf.isbn13(value), zlibrary_id) for zlibrary_id, isbns in listed_isbns
                                for value in isbns.split()))
        connection.execute("COMMIT")
        log(f"zlib index: records merged, building full-text index ({time.monotonic() - started:.0f}s)")
        connection.execute(FTS)
        connection.execute("INSERT INTO books_fts (books_fts) VALUES ('rebuild')")
        connection.execute("INSERT INTO books_fts (books_fts) VALUES ('optimize')")
        books = connection.execute("SELECT count(*) FROM books").fetchone()[0]
        isbns = connection.execute("SELECT count(*) FROM isbn").fetchone()[0]
        meta = {"schema": pf.ZLIB_INDEX_SCHEMA, "snapshot": snapshot, "snapshot_first_record": first,
                "dump": dump.name, "dump_bytes": str(dump.stat().st_size), "listed": listed, "btih": btih,
                "built_at": datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
                "lines": str(lines), "books": str(books), "isbn_rows": str(isbns),
                "dropped_after_merge": str(dropped), "scope": "pdf, md5, not DMCA, not scimag"}
        connection.executemany("INSERT INTO meta VALUES (?, ?)", meta.items())
        log(f"zlib index: compacting ({time.monotonic() - started:.0f}s)")
        connection.execute("VACUUM")
        connection.close()
        # synchronous=OFF skipped every sync; make the bytes durable before the
        # rename so a crash cannot leave a torn file in place of the old index.
        _full_fsync(partial)
        os.replace(partial, out)
        _full_fsync(out.parent)
    except BaseException:
        connection.close()
        partial.unlink(missing_ok=True)
        raise
    meta["build_seconds"] = round(time.monotonic() - started)
    meta["index"] = str(out)
    meta["index_bytes"] = out.stat().st_size
    return meta


def _full_fsync(path: Path) -> None:
    descriptor = os.open(path, os.O_RDONLY)
    try:
        fcntl.fcntl(descriptor, getattr(fcntl, "F_FULLFSYNC", 51))
    except OSError:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


def index_meta(index: Path) -> dict:
    """Meta of an existing index ({} when there is none)."""
    if not index.is_file():
        return {}
    connection = sqlite3.connect(f"{index.resolve().as_uri()}?mode=ro", uri=True)
    try:
        return pf.zlib_index_meta(connection)
    finally:
        connection.close()


# ---------------------------------------------------------------------------
# Refresh: newest seeded dump -> Transmission -> rebuilt index
# ---------------------------------------------------------------------------


def torrent_list(http: pf.Http, hosts: list[str]) -> list[dict]:
    """Anna's torrent list (plain JSON; no key, no bot challenge observed)."""
    failures = []
    for host in hosts:
        try:
            response = http.get(f"https://{host}/{TORRENTS_PATH}", timeout=180)
        except Exception as exc:  # noqa: BLE001 - try the next mirror
            failures.append(f"{host}: {type(exc).__name__}")
            continue
        if response.status != 200 or response.is_challenge:
            failures.append(f"{host}: HTTP {response.status}{' challenge' if response.is_challenge else ''}")
            continue
        try:
            entries = json.loads(response.text)
        except ValueError:
            failures.append(f"{host}: not JSON")
            continue
        if isinstance(entries, list):
            return entries
        failures.append(f"{host}: unexpected layout")
    raise ZlibError("Anna's torrent list is unavailable (" + "; ".join(failures) + ")")


def choose_snapshot(entries: list[dict]) -> tuple[dict, list[dict]]:
    """Newest zlib3_records dump with seeders, plus the newer ones skipped for having none."""
    dumps = []
    for entry in entries:
        name = str(entry.get("display_name") or "").removesuffix(".torrent")
        match = DUMP_RE.match(name)
        if match and entry.get("btih") and entry.get("url"):
            dumps.append((match[2], match[1], name, entry))
    dumps.sort(key=lambda item: item[:3], reverse=True)
    skipped = []
    for _end, _start, name, entry in dumps:
        if int(entry.get("seeders") or 0) > 0:
            return {**entry, "file_name": name}, skipped
        skipped.append({"file_name": name, "seeders": entry.get("seeders"),
                        "listed": entry.get("added_to_torrents_list_at")})
    raise ZlibError("no zlib3_records dump in Anna's torrent list has seeders"
                    + (f" ({len(skipped)} without seeders)" if skipped else ""))


def transmission(*args: str) -> dict:
    """Run transmission-remote with JSON output against the local daemon."""
    command = shutil.which("transmission-remote")
    if not command:
        raise ZlibError("refresh needs transmission-remote and a running transmission daemon")
    result = subprocess.run([command, "-j", *args], capture_output=True, text=True, timeout=120)
    try:
        payload = json.loads(result.stdout)
    except ValueError:
        payload = None
    if result.returncode != 0 or not isinstance(payload, dict) or "error" in payload:
        detail = (payload or {}).get("error") if isinstance(payload, dict) else (result.stderr or result.stdout)
        raise ZlibError(f"transmission-remote {' '.join(args[:2])} failed: {str(detail).strip()[:300]}")
    return payload.get("result") if isinstance(payload.get("result"), dict) else {}


def torrent_state(btih: str) -> dict | None:
    torrents = transmission("-t", btih, "-i").get("torrents") or []
    return torrents[0] if torrents else None


def download_dump(http: pf.Http, entry: dict, directory: Path, log: Callable[[str], None],
                  timeout: int = DOWNLOAD_TIMEOUT_SECONDS) -> Path:
    """Fetch ENTRY's data through Transmission into DIRECTORY, without seeding it."""
    btih = entry["btih"].lower()
    if not re.fullmatch(r"[0-9a-f]{40}", btih):
        raise ZlibError(f"unexpected torrent hash {btih!r}")
    if torrent_state(btih) is None:
        host = urllib.parse.urlparse(entry["url"]).hostname or ""
        if not pf.ANNAS_HOST_RE.match(host):
            raise ZlibError(f"refusing a torrent file from {host}")
        response = http.get(entry["url"], timeout=120)
        if response.status != 200 or not response.content.startswith(b"d"):
            raise ZlibError(f"torrent file download failed (HTTP {response.status})")
        with tempfile.NamedTemporaryFile(dir=directory, suffix=".torrent", delete=False) as handle:
            handle.write(response.content)
        try:
            transmission("-a", handle.name, "-w", str(directory), "--start-paused")
        finally:
            Path(handle.name).unlink(missing_ok=True)
    # A seed-ratio limit of 0 stops the torrent as soon as it completes; the
    # torrent is removed from Transmission once its data is verified below.
    transmission("-t", btih, "-sr", "0")
    transmission("-t", btih, "-s")
    log(f"zlib refresh: downloading {entry['file_name']} through transmission")
    deadline = time.monotonic() + timeout
    while True:
        state = torrent_state(btih)
        if state is None:
            raise ZlibError("the torrent disappeared from transmission while downloading")
        if state.get("error"):
            raise ZlibError(f"transmission reports an error: {state.get('error_string')}")
        if state.get("percent_done") == 1 and not state.get("left_until_done"):
            break
        if time.monotonic() > deadline:
            raise ZlibError(f"download incomplete after {timeout // 3600}h "
                            f"({100 * float(state.get('percent_done') or 0):.1f}% done); left in transmission")
        time.sleep(DOWNLOAD_POLL_SECONDS)
    path = Path(state["download_dir"]) / state["name"]
    if path.resolve().parent != directory.resolve() or path.name != entry["file_name"]:
        raise ZlibError(f"transmission saved the dump as {path}, not in {directory}")
    transmission("-t", btih, "-r")  # keep the data, stop tracking (no seeding)
    return path


def _remove_superseded(directory: Path, keep: Path, log: Callable[[str], None]) -> list[str]:
    """Delete dumps older than KEEP (never a newer one, e.g. built by hand)."""
    removed = []
    keep_end = DUMP_RE.match(keep.name)[2]
    stale = [path for path in directory.iterdir()
             if DUMP_RE.match(path.name) and DUMP_RE.match(path.name)[2] < keep_end]
    if not stale:
        return removed
    names = {path.name for path in stale}
    for torrent in transmission("-l").get("torrents") or []:
        if torrent.get("name") in names:
            transmission("-t", str(torrent["id"]), "-r")  # forget it first; the file is deleted next
    for path in stale:
        path.unlink()
        removed.append(path.name)
        log(f"zlib refresh: deleted superseded {path.name}")
    return removed


@contextmanager
def refresh_lock(directory: Path):
    if "My Drive" in str(directory.resolve()):
        raise ZlibError("the Z-Library dumps and index must not live under Google Drive")
    directory.mkdir(parents=True, exist_ok=True)
    with (directory / ".refresh.lock").open("w") as handle:
        try:
            fcntl.flock(handle, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            raise ZlibError("another Z-Library refresh or build is running") from None
        yield


def refresh(http: pf.Http, directory: Path = pf.ZLIB_DIR, *, dry_run: bool = False,
            log: Callable[[str], None] = lambda _message: None) -> dict:
    index = directory / INDEX_NAME
    entries = torrent_list(http, pf.resolve_annas_hosts(http))
    chosen, skipped = choose_snapshot(entries)
    try:
        current = index_meta(index)
    except (sqlite3.Error, pf.PaperFetchError) as exc:
        # An old schema or a damaged index is exactly what a rebuild fixes.
        log(f"zlib refresh: rebuilding the unusable index ({exc})")
        current = {}
    plan = {"snapshot": chosen["file_name"], "seeders": chosen.get("seeders"),
            "listed": chosen.get("added_to_torrents_list_at"), "data_size": chosen.get("data_size"),
            "skipped_unseeded": skipped, "current": current.get("dump", "")}
    chosen_end = snapshot_of(Path(chosen["file_name"]))[1]
    if current.get("snapshot", "") >= chosen_end:
        # Seeder counts fluctuate: never step back to an older seeded dump.
        return {"status": "up-to-date", **plan}
    if dry_run:
        return {"status": "dry-run", **plan}
    with refresh_lock(directory):
        dump = directory / chosen["file_name"]
        # A torrent still in Transmission may be preallocated at full size but
        # incomplete, so only an untracked full-size file counts as downloaded.
        if (torrent_state(chosen["btih"].lower()) is not None
                or not (dump.is_file() and dump.stat().st_size == chosen.get("data_size"))):
            dump = download_dump(http, chosen, directory, log)
        if dump.stat().st_size != chosen.get("data_size"):
            raise ZlibError(f"{dump.name} has {dump.stat().st_size} bytes; the torrent list says "
                            f"{chosen.get('data_size')}")
        built = build_index(dump, index, listed=str(chosen.get("added_to_torrents_list_at") or ""),
                            btih=chosen["btih"], log=log)
        removed = _remove_superseded(directory, dump, log)
    return {"status": "ok", **plan, "index": built, "deleted_dumps": removed}


def preflight(directory: Path = pf.ZLIB_DIR) -> list[str]:
    """Problems that would stop a refresh before it does any work ([] when ready)."""
    problems = [f"{command} is not on PATH" for command in ("zstd", "transmission-remote")
                if not shutil.which(command)]
    probe = directory if directory.exists() else directory.parent
    if not os.access(probe, os.W_OK):
        problems.append(f"{probe} is not writable")
    if "My Drive" in str(directory.resolve()):
        problems.append(f"{directory} is under Google Drive")
    return problems
