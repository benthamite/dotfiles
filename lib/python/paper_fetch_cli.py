#!/usr/bin/env python3
"""Download a paper by DOI, URL, arXiv id, title or Anna's Archive md5.

    paper-fetch get IDENT [--out DIR] [--name STEM] [--author NAME] [--md5 MD5] [--json]
    paper-fetch collect JOB.json [--out DIR] [--name STEM] [--json]
    paper-fetch book-candidates --target TARGET.json --out CANDIDATES.json
    paper-fetch book-register CANDIDATES.json --file LOCAL.pdf --source URL --out NEW.json
    paper-fetch book-stage CANDIDATES.json --md5 MD5 --out DIR [--file LOCAL.pdf]
    paper-fetch book-inspect LOCAL.pdf --out NEW-DIR [--pages 1,2,3,last]
    paper-fetch book-select CANDIDATES.json --reviews REVIEWS.json
    paper-fetch hosts

`get` tries every route that works from a shell (open access, LibGen JSON ->
Anna's Archive fast-download API, SciDB) and stages the verified PDF under
--out. When the only remaining copies sit behind a browser bot challenge it
writes a browser job (JSON plus a JavaScript snippet per origin) and exits with
status `needs-browser`; a real Chrome session runs the snippet on that origin,
the files land in ~/Downloads, and `collect` verifies and installs them.

Book commands return JSON. Discovery/staging/inspection require review, never
approve a book. See agents/bibliography-policy.md and docs/book-acquisition.md.
Exit codes: 0 ok, 2 needs-browser, 3 not-member, 4 unavailable, 5 needs-review,
6 unknown (search incomplete), 1 error.
The Anna's Archive key comes from ANNAS_SECRET_KEY or the tlon/core/annas-archive
pass entry; it is never printed. Shared logic lives in lib/python/paper_fetch.py.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
import subprocess
import sys
from pathlib import Path

sys.dont_write_bytecode = True

_LIB_FILE = Path(__file__).resolve().with_name("paper_fetch.py")
_SPEC = importlib.util.spec_from_file_location("paper_fetch", _LIB_FILE)
if _SPEC is None or _SPEC.loader is None:
    raise SystemExit(f"cannot load shared paper library: {_LIB_FILE}")
pf = importlib.util.module_from_spec(_SPEC)
sys.modules["paper_fetch"] = pf  # dataclasses resolve annotations through sys.modules
_SPEC.loader.exec_module(pf)

EXIT_CODES = {"ok": 0, "needs-browser": 2, "not-member": 3, "unavailable": 4, "mismatch": 4,
              "no-downloads": 4, "needs-review": 5, "unknown": 6, "error": 1}
DEFAULT_OUT = Path(os.environ.get("PAPER_FETCH_OUT", Path.home() / "Downloads" / "paper-fetch"))
DEFAULT_JOBS = Path(os.environ.get("PAPER_FETCH_JOBS", Path.home() / ".cache" / "paper-fetch" / "jobs"))


def log(message: str) -> None:
    print(message, file=sys.stderr)


def print_outcome(outcome: dict, as_json: bool) -> None:
    if as_json:
        print(json.dumps(outcome, indent=1))
        return
    work = outcome.get("work") or {}
    if work.get("title"):
        print(f"work: {work.get('title')} ({', '.join(work.get('authors') or [])[:80]}; {work.get('year')})")
    print(f"status: {outcome['status']}")
    if outcome.get("file"):
        verification = outcome.get("verification") or {}
        print(f"file: {outcome['file']}")
        print(f"route: {outcome.get('route')}; verification: {verification.get('verdict')} "
              f"(title match {verification.get('title_match')}, doi {verification.get('doi_found')}, "
              f"{verification.get('pages')} pages)")
    for attempt in outcome.get("attempts") or []:
        print(f"  - {attempt['route']} {attempt['target']}: {attempt['result']}")
    job = outcome.get("browser_job")
    if job:
        print(f"browser job: {job['path']}")
        for group in job["origins"]:
            print(f"  origin {group['origin']}: {len(group['items'])} item(s); snippet {group['snippet']}")
        print("  Open the origin in the user's Chrome tab, run the snippet there, then:")
        print(f"  paper-fetch collect '{job['path']}'")
    if outcome.get("message"):
        print(f"note: {outcome['message']}")


def cmd_get(args: argparse.Namespace) -> int:
    http = pf.Http()
    fetcher = pf.Fetcher(http, Path(args.out), Path(args.jobs), annas_host=args.annas_host or "",
                         log=log if args.verbose else None, skip_annas=args.no_annas)
    outcome = fetcher.fetch(args.ident, author=args.author or "", name=args.name or "", md5=args.md5 or "")
    print_outcome(outcome.to_dict(), args.json)
    return EXIT_CODES.get(outcome.status, 1)


def cmd_collect(args: argparse.Namespace) -> int:
    job = pf.load_browser_job(Path(args.job))
    result = pf.collect_browser_downloads(job, Path(args.out), args.name or "")
    if args.json:
        print(json.dumps(result, indent=1))
    else:
        print(f"status: {result['status']}")
        if result["file"]:
            print(f"file: {result['file']}")
        for entry in result["results"]:
            verdict = entry["verification"]["verdict"]
            print(f"  - {entry['downloaded']}: {verdict}" + (f" -> {entry['installed']}" if entry.get("installed") else ""))
        if result.get("message"):
            print(f"note: {result['message']}")
    return EXIT_CODES.get(result["status"], 1)


def cmd_hosts(args: argparse.Namespace) -> int:
    hosts = pf.resolve_annas_hosts(pf.Http(), args.annas_host or "")
    print("\n".join(hosts))
    return 0


def read_json(path: str) -> dict:
    try:
        return json.loads(Path(path).read_text())
    except (OSError, ValueError) as exc:
        raise pf.PaperFetchError(f"Cannot read JSON input ({type(exc).__name__})") from None


def book_outcome(result: dict) -> int:
    print(json.dumps(result, indent=2))
    return EXIT_CODES.get(result["status"], 1)


def cmd_book_candidates(args: argparse.Namespace) -> int:
    target = pf.book_target(read_json(args.target))
    destination = Path(args.out).expanduser()
    if destination.exists():
        raise pf.PaperFetchError("Candidate manifest already exists; use a new output path")
    result = pf.discover_book_candidates(pf.Http(), target, annas_host=args.annas_host or "")
    destination.parent.mkdir(parents=True, exist_ok=True)
    with destination.open("x") as output:
        output.write(json.dumps(result, indent=2) + "\n")
    return book_outcome({**result, "manifest": str(destination.resolve())})


def cmd_book_stage(args: argparse.Namespace) -> int:
    manifest = pf.read_book_candidates(Path(args.candidates))
    out_dir = Path(args.out).expanduser()
    # Validate the explicit candidate before constructing a client or reading a key.
    candidate = next((item for item in manifest["candidates"] if item["md5"] == args.md5 and item.get("format") == "pdf"), None)
    if candidate is None:
        raise pf.PaperFetchError("Choose an explicit PDF candidate from the inventory")
    local_file = args.file or candidate.get("local_file")
    fetcher = None if local_file else pf.Fetcher(pf.Http(), out_dir, Path(args.jobs), annas_host=args.annas_host or "")
    result = pf.stage_book_candidate(manifest, args.md5, out_dir, fetcher=fetcher,
                                     local_file=Path(local_file) if local_file else None, name=args.name or "")
    return book_outcome(result)


def cmd_book_register(args: argparse.Namespace) -> int:
    manifest = pf.read_book_candidates(Path(args.candidates))
    destination = Path(args.out).expanduser()
    if destination.exists():
        raise pf.PaperFetchError("Candidate manifest already exists; use a new output path")
    result = pf.register_book_candidate(manifest, Path(args.file), args.source)
    destination.parent.mkdir(parents=True, exist_ok=True)
    with destination.open("x") as output:
        output.write(json.dumps(result, indent=2) + "\n")
    return book_outcome({**result, "manifest": str(destination.resolve())})


def cmd_book_inspect(args: argparse.Namespace) -> int:
    return book_outcome(pf.inspect_book_pdf(Path(args.file), Path(args.out).expanduser(), args.pages))


def cmd_book_select(args: argparse.Namespace) -> int:
    manifest = pf.read_book_candidates(Path(args.candidates))
    return book_outcome(pf.select_book_candidate(manifest, read_json(args.reviews)))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="paper-fetch", description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--annas-host", help="Anna's Archive host override (annas-archive.XX)")
    sub = parser.add_subparsers(dest="command", required=True)

    get = sub.add_parser("get", help="resolve and download one paper")
    get.add_argument("ident", help="DOI, doi.org/publisher URL, arXiv id, md5, or title")
    get.add_argument("--author", help="author surname to disambiguate a title search")
    get.add_argument("--md5", help="Anna's Archive md5 already known for this work")
    get.add_argument("--name", help="file stem for the staged PDF (default: derived from metadata)")
    get.add_argument("--out", default=str(DEFAULT_OUT), help=f"staging directory (default {DEFAULT_OUT})")
    get.add_argument("--jobs", default=str(DEFAULT_JOBS), help="browser job directory")
    get.add_argument("--no-annas", action="store_true", help="skip LibGen/Anna's Archive routes")
    get.add_argument("--json", action="store_true")
    get.add_argument("--verbose", action="store_true")
    get.set_defaults(func=cmd_get)

    collect = sub.add_parser("collect", help="verify and install files a browser job downloaded")
    collect.add_argument("job")
    collect.add_argument("--name")
    collect.add_argument("--out", default=str(DEFAULT_OUT))
    collect.add_argument("--json", action="store_true")
    collect.set_defaults(func=cmd_collect)

    hosts = sub.add_parser("hosts", help="print the resolved Anna's Archive host order")
    hosts.set_defaults(func=cmd_hosts)

    candidates = sub.add_parser("book-candidates", help="inventory book files without selecting or downloading")
    candidates.add_argument("--target", required=True, help="JSON describing the exact edition")
    candidates.add_argument("--out", required=True, help="new candidate-manifest path outside Drive")
    candidates.set_defaults(func=cmd_book_candidates)

    register = sub.add_parser("book-register", help="register a local publisher/archive PDF for review")
    register.add_argument("candidates")
    register.add_argument("--file", required=True)
    register.add_argument("--source", required=True, help="permanent public source URL, without credentials or signed parameters")
    register.add_argument("--out", required=True, help="new candidate-manifest path outside Drive")
    register.set_defaults(func=cmd_book_register)

    stage = sub.add_parser("book-stage", help="stage one explicitly named candidate for review")
    stage.add_argument("candidates")
    stage.add_argument("--md5", required=True)
    stage.add_argument("--file", help="verify and copy an existing browser download instead of fetching")
    stage.add_argument("--name")
    stage.add_argument("--out", required=True, help="staging directory outside Drive")
    stage.add_argument("--jobs", default=str(DEFAULT_JOBS))
    stage.set_defaults(func=cmd_book_stage)

    inspect = sub.add_parser("book-inspect", help="render PDF pages and extract metadata/text for review")
    inspect.add_argument("file")
    inspect.add_argument("--out", required=True, help="new inspection directory outside Drive")
    inspect.add_argument("--pages", default="1,2,3,last")
    inspect.set_defaults(func=cmd_book_inspect)

    select = sub.add_parser("book-select", help="select the smallest explicitly reviewed eligible PDF")
    select.add_argument("candidates")
    select.add_argument("--reviews", required=True)
    select.set_defaults(func=cmd_book_select)

    args = parser.parse_args(argv)
    try:
        return args.func(args)
    except pf.PaperFetchError as exc:
        log(f"paper-fetch: {exc}")
        return 1
    except (OSError, subprocess.SubprocessError) as exc:
        log(f"paper-fetch: local file/inspection failure ({type(exc).__name__})")
        return 1


if __name__ == "__main__":
    sys.exit(main())
