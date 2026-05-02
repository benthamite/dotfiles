#!/usr/bin/env python3
"""sheets.py — thin Google Sheets API wrapper for the pablo@epoch.ai account.

Replaces the sheets_* tools from the google-workspace-epoch MCP server.
Auth is shared with gmail.py via _gworkspace_auth.py.

A "spreadsheet ID" is the long string in a Sheets URL:
  https://docs.google.com/spreadsheets/d/<ID>/edit
A "range" is A1 notation, optionally prefixed with the sheet name:
  Sheet1!A1:C10  |  A:A  |  Quarterly!B2

Usage:
  sheets.py read <spreadsheet-id> <range> [--format=tsv|json]
      Print the cell values. Default format=tsv (one row per line, tab separated).
  sheets.py write <spreadsheet-id> <range>
      Read TSV from stdin and write into the range. One line = one row.
  sheets.py append <spreadsheet-id> <range>
      Read TSV from stdin and append rows after existing data in the range.
  sheets.py clear <spreadsheet-id> <range>
      Clear cell values in the range.
  sheets.py add-sheet <spreadsheet-id> <title>
      Add a new sheet (tab) to an existing spreadsheet. Prints the new sheet ID.
  sheets.py delete-sheet <spreadsheet-id> <sheet-id>
      Delete a sheet by its numeric sheet ID.
  sheets.py create <title>
      Create a new spreadsheet. Prints the new spreadsheet ID.
  sheets.py info <spreadsheet-id>
      Print sheet titles and IDs.
"""

import argparse
import json
import os
import sys

sys.path.insert(0, os.path.dirname(__file__))
from _gworkspace_auth import api_request  # noqa: E402

API = "https://sheets.googleapis.com/v4/spreadsheets"


def _q(v):
    import urllib.parse
    return urllib.parse.quote(str(v), safe="")


def cmd_read(args):
    out = api_request("GET", f"{API}/{args.spreadsheet_id}/values/{_q(args.range)}")
    values = out.get("values", [])
    if args.format == "json":
        print(json.dumps(values))
        return
    for row in values:
        print("\t".join(str(c) for c in row))


def cmd_write(args):
    rows = [line.split("\t") for line in sys.stdin.read().splitlines()]
    api_request(
        "PUT",
        f"{API}/{args.spreadsheet_id}/values/{_q(args.range)}?valueInputOption=USER_ENTERED",
        body={"values": rows},
    )
    print(f"Wrote {len(rows)} rows to {args.range}")


def cmd_append(args):
    rows = [line.split("\t") for line in sys.stdin.read().splitlines()]
    api_request(
        "POST",
        f"{API}/{args.spreadsheet_id}/values/{_q(args.range)}:append?valueInputOption=USER_ENTERED",
        body={"values": rows},
    )
    print(f"Appended {len(rows)} rows to {args.range}")


def cmd_clear(args):
    api_request("POST", f"{API}/{args.spreadsheet_id}/values/{_q(args.range)}:clear", body={})
    print(f"Cleared {args.range}")


def cmd_add_sheet(args):
    out = api_request(
        "POST",
        f"{API}/{args.spreadsheet_id}:batchUpdate",
        body={"requests": [{"addSheet": {"properties": {"title": args.title}}}]},
    )
    new_id = out["replies"][0]["addSheet"]["properties"]["sheetId"]
    print(new_id)


def cmd_delete_sheet(args):
    api_request(
        "POST",
        f"{API}/{args.spreadsheet_id}:batchUpdate",
        body={"requests": [{"deleteSheet": {"sheetId": int(args.sheet_id)}}]},
    )
    print(f"Deleted sheet {args.sheet_id}")


def cmd_create(args):
    out = api_request("POST", API, body={"properties": {"title": args.title}})
    print(out["spreadsheetId"])


def cmd_info(args):
    out = api_request("GET", f"{API}/{args.spreadsheet_id}?fields=sheets.properties")
    for s in out.get("sheets", []):
        p = s["properties"]
        print(f"{p['sheetId']}\t{p['title']}")


def main():
    p = argparse.ArgumentParser(description="Google Sheets API wrapper")
    sub = p.add_subparsers(dest="cmd", required=True)

    r = sub.add_parser("read")
    r.add_argument("spreadsheet_id")
    r.add_argument("range")
    r.add_argument("--format", choices=["tsv", "json"], default="tsv")
    r.set_defaults(func=cmd_read)

    for name, fn in [("write", cmd_write), ("append", cmd_append), ("clear", cmd_clear)]:
        s = sub.add_parser(name)
        s.add_argument("spreadsheet_id")
        s.add_argument("range")
        s.set_defaults(func=fn)

    a = sub.add_parser("add-sheet")
    a.add_argument("spreadsheet_id")
    a.add_argument("title")
    a.set_defaults(func=cmd_add_sheet)

    d = sub.add_parser("delete-sheet")
    d.add_argument("spreadsheet_id")
    d.add_argument("sheet_id")
    d.set_defaults(func=cmd_delete_sheet)

    c = sub.add_parser("create")
    c.add_argument("title")
    c.set_defaults(func=cmd_create)

    i = sub.add_parser("info")
    i.add_argument("spreadsheet_id")
    i.set_defaults(func=cmd_info)

    args = p.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
