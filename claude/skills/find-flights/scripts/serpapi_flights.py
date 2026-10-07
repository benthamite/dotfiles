#!/usr/bin/env python3
"""Search Google Flights through SerpApi and print normalized JSON.

The SerpApi key is read in-process from the personal 1Password Automation
vault (op://Automation/serpapi/credential) through `op-automations`; it is
never printed. Every request costs one SerpApi search credit, except repeats
served from SerpApi's one-hour cache.

Usage:
  serpapi_flights.py search --from EZE,AEP --to MAD --dates 2026-11-10,2026-11-11
                            [--return-date 2026-11-24] [--out FILE]
  serpapi_flights.py return --from EZE,AEP --to MAD --date 2026-11-10
                            --return-date 2026-11-24 --token DEPARTURE_TOKEN
  serpapi_flights.py booking --from EZE,AEP --to MAD --date 2026-11-10
                             [--return-date 2026-11-24] --token BOOKING_TOKEN
"""

import argparse
import json
import os
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request
from concurrent.futures import ThreadPoolExecutor

ENDPOINT = "https://serpapi.com/search.json"
KEY_REF = "op://Automation/serpapi/credential"


def api_key():
    """Return the SerpApi key from the environment or 1Password."""
    key = os.environ.get("SERPAPI_KEY")
    if key:
        return key
    proc = subprocess.run(
        ["op-automations", "@personal", "read", KEY_REF],
        capture_output=True, text=True,
    )
    if proc.returncode != 0 or not proc.stdout.strip():
        sys.exit(f"error: could not read {KEY_REF} via op-automations "
                 f"(exit {proc.returncode}); is the serpapi item in the "
                 "personal Automation vault?")
    return proc.stdout.strip()


def call(params, key):
    """Run one SerpApi request and return the decoded JSON."""
    query = dict(params, engine="google_flights", api_key=key)
    url = ENDPOINT + "?" + urllib.parse.urlencode(query)
    try:
        with urllib.request.urlopen(url, timeout=120) as resp:
            data = json.load(resp)
    except urllib.error.HTTPError as err:
        body = err.read().decode(errors="replace")
        try:
            message = json.loads(body).get("error", body)
        except ValueError:
            message = body
        return {"error": f"HTTP {err.code}: {message}"}
    if "error" in data:
        return {"error": data["error"]}
    return data


def segment(seg):
    dep, arr = seg.get("departure_airport", {}), seg.get("arrival_airport", {})
    return {
        "flight": seg.get("flight_number"),
        "airline": seg.get("airline"),
        "from": dep.get("id"),
        "dep": dep.get("time"),
        "to": arr.get("id"),
        "arr": arr.get("time"),
        "minutes": seg.get("duration"),
        "aircraft": seg.get("airplane"),
        "legroom": seg.get("legroom"),
        "overnight": seg.get("overnight", False),
        "notes": seg.get("extensions", []),
    }


def itinerary(item, date):
    return {
        "date": date,
        "price": item.get("price"),
        "type": item.get("type"),
        "total_minutes": item.get("total_duration"),
        "stops": len(item.get("layovers", [])),
        "segments": [segment(s) for s in item.get("flights", [])],
        "layovers": [
            {"airport": l.get("id"), "minutes": l.get("duration"),
             "overnight": l.get("overnight", False)}
            for l in item.get("layovers", [])
        ],
        "notes": item.get("extensions", []),
        "departure_token": item.get("departure_token"),
        "booking_token": item.get("booking_token"),
    }


def base_params(args, date):
    params = {
        "departure_id": args.origin,
        "arrival_id": args.dest,
        "outbound_date": date,
        "type": "1" if args.return_date else "2",
        "travel_class": "1",
        "currency": args.currency,
        "hl": "en",
        "deep_search": "true",
    }
    if args.return_date:
        params["return_date"] = args.return_date
    if args.gl:
        params["gl"] = args.gl
    return params


def flights_of(data, date):
    if "error" in data:
        return {"date": date, "error": data["error"]}
    items = data.get("best_flights", []) + data.get("other_flights", [])
    return {
        "date": date,
        "itineraries": [itinerary(i, date) for i in items],
        "price_insights": data.get("price_insights"),
    }


def cmd_search(args, key):
    dates = [d.strip() for d in args.dates.split(",") if d.strip()]

    def one(date):
        return flights_of(call(base_params(args, date), key), date)

    with ThreadPoolExecutor(max_workers=len(dates)) as pool:
        return list(pool.map(one, dates))


def cmd_return(args, key):
    params = base_params(args, args.date)
    params["departure_token"] = args.token
    return flights_of(call(params, key), args.return_date)


def cmd_booking(args, key):
    params = base_params(args, args.date)
    params["booking_token"] = args.token
    data = call(params, key)
    if "error" in data:
        return {"error": data["error"]}
    options = []
    for opt in data.get("booking_options", []):
        for part in ("together", "departing", "returning"):
            if part in opt:
                o = opt[part]
                options.append({
                    "part": part,
                    "seller": o.get("book_with"),
                    "airline_direct": o.get("airline", False),
                    "price": o.get("price"),
                    "fare": o.get("option_title"),
                    "notes": o.get("extensions", []),
                    "baggage": o.get("baggage_prices", []),
                })
    return {"booking_options": options,
            "selected_flights": [itinerary(i, args.date)
                                 for i in data.get("selected_flights", [])]}


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="cmd", required=True)
    for name in ("search", "return", "booking"):
        p = sub.add_parser(name)
        p.add_argument("--from", dest="origin", required=True,
                       help="IATA codes, comma-separated (e.g. EZE,AEP)")
        p.add_argument("--to", dest="dest", required=True)
        p.add_argument("--return-date")
        p.add_argument("--currency", default="USD")
        p.add_argument("--gl", help="Google country code (point of sale)")
        p.add_argument("--out", help="write JSON here instead of stdout")
        if name == "search":
            p.add_argument("--dates", required=True,
                           help="outbound dates, comma-separated")
        else:
            p.add_argument("--date", required=True, help="outbound date")
            p.add_argument("--token", required=True)
    args = parser.parse_args()
    if args.cmd == "return" and not args.return_date:
        parser.error("return needs --return-date")

    key = api_key()
    result = {"search": cmd_search, "return": cmd_return,
              "booking": cmd_booking}[args.cmd](args, key)
    text = json.dumps(result, indent=1, ensure_ascii=False)
    if args.out:
        with open(args.out, "w") as fh:
            fh.write(text)
        print(f"wrote {args.out}")
    else:
        print(text)


if __name__ == "__main__":
    main()
