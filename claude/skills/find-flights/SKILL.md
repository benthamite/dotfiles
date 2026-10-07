---
name: find-flights
description: Search, rank and prepare flight bookings for Pablo against his flight booking profile, using Google Flights (SerpApi) cross-checked with Skyscanner, and stop before payment. Use for finding, comparing or booking flights; for hotels use find-hotel, for short-term rentals find-airbnb.
---

# Find flights

Find the best flight for a trip, explain the ranking, and on request prepare
the booking up to payment. Adapted from Peter Hartree's
[find-flights](https://github.com/HartreeWorks/skill--find-flights).

## Preferences

Read Pablo's flight booking profile before searching:
`~/My Drive/notes/flight-booking-profile.org`. It owns every preference:
default origin, cabin, fare, booking channel, seats, baggage, sleep and
departure-time rules, nonstop premium, connections, extras, meals, documents,
entry requirements, value of time and authorization. This skill owns only the
procedure. Where the two seem to conflict, the profile wins; tell Pablo about
the conflict.

## Step 1: Inputs

Establish the origin (profile default unless stated), destination, target
date, and return date if any. Ask only for what is missing and cannot be
defaulted. Unless Pablo says a date is fixed, search the target date ±2 days.
Before searching, work out from the profile and the actual time zones:

- the earliest acceptable departure for each origin airport (wake-up time,
  preparation, current ground travel time to that airport, 90 minutes at the
  airport);
- for each date, which flight lengths must be overnight and the target
  bedtime.

## Step 2: Google Flights via SerpApi (background)

Run the search script in the background, one call for all candidate dates:

```bash
python3 <skill-dir>/scripts/serpapi_flights.py search \
  --from EZE,AEP --to MAD --dates 2026-11-08,2026-11-09,2026-11-10,2026-11-11,2026-11-12 \
  --out "$TMPDIR/flights-out.json"
```

`<skill-dir>` is this skill's directory; write output outside the Drive sync
root (the session scratchpad if there is one). The script reads the key from 1Password
itself (`op://Automation/serpapi/credential`, personal account) and never
prints it. Each date costs one SerpApi search credit (free plan: a limited
monthly quota), so do not re-run searches needlessly.

- Comma-separated airports search them together (both Buenos Aires airports,
  or several destination airports).
- With a return date, also run `--return-date`: results then carry round-trip
  prices, and `return --token DEPARTURE_TOKEN` lists the return options for a
  chosen outbound. Run one-way searches for the return leg too, so the
  round-trip and two-one-way prices can be compared.
- For each finalist, `booking --token BOOKING_TOKEN` lists sellers and their
  prices (`airline_direct` marks the airline itself). Use it to apply the
  profile's direct-versus-third-party rule.
- If the key is missing or SerpApi errors, say so and continue with Skyscanner
  as the only source.

## Step 3: Skyscanner cross-check (browser, main thread)

Load `chrome-use` first and follow it. While the script runs, search
Skyscanner in Pablo's Chrome; SerpApi misses some carriers, sellers and fares.

```text
https://www.skyscanner.com/transport/flights/{from}/{to}/{YYMMDD}/?adultsv2=1&cabinclass=economy&rtn=0&currency=USD&locale=en-US&market=US
https://www.skyscanner.com/transport/flights/{from}/{to}/{YYMMDD}/{YYMMDD_return}/?adultsv2=1&cabinclass=economy&rtn=1&currency=USD&locale=en-US&market=US
```

Codes are lowercase IATA; `buea` covers both Buenos Aires airports. Append
`&stops=!oneStop,!twoPlusStops` for nonstop only. Wait about eight seconds for
the results to load, then read the page text: each result is a "Flight option
N:" paragraph with airline, airports, times, duration, stops and total price.
Skip the sponsored "Advertisement by" options that duplicate other results.
"Self-transfer" itineraries are separate tickets: exclude them unless Pablo
approves (profile). Use "Show more results" when the first page is mostly
excluded options.

## Step 4: Reconcile and filter

Wait for both sources. Merge and deduplicate. Add options only Skyscanner
found and flag material disagreements in price or routing. Drop options that
break a firm profile rule: sleep, earliest departure, cabin, separate tickets,
connection times. Check each segment of a connecting itinerary separately
for sleep suitability.

## Step 5: Rank

Compare options by effective cost: ticket price plus the time cost of every
hour from leaving home to arriving at the destination, priced by the profile's
value-of-time rule ($100 × (1 − p) per hour, with p estimated per setting).
Then apply the profile's nonstop premium. Hours in the planned sleep window are
judged by the sleep rule, not costed. Show the p values used.

When the profile's rules and the effective-cost ranking disagree, follow the
profile and say why.

## Step 6: Entry requirements

For international trips, run the profile's entry-requirements check for the
finalists (destination and transit countries, actual dates and passport)
before calling any option ready to book.

## Step 7: Output

Top three options in a table:

| # | Price (USD) | Effective cost | Airline / flights | Route and local times | Duration | Stops / layovers | Sleep | Seller |
|---|---|---|---|---|---|---|---|---|

Under the table, one line per option: why it ranks there, with the price and
time-cost difference against #1. Then a longer shortlist of other viable
options, one line each. When the preferred date has several viable times, give
each its own row: the choice of time of day is Pablo's. Add the entry
requirements result and any extra worth considering (for example lounge
access, with its value-of-time calculation; never buy it). Link each option to
where it can be booked.

## Step 8: Prepare the booking (only when asked)

Follow the profile's authorization section. On the chosen seller's site, fill
in traveler details from `claude/context/personal-details.md`, select the
fare, a free window seat if offered, a free vegetarian meal if offered, and the
applicable frequent-flyer number. Decline every paid extra. Leave the passport
number for Pablo, as `chrome-use` requires. Stop at the payment step and tell
Pablo which tab and Chrome profile window to finish in.

## Step 9: Calendar (after Pablo confirms the booking)

Once Pablo says the flight is booked, add one timed event per flight (one
event per itinerary direction, from first departure to final arrival, with the
connections in the description). Check for an existing event first so a retry
does not duplicate it:

```bash
gcalcli-epoch --calendar 'pablo@epoch.ai' search "✈️" 2026-11-10 2026-11-12
gcalcli-epoch --calendar 'pablo@epoch.ai' add --noprompt \
  --title "✈️ EZE→MAD Iberia IB6844" \
  --when "2026-11-10T23:55:00-03:00" --end "2026-11-11T16:25:00+01:00" \
  --where "Ezeiza International Airport (EZE)" \
  --description "Iberia IB6844 · booking ref XXXXXX"
```

Use each airport's local time with its UTC offset on that date; gcalcli
honours the offsets. Do not add an all-day travel-day event. Report the created
events back.
