---
name: find-hotel
description: Search and compare hotels for Pablo on Booking.com and the hotel's own site against his accommodation booking profile, recommend one with links, and on request take the booking up to payment. Use for finding or booking a hotel or room in a city or near an airport; for short-term rentals use find-airbnb, for flights find-flights.
---

# Find hotel

Find a well-rated hotel that meets Pablo's requirements at the best price, and
on request prepare the booking up to payment. Adapted from Peter Hartree's
[find-hotel](https://github.com/HartreeWorks/skill--find-hotel).

## Preferences

Read `~/My Drive/notes/accommodation-booking-profile.org` (shared preferences
and the Hotels section) before searching. It owns rate type, booking channel,
quality floor, amenities, location, currency and authorization. The
value-of-time rule it links to is in
`~/My Drive/notes/flight-booking-profile.org`. This skill owns only the
procedure; where they seem to conflict, the profile wins and you tell Pablo.

## Step 1: Inputs

Establish the target (an address, area or airport), check-in and check-out
dates and guests. When the stay accompanies a flight, derive the dates from the
flight's arrival and departure. Ask only for what is missing; ask about the
area when it is ambiguous.

## Step 2: Search Booking.com

Load `chrome-use` first and follow it. Use Pablo's Chrome; Booking.com shows
member prices when he is signed in. If it is signed out, continue with public
prices and say so; never sign in.

Start from a search URL. Put only the city in `ss`: free text such as
"Madrid city centre" is misparsed and returns the whole region. Narrow with
`nflt` filters joined by `%3B`:

- `review_score%3D80`: 8+ review score.
- `ht_id%3D204`: hotels only (otherwise apartments and hostels are mixed in).
- `roomfacility%3D11`: air conditioning (outside the United States).
- `di%3DNNN`: a district. Read its number from the page's filter checkboxes
  (for example Madrid City Center is `di=176`).

```text
https://www.booking.com/searchresults.html?ss=Madrid&checkin=YYYY-MM-DD&checkout=YYYY-MM-DD&group_adults=1&no_rooms=1&group_children=0&selected_currency=USD&nflt=review_score%3D80%3Bht_id%3D204%3Broomfacility%3D11%3Bdi%3D176
```

To read results, run JavaScript over `[data-testid=property-card]`
(`[data-testid=title]`, `review-score`, `price-and-discounted-price`,
`distance`; the link is `a[data-testid=title-link]`). The browser tool refuses
to return URLs with query strings, so return `origin + pathname` and add the
dates yourself.

For an area search, sweep in two passes before shortlisting; one
price-sorted page misses mid-priced properties.

1. List view, pages 1–3 (about 25 results each; `&offset=25`, `&offset=50`).
   Read every result.
2. Map view zoomed to the exact target, with "only show available properties"
   ticked (`nflt=oos%3D1`). Pan until the acceptable radius is covered and
   read every pin.

Booking.com's results page sometimes marks a property "unavailable" when its
own page has rooms. Before ruling one out, open the property page with the
dates in the query string and check again.

For each candidate, open the property page with the dates and read its text
(the page-text tool captures the rate table): total and nightly price,
refundable or not, prepay or pay at property, breakfast. Confirm air
conditioning (outside the US) and a desk or workspace from the room description
or photos. Read the fine print too: some hotels list air conditioning but run
it only in summer (for example "available from 1 June to 30 September"), which
fails the requirement outside those dates. Note a gym.

## Step 3: Cross-check the direct site

For the strongest candidates, search the hotel's own or chain site for the
same room, dates and guests. Capture the non-refundable rate. Convert currencies
to compare like for like. Apply the profile's booking-channel rule. Never sign
in or create an account for a member rate.

## Step 4: Recommend

```text
**HOTEL — AREA · check-in DD Mon → check-out DD Mon (N nights) · 1 guest**
Recommended: [site] [room], non-refundable — USD X total (local currency Y; taxes included; breakfast yes/no)
Channel: [Booking.com vs direct, with the difference]
Fit: [review score; A/C and desk confirmed; gym; distance or travel time to the target]
Links: [Booking.com page with dates] · [direct site page, if checked]
```

Then a shortlist of the other viable hotels, one line each. Every hotel named
carries a link to its Booking.com page with the stay's dates and guests in the
query string (`https://www.booking.com/hotel/CC/SLUG.html?checkin=…&checkout=…&group_adults=1&no_rooms=1`),
plus the direct-site link when checked, so Pablo can open any option without
searching again. Where location trades off against price, show the daily
travel-time cost from the value-of-time rule.

## Step 5: Prepare the booking (only when asked)

Follow the profile's authorization rule. On the chosen site, select the room and
rate, fill in guest details from `claude/context/personal-details.md`, decline
paid extras and currency conversion, and stop at the payment step. Never enter
card details or click the final reserve or confirm control. Tell Pablo which
tab and Chrome profile window to finish in.
