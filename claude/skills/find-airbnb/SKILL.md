---
name: find-airbnb
description: Search Airbnb in Pablo's Chrome for short-term rentals, rank listings by his accommodation booking profile and taste (judged from the photos), and on request take a booking up to payment. Use for Airbnb, holiday, vacation or short-term rental requests; for hotels use find-hotel.
---

# Find Airbnb

Find short-term rentals that meet Pablo's requirements, rank them by his taste
and price, and on request prepare the booking up to payment. Adapted from Peter
Hartree's [find-airbnb](https://github.com/HartreeWorks/skill--find-airbnb),
which searched through a paid API and scored photos with Gemini; here the
search runs in the browser and the agent judges the photos itself.

## Preferences

Read `~/My Drive/notes/accommodation-booking-profile.org` (shared preferences,
the Short-term rentals section and its Taste subsection) before searching. It
owns type of place, quality floor, amenities, location, rate type, taste and
authorization. This skill owns only the procedure; where they seem to conflict,
the profile wins and you tell Pablo.

## Step 1: Inputs

Establish the area, dates and guests. Ask only for what is missing; ask about
the area when it is ambiguous. For a rural or regional search, pick a map
area rather than a place name and state its bounds.

## Step 2: Search

Load `chrome-use` first and follow it. Search Airbnb in Pablo's Chrome:

```text
https://www.airbnb.com/s/PLACE/homes?checkin=YYYY-MM-DD&checkout=YYYY-MM-DD&adults=1&room_types%5B%5D=Entire%20home%2Fapt&currency=USD
```

Add the air-conditioning amenity filter outside the United States
(`amenities%5B%5D=5`). Read results with JavaScript: collect the distinct
`a[href*="/rooms/"]` links and the text of the card around each; return only
the URL's pathname, since the browser tool refuses URLs with query strings.
Cards show the total "for N nights", sometimes beside a struck-through price;
use the current total. The entire-home filter still lets some hotel rooms
through, so drop cards that are not an entire place. Read every result on the
first three pages and the map at the target. Collect for each listing: title, link, total price
including fees, nightly price, rating and review count, beds and bedrooms, and
location relative to the target. Drop listings below the profile's quality
floor.

## Step 3: Judge the shortlist

For the strongest 10 or so listings, open each listing page in your tab
(`/rooms/ID?check_in=…&check_out=…&adults=1&currency=USD`), wait a few seconds
for the photos to load, and look at them in a screenshot; open "Show all
photos" when the first five leave the interior unclear. Confirm the required amenities from the amenities list and
photos (a desk or workspace is often visible but not listed). Score taste from
1 to 10 against the profile's Taste section, and write one or two sentences
naming the concrete features behind the score. Note anything the photos leave
unclear rather than guessing.

## Step 4: Present

Rank by taste and price together: show both, and do not hide a cheaper, less
elegant option that may still be the better deal. For each listing:

```text
**#1 · USD TOTAL (USD X/night) · ★ 4.92 (85 reviews) · Taste 8/10**
[Title](https://www.airbnb.com/rooms/ID?check_in=…&check_out=…&adults=1)
1 bedroom · area, distance to target · A/C ✓ desk ✓ · cancellation policy
Why: tall ceilings, parquet and moulded panelling; furniture partly modern.
```

Show up to ten. Every listing links to its page with the stay's dates.

## Step 5: Prepare the booking (only when asked)

Follow the profile's authorization rule. Open the listing with the dates and
guests, choose the cheapest non-refundable option Airbnb offers, fill in
anything required from `claude/context/personal-details.md`, and stop at the
payment step. Never enter payment details or click the final confirm control.
If the host requires a message to request booking, draft it and ask Pablo
before sending. Tell Pablo which tab and Chrome profile window to finish in.
