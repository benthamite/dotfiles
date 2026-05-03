---
name: ahrefs-api
description: Cost-optimal patterns for the Ahrefs API v3. Use when writing or modifying code that calls api.ahrefs.com or the Ahrefs MCP server, when investigating Ahrefs unit consumption, when adding a new Ahrefs-using project, or whenever a task involves designing how many Ahrefs calls to make. Trigger phrases include "Ahrefs API", "Ahrefs units", "Ahrefs quota", "site-explorer", and any reference to api.ahrefs.com endpoints.
---

# Ahrefs API v3 — cost-aware usage

Epoch is on the Advanced plan with a **1,000,000 unit/month** workspace cap (verified 2026-05-01 via `subscription-info/limits-and-usage`; reset on the 7th of each month). The connector code in `analytics-aggregation` hardcodes the limit but you should read the live value before relying on it. Standard plans are 150K/month — if Epoch ever downgrades, every cost below has to be reconsidered.

The April 11, 2026 quota crisis (150,014 / 150,000 units exhausted by a retry storm in `media-mentions`) is the cautionary tale: the limit is shared across every Epoch project that calls the API, and exceeding it kills all consumers until the cycle reset. Always probe `subscription-info` before and after a run, and surface the delta in logs.

## Cost model (empirical)

Most Site Explorer endpoints charge a **flat per-call cost**, not per-row. Verified empirically against the live workspace:

| Endpoint | Cost per call | Notes |
|---|---|---|
| `subscription-info/limits-and-usage` | **0** | Probe before/after every run |
| `site-explorer/top-pages` | ~120 | Same cost for 1, 5, 25, or ~50 URLs in a `where` IN-filter |
| `site-explorer/domain-rating` | ~50 | Per-target (domain). No effective batching. |
| `site-explorer/refdomains-history` | ~2,000 | Per-target. The `where` filter aggregates rather than splitting per-URL. |
| `site-explorer/url-rating-history` | ~0 | Per-URL. Effectively free in our test. |

**The batching insight**: endpoints that accept a `where` clause + URL list (`top-pages`, `pages-by-traffic`, `organic-keywords`, `pages-by-backlinks`) collapse N per-URL calls into 1 batched call. The cost stays at 120 units regardless of batch size — until you hit the GET URL-length cap.

## URL-length cap on `where` filters

GET requests fail with HTTP 414 above ~500 URLs in a `where` clause. The practical limit is **50 URLs per call**. Plan batches accordingly:

```python
PER_URL_BATCH_SIZE = 50

for i in range(0, len(urls), PER_URL_BATCH_SIZE):
    batch = urls[i : i + PER_URL_BATCH_SIZE]
    where_clause = json.dumps({
        "or": [{"field": "url", "is": ["eq", u]} for u in batch]
    })
    # ... single API call covering 50 URLs ...
```

Reference implementation: `analytics-aggregation/repo/connectors/ahrefs.py::_fetch_per_url_metrics`.

## Endpoints that DON'T batch

Some endpoints really are per-target with no effective batching. For these, the only optimization is caching:

- **`domain-rating`**: per-host. The `where` filter aggregates DR across listed domains rather than returning one per domain. Mitigation: in-process dict cache keyed by host (see `media-mentions-automation/repo/mentions.py::_DR_CACHE`). For repeat outlets within a single run, this is free.
- **`refdomains-history`** and similar timeseries-per-target endpoints: each call returns a single timeseries, so per-page tracking requires N separate calls. Mitigation: persist results to a JSON cache file with a 25–30 day TTL — the data is monthly-grouped and rarely changes week-to-week. Reference: `citation-tracking-automation/repo/generate_charts.py::REFDOMAINS_CACHE_TTL_DAYS`.

## Anti-patterns to avoid

1. **Per-URL loops calling a batchable endpoint**. The most common waste. If the endpoint supports `where` IN-filter, batch it.
2. **Retry storms** (the April incident). When the API returns 403/429 with a units-exhaustion message, the connector must short-circuit the rest of the run, not retry. Check for "units" + "limit"/"left" in the error body. Reference: `analytics-aggregation/repo/connectors/ahrefs.py::_is_quota_error`.
3. **Refreshing slow-changing data on a fast cadence**. Domain Ratings, refdomains histories, organic keywords for established pages — none of these need weekly refresh. Cache with a TTL.
4. **No usage probe**. Every Ahrefs-using script should log workspace usage delta. The connector pattern: probe before, run, probe after, log the diff. Without this, the next quota crisis is invisible until it lands.
5. **Hardcoding the workspace cap**. The connector's `API_INTEGRATION_LIMIT` is a backup; prefer `fetch_usage()["limit"]` for live decisions.

## Where to look for the cost of a new endpoint

Before adding a new Ahrefs call to a workflow, **measure it**. The pattern:

```python
before = fetch_usage()
# call endpoint
after = fetch_usage()
print(f"cost: {after['usage'] - before['usage']} units")
```

`subscription-info` is free, so there's no penalty for bracketing every test. Run it twice in case other concurrent calls noise the delta.

## Rotation / quota events

The workspace's `usage_reset_date` is exposed in `subscription-info/limits-and-usage`. If a project must run before the reset and the quota's near zero, check this date and either skip the run or downgrade scope. Don't retry blindly.

## Epoch repos that consume Ahrefs

- `analytics-aggregation` — full per-URL coverage of the content registry via batched `top-pages` (35 calls, ~3,800 units/run).
- `media-mentions-automation` — per-mention DR lookups, cached per host within a run.
- `citation-tracking-automation` — 8-slug refdomains-history, cached on disk for ~25 days.
- `backlinks-health-automation` — 2-domain `broken-backlinks`. Already cheap; skip optimization.

When adding a fifth, follow this same pattern: bracket usage probes, batch where the endpoint supports it, cache where it doesn't, and document the per-run cost in the connector's docstring so future agents know what they're modifying.
