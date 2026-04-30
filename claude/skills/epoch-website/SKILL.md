---
name: epoch-website
description: Reference for working with epoch.ai content programmatically. Use when scraping or fetching metadata from epoch.ai, building or maintaining a content registry, classifying Epoch URLs, walking publication source files (blog, gradient updates, data insights, FrontierMath, Epoch After Hours), or planning CI workflows that need access to the website source.
user-invocable: false
---

# Epoch website source is locally available

The Astro source for [epoch.ai](https://epoch.ai) is checked out locally at:

```
~/My Drive/repos/epoch-website-astro
```

Remote: `epoch-research/epoch-website-astro` (private). Always prefer reading the local source files over scraping the live site.

## Why prefer the local repo

- **Accurate dates.** Markdown frontmatter has real ISO timestamps (`date: 2024-06-19T00:00:00.000Z`). The live site exposes a derivative `pagefind:date` meta tag that's lossier.
- **Richer metadata.** Frontmatter includes `title`, `authors`, `tags`, and — most importantly — `relatedDatasets` (explicit cross-references between content, e.g. a data-insight pointing at `/data/ai-models`). None of this is available from scraped HTML.
- **Faster.** Filesystem walk vs ~1700 HTTP requests.
- **More reliable.** Works regardless of the live site's state.

## Content layout

Publication content lives under `src/data/`:

| Type             | Directory                  | Pattern              |
|------------------|----------------------------|----------------------|
| `blog`           | `src/data/blog/`           | `*/index.mdx`        |
| `gradient_update`| `src/data/gradient-updates/`| `*/index.mdx`       |
| `data_insight`   | `src/data/data-insights/`  | `*.md`               |
| `frontiermath`   | `src/data/frontiermath/`   | `**/*.mdx`           |
| `event`          | `src/data/epoch-after-hours/` | `*/index.md`      |

Directory names use a `YYYY-MM-DD-slug` prefix; the prefix should be stripped to get the canonical URL slug. For example, `src/data/blog/2024-03-12-algorithmic-progress-in-language-models/index.mdx` maps to `https://epoch.ai/blog/algorithmic-progress-in-language-models`.

## Frontmatter conventions

```yaml
---
title: Training compute has scaled up faster for language than vision
authors:
  - Robi Rahman
  - David Owen
tags:
  - Scaling
date: 2024-06-19T00:00:00.000Z
relatedDatasets:
  - /data/ai-models
chart: compute-trend-language-vs-vision
---
```

Notes:
- `date` may be an ISO timestamp, a date-only string, or a YAML date — coerce defensively.
- `authors`, `tags`, `relatedDatasets` are lists; serialize as pipe-delimited strings if you need a flat representation.
- `relatedDatasets` is the strongest signal for cross-source content correlation.

## What the local repo does NOT cover

The repo is the authoritative source for *publications* (blog, gradient updates, data insights, FrontierMath pages, Epoch After Hours). It is not the source for *reference products* generated from data:

- `/benchmarks/*` — benchmark pages
- `/data/*` — data explorers
- `/models/*` — model reference pages
- `/topics/*` — topic landing pages

These are rendered from data sources rather than markdown. For these types, fall back to scraping the live sitemap (`https://epoch.ai/sitemap-0.xml`) for URL + title metadata. Lighter metadata is acceptable — they aren't discrete publication events.

## CI / remote-agent considerations

When code that depends on this repo runs on GitHub Actions or another remote machine, the repo must be checked out alongside the consuming repo:

- Use `actions/checkout@v6` with a fine-grained PAT that has read access to `epoch-research/epoch-website-astro`.
- Pass the local path via an environment variable (e.g. `WEBSITE_REPO_PATH`) so the consumer doesn't hardcode a developer machine path.
- If the repo isn't available, the consumer should degrade gracefully to live scraping rather than fail outright — the live site is a complete fallback for everything except the richer frontmatter fields.

## Pointers

- Astro project root: `~/My Drive/repos/epoch-website-astro/`
- Project-level CLAUDE.md (the website's own): `~/My Drive/repos/epoch-website-astro/CLAUDE.md` — has rules for newsletter and data-insight publishing workflows.
- Reference consumer of this skill: [`analytics-aggregation/repo/registry/scraper.py`](https://github.com/epoch-research/analytics-aggregation/blob/master/registry/scraper.py) — the dual-source scraper that reads local markdown for publications and falls back to the sitemap for reference pages.
