---
name: humanize
description: Rewrite prose to remove patterns that read as AI-generated — vocabulary tells (delve, tapestry, pivotal, underscore), copula avoidance (stands as, serves as, marks), negative parallelism, rule-of-three stacks, trailing -ing summary clauses, significance puffery, compulsive summaries, em-dash overuse, mechanical bolding, bullet-with-bold-header lists, render artefacts (oaicite, contentReference, turn0search0), and similar fingerprints. Use for /humanize, de-slop, removing AI tells, "sounds like ChatGPT/Claude/Gemini", anti-AI-detection cleanup, or as the final pass before publishing AI-assisted prose. Also invoked by other skills (e.g. wikipedia-article) as a last-mile cleanup phase.
---

# Humanize

Rewrite prose to remove the patterns Wikipedia editors and other careful readers treat as signs of AI authorship — without sterilising voice or altering facts.

**Iron rule: clusters, not single tells.** A lone em dash means nothing. An em dash plus a triadic adjective stack plus "stands as a testament to" in the same paragraph is fingerprinted. Edit clusters; ignore singletons.

## When to use

- The user runs `/humanize` on a file or pasted text.
- The user says "de-slop", "remove AI tells", "make this sound less like ChatGPT/Claude", "humanise this", "kill the LLM register".
- Another skill calls humanize as a final cleanup pass (e.g. `wikipedia-article` Phase 5).

## When NOT to use

- Code, configuration, schemas, or other non-prose artefacts.
- Verbatim quotations from sources — preserve them even if they contain tells.
- Organic human writing the user hasn't flagged as AI-touched. Humanize is for output you have reason to think is LLM-generated; do not redact natural voice.
- Non-English source text — the catalogue below is English-only.

## Inputs

- One or more file paths, OR inline text.
- Optional **context flag**: `wikipedia | markdown | email | blog | casual`. Default is `markdown`.
  - `wikipedia` — preserve `{{cite …}}`, `<ref>`/`<ref name="…" />`, wikilinks, infobox markup, `{{reflist}}`, categories, navboxes, `{{Authority control}}`; sentence-case section headings; honour the article's ENGVAR.
  - `markdown` (default) — preserve fenced code, inline code, link syntax, frontmatter, and admonitions.
  - `email`, `blog`, `casual` — preserve voice more aggressively; tolerate looser register; tolerate slightly higher em-dash density.

## Workflow

1. **Read the entire input first.** Tells are cluster-level; you cannot detect them line by line.
2. **Pass 1 — mark.** Walk the prose and note co-occurring tells from the catalogue. Ignore lone singletons. Note which paragraphs have ≥2 tells.
3. **Pass 2 — edit in place.** Use `Edit` one cluster at a time, in context. Never bulk-replace by keyword.
4. **Preserve.** Citations (URLs, identifiers, `{{cite …}}` parameters), direct quotations, factual claims, structural divisions, named-ref keys, fenced code, links, ENGVAR.
5. **Pass 3 — re-read.** Check for tells you just introduced (especially over-correction into stilted, hedge-laden prose).
6. **Report.** Use the output format below.

## Tells catalogue

Each entry is a tell paired with its fix. Apply only when a tell appears as part of a cluster (≥2 tells in the same paragraph or section).

### Vocabulary

| Tell | Fix |
|---|---|
| AI vocab in density — *delve, intricate, pivotal, tapestry, testament, underscore, landscape (figurative), meticulous, vibrant, garner, bolster, foster, enhance, align with, showcasing, highlighting, fostering, robust, comprehensive* | Replace with plain alternatives. Cap at roughly one per ~500 words. |
| **Copula avoidance** — *serves as, stands as, marks, represents, boasts, features, offers, maintains* in place of *is/are/has* | Switch back to the plain copula unless the avoidance is genuinely earning a different meaning. |
| **"Concrete" / "robust" as filler** — especially in defences against AI accusations | Cut the adjective. |
| **Elegant variation** — three synonyms for the same referent in one paragraph | Pick one term and reuse it. |

### Phrasing and syntax

- **Negative parallelism**: "not just X, but Y", "not X — Y", "it's not about X, it's about Y", used as a recurring move. Allow at most one instance per piece.
- **Rule of three**: stacked triadic adjectives or three-item lists used to inflate thin content. Cut to two items, or to one specific item.
- **Trailing -ing summary clauses**: "…, highlighting the broader significance of …", "…, reflecting an enduring legacy of …", "…, underscoring its pivotal role in …". Delete the clause or fold the claim into the main sentence with a real verb.
- **Significance puffery**: *stands as a testament to, marks a pivotal moment, underscores the importance of, a key turning point, deeply rooted, broader trends, lasting legacy*. Either back the claim with a specific source and rewrite as a concrete fact, or remove.
- **Compulsive summary**: "Overall," / "In conclusion," / "In summary," appended to short passages that need no restatement. Remove.
- **Vague attribution**: "observers have noted", "experts argue", "industry reports", "some critics", "several sources". Either name the source or remove.
- **Promotional / travel-guide register**: "nestled", "vibrant", "rich heritage", "renowned for", "boasts a … of", "natural beauty". Replace with one concrete fact.

### Structure

- **Outline-shaped tail**: a final "Challenges", "Future Prospects", or "Looking Ahead" section with speculative content. Cut unless the sources genuinely discuss it.
- **Mechanical bolding**: every keyword or "key takeaway" bolded. Strip back to boldface only at first definition of a defined term or in table headers.
- **Bullet-with-bold-header lists**: `• **Header:** prose…` or `1. **Header:** prose…`. Convert to prose paragraphs unless the content is genuinely list-shaped (e.g. a parts list).
- **Title Case In Section Headings** when the house style is sentence case. Lowercase non-proper-noun words.
- **Skipped heading levels** (`##` → `####`). Re-level.
- **Horizontal rules (`---`) before headings**. Remove.

### Formatting

- **Em-dash overuse**: more than ~1 per 200 words is the fingerprint. Reduce to commas, parentheses, or full stops; keep em dashes where the break is genuinely needed.
- **Curly quotes/apostrophes** when straight is the house style. Normalise.
- **Markdown inside wikitext** (`**bold**`, `*italic*`, leading `#` headings) when the target is wikicode. Convert to `'''bold'''`, `''italic''`, `==Heading==`.
- **Stray render artefacts** (always remove, even as singletons): `contentReference`, `oaicite`, `oai_citation`, `turn0search0`, `grok_card`, `+1`, `attached_file`, unfilled placeholders like `[Birth Date]`, `[insert citation]`, `[YOUR_NAME]`.
- **Emoji as bullets or section markers** (✓, ✗, •, 🔹) when the house style is plain. Remove.

### Citations and references

- **Invalid identifiers**: dead URLs, malformed ISBNs, unresolvable DOIs, DOIs that resolve to the wrong paper. Verify; if you cannot, remove the citation and flag in the report.
- **`utm_source=` / `utm_medium=`** parameters on cited URLs. Strip the query string.
- **Named refs declared but unused** (`<ref name="x">…</ref>` with no second invocation). Inline as a plain `<ref>` or delete the name.
- **"Several sources" / "as reported by various outlets"** with only one or two refs. Cite the specific source by name.

### Meta-text (always remove, even as singletons)

Knowledge-cutoff disclaimers ("As of my last update…"), "as an AI language model", refusal phrases ("I cannot generate…"), abrupt mid-sentence cutoffs, prompt-fragment echoes ("Sure! Here is the article…"), and visible system-prompt scaffolding.

## Calibration rules

1. **Cluster threshold.** Edit only when ≥2 tells co-occur in a paragraph or section. The exceptions — always-remove — are the render artefacts and the meta-text categories.
2. **Don't sterilise.** Keep specific, surprising, vivid word choices. "Tapestry" describing an actual woven artefact is fine; "tapestry of innovation" is not.
3. **Don't equalise.** Real human writing has irregular cadence, mid-paragraph asides, and the occasional cliché. Don't smooth those into beige.
4. **Preserve quoted material verbatim.** Even if the quote contains *delve* — that's the source's word.
5. **Pre-November-2022 text is presumed human.** If you can tell content predates ChatGPT (commit dates, references, internal evidence), don't humanise it.
6. **Don't add insecurity to "sound human".** Plain prose, not hedged prose. "X is Y" beats "I think X might in some sense be Y".
7. **No new facts.** Humanise rewords; it does not research. If a sentence is empty of meaning once the puffery is stripped, flag it for the user; don't backfill.

## Output format

Final reply to the user (or to the calling skill):

```
Humanised <path>.

Vocabulary: <n> edits   (e.g. delve→examine ×3; serves as→is ×2)
Phrasing:   <n> edits   (e.g. removed 4 trailing -ing clauses; cut 2 "stands as a testament")
Structure:  <n> edits   (e.g. removed "Future Prospects" section; merged 3 bullet-bold rows into prose)
Formatting: <n> edits   (e.g. em-dashes 18 → 4; curly quotes → straight)
Citations:  <n> edits   (e.g. stripped utm_source from 6 refs; removed 1 unused named ref)
Meta:       <n> edits   (e.g. removed knowledge-cutoff disclaimer)

Left untouched (load-bearing): <list with one-line rationale each, or "none">
Flagged for the user: <claims left empty after puffery stripped, citations that couldn't be verified, or "none">
```

## Common mistakes

| Mistake | Fix |
|---|---|
| Bulk-replacing every "delve" | Edit by cluster, not by keyword. One "delve" in a 1,000-word piece is not a tell. |
| Stripping all em dashes | Reduce density; do not ban the punctuation. |
| Replacing copulas mechanically | "Serves as" sometimes earns meaning. Weigh each instance. |
| Changing facts to dodge AI register | Never. Keep the fact; reword the framing. |
| Adding wordy hedging to "sound human" | Plain prose is human prose. Hedging is its own tell. |
| Running humanise on quotations | Quotes are verbatim. Stop. |
| Editing pre-2022 prose | Strong presumption of human authorship. Leave alone. |
| Touching code/config | Out of scope. Only prose. |

## Worked example

Before (one paragraph, fictional org):

> Tapestry Labs stands as a testament to the vibrant landscape of modern AI research, nestled at the intersection of three pivotal domains — interpretability, alignment, and capability. Boasting a roster of leading scientists, the lab serves as a foundational pillar of the field, highlighting the broader importance of safety-conscious development.

After:

> Tapestry Labs is an AI research lab working on interpretability, alignment, and capability. Its researchers have published on [specific topic / venue].

Tells removed: *stands as a testament*, *vibrant landscape*, *nestled*, *pivotal*, *—* (overuse), rule-of-three adjective stack, *boasting*, *serves as*, *highlighting the broader importance*, *foundational pillar*. The replacement is shorter, anchored to a citation slot the writer must fill, and humanly drab. Better drab and true than ornate and synthetic.
