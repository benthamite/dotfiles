---
name: update-situational-awareness-filing
description: Update Pablo's Situational Awareness LP public note after a new SEC filing. Use when a new SA LP 13F, Schedule 13G/13G-A, or Schedule 13D/13D-A appears; when asked to refresh, publish, or verify the SA LP portfolio, backtest, charts, calculator, sensitivity analysis, filing table, or disclosure prose; or when any generated SA LP artifact may be stale.
---

# Update Situational Awareness Filing

Update the note and every derived artifact as one filing refresh. Treat a partial refresh as a failure.

## Interpret the request

Use these modes:

- **Prepare**: research, edit, refresh, and verify locally. Do not push or deploy.
- **Publish**: do all prepare steps, then commit, push, deploy, and verify the live page. Treat an explicit request to publish, deploy, or update the live note as authorization.
- **Verify**: inspect freshness without editing unless the user also asks for a fix.

Use the latest filing by default. If the user names an accession, form, or reporting period, use that target.

## Preflight

1. Work from:
   - Note: `/Users/pablostafforini/My Drive/notes/public/situational-awareness-lp.org`
   - Notes repo: `/Users/pablostafforini/My Drive/notes`
   - Site repo: `/Users/pablostafforini/repos/stafforini.com`
2. Read both repositories' instructions and inspect both worktrees. Preserve unrelated changes.
3. Use `org-note-conventions` and `personalize` for published prose.
4. Read `/Users/pablostafforini/My Drive/dotfiles/claude/context/secrets.md` before using `pass`. Never print `MARKETDATA_KEY`.
5. Never edit generated `content/` files.

## Establish the filing record

Use SEC primary sources for every filing fact:

1. Read the filing index, cover page, and information-table XML.
2. Record the form, reporting date, filing and acceptance time, accession, holdings count, reported total value, and each CUSIP, position type, and value.
3. Cross-check the parsed row count and total against the SEC cover page.
4. Resolve new CUSIPs from SEC issuer data or another authoritative issuer source. Add mappings to `CUSIP_TICKER`; do not guess tickers.
5. Inspect nearby 13G, 13G/A, 13D, and 13D/A filings. Order same-day filings by acceptance time.
6. Decide whether an issuer-specific filing creates a temporary layered portfolio or is superseded by a later full 13F. Preserve historically valid intermediate periods.

A 13F is a quarter-end snapshot. Do not describe it as the fund's current portfolio when later public evidence makes it stale.

## Review later material events

Search for material events between the report date and the current date, such as forced sales, margin calls, block trades, or amendments.

- Prefer SEC filings.
- Use named, reputable reporting when primary filings do not disclose the event.
- State uncertainty when reports conflict.
- Add a concise dated caveat when the filing is materially stale.
- Explain why the mechanical copycat still follows disclosed portfolios instead of reconstructing undisclosed holdings.
- Do not infer exact current positions from incomplete reports.

## Update the Org source

Make the smallest coherent edits:

1. Refresh `sa-data` and add any required CUSIP mappings.
2. Update the disclosure chronology and the **Latest 13G disclosure** section.
3. Update **Staying updated**:
   - filing count;
   - quarter end;
   - 45-day deadline;
   - actual filing date;
   - days early.
4. Update prose and footnotes for material post-period events.
5. Renumber footnotes safely when adding one.
6. Update `#+lastmod:`.

Keep SEC acceptance time, SEC filing date, report date, and statutory deadline distinct.

## Refresh every derived artifact

A new filing or model change requires the sensitivity sweep. Daily price refreshes do not.

Run the complete refresh locally first:

```bash
cd /Users/pablostafforini/repos/stafforini.com
DRY_RUN=1 bash scripts/sa-lp-refresh.sh --with-sensitivity
```

The required block order is:

1. `sa-data`
2. `sa-perf`
3. `sa-chart`
4. `sa-chart-ais`
5. `sa-sensitivity`
6. `sa-delay`
7. `sa-calc`

The dry run still evaluates blocks and exports the note, but it does not commit, push, or deploy.

### MarketData constraints

Use MarketData for historical option chains and quotes. Do not add a silent historical-data fallback.

- Reuse and retain `.sa-lp-option-cache/`.
- Treat documented “no eligible contract” exclusions as valid only when the API call succeeded.
- Treat quota, authentication, transport, or API failures as refresh failures.
- Search the sensitivity results for literal `err` cells. The sensitivity wrapper can convert an API failure into a completed Babel block with `err` output.
- If the daily credit cap is exhausted, stop. Do not publish mixed-generation output. Record which blocks remain stale and retry after the quota resets.
- Do not treat the calculator's current-contract fallback as validation of historical backtest data.

## Prove freshness

For a full 13F, run the bundled checker:

```bash
python3 SKILL_DIR/scripts/check_freshness.py \
  --quarter QN_YYYY \
  --filing-date YYYY-MM-DD \
  --effective-date YYYY-MM-DD \
  --accession ACCESSION \
  --holding-count COUNT \
  --reported-total TOTAL
```

Replace `SKILL_DIR` with the active skill directory. The effective date is the
first trading date on which the model can act. Omit it only when it equals the
official filing date. Get the count and total from the SEC cover page. Then
inspect the diffs directly. The checker is necessary, but it does not prove
that the sensitivity sweep ran in this refresh or that all prose is current.

For an issuer-specific filing without a new 13F, perform the equivalent checks manually. Its accession and synthetic layered period may live outside the `sa-data` 13F result.

Require all of these conditions:

- `sa-data` contains the target accession and complete holdings.
- `sa-perf` ends the prior period on the model's effective date. Once a later trading date exists, it also includes the new quarter as the active final period.
- `sa-delay` includes the new filing transition or window.
- `sa-sensitivity` was rerun and contains no `err` cells.
- `static/images/sa-lp-returns.html` and `sa-lp-returns-ais.html` include a vertical rebalance marker on the new filing date and current data.
- `static/images/sa-lp-calculator.html` names the new quarter and filing date and contains the new holdings.
- The prior active disclosure label appears only where it remains historically correct.
- The explanatory prose agrees with the generated results.

A successful export, one corrected chart, or one corrected calculator label is not sufficient.

## Verify the site

1. Run the project tests:
   ```bash
   cd /Users/pablostafforini/repos/stafforini.com
   npm test
   ```
2. Build and verify the production configuration in a temporary directory:
   ```bash
   sa_verify_dir=$(mktemp -d)
   trap 'trash "$sa_verify_dir"' EXIT
   hugo --minify --config hugo.toml,hugo.deploy.toml \
     --destination "$sa_verify_dir" --noBuildLock --quiet
   python3 scripts/verify-site.py --dir "$sa_verify_dir"
   ```
3. Inspect the rendered note, both return charts, and the calculator in a real browser. Confirm the new quarter, filing date, holdings, and absence of the stale current label.

## Commit and publish

Commit only scoped files.

In the notes repo, expect:

- `public/situational-awareness-lp.org`
- changed files under `.sa-lp-option-cache/`

In the site repo, expect:

- `static/images/sa-lp-returns.html`
- `static/images/sa-lp-returns-ais.html`
- `static/images/sa-lp-calculator.html`
- generated metadata only when the source change requires it

Keep the commits single-purpose. Preserve unrelated worktree changes.

For publish mode:

1. Push the scoped commits in both repositories.
2. Run `post-push-ci` when either repository has relevant remote checks.
3. Deploy:
   ```bash
   cd /Users/pablostafforini/repos/stafforini.com
   bash scripts/deploy.sh --fast-note
   ```
4. Reload the production page with cache busting.
5. Verify the live note, both charts, and calculator in a real browser.
6. Compare each live static asset's hash with the committed local artifact when practical.

Report the target filing, commits, deployment URL, exact live observations, and any remaining stale artifact.
