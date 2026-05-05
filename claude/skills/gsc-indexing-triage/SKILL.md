---
name: gsc-indexing-triage
description: Triage Google Search Console Page indexing emails for stafforini.com end to end. Use when the user says "GSC", "Google Search Console", "Search Console issues", "indexing issues", "validate fixes", or wants Codex/Claude to read Search Console emails, fix stafforini.com indexing problems, quick-deploy, validate fixes in the browser, archive handled emails, and write a persistent log.
---

# GSC indexing triage

Resolve Search Console Page indexing issues for `stafforini.com` from inbox alert to browser validation. This workflow is deliberately end-to-end, but deployment, Search Console validation, and email archiving are externally visible actions: do them only when the user's invocation explicitly authorizes that scope.

## First checks

1. Read `references/stafforini-com.md` from this skill.
2. Read `/Users/pablostafforini/My Drive/repos/stafforini.com/CLAUDE.md`.
3. Read the persistent log if it exists:
   `/Users/pablostafforini/My Drive/repos/stafforini.com/logs/gsc-indexing.md`.
4. Inspect worktrees before editing:
   - `/Users/pablostafforini/My Drive/repos/stafforini.com`
   - `/Users/pablostafforini/My Drive/notes` if org notes may need edits
   - `/Users/pablostafforini/My Drive/bibliographic-notes` if quote sources may need edits

Do not touch unrelated dirty files. Never edit `content/` directly.

## Gather current alerts

Use personal Gmail:

```bash
python3 "/Users/pablostafforini/My Drive/dotfiles/claude/bin/gmail.py" query \
  'from:(sc-noreply@google.com) ("Page indexing" OR "Search Console") newer_than:45d' \
  --account personal --max 20
```

Prioritize newest failed-validation emails and current validation-started emails. Read candidate messages with `get`; extract issue-detail links with this skill's helper:

```bash
scripts/extract-gsc-links.py --account personal MESSAGE_ID...
```

The helper decodes the raw Gmail HTML, finds the "View issue details" links, follows the `c.gle` redirect without needing browser auth, and prints the Search Console issue URL when available.

## Diagnose before changing

For each issue type, collect:

- Issue label, property, message ids, thread ids, and Search Console issue URL.
- Example URLs from Search Console if browser access is available.
- Whether the examples are still live problems by checking status, redirect chain, canonical, robots meta, sitemap membership, and internal links.

Do not assume every GSC example needs a redirect. Common root causes include stale sitemap URLs, `noindex` pages listed in the sitemap, old WordPress query URLs, legacy `www` URLs, deleted PDFs, Tango feed URLs, canonical mismatches, and generated content coming from upstream org files.

## Fix

Keep fixes at the source of truth:

- Notes: edit `/Users/pablostafforini/My Drive/notes/*.org`, then run `bash scripts/export-notes.sh`.
- Quotes: edit `/Users/pablostafforini/My Drive/bibliographic-notes/*.org`, then run the quote export path.
- Templates, sitemap, redirects, and verification: edit the `stafforini.com` repo.
- Generated `content/` files are outputs only.

Prefer root-cause fixes over broad catch-all redirects. If redirecting historical URL families is appropriate, keep Netlify rules specific enough to avoid masking future bugs.

## Verify locally

Run the relevant export first. Then run:

```bash
npm test
tmp=$(mktemp -d)
hugo --minify --config hugo.toml,hugo.deploy.toml --destination "$tmp" --noBuildLock --quiet
python3 scripts/verify-site.py --dir "$tmp"
trash "$tmp"
```

Also spot-check affected live-style URLs with `curl -IL` or equivalent. If verification cannot cover a class of issue, log the gap explicitly.

## Commit and deploy

Commit each logical change in the repo that owns it. If the user explicitly authorized end-to-end handling, quick-deploy after local verification:

```bash
bash scripts/deploy.sh --quick
```

When telling the user to deploy manually, mention the Emacs command too: `stafforini-deploy` with `C-u` for quick deploy.

After deploy, confirm the live site reflects the fix before Search Console validation. At minimum, fetch `https://stafforini.com/sitemap.xml` and spot-check representative example URLs.

## Browser validation

Use the browser surface available in the current agent:

- Codex: use the Browser Use skill/plugin when exposed. If the Node REPL browser tool is unavailable, say so explicitly before falling back.
- Claude: use the configured Chrome/browser tool when available.
- If using the user's existing Chrome via AppleScript, only do so when already logged in and the user has authorized validation. Do not handle passwords, OTP, CAPTCHA, or account recovery.

Known account hint: `pablo@stafforini.com` has had access to the domain property as `authuser=1`; `pablo.stafforini@gmail.com` may not.

For each issue:

1. Open the issue detail URL from the email or Search Console.
2. If validation details show a previous failure, open "SEE DETAILS".
3. Confirm visible examples are no longer broken on the live site.
4. Click "START NEW VALIDATION" or "VALIDATE FIX".
5. Record the resulting status and counts, such as `Validation started`, `Started: DATE`, `PENDING`, and `FAILED`.

Do not start validation if representative examples still fail live checks.

## Archive emails

Archive only emails that were handled or logged as intentionally non-actionable:

```bash
python3 "/Users/pablostafforini/My Drive/dotfiles/claude/bin/gmail.py" archive MESSAGE_ID --account personal
```

Do not archive unrelated Search Console messages that still need investigation.

## Persistent log

Append to `/Users/pablostafforini/My Drive/repos/stafforini.com/logs/gsc-indexing.md` before finishing. Include:

- Date/time and agent.
- Gmail message ids and subjects.
- GSC issue labels and issue URLs.
- Representative examples and live status after fix.
- Root cause.
- Files changed and commits.
- Verification commands and results.
- Deploy result.
- Browser validation result and counts.
- Emails archived.
- Open follow-up or reason none remains.

If the log file does not exist, create it with a short heading and append the first dated entry.
