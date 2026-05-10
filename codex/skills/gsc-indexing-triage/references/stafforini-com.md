# stafforini.com Search Console reference

## Paths and tools

- Site repo: `/Users/pablostafforini/My Drive/repos/stafforini.com`
- Notes source: `/Users/pablostafforini/My Drive/notes`
- Bibliographic quote source: `/Users/pablostafforini/My Drive/bibliographic-notes`
- Gmail CLI: `/Users/pablostafforini/My Drive/dotfiles/claude/bin/gmail.py`
- Personal Gmail account flag: `--account personal`
- Persistent log: `/Users/pablostafforini/My Drive/repos/stafforini.com/logs/gsc-indexing.md`
- Domain property: `sc-domain:stafforini.com`
- Known accessible Search Console account in Chrome: `pablo@stafforini.com`, observed as `authuser=1`

Always prefer issue URLs extracted from the emails. Observed Page indexing item keys:

| Issue | item_key |
|---|---|
| Not found (404) | `CAMYDSAC` |
| Page with redirect | `CAMYCyAC` |
| Indexed, though blocked by robots.txt | `CAMYBCAD` |

## Commands

Query GSC emails:

```bash
python3 "/Users/pablostafforini/My Drive/dotfiles/claude/bin/gmail.py" query \
  'from:(sc-noreply@google.com) ("Page indexing" OR "Search Console") newer_than:45d' \
  --account personal --max 20
```

Read a message:

```bash
python3 "/Users/pablostafforini/My Drive/dotfiles/claude/bin/gmail.py" get MESSAGE_ID --account personal
```

Decode issue links:

```bash
tool=codex  # use claude in Claude Code
skill_file=$("$HOME/My Drive/dotfiles/bin/agent-skill" path gsc-indexing-triage --tool "$tool")
skill_dir=$(dirname "$skill_file")
python3 "$skill_dir/scripts/extract-gsc-links.py" --account personal MESSAGE_ID...
```

Archive a handled message:

```bash
python3 "/Users/pablostafforini/My Drive/dotfiles/claude/bin/gmail.py" archive MESSAGE_ID --account personal
```

## Site-specific checks

Use `npm test` or `bash scripts/test.sh`, not global `pytest`.

From `/Users/pablostafforini/My Drive/repos/stafforini.com`, use a production-profile temporary render before deploying:

```bash
tmp=$(mktemp -d)
trap 'trash "$tmp"' EXIT
hugo --minify --config hugo.toml,hugo.deploy.toml --destination "$tmp" --noBuildLock --quiet
python3 scripts/verify-site.py --dir "$tmp"
```

After quick deploy, check the live sitemap and representative URLs:

```bash
curl -Ls https://stafforini.com/sitemap.xml | python3 -c 'import sys; s=sys.stdin.read(); print(s.count("<url>"), "urls")'
curl -IL "https://example-affected-url"
```

## Recent seed context

On 2026-05-05, the current GSC failures were `Not found (404)` and `Page with redirect`. Fixes included:

- Excluding non-indexable pages from `sitemap.xml` with a shared `is-indexable` partial.
- Aligning head robots meta and sitemap inclusion.
- Adding Netlify redirects for old WordPress, doubled asset, Anki, and `blog/bostrom` URLs.
- Normalizing legacy `www.stafforini.com` and `/blog/` links in work-page canonical link rendering.
- Extending `scripts/verify-site.py` to catch sitemap noindex/canonical/broken-link problems in rendered output.

After deploy, Search Console validation was restarted:

- `Not found (404)`: `Validation started`, `Started: 05/05/2026`, `PENDING 375`, `FAILED 0`.
- `Page with redirect`: `Validation started`, `Started: 05/05/2026`, `PENDING 1,556`, `FAILED 0`.

Use this only as context. Always re-read the persistent log and current GSC examples before making new changes.

## Log entry template

```markdown
## YYYY-MM-DD - GSC indexing triage

- Messages:
  - `MESSAGE_ID` - SUBJECT
- Issues:
  - ISSUE_LABEL - ISSUE_URL
- Examples checked:
  - URL -> FINAL_URL, STATUS
- Root cause:
- Changes:
  - Repo commit/path summary
- Verification:
  - Command -> result
- Deploy:
  - Command/result or not done
- Browser validation:
  - Issue -> status/counts
- Archived:
  - MESSAGE_ID
- Follow-up:
```
