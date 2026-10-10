# Service access tools

Local tools for external services. Prefer them over ad-hoc API calls or
browser automation; each already carries the right account and auth.

| Service | Tool |
|---|---|
| Paper PDFs (DOI, URL, arXiv, title; Anna's Archive, LibGen, open access) | `bin/paper-fetch` |
| Book PDF candidates, local registration, staging, inspection and reviewed selection | `bin/paper-fetch` (`book-*` commands) |
| Gmail | `claude/bin/gmail.py` |
| Google Sheets | `claude/bin/sheets.py` |
| Slack | `claude/bin/slack.py` |
| Google Calendar | `gcalcli-epoch` (canonical Epoch OAuth grant; shared personal calendars) |
| Google Docs / Drive | `gdoc` (comments vs. suggestions: see `google-services.md`, "Reading comments and suggestions") |
| GitHub | `gh` |
| Signal (read-only) | `bin/sgn-read` |

Relative paths are under the dotfiles root, `~/My Drive/dotfiles/`. For
Google account and auth details, read `google-services.md` in this
directory.

## Paper PDFs

`paper-fetch get IDENT` is the single implementation for obtaining a paper PDF:
open-access routes first (Unpaywall, OpenAlex, arXiv, publisher page), then
LibGen -> Anna's Archive member fast download, then Anna's Archive SciDB, then a
browser job for hosts that only answer a real browser (PhilPapers, PhilArchive,
Anna's Archive HTML). It verifies that the file's text matches the requested
title or DOI before reporting `ok`. The `download-paper` skill drives it,
including the browser fallback; `add-bib-entry` uses it for attachments. Shared
logic lives in `lib/python/paper_fetch.py`; do not add another Anna's Archive
client. The tool reads the Anna's Archive key itself (see `secrets.md`) and
needs an active membership: `status: not-member` means the membership lapsed.

Book acquisition uses the same implementation. Its command and review-manifest
contract is in [book-acquisition.md](../../docs/book-acquisition.md); edition,
metadata and attachment decisions follow the shared
[bibliography policy](../../agents/bibliography-policy.md).

## Signal

Pablo uses Signal through `sgn`, his Emacs client, which runs `signal-cli` as
a daemon and stores every message in `~/.local/share/sgn/sgn.db`. Do not call
`signal-cli` directly: the daemon holds its lock, so commands hang. Read
history with `sgn-read chats|show|search`, which opens the database read-only.
There is no agent send path; sending goes through sgn in Emacs, and any message
on Pablo's behalf needs his confirmation.

## Google Drive original files

`gdoc download FILE_ID --account personal --output NEW_PATH` downloads the original
bytes of a PDF or other stored file. Use the appropriate account from
`google-services.md`. It refuses existing destinations and native Google Workspace
files; `gdoc export` remains the command for converting native documents.

The canonical `bin/gdoc` launcher extends the installed uv-managed CLI with
`claude/bin/gdoc_download.py`, reusing its parser, account selection, authentication,
and command allowlist. Existing commands are delegated unchanged. Downloads check
Drive permission, size, and available MD5 metadata before installing the file. The
installed package is not patched. If upstream adds `download`, the extension stops
with an explicit retirement message rather than shadowing that implementation.

## Browser-only flows

### Forethought Chrome profile

Pablo has a dedicated Forethought Chrome profile, shown as `forethought.org`
by browser discovery, signed in as `pablo@forethought.org` (verified 29 September
2026). Use this profile for Forethought work services when browser interaction
is authorized. Do not substitute the Personal or Epoch profile. The Personal
profile also has guest access to Pablo's Forethought Notion onboarding checklist;
Pablo explicitly permits using it for that checklist. This is distinct from the
Forethought account's full Notion workspace access.

Pablo signs in to the Forethought Slack workspace as `pablo.stafforini@gmail.com`
(user `pablo.stafforini`, `U0C72C0S1NW`), not as `pablo@forethought.org`.
`slack.py -w forethought` acts as that user. The workspace also lists a
`pablo@forethought.org` user (`U0C8B8TME65`). An agent created it in error by
assuming Pablo's Slack identity followed his Google account; it is now
deactivated and awaiting deletion by a workspace admin. It still appears in
user searches. Never use it: do not message, mention, invite or act as it.

Forethought Gmail syncs into mu4e (`gmail-maildir-sync --account forethought`);
Docs/Drive use `gdoc --account forethought`; the other mapped Google tools have no Forethought authentication. See `google-services.md` for account routing. Discover the connected
profile at runtime; browser/tab IDs are session-specific.

### Browser access rules

Before any browser step, load the `chrome-use` skill; it holds the browser rules.
