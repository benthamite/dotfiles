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

For background web research, use the web search/browsing tool and the mapped
service clients or direct public downloads. Do not switch to Pablo's live Chrome
because an HTTP request fails or a site presents a challenge. Live Chrome
automation can steal focus even when it only opens or navigates a tab; the
restriction is not limited to launchers or new windows. Use that browser only
when Pablo explicitly authorizes interaction with it for the current task.
Otherwise record the blocked route and continue through non-disruptive routes.

For Codex automation that depends on an existing Chrome session, use the
installed Chrome plugin through its `control-chrome` skill and browser client.
Verify the connection with a read-only tab listing before navigating. A failed
generic Playwright or guessed CDP connection does not show that Chrome control
is unavailable.

Classify a failure before recovering from it. The plugin's
`chrome-troubleshooting` checks and its `open-chrome-window.js` step cover only
a communication failure: setup, browser selection or `browser.tabs.list()`
fails. Once a browser is selected and a tab exists, a `goto` timeout ("Timed
out waiting for tab … to navigate"), a hung page read, or `js execution timed
out; kernel reset` is a page or REPL failure. The connection checks pass and
prove nothing about it. Recover without asking: read the tab again; after a
kernel reset, rerun the bootstrap and rebind the browser; then open a fresh
tab from that browser and retry the navigation once. Challenge-protected hosts
(DDoS-Guard, Cloudflare; for example Anna's Archive) can outlast `goto`'s load
wait while their interstitial runs, so a timeout there is expected: read the
tab until its title changes before calling the page unavailable. Treat it as a
communication failure only if a fresh `browser.tabs.list()` also fails.

Never open a new Chrome window, and never ask to: Pablo rejected an
agent-opened window on 2026-09-25, and a window does not fix a page stall.
When live Chrome use is authorized, work in tabs of the existing window.
If communication still fails after the
documented checks, report which check failed.

`chrome-profile-open <alias> URL` is only a launcher for a page the user wants
opened manually. It activates Chrome and can steal focus. Never use it,
AppleScript, System Events, screenshots, or coordinate clicking as a fallback
for Codex browser automation. If the Chrome plugin cannot connect after its
documented checks, fail closed and report the connection problem.

Configure launcher aliases with `chrome-profile-open --setup <alias>`.
Project wrappers may call the launcher when manual opening is the requested
action, for example `trajectory-open URL` for Trajectory/CR pages.
