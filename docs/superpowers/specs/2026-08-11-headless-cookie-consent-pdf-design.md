# Headless Cookie-Consent PDF Design

## Goal

Generate PDFs from public webpages without showing cookie-consent prompts or
their overlays in the output. The command remains fully programmatic and uses a
headless system Chrome process. It must not open a visible browser, copy the
personal Chrome profile, or require a cookie-export extension.

The renderer must not silently produce a PDF when it cannot clear a blocking
consent prompt. It must fail with a useful error instead.

## Current behavior and problem

`eww-extras-url-to-pdf` calls `eww-extras-render-url.js`, which launches Chrome
with `--headless=new` and prints through the Chrome DevTools Protocol. The
renderer already removes common dialogs, cookie elements, backdrops, and large
fixed overlays before printing.

Authentication currently comes from persistent copies of the complete Chrome
profile at `~/.chrome-data-pdf/` and `~/.chrome-data-html/`. Those copies contain
sensitive browser state and require Chrome to be closed while they are updated.
The renderer disables extensions and never reads a `cookies.txt` file, so the
cookie-export extensions do not serve this workflow.

Removing an overlay alone is incomplete. Some consent managers do not load or
unlock the page until the visitor submits a choice. The renderer therefore
needs to perform the consent action before it uses DOM cleanup as a fallback.

## Design decision

Use `@duckduckgo/autoconsent` as the consent engine, configured for opt-out, in
a Playwright-controlled system Chrome process. Playwright is only the control
layer for the existing headless Chrome execution. The implementation must not
use coordinate clicks, screenshot geometry, or selectors maintained for
individual sites in this repository.

Autoconsent owns consent-platform detection and the rules for platforms such as
OneTrust, Cookiebot, Sourcepoint, Quantcast, and Usercentrics. The dotfiles code
provides a thin adapter that launches Chrome, injects the published engine and
rules, waits for a bounded result, applies the existing generic cleanup, checks
the final page state, and prints the PDF.

The implementation uses `playwright-core` with the configured system Chrome
executable. It must not download or manage a separate Chromium build.

## Runtime components

### Emacs command layer

`eww-extras.el` continues to own URL and output-path selection, process
lifecycle, timeout enforcement, callbacks, and user-facing errors. It stops
selecting persistent copied-profile directories and passes only the URL,
output, type, Chrome executable, and renderer entry point.

The old profile-copy commands become obsolete. They are removed after the new
renderer passes end-to-end verification and the persistent copies are moved to
Trash.

### Headless renderer

`eww-extras-render-url.js` launches system Chrome headlessly through
`playwright-core`. Each invocation creates an isolated temporary browser
context outside the Google Drive tree. The context has no personal cookies,
extensions, history, credentials, or other Chrome profile state.

The renderer injects the Autoconsent script and pinned rule bundle before page
scripts run. It configures the engine with `autoAction: "optOut"`, pre-hiding,
cosmetic rules, generated rules, and heuristic detection enabled. It injects
the engine into child frames as supported by the published integration.

The renderer retains a small local cleanup module for generic residual UI. It
may close semantically labelled dialogs and remove remaining consent-related
dialogs, backdrops, and large fixed overlays. It must not contain domain names,
site-specific selectors, or viewport coordinates.

### Dependency cache

Exact `playwright-core` and `@duckduckgo/autoconsent` versions are recorded in
tracked manifests at `emacs/extras/scripts/eww-extras-renderer/package.json`
and `package-lock.json`. No dependency tree, build output, or npm cache may be
created under the Google Drive repository.

The renderer installs the locked graph with `npm ci --ignore-scripts` into
`~/Library/Caches/eww-extras-renderer/LOCK_HASH/` and reuses that cache on later
runs. It uses system Chrome, so dependency installation must not download a
browser. A lockfile change creates a new cache entry rather than mutating an
existing one. Installation failures return a clear error and do not fall back
to an unpinned package or `npx`.

## Data flow

1. Emacs resolves the URL and output path and starts the renderer.
2. The renderer ensures that the lock-hash dependency cache exists.
3. It creates a permission-restricted temporary browser directory outside
   Google Drive.
4. It launches system Chrome with `--headless=new` and a fresh context.
5. It registers Autoconsent and its rules before navigation.
6. It navigates to the target and handles consent concurrently with page load.
7. It waits until Autoconsent reports opt-out success, reports no supported
   prompt after a short observation period, or reaches the consent deadline.
8. It runs generic overlay and scroll-lock cleanup.
9. It checks for a remaining visible consent dialog or blocking overlay.
10. If the check passes, it prints the PDF or writes the HTML.
11. It closes Chrome and removes the temporary browser directory on every exit
    path.
12. The existing Emacs sentinel validates the nonempty output and runs the
    callback.

A website may set a consent cookie inside the temporary context. That cookie
exists only for the current rendering run and is deleted with the context. The
workflow does not export, import, or retain cookies between runs.

## Failure behavior

The renderer fails without producing a successful output when:

- Chrome or the locked dependency cache cannot start;
- navigation fails before useful content loads;
- Autoconsent reports an error that prevents handling the prompt;
- the consent deadline expires while a blocking prompt remains;
- the final check finds a visible consent dialog, consent-related cross-frame
  obstruction, scroll lock, or large blocking overlay; or
- printing or file writing fails.

An unknown prompt is not a reason to add an immediate local site rule. The
error identifies the URL, detected consent platform when known, and failure
stage without logging cookies or page contents. A local rule is considered
only when an important site repeatedly fails and no upstream rule is available.

Cleanup is best-effort only after a real consent action or a no-prompt result.
It must not convert an unresolved consent action into a false success merely by
hiding a visible element while the page remains blocked.

## Performance requirements

Consent handling runs during page loading rather than as a separate fixed
delay. The implementation targets:

- no more than one second of added latency on pages without a prompt;
- ordinary supported prompts completing within seven seconds, excluding slow
  page delivery;
- a five-second consent-handling budget;
- unresolved prompts failing within twelve seconds; and
- retention of the existing 30-second outer process limit.

The current unconditional 2.5-second post-load sleep is replaced by explicit
consent completion and a short layout-settle delay of approximately 250 ms.
Implementation verification records end-to-end timings instead of treating
these estimates as established results.

## Maintainability constraints

- Do not implement repository-owned rules for common consent platforms.
- Do not use coordinate clicks, screenshots, viewport geometry, or visual OCR
  to operate prompts.
- Do not add selectors for individual domains to the generic cleanup module.
- Pin dependency versions and update them intentionally through the lockfile.
- Treat upstream rule updates as dependency updates with regression tests.
- Keep the adapter independent from Autoconsent's internal rule format beyond
  its documented integration interface.
- Prefer a clear unsupported-prompt error over growing an unreviewed local rule
  collection.

## Verification

### Deterministic tests

Tests use local fixtures and a real headless system Chrome process to cover:

- a page without a consent prompt;
- representative supported consent-manager prompts;
- an opt-out flow that changes page state before content unlocks;
- a generic residual overlay removed after consent handling;
- an unknown blocking prompt that must fail without a successful PDF;
- HTML and PDF output creation;
- cleanup of temporary browser data after success, timeout, and renderer error;
- absence of personal-profile and `cookies.txt` arguments; and
- dependency installation into the outside-Drive cache from the locked graph.

Unit tests continue to cover Emacs command construction and sentinel behavior.
They also assert that the command no longer references the persistent Chrome
profile copies.

### End-to-end verification

Before removing the old workflow, run the real interactive Emacs command
against a small set of public pages that includes no prompt, at least two
different supported consent platforms, a dynamically loaded prompt, and one
unsupported fixture. Inspect the generated PDFs to confirm that supported pages
contain the expected article content without consent UI and that the
unsupported case produces no successful PDF.

Record command-to-PDF timings for each successful case and confirm the stated
performance bounds. Verify that no renderer process or temporary browser
directory remains afterward.

Only after these checks pass may the obsolete persistent profile copies be
moved to Trash and the two cookie-export extensions be removed. Those cleanup
actions are separate from the code commit and must not happen before the new
user-visible workflow is verified.

## Non-goals

- Rendering pages that require a personal authenticated browser session.
- Preserving consent choices between runs.
- Blocking all tracking or enforcing a complete browser privacy policy.
- Maintaining a repository-specific catalogue of website consent prompts.
- Guaranteeing support for every future custom prompt. Unsupported prompts
  fail explicitly instead of producing a known-bad PDF.
