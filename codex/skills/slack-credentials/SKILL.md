---
name: slack-credentials
description: Refresh or add the xoxc/xoxd browser-session credentials that claude/bin/slack.py uses for a Slack workspace. Use when slack.py reports not_authed, invalid_auth or token_revoked, when a workspace has no credentials yet, or when Pablo asks to refresh Slack tokens. Not for Slack's own OAuth apps or bot tokens.
---

# Slack session credentials

`claude/bin/slack.py` authenticates as Pablo with two values per workspace: the
`xoxc-…` token (Bearer) and Slack's `d` cookie (`xoxd-…`). Where each workspace
keeps them is the `WORKSPACES` registry in `slack.py`; for the `automation`
source it is an item of the personal 1Password `Automation` vault with fields
`token` and `cookie` (see `claude/context/secrets.md`).

Neither value is ever printed, returned to the agent, or written to a file.

## When both values change

Signing out of Slack in that Chrome profile, or changing the account's email
(which signs out every session; Forethought, 2026-10-23), replaces both the
token and the cookie. Expect to do steps 1 and 2. A stale token alone needs only
step 1.

## Working with Pablo

The agent runs every command, including both `op-clipboard-store` calls.
Pablo's only possible jobs are pasting one console line (1b) and copying the
cookie (2). Ask for one job per message, worded as in those steps, and run the
store as soon as he says "done". Never ask him to paste a command into Emacs
or a terminal while a credential is on the clipboard: Emacs yanks the newer
clipboard contents, so he would paste the secret instead of the command.

## 1. The token

1. Follow the `chrome-use` skill to open, in the Chrome profile signed in to
   that workspace (`baish` for becas-aisar and baish, `forethought` for
   forethought), a tab on `https://app.slack.com/client/TEAM_ID`
   (forethought: `TC3AMF287`). The workspace's subdomain is its `domain` in
   `slack.py`'s `WORKSPACES` without `.slack.com` (`forethought-ea`,
   `becasaisar`, `baish`, ...).
2. **1a, agent alone.** Run this page script with the browser tool, replacing
   `DOMAIN`. It copies the token to the macOS clipboard and returns only its
   length and prefix:

   ```js
   const teams = Object.values(JSON.parse(localStorage.localConfig_v2).teams);
   const t = teams.find(x => x.domain === 'DOMAIN');
   let r;
   if (!t) { r = 'no such team; domains: ' + teams.map(x => x.domain).join(','); }
   else { try { window.focus(); await navigator.clipboard.writeText(t.token);
     r = 'copied, length ' + t.token.length + ', xoxc prefix ' + t.token.startsWith('xoxc-'); }
     catch (e) { r = 'failed: ' + e.name; } }
   r
   ```

   Run it as top-level code (the tool evaluates with top-level `await`); an
   async IIFE returns `{}`. Never return `t.token` itself: the extension
   blocks it, and it must not enter the transcript.
3. **1b, if the auto-mode classifier denies 1a** (it is inconsistent: on
   2026-10-23 it passed this script in one session and denied it twice as
   "Credential Exploration" in another). Do not retry or look for a way
   around it. Put this line on the clipboard with
   `printf '%s' "LINE" | claude/bin/kill-ring-put --clipboard`:

   ```js
   copy(Object.values(JSON.parse(localStorage.localConfig_v2).teams).find(x => x.domain === 'DOMAIN').token)
   ```

   Then tell Pablo exactly: "In Chrome, in your PROFILE window, open the tab
   titled Slack. Press ⌥⌘J, then ⌘V, then Return. Don't copy anything else;
   just tell me 'done'."
4. Store it: `op-clipboard-store --vault Automation --account my.1password.com
   --title ITEM --field token --prefix xoxc- --token` (add `--force` when the
   item exists). Run it immediately after the copy: anything that touches
   the clipboard in between makes it fail on the prefix check. Then run the
   check in step 3; if it still says `invalid_auth`, the cookie changed too.

## 2. The cookie: Pablo, only when it changed

The `d` cookie is HttpOnly, so no page script can read it, and the agent may
not copy a credential out of 1Password onto the clipboard. It is shared by
every workspace signed in to that Chrome profile, so the tab's workspace does
not matter.

Tell Pablo exactly: "In the same Slack tab, with DevTools open (⌥⌘J), click
the Application tab at the top of DevTools (under » if hidden). In the left
sidebar, under Storage → Cookies, click https://app.slack.com. In the row
named d, double-click the Value cell, then press ⌘A and ⌘C. Tell me 'done'."
Then store it for each workspace item that profile serves, with `--field
cookie --prefix xoxd- --token --force`. If a second store fails on the
prefix, the clipboard changed: ask him to copy it again.

## 3. Verify

`python3 claude/bin/slack.py -w WORKSPACE channels | head -3` must list
channels. A cached copy of the old value is refreshed automatically once after
a rejection. Close the tab you opened.
