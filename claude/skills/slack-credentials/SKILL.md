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

## 1. The token: the agent does it alone

1. Follow the `chrome-use` skill to open, in the Chrome profile signed in to
   that workspace (`baish` for becas-aisar and baish), a tab on
   `https://app.slack.com/client/TEAM_ID` (or any page of the workspace).
2. Run this page script with the browser tool, replacing `DOMAIN` with the
   workspace's subdomain (`becasaisar`, `baish`, …). It copies the token to the
   macOS clipboard and returns only its length and prefix:

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
3. Store it: `op-clipboard-store --vault Automation --account my.1password.com
   --title ITEM --field token --prefix xoxc- --token` (add `--force` when the
   item exists). Run the store immediately after the copy: anything that
   touches the clipboard in between makes it fail on the prefix check.

## 2. The cookie: Pablo, once, and only when it changed

The `d` cookie is HttpOnly, so no page script can read it, and the agent may
not copy a credential out of 1Password onto the clipboard. It is shared by
every workspace signed in to that Chrome profile, so the tab's workspace does
not matter, and it changes only when Pablo signs out of Slack there.

- If only the token was stale (the common case), skip this step.
- Otherwise ask Pablo, with DevTools open in that tab (Cmd+Option+J): **Application**
  → **Storage → Cookies → https://app.slack.com** → row **`d`** → double-click
  Value → Cmd+A → Cmd+C. Then store it for each workspace item at once, with
  `--field cookie --prefix xoxd- --token --force`; if a second store fails on
  the prefix, ask Pablo to copy it again (the clipboard changed).

## 3. Verify

`python3 claude/bin/slack.py -w WORKSPACE channels | head -3` must list
channels. A cached copy of the old value is refreshed automatically once after
a rejection.
