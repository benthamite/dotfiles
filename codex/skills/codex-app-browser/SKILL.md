---
name: codex-app-browser
description: Use when the user explicitly invokes `$codex-app-browser`, asks to use the Codex App, in-app browser, in-built browser, Browser Use, or to open/navigate/click/test/screenshot something in the Codex app; also use when the user says they switched or resumed a Codex CLI/Emacs session in the Codex App to get browser access.
---

# Codex App browser handoff

This is a Codex-only operational skill. Do not mirror it into Claude Code: it exists to prevent false "browser unavailable" claims when Pablo moves a Codex CLI/Emacs session into the Codex App so the in-app browser is available.

Use this skill together with the bundled `browser-use:browser` skill. This skill explains the handoff and the failure modes; the bundled skill is the authoritative API reference.

## Root cause to remember

Browser Use is only exposed to the model in Codex App turns where the Browser plugin / Node REPL MCP is present. A Codex CLI or Emacs turn cannot gain that tool midway through the same turn just because the desktop app has been opened; the next turn must run from the Codex App thread.

Codex App supports local conversation deeplinks in the form `codex://threads/<THREAD_ID>`. In CLI/Emacs sessions, `CODEX_THREAD_ID` normally contains the current thread id. Use the `codex-app-handoff` command to open the exact current thread in the desktop app.

## When this applies

Explicit invocation:

- Pablo can say `$codex-app-browser` when he wants this handoff/preflight procedure used.
- Treat `$codex-app-browser <task>` as complete user intent. Do not require Pablo to mention CLI, Emacs, `CODEX_THREAD_ID`, handoff, Node REPL, or tool visibility.
- Treat `$codex-app-browser` as a direct instruction to handle the CLI/App handoff or run the Browser Use bootstrap before claiming browser access is unavailable.

Trigger on phrases such as:

- "use the in-app browser"
- "use the in-built browser"
- "open/navigate in the Codex app"
- "I am in the Codex app"
- "I switched to the app"
- "test/click/screenshot this in Codex"
- "Browser Use"

## Minimal user prompt

Pablo's prompt can be only:

```text
$codex-app-browser <the browser-dependent task>
```

The skill must provide the environment-specific context automatically:

- If this is a CLI/Emacs turn, run `codex-app-handoff`. It opens the thread and copies a continuation prompt. Tell Pablo to paste/send that prompt from the Codex App thread.
- If this is already a Codex App turn, run the Browser Use preflight immediately.
- If signed-in sites, browser extensions, or an existing browser profile are needed, choose Computer Use or a structured service connector rather than the unauthenticated in-app browser.

If the user starts in Codex CLI or Emacs and asks for browser work, do not say the task is impossible. Run:

```bash
codex-app-handoff
```

Then say, briefly, that the exact thread has been opened in the Codex App, that a continuation prompt was copied, and that the next browser-capable turn must be sent from there. Once Pablo sends a message from the app, immediately retry the Browser Use bootstrap.

Current limitation: the supported `codex://threads/<THREAD_ID>` deeplink opens an existing thread, but it does not submit a message into that thread. Do not promise fully automatic continuation unless a future Codex App/App Server API exposes that action.

If `CODEX_THREAD_ID` is unavailable, run `codex app "$PWD"` instead and explain that the App should be opened on the current workspace, but that the exact thread could not be deeplinked automatically.

## Required preflight

Before saying the in-app browser is unavailable from a Codex App turn, actually run the Browser Use bootstrap through the Node REPL MCP `js` tool.

Important naming trap: in Codex tool listings, the callable may appear as namespace `mcp__node_repl__` with function `js`, not as a single tool named `mcp__node_repl__js`. Do not infer that Node REPL is unavailable just because the joined name is absent.

Use this first cell:

```js
if (!globalThis.agent) {
  const { setupAtlasRuntime } = await import("/Users/pablostafforini/.codex/plugins/cache/openai-bundled/browser-use/0.1.0-alpha1/scripts/browser-client.mjs");
  await setupAtlasRuntime({ globals: globalThis, backend: "iab" });
}
await agent.browser.nameSession("🔎 browser task");
if (typeof tab === "undefined") {
  globalThis.tab = await agent.browser.tabs.new();
}
nodeRepl.write(JSON.stringify({ ok: true, title: await tab.title(), url: await tab.url() }));
```

If that succeeds, continue with normal Browser Use operations via `tab`.

## What not to do

Do not do any of these before the required preflight:

- Do not claim that CLI/Emacs cannot be moved to the Codex App; use `codex-app-handoff` first.
- Do not inspect or patch `/tmp/codex-browser-use` sockets.
- Do not use macOS screen capture, OCR, or AppleScript UI scraping as a substitute for Browser Use.
- Do not open external Chrome or use the Slack/API layer when the user asked for in-app browser operation.
- Do not conclude the browser is unavailable from skill text alone.
- Do not treat "current URL: not currently navigated" as an error; it just means the in-app browser tab is blank.

## If the preflight fails

Report the exact failure in plain language and give the next practical option. Keep it short. For example:

> I tried the Browser Use bootstrap from the Codex App session, but it failed with `<error>`. That means I cannot control the in-app browser from this turn. The clean fallback is `<fallback>`.

Only then choose a fallback, and only if it matches the user's constraints.
