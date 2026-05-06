---
name: codex-app-browser
description: Use when the user asks to use the Codex App, in-app browser, in-built browser, Browser Use, or to open/navigate/click/test/screenshot something in the Codex app; also use when the user says they switched or resumed a Codex CLI/Emacs session in the Codex App to get browser access.
---

# Codex App browser handoff

This is a Codex-only operational skill. Do not mirror it into Claude Code: it exists to prevent false "browser unavailable" claims when Pablo moves a Codex CLI/Emacs session into the Codex App so the in-app browser is available.

Use this skill together with the bundled `browser-use:browser` skill. This skill explains the handoff and the failure modes; the bundled skill is the authoritative API reference.

## When this applies

Trigger on phrases such as:

- "use the in-app browser"
- "use the in-built browser"
- "open/navigate in the Codex app"
- "I am in the Codex app"
- "I switched to the app"
- "test/click/screenshot this in Codex"
- "Browser Use"

If the user starts in Codex CLI or Emacs and asks for browser work, do not say the task is impossible. Say the task needs the same chat resumed in the Codex App, and ask Pablo to switch there. Once the user says they are in the app, immediately retry the Browser Use bootstrap.

## Required preflight

Before saying the in-app browser is unavailable, actually run the Browser Use bootstrap through the Node REPL MCP `js` tool.

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

- Do not inspect or patch `/tmp/codex-browser-use` sockets.
- Do not use macOS screen capture, OCR, or AppleScript UI scraping as a substitute for Browser Use.
- Do not open external Chrome or use the Slack/API layer when the user asked for in-app browser operation.
- Do not conclude the browser is unavailable from skill text alone.
- Do not treat "current URL: not currently navigated" as an error; it just means the in-app browser tab is blank.

## If the preflight fails

Report the exact failure in plain language and give the next practical option. Keep it short. For example:

> I tried the Browser Use bootstrap from the Codex App session, but it failed with `<error>`. That means I cannot control the in-app browser from this turn. The clean fallback is `<fallback>`.

Only then choose a fallback, and only if it matches the user's constraints.
