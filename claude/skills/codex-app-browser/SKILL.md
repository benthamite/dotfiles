---
name: codex-app-browser
description: Use when the user asks Claude about the Codex App in-app browser handoff skill or why `$codex-app-browser` works only in Codex.
---

# Codex App browser handoff

This is a Claude-side pointer for a Codex-only operational skill. Claude Code cannot use the Codex App in-app browser backend directly.

The source-of-truth workflow is:

```text
codex/skills/codex-app-browser/SKILL.md
```

When Pablo asks about this from Claude, explain that `$codex-app-browser` must be invoked from Codex. If the issue is skill behavior, inspect and update the Codex skill, then keep this pointer in sync if its routing or failure-mode summary changes.

Current Codex behavior to preserve:

- Use the bundled Browser skill as the authoritative API reference.
- Bootstrap with `setupBrowserRuntime`, not the removed `setupAtlasRuntime`.
- Check `agent.browsers.list()` for either string entries or BrowserInfo objects with `id` or `type` equal to `iab` before claiming that `iab` is unavailable.
- If the list is empty or `iab` is missing, treat it as a Codex App/backend attachment failure and run `codex-app-handoff`.
- Do not offer a generic fallback unless Pablo explicitly permits a non-in-app-browser path.
