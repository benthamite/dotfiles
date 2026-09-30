# slack.py multi-workspace support — design

Date: 2026-06-23

## Problem

`claude/bin/slack.py` is hardcoded to a single workspace. `TOKEN_OP_PATH`
points at `op://Automations/Slack MCP - Epoch Unofficial`, so every call reads
the **Epoch** workspace via 1Password-stored xoxc/xoxd session tokens. There is
no way to read the **Trajectory Labs** Slack workspace.

Goal: let `slack.py` target Trajectory Labs (and remain extensible to other
workspaces) without breaking any existing Epoch caller.

## Token storage

Trajectory tokens are stored in `pass`, following the existing org-prefixed
session-token model already used by:

- `epoch/slack.com/epochai` — fields `token:` (xoxc) and `cookie:` (xoxd)
- `tlon/core/slack.com/tlon` — same field shape

New entry: **`trajectory/slack.com/trajectorylabs`**, with the same two named
fields:

```
<line 1: optional login password or blank>
token: xoxc-...
cookie: xoxd-...
```

Note: the pre-existing `chrome/slack.com/trajectorylabs` entry holds only a
Chrome-saved **login password** (1 line, no token/cookie). It is the wrong
model and is left untouched. `chrome/...` means "browser password dump" in this
store; session tokens live under org-prefixed paths.

These session tokens do not exist yet — they must be scraped from a logged-in
Trajectory Labs Slack browser tab (xoxc Bearer token + xoxd `d` cookie) and
saved via `pass insert -m trajectory/slack.com/trajectorylabs`.

## Design

### Workspace registry

A module-level dict maps a workspace key to a token source. Two source kinds:

```python
WORKSPACES = {
    "epoch": {
        "source": "op",
        "op_path": "op://Automations/Slack MCP - Epoch Unofficial",
    },
    "trajectory": {
        "source": "pass",
        "pass_entry": "trajectory/slack.com/trajectorylabs",
        "token_field": "token",   # -> xoxc Bearer token
        "cookie_field": "cookie", # -> xoxd 'd' cookie
    },
}
DEFAULT_WORKSPACE = "epoch"
```

### Workspace selection

Global optional on the top-level argparse parser (before the subcommand):

```
slack.py [-w WORKSPACE | --workspace WORKSPACE] <subcommand> ...
```

Resolution order: `--workspace` flag → `SLACK_WORKSPACE` env var → `epoch`.
Implemented as `default=os.environ.get("SLACK_WORKSPACE", DEFAULT_WORKSPACE)`.
An unknown workspace key exits non-zero with a clear error listing valid keys.

### Token resolution

- New `_pass_field(entry, field)` helper: runs `pass show <entry>`, finds the
  line beginning `"<field>:"`, returns the trimmed value. Exits non-zero with a
  clear message if the entry or field is missing. Never prints the value.
- `_tokens()` becomes workspace-aware: looks up the selected workspace in
  `WORKSPACES`; for `source == "op"` reads `<op_path>/xoxc_token` and
  `<op_path>/xoxd_token` (today's behavior exactly); for `source == "pass"`
  reads the two `pass` fields. Caching of `_xoxc`/`_xoxd` is preserved.
- The selected workspace is stored in a module-level variable set in `main()`
  from `args.workspace`, consumed by `_tokens()`.

`call()` is unchanged — it already routes through `_tokens()`.

## Backward compatibility

No flag and no `SLACK_WORKSPACE` → `epoch` via the existing `op://` path.
Every existing caller, skill, and command keeps working unchanged. The Epoch
branch reads the same 1Password item with the same field names as today.

## Verification

1. `slack.py channels` (no flag) still lists Epoch channels — no regression.
2. After storing Trajectory tokens: `slack.py -w trajectory channels` lists
   Trajectory Labs channels.
3. `slack.py -w trajectory history <channel-id> --limit 5` returns recent
   messages from a Trajectory channel.
4. `SLACK_WORKSPACE=trajectory slack.py channels` resolves via the env var.
5. Unknown workspace (`slack.py -w bogus channels`) exits non-zero with a
   message listing valid keys.

## Out of scope

- Adding more workspaces beyond Epoch and Trajectory (the registry makes this a
  one-entry change later).
- Migrating Epoch from `op://` to `pass` (left as-is).
- Any write operations beyond what `slack.py` already supports.
