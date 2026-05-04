# MCP servers

Read this file when adding, removing, debugging, or choosing placement for Claude Code MCP servers.

## Placement

Claude Code can load MCP servers through five mechanisms:

| Mechanism | Source of truth | Loaded when | Edit how |
|-----------|-----------------|-------------|----------|
| User-level | `~/.claude.json` top-level `mcpServers` | Every session, every directory | Edit `~/.claude.json`, then run `claude/bin/sync-mcp-servers.sh` to propagate to `~/.claude-epoch/`, `~/.claude-personal/`, and `~/.claude-tlon/` |
| Project-local | `<project>/.mcp.json` | Only when CWD is that project | Edit the file directly; no sync needed |
| claude.ai connectors | Server-side, no local file | Every session, regardless of CWD | Manage in claude.ai Settings > Connectors |
| Plugin-managed | `~/.claude/plugins/...` | When the plugin is enabled | Use `/plugin enable/disable <name>` |
| `claude-in-chrome` | Chrome extension plus native messaging host | When Claude Code is started with `--chrome` | Use `/chrome` or `claudeInChromeDefaultEnabled` in `~/.claude.json` |

Do not use `~/.claude.json`'s `projects.<path>.mcpServers` block. Claude Code supports it, but it duplicates `<project>/.mcp.json` in a hidden location. Always put project-scoped MCPs in `<project>/.mcp.json`.

When source code for an MCP server needs to be cloned locally, use `~/My Drive/dotfiles/claude/mcp-servers/<name>/`. The parent directory may otherwise be empty.

## Multi-account notes

Claude Code reads `.claude.json` from `$CLAUDE_CONFIG_DIR`, not `$HOME`, when that variable is set. The canonical user-level source of truth is `~/.claude.json`; run `claude/bin/sync-mcp-servers.sh` after adding or removing any user-level MCP server there.

When an MCP server needs different credentials per account, place `op://` references or plain text values directly in each per-account `.claude.json` file's MCP server `env` block. Claude Code resolves `op://` natively. The sync function deep-merges `mcpServers` per server, preserving per-account `env` entries. To add a new account-specific secret:

1. Add the `env` entry to each per-account `.claude.json`, such as `~/.claude-tlon/.claude.json` or `~/.claude-epoch/.claude.json`.
2. Leave the canonical `~/.claude.json` with an empty `env` for that server.

Chrome integration uses three Chrome profiles, one per Claude Code account. Browser automation from a given Claude Code session targets the Chrome profile its account is paired to. Full Chrome integration details live in `README.org` under "Chrome integration and multi-account".

## Current inventory

- User-level: empty.
- Project-local: `~/My Drive/Epoch/.mcp.json` holds `airtable`, `asana`, and `gmail-epoch-triage`; `~/source/ForumMagnum/.mcp.json` holds `playwright`.
- claude.ai connectors: GitHub Integration, Ahrefs, Gmail, Google Calendar, Google Drive, Slack, GitHub via Copilot MCP, and Asana.
- Plugin-managed: none currently enabled.
- `claude-in-chrome`: enabled and heavily used for browser automation.

General service access tools are listed in `CLAUDE.md`.
