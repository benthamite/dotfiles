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

When an explicitly requested MCP repository needs to be cloned locally, use
`~/repos/mcp-servers/<name>/`; do not create repositories or dependency trees
under Google Drive. The canonical dotfiles tree is the existing exception.

## Multi-account notes

Claude Code reads `.claude.json` from `$CLAUDE_CONFIG_DIR`, not `$HOME`, when that variable is set. The canonical user-level source of truth is `~/.claude.json`; run `claude/bin/sync-mcp-servers.sh` after adding or removing any user-level MCP server there.

Claude Code expands `${VAR}` references but does not resolve `op://` secrets
natively. Use an explicit resolver or an approved parent-process environment
injection path. For Epoch Automations credentials, the stdio server can run
through `bin/op-automations run --env-file FILE -- SERVER ...`; follow
`secrets.md` for the owning account and never route personal/Tlön secrets
through the Epoch service account. HTTP header expansion needs the resolved
value in Claude's parent environment.

For account-specific credentials, keep references in each account's MCP `env`
block only when the configured launcher will resolve them. Do not place raw
values in tracked configuration. The sync function deep-merges `mcpServers`
per server, preserving per-account `env` entries. To add an account-specific
reference:

1. Add the reference to the appropriate per-account `.claude.json`, such as
   `~/.claude-tlon/.claude.json` or `~/.claude-epoch/.claude.json`, and verify
   that account's launcher can resolve it without displaying the value.
2. Leave the canonical `~/.claude.json` with an empty `env` for that server.

Browser automation from a Claude Code session targets the Chrome profile its
account is paired to. Current account/profile details live in `README.org`
under "Chrome integration and multi-account".

## Current inventory

- User-level: empty.
- Project-local: `~/My Drive/Epoch/.mcp.json` holds `asana`; `~/source/ForumMagnum/.mcp.json` holds `playwright`. These are exceptions, not the default service-access path. The Epoch Asana MCP is the canonical Asana interface for reads and writes, and its token must resolve to `pablo@epoch.ai`; verify with `~/My Drive/Epoch/scripts/check-asana-mcp-identity.sh` after any Asana token change, then restart the agent session so the running MCP server picks up the new environment. Airtable is no longer loaded as a broad Epoch MCP server; use project scripts, the Airtable REST API, or hosted connectors for explicit Airtable tasks. The email-triage bot Gmail account is accessed through `claude/bin/gmail.py --account email-triage`, not an MCP server. The global shared-systems rule still requires explicit user confirmation before creating or updating tasks.
- claude.ai connectors: disabled globally in Claude Code with
  `disableClaudeAiConnectors: true` in the shared user settings. Hosted
  connectors may remain connected for claude.ai surfaces, but Claude Code does
  not load them or their tool schemas. They are not canonical for shared
  Claude/Codex local workflows.
- Plugin-managed: none currently enabled.
- `claude-in-chrome`: enabled and heavily used for browser automation.

General service access tools are listed in `CLAUDE.md`.
