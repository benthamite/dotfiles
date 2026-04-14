# Secrets setup

- When reading secrets from `pass` or `.zshenv-secrets`, never echo or print them in the terminal or logs.
- When using `pass`, always use full paths (e.g. `env/home-assistant-token`, not `home-assistant-token`). Never grep `pass ls` output — it strips directory context. Use `pass find <name>` to search.
- **Personal secrets** (non-Epoch): stored in `pass` (GPG-encrypted). The **Epoch** secrets setup is described in `../../Epoch/CLAUDE.md`.
- **Account-specific MCP secrets** (e.g. different API keys per Claude Code account): place `op://` references (or plain text values) directly in each per-account `.claude.json` file's MCP server `env` block. Claude Code resolves `op://` references natively. The sync deep-merges per-server, preserving per-account `env` entries.
