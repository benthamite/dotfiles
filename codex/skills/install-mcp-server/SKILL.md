---
name: install-mcp-server
description: Install and configure a new MCP server for Claude Code. Handles discovery, auth verification, credential-safe setup, scope placement, registration, account sync, and restart/testing expectations. Use when the user says "install mcp server", "add mcp server", "set up mcp", "new mcp server", or wants to connect a new service via MCP.
---

# Install MCP server

Install a new MCP server into Claude Code, avoiding common pitfalls.

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

## Arguments

`$ARGUMENTS` contains the server name, GitHub URL, npm package, or a description of the service the user wants to connect (e.g. "Gmail", "Notion", "Jira"). If empty, ask what service they want to connect.

## Local placement authority

Before choosing scope, use `/Users/pablostafforini/My Drive/dotfiles/claude/context/mcp-servers.md` as the source of truth for this setup:

- User-level MCPs belong in `~/.claude.json` top-level `mcpServers`; after any user-level add/remove, run `/Users/pablostafforini/My Drive/dotfiles/claude/bin/sync-mcp-servers.sh`.
- Shared project MCPs belong in the project's `.mcp.json`.
- Do not use `~/.claude.json`'s `projects.<path>.mcpServers` block for project-scoped MCPs.

## Step 1: Discovery

Find the right MCP server for the requested service.

1. **If the user provided a specific package or URL**, use that directly.
2. **If the user described a service**, search for MCP servers that support it:
   - Check https://github.com/modelcontextprotocol/servers for official/community servers
   - Search npm (`npx` servers) and PyPI (`uvx` servers)
   - Search GitHub for `<service> MCP server`
3. **Check for duplicates**: Run `claude mcp list` to see what's already installed. If a server for this service already exists, tell the user and ask if they want a replacement or addition.

Present the candidate(s) to the user with:
- Name and URL
- What tools/services it provides
- How auth works (OAuth, API key, token, none)
- Whether it's official (from the service provider) or community-built

**Get user confirmation before proceeding.**

## Step 2: Auth verification (CRITICAL — do this BEFORE installing)

This is the most important step. Many MCP servers have auth mechanisms that are locked to specific platforms or require non-trivial setup. Investigate BEFORE spending time on installation.

Check the server's README, source code, or docs for:

1. **OAuth client ID**: Is it built-in or user-provided?
   - If built-in: check whether it's restricted to a specific platform (e.g. Gemini CLI, Cursor, VS Code). Look for domain restrictions, app verification status, or platform-specific cloud functions in the auth flow. **If the OAuth client is platform-locked, STOP and tell the user this server won't work with Claude Code.**
   - If user-provided: the user will need to create their own OAuth app in the service's developer console. Document what's needed.
2. **API key / token**: Straightforward — just needs the key. Check where to get one.
3. **No auth**: Proceed directly.
4. **Credential shape**: Identify the exact env vars, headers, config files, or OAuth flags the server needs. Decide how each secret will be supplied before registration. Do not put raw secret values in shell commands, command output, tracked files, or chat.

**If auth requires creating OAuth credentials, GCP projects, or other setup**: explain exactly what's needed and get confirmation before proceeding. Don't install first and discover auth problems later.

## Step 3: Install dependencies

Based on the server type:

### npm/npx server
- These typically need no local installation — `npx -y <package>` runs them directly.
- Test that the package exists: `npx -y <package> --help` or similar.

### Python/uvx server
- These typically need no local installation — `uvx <package>` runs them directly.
- Test that the package exists: `uvx <package> --help` or similar.

### Git repo (needs cloning and building)
- Clone to `~/My Drive/dotfiles/claude/mcp-servers/<name>/` (or ask the user where they want it).
- Run `npm install && npm run build` (or equivalent).
- Run `/nosync` on any `node_modules` directory inside the clone to avoid Google Drive sync issues.
- **Important**: If the entry point is a built/bundled JS file, verify it exists after the build.

## Step 4: Determine the command

**CRITICAL**: Do NOT use bare interpreter names like `node`, `python`, or `python3` as the command. Claude Code spawns MCP servers as child processes without a shell, so shell functions (e.g. nvm's lazy-loading `node` wrapper) and PATH-dependent lookups may fail silently.

Use these rules:
- **npx/uvx servers**: `npx` and `uvx` are usually real binaries. Verify with `command -v npx` / `command -v uvx`. If they resolve to shell functions, use absolute paths.
- **Node.js servers**: Use the absolute path to the node binary. Find it with:
  ```bash
  command -v node  # Check if it's a function or binary
  which -a node    # Find all candidates
  ```
  On this system, use `/opt/homebrew/bin/node` (Homebrew's system node), NOT `node` (which is an nvm shell function).
- **Python servers**: Use the absolute path or `uvx` (which is a real binary).

## Step 5: Register with Claude Code

### Scope
- **`-s user`** (global): Available in all projects. Use this by default for generally useful servers. The canonical source is `~/.claude.json`; after adding or removing a user-level server there, run `/Users/pablostafforini/My Drive/dotfiles/claude/bin/sync-mcp-servers.sh` so the personal, tlon, and epoch account configs receive the change.
- **`-s project`** (shared project): Writes the current project's `.mcp.json`. Use this for project-specific servers that should be available to agents working in that repo. No account sync is needed.
- **`-s local`** (private local): Claude CLI's default, but it stores project-local config in a hidden per-path block. Do not use it for normal project-scoped servers in this dotfiles setup. Use it only if the user explicitly asks for a private, unshared local exception, and document that exception.

Ask the user which scope they want if not obvious.

Before running a user-scope command, check whether `CLAUDE_CONFIG_DIR` is set. If it is, do not assume `claude mcp add -s user` will update canonical `~/.claude.json`; run the command with the canonical config environment or edit `~/.claude.json` deliberately, then sync.

### Credentials before registration

If the server needs an API key, token, OAuth client secret, or header:

1. Create or locate the credential before `claude mcp add`.
2. Prefer an `op://` reference in MCP config when Claude Code can resolve it. If the server reads a process env var inherited by Claude Code instead, store/export it through `~/.zshenv-secrets` and restart Claude Code so it inherits the variable.
3. Never pass a raw secret value in the `claude mcp add -e KEY=value` command, because it can end up in shell history, logs, or config. Use a non-secret reference such as `op://...` or edit the config without printing the value.
4. For account-specific credentials, keep canonical `~/.claude.json` free of raw secret values, put the per-account values or references in the corresponding account configs, and verify they are still present after any sync. Do not print the values while checking.

### Registration command

```bash
# For npx servers:
claude mcp add -s user <name> -- npx -y <package>

# For npx servers with a secret reference, not a raw secret:
claude mcp add -s user -e API_KEY='op://Vault/Item/field' <name> -- npx -y <package>

# For node servers (absolute path!):
claude mcp add -s user <name> -- /opt/homebrew/bin/node /path/to/server/dist/index.js

# For uvx servers:
claude mcp add -s user <name> -- uvx <package>

# For HTTP/SSE servers:
claude mcp add -s user --transport http <name> <url>

# For project-scoped servers:
claude mcp add -s project <name> -- npx -y <package>
```

### HTTP/SSE URL formatting (CRITICAL)

**Always include a trailing slash on HTTP MCP server URLs.** Claude Code's HTTP transport handler may silently drop servers whose URL lacks a trailing slash — the server entry is written to config but never appears in `claude mcp list` and is completely invisible at runtime.

- WRONG: `https://mcp.example.com`
- RIGHT: `https://mcp.example.com/`

When extracting a URL from documentation, always verify it ends with `/`. If it doesn't, add one.

### Sync and verify registration (CRITICAL — use `claude mcp list`, NOT `claude mcp get`)

After adding, run:

```bash
/Users/pablostafforini/My\ Drive/dotfiles/claude/bin/sync-mcp-servers.sh  # only for -s user changes to canonical ~/.claude.json
claude mcp list 2>&1 | grep <name>
```

The server **MUST** appear in `claude mcp list` output. This command checks actual runtime health. It may spawn stdio servers from `.mcp.json`, so run it only from a directory you trust.

**Do NOT rely on `claude mcp get <name>`** — it reads from the config file and will report a server as present even when Claude Code cannot actually load it. A server that appears in `claude mcp get` but not `claude mcp list` is broken.

If the server doesn't appear in `claude mcp list`:
1. Check the URL has a trailing slash (for HTTP servers).
2. Test the endpoint with `curl -s -o /dev/null -w "%{http_code}" <url>` — a 401 is OK (means endpoint exists but needs auth), a connection error means the URL is wrong.
3. Remove and re-add with corrected config.

Expected statuses in `claude mcp list`:
- `✓ Connected` — working.
- `! Needs authentication` — server loaded, OAuth flow will trigger on first tool use. This is fine for OAuth-based servers.
- Not listed at all — server is broken. Fix the config.

## Step 6: Restart and test

`claude mcp list` verifies the saved configuration and server health from the shell. It does not hot-load new MCP tools into an already running Claude Code session.

If the current Claude Code session is already running, tell the user to restart Claude Code or start a fresh session before expecting the new MCP tools to appear. After restart, test with a simple read-only operation:
- Gmail: search for recent emails
- Calendar: list today's events
- Drive: search for a file
- etc.

If the server requires OAuth, the first tool call will open a browser for authentication.

## Step 7: Final credential check

Before reporting success, confirm credential handling is complete:

1. Check the relevant config files without printing secret values.
2. Confirm raw secrets are not present in the shell command you will report, tracked files, or command output.
3. If the credential is process-env based, confirm the restart requirement is explicit.

## Step 8: Report

Tell the user:
1. Server name and scope (`user`, `project`, or explicitly requested `local`)
2. What tools are now available
3. Config file touched (`~/.claude.json` or `.mcp.json`) and whether `/Users/pablostafforini/My Drive/dotfiles/claude/bin/sync-mcp-servers.sh` was run
4. Any restart or auth steps still needed (e.g. "restart Claude Code; first use will open a browser for OAuth")
5. How to remove it: `claude mcp remove <name> -s <scope>`

## Common pitfalls (for reference)

| Pitfall | Symptom | Fix |
|---|---|---|
| Missing trailing slash on HTTP URL | Server in config but invisible in `claude mcp list` | Add `/` to end of URL |
| Verifying with `claude mcp get` instead of `claude mcp list` | Looks OK but server doesn't actually work | Always use `claude mcp list` to verify |
| Bare `node` command | Server silently missing after restart | Use `/opt/homebrew/bin/node` |
| Platform-locked OAuth | "This app is blocked" in browser | Server is unusable with Claude Code; find alternative |
| Native module ABI mismatch | Server crashes on startup | Rebuild with the same node version you're running |
| Forgot `-s user` | Server only works in one project | Re-add with `-s user`, then run `/Users/pablostafforini/My Drive/dotfiles/claude/bin/sync-mcp-servers.sh` |
| Used default `-s local` for a project server | Server is hidden in per-path config instead of `.mcp.json` | Re-add with `-s project` |
| User-level change not synced | Works in one Claude account but not others | Run `/Users/pablostafforini/My Drive/dotfiles/claude/bin/sync-mcp-servers.sh` and restart affected sessions |
| Raw secret in `claude mcp add -e` | Secret may leak through history/logs/config | Use `op://` or another non-secret reference; clean up leaked copies if one was used |
| Space in path | Server fails to start | Use symlink or quote properly |
