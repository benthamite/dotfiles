---
name: install-mcp-server
description: Install and configure a new MCP server for Claude Code. Handles discovery, auth verification, installation, and registration. Use when the user says "install mcp server", "add mcp server", "set up mcp", "new mcp server", or wants to connect a new service via MCP.
argument-hint: <server name, URL, or description of what service to connect>
---

# Install MCP server

Install a new MCP server into Claude Code, avoiding common pitfalls.

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

## Arguments

`$ARGUMENTS` contains the server name, GitHub URL, npm package, or a description of the service the user wants to connect (e.g. "Gmail", "Notion", "Jira"). If empty, ask what service they want to connect.

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

**CRITICAL**: Always use the `claude mcp add` CLI command. NEVER hand-edit `~/.claude.json` — Claude Code reads MCP server configs from an internal location that `claude mcp add` knows about, and hand-edits to `~/.claude.json` are silently ignored.

### Scope
- **`-s user`** (global): Available in all projects. Use this by default.
- **`-s local`** (project): Only available in the current project. Use for project-specific servers.

Ask the user which scope they want if not obvious.

### Registration command

```bash
# For npx servers:
claude mcp add -s user <name> -- npx -y <package>

# For npx servers with env vars:
claude mcp add -s user -e API_KEY=xxx <name> -- npx -y <package>

# For node servers (absolute path!):
claude mcp add -s user <name> -- /opt/homebrew/bin/node /path/to/server/dist/index.js

# For uvx servers:
claude mcp add -s user <name> -- uvx <package>

# For HTTP/SSE servers:
claude mcp add -s user --transport http <name> <url>
```

### Verify registration

```bash
claude mcp get <name>
```

This should show "Status: Connected". If it doesn't:
1. Check the error message.
2. Try running the command manually to see stderr output.
3. Common fixes: absolute paths, missing env vars, native module ABI mismatches.

## Step 6: Test

Tell the user to **restart Claude Code** (the server won't appear until restart).

After restart, test with a simple read-only operation:
- Gmail: search for recent emails
- Calendar: list today's events
- Drive: search for a file
- etc.

If the server requires OAuth, the first tool call will open a browser for authentication.

## Step 7: Store credentials

If the server needs an API key or token:
1. Store it in `~/.zshenv-secrets` (NEVER leave it only in the `claude mcp add` command or env).
2. Reference the env var in the server config.

## Step 8: Report

Tell the user:
1. Server name and scope (user/local)
2. What tools are now available
3. Any auth steps still needed (e.g. "first use will open a browser for OAuth")
4. How to remove it: `claude mcp remove <name> -s <scope>`

## Common pitfalls (for reference)

| Pitfall | Symptom | Fix |
|---|---|---|
| Bare `node` command | Server silently missing after restart | Use `/opt/homebrew/bin/node` |
| Hand-editing `~/.claude.json` | Server invisible to `claude mcp list` | Use `claude mcp add` CLI |
| Platform-locked OAuth | "This app is blocked" in browser | Server is unusable with Claude Code; find alternative |
| Native module ABI mismatch | Server crashes on startup | Rebuild with the same node version you're running |
| Forgot `-s user` | Server only works in one project | Re-add with `-s user` |
| Space in path | Server fails to start | Use symlink or quote properly |
