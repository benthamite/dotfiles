#!/usr/bin/env bash
# sync-mcp-servers.sh — Sync top-level mcpServers from ~/.claude.json to account config dirs.
#
# Claude Code reads .claude.json from $CLAUDE_CONFIG_DIR when set, so user-level
# MCP servers must be synced to each account directory. Run this after adding or
# removing MCP servers in ~/.claude.json (the canonical source of truth).
#
# Merge semantics (documented in claude/context/mcp-servers.md): the canonical
# file owns the server set and each server's configuration, but per-account
# `env` entries are preserved — the canonical copy keeps an empty `env` for
# account-specific secrets, and each account's values win on key conflicts.
set -euo pipefail

# The account list lives in emacs/agent-accounts.json, read through
# lib/python/agent_accounts.py; this script sits two levels below the root.
DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
export SYNC_MCP_DOTFILES="$DOTFILES"

exec python3 - "$@" <<'PYTHON'
import importlib.util
import json
import os
import sys
import tempfile

sys.dont_write_bytecode = True  # The checkout is under Drive, where caches do not belong.

CANONICAL = os.path.expanduser("~/.claude.json")
# Every Claude account declared in the shared account list.
_library = os.path.join(os.environ["SYNC_MCP_DOTFILES"], "lib/python/agent_accounts.py")
_spec = importlib.util.spec_from_file_location("agent_accounts", _library)
_accounts = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_accounts)
TARGETS = [
    os.path.join(os.path.expanduser(entry["home"]), ".claude.json")
    for entry in _accounts.load_accounts()["claude"]
]

if not os.path.isfile(CANONICAL):
    print(f"Error: canonical config {CANONICAL} not found", file=sys.stderr)
    sys.exit(1)

with open(CANONICAL) as f:
    canonical = json.load(f)

mcp_servers = canonical.get("mcpServers")
if mcp_servers is None:
    print("Error: no mcpServers key in canonical config", file=sys.stderr)
    sys.exit(1)

print(f"Source: {CANONICAL} ({len(mcp_servers)} servers)")


def merge_servers(canonical_servers, account_servers):
    """Per-server deep merge: canonical config wins except account env entries."""
    merged = {}
    for name, config in canonical_servers.items():
        server = json.loads(json.dumps(config))  # deep copy
        account_env = (account_servers.get(name) or {}).get("env") or {}
        if account_env:
            env = dict(server.get("env") or {})
            env.update(account_env)
            server["env"] = env
        merged[name] = server
    return merged


for target_path in TARGETS:
    target_dir = os.path.dirname(target_path)
    if not os.path.isdir(target_dir):
        print(f"  Skipping {target_path} (directory does not exist)")
        continue

    if os.path.isfile(target_path):
        with open(target_path) as f:
            data = json.load(f)
    else:
        data = {}

    merged = merge_servers(mcp_servers, data.get("mcpServers") or {})

    if data.get("mcpServers") == merged:
        print(f"  Already up to date: {target_path}")
        continue

    data["mcpServers"] = merged

    # Atomic write via temp file
    fd, tmp = tempfile.mkstemp(dir=target_dir, suffix=".json")
    try:
        with os.fdopen(fd, "w") as f:
            json.dump(data, f, indent=2)
            f.write("\n")
        os.rename(tmp, target_path)
    except BaseException:
        os.unlink(tmp)
        raise

    print(f"  Synced to {target_path}")

print("Done.")
PYTHON
