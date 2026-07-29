#!/usr/bin/env bash
# sync-mcp-servers.sh — Merge top-level mcpServers into account config dirs.
#
# Claude Code reads .claude.json from $CLAUDE_CONFIG_DIR when set, so user-level
# MCP server additions and updates must be merged into each account directory.
# Account-only servers and fields survive, and account env values win conflicts.
# Canonical deletions require explicit cleanup in each account config.
set -euo pipefail

exec python3 - "$@" <<'PYTHON'
import json
import os
import sys
import tempfile

CANONICAL = os.path.expanduser("~/.claude.json")
# Active account dirs only. ~/.claude-work is obsolete and intentionally
# excluded; do not re-add unless that account is reactivated.
TARGETS = [
    os.path.expanduser("~/.claude-epoch/.claude.json"),
    os.path.expanduser("~/.claude-personal/.claude.json"),
    os.path.expanduser("~/.claude-tlon/.claude.json"),
    os.path.expanduser("~/.claude-trajectory/.claude.json"),
]


def deep_merge(base, updates):
    merged = dict(base)
    for key, update_value in updates.items():
        base_value = merged.get(key)
        if isinstance(base_value, dict) and isinstance(update_value, dict):
            merged[key] = deep_merge(base_value, update_value)
        else:
            merged[key] = update_value
    return merged


def merge_mcp_servers(account_servers, canonical_servers):
    merged = dict(account_servers)
    for name, canonical_server in canonical_servers.items():
        account_server = merged.get(name)
        if not isinstance(account_server, dict) or not isinstance(canonical_server, dict):
            merged[name] = canonical_server
            continue

        merged_server = deep_merge(account_server, canonical_server)
        account_env = account_server.get("env")
        canonical_env = canonical_server.get("env")
        if isinstance(account_env, dict) and isinstance(canonical_env, dict):
            merged_server["env"] = deep_merge(canonical_env, account_env)
        merged[name] = merged_server
    return merged


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

    account_servers = data.get("mcpServers", {})
    merged_servers = merge_mcp_servers(account_servers, mcp_servers)
    if account_servers == merged_servers:
        print(f"  Already up to date: {target_path}")
        continue

    data["mcpServers"] = merged_servers

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
