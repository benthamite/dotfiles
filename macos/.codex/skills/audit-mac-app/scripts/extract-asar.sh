#!/bin/bash

set -euo pipefail

if [ "$#" -ne 2 ]; then
    echo "Usage: extract-asar.sh ARCHIVE DESTINATION" >&2
    exit 2
fi

ARCHIVE=$1
DESTINATION=$2
SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
SKILL_DIR=$(cd -- "$SCRIPT_DIR/.." && pwd)

compatible_node() {
    local candidate=$1
    [ -x "$candidate" ] || return 1
    "$candidate" -e '
        const [major, minor] = process.versions.node.split(".").map(Number);
        process.exit(major > 22 || (major === 22 && minor >= 12) ? 0 : 1);
    ' >/dev/null 2>&1
}

select_node() {
    local candidate

    if [ -n "${AUDIT_MAC_APP_NODE:-}" ]; then
        if compatible_node "$AUDIT_MAC_APP_NODE"; then
            printf '%s\n' "$AUDIT_MAC_APP_NODE"
            return 0
        fi
        echo "AUDIT_MAC_APP_NODE must point to Node 22.12 or newer." >&2
        return 1
    fi

    candidate=$(command -v node 2>/dev/null || true)
    if [ -n "$candidate" ] && compatible_node "$candidate"; then
        printf '%s\n' "$candidate"
        return 0
    fi

    for candidate in /opt/homebrew/bin/node /usr/local/bin/node; do
        if compatible_node "$candidate"; then
            printf '%s\n' "$candidate"
            return 0
        fi
    done

    local nvm_root=${NVM_DIR:-"$HOME/.nvm"}
    if [ -d "$nvm_root/versions/node" ]; then
        while IFS= read -r candidate; do
            if compatible_node "$candidate"; then
                printf '%s\n' "$candidate"
                return 0
            fi
        done < <(find "$nvm_root/versions/node" -mindepth 3 -maxdepth 3 -type f -path '*/bin/node' -print | sort -r)
    fi

    echo "Electron extraction requires Node 22.12 or newer." >&2
    return 1
}

NODE_BIN=$(select_node)
NODE_DIR=$(dirname -- "$NODE_BIN")
NPM_BIN="$NODE_DIR/npm"
LOCK_HASH=$(/usr/bin/shasum -a 256 "$SKILL_DIR/package-lock.json" | awk '{print $1}')
CACHE_BASE=${AUDIT_MAC_APP_CACHE_DIR:-"${XDG_CACHE_HOME:-"$HOME/.cache"}/audit-mac-app/asar"}
INSTALL_DIR="$CACHE_BASE/$LOCK_HASH"
ASAR_MODULE="$INSTALL_DIR/node_modules/@electron/asar/bin/asar.mjs"

if [ ! -f "$ASAR_MODULE" ]; then
    if [ ! -x "$NPM_BIN" ]; then
        echo "The selected Node runtime has no sibling npm executable: $NODE_BIN" >&2
        exit 1
    fi

    umask 077
    mkdir -p "$CACHE_BASE"
    STAGING_DIR=$(mktemp -d "$CACHE_BASE/.install.XXXXXX")
    cleanup_staging() {
        [ -n "${STAGING_DIR:-}" ] && [ -d "$STAGING_DIR" ] || return
        if command -v trash >/dev/null 2>&1; then
            trash "$STAGING_DIR" >/dev/null
        else
            find "$STAGING_DIR" -depth -delete
        fi
    }
    trap cleanup_staging EXIT

    cp "$SKILL_DIR/package.json" "$SKILL_DIR/package-lock.json" "$STAGING_DIR/"
    echo "Installing the locked Electron ASAR extractor..." >&2
    PATH="$NODE_DIR:$PATH" "$NPM_BIN" ci \
        --prefix "$STAGING_DIR" \
        --ignore-scripts \
        --no-audit \
        --no-fund

    if [ -e "$INSTALL_DIR" ]; then
        echo "The extractor cache is incomplete: $INSTALL_DIR" >&2
        exit 1
    fi
    mv "$STAGING_DIR" "$INSTALL_DIR"
    STAGING_DIR=""
    trap - EXIT
fi

PATH="$NODE_DIR:$PATH" "$NODE_BIN" "$ASAR_MODULE" extract "$ARCHIVE" "$DESTINATION"
