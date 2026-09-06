#!/bin/bash
set -euo pipefail
SCRIPT_DIR=$(cd -P -- "$(dirname -- "$0")" && pwd)
exec /opt/homebrew/bin/python3 -I "$SCRIPT_DIR/extract-asar.py" "$@"
