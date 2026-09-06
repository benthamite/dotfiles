#!/bin/bash
# Static inventory only; JSON output is not a safety verdict.
set -euo pipefail
SCRIPT_DIR=$(cd -P -- "$(dirname -- "$0")" && pwd)
exec /usr/bin/python3 -I -B "$SCRIPT_DIR/audit_mac_app.py" "$@"
