#!/bin/bash
# Run an emacs --batch test with the correct elpaca load-path.
#
# Usage: batch-test.sh <package> [elisp-expressions...]
#
# Resolves the active elpaca profile, adds all build directories to
# load-path, puts emacs/extras at the front so edited sources take
# precedence over stale .elc in elpaca/builds, and runs emacs --batch.
#
# Examples:
#   batch-test.sh claude-log
#   batch-test.sh gptel-extras '(message "%s" (gptel-extras-some-fn))'

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: batch-test.sh <package> [elisp-expressions...]" >&2
  exit 1
fi

PACKAGE="$1"
shift

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
PROFILE=$(emacsclient -e 'init-current-profile' 2>/dev/null | tr -d '"')
ELPACA="$HOME/.config/emacs-profiles/$PROFILE/elpaca"
EXTRAS="$REPO_ROOT/emacs/extras"

# Remove stale .elc so the edited .el source is picked up
rm -f "$EXTRAS/$PACKAGE.elc"

ARGS=(--batch)

# All elpaca builds on load-path
ARGS+=(--eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))")

# Extras at the FRONT so edited sources win
ARGS+=(--eval "(push \"$EXTRAS\" load-path)")

# Load the package
ARGS+=(--eval "(require '$PACKAGE)")

# User expressions or default success message
if [ $# -eq 0 ]; then
  ARGS+=(--eval "(message \"$PACKAGE loaded successfully\")")
else
  for expr in "$@"; do
    ARGS+=(--eval "$expr")
  done
fi

exec emacs "${ARGS[@]}" 2>&1
