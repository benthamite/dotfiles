#!/bin/bash
# Run an emacs --batch test for an Elisp package.
#
# Usage: batch-test.sh <package> [elisp-expressions...]
#
# Handles two package layouts:
#
#   1. Dotfiles extras (canonical source in ~/My Drive/dotfiles/emacs/extras/,
#      separate elpaca clone). Removes the canonical .elc, adds all elpaca
#      builds to load-path, then prepends emacs/extras so the edited source
#      wins over the stale .elc in the elpaca clone.
#
#   2. Standalone elpaca/sources/<pkg> packages (canonical source IS the
#      elpaca clone). Adds all elpaca builds to load-path; the auto-rebuild
#      hook keeps elpaca/builds/<pkg>/ in sync with the source.
#
# Examples:
#   batch-test.sh agent-log
#   batch-test.sh sgn '(message "result: %S" (sgn-some-fn))'

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

# Detect package layout: extras (canonical .el in dotfiles/emacs/extras)
# vs standalone (canonical .el directly under elpaca/sources/<pkg>/).
IS_EXTRAS=false
if [ -f "$EXTRAS/$PACKAGE.el" ]; then
  IS_EXTRAS=true
fi

ARGS=(--batch)

# All elpaca builds on load-path (both layouts need this for dependencies).
ARGS+=(--eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))")

if [ "$IS_EXTRAS" = true ]; then
  # Remove co-located .elc so the edited .el wins.
  rm -f "$EXTRAS/$PACKAGE.elc"
  # Push extras to FRONT so canonical source beats stale elpaca-build .elc.
  ARGS+=(--eval "(push \"$EXTRAS\" load-path)")
fi

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
