#!/usr/bin/env bash
# Run a script from an elpaca package, resolving the active profile dynamically.
# Usage: elpaca-run.sh <package> <relative-path> [args...]
# Searches repos/ then sources/ to handle different elpaca layouts.

PROFILE=$(timeout 5 emacsclient -e 'init-current-profile' 2>/dev/null | tr -d '"')
[ -z "$PROFILE" ] && exit 0

ELPACA="$HOME/.config/emacs-profiles/$PROFILE/elpaca"
PKG=$1; shift
REL=$1; shift

for base in repos sources; do
  script="$ELPACA/$base/$PKG/$REL"
  [ -x "$script" ] && exec "$script" "$@"
done
