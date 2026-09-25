# Body of every hook in this global core.hooksPath directory.
# Agent sessions get the Elisp evidence check on pre-commit and
# post-commit.  Every hook then runs the repository's own hook, which a
# global core.hooksPath would otherwise disable.
hook=$(basename "$0")
hooks_dir=$(cd "$(dirname "$0")" && pwd -P)
if [ -n "${CLAUDE_CODE_SESSION_ID:-}${CODEX_THREAD_ID:-}" ]; then
  case "$hook" in
    pre-commit) "$hooks_dir/../bin/elisp-evidence" pre-commit || exit 1 ;;
    post-commit) "$hooks_dir/../bin/elisp-evidence" post-commit ;;
  esac
fi
common_dir=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null) || exit 0
if [ -x "$common_dir/hooks/$hook" ]; then
  exec "$common_dir/hooks/$hook" "$@"
fi
exit 0
