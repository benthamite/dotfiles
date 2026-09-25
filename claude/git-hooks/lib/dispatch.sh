# Body of every hook in this global core.hooksPath directory.
# Agent sessions get the Elisp evidence check on pre-commit and
# post-commit, and the documentation and paired-config gates on
# pre-commit.  Every hook then runs the repository's own hook, which a
# global core.hooksPath would otherwise disable.
hook=$(basename "$0")
hooks_dir=$(cd "$(dirname "$0")" && pwd -P)
if [ -n "${CLAUDE_CODE_SESSION_ID:-}${CODEX_THREAD_ID:-}" ]; then
  case "$hook" in
    pre-commit)
      # The parent is the git process, so its arguments show an --amend.
      AGENT_GIT_ARGS=$(ps -o args= -p "$PPID" 2>/dev/null || true)
      export AGENT_GIT_ARGS
      status=0
      "$hooks_dir/../bin/elisp-evidence" pre-commit || status=1
      "$hooks_dir/../bin/agent-commit-gates" || status=1
      [ "$status" -eq 0 ] || exit 1
      ;;
    post-commit) "$hooks_dir/../bin/elisp-evidence" post-commit ;;
  esac
fi
common_dir=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null) || exit 0
if [ -x "$common_dir/hooks/$hook" ]; then
  exec "$common_dir/hooks/$hook" "$@"
fi
exit 0
