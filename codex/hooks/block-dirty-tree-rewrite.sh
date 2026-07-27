#!/bin/bash
# PreToolUse hook: block a git history rewrite while the working tree is dirty.
#
# `git filter-repo` and `git filter-branch` hard-reset the working tree as part
# of rewriting history. Any uncommitted change to a tracked file is destroyed,
# and because those edits were never in git there is nothing to recover from:
# no reflog entry, no dangling blob, no stash. A backup bundle does not help
# either — it only carries committed history.
#
# This happened on 2026-07-27: a filter-repo run wiped three files of
# in-progress work that had been left uncommitted on purpose. They came back
# only because Google Drive had not yet synced the deletion. Luck is not a
# safety net, hence this hook.
#
# Untracked files are deliberately NOT counted: a hard reset leaves them alone,
# and the notes repo routinely carries untracked scratch, which would make this
# fire constantly and train the reader to ignore it.
#
# Reads JSON from stdin and normalizes Bash, exec_command, functions.exec, and
# functions.exec_command payloads via lib-codex-hook-json.sh.
# Outputs JSON with permissionDecision to allow or deny.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"

INPUT=$(cat)

COMMAND=$(codex_shell_command "$INPUT")
[ -n "$COMMAND" ] || exit 0

# Cheap pre-filter: skip the expensive parsing for the vast majority of commands.
if ! printf '%s' "$COMMAND" | grep -qE 'filter-repo|filter-branch'; then
  exit 0
fi

# Documented escape hatch for a deliberate rewrite over a dirty tree.
if printf '%s' "$COMMAND" | grep -qE '(^|[[:space:]])ALLOW_DIRTY_REWRITE=1([[:space:]]|$)'; then
  exit 0
fi

# Decide whether this is a real invocation, and against which repo.
# Exits 1 when the match was incidental (e.g. the phrase appears inside a
# heredoc such as a commit message describing a past rewrite).
TARGET=$(CMD="$COMMAND" python3 <<'PY'
import os
import re
import sys

cmd = os.environ["CMD"]

# Drop heredoc bodies first. A commit message that merely mentions
# "git filter-repo" must not be treated as an invocation.
kept, marker = [], None
for line in cmd.split("\n"):
    if marker is not None:
        if line.strip() == marker:
            marker = None
        continue
    kept.append(line)
    opener = re.search(r"<<-?\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1", line)
    if opener:
        marker = opener.group(2)
stripped = "\n".join(kept)

PATH_RE = r"\"[^\"]+\"|'[^']+'|[^\s;&|]+"

# Require a command position: start of string, or after a newline or a shell
# operator. This is what separates an invocation from a mention in an argument.
rewrite = re.compile(
    r"(?:^|[\n;&|(])\s*"
    r"(?:[A-Za-z_][A-Za-z0-9_]*=\S*\s+)*"
    r"(?:git(?:\s+-C\s+(?P<cdir>" + PATH_RE + r"))?\s+filter-(?:repo|branch)"
    r"|git-filter-repo)\b"
)
match = rewrite.search(stripped)
if not match:
    sys.exit(1)

# Which repo is at risk? `git -C <dir>` wins; otherwise the last `cd <dir>`
# in the command; otherwise the hook's own working directory.
target = (match.group("cdir") or "").strip("\"'")
if not target:
    for cd in re.finditer(r"(?:^|[\n;&|(])\s*cd\s+(" + PATH_RE + r")", stripped):
        target = cd.group(1).strip("\"'")
print(target)
PY
) || exit 0

if [ -z "$TARGET" ]; then
  CWD=$(printf '%s' "$INPUT" | jq -r '.cwd // empty')
  TARGET="${CWD:-$PWD}"
fi

# Staged and unstaged changes to tracked files. Untracked deliberately excluded.
DIRTY=$(git -C "$TARGET" status --porcelain --untracked-files=no 2>/dev/null || true)
[ -n "$DIRTY" ] || exit 0

REPO=$(git -C "$TARGET" rev-parse --show-toplevel 2>/dev/null || printf '%s' "$TARGET")
COUNT=$(printf '%s\n' "$DIRTY" | grep -c . || true)

REASON="BLOCKED: git history rewrite with a dirty working tree in ${REPO}

filter-repo and filter-branch hard-reset the working tree. The ${COUNT} uncommitted change(s) below would be destroyed, and since they were never committed, git cannot recover them — no reflog, no stash, no backup bundle.

${DIRTY}

Commit them, or stash with \`git stash push -u\`, then rerun the rewrite and \`git stash pop\` afterwards.

If the changes really are disposable, prefix the command with ALLOW_DIRTY_REWRITE=1."

jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
