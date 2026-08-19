#!/bin/bash
# PreToolUse hook: block git commit when Elisp files are staged
# but no test has been run in this session.
#
# Reads JSON from stdin (Claude Code PreToolUse format).
# Outputs JSON with permissionDecision to allow or deny.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"

INPUT=$(cat)

COMMAND=$(codex_shell_command "$INPUT")
SESSION_ID=$(codex_session_id "$INPUT")
TOOL_NAME=$(codex_tool_name "$INPUT")
CODEX_GIT_PARSE_CONTEXT=$(printf '%s' "$INPUT" | jq -r "${CODEX_HOOK_JQ_DEFS}
  codex_tool_input.workdir // codex_tool_input.cwd //
  codex_tool_input.working_directory // codex_tool_input.working_dir //
  .workdir // .cwd // .working_directory // .working_dir // empty")
[ -n "$CODEX_GIT_PARSE_CONTEXT" ] || CODEX_GIT_PARSE_CONTEXT=$PWD
if [[ "$CODEX_GIT_PARSE_CONTEXT" != /* ]]; then
  CODEX_GIT_PARSE_CONTEXT="$PWD/$CODEX_GIT_PARSE_CONTEXT"
fi
[ -d "$CODEX_GIT_PARSE_CONTEXT" ] || CODEX_GIT_PARSE_CONTEXT=$PWD

deny() {
  local reason="$1"
  jq -n --arg reason "$reason" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": $reason
    }
  }'
  exit 0
}

# functions.exec does not dispatch separate pre-tool events for nested shell
# calls. Route each literal nested call through this gate. A missing nested
# workdir inherits the outer hook context, which is also what exec_command does.
if [ "$TOOL_NAME" = functions.exec ]; then
  OUTER_CONTEXT=$(printf '%s' "$INPUT" | jq -r "${CODEX_HOOK_JQ_DEFS}
    codex_tool_input.workdir // codex_tool_input.cwd //
    codex_tool_input.working_directory // codex_tool_input.working_dir //
    .workdir // .cwd // .working_directory // .working_dir // empty")
  [ -n "$OUTER_CONTEXT" ] || OUTER_CONTEXT=$PWD
  ROUTED_CONTEXTS=()
  while IFS= read -r -d '' context; do
    NESTED_COMMAND=$(printf '%s' "$context" | jq -r '.cmd // empty')
    AMBIGUOUS=$(printf '%s' "$context" | jq -r '.ambiguous')
    if [ "$AMBIGUOUS" = true ]; then
      if [ -n "$NESTED_COMMAND" ] &&
         [ "$(printf '%s' "$NESTED_COMMAND" | codex_git_commit_count)" -gt 0 ]; then
        deny "BLOCKED: A nested functions.exec Git commit has a dynamic or ambiguous cmd/workdir. Use literal cmd and workdir fields so the Elisp evidence gate can identify the target repository."
      fi
      continue
    fi
    [ -n "$NESTED_COMMAND" ] || continue
    NESTED_WORKDIR=$(printf '%s' "$context" | jq -r '.workdir // empty')
    if [ -z "$NESTED_WORKDIR" ]; then
      NESTED_WORKDIR=$OUTER_CONTEXT
    elif [[ "$NESTED_WORKDIR" != /* ]]; then
      NESTED_WORKDIR="$OUTER_CONTEXT/$NESTED_WORKDIR"
    fi
    if [ -d "$NESTED_WORKDIR" ]; then
      NESTED_WORKDIR=$(cd -- "$NESTED_WORKDIR" && pwd -P)
    fi
    ROUTED_CONTEXTS+=("$(jq -nc --arg cmd "$NESTED_COMMAND" --arg dir "$NESTED_WORKDIR" '{cmd:$cmd,dir:$dir}')")
  done < <(printf '%s' "$COMMAND" | codex_nested_exec_contexts)
  # Check every group with the same resolved workdir as one program. This
  # closes the gap where separate nested calls stage and then commit before
  # either call receives its own pre-tool event.
  for routed_context in "${ROUTED_CONTEXTS[@]}"; do
    GROUP_WORKDIR=$(printf '%s' "$routed_context" | jq -r '.dir')
    GROUP_COMMANDS=""
    for candidate_context in "${ROUTED_CONTEXTS[@]}"; do
      [ "$(printf '%s' "$candidate_context" | jq -r '.dir')" = "$GROUP_WORKDIR" ] || continue
      GROUP_COMMANDS="${GROUP_COMMANDS}$(printf '%s' "$candidate_context" | jq -r '.cmd')"$'\n'
    done
    if [ "$(printf '%s' "$GROUP_COMMANDS" | codex_git_commit_count "$GROUP_WORKDIR")" -gt 0 ] &&
       [ "$(printf '%s' "$GROUP_COMMANDS" | codex_git_subcommand_count add "$GROUP_WORKDIR")" -gt 0 ]; then
      NESTED_INPUT=$(jq -nc --arg cmd "$GROUP_COMMANDS" --arg workdir "$GROUP_WORKDIR" --arg session "$SESSION_ID" \
        '{tool_name:"functions.exec_command",session_id:$session,tool_input:{cmd:$cmd,workdir:$workdir}}')
      NESTED_RESULT=$(printf '%s' "$NESTED_INPUT" | "$0")
      if [ -n "$NESTED_RESULT" ]; then
        printf '%s\n' "$NESTED_RESULT"
        exit 0
      fi
    fi
  done
  for routed_context in "${ROUTED_CONTEXTS[@]}"; do
    NESTED_COMMAND=$(printf '%s' "$routed_context" | jq -r '.cmd')
    NESTED_WORKDIR=$(printf '%s' "$routed_context" | jq -r '.dir')
    NESTED_INPUT=$(jq -nc --arg cmd "$NESTED_COMMAND" --arg workdir "$NESTED_WORKDIR" --arg session "$SESSION_ID" \
      '{tool_name:"functions.exec_command",session_id:$session,tool_input:{cmd:$cmd,workdir:$workdir}}')
    NESTED_RESULT=$(printf '%s' "$NESTED_INPUT" | "$0")
    if [ -n "$NESTED_RESULT" ]; then
      printf '%s\n' "$NESTED_RESULT"
      exit 0
    fi
  done
  exit 0
fi

# Only intercept git commit commands
if [ "$(printf '%s' "$COMMAND" | codex_git_commit_count)" -eq 0 ]; then
  exit 0
fi

# Reject ambiguity before repository lookup. Target-changing environment
# variables can otherwise make lookup fail and cause this gate to stand down.
COMMIT_RECORD=""
SYNTAX_AMBIGUOUS=false
while IFS= read -r -d '' record; do
  if [ "$(printf '%s' "$record" | jq -r '.subcommand')" = commit ]; then
    if [ "$(printf '%s' "$record" | jq -r '.ambiguous // false')" = true ]; then
      AMBIGUITY=$(printf '%s' "$record" | jq -r '.ambiguity // empty')
      case "$AMBIGUITY" in
        unknown-git-global-option)
          deny "BLOCKED: Git global options make the commit subcommand ambiguous. Use recognized Git global options so the Elisp evidence gate can identify the commit and its target repository."
          ;;
        git-environment|shell-recursion-limit|invalid-command-substitution|missing-interpreter-command|dynamic-shell-executable|git-shell-alias|git-alias-depth|invalid-git-alias|empty-git-alias)
          deny "BLOCKED: Git commit syntax is dynamic or ambiguous. Use a literal Git commit command, target repository, index, and supported shell form so the Elisp evidence gate can inspect the exact commit."
          ;;
        *) SYNTAX_AMBIGUOUS=true ;;
      esac
    fi
    [ -n "$COMMIT_RECORD" ] || COMMIT_RECORD=$record
  fi
done < <(printf '%s' "$COMMAND" | codex_git_invocations)

# Inspect staged files in the repo targeted by the command, not the hook cwd.
# shellcheck source=lib-repo-root.sh
source "$SCRIPT_DIR/lib-repo-root.sh"
if [ -z "$REPO_ROOT" ]; then
  exit 0
fi

# Check if any staged files are Elisp-related (amend-aware: see lib-staged-files.sh)
# shellcheck source=lib-staged-files.sh
source "$SCRIPT_DIR/lib-staged-files.sh"

HAS_ELISP=false
if [ -n "$STAGED_STATUS" ]; then
  while IFS=$'\t' read -r status first second; do
    case "$status" in R*|C*) paths="$first"$'\n'"$second" ;; *) paths="$first" ;; esac
    while IFS= read -r file; do
      case "$file" in *.el|emacs/config.org) HAS_ELISP=true ;; esac
    done <<< "$paths"
    [ "$HAS_ELISP" = false ] || break
  done <<< "$STAGED_STATUS"
fi

# A combined add+commit call cannot be checked against the future index. Make
# the agent split the operations so the commit gate can identify exact bytes.
while IFS= read -r -d '' record; do
  [ "$(printf '%s' "$record" | jq -r '.subcommand')" = add ] || continue
  ADD_REPO_ROOT=$(codex_git_invocation_repo "$record" "${REPO_COMMAND_CONTEXT:-$REPO_ROOT}" || true)
  [ "$ADD_REPO_ROOT" = "$REPO_ROOT" ] || continue
  if codex_git_add_selects_elisp "$REPO_ROOT" "$record"; then
    deny "BLOCKED: Stage Elisp source in a separate command before git commit. A combined git add and git commit call cannot bind test evidence to the future index."
  fi
done < <(printf '%s' "$COMMAND" | codex_git_invocations)

if [ "$SYNTAX_AMBIGUOUS" = true ] && [ "$HAS_ELISP" = true ]; then
  deny "BLOCKED: Git commit syntax is dynamic or ambiguous. Use a literal Git commit command, target repository, index, and supported shell form so the Elisp evidence gate can inspect the exact commit."
fi

if [ "$HAS_ELISP" = false ]; then
  exit 0
fi

# Check for evidence tied to this repository, package, and source revision.
MARKER="/tmp/claude-elisp-tested-${SESSION_ID}"
DOTFILES_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)
REVISION_HELPER="$DOTFILES_ROOT/claude/bin/elisp-source-revision"
REVISION=$("$REVISION_HELPER" "$REPO_ROOT")
REPO_B64=$(printf '%s' "$REPO_ROOT" | base64 | tr -d '\n')

evidence_matches_package() {
  local package="$1" package_b64
  package_b64=$(printf '%s' "$package" | base64 | tr -d '\n')
  [ -f "$MARKER" ] && grep -qxF "$REPO_B64:$package_b64:$REVISION" "$MARKER"
}

EXPECTED_PACKAGES=()
INDEX_DIVERGENCE=""
record_expected_package() {
  local file="$1" change_kind="$2" package
  if [ "$change_kind" = deleted ]; then
    case "$file" in *.el|emacs/config.org) EXPECTED_PACKAGES+=("file:$file") ;; esac
  else
  case "$file" in
    emacs/extras/*.el)
      package=$(basename "$file" .el)
      package=${package#test-}
      package=${package%-tests}
      package=${package%-test}
      EXPECTED_PACKAGES+=("$package")
      ;;
    emacs/config.org) EXPECTED_PACKAGES+=("file:emacs/config.org") ;;
    .dir-locals.el|*/.dir-locals.el|lockfile.el|*/lockfile.el|*-autoloads.el|*-pkg.el)
      EXPECTED_PACKAGES+=("file:$file")
      ;;
    *.el)
      if [ "$REPO_ROOT" = "$DOTFILES_ROOT" ]; then
        EXPECTED_PACKAGES+=("file:$file")
      else
        EXPECTED_PACKAGES+=("$(basename "$REPO_ROOT")")
      fi
      ;;
  esac
  fi
  case "$file" in
    *.el|emacs/config.org)
      if ! git -C "$REPO_ROOT" diff --quiet -- "$file"; then
        INDEX_DIVERGENCE="$file"
      fi
      ;;
  esac
}
while IFS=$'\t' read -r status first second; do
  [ -n "$first" ] || continue
  case "$status" in
    D*) record_expected_package "$first" deleted ;;
    R*)
      record_expected_package "$first" deleted
      record_expected_package "$second" present
      ;;
    C*) record_expected_package "$second" present ;;
    *) record_expected_package "$first" present ;;
  esac
done <<< "$STAGED_STATUS"

USE_STAGED_EVIDENCE=false
if [ -n "$INDEX_DIVERGENCE" ]; then
  for package in "${EXPECTED_PACKAGES[@]}"; do
    if [[ "$package" != file:* ]]; then
      REASON="BLOCKED: The staged and working-tree versions differ for ${INDEX_DIVERGENCE}. Package batch evidence must test the same bytes that the commit will contain."
      jq -n --arg reason "$REASON" '{
        "hookSpecificOutput": {
          "hookEventName": "PreToolUse",
          "permissionDecision": "deny",
          "permissionDecisionReason": $reason
        }
      }'
      exit 0
    fi
  done
  REVISION=$("$REVISION_HELPER" --index "$REPO_ROOT")
  USE_STAGED_EVIDENCE=true
fi

MISSING_PACKAGE=""
for package in "${EXPECTED_PACKAGES[@]}"; do
  if ! evidence_matches_package "$package"; then
    MISSING_PACKAGE="$package"
    break
  fi
done
if [ -z "$MISSING_PACKAGE" ]; then
  exit 0
fi

# Block the commit
# Extract the first staged .el filename to suggest a ready-to-paste command
STAGED_EL=$(echo "$STAGED" | grep '\.el$' | head -1 || true)
PKG_NAME=""
if [ -n "$STAGED_EL" ]; then
  PKG_NAME=$(basename "$STAGED_EL" .el)
fi

if [ -n "$MISSING_PACKAGE" ]; then
  PKG_NAME="$MISSING_PACKAGE"
fi
if [[ "$PKG_NAME" == file:* ]]; then
  FILE_LABEL=${PKG_NAME#file:}
  STAGED_FLAG=""
  [ "$USE_STAGED_EVIDENCE" = true ] && STAGED_FLAG="--staged "
  EXAMPLE_CMD="\"$DOTFILES_ROOT/claude/bin/elisp-check-evidence\" ${STAGED_FLAG}file:${FILE_LABEL} -- PROJECT-CHECK"
elif [ -n "$PKG_NAME" ]; then
  EXAMPLE_CMD="\"$DOTFILES_ROOT/claude/bin/batch-test.sh\" ${PKG_NAME}"
else
  EXAMPLE_CMD="\"$DOTFILES_ROOT/claude/bin/batch-test.sh\" YOUR-PACKAGE"
fi

REASON="BLOCKED: The staged Elisp source does not have matching test evidence for this repository, package or file label, and source revision.\n\nCommand:\n${EXAMPLE_CMD}\n\nReplace PROJECT-CHECK with a tracked executable in the owning repository. Run the test after the final source edit. For config.org, use the exact file:emacs/config.org label and a tracked check that tangles and validates the affected output."
jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
