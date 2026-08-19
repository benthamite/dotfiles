#!/usr/bin/env bash
# PostToolUse hook: bind Elisp commits to package-specific live evidence.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"
# shellcheck source=lib-elisp-evidence.sh
source "$SCRIPT_DIR/lib-elisp-evidence.sh"

INPUT=$(cat)
COMMAND=$(codex_shell_command "$INPUT")
TOOL_NAME=$(codex_tool_name "$INPUT")
SESSION_ID=$(codex_session_id "$INPUT")
EXIT_CODE=$(codex_hook_jq "$INPUT" '
  .tool_output.exitCode // .tool_output.exit_code //
  codex_tool_response.exitCode // codex_tool_response.exit_code // "0"
')
STDOUT=$(codex_hook_jq "$INPUT" '
  .tool_output.stdout // codex_tool_response.stdout //
  codex_tool_response.output // codex_tool_response.text // empty
')

MARKER="/tmp/claude-elisp-verify-needed-${SESSION_ID}"
LOCK_DIR="${MARKER}.lock"
DOTFILES_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)

acquire_lock() {
  local attempt
  for attempt in $(seq 1 200); do
    if mkdir "$LOCK_DIR" 2>/dev/null; then
      printf '%s\n' "$$" > "$LOCK_DIR/owner"
      return 0
    fi
    if [ -s "$LOCK_DIR/owner" ]; then
      lock_owner=$(sed -n '1p' "$LOCK_DIR/owner")
      if ! kill -0 "$lock_owner" 2>/dev/null; then
        rm -f "$LOCK_DIR/owner"
        rmdir "$LOCK_DIR" 2>/dev/null || true
      fi
    fi
    sleep 0.01
  done
  return 1
}
release_lock() { rm -f "$LOCK_DIR/owner"; rmdir "$LOCK_DIR" 2>/dev/null || true; }
encode_base64() { printf '%s' "$1" | base64 | tr -d '\n'; }
decode_base64() {
  if printf '%s' "$1" | base64 -D 2>/dev/null; then return 0; fi
  printf '%s' "$1" | base64 -d 2>/dev/null
}

production_label() {
  local repo_root="$1" file="$2" change_kind="${3:-present}" package
  case "$file" in
    test/*|tests/*|*/test/*|*/tests/*|*-test.el|*-tests.el|*/test-*.el) return 1 ;;
    emacs/extras/*.el)
      package=$(basename "$file" .el)
      if [ "$change_kind" = deleted ]; then printf 'deleted:%s' "$package"
      else printf '%s' "$package"
      fi
      ;;
    *.el)
      if [ "$repo_root" = "$DOTFILES_ROOT" ]; then return 1
      else
        case "$repo_root" in
          */elpaca/sources/*|*/elpaca/repos/*)
            package=$(basename "$repo_root")
            if [ "$change_kind" = deleted ] &&
               [ "$(basename "$file")" = "$package.el" ]; then
              printf 'deleted:%s' "$package"
            else
              printf '%s' "$package"
            fi
            ;;
          *) return 1 ;;
        esac
      fi
      ;;
    *) return 1 ;;
  esac
}

append_production_label() {
  local repo_root="$1" file="$2" change_kind="$3" labels_file="$4"
  local label label_b64
  if label=$(production_label "$repo_root" "$file" "$change_kind"); then
    label_b64=$(encode_base64 "$label")
    printf '%s\n' "$label_b64" >> "$labels_file"
  fi
}

record_commits() {
  local repo_root="$1" count="$2" head repo_b64 temporary labels_file
  local offset ref committed status first second label_b64 existing_repo old_commit existing_label
  head=$(git -C "$repo_root" rev-parse HEAD 2>/dev/null) || return 1
  repo_b64=$(encode_base64 "$repo_root")
  labels_file=$(mktemp "${TMPDIR:-/tmp}/elisp-labels.XXXXXX")
  temporary=$(mktemp "${TMPDIR:-/tmp}/elisp-verify.XXXXXX")
  if ! acquire_lock; then
    printf 'MALFORMED:lock-failed:%s\n' "$head" >> "$MARKER"
    rm -f "$labels_file" "$temporary"
    return 1
  fi
  trap 'release_lock; rm -f "$labels_file" "$temporary"' EXIT

  if [ -s "$MARKER" ]; then
    while IFS=: read -r existing_repo old_commit existing_label; do
      if [ -z "$existing_repo" ] || [ -z "$old_commit" ] || [ -z "$existing_label" ]; then
        printf '%s:%s:%s\n' "$existing_repo" "$old_commit" "$existing_label" >> "$temporary"
      elif [ "$existing_repo" = "$repo_b64" ]; then
        printf '%s\n' "$existing_label" >> "$labels_file"
      else
        printf '%s:%s:%s\n' "$existing_repo" "$old_commit" "$existing_label" >> "$temporary"
      fi
    done < "$MARKER"
  fi

  offset=0
  while [ "$offset" -lt "$count" ]; do
    if [ "$offset" -eq 0 ]; then ref=HEAD; else ref="HEAD~$offset"; fi
    if ! committed=$(git -C "$repo_root" diff-tree --root --no-commit-id --name-status -M -r "$ref" 2>/dev/null); then
      printf 'MALFORMED:diff-tree-failed:%s\n' "$ref" >> "$temporary"
      break
    fi
    while IFS=$'\t' read -r status first second; do
      [ -n "$first" ] || continue
      case "$status" in
        D*) append_production_label "$repo_root" "$first" deleted "$labels_file" ;;
        R*)
          append_production_label "$repo_root" "$first" deleted "$labels_file"
          append_production_label "$repo_root" "$second" present "$labels_file"
          ;;
        C*) append_production_label "$repo_root" "$second" present "$labels_file" ;;
        *) append_production_label "$repo_root" "$first" present "$labels_file" ;;
      esac
    done <<< "$committed"
    offset=$((offset + 1))
  done

  sort -u "$labels_file" | while IFS= read -r label_b64; do
    [ -n "$label_b64" ] || continue
    printf '%s:%s:%s\n' "$repo_b64" "$head" "$label_b64" >> "$temporary"
  done
  if [ -s "$temporary" ]; then mv -f "$temporary" "$MARKER"; else rm -f "$temporary"; fi
  rm -f "$labels_file"
  release_lock
  trap - EXIT
}

consume_live_evidence() {
  local evidence verified_evidence version repo_b64 label_b64 commit repo temporary
  evidence=$(printf '%s\n' "$STDOUT" | grep '^ELISP_LIVE_EVIDENCE_V2:' | tail -1 || true)
  [ -n "$evidence" ] || return 1
  verified_evidence=$(elisp_evidence_consume live "$evidence") || return 1
  IFS=: read -r version repo_b64 label_b64 commit <<< "$verified_evidence"
  [ "$version" = ELISP_LIVE_EVIDENCE_V2 ] && [[ "$commit" =~ ^[0-9a-f]{40,64}$ ]] || return 1
  repo=$(decode_base64 "$repo_b64") || return 1
  [ -d "$repo" ] && [ "$(git -C "$repo" rev-parse HEAD 2>/dev/null || true)" = "$commit" ] || return 1
  acquire_lock || return 1
  trap release_lock EXIT
  [ -s "$MARKER" ] || { release_lock; trap - EXIT; return 1; }
  temporary=$(mktemp "${TMPDIR:-/tmp}/elisp-verify.XXXXXX")
  awk -F: -v repo="$repo_b64" -v commit="$commit" -v label="$label_b64" \
    '$1 != repo || $2 != commit || $3 != label' "$MARKER" > "$temporary"
  if [ -s "$temporary" ]; then mv -f "$temporary" "$MARKER"; else rm -f "$temporary" "$MARKER"; fi
  release_lock
  trap - EXIT
}

process_command() {
  local completed_command="$1" context_dir="$2" commit_count repo_root live_count
  commit_count=$(printf '%s' "$completed_command" | codex_git_commit_count)
  if [ "$commit_count" -gt 0 ]; then
    COMMAND="$completed_command"
    REPO_CONTEXT_DIR="$context_dir"
    # shellcheck source=lib-repo-root.sh
    source "$SCRIPT_DIR/lib-repo-root.sh"
    repo_root="$REPO_ROOT"
    if [ -n "$repo_root" ]; then record_commits "$repo_root" "$commit_count" || true; fi
  fi
  live_count=$(printf '%s' "$completed_command" | codex_executable_count elisp-live-verify)
  if [ "$live_count" -eq 1 ]; then consume_live_evidence || true; fi
}

if [ "$TOOL_NAME" = functions.exec ]; then
  repos=()
  counts=()
  outer_live_count=0
  outer_workdir=$PWD
  while IFS= read -r -d '' context; do
    [ "$(printf '%s' "$context" | jq -r '.ambiguous')" = false ] || continue
    nested_command=$(printf '%s' "$context" | jq -r '.cmd // empty')
    nested_workdir=$(printf '%s' "$context" | jq -r '.workdir // empty')
    if [ -z "$nested_workdir" ]; then
      nested_workdir=$outer_workdir
    elif [[ "$nested_workdir" != /* ]]; then
      nested_workdir="$outer_workdir/$nested_workdir"
    fi
    [ -d "$nested_workdir" ] || continue
    nested_count=$(printf '%s' "$nested_command" | codex_git_commit_count)
    nested_live_count=$(printf '%s' "$nested_command" | codex_executable_count elisp-live-verify)
    outer_live_count=$((outer_live_count + nested_live_count))
    [ "$nested_count" -gt 0 ] || continue
    COMMAND="$nested_command"
    REPO_CONTEXT_DIR="$nested_workdir"
    # shellcheck source=lib-repo-root.sh
    source "$SCRIPT_DIR/lib-repo-root.sh"
    [ -n "$REPO_ROOT" ] || continue
    found=-1
    for index in "${!repos[@]}"; do [ "${repos[$index]}" = "$REPO_ROOT" ] && found=$index; done
    if [ "$found" -ge 0 ]; then counts[found]=$((counts[found] + nested_count))
    else repos+=("$REPO_ROOT"); counts+=("$nested_count")
    fi
  done < <(printf '%s' "$COMMAND" | codex_nested_exec_contexts)
  for index in "${!repos[@]}"; do record_commits "${repos[$index]}" "${counts[$index]}" || true; done
  if [ "$outer_live_count" -eq 1 ] && [ "$EXIT_CODE" = 0 ]; then consume_live_evidence || true; fi
elif [ "$EXIT_CODE" = 0 ]; then
  process_command "$COMMAND" ""
fi

exit 0
