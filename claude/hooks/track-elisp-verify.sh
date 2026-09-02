#!/usr/bin/env bash
# PostToolUse hook: bind Elisp commits to package-specific live evidence.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# Reuse the quote-aware shell classifiers used by the paired Codex hook.
# shellcheck source=../../codex/hooks/lib-codex-hook-json.sh
source "$SCRIPT_DIR/../../codex/hooks/lib-codex-hook-json.sh"
# shellcheck source=lib-elisp-evidence.sh
source "$SCRIPT_DIR/lib-elisp-evidence.sh"

INPUT=$(cat)
COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
SESSION_ID=$(printf '%s' "$INPUT" | jq -r '.session_id // empty')
EXIT_CODE=$(printf '%s' "$INPUT" | jq -r '.tool_output.exitCode // .tool_response.exitCode // "0"')
STDOUT=$(printf '%s' "$INPUT" | jq -r '
  .tool_output.stdout //
  (if (.tool_response | type) == "object" then
     (.tool_response.stdout // .tool_response.output // .tool_response.text // empty)
   else (.tool_response // empty) end) // empty
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
               { [ "$file" = "$package.el" ] || [ "$file" = "lisp/$package.el" ]; } &&
               ! git -C "$repo_root" cat-file -e "HEAD:$package.el" 2>/dev/null &&
               ! git -C "$repo_root" cat-file -e "HEAD:lisp/$package.el" 2>/dev/null; then
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

# A detected live-verify run whose evidence cannot be consumed must say
# so visibly: the verify gate would otherwise keep blocking later with a
# message that no longer names the cause. CONSUME_REASON stays empty for
# the benign no-pending-marker case, which needs no report.
report_unverified() {
  jq -n --arg message "Elisp live-verify evidence NOT recorded: $1 The verify gate will keep requiring it." \
    '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":$message}}'
  exit 0
}

consume_live_evidence() {
  local expected_label="$1" evidence verified_evidence version repo_b64 label_b64 commit repo label temporary
  CONSUME_REASON=""
  evidence=$(printf '%s\n' "$STDOUT" | grep '^ELISP_LIVE_EVIDENCE_V2:' | tail -1 || true)
  [ -n "$evidence" ] ||
    { CONSUME_REASON="the run printed no live-evidence line."; return 1; }
  verified_evidence=$(elisp_evidence_consume live "$evidence") ||
    { CONSUME_REASON="the live evidence has no valid one-time receipt."; return 1; }
  IFS=: read -r version repo_b64 label_b64 commit <<< "$verified_evidence"
  [ "$version" = ELISP_LIVE_EVIDENCE_V2 ] && [[ "$commit" =~ ^[0-9a-f]{40,64}$ ]] ||
    { CONSUME_REASON="the live evidence line is malformed."; return 1; }
  repo=$(decode_base64 "$repo_b64") ||
    { CONSUME_REASON="the live evidence repository field cannot be decoded."; return 1; }
  label=$(decode_base64 "$label_b64") ||
    { CONSUME_REASON="the live evidence label field cannot be decoded."; return 1; }
  [ "$label" = "$expected_label" ] ||
    { CONSUME_REASON="the live evidence label does not match the wrapper command."; return 1; }
  [ -d "$repo" ] && [ "$(git -C "$repo" rev-parse HEAD 2>/dev/null || true)" = "$commit" ] ||
    { CONSUME_REASON="the live evidence commit does not match the repository HEAD. Re-run after committing."; return 1; }
  acquire_lock ||
    { CONSUME_REASON="the verify marker could not be locked. Re-run the check."; return 1; }
  trap release_lock EXIT
  [ -s "$MARKER" ] || { release_lock; trap - EXIT; return 1; }
  temporary=$(mktemp "${TMPDIR:-/tmp}/elisp-verify.XXXXXX")
  # The evidence is bound to the repository's current HEAD, checked
  # above, so it certifies the newest committed state of this label.
  # Clear every pending row for the same repository and label, not just
  # one naming that exact commit: a commit made outside this tool leaves
  # a row naming a commit that is no longer HEAD, and matching on it
  # would strand the row forever and block the session.
  awk -F: -v repo="$repo_b64" -v label="$label_b64" \
    '$1 != repo || $3 != label' "$MARKER" > "$temporary"
  if [ -s "$temporary" ]; then mv -f "$temporary" "$MARKER"; else rm -f "$temporary" "$MARKER"; fi
  release_lock
  trap - EXIT
}

if [ "$EXIT_CODE" = 0 ]; then
  command_workdir=$(printf '%s' "$INPUT" | jq -r '
    .tool_input.workdir // .tool_input.cwd //
    .workdir // .cwd // empty')
  [ -n "$command_workdir" ] || command_workdir=$PWD
  if [[ "$command_workdir" != /* ]]; then command_workdir="$PWD/$command_workdir"; fi
  repos=()
  counts=()
  while IFS= read -r -d '' record; do
    [ "$(printf '%s' "$record" | jq -r '.subcommand')" = commit ] || continue
    # A successful direct `A && B` event proves that A succeeded. Other
    # control flow, substitutions, pipelines, or later commands do not prove
    # the parsed commit's result.
    if [ "$(printf '%s' "$record" | jq -r '.ambiguous // false')" = true ]; then
      [ "$(printf '%s' "$record" | jq -r '.ambiguity // empty')" = shell-status-decoupled ] &&
        [ "$(printf '%s' "$record" | jq -r '.status_operator // empty')" = '&&' ] || continue
    fi
    record_context=$(printf '%s' "$record" | jq -r '.context_dir // empty')
    [ -n "$record_context" ] || record_context=$command_workdir
    repo_root=$(codex_git_invocation_repo "$record" "$record_context" || true)
    [ -n "$repo_root" ] || continue
    found=-1
    for index in "${!repos[@]}"; do
      [ "${repos[$index]}" = "$repo_root" ] && found=$index
    done
    if [ "$found" -ge 0 ]; then
      counts[found]=$((counts[found] + 1))
    else
      repos+=("$repo_root")
      counts+=(1)
    fi
  done < <(printf '%s' "$COMMAND" | codex_git_invocations "$command_workdir")
  for index in "${!repos[@]}"; do
    record_commits "${repos[$index]}" "${counts[$index]}" || true
  done
  live_count=0
  live_label=""
  executable_count=0
  while IFS= read -r -d '' candidate; do
    live_count=$((live_count + 1))
    live_label="$candidate"
  done < <(printf '%s' "$COMMAND" | codex_elisp_evidence_labels live)
  while IFS= read -r -d '' executable; do
    executable_count=$((executable_count + 1))
  done < <(printf '%s' "$COMMAND" | codex_shell_executables)
  if [ "$live_count" -gt 0 ]; then
    if [ "$live_count" -gt 1 ]; then
      report_unverified "the command contains $live_count live-verify invocations. Run each check as its own command."
    fi
    if [ "$executable_count" -ne 1 ]; then
      report_unverified "the wrapper must be the only executable in the command. Re-run it bare, without pipes, chains, or other commands."
    fi
    if ! consume_live_evidence "$live_label" && [ -n "$CONSUME_REASON" ]; then
      report_unverified "$CONSUME_REASON"
    fi
  fi
fi

exit 0
