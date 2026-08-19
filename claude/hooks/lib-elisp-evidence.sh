#!/usr/bin/env bash
# Issue and consume one-time correlation receipts for Elisp evidence.
#
# These receipts prevent accidental output reuse and replay between supported
# wrapper calls. They do not authenticate evidence against a process running as
# the same user, which can source this public issuer or write the user-owned
# marker and receipt files directly.

elisp_evidence_cleanup_receipts() {
  local root="$1" candidate
  for candidate in "$root"/receipt.* "$root"/.claim.*; do
    [ -f "$candidate" ] && [ ! -L "$candidate" ] && [ -O "$candidate" ] || continue
    find "$candidate" -type f -mtime +0 -exec rm -f {} \;
  done
}

elisp_evidence_receipt_root() {
  local root current_uid
  current_uid=${UID:-$(id -u)}
  root=${ELISP_EVIDENCE_RECEIPT_DIR:-${TMPDIR:-/tmp}/claude-elisp-evidence-$current_uid}
  case "$root" in
    /*) ;;
    *) return 1 ;;
  esac
  [ ! -L "$root" ] || return 1
  if [ ! -e "$root" ]; then
    (umask 077 && mkdir "$root") 2>/dev/null || {
      [ -d "$root" ] && [ ! -L "$root" ] || return 1
    }
  fi
  [ -d "$root" ] && [ -O "$root" ] || return 1
  chmod 700 "$root" || return 1
  elisp_evidence_cleanup_receipts "$root"
  printf '%s\n' "$root"
}

elisp_evidence_prefix() {
  case "$1" in
    test) printf '%s\n' ELISP_TEST_EVIDENCE_V2 ;;
    live) printf '%s\n' ELISP_LIVE_EVIDENCE_V2 ;;
    *) return 1 ;;
  esac
}

elisp_evidence_valid_identity() {
  case "$1:$2" in
    test:*) [[ "$2" =~ ^[0-9a-f]{64}$ ]] ;;
    live:*) [[ "$2" =~ ^[0-9a-f]{40,64}$ ]] ;;
    *) return 1 ;;
  esac
}

elisp_evidence_emit() {
  local kind="$1" repo_b64="$2" label_b64="$3" identity="$4"
  local prefix root receipt token evidence
  prefix=$(elisp_evidence_prefix "$kind") || return 1
  [ -n "$repo_b64" ] && [ -n "$label_b64" ] || return 1
  [[ "$repo_b64" != *:* ]] && [[ "$label_b64" != *:* ]] || return 1
  elisp_evidence_valid_identity "$kind" "$identity" || return 1
  root=$(elisp_evidence_receipt_root) || return 1
  receipt=$(umask 077 && mktemp "$root/receipt.XXXXXXXX") || return 1
  token=${receipt##*/}
  evidence="$prefix:$repo_b64:$label_b64:$identity"
  if ! printf '%s\n' "$evidence" > "$receipt"; then
    rm -f "$receipt"
    return 1
  fi
  printf '%s:%s\n' "$evidence" "$token"
}

elisp_evidence_consume() {
  local kind="$1" evidence="$2"
  local version repo_b64 label_b64 identity token prefix root receipt claim
  local expected stored size expected_size valid
  IFS=: read -r version repo_b64 label_b64 identity token <<< "$evidence"
  prefix=$(elisp_evidence_prefix "$kind") || return 1
  [ "$version" = "$prefix" ] || return 1
  [ -n "$repo_b64" ] && [ -n "$label_b64" ] || return 1
  elisp_evidence_valid_identity "$kind" "$identity" || return 1
  [[ "$token" =~ ^receipt\.[A-Za-z0-9]+$ ]] || return 1
  expected="$version:$repo_b64:$label_b64:$identity:$token"
  [ "$evidence" = "$expected" ] || return 1

  root=$(elisp_evidence_receipt_root) || return 1
  receipt="$root/$token"
  claim="$root/.claim.${token#receipt.}.$$.$RANDOM"
  mv "$receipt" "$claim" 2>/dev/null || return 1

  valid=false
  if [ -f "$claim" ] && [ ! -L "$claim" ] && [ -O "$claim" ]; then
    stored=$(LC_ALL=C cat "$claim" 2>/dev/null || true)
    size=$(LC_ALL=C wc -c < "$claim" 2>/dev/null | tr -d ' ' || true)
    expected=${evidence%:"$token"}
    expected_size=$((${#expected} + 1))
    if [ "$stored" = "$expected" ] && [ "$size" = "$expected_size" ]; then
      valid=true
    fi
  fi
  rm -f "$claim"
  [ "$valid" = true ] || return 1
  printf '%s\n' "$expected"
}
