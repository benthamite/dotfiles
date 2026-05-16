#!/usr/bin/env bash
# Stop hook: audit final-answer completion claims for verification evidence.
#
# This cannot prevent the already-emitted final answer, but it makes ungrounded
# "fixed/done" claims visible immediately and records a reusable failure signal.

set -euo pipefail

payload=$(cat)

event=$(jq -r '.hook_event_name // .hookEventName // .event // empty' <<<"$payload" 2>/dev/null || true)
if [[ -n "$event" && "$event" != "Stop" ]]; then
  exit 0
fi

transcript_path=$(jq -r '.transcript_path // .transcriptPath // empty' <<<"$payload" 2>/dev/null || true)
if [[ -z "$transcript_path" || ! -f "$transcript_path" ]] && [[ -d "${HOME}/.codex/sessions" ]]; then
  transcript_path=$(find "${HOME}/.codex/sessions" -type f -name '*.jsonl' -mmin -10 -print 2>/dev/null \
    | sort \
    | tail -n 1 || true)
fi

if [[ -z "$transcript_path" || ! -f "$transcript_path" ]]; then
  exit 0
fi

final_answer=$(jq -rs '
  [
    .[]
    | select(.payload.phase? == "final_answer")
    | if (.payload.message? // "") != "" then
        .payload.message
      elif (.payload.content? // null) != null then
        ([.payload.content[]? | .text? // empty] | join("\n"))
      else
        empty
      end
  ]
  | last // ""
' "$transcript_path" 2>/dev/null || true)

if [[ -z "$final_answer" ]]; then
  exit 0
fi

risky_claim_re='(^|[^[:alpha:]])(fixed|fix(ed)?|resolved|done|working|works now|now works|completed|complete)([^[:alpha:]]|$)'
verification_re='(Verification:|Not verified end-to-end:|verified end-to-end|e2e verified|not verified|could not verify|verification blocked|partial verification|tests? (pass|passed)|verified by reproducing|verified the reported|verified the exact)'

if grep -Eiq "$risky_claim_re" <<<"$final_answer" \
   && ! grep -Eiq "$verification_re" <<<"$final_answer"; then
  message="COMPLETION CLAIM GUARD: Final answer appears to claim completion/fix without an explicit verification receipt. Use a Verification: sentence for verified outcomes, or Not verified end-to-end: when only partial evidence exists."
  jq -n --arg message "$message" '{decision: "block", reason: $message}'
fi

exit 0
