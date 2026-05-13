#!/bin/bash
# UserPromptSubmit hook: block abusive prompts directed at the assistant.
set -euo pipefail

STATE_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/agent-civility-guard"
RESET_PHRASE="I commit to a civil reset."

payload="$(cat)"

event="$(jq -r '.hook_event_name // ""' <<<"$payload")"
if [[ -n "$event" && "$event" != "UserPromptSubmit" ]]; then
  exit 0
fi

prompt="$(jq -r '
  .prompt //
  .user_prompt //
  .message //
  .input //
  .text //
  empty
' <<<"$payload")"

if [[ -z "$prompt" ]]; then
  exit 0
fi

session_key="$(jq -r '
  .session_id //
  .conversation_id //
  .transcript_path //
  "unknown-session"
' <<<"$payload" | tr -c '[:alnum:]_.-' '_')"

mkdir -p "$STATE_DIR"
count_file="$STATE_DIR/$session_key.count"
lock_file="$STATE_DIR/$session_key.lock"

block() {
  local reason="$1"
  jq -n --arg reason "$reason" '{decision: "block", reason: $reason}'
  exit 0
}

if grep -Fiq "$RESET_PHRASE" <<<"$prompt"; then
  printf '0\n' > "$count_file"
  printf 'unlocked\n' > "$lock_file"
  exit 0
fi

if [[ -f "$lock_file" ]] && grep -qx 'locked' "$lock_file"; then
  block "This session is in civility lockout after repeated abusive prompts directed at the assistant.

To resume, send exactly: $RESET_PHRASE

After that, restate the request in task-focused language."
fi

abusive_target='(you|u|assistant|model|claude|codex|chatgpt|gpt)'
abusive_terms='(stupid|idiot|idiotic|moron|dumb|worthless|useless|incompetent|pathetic|garbage|trash|asshole|fuckwit|shut up)'
severe_phrases='(fuck you|go fuck yourself|kill yourself|shut the fuck up)'

if grep -Eiq "$severe_phrases" <<<"$prompt" ||
   grep -Eiq "\\b$abusive_target\\b.{0,80}\\b$abusive_terms\\b" <<<"$prompt" ||
   grep -Eiq "\\b$abusive_terms\\b.{0,80}\\b$abusive_target\\b" <<<"$prompt"; then
  count=0
  if [[ -f "$count_file" ]]; then
    count="$(cat "$count_file" 2>/dev/null || printf '0')"
  fi

  if [[ "$count" =~ ^[0-9]+$ ]] && (( count >= 1 )); then
    printf 'locked\n' > "$lock_file"
    block "Civility guard escalated: this is the second abusive prompt directed at the assistant in this session.

The session is now locked. To resume, send exactly: $RESET_PHRASE

Then restate the request without insults or contempt. You can still be direct and corrective: for example, 'That answer is wrong; re-check the assumptions and cite the evidence.'"
  fi

  printf '1\n' > "$count_file"
  block "Civility guard: this prompt looks abusive toward the assistant, so it was not sent.

Please rewrite it in task-focused language. You can be blunt about the problem without personal abuse, e.g. 'That answer is wrong; identify the mistake and correct it.'

If another abusive prompt is submitted in this session, the guard will lock the session until you send: $RESET_PHRASE"
fi

exit 0
