#!/bin/bash
# PreToolUse hook: deny state-creating commands under ~/My Drive.
#
# Blocks dependency, build, cache, virtualenv, bytecode, and worktree state
# from being created inside the Google Drive sync root: npm ci/install and
# npm run build|dev|test, uv sync and project-bound uv run, python -m venv,
# every pip install form, Python without bytecode prevention, pytest without
# both bytecode and cache prevention, and git worktree add targeting Drive.
#
# ALL command parsing lives in lib/drive_runtime_command.py (the single
# parser); this script only extracts the payload, gates on a cheap masked
# keyword match, and maps the parser's segment verdicts to a decision. A
# command the parser cannot model is indeterminate and is DENIED — there is
# no silent allow fallback and no ask/approval response, ever.
#
# This file is byte-identical between claude/hooks/ and codex/hooks/ (a
# parity test enforces it), so it normalizes both payload shapes itself:
# Claude object tool_input with a top-level cwd, and Codex string-wrapped
# tool_input with a workdir field.
#
# Registration: Claude via the pretooluse-bash.sh dispatcher (delegated
# behind a cheap keyword gate); Codex via codex/hooks.json on the shell
# matcher (Bash|exec_command|functions.exec|functions.exec_command).

set -euo pipefail

INPUT=$(cat)

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')
case "$TOOL_NAME" in
  Bash|exec_command|functions.exec|functions.exec_command) : ;;
  *) exit 0 ;;
esac

COMMAND=$(printf '%s' "$INPUT" | jq -r '
  (.tool_input // {}) as $raw
  | (if ($raw | type) == "object" then $raw
     elif ($raw | type) == "string" then
       ((try ($raw | fromjson) catch {command: $raw})
        | if type == "object" then . else {command: $raw} end)
     else {} end) as $ti
  | ($ti.command // $ti.cmd // $ti.input // "")
  | if type == "array" then join(" ") else tostring end')
[ -z "$COMMAND" ] && exit 0

CWD=$(printf '%s' "$INPUT" | jq -r '
  (.tool_input // {}) as $raw
  | (if ($raw | type) == "object" then $raw
     elif ($raw | type) == "string" then (try ($raw | fromjson) catch {})
     else {} end) as $ti
  | (.cwd
     // (if ($ti | type) == "object" then ($ti.workdir // $ti.cwd) else null end)
     // empty)')
[ -n "$CWD" ] || CWD=$PWD

# --- quoted-content masking for the keyword gate -----------------------------
# The gate below matches command *words*, so string literals must not trigger
# it: a commit message mentioning `npm install` is not an npm install. This is
# the established destructive-command masking helper (source of truth:
# claude/hooks/block-destructive-command.sh mask_quoted); it blanks
# single-quoted, $'...', and $-and-backtick-free double-quoted spans plus
# quoted-delimiter heredoc bodies, keeps everything verbatim when a shell
# interpreter word is present (their quoted arguments ARE executed), and
# copies any unparseable remainder verbatim so masking can only suppress a
# quoted-literal match, never hide unquoted syntax. Masking is used ONLY for
# the gate; the parser always receives the raw command.
mask_quoted() {
  if printf '%s' "$COMMAND" | grep -qE '(^|[ \t|;&(/])(eval|bash|sh|zsh|dash|ksh|xargs|ssh)([ \t]|$)'; then
    printf '%s' "$COMMAND"
    return 0
  fi
  printf '%s\n' "$COMMAND" | awk '
    { lines[NR] = $0 }
    END {
      SQ = sprintf("%c", 39); DQ = "\""; BS = "\\"; NL = "\n"
      cmd = ""
      for (r = 1; r <= NR; r++) cmd = cmd lines[r] NL
      n = length(cmd); out = ""; i = 1
      nq = 0; qh = 1
      while (i <= n) {
        c = substr(cmd, i, 1)
        if (c == NL) {
          out = out c; i++
          # heredoc bodies queued on this line start after this newline
          while (qh <= nq) {
            delim = hqd[qh]; strip = hqs[qh]; maskbody = hqm[qh]
            found = 0; j = i
            while (j <= n) {
              k = index(substr(cmd, j), NL)
              lineend = j + k - 1
              linetxt = substr(cmd, j, lineend - j)
              test = linetxt
              if (strip) sub(/^\t+/, "", test)
              if (test == delim) { found = 1; break }
              j = lineend + 1
            }
            if (!found) { out = out substr(cmd, i); i = n + 1; qh = nq + 1; break }
            if (maskbody) out = out substr(cmd, j, lineend - j + 1)
            else out = out substr(cmd, i, lineend - i + 1)
            i = lineend + 1
            qh++
          }
          continue
        }
        if (c == BS) { out = out substr(cmd, i, 2); i += 2; continue }
        if (c == SQ) {
          p = index(substr(cmd, i + 1), SQ)
          if (p == 0) { out = out substr(cmd, i); break }
          out = out SQ SQ
          i = i + p + 1
          continue
        }
        if (c == DQ) {
          j = i + 1; span = ""; closed = 0
          while (j <= n) {
            d = substr(cmd, j, 1)
            if (d == BS) { span = span substr(cmd, j, 2); j += 2; continue }
            if (d == DQ) { closed = 1; break }
            span = span d; j++
          }
          if (!closed) { out = out substr(cmd, i); break }
          if (index(span, "$") > 0 || index(span, "`") > 0) out = out DQ span DQ
          else out = out DQ DQ
          i = j + 1
          continue
        }
        if (c == "$" && substr(cmd, i + 1, 1) == SQ) {
          j = i + 2; closed = 0
          while (j <= n) {
            d = substr(cmd, j, 1)
            if (d == BS) { j += 2; continue }
            if (d == SQ) { closed = 1; break }
            j++
          }
          if (!closed) { out = out substr(cmd, i); break }
          out = out "$" SQ SQ
          i = j + 1
          continue
        }
        if (c == "<" && substr(cmd, i + 1, 1) == "<" && substr(cmd, i + 2, 1) != "<") {
          j = i + 2; strip = 0
          if (substr(cmd, j, 1) == "-") { strip = 1; j++ }
          while (substr(cmd, j, 1) == " " || substr(cmd, j, 1) == "\t") j++
          d1 = substr(cmd, j, 1)
          if (d1 == SQ || d1 == DQ) {
            p = index(substr(cmd, j + 1), d1)
            if (p == 0) { out = out substr(cmd, i); break }
            delim = substr(cmd, j + 1, p - 1)
            nq++; hqd[nq] = delim; hqs[nq] = strip; hqm[nq] = 1
            out = out substr(cmd, i, (j + p) - i + 1)
            i = j + p + 1
            continue
          }
          k = j
          while (k <= n) {
            d = substr(cmd, k, 1)
            if (d == " " || d == "\t" || d == NL || d == ";" || d == "|" || d == "&" || d == "<" || d == ">" || d == "(" || d == ")") break
            k++
          }
          if (k > j) { delim = substr(cmd, j, k - j); nq++; hqd[nq] = delim; hqs[nq] = strip; hqm[nq] = 0 }
          out = out substr(cmd, i, k - i)
          i = k
          continue
        }
        out = out c; i++
      }
      printf "%s", out
    }'
}

# Cheap keyword gate: only commands that mention a state-creating command word
# in executable (non-quoted-prose) position reach the parser. This is a
# presence check, not parsing — classification is entirely the parser's job.
GATE='(^|[^[:alnum:]_-])(npm|npx|uv|pip[0-9.]*|python[0-9.]*|pytest|virtualenv)([^[:alnum:]_-]|$)|(^|[^[:alnum:]_-])worktree([^[:alnum:]_-]|$)'
SCAN=$(mask_quoted) || SCAN="$COMMAND"
[ -n "$SCAN" ] || SCAN="$COMMAND"
printf '%s' "$SCAN" | grep -qE "$GATE" || exit 0

REASON='Active dependency, build, cache, or worktree state is not allowed under ~/My Drive; migrate the repository or run the workflow from its approved external workspace.'

deny() {
  jq -n --arg r "$REASON" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": $r
    }
  }'
  exit 0
}

PARSER=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)/lib/drive_runtime_command.py
[ -r "$PARSER" ] || deny

SEGMENTS=$(python3 -I "$PARSER" --cwd "$CWD" --command "$COMMAND" 2>/dev/null) || deny
printf '%s' "$SEGMENTS" \
  | jq -e 'type == "array" and length > 0 and all(.[]; .verdict == "allow")' \
  >/dev/null 2>&1 || deny

exit 0
