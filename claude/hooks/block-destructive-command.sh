#!/bin/bash
# PreToolUse hook: block destructive shell commands that are hard to reverse.
#
# Enforces CLAUDE.md's "use trash instead of rm -rf" instruction with code,
# not just prose.
#
# Matcher: Bash

set -euo pipefail

INPUT=$(cat)

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')
[ "$TOOL_NAME" != "Bash" ] && exit 0

CMD=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
[ -z "$CMD" ] && exit 0

deny() {
  local label="$1"
  local suggestion="$2"
  jq -n --arg label "$label" --arg suggestion "$suggestion" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: " + $label + ".\n\n" + $suggestion + "\n\nIf you are certain this is the right action, ask the user to run it manually with `!`.")
    }
  }'
  exit 0
}

ask() {
  local label="$1"
  local suggestion="$2"
  jq -n --arg label "$label" --arg suggestion "$suggestion" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "ask",
      "permissionDecisionReason": ($label + ". " + $suggestion)
    }
  }'
  exit 0
}

# --- quoted-content masking --------------------------------------------------
# The checks below match shell *syntax*, so string literals must not trigger
# them: a commit message mentioning a force push is not a force push. Three
# real false positives motivated this: a force-push mention quoted in a commit
# message, doc prose containing the rm-rf literal inside a quoted python
# heredoc, and a grep pattern argument. mask_quoted blanks the contents of
# single-quoted, $'...', and $-and-backtick-free double-quoted strings, plus
# quoted-delimiter heredoc bodies; expansions, unquoted syntax, and
# unquoted-delimiter heredoc bodies (which expand) stay verbatim.
#
# Safety properties (masking must never weaken the guard):
# - Masking is SKIPPED when the command mentions a shell-interpreter word
#   (eval, bash, sh, zsh, dash, ksh, xargs, ssh): their quoted arguments and
#   heredoc bodies ARE executed (bash -c "...", eval "...", echo "..." | bash),
#   so those commands keep strict raw matching.
# - Any parse uncertainty (unterminated quote or heredoc) copies the remainder
#   verbatim: masking can only suppress a quoted-literal match, never hide
#   unquoted syntax.
# - Double-quoted spans containing $ or a backtick are kept verbatim (command
#   substitution inside them executes).
# Quoted flags (git push "--force") were never caught by the raw patterns
# either, so masking them away is not a regression.
mask_quoted() {
  if printf '%s' "$CMD" | grep -qE '(^|[ \t|;&(/])(eval|bash|sh|zsh|dash|ksh|xargs|ssh)([ \t]|$)'; then
    printf '%s' "$CMD"
    return 0
  fi
  printf '%s\n' "$CMD" | awk '
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
SCAN=$(mask_quoted) || SCAN="$CMD"
[ -n "$SCAN" ] || SCAN="$CMD"

# --- rm -rf / rm -r (should use trash) ---
if echo "$SCAN" | grep -qE '\brm\s+(-[a-zA-Z]*r[a-zA-Z]*f|(-[a-zA-Z]*f[a-zA-Z]*r)|-rf|-fr)\b'; then
  deny "rm -rf detected" "Use 'trash' instead of 'rm -rf' to allow recovery."
fi

# --- git push --force / -f (dangerous to shared branches) ---
# --force-with-lease is the safe form (per CLAUDE.md, it's medium-risk on a
# verified user-owned feature branch) — allow it through.
# The -f/--force flag must appear WITHIN the git push invocation (before the
# next command separator), not merely anywhere in a compound command — a
# `[ -f file ]` test elsewhere in the string used to false-positive this.
if echo "$SCAN" | grep -qE '\bgit\s+push[^|;&]*(\s-f\b|\s--force\b)' && \
   ! echo "$SCAN" | grep -qE '\bgit\s+push[^|;&]*--force-with-lease'; then
  deny "git push --force detected" "Force-pushing can overwrite upstream history. Use --force-with-lease for branch-scoped pushes, or confirm with the user."
fi

# --- git clone (prevent cloning without approval) ---
if echo "$SCAN" | grep -qE '\b(git\s+clone|gh\s+repo\s+clone)\b'; then
  ask "git clone detected" "Only clone repositories the user has explicitly requested."
fi

# --- git reset --hard ---
if echo "$SCAN" | grep -qE '\bgit\s+reset\s+--hard\b'; then
  deny "git reset --hard detected" "This discards uncommitted changes irreversibly. Consider 'git stash' instead."
fi

# --- git clean -f ---
if echo "$SCAN" | grep -qE '\bgit\s+clean\s+.*-[a-zA-Z]*f'; then
  deny "git clean -f detected" "This permanently deletes untracked files. Review 'git clean -n' first."
fi

# --- git checkout -- . (discard all changes) ---
if echo "$SCAN" | grep -qE '\bgit\s+checkout\s+--\s+\.'; then
  deny "git checkout -- . detected" "This discards all unstaged changes. Consider 'git stash' instead."
fi

# --- git branch -D (force delete) ---
if echo "$SCAN" | grep -qE '\bgit\s+branch\s+-D\b'; then
  deny "git branch -D detected" "Force-deleting a branch can lose unmerged work. Use -d for safe delete."
fi

# --- GitHub repo visibility change (destroys stars and watchers) ---
if echo "$SCAN" | grep -qE '\bgh\s+api\s+.*repos/.*visibility|gh\s+repo\s+edit\s+.*--visibility'; then
  deny "GitHub repo visibility change detected" "Toggling a repo private permanently destroys all stars and watchers. NEVER do this."
fi

# --- gh repo delete (irreversible) ---
if echo "$SCAN" | grep -qE '\bgh\s+repo\s+delete\b'; then
  deny "gh repo delete detected" "Deleting a GitHub repo is irreversible and destroys all stars, forks, and history. Confirm with the user first."
fi

# --- dropdb (PostgreSQL/MySQL database removal) ---
if echo "$SCAN" | grep -qE '\bdropdb\b'; then
  deny "dropdb detected" "Dropping a database is irreversible. Confirm with the user first."
fi

# --- bq rm (BigQuery dataset/table removal) ---
if echo "$SCAN" | grep -qE '\bbq\s+rm\b'; then
  deny "bq rm detected" "BigQuery rm is irreversible. Confirm with the user first."
fi

# --- aws s3 rm --recursive ---
if echo "$SCAN" | grep -qE '\baws\s+s3\s+rm\b' && \
   echo "$SCAN" | grep -qE '(--recursive\b|\s-r\b)'; then
  deny "aws s3 rm --recursive detected" "Recursive S3 deletion is irreversible. Confirm with the user first."
fi

# --- op item delete (1Password) ---
if echo "$SCAN" | grep -qE '\bop\s+item\s+(delete|remove)\b'; then
  deny "op item delete detected" "Deleting a 1Password item is irreversible. Confirm with the user first."
fi

exit 0
