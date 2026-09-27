#!/bin/bash
# lib-heredoc.sh — heredoc-aware masking for guards that classify command text.
#
# mask_heredoc_bodies COMMAND [MODE]
#   Prints COMMAND with heredoc body lines removed. A heredoc body is data for
#   the program that reads it, never command words of the outer shell, so a
#   guard that classifies command words (executable globs, protected tool
#   names, sensitive path mentions) must not read it as shell text. A body is
#   dropped only when the guard can prove it is inert data:
#     - the `<<` operator sits outside quotes,
#     - the receiving command is a known data sink (cat, tee, git, gh, ...),
#     - it is a plain simple command: no pipe after it on the operator line and
#       no command or process substitution around it.
#   Everything else keeps the body in the scan (fail closed): a heredoc fed to
#   bash, python, xargs, or an unknown program is a program, not data. An
#   unterminated heredoc is body to end of input, exactly as the shell reads it.
#   MODE "all" drops every heredoc body regardless of sink; guards use it to
#   tell "mentioned only inside an interpreter-fed heredoc" apart from
#   "mentioned as a command argument" when wording a denial.
#   MODE "nonshell" additionally drops bodies fed to known non-shell
#   interpreters (python, node, ruby, emacs, sqlite3, jq, ...). Their source is
#   never the outer shell's command words, so `?`, `*` and `[` in it are not
#   executable globs. Callers must still scan those bodies for protected tool
#   *names* in the default mode; only the shell-lexical rules use "nonshell".
#   MODE "network" buffers until EOF and drops only the body of a terminal,
#   quoted, simple cat write to a literal document pathname. It is solely an
#   activation projection: callers must scan original bytes for payloads.

mask_heredoc_bodies() {
  printf '%s\n' "$1" | awk -v mode="${2:-sinks}" '
    function neutralize(text,    out, i, n, c, sq, dq) {
      # Quoted spans and escaped characters can hold neither a control
      # operator nor a word break, so replace them before tokenizing: a `&`
      # inside a quoted URL or an escaped space in the program path must not
      # hide the command word that owns the heredoc.
      out = ""; sq = 0; dq = 0; n = length(text)
      for (i = 1; i <= n; i++) {
        c = substr(text, i, 1)
        if (sq) { out = out "x"; if (c == SQ) sq = 0; continue }
        if (dq) {
          if (c == BS) { out = out "xx"; i++; continue }
          out = out "x"; if (c == DQ) dq = 0; continue
        }
        if (c == BS) { out = out "xx"; i++; continue }
        if (c == SQ) { sq = 1; out = out "x"; continue }
        if (c == DQ) { dq = 1; out = out "x"; continue }
        out = out c
      }
      return out
    }
    function maskable(prefix, suffix,    seg, k, ntok, tok, word, p, owner) {
      if (mode == "all") return 1
      if (suffix ~ /[|(`]/) return 0
      seg = neutralize(prefix)
      # The simple command owning the heredoc starts after the last control
      # operator. Quoted separators earlier on the line only make the guard
      # keep the body, never drop it.
      while (match(seg, /[;|&]/)) seg = substr(seg, RSTART + 1)
      ntok = split(seg, tok, /[ \t]+/)
      word = ""
      for (k = 1; k <= ntok; k++) {
        if (tok[k] == "") continue
        if (tok[k] ~ /^[A-Za-z_][A-Za-z0-9_]*=/) continue
        if (tok[k] ~ /^(command|env|sudo|timeout|nice|exec|nohup|time|builtin)$/) continue
        # `pyenv exec PROGRAM` forwards stdin to PROGRAM, exactly as
        # lib-python-heredoc.py recognizes it; other pyenv subcommands are
        # the program themselves.
        if (tok[k] ~ /(^|\/)pyenv$/ && tok[k + 1] == "exec") { k++; continue }
        if (tok[k] ~ /^-/) continue
        if (tok[k] ~ /^[0-9]+[smhd]?$/) continue
        word = tok[k]; break
      }
      if (word == "") return 0
      p = word; sub(/.*\//, "", p)
      # A substitution in an earlier command on the line (`f=$(ls x) && python -`)
      # does not change who reads the body. For the glob rule alone, only the
      # owning command must be free of one; sinks keep the whole-line check,
      # since sink output inside `$(...)` becomes command words.
      owner = substr(prefix, length(prefix) - length(seg) + 1)
      if (mode != "nonshell" && prefix ~ /[(`]/) return 0
      if (owner ~ /[(`]/) return 0
      if (mode == "nonshell" && p ~ /^(python[0-9.]*|node|nodejs|deno|bun|ruby|perl|php|osascript|emacs|emacsclient|sqlite3|psql|mysql|jq|yq|Rscript|lua|luajit|swift|julia|g?awk|mawk|sed|bc|dc)$/) return 1
      if (prefix ~ /[(`]/) return 0
      if (mode == "network") return (prefix ~ /^[ \t]*cat[ \t]+>>?[ \t]*[A-Za-z0-9_.\/-]+\.(md|org|txt|json)[ \t]*$/ && suffix ~ /^[ \t]*$/)
      return (p in sinks)
    }
    BEGIN {
      SQ = sprintf("%c", 39); DQ = "\""; BS = "\\"
      nsinks = split("cat tee git gh head tail wc sort uniq grep rg diff cmp tr cut fold less more md5 md5sum shasum sha256sum column nl paste copy-slack-draft kill-ring-put", arr, " ")
      for (k = 1; k <= nsinks; k++) sinks[arr[k]] = 1
      in_sq = 0; in_dq = 0; nq = 0
    }
    {
      line = $0
      if (mode == "network") {
        original[NR] = line
        if (line !~ /^[ \t]*$/) last_nonempty = NR
      }
      if (nq > 0) {
        t = line
        if (stripq[1]) sub(/^\t+/, "", t)
        if (t == term[1]) {
          if (mode == "network" && drop[1]) { terminal_end = NR; terminal_id = identity[1] }
          for (k = 1; k < nq; k++) { term[k] = term[k + 1]; stripq[k] = stripq[k + 1]; drop[k] = drop[k + 1]; identity[k] = identity[k + 1] }
          nq--
          if (mode != "network") print line
        } else if (mode == "network") {
          if (drop[1]) masked[NR] = identity[1]
        } else if (!drop[1]) {
          print line
        }
        next
      }
      if (mode != "network") print line
      m = length(line); j = 1
      while (j <= m) {
        c = substr(line, j, 1)
        if (in_sq) { if (c == SQ) in_sq = 0; j++; continue }
        if (in_dq) { if (c == BS) { j += 2; continue } if (c == DQ) in_dq = 0; j++; continue }
        if (c == BS) { j += 2; continue }
        if (c == SQ) { in_sq = 1; j++; continue }
        if (c == DQ) { in_dq = 1; j++; continue }
        if (c == "<" && substr(line, j + 1, 1) == "<" && substr(line, j + 2, 1) != "<" && (j == 1 || substr(line, j - 1, 1) != "<")) {
          rest0 = substr(line, j + 2); rest = rest0; strip = 0
          if (substr(rest, 1, 1) == "-") { strip = 1; rest = substr(rest, 2) }
          sub(/^[ \t]*/, "", rest)
          word = ""; consumed = 0; quoted = 0
          if (match(rest, "^" SQ "[^" SQ "]*" SQ)) { word = substr(rest, 2, RLENGTH - 2); consumed = RLENGTH; quoted = 1 }
          else if (match(rest, /^"[^"]*"/)) { word = substr(rest, 2, RLENGTH - 2); consumed = RLENGTH; quoted = 1 }
          else if (match(rest, /^[A-Za-z_][A-Za-z0-9_.-]*/)) { word = substr(rest, 1, RLENGTH); consumed = RLENGTH }
          if (word == "") { j += 2; continue }
          nq++; term[nq] = word; stripq[nq] = strip; identity[nq] = ++sequence
          drop[nq] = maskable(substr(line, 1, j - 1), substr(rest, consumed + 1)) && (mode != "network" || (quoted && substr(original[NR - 1], length(original[NR - 1]), 1) != BS))
          j = j + 2 + (length(rest0) - length(rest)) + consumed
          continue
        }
        j++
      }
    }
    END {
      if (mode == "network") {
        for (i = 1; i <= NR; i++)
          if (!(terminal_end == last_nonempty && terminal_id && masked[i] == terminal_id)) print original[i]
      }
    }'
}

# mask_git_commit_messages COMMAND
#   Prints COMMAND with quoted `git commit -m` / `--message=` arguments replaced
#   by COMMIT_MESSAGE. The message is data git stores, never a file git reads,
#   so a sensitive path named inside it is an inert mention. Only a quoted
#   message that follows a `commit` word inside the same simple command is
#   masked, and only when it holds no command substitution; `less -m FILE`,
#   `git commit -F FILE` and `git commit -m "$(cat FILE)"` keep their text.
mask_git_commit_messages() {
  printf '%s' "$1" | perl -0pe '
    1 while s{(\bcommit\b[^;&|\n]*?\s(?:-[a-zA-Z]*m|--message)(?:=|\s+))(?:\x27[^\x27`]*\x27|"(?:[^"`\\\$]|\\.|\$(?!\())*")}{$1COMMIT_MESSAGE}
  '
}
