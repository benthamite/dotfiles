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

mask_heredoc_bodies() {
  printf '%s\n' "$1" | awk -v mode="${2:-sinks}" '
    function maskable(prefix, suffix,    seg, k, ntok, tok, word, p) {
      if (mode == "all") return 1
      if (prefix ~ /[(`]/ || suffix ~ /[|(`]/) return 0
      seg = prefix
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
        if (tok[k] ~ /^-/) continue
        if (tok[k] ~ /^[0-9]+[smhd]?$/) continue
        word = tok[k]; break
      }
      if (word == "") return 0
      p = word; sub(/.*\//, "", p)
      return (p in sinks)
    }
    BEGIN {
      SQ = sprintf("%c", 39); DQ = "\""; BS = "\\"
      nsinks = split("cat tee git gh head tail wc sort uniq grep rg diff cmp tr cut fold less more md5 md5sum shasum sha256sum column nl paste", arr, " ")
      for (k = 1; k <= nsinks; k++) sinks[arr[k]] = 1
      in_sq = 0; in_dq = 0; nq = 0
    }
    {
      line = $0
      if (nq > 0) {
        t = line
        if (stripq[1]) sub(/^\t+/, "", t)
        if (t == term[1]) {
          for (k = 1; k < nq; k++) { term[k] = term[k + 1]; stripq[k] = stripq[k + 1]; drop[k] = drop[k + 1] }
          nq--
          print line
        } else if (!drop[1]) {
          print line
        }
        next
      }
      print line
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
          word = ""; consumed = 0
          if (match(rest, "^" SQ "[^" SQ "]*" SQ)) { word = substr(rest, 2, RLENGTH - 2); consumed = RLENGTH }
          else if (match(rest, /^"[^"]*"/)) { word = substr(rest, 2, RLENGTH - 2); consumed = RLENGTH }
          else if (match(rest, /^[A-Za-z_][A-Za-z0-9_.-]*/)) { word = substr(rest, 1, RLENGTH); consumed = RLENGTH }
          if (word == "") { j += 2; continue }
          nq++; term[nq] = word; stripq[nq] = strip
          drop[nq] = maskable(substr(line, 1, j - 1), substr(rest, consumed + 1))
          j = j + 2 + (length(rest0) - length(rest)) + consumed
          continue
        }
        j++
      }
    }'
}
