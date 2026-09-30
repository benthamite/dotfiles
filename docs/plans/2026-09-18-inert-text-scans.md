# Text-scan guards and commit parsers: prose is not a command

Decision record, 2026-09-18. Follows the learning-inbox guard triage of the
same day (private tables under `.agent-learnings/reviews/2026-09-18-guard-cluster/`).

## Cluster 1: three guards read prose as commands

The GitHub write guard, the Ahrefs guard and the walk-list guard grepped the
raw command text. A commit message saying "git push runs from CI now", a
heredoc writing a note that mentions `gh pr create`, or a message naming
`api.ahrefs.com` was denied as a write, a raw API call, or a store access.
31 inbox candidates for the GitHub guard alone, from 2026-06 to 2026-09.

The masking already existed: `lib-heredoc.sh` gives `mask_heredoc_bodies`
(drops heredoc bodies fed to known data sinks; keeps bodies fed to shells,
interpreters or anything ambiguous) and `mask_git_commit_messages` (drops a
quoted `-m` message after a `commit` word in the same simple command). The
sensitive-read guard adopted the first on 2026-09-02 (`20df5b4a5`) and the
second on 2026-09-18 (`fe411b79c`); the secret-leak guard adopted sink masking
for its tool-name rule. These three guards never did.

Decision: each guard computes a scan text with both masks applied and runs its
*detection* greps on that text. Target extraction, compound detection and the
dry-run shortcut keep reading the raw command. Consequences kept deliberately:

- `bash <<EOF … git push … EOF` and any heredoc whose operator line carries a
  substitution stay in the scan and are denied (lib-heredoc fails closed).
- A write in a compound command is still denied (0ff426162, 2026-09-16).
- A heredoc naming the Ahrefs host that is fed to an interpreter is still a
  raw call.

A pre-existing gap surfaced by the new negative controls: the detectors
require a separator before `git`/`gh`, so `sh -c 'git push …'` was never
detected by either copy, and the Claude dispatcher's prefilter did not even
delegate it. Fixed in the same change: the scan text drops the quote after a
shell's `-c`, and the dispatcher treats a quote as a separator when deciding
whether to delegate. The dispatcher prefilter itself had been the larger hole
(`git -C DIR push` and `/usr/bin/git push` never reached the guard), fixed in
`bf1a6d20` earlier the same day.

## Cluster 2: commit-selection parsers read redirections as pathspecs

`commit-file-selection.py` (2026-09-04, `9b0fb5364`) lexes the command with
`shlex` and punctuation `;&|\n<`. `>` is not punctuation, so `git commit -m x
2>&1 | tail` produced the tokens `2>`, `&`, `1`, and `2>` became a pathspec,
failing with "no tracked files match the commit selection" three times over
(once per delegating commit gate). The same class had been fixed in
`ai-config-sync` on 2026-08-04 and in the shell hooks on 2026-08-02; the new
helper reintroduced it with its own lexer. Separately, the Codex invocation
classifier hands the helper `<<EOF` as one token, which the `<<` branch did not
recognise, and `ai-config-sync` refused the `-qm` cluster the helper accepts.

Decision: the helper skips redirection operators and their targets (`2>&1`
lexes as `2>` `&` `1`), accepts a joined `<<DELIM`, and prefixes its errors
with `BLOCKED:` so the dispatcher's concatenated reasons read as denials. It
also looks for the heredoc operator on quote-masked text, because a commit
message that merely mentions `<<EOF` used to swallow the rest of the command
and fail with "No closing quotation" (the first attempt to commit this very
change tripped it); the delimiter itself is read from the original line, so
`<<'EOF'` still terminates on `EOF`. `require-doc-update.sh` carries its own
copy of that stripper, with the same defect and a worse failure mode: the
unlexable remainder was reported as staged Elisp ("Elisp files are staged but
no doc/*.org file is included"). Fixed the same way in both copies.
`ai-config-sync` accepts `-[qsv]+m MSG`, `-[qsv]+mMSG` and neutral `-[qsv]+`
clusters. A real pathspec after `--` is still read (`> /dev/null -- README.org`).

## Verification

`tests/test_inert_text_scans.py`: all three guard families, all copies plus
the dispatcher, positive and negative controls; the selection helper against a
temporary repository for six redirect and heredoc shapes; `ai-config-sync`'s
parser for the clusters. Live: this record's own commit message names a push.
