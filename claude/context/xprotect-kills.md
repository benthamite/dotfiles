# XProtect kills of Emacs and agent sessions

Since macOS 26.4, `xprotectd` watches scripted commands. It reads host-like words in a command line as hosts, checks them against Apple's website-reputation list (Safe Browsing), and on a bad verdict kills the whole process tree it holds responsible (its "container"). For a session inside Emacs that tree is Emacs itself, with every agent session in it. A file name can be a host name: `sites.rs`, or any `NAME.org` note.

**Symptom.** Emacs (or a CLI session in a terminal) vanishes: no crash report in `~/Library/Logs/DiagnosticReports`, `kill-emacs-hook` does not run, sometimes a "Malicious Script Blocked" dialog. It is not a crash and not a freeze.

**Check.** The unified log names the kill, in the same tenth of a second as the exit:

    /usr/bin/log show --start "YYYY-MM-DD HH:MM:SS" --end "YYYY-MM-DD HH:MM:SS" --style compact \
      --predicate 'process == "xprotectd" AND eventMessage CONTAINS "Killing container"'

`~/Library/Logs/emacs-server-watchdog.samples.jsonl` gives the minute Emacs's pid changed.

**Which name.** The log redacts the rule. The block history does not: `historic_malware_blocks` in `/var/db/SystemPolicyConfiguration/ExecPolicy` (root only; Pablo runs `sudo sqlite3 -readonly`) holds one row per blocked item with its latest time. A key `url-<hex>` is the SHA-256 of `http://HOST` or `http://HOST/`; `paste-<hex>` is a blocked paste. Hash the candidate names (file names and their dotted suffixes in the repositories the commands of that minute touched) and compare. Do not type a suspected name into a shell command while testing: that is the trigger.

**Known names.** The names found so far are in the private note `~/repos/notes/mac-hardening/xprotect-kills.md`, with what is still unexplained. Record new ones there: this repository is public.

**It does not fire every time.** The same command line can pass dozens of times and be killed once; four reruns of the 23 Nov replay in a throwaway Emacs were not killed. A clean rerun proves nothing.

**Avoiding it.**
- Keep long lists of file names off command lines: one search process over a directory (`rg PATTERN DIR`, `git grep`), a glob pathspec (`':(glob)**/*.org'`), or names on stdin. The agent-guard policy `emacs_tree_traversal` blocks the shell shapes that expand to more than 500 names.
- Give a file whose name is a listed host a name that cannot be a host (an underscore is enough).

Sources: Claude Code issue 91447 (anthropics/claude-code), hunk pull request 1140 (modem-dev/hunk), and the 23 Nov 2026 diagnosis (session `1b9e6121`, Protean log of that date).
