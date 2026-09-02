---
name: paste-via-kill-ring
description: Hand Pablo text he must paste himself — a message, credential, URL, or code snippet — by placing it on the Emacs kill ring, and on the macOS clipboard when the target is Chrome or a native form. Use whenever you would otherwise print something for manual copying.
---

# Paste via the Emacs kill ring

Never print text for Pablo to copy by hand. Put it where `C-y` or `Cmd+V`
will find it, then tell him in one line what is there and where to paste it.
Do not repeat the content in your reply.

## Procedure

1. **Single-line, non-secret text.** Run
   `emacsclient -e '(kill-new "TEXT")'` with the string escaped for Elisp.
2. **Multi-line or secret text.** Write it to a temp file created with mode
   600, then run
   `emacsclient -e '(with-temp-buffer (insert-file-contents "FILE") (kill-new (buffer-string)))'`
   and delete the file. This avoids Elisp string-escaping problems and keeps
   the secret off the command line.
3. **Chrome or native macOS form as the paste target (`Cmd+V`).** Also copy
   to the system clipboard with `pbcopy`, feeding it from the same temp file;
   never echo a secret into the command.
4. **Message for Slack, email, or a comment thread.** Also open the relevant
   thread in Emacs so the paste lands in place.
