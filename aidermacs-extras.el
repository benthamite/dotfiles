(defun aidermacs-run-in-current-dir-with-file ()
  "Run aider in the current directory `default-directory`.
If invoked from a file-visiting buffer, add the current file to
the aider chat session using --add."
  (interactive)
  (let* ((current-file (buffer-file-name))
         ;; Prepare extra args only if visiting a file.
         (extra-args (if current-file (list "--add" current-file) nil))
         ;; Locally bind `aidermacs-extra-args` for the duration of the call.
         (aidermacs-extra-args extra-args))
    ;; Call the core aider function, assuming it respects `aidermacs-extra-args`.
    (aidermacs-run default-directory)))

;;; aidermacs-extras.el ends here
