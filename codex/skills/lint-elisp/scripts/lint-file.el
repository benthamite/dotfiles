;;; lint-file.el --- Report isolated compiler and Checkdoc diagnostics -*- lexical-binding: t; -*-

;;; Commentary:

;; Compilation can evaluate macros and requires.  This runner is not a sandbox.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'bytecomp)
(require 'checkdoc)

(defun lint-file--main ()
  "Run the literal command-line target and emit one JSON result."
  (let* ((args command-line-args-left)
         (target (cadr args))
         (compiler (lint-file--stage "not-run"))
         (checkdoc (lint-file--stage "not-run"))
         (cleanup '((completed . t) (retainedPath)))
         resolved before after scratch scratch-identity errors result)
    (setq command-line-args-left nil)
    (with-temp-buffer
      (let ((standard-output (current-buffer))
            (inhibit-message nil)
            (message-log-max nil)
            (enable-local-eval nil)
            (enable-local-variables nil)
            (enable-dir-local-variables nil)
            (native-comp-jit-compilation nil)
            (process-environment (copy-sequence process-environment)))
        (condition-case err
            (progn
              (unless (and (= (length args) 2) (equal (car args) "--"))
                (error "Expected -- followed by one absolute local .el target"))
              (setq resolved (lint-file--target target)
                    before (lint-file--digest resolved)
                    scratch (lint-file--scratch)
                    scratch-identity (lint-file--identity scratch))
              (setenv "EMACS_DYNVARS_FILE" nil)
              (setenv "EMACS_GENERATE_DYNVARS" nil)
              (let ((load-path (cons (file-name-directory resolved) load-path))
                    (load-prefer-newer nil)
                    (load-suffixes (cons ".el" (remove ".el" load-suffixes)))
                    (temporary-file-directory scratch))
                (setq compiler (lint-file--compile resolved scratch)
                      checkdoc (lint-file--checkdoc resolved)))
              (setq after (lint-file--digest resolved))
              (unless (and (equal resolved (file-truename target))
                           (equal before after))
                (push "Target changed during the checks" errors)))
          ((error quit) (push (error-message-string err) errors)))
        (when scratch
          (setq cleanup (lint-file--cleanup scratch scratch-identity)))))
    (setq result
          `((schemaVersion . 1) (emacsVersion . ,emacs-version)
            (compilerWarningPolicy . "all")
            (target . ,target) (resolvedTarget . ,resolved)
            (sha256Before . ,before) (sha256After . ,after)
            (sourceUnchanged . ,(if (and before after (equal before after)
                                        (null errors)) t :json-false))
            (compiler . ,compiler) (checkdoc . ,checkdoc)
            (errors . ,(vconcat (nreverse errors))) (cleanup . ,cleanup)))
    (princ (json-encode result))
    (terpri)
    (kill-emacs (lint-file--exit-code result))))

(defun lint-file--stage (status &optional diagnostics errors)
  "Build a stage result with STATUS, DIAGNOSTICS and ERRORS."
  `((completed . ,(if (equal status "complete") t :json-false))
    (status . ,status) (diagnostics . ,(vconcat (nreverse diagnostics)))
    (errors . ,(vconcat (nreverse errors)))))

(defun lint-file--target (target)
  "Validate TARGET and return its canonical local filename."
  (unless (and (stringp target) (file-name-absolute-p target)
               (not (file-remote-p target)) (string-suffix-p ".el" target)
               (file-regular-p target) (file-readable-p target))
    (error "Target must be one readable absolute local regular .el file"))
  (file-truename target))

(defun lint-file--digest (target)
  "Return the SHA256 of the literal bytes of TARGET."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally target)
    (secure-hash 'sha256 (current-buffer))))

(defun lint-file--scratch ()
  "Create a private compilation directory outside the Drive root."
  (let ((base (file-truename "/tmp/"))
        (drive (file-truename (expand-file-name "~/My Drive/"))))
    (when (or (equal (directory-file-name base) (directory-file-name drive))
              (file-in-directory-p base drive))
      (error "System temporary directory resolves inside Google Drive"))
    (make-temp-file (expand-file-name "lint-elisp-" base) t)))

(defun lint-file--identity (path)
  "Return the filesystem identity of PATH without following symlinks."
  (let ((attributes (file-attributes path 'integer)))
    (list (file-attribute-type attributes)
          (file-attribute-inode-number attributes)
          (file-attribute-device-number attributes))))

(defun lint-file--compile (target scratch)
  "Compile TARGET into SCRATCH and collect native diagnostics."
  (let* (diagnostics errors result
         (destination (expand-file-name "compiled.elc" scratch))
         (byte-compile-dest-file-function (lambda (_file) destination))
         (byte-compile-error-on-warn nil)
         (byte-compile-warnings 'all)
         (byte-compile-verbose nil)
         (byte-compile-log-warning-function
          (lambda (text position _fill level)
            (push `((severity . ,(if (eq level :error) "error" "warning"))
                    (message . ,text) (file . ,byte-compile-current-file)
                    (position . ,(and (integerp position) position)))
                  diagnostics))))
    (condition-case err
        (setq result (byte-compile-file target))
      ((error quit) (push (error-message-string err) errors)))
    (cond
     ((eq result 'no-byte-compile)
      (lint-file--stage "skipped" diagnostics '("File declares no-byte-compile")))
     ((or errors (not (eq result t))
          (cl-some (lambda (entry) (equal (alist-get 'severity entry) "error"))
                   diagnostics))
      (lint-file--stage "error" diagnostics
                       (or errors '("Compiler did not complete successfully"))))
     (t (lint-file--stage "complete" diagnostics)))))

(defun lint-file--checkdoc (target)
  "Run non-fixing, non-spelling Checkdoc directly on TARGET."
  (let (diagnostics errors)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents target)
          (setq buffer-file-name target
                default-directory (file-name-directory target))
          (delay-mode-hooks (emacs-lisp-mode))
          (let ((checkdoc-autofix-flag 'never)
                (checkdoc-spellcheck-documentation-flag nil)
                (checkdoc-generate-compile-warnings-flag nil)
                (checkdoc-diagnostic-buffer (generate-new-buffer " *lint-checkdoc*"))
                (checkdoc-create-error-function
                 (lambda (text start end &optional _unfixable)
                   (push `((severity . "note") (message . ,text)
                           (start . ,start) (end . ,end)) diagnostics)
                   nil)))
            (unwind-protect (checkdoc-current-buffer t)
              (kill-buffer checkdoc-diagnostic-buffer))))
      ((error quit) (push (error-message-string err) errors)))
    (lint-file--stage (if errors "error" "complete") diagnostics errors)))

(defun lint-file--cleanup (scratch identity)
  "Remove only expected output in SCRATCH if IDENTITY still matches."
  (condition-case err
      (let* ((output (expand-file-name "compiled.elc" scratch))
             (entries (directory-files scratch nil directory-files-no-dot-files-regexp)))
        (unless (and (equal identity (lint-file--identity scratch))
                     (or (null entries)
                         (and (equal entries '("compiled.elc"))
                              (not (file-symlink-p output))
                              (file-regular-p output))))
          (error "Unexpected temporary artifacts preserved"))
        (when entries (delete-file output))
        (delete-directory scratch)
        '((completed . t) (retainedPath)))
    (error `((completed . :json-false) (retainedPath . ,scratch)
             (error . ,(error-message-string err))))))

(defun lint-file--exit-code (result)
  "Return the process status for structured RESULT."
  (cond
   ((or (> (length (alist-get 'errors result)) 0)
        (not (eq (alist-get 'sourceUnchanged result) t))
        (not (eq (alist-get 'completed (alist-get 'cleanup result)) t))
        (cl-some (lambda (stage)
                   (not (eq (alist-get 'completed (alist-get stage result)) t)))
                 '(compiler checkdoc)))
    2)
   ((cl-some (lambda (stage)
               (> (length (alist-get 'diagnostics (alist-get stage result))) 0))
             '(compiler checkdoc))
    1)
   (t 0)))

(if noninteractive
    (lint-file--main)
  (user-error "Run this helper only in a fresh Emacs -Q --batch process"))

;;; lint-file.el ends here
