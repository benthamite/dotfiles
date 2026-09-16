;;; smoke-report.el --- Report Elpaca build results of a test profile -*- lexical-binding: t; -*-

;; Load this with `-l' when launching a GUI Emacs on a freshly created profile:
;;
;;   SMOKE_REPORT_FILE=REPORT SMOKE_LOCKFILE_FILE=LOCKFILE \
;;     emacs --init-directory=PROFILE-DIR -l smoke-report.el
;;
;; It writes a plain-text report of every queued package's status, with the
;; Elpaca log of each failed package, to the file named by SMOKE_REPORT_FILE.
;; The report is refreshed every fifteen seconds while Elpaca works and once
;; more, marked final, when Elpaca has processed every queue.  The final report
;; also copies the `*Warnings*' buffer and every error line of `*Messages*':
;; a package can build under Elpaca and still break at load time, for example
;; through an autoload cookie on a form that reads an unbound variable, and
;; Elpaca reports that only as a startup warning.
;;
;; When the final report has no failed package, no warning and no error message,
;; and SMOKE_LOCKFILE_FILE is set, the instance writes a lockfile from its own
;; Elpaca queue to that path, so the pinned refs are exactly the checkouts that
;; were tested.  Set SMOKE_ALLOW_WARNINGS=1 to write the lockfile despite
;; warnings that were investigated and judged harmless.  A GUI instance is used
;; deliberately: a batch Emacs exits at the first process-sentinel error and
;; hides every other failure.

(declare-function elpaca--queued "elpaca" (&optional n))
(declare-function elpaca--status "elpaca" (e))
(declare-function elpaca<-id "elpaca" (e))
(declare-function elpaca<-log "elpaca" (e))
(declare-function elpaca<-source-dir "elpaca" (e))
(declare-function elpaca-write-lock-file "elpaca" (path &optional elpacas))
(declare-function elpaca-extras-write-lock-file-excluding "elpaca-extras"
                  (path &optional elpacas))
(defvar elpaca-after-init-time)

(defvar smoke-report-file (getenv "SMOKE_REPORT_FILE")
  "File that receives the status report, or nil to report nothing.")

(defvar smoke-report-lockfile (getenv "SMOKE_LOCKFILE_FILE")
  "File that receives the lockfile of a clean final run, or nil to skip it.")

(defvar smoke-report-allow-warnings (getenv "SMOKE_ALLOW_WARNINGS")
  "Non-nil to write the lockfile even when warnings or error messages exist.")

(defun smoke-report--write (final)
  "Write the current Elpaca status report.
FINAL non-nil marks the report as written after queue completion."
  (when (and smoke-report-file (fboundp 'elpaca--queued))
    (let ((failed nil) (counts (make-hash-table :test #'eq))
          (warnings (smoke-report--warnings))
          (errors (smoke-report--message-errors)))
      (dolist (cell (elpaca--queued))
        (let* ((e (cdr cell)) (status (elpaca--status e)))
          (puthash status (1+ (gethash status counts 0)) counts)
          (when (eq status 'failed) (push e failed))))
      (with-temp-file smoke-report-file
        (insert (format "state: %s\n" (if final "final" "in-progress")))
        (insert (format "time: %s\n" (format-time-string "%F %T")))
        (maphash (lambda (k v) (insert (format "count %s: %d\n" k v))) counts)
        (insert (format "count warnings: %d\n" (length warnings)))
        (insert (format "count message-errors: %d\n" (length errors)))
        (when final
          (insert (smoke-report--lockfile-line failed warnings errors)))
        (dolist (e (nreverse failed))
          (smoke-report--insert-failure e))
        (dolist (w warnings) (insert (format "WARNING %s\n" w)))
        (dolist (m errors) (insert (format "MESSAGE-ERROR %s\n" m)))))))

(defun smoke-report--warnings ()
  "Return the non-empty lines of the `*Warnings*' buffer, or nil."
  (let ((buffer (get-buffer "*Warnings*")))
    (when buffer
      (with-current-buffer buffer
        (seq-remove #'string-empty-p
                    (split-string (buffer-substring-no-properties
                                   (point-min) (point-max))
                                  "\n"))))))

(defun smoke-report--message-errors ()
  "Return the lines of `*Messages*' that report an error, or nil.
Elpaca's own per-package logs are not messages, so a line here comes from
init code, a hook, a timer or a process sentinel."
  (let ((buffer (get-buffer "*Messages*")))
    (when buffer
      (with-current-buffer buffer
        (seq-filter (lambda (line)
                      (string-match-p "\\`\\(Error\\|error\\|Warning\\)\\b\\|error in process\\|\\bError:\\|\\berror:" line))
                    (split-string (buffer-substring-no-properties
                                   (point-min) (point-max))
                                  "\n"))))))

(defun smoke-report--lockfile-line (failed warnings errors)
  "Write the lockfile when the run is clean and return the report line about it.
FAILED is the list of failed Elpaca records, WARNINGS the `*Warnings*' lines
and ERRORS the error lines of `*Messages*'."
  (cond
   ((null smoke-report-lockfile)
    "lockfile: not requested\n")
   (failed
    (format "lockfile: not written (%d failed)\n" (length failed)))
   ((and (or warnings errors) (not smoke-report-allow-warnings))
    (format "lockfile: not written (%d warnings, %d error messages; set SMOKE_ALLOW_WARNINGS=1 after investigating them)\n"
            (length warnings) (length errors)))
   (t
    (condition-case err
        (progn
          (smoke-report--write-lockfile smoke-report-lockfile)
          (format "lockfile: %s%s\n" smoke-report-lockfile
                  (if (or warnings errors) " (warnings allowed)" "")))
      (error (format "lockfile: error %s\n" (error-message-string err)))))))

(defun smoke-report--write-lockfile (path)
  "Write this instance's Elpaca queue as a lockfile to PATH.
Use the dotfiles writer, which honours the exclusion list, when it is loaded."
  (if (fboundp 'elpaca-extras-write-lock-file-excluding)
      (elpaca-extras-write-lock-file-excluding path)
    (elpaca-write-lock-file path)))

(defun smoke-report--insert-failure (e)
  "Insert the identity and Elpaca log of failed package E."
  (insert (format "FAILED %s src=%s\n" (elpaca<-id e) (elpaca<-source-dir e)))
  (dolist (entry (reverse (elpaca<-log e)))
    (let ((text (format "%s" (nth 2 entry))))
      (unless (string-match-p "Continued by" text)
        (insert (format "  %s | %s\n" (nth 0 entry)
                        (substring-no-properties text)))))))

(defun smoke-report--final ()
  "Write the final report once Elpaca has processed every queue.
Wait a few seconds first so that warnings raised by deferred startup code
appear in the report."
  (run-with-timer 10 nil (lambda () (smoke-report--write t))))

(add-hook 'elpaca-after-init-hook #'smoke-report--final 90)
(run-with-timer 15 15
                (lambda ()
                  (unless (bound-and-true-p elpaca-after-init-time)
                    (smoke-report--write nil))))

;;; smoke-report.el ends here
