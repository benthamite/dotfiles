;;; smoke-report.el --- Report Elpaca build results of a test profile -*- lexical-binding: t; -*-

;; Load this with `-l' when launching a GUI Emacs on a freshly created profile:
;;
;;   SMOKE_REPORT_FILE=REPORT emacs --init-directory=PROFILE-DIR -l smoke-report.el
;;
;; It writes a plain-text report of every queued package's status, with the
;; Elpaca log of each failed package, to the file named by SMOKE_REPORT_FILE.
;; The report is refreshed every fifteen seconds while Elpaca works and once
;; more, marked final, when Elpaca has processed every queue.  A GUI instance is
;; used deliberately: a batch Emacs exits at the first process-sentinel error
;; and hides every other failure.

(declare-function elpaca--queued "elpaca" (&optional n))
(declare-function elpaca--status "elpaca" (e))
(declare-function elpaca<-id "elpaca" (e))
(declare-function elpaca<-log "elpaca" (e))
(declare-function elpaca<-source-dir "elpaca" (e))
(defvar elpaca-after-init-time)

(defvar smoke-report-file (getenv "SMOKE_REPORT_FILE")
  "File that receives the status report, or nil to report nothing.")

(defun smoke-report--write (final)
  "Write the current Elpaca status report.
FINAL non-nil marks the report as written after queue completion."
  (when (and smoke-report-file (fboundp 'elpaca--queued))
    (let ((failed nil) (counts (make-hash-table :test #'eq)))
      (dolist (cell (elpaca--queued))
        (let* ((e (cdr cell)) (status (elpaca--status e)))
          (puthash status (1+ (gethash status counts 0)) counts)
          (when (eq status 'failed) (push e failed))))
      (with-temp-file smoke-report-file
        (insert (format "state: %s\n" (if final "final" "in-progress")))
        (insert (format "time: %s\n" (format-time-string "%F %T")))
        (maphash (lambda (k v) (insert (format "count %s: %d\n" k v))) counts)
        (dolist (e (nreverse failed))
          (smoke-report--insert-failure e))))))

(defun smoke-report--insert-failure (e)
  "Insert the identity and Elpaca log of failed package E."
  (insert (format "FAILED %s src=%s\n" (elpaca<-id e) (elpaca<-source-dir e)))
  (dolist (entry (reverse (elpaca<-log e)))
    (let ((text (format "%s" (nth 2 entry))))
      (unless (string-match-p "Continued by" text)
        (insert (format "  %s | %s\n" (nth 0 entry)
                        (substring-no-properties text)))))))

(defun smoke-report--final ()
  "Write the final report once Elpaca has processed every queue."
  (smoke-report--write t))

(add-hook 'elpaca-after-init-hook #'smoke-report--final 90)
(run-with-timer 15 15
                (lambda ()
                  (unless (bound-and-true-p elpaca-after-init-time)
                    (smoke-report--write nil))))

;;; smoke-report.el ends here
