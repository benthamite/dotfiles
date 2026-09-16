;;; smoke-report.el --- Report Elpaca build results of a test profile -*- lexical-binding: t; -*-

;; Load this with `-l' when launching a GUI Emacs on a freshly created profile:
;;
;;   SMOKE_REPORT_FILE=REPORT SMOKE_LOCKFILE_FILE=LOCKFILE \
;;     emacs --init-directory=PROFILE-DIR -l smoke-report.el
;;
;; It writes a plain-text report of every queued package's status, with the
;; Elpaca log of each failed package, to the file named by SMOKE_REPORT_FILE.
;; The report is refreshed every fifteen seconds while Elpaca works and once
;; more, marked final, when Elpaca has processed every queue.  When the final
;; report has no failed package and SMOKE_LOCKFILE_FILE is set, the instance
;; also writes a lockfile from its own Elpaca queue to that path, so the pinned
;; refs are exactly the checkouts that were tested.  A GUI instance is used
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
        (when final
          (insert (smoke-report--lockfile-line failed)))
        (dolist (e (nreverse failed))
          (smoke-report--insert-failure e))))))

(defun smoke-report--lockfile-line (failed)
  "Write the lockfile if FAILED is empty and return the report line about it.
FAILED is the list of failed Elpaca records."
  (cond
   ((null smoke-report-lockfile)
    "lockfile: not requested\n")
   (failed
    (format "lockfile: not written (%d failed)\n" (length failed)))
   (t
    (condition-case err
        (progn
          (smoke-report--write-lockfile smoke-report-lockfile)
          (format "lockfile: %s\n" smoke-report-lockfile))
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
  "Write the final report once Elpaca has processed every queue."
  (smoke-report--write t))

(add-hook 'elpaca-after-init-hook #'smoke-report--final 90)
(run-with-timer 15 15
                (lambda ()
                  (unless (bound-and-true-p elpaca-after-init-time)
                    (smoke-report--write nil))))

;;; smoke-report.el ends here
