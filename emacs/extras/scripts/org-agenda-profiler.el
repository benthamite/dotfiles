;;; org-agenda-profiler.el --- Batch profiler for Org agenda scans -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Profile Org agenda file preparation in a separate batch Emacs process.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar org-agenda-profiler-default-top 20
  "Default number of slow files to show in the report.")

(defun org-agenda-profiler-main (&optional args)
  "Run the Org agenda profiler with command-line ARGS."
  (let* ((options (org-agenda-profiler-parse-args
                   (or args command-line-args-left))))
    (setq command-line-args-left nil)
    (let* ((entries (org-agenda-profiler-run options))
           (output (org-agenda-profiler-write-report entries options)))
      (message "org-agenda-profiler: wrote %s" output)
      output)))

(defun org-agenda-profiler-parse-args (args)
  "Parse command-line ARGS into a property list."
  (let ((args (cl-remove "--" args :test #'string=))
        (options (list :output nil
                       :limit nil
                       :top org-agenda-profiler-default-top
                       :manifest nil
                       :skip-dynamic-update nil)))
    (while args
      (let ((arg (pop args)))
        (pcase arg
          ("--output"
           (setq options (plist-put options :output
                                    (org-agenda-profiler-pop-value arg args)))
           (pop args))
          ("--limit"
           (setq options (plist-put options :limit
                                    (string-to-number
                                     (org-agenda-profiler-pop-value arg args))))
           (pop args))
          ("--top"
           (setq options (plist-put options :top
                                    (string-to-number
                                     (org-agenda-profiler-pop-value arg args))))
           (pop args))
          ("--manifest"
           (setq options (plist-put options :manifest
                                    (org-agenda-profiler-pop-value arg args)))
           (pop args))
          ("--skip-dynamic-update"
           (setq options (plist-put options :skip-dynamic-update t)))
          ("--help"
           (org-agenda-profiler-print-help)
           (kill-emacs 0))
          (_
           (user-error "Unknown org-agenda-profiler argument: %s" arg)))))
    options))

(defun org-agenda-profiler-pop-value (arg args)
  "Return the value following ARG in ARGS."
  (unless args
    (user-error "%s requires a value" arg))
  (car args))

(defun org-agenda-profiler-print-help ()
  "Print org-agenda-profiler usage."
  (princ
   (string-join
    '("Usage: org-agenda-profiler [OPTIONS]"
      ""
      "Options:"
      "  --output FILE          Write report to FILE."
      "  --limit N              Profile only the first N files."
      "  --top N                Show the N slowest files in the summary."
      "  --manifest FILE        Read agenda files from FILE, one per line."
      "  --skip-dynamic-update  Do not call vulpea agenda file refresh."
      "")
    "\n")))

(defun org-agenda-profiler-run (options)
  "Profile agenda file preparation according to OPTIONS."
  (org-agenda-profiler-add-profile-load-paths)
  (require 'org)
  (require 'org-agenda)
  (org-agenda-profiler-load-agenda-helpers)
  (let* ((files (org-agenda-profiler-files options))
         (limited (org-agenda-profiler-limit files
                                             (plist-get options :limit))))
    (message "org-agenda-profiler: profiling %d of %d files"
             (length limited) (length files))
    (mapcar #'org-agenda-profiler-profile-file limited)))

(defun org-agenda-profiler-add-profile-load-paths ()
  "Add active profile Elpaca build directories to `load-path'."
  (let ((builds-dir (expand-file-name "elpaca/builds" user-emacs-directory)))
    (when (file-directory-p builds-dir)
      (dolist (file (directory-files builds-dir t "\\`[^.]"))
        (when (file-directory-p file)
          (add-to-list 'load-path file t))))))

(defun org-agenda-profiler-load-agenda-helpers ()
  "Load optional helpers that own dynamic agenda file setup."
  (dolist (feature '(files-extras flycheck git-auto-commit-mode org-extras
                     org-roam org-roam-extras vulpea vulpea-extras))
    (condition-case err
        (require feature nil t)
      (error
       (message "org-agenda-profiler: could not load %s: %s"
                feature (error-message-string err))))))

(defun org-agenda-profiler-files (options)
  "Return agenda files selected by OPTIONS."
  (if-let* ((manifest (plist-get options :manifest)))
      (org-agenda-profiler-read-manifest manifest)
    (unless (plist-get options :skip-dynamic-update)
      (when (fboundp 'vulpea-extras-agenda-files-update)
        (vulpea-extras-agenda-files-update)))
    (org-agenda-files nil 'ifmode)))

(defun org-agenda-profiler-read-manifest (file)
  "Read agenda file names from manifest FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (cl-remove-if #'string-empty-p
                  (mapcar #'string-trim
                          (split-string (buffer-string) "\n")))))

(defun org-agenda-profiler-limit (files limit)
  "Return FILES truncated to LIMIT when LIMIT is non-nil."
  (if (and limit (> limit 0))
      (seq-take files limit)
    files))

(defun org-agenda-profiler-profile-file (file)
  "Return a profile entry for agenda preparation of FILE."
  (let* ((start (float-time))
         (already-open (org-agenda-profiler-file-open-p file))
         (status 'ok)
         (error-message nil)
         (new-buffers 0)
         (org-agenda-new-buffers nil))
    (unwind-protect
        (let ((debug-on-error nil))
          (condition-case err
              (progn
                (org-agenda-prepare-buffers (list file))
                (setq new-buffers (length org-agenda-new-buffers)))
            (error
             (setq status 'error)
             (setq error-message (error-message-string err)))))
      (when org-agenda-new-buffers
        (ignore-errors
          (org-release-buffers org-agenda-new-buffers))))
    (list :seconds (- (float-time) start)
          :status status
          :already-open already-open
          :new-buffers new-buffers
          :file (org-agenda-profiler-file-name file)
          :error error-message)))

(defun org-agenda-profiler-file-open-p (file)
  "Return non-nil when FILE is already visiting an Org buffer."
  (and (stringp file)
       (fboundp 'org-find-base-buffer-visiting)
       (org-find-base-buffer-visiting file)
       t))

(defun org-agenda-profiler-file-name (file)
  "Return display name for agenda FILE."
  (if (bufferp file)
      (buffer-name file)
    file))

(defun org-agenda-profiler-write-report (entries options)
  "Write profiling ENTRIES according to OPTIONS and return output file."
  (let ((output (or (plist-get options :output)
                    (org-agenda-profiler-default-output-file))))
    (with-temp-file output
      (insert (org-agenda-profiler-summary entries options))
      (insert "\nseconds\tstatus\talready_open\tnew_buffers\tfile\terror\n")
      (dolist (entry (org-agenda-profiler-sort-entries entries))
        (insert (org-agenda-profiler-format-entry entry) "\n")))
    output))

(defun org-agenda-profiler-default-output-file ()
  "Return a default profiler output file path."
  (expand-file-name
   (format "org-agenda-profile-%s.tsv" (format-time-string "%Y%m%d-%H%M%S"))
   (or (getenv "TMPDIR") temporary-file-directory)))

(defun org-agenda-profiler-summary (entries options)
  "Return summary text for profile ENTRIES using OPTIONS."
  (let* ((sorted (org-agenda-profiler-sort-entries entries))
         (top (or (plist-get options :top) org-agenda-profiler-default-top))
         (slowest (seq-take sorted top)))
    (concat
     (format "# generated_at\t%s\n" (format-time-string "%FT%T%z"))
     (format "# file_count\t%d\n" (length entries))
     (format "# total_seconds\t%.3f\n"
             (seq-reduce (lambda (total entry)
                           (+ total (plist-get entry :seconds)))
                         entries 0.0))
     "# slowest_files\n"
     (mapconcat #'org-agenda-profiler-format-entry slowest "\n")
     "\n")))

(defun org-agenda-profiler-sort-entries (entries)
  "Return ENTRIES sorted by descending elapsed time."
  (sort (copy-sequence entries)
        (lambda (a b)
          (> (plist-get a :seconds) (plist-get b :seconds)))))

(defun org-agenda-profiler-format-entry (entry)
  "Format a profile ENTRY as a tab-separated row."
  (format "%.3f\t%s\t%s\t%d\t%s\t%s"
          (plist-get entry :seconds)
          (plist-get entry :status)
          (plist-get entry :already-open)
          (or (plist-get entry :new-buffers) 0)
          (plist-get entry :file)
          (or (plist-get entry :error) "")))

(provide 'org-agenda-profiler)
;;; org-agenda-profiler.el ends here
