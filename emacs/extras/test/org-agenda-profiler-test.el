;;; org-agenda-profiler-test.el --- Tests for org agenda profiler -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-agenda-profiler)

(ert-deftest org-agenda-profiler-test-parse-args ()
  "Parse command-line arguments into profiler options."
  (should
   (equal
    (org-agenda-profiler-parse-args
     '("--output" "/tmp/agenda.tsv" "--limit" "12" "--top" "7"))
    '(:output "/tmp/agenda.tsv" :limit 12 :top 7 :manifest nil
      :skip-dynamic-update nil))))

(ert-deftest org-agenda-profiler-test-sort-entries ()
  "Sort profile entries by descending elapsed time."
  (let ((entries
         '((:file "fast.org" :seconds 0.1)
           (:file "slow.org" :seconds 2.0)
           (:file "medium.org" :seconds 1.0))))
    (should
     (equal
      (mapcar (lambda (entry) (plist-get entry :file))
              (org-agenda-profiler-sort-entries entries))
      '("slow.org" "medium.org" "fast.org")))))

(ert-deftest org-agenda-profiler-test-format-entry ()
  "Format a profile ENTRY as a tab-separated row."
  (should
   (equal
    (org-agenda-profiler-format-entry
     '(:seconds 1.23456 :status ok :already-open nil :new-buffers 2
       :file "/tmp/example.org" :error nil))
    "1.235\tok\tnil\t2\t/tmp/example.org\t")))

(ert-deftest org-agenda-profiler-test-add-profile-load-paths ()
  "Add Elpaca build directories from `user-emacs-directory'."
  (let* ((profile-dir (make-temp-file "org-agenda-profiler-profile" t))
         (builds-dir (expand-file-name "elpaca/builds" profile-dir))
         (pkg-dir (expand-file-name "example-package" builds-dir)))
    (unwind-protect
        (progn
          (make-directory pkg-dir t)
          (let ((user-emacs-directory (file-name-as-directory profile-dir))
                (load-path nil))
            (org-agenda-profiler-add-profile-load-paths)
            (should (member pkg-dir load-path))))
      (delete-directory profile-dir t))))

(ert-deftest org-agenda-profiler-test-empty-summary ()
  "Format an empty report summary."
  (should
   (string-match-p "# total_seconds\t0.000"
                   (org-agenda-profiler-summary nil '(:top 10)))))

(ert-deftest org-agenda-profiler-test-main-clears-command-line-args ()
  "Clear `command-line-args-left' after parsing profiler options."
  (let ((command-line-args-left '("--limit" "1")))
    (cl-letf (((symbol-function #'org-agenda-profiler-run)
               (lambda (_options) nil))
              ((symbol-function #'org-agenda-profiler-write-report)
               (lambda (_entries _options) "/tmp/profile.tsv")))
      (org-agenda-profiler-main)
      (should (null command-line-args-left)))))

(ert-deftest org-agenda-profiler-test-profile-file-catches-debug-errors ()
  "Record per-file errors when `debug-on-error' is non-nil."
  (let ((debug-on-error t))
    (cl-letf (((symbol-function #'org-agenda-prepare-buffers)
               (lambda (_files) (error "profile boom"))))
      (let ((entry (org-agenda-profiler-profile-file "/tmp/example.org")))
        (should (eq (plist-get entry :status) 'error))
        (should (string= (plist-get entry :error) "profile boom"))))))

(provide 'org-agenda-profiler-test)
;;; org-agenda-profiler-test.el ends here
