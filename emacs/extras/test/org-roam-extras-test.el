;;; org-roam-extras-test.el --- Tests for org-roam-extras -*- lexical-binding: t -*-

;; Tests for org-roam node management, filtering, and statistics in org-roam-extras.el.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; org-roam-extras requires org-roam, which may not be available everywhere.
;; Guard the require and skip tests when the dependency is missing.
(require 'org-roam-extras nil t)

;; Declare dynamic variables used in tests to suppress byte-compiler warnings.
(defvar org-roam-directory)
(defvar org-roam-file-exclude-regexp)
(defvar paths-dir-journal)
(defvar paths-dir-dropbox)
(defvar paths-dir-notes)

;;;; node-include-function

(ert-deftest org-roam-extras-test-node-include-function-plain-heading ()
  "Return t for a normal heading with no exclusion criteria."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* Normal heading\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org")))
      (should (eq t (org-roam-extras-node-include-function))))))

(ert-deftest org-roam-extras-test-node-include-function-noid-tag ()
  "Return nil when the heading has the \"noid\" tag."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* Tagged heading  :noid:\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org")))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-archive-tag ()
  "Return nil when the heading has the \"ARCHIVE\" tag."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* Archived heading  :ARCHIVE:\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org")))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-local-variables ()
  "Return nil for the \"Local variables\" heading."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* Local variables\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org")))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-evaluation ()
  "Return nil for the \"Evaluation\" heading."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* Evaluation\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org")))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-history ()
  "Return nil for the \"History\" heading."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* History\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org")))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-further-reading ()
  "Return nil for the \"Further reading\" heading."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* Further reading\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org")))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-external-links ()
  "Return nil for the \"External links\" heading."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* External links\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org")))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-related-entries ()
  "Return nil for the \"Related entries\" heading."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* Related entries\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org")))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-archive-heading ()
  "Return nil for the \"Archive :ARCHIVE:\" heading."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* Archive :ARCHIVE:\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org"))
              ((symbol-function 'org-get-heading)
               (lambda (&rest _) "Archive :ARCHIVE:")))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-comment-local-variables ()
  "Return nil for the \"COMMENT Local variables\" heading."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* COMMENT Local variables\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org")))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-todo-local-variables ()
  "Return nil for the \"TODO Local variables\" heading."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Local variables\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org")))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-special-dir-level-2 ()
  "Return nil for level-2 headings in a special directory."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* Top heading\n** Sub heading\n")
    (goto-char (point-min))
    (forward-line 1)
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/journal/entry.org"))
              ((symbol-value 'paths-dir-journal) "/tmp/journal")
              ((symbol-value 'paths-dir-dropbox) "/tmp/dropbox")
              ((symbol-value 'paths-dir-notes) "/tmp/notes"))
      (should-not (org-roam-extras-node-include-function)))))

(ert-deftest org-roam-extras-test-node-include-function-special-dir-level-1 ()
  "Return t for level-1 headings even in a special directory."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* Top heading\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/journal/entry.org"))
              ((symbol-value 'paths-dir-journal) "/tmp/journal")
              ((symbol-value 'paths-dir-dropbox) "/tmp/dropbox")
              ((symbol-value 'paths-dir-notes) "/tmp/notes"))
      (should (eq t (org-roam-extras-node-include-function))))))

(ert-deftest org-roam-extras-test-node-include-function-non-special-dir-level-2 ()
  "Return t for level-2 headings in a non-special directory."
  (skip-unless (featurep 'org-roam))
  (with-temp-buffer
    (org-mode)
    (insert "* Top heading\n** Sub heading\n")
    (goto-char (point-min))
    (forward-line 1)
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/notes/test.org"))
              ((symbol-value 'paths-dir-journal) "/tmp/journal")
              ((symbol-value 'paths-dir-dropbox) "/tmp/dropbox")
              ((symbol-value 'paths-dir-notes) "/tmp/notes"))
      (should (eq t (org-roam-extras-node-include-function))))))

;;;; recent

(ert-deftest org-roam-extras-test-recent-filters-excluded ()
  "Filter out files matching `org-roam-file-exclude-regexp'."
  (skip-unless (featurep 'org-roam))
  (let ((org-roam-directory "/tmp/roam")
        (org-roam-file-exclude-regexp "excluded"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) "/tmp/roam/good.org\n/tmp/roam/excluded-file.org\n/tmp/roam/another.org\n")))
      (let ((result (org-roam-extras-recent 7)))
        (should (= (length result) 2))
        (should (member "/tmp/roam/good.org" result))
        (should (member "/tmp/roam/another.org" result))
        (should-not (member "/tmp/roam/excluded-file.org" result))))))

(ert-deftest org-roam-extras-test-recent-limit-under ()
  "Return the list when its length is under LIMIT."
  (skip-unless (featurep 'org-roam))
  (let ((org-roam-directory "/tmp/roam")
        (org-roam-file-exclude-regexp "NOMATCH"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) "/tmp/roam/a.org\n/tmp/roam/b.org\n")))
      (let ((result (org-roam-extras-recent 7 10)))
        (should (= (length result) 2))))))

(ert-deftest org-roam-extras-test-recent-limit-over ()
  "Return nil when the file count meets or exceeds LIMIT."
  (skip-unless (featurep 'org-roam))
  (let ((org-roam-directory "/tmp/roam")
        (org-roam-file-exclude-regexp "NOMATCH"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) "/tmp/roam/a.org\n/tmp/roam/b.org\n/tmp/roam/c.org\n")))
      (let ((result (org-roam-extras-recent 7 2)))
        (should-not result)))))

(ert-deftest org-roam-extras-test-recent-no-limit ()
  "Return all files when no LIMIT is given."
  (skip-unless (featurep 'org-roam))
  (let ((org-roam-directory "/tmp/roam")
        (org-roam-file-exclude-regexp "NOMATCH"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) "/tmp/roam/a.org\n/tmp/roam/b.org\n/tmp/roam/c.org\n")))
      (let ((result (org-roam-extras-recent 7)))
        (should (= (length result) 3))))))

;;;; count-todos-and-efforts

(ert-deftest org-roam-extras-test-count-todos-and-efforts-basic ()
  "Count open TODOs and sum their effort estimates."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query)
               ;; Two open TODOs with effort, one DONE (should be excluded)
               '(("TODO" (("EFFORT" . "1:30")))
                 ("TODO" (("EFFORT" . "0:30")))
                 ("DONE" (("EFFORT" . "3:00")))))))
    (let ((msg (org-roam-extras-count-todos-and-efforts)))
      (should (stringp msg))
      (should (string-match-p "2 open TODOs" msg))
      (should (string-match-p "2:00" msg)))))

(ert-deftest org-roam-extras-test-count-todos-and-efforts-no-effort ()
  "Count TODOs correctly when no effort is set."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query)
               '(("TODO" nil)
                 ("TODO" nil)))))
    (let ((msg (org-roam-extras-count-todos-and-efforts)))
      (should (string-match-p "2 open TODOs" msg))
      (should (string-match-p "0:00" msg)))))

(ert-deftest org-roam-extras-test-count-todos-and-efforts-single-todo ()
  "Use singular \"TODO\" in message for exactly one open TODO."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query)
               '(("TODO" (("EFFORT" . "0:45")))))))
    (let ((msg (org-roam-extras-count-todos-and-efforts)))
      (should (string-match-p "1 open TODO " msg))
      (should-not (string-match-p "1 open TODOs" msg))
      (should (string-match-p "0:45" msg)))))

(ert-deftest org-roam-extras-test-count-todos-and-efforts-excludes-done ()
  "Exclude DONE states from counting."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query)
               '(("DONE" (("EFFORT" . "1:00")))
                 ("DONE" (("EFFORT" . "2:00")))))))
    (let ((msg (org-roam-extras-count-todos-and-efforts)))
      (should (string-match-p "0 open TODOs" msg))
      (should (string-match-p "0:00" msg)))))

(ert-deftest org-roam-extras-test-count-todos-and-efforts-mixed-effort ()
  "Sum efforts from rows where only some have EFFORT set."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query)
               '(("TODO" (("EFFORT" . "1:00")))
                 ("TODO" nil)
                 ("TODO" (("EFFORT" . "0:15")))))))
    (let ((msg (org-roam-extras-count-todos-and-efforts)))
      (should (string-match-p "3 open TODOs" msg))
      (should (string-match-p "1:15" msg)))))

;;;; get-id-of-title

(ert-deftest org-roam-extras-test-get-id-of-title-single-match ()
  "Return the ID when exactly one node matches the title."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query _title)
               '(("abc-123" "/tmp/notes/foo.org")))))
    (should (equal (org-roam-extras-get-id-of-title "Foo") "abc-123"))))

(ert-deftest org-roam-extras-test-get-id-of-title-no-match ()
  "Signal an error when no node matches the title."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query _title) nil)))
    (should-error (org-roam-extras-get-id-of-title "Nonexistent")
                  :type 'user-error)))

(ert-deftest org-roam-extras-test-get-id-of-title-multiple-matches ()
  "Signal an error when multiple nodes share the title."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query _title)
               '(("id-1" "/tmp/notes/a.org")
                 ("id-2" "/tmp/notes/b.org")))))
    (should-error (org-roam-extras-get-id-of-title "Duplicate")
                  :type 'user-error)))

(ert-deftest org-roam-extras-test-get-id-of-title-dir-filter ()
  "Filter results by directory when DIRS is provided."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query _title)
               '(("id-1" "/tmp/notes/a.org")
                 ("id-2" "/tmp/other/b.org")))))
    ;; Only the entry in /tmp/notes/ should remain
    (should (equal (org-roam-extras-get-id-of-title "Title" nil '("/tmp/notes"))
                   "id-1"))))

(ert-deftest org-roam-extras-test-get-id-of-title-dir-filter-no-match ()
  "Signal an error when directory filter excludes all matches."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query _title)
               '(("id-1" "/tmp/other/a.org")))))
    (should-error (org-roam-extras-get-id-of-title "Title" nil '("/tmp/notes"))
                  :type 'user-error)))

(ert-deftest org-roam-extras-test-get-id-of-title-dir-filter-multiple-dirs ()
  "Accept matches from any directory in the DIRS list."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query _title)
               '(("id-1" "/tmp/notes/a.org")
                 ("id-2" "/tmp/other/b.org")
                 ("id-3" "/tmp/archive/c.org")))))
    ;; Two dirs: /tmp/notes and /tmp/archive; both matches should survive,
    ;; but that gives two results, so an error is signalled.
    (should-error (org-roam-extras-get-id-of-title "Title" nil '("/tmp/notes" "/tmp/archive"))
                  :type 'user-error)))

(ert-deftest org-roam-extras-test-get-id-of-title-dir-single-string ()
  "Accept a single directory string (not a list) for DIRS."
  (skip-unless (featurep 'org-roam))
  (cl-letf (((symbol-function 'org-roam-db-query)
             (lambda (_query _title)
               '(("id-1" "/tmp/notes/a.org")
                 ("id-2" "/tmp/other/b.org")))))
    (should (equal (org-roam-extras-get-id-of-title "Title" nil "/tmp/notes")
                   "id-1"))))

(provide 'org-roam-extras-test)
;;; org-roam-extras-test.el ends here
