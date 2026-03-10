;;; vulpea-extras-test.el --- Tests for vulpea-extras -*- lexical-binding: t -*-

;; Tests for path exclusion logic and buffer classification
;; in vulpea-extras.el.

;;; Code:

(require 'ert)
(require 'vulpea-extras)

;; Ensure org-roam-directory is globally bound so `boundp' returns t
;; inside let-bindings (vulpea-extras-buffer-p checks boundp).
(defvar org-roam-directory nil)

;;;; file-in-excluded-directory-p

(ert-deftest vulpea-extras-test-file-in-excluded-directory ()
  "Return non-nil when file is in an excluded directory."
  (let* ((tmpdir (make-temp-file "vulpea-test-" t))
         (vulpea-extras-excluded-directories (list tmpdir)))
    (unwind-protect
        (should (vulpea-extras-file-in-excluded-directory-p
                 (expand-file-name "notes.org" tmpdir)))
      (delete-directory tmpdir t))))

(ert-deftest vulpea-extras-test-file-not-in-excluded-directory ()
  "Return nil when file is not in any excluded directory."
  (let* ((excluded (make-temp-file "vulpea-excl-" t))
         (other (make-temp-file "vulpea-other-" t))
         (vulpea-extras-excluded-directories (list excluded)))
    (unwind-protect
        (should-not (vulpea-extras-file-in-excluded-directory-p
                     (expand-file-name "todo.org" other)))
      (delete-directory excluded t)
      (delete-directory other t))))

(ert-deftest vulpea-extras-test-file-in-excluded-subdirectory ()
  "Return non-nil for files in subdirectories of excluded dirs."
  (let* ((tmpdir (make-temp-file "vulpea-test-" t))
         (subdir (expand-file-name "sub/deep" tmpdir))
         (vulpea-extras-excluded-directories (list tmpdir)))
    (unwind-protect
        (progn
          (make-directory subdir t)
          (should (vulpea-extras-file-in-excluded-directory-p
                   (expand-file-name "notes.org" subdir))))
      (delete-directory tmpdir t))))

(ert-deftest vulpea-extras-test-file-in-excluded-empty-list ()
  "Return nil when excluded list is empty."
  (let ((vulpea-extras-excluded-directories nil))
    (should-not (vulpea-extras-file-in-excluded-directory-p
                 "/tmp/any-file.org"))))

(ert-deftest vulpea-extras-test-file-in-excluded-multiple-dirs ()
  "Match against any directory in the excluded list."
  (let* ((dir-a (make-temp-file "vulpea-a-" t))
         (dir-b (make-temp-file "vulpea-b-" t))
         (dir-c (make-temp-file "vulpea-c-" t))
         (vulpea-extras-excluded-directories (list dir-a dir-b)))
    (unwind-protect
        (progn
          (should (vulpea-extras-file-in-excluded-directory-p
                   (expand-file-name "file.org" dir-b)))
          (should-not (vulpea-extras-file-in-excluded-directory-p
                       (expand-file-name "file.org" dir-c))))
      (delete-directory dir-a t)
      (delete-directory dir-b t)
      (delete-directory dir-c t))))

;;;; buffer-p

(ert-deftest vulpea-extras-test-buffer-p-in-roam-directory ()
  "Return non-nil when buffer file is under org-roam-directory."
  (let* ((tmpdir (file-truename (make-temp-file "vulpea-roam-" t)))
         (tmpfile (expand-file-name "test.org" tmpdir)))
    (unwind-protect
        (progn
          (write-region "" nil tmpfile)
          (with-current-buffer (find-file-noselect tmpfile)
            (unwind-protect
                (let ((org-roam-directory tmpdir))
                  (should (vulpea-extras-buffer-p)))
              (kill-buffer))))
      (delete-directory tmpdir t))))

(ert-deftest vulpea-extras-test-buffer-p-outside-roam-directory ()
  "Return nil when buffer file is outside org-roam-directory."
  (let* ((roam-dir (file-truename (make-temp-file "vulpea-roam-" t)))
         (other-dir (file-truename (make-temp-file "vulpea-other-" t)))
         (tmpfile (expand-file-name "test.org" other-dir)))
    (unwind-protect
        (progn
          (write-region "" nil tmpfile)
          (with-current-buffer (find-file-noselect tmpfile)
            (unwind-protect
                (let ((org-roam-directory roam-dir))
                  (should-not (vulpea-extras-buffer-p)))
              (kill-buffer))))
      (delete-directory roam-dir t)
      (delete-directory other-dir t))))

(ert-deftest vulpea-extras-test-buffer-p-no-file ()
  "Return nil when buffer has no associated file."
  (with-temp-buffer
    (let ((buffer-file-name nil)
          (org-roam-directory "/tmp"))
      (should-not (vulpea-extras-buffer-p)))))

;;;; anniversary-p

(ert-deftest vulpea-extras-test-anniversary-p-present ()
  "Return non-nil when buffer contains an anniversary entry."
  (with-temp-buffer
    (org-mode)
    (insert "* Birthdays\n%%(org-anniversary 1990 6 15) John Doe\n")
    (should (vulpea-extras-anniversary-p))))

(ert-deftest vulpea-extras-test-anniversary-p-absent ()
  "Return nil when buffer has no anniversary entry."
  (with-temp-buffer
    (org-mode)
    (insert "* Regular heading\nSome text\n")
    (should-not (vulpea-extras-anniversary-p))))

(provide 'vulpea-extras-test)
;;; vulpea-extras-test.el ends here
