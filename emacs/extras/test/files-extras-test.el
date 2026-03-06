;;; files-extras-test.el --- Tests for files-extras -*- lexical-binding: t -*-

;; Tests for file operations, buffer utilities, path processing,
;; and text cleanup functions in files-extras.el.

;;; Code:

(require 'ert)
(require 'files-extras)

;;;; bollp (beginning of last line predicate)

(ert-deftest files-extras-test-bollp-at-last-line ()
  "Bollp returns t when point is at beginning of last line."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char (point-max))
    (beginning-of-line)
    (should (files-extras-bollp))))

(ert-deftest files-extras-test-bollp-at-end-of-buffer ()
  "Bollp returns t when point is at end of buffer."
  (with-temp-buffer
    (insert "first\nsecond")
    (goto-char (point-max))
    (should (files-extras-bollp))))

(ert-deftest files-extras-test-bollp-not-at-last-line ()
  "Bollp returns nil when point is not at last line."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char (point-min))
    (should-not (files-extras-bollp))))

(ert-deftest files-extras-test-bollp-single-line ()
  "Bollp returns t in a single-line buffer."
  (with-temp-buffer
    (insert "only line")
    (goto-char (point-min))
    (should (files-extras-bollp))))

(ert-deftest files-extras-test-bollp-empty-buffer ()
  "Bollp returns t in an empty buffer."
  (with-temp-buffer
    (should (files-extras-bollp))))

;;;; get-nth-directory

(ert-deftest files-extras-test-get-nth-directory-first ()
  "Get-nth-directory returns the first path component.
For absolute paths, split-string on \"/\" gives (\"\") as first element,
which `file-name-as-directory' converts to \"./\"."
  (should (equal (files-extras-get-nth-directory "/Users/foo/bar/") "./")))

(ert-deftest files-extras-test-get-nth-directory-second ()
  "Get-nth-directory returns the nth path component."
  (should (equal (files-extras-get-nth-directory "/Users/foo/bar/" 1) "Users/")))

(ert-deftest files-extras-test-get-nth-directory-deep ()
  "Get-nth-directory returns a deep path component."
  (should (equal (files-extras-get-nth-directory "/Users/foo/bar/" 3) "bar/")))

;;;; lines-to-list / list-to-lines round trip

(ert-deftest files-extras-test-lines-to-list ()
  "Lines-to-list reads file lines into a list."
  (let ((tmp (make-temp-file "test-lines")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "alpha\nbeta\ngamma"))
          (let ((result (files-extras-lines-to-list tmp)))
            (should (equal result '("alpha" "beta" "gamma")))))
      (delete-file tmp))))

(ert-deftest files-extras-test-lines-to-list-empty-file ()
  "Lines-to-list returns nil for an empty file."
  (let ((tmp (make-temp-file "test-empty")))
    (unwind-protect
        (should (null (files-extras-lines-to-list tmp)))
      (delete-file tmp))))

(ert-deftest files-extras-test-list-to-lines ()
  "List-to-lines writes list elements as file lines."
  (let ((tmp (make-temp-file "test-write")))
    (unwind-protect
        (progn
          (files-extras-list-to-lines '("one" "two" "three") tmp)
          (should (equal (files-extras-lines-to-list tmp)
                         '("one" "two" "three"))))
      (delete-file tmp))))

(ert-deftest files-extras-test-lines-round-trip ()
  "Lines-to-list and list-to-lines round-trip correctly."
  (let ((tmp (make-temp-file "test-roundtrip"))
        (data '("foo" "bar" "baz" "quux")))
    (unwind-protect
        (progn
          (files-extras-list-to-lines data tmp)
          (should (equal (files-extras-lines-to-list tmp) data)))
      (delete-file tmp))))

;;;; Screenshot regexp

(ert-deftest files-extras-test-screenshot-regexp-matches ()
  "Screenshot regexp matches macOS screenshot filenames."
  (should (string-match-p files-extras-screenshot-regexp
                          "Screenshot 2024-01-15 at 14.30.45.png"))
  (should (string-match-p files-extras-screenshot-regexp
                          "Screenshot 2023-12-31 at 09.05.00.jpg")))

(ert-deftest files-extras-test-screenshot-regexp-rejects ()
  "Screenshot regexp rejects non-screenshot filenames."
  (should-not (string-match-p files-extras-screenshot-regexp
                              "document.pdf"))
  (let ((case-fold-search nil))
    (should-not (string-match-p files-extras-screenshot-regexp
                                "Screenshot 2024-1-5 at 9.30.45.png"))
    (should-not (string-match-p files-extras-screenshot-regexp
                                "screenshot 2024-01-15 at 14.30.45.png"))))

;;;; Remove extra blank lines

(ert-deftest files-extras-test-remove-extra-blank-lines ()
  "Remove-extra-blank-lines collapses multiple blank lines."
  (with-temp-buffer
    (insert "line one\n\n\n\nline two\n\n\nline three")
    (files-extras-remove-extra-blank-lines)
    (should (equal (buffer-string) "line one\n\nline two\n\nline three"))))

(ert-deftest files-extras-test-remove-extra-blank-lines-single ()
  "Remove-extra-blank-lines preserves single blank lines."
  (with-temp-buffer
    (insert "line one\n\nline two\n\nline three")
    (files-extras-remove-extra-blank-lines)
    (should (equal (buffer-string) "line one\n\nline two\n\nline three"))))

(ert-deftest files-extras-test-remove-extra-blank-lines-none ()
  "Remove-extra-blank-lines leaves text without blank lines unchanged."
  (with-temp-buffer
    (insert "line one\nline two\nline three")
    (files-extras-remove-extra-blank-lines)
    (should (equal (buffer-string) "line one\nline two\nline three"))))

;;;; Get stem of current buffer

(ert-deftest files-extras-test-get-stem-with-file ()
  "Get-stem-of-current-buffer returns filename without extension."
  (with-temp-buffer
    (setq buffer-file-name "/path/to/my-file.el")
    (should (equal (files-extras-get-stem-of-current-buffer) "my-file"))))

(ert-deftest files-extras-test-get-stem-no-file ()
  "Get-stem-of-current-buffer returns nil for non-file buffers."
  (with-temp-buffer
    (should-not (files-extras-get-stem-of-current-buffer))))

(ert-deftest files-extras-test-get-stem-nested-path ()
  "Get-stem-of-current-buffer ignores directory components."
  (with-temp-buffer
    (setq buffer-file-name "/a/b/c/d/test-file.org")
    (should (equal (files-extras-get-stem-of-current-buffer) "test-file"))))

;;;; Get current dir lowercased

(ert-deftest files-extras-test-get-current-dir-lowercased ()
  "Get-current-dir-lowercased returns lowercased dir name with underscores."
  (let ((default-directory "/home/user/My-Project/"))
    (should (equal (files-extras-get-current-dir-lowercased) "My_Project"))))

(ert-deftest files-extras-test-get-current-dir-lowercased-no-hyphens ()
  "Get-current-dir-lowercased leaves non-hyphen names unchanged."
  (let ((default-directory "/home/user/project/"))
    (should (equal (files-extras-get-current-dir-lowercased) "project"))))

;;;; Bury scratch buffer

(ert-deftest files-extras-test-bury-scratch-buffer-non-scratch ()
  "Bury-scratch-buffer returns t for non-scratch buffers."
  (with-temp-buffer
    (rename-buffer "not-scratch" t)
    (should (files-extras-bury-scratch-buffer))))

;;;; Get help file

(ert-deftest files-extras-test-get-help-file-org-exists ()
  "Get-help-file finds .org file in doc/ subdirectory."
  (let* ((tmp-dir (make-temp-file "test-help" t))
         (doc-dir (file-name-concat tmp-dir "doc/"))
         (source-file (file-name-concat tmp-dir "my-package.el"))
         (help-file (file-name-concat doc-dir "my-package.org")))
    (unwind-protect
        (progn
          (make-directory doc-dir t)
          (with-temp-file source-file (insert ""))
          (with-temp-file help-file (insert ""))
          (should (equal (files-extras-get-help-file source-file) help-file)))
      (delete-directory tmp-dir t))))

(ert-deftest files-extras-test-get-help-file-md-exists ()
  "Get-help-file finds .md file in doc/ subdirectory."
  (let* ((tmp-dir (make-temp-file "test-help" t))
         (doc-dir (file-name-concat tmp-dir "doc/"))
         (source-file (file-name-concat tmp-dir "my-package.el"))
         (help-file (file-name-concat doc-dir "my-package.md")))
    (unwind-protect
        (progn
          (make-directory doc-dir t)
          (with-temp-file source-file (insert ""))
          (with-temp-file help-file (insert ""))
          (should (equal (files-extras-get-help-file source-file) help-file)))
      (delete-directory tmp-dir t))))

(ert-deftest files-extras-test-get-help-file-prefers-org ()
  "Get-help-file prefers .org over .md when both exist."
  (let* ((tmp-dir (make-temp-file "test-help" t))
         (doc-dir (file-name-concat tmp-dir "doc/"))
         (source-file (file-name-concat tmp-dir "my-package.el"))
         (org-file (file-name-concat doc-dir "my-package.org"))
         (md-file (file-name-concat doc-dir "my-package.md")))
    (unwind-protect
        (progn
          (make-directory doc-dir t)
          (with-temp-file source-file (insert ""))
          (with-temp-file org-file (insert ""))
          (with-temp-file md-file (insert ""))
          (should (equal (files-extras-get-help-file source-file) org-file)))
      (delete-directory tmp-dir t))))

(ert-deftest files-extras-test-get-help-file-docs-dir ()
  "Get-help-file also searches docs/ subdirectory."
  (let* ((tmp-dir (make-temp-file "test-help" t))
         (docs-dir (file-name-concat tmp-dir "docs/"))
         (source-file (file-name-concat tmp-dir "my-package.el"))
         (help-file (file-name-concat docs-dir "my-package.org")))
    (unwind-protect
        (progn
          (make-directory docs-dir t)
          (with-temp-file source-file (insert ""))
          (with-temp-file help-file (insert ""))
          (should (equal (files-extras-get-help-file source-file) help-file)))
      (delete-directory tmp-dir t))))

(ert-deftest files-extras-test-get-help-file-none ()
  "Get-help-file returns nil when no help file exists."
  (let* ((tmp-dir (make-temp-file "test-help" t))
         (source-file (file-name-concat tmp-dir "my-package.el")))
    (unwind-protect
        (progn
          (with-temp-file source-file (insert ""))
          (should-not (files-extras-get-help-file source-file)))
      (delete-directory tmp-dir t))))

;;;; Open buffer files

(ert-deftest files-extras-test-open-buffer-files-filters-non-org ()
  "Open-buffer-files only returns .org file-visiting buffers."
  ;; This function filters for .org files specifically
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test.el")
    (should-not (member "/tmp/test.el" (files-extras-open-buffer-files)))))

;;;; Newest file

(ert-deftest files-extras-test-newest-file-returns-most-recent ()
  "Return the newest file in a directory with multiple files."
  (let ((tmp-dir (make-temp-file "test-newest" t)))
    (unwind-protect
        (let ((file1 (file-name-concat tmp-dir "old.txt"))
              (file2 (file-name-concat tmp-dir "new.txt")))
          (with-temp-file file1 (insert "old"))
          ;; Ensure file2 has a later modification time
          (sleep-for 1)
          (with-temp-file file2 (insert "new"))
          (should (equal (files-extras-newest-file tmp-dir) file2)))
      (delete-directory tmp-dir t))))

(ert-deftest files-extras-test-newest-file-excludes-ds-store ()
  "Exclude .DS_Store files even if they are the newest."
  (let ((tmp-dir (make-temp-file "test-newest" t)))
    (unwind-protect
        (let ((file1 (file-name-concat tmp-dir "real.txt"))
              (ds-store (file-name-concat tmp-dir ".DS_Store")))
          (with-temp-file file1 (insert "content"))
          (sleep-for 1)
          (with-temp-file ds-store (insert ""))
          (should (equal (files-extras-newest-file tmp-dir) file1)))
      (delete-directory tmp-dir t))))

(ert-deftest files-extras-test-newest-file-excludes-localized ()
  "Exclude .localized files even if they are the newest."
  (let ((tmp-dir (make-temp-file "test-newest" t)))
    (unwind-protect
        (let ((file1 (file-name-concat tmp-dir "real.txt"))
              (localized (file-name-concat tmp-dir ".localized")))
          (with-temp-file file1 (insert "content"))
          (sleep-for 1)
          (with-temp-file localized (insert ""))
          (should (equal (files-extras-newest-file tmp-dir) file1)))
      (delete-directory tmp-dir t))))

(ert-deftest files-extras-test-newest-file-excludes-directories ()
  "Exclude subdirectories from the result."
  (let ((tmp-dir (make-temp-file "test-newest" t)))
    (unwind-protect
        (let ((file1 (file-name-concat tmp-dir "file.txt"))
              (subdir (file-name-concat tmp-dir "subdir")))
          (with-temp-file file1 (insert "content"))
          (make-directory subdir)
          (should (equal (files-extras-newest-file tmp-dir) file1)))
      (delete-directory tmp-dir t))))

;;;; Copy current path

(ert-deftest files-extras-test-copy-current-path-file-buffer ()
  "Copy buffer-file-name to kill ring for a file-visiting buffer."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-copy-path.el")
    (files-extras-copy-current-path)
    (should (equal (current-kill 0) "/tmp/test-copy-path.el"))))

(ert-deftest files-extras-test-copy-current-path-non-file-buffer ()
  "Copy default-directory to kill ring for a non-file buffer."
  (with-temp-buffer
    (let ((default-directory "/tmp/some-dir/"))
      (files-extras-copy-current-path)
      (should (equal (current-kill 0) "/tmp/some-dir/")))))

;;;; Kill all file-visiting buffers

(ert-deftest files-extras-test-kill-all-file-visiting-buffers-kills-file-buffers ()
  "Kill buffers visiting files."
  (let* ((tmp1 (make-temp-file "test-kill1"))
         (tmp2 (make-temp-file "test-kill2"))
         (buf1 (find-file-noselect tmp1))
         (buf2 (find-file-noselect tmp2)))
    (unwind-protect
        (progn
          (should (buffer-live-p buf1))
          (should (buffer-live-p buf2))
          (files-extras-kill-all-file-visiting-buffers)
          (should-not (buffer-live-p buf1))
          (should-not (buffer-live-p buf2)))
      ;; Clean up in case test fails
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2))
      (delete-file tmp1)
      (delete-file tmp2))))

(ert-deftest files-extras-test-kill-all-file-visiting-buffers-respects-exclusions ()
  "Do not kill buffers visiting excluded files."
  (let* ((tmp1 (make-temp-file "test-kill1"))
         (tmp2 (make-temp-file "test-kill2"))
         (buf1 (find-file-noselect tmp1))
         (buf2 (find-file-noselect tmp2)))
    (unwind-protect
        (progn
          (files-extras-kill-all-file-visiting-buffers
           (list (buffer-file-name buf1)))
          (should (buffer-live-p buf1))
          (should-not (buffer-live-p buf2)))
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2))
      (delete-file tmp1)
      (delete-file tmp2))))

(ert-deftest files-extras-test-kill-all-file-visiting-buffers-ignores-non-file-buffers ()
  "Do not kill buffers that are not visiting files."
  (let ((tmp-buf (generate-new-buffer "test-non-file")))
    (unwind-protect
        (progn
          (files-extras-kill-all-file-visiting-buffers)
          (should (buffer-live-p tmp-buf)))
      (when (buffer-live-p tmp-buf) (kill-buffer tmp-buf)))))

;;;; New empty buffer

(ert-deftest files-extras-test-new-empty-buffer-creates-buffer ()
  "Create a new buffer named untitled."
  (let ((files-extras-new-empty-buffer-major-mode nil)
        (buf nil))
    (unwind-protect
        (progn
          (setq buf (files-extras-new-empty-buffer))
          (should (buffer-live-p buf))
          (should (string-match-p "untitled" (buffer-name buf))))
      (when (and buf (buffer-live-p buf)) (kill-buffer buf)))))

(ert-deftest files-extras-test-new-empty-buffer-sets-major-mode ()
  "Set the configured major mode on the new buffer."
  (let ((files-extras-new-empty-buffer-major-mode 'text-mode)
        (buf nil))
    (unwind-protect
        (progn
          (setq buf (files-extras-new-empty-buffer))
          (with-current-buffer buf
            (should (eq major-mode 'text-mode))))
      (when (and buf (buffer-live-p buf)) (kill-buffer buf)))))

(ert-deftest files-extras-test-new-empty-buffer-offers-save ()
  "Set buffer-offer-save to t on the new buffer."
  (let ((files-extras-new-empty-buffer-major-mode nil)
        (buf nil))
    (unwind-protect
        (progn
          (setq buf (files-extras-new-empty-buffer))
          (with-current-buffer buf
            (should (eq buffer-offer-save t))))
      (when (and buf (buffer-live-p buf)) (kill-buffer buf)))))

(provide 'files-extras-test)
;;; files-extras-test.el ends here
