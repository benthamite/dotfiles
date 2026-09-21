;;; files-extras-test.el --- Tests for files-extras -*- lexical-binding: t -*-

;; Tests for file operations, buffer utilities, path processing,
;; and text cleanup functions in files-extras.el.

;;; Code:

(require 'ert)
(require 'files-extras)

(defconst files-extras-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

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

(ert-deftest files-extras-test-copy-current-path-buffer-name ()
  "Copy buffer name to kill ring with prefix argument."
  (with-temp-buffer
    (rename-buffer "test-buffer-name" t)
    (files-extras-copy-current-path '(4))
    (should (equal (current-kill 0) "test-buffer-name"))))

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

;;;; Copy as kill DWIM

(ert-deftest files-extras-test-copy-as-kill-dwim-file-buffer ()
  "Copy the file name in a file-visiting buffer."
  (let* ((file (make-temp-file "files-extras-copy"))
	 (buf (find-file-noselect file)))
    (unwind-protect
	(with-current-buffer buf
	  (files-extras-copy-as-kill-dwim)
	  (should (equal (current-kill 0) file)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file file))))

(ert-deftest files-extras-test-ocr-missing-language-code ()
  "Reject missing OCR language codes before starting a subprocess."
  (skip-unless (require 'tlon-core nil t))
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/ocrmypdf"))
            ((symbol-function 'start-process-shell-command)
             (lambda (&rest _) (error "OCR must not start"))))
    (should-error
     (files-extras-ocr-pdf nil "/tmp/unused.pdf" nil "unconfigured-language")
     :type 'user-error)))

(ert-deftest files-extras-test-ocr-preserves-scans-in-mixed-pdf ()
  "Add searchable text without replacing original images in a mixed PDF."
  (skip-unless (require 'tlon-core nil t))
  (dolist (command '("ocrmypdf" "pdfimages" "pdftotext"))
    (skip-unless (executable-find command)))
  (let* ((directory (make-temp-file "files-extras-ocr-test-" t))
         (file (expand-file-name "mixed.pdf" directory))
         (tlon-languages-properties '((:name "english" :iso-639-2 "eng")))
         process)
    (unwind-protect
        (progn
          (copy-file (expand-file-name "fixtures/files-extras-ocr-mixed.pdf"
                                       files-extras-test--directory) file)
          (let ((images (files-extras-test--pdf-image-signatures
                          file (expand-file-name "before" directory))))
            (should (= (length images) 2))
            (setq process (files-extras-ocr-pdf nil file nil "english"))
            (let ((deadline (+ (float-time) 60)))
              (while (and (process-live-p process) (< (float-time) deadline))
                (accept-process-output process 0.1)))
            (should (eq (process-status process) 'exit))
            (should (zerop (process-exit-status process)))
            (should (equal images (files-extras-test--pdf-image-signatures
                                   file (expand-file-name "after" directory))))
            (with-temp-buffer
              (should (zerop (process-file "pdftotext" nil t nil file "-")))
              (should (string-match-p "Existing searchable page" (buffer-string)))
              (should (string-match-p "SCANNED ONLY PAGE" (buffer-string))))))
      (when (and process (process-live-p process)) (delete-process process))
      (when (and process (buffer-live-p (process-buffer process)))
        (kill-buffer (process-buffer process)))
      (delete-directory directory t))))

(defun files-extras-test--pdf-image-signatures (file prefix)
  "Return hashes of images extracted from FILE with PREFIX."
  (with-temp-buffer
    (should (zerop (process-file "pdfimages" nil t nil "-all" file prefix))))
  (mapcar (lambda (image)
            (with-temp-buffer
              (insert-file-contents-literally image)
              (secure-hash 'sha256 (current-buffer))))
          (directory-files (file-name-directory prefix) t
                           (concat "\\`" (regexp-quote (file-name-nondirectory prefix))
                                   "-"))))

(ert-deftest files-extras-test-explicit-ocr-options-retained ()
  "Retain forced deskewing and explicitly supplied OCR parameters."
  (skip-unless (require 'tlon-core nil t))
  (let ((tlon-languages-properties '((:name "english" :iso-639-2 "eng")))
        command)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/ocrmypdf"))
              ((symbol-function 'start-process-shell-command)
               (lambda (_name _buffer invocation) (setq command invocation)))
              ((symbol-function 'set-process-filter) #'ignore))
      (files-extras-ocr-pdf t "/tmp/scan.pdf" nil "english")
      (should (string-match-p "--force-ocr --deskew" command))
      (should-not (string-match-p "--skip-text\\|--optimize 0" command))
      (files-extras-ocr-pdf nil "/tmp/scan.pdf" "--redo-ocr custom-in.pdf custom-out.pdf"
                            "english")
      (should (equal command "ocrmypdf --redo-ocr custom-in.pdf custom-out.pdf")))))

(provide 'files-extras-test)
;;; files-extras-test.el ends here
