;;; eww-extras-test.el --- Tests for eww-extras -*- lexical-binding: t -*-

;; Tests for URL command construction and readable exceptions
;; in eww-extras.el.

;;; Code:

(require 'ert)
(require 'eww-extras)

;;;; URL-to-file command construction

(ert-deftest eww-extras-test-make-command-pdf ()
  "Make-command constructs a PDF download command."
  (let ((browse-url-chrome-program "/usr/bin/chrome")
        (eww-extras-node-program "/usr/bin/node"))
    (let ((cmd (eww-extras-url-to-file-make-command
                "https://example.com" "/tmp/out.pdf" "pdf")))
      (should (listp cmd))
      (should (string-match-p "eww-extras-renderer/run\\.sh" (car cmd)))
      (should (equal (cadr cmd) "render"))
      (should (equal (cadr (member "--type" cmd)) "pdf"))
      (should-not (member "--profile-directory" cmd))
      (should-not (member "--user-data-dir" cmd))
      (should (member "https://example.com" cmd)))))

(ert-deftest eww-extras-test-make-command-html ()
  "Make-command constructs an HTML download command."
  (let ((browse-url-chrome-program "/usr/bin/chrome")
        (eww-extras-node-program "/usr/bin/node"))
    (let ((cmd (eww-extras-url-to-file-make-command
                "https://example.com" "/tmp/out.html" "html")))
      (should (listp cmd))
      (should (string-match-p "eww-extras-renderer/run\\.sh" (car cmd)))
      (should (equal (cadr (member "--type" cmd)) "html")))))

(ert-deftest eww-extras-test-make-command-needs-no-node-flags ()
  "Make-command leaves Node and browser state to the renderer wrapper."
  (let ((browse-url-chrome-program "/usr/bin/chrome")
        (eww-extras-node-program "/usr/bin/node"))
    (let ((command (eww-extras-url-to-file-make-command
                    "https://example.com" "/tmp/out.pdf" "pdf")))
      (should-not (member "--experimental-websocket" command))
      (should-not (member "/usr/bin/node" command)))))

(ert-deftest eww-extras-test-make-command-invalid-type ()
  "Make-command signals error for invalid type."
  (let ((browse-url-chrome-program "/usr/bin/chrome"))
    (should-error (eww-extras-url-to-file-make-command
                   "https://example.com" "/tmp/out.xyz" "xyz"))))

;;;; Run callback

(ert-deftest eww-extras-test-run-callback-with-fn ()
  "Run-callback calls the function when provided."
  (let ((called nil))
    (eww-extras-run-callback
     (lambda (file key) (setq called (list file key)))
     "/tmp/test.pdf" "smith2024")
    (should (equal called '("/tmp/test.pdf" "smith2024")))))

(ert-deftest eww-extras-test-run-callback-nil ()
  "Run-callback does nothing when callback is nil."
  (should-not (eww-extras-run-callback nil "/tmp/test.pdf" "key")))

(ert-deftest eww-extras-test-sentinel-rejects-output-after-failure ()
  "A failed renderer must not attach a nonempty destination file."
  (let ((file (make-temp-file "eww-extras-failed"))
        (called nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "pre-existing"))
          (cl-letf (((symbol-function 'process-exit-status) (lambda (_proc) 1)))
            (should-error
             (funcall (eww-extras-url-to-file-sentinel
                       (lambda (&rest _) (setq called t)) file nil)
                      nil "exited abnormally\n")
             :type 'user-error))
          (should-not called)
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "pre-existing")))
      (delete-file file))))

(ert-deftest eww-extras-test-url-to-file-uses-private-process-buffers ()
  "Concurrent renders must not share diagnostic output."
  (let ((buffers nil)
        (paths-dir-downloads temporary-file-directory))
    (cl-letf (((symbol-function 'simple-extras-get-url) #'identity)
              ((symbol-function 'make-process)
               (lambda (&rest arguments)
                 (push (plist-get arguments :buffer) buffers)
                 'eww-extras-test-process))
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'message) #'ignore))
      (eww-extras-url-to-file "pdf" "https://example.com/one")
      (eww-extras-url-to-file "pdf" "https://example.com/two"))
    (unwind-protect
        (progn
          (should (= (length buffers) 2))
          (should (cl-every #'bufferp buffers))
          (should-not (eq (car buffers) (cadr buffers))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            buffers))))

(ert-deftest eww-extras-test-copied-profile-api-is-retired ()
  "Rendering must not expose commands that copy personal Chrome state."
  (should-not (fboundp 'eww-extras-chrome-copy-data-dirs))
  (should-not (fboundp 'eww-extras-chrome-delete-data-dirs))
  (should-not (boundp 'eww-extras-chrome-data-dir-copy-pdf))
  (should-not (boundp 'eww-extras-chrome-data-dir-copy-html)))

;;;; Readable exceptions from file

(ert-deftest eww-extras-test-set-readable-exceptions-from-file ()
  "Set-readable-exceptions-from-file populates the exception list."
  (let* ((tmpfile (make-temp-file "eww-test-exceptions"))
         (eww-extras-readable-exceptions-file tmpfile)
         (eww-extras-readable-exceptions nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "example.com\ngithub.com\n"))
          (eww-extras-set-readable-exceptions-from-file)
          (should (= (length eww-extras-readable-exceptions) 2))
          ;; Each entry should be regexp-quoted
          (should (cl-every #'stringp eww-extras-readable-exceptions)))
      (delete-file tmpfile))))

(ert-deftest eww-extras-test-set-readable-exceptions-missing-file ()
  "Set-readable-exceptions-from-file does nothing when file is missing."
  (let ((eww-extras-readable-exceptions-file "/nonexistent/file")
        (eww-extras-readable-exceptions '("existing")))
    (eww-extras-set-readable-exceptions-from-file)
    ;; Should not have changed the variable
    (should (equal eww-extras-readable-exceptions '("existing")))))

;;;; Readable autoview

(ert-deftest eww-extras-test-readable-autoview-exception ()
  "Readable-autoview skips readability for URLs in exception list."
  (let ((eww-extras-readable-exceptions '("example\\.com")))
    (with-temp-buffer
      ;; Simulate eww buffer with URL
      (let ((eww-data (list :url "https://example.com/page" :source "<p>test</p>")))
        ;; Should not error, should skip eww-readable
        (eww-extras-readable-autoview)))))

(provide 'eww-extras-test)
;;; eww-extras-test.el ends here
