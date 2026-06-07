;;; elpaca-extras-test.el --- Tests for elpaca-extras -*- lexical-binding: t -*-

;; Tests for package management helpers in elpaca-extras.el.

;;; Code:

(require 'ert)
(require 'elpaca-extras)

;;;; elpaca-extras-write-lock-file-excluding

(ert-deftest elpaca-extras-test-write-lock-file-excluding-filters-packages ()
  "Excluded packages are removed from the list written to the lock file."
  (let ((elpaca-extras-write-lock-file-excluded '(pkg-b pkg-d))
        (fake-queue '((pkg-a . a-data) (pkg-b . b-data) (pkg-c . c-data) (pkg-d . d-data)))
        written-elpacas)
    (cl-letf (((symbol-function 'elpaca-write-lock-file)
               (lambda (_path elpacas) (setq written-elpacas elpacas))))
      (elpaca-extras-write-lock-file-excluding "/tmp/lock" fake-queue)
      (should (equal written-elpacas '((pkg-a . a-data) (pkg-c . c-data)))))))

(ert-deftest elpaca-extras-test-write-lock-file-excluding-no-exclusions ()
  "When the exclusion list is empty, all packages are passed through."
  (let ((elpaca-extras-write-lock-file-excluded nil)
        (fake-queue '((pkg-a . a-data) (pkg-b . b-data)))
        written-elpacas)
    (cl-letf (((symbol-function 'elpaca-write-lock-file)
               (lambda (_path elpacas) (setq written-elpacas elpacas))))
      (elpaca-extras-write-lock-file-excluding "/tmp/lock" fake-queue)
      (should (equal written-elpacas '((pkg-a . a-data) (pkg-b . b-data)))))))

(ert-deftest elpaca-extras-test-write-lock-file-excluding-all-excluded ()
  "When every package is excluded, an empty list is written."
  (let ((elpaca-extras-write-lock-file-excluded '(pkg-a pkg-b))
        (fake-queue '((pkg-a . a-data) (pkg-b . b-data)))
        written-elpacas)
    (cl-letf (((symbol-function 'elpaca-write-lock-file)
               (lambda (_path elpacas) (setq written-elpacas elpacas))))
      (elpaca-extras-write-lock-file-excluding "/tmp/lock" fake-queue)
      (should (equal written-elpacas nil)))))

(ert-deftest elpaca-extras-test-write-lock-file-excluding-uses-queued-when-nil ()
  "When ELPACAS is nil, the function falls back to `elpaca--queued'."
  (let ((elpaca-extras-write-lock-file-excluded '(pkg-b))
        written-elpacas)
    (cl-letf (((symbol-function 'elpaca--queued)
               (lambda () '((pkg-a . a-data) (pkg-b . b-data) (pkg-c . c-data))))
              ((symbol-function 'elpaca-write-lock-file)
               (lambda (_path elpacas) (setq written-elpacas elpacas))))
      (elpaca-extras-write-lock-file-excluding "/tmp/lock")
      (should (equal written-elpacas '((pkg-a . a-data) (pkg-c . c-data)))))))

(ert-deftest elpaca-extras-test-write-lock-file-excluding-passes-path ()
  "The PATH argument is forwarded to `elpaca-write-lock-file'."
  (let ((elpaca-extras-write-lock-file-excluded nil)
        written-path)
    (cl-letf (((symbol-function 'elpaca-write-lock-file)
               (lambda (path _elpacas) (setq written-path path))))
      (elpaca-extras-write-lock-file-excluding "/my/lock/file" '((pkg-a . data)))
      (should (equal written-path "/my/lock/file")))))

;;;; elpaca-extras--handle-build-complete

(ert-deftest elpaca-extras-test-handle-build-complete-finished ()
  "On finished status, the callback is removed and reload is invoked."
  (let ((hook-removed nil)
        (reloaded nil)
        (message-result nil)
        token-status
        (fake-callback (lambda () nil)))
    (cl-letf (((symbol-function 'elpaca-get)
               (lambda (_pkg) 'fake-elpaca))
              ((symbol-function 'elpaca--status)
               (lambda (_e) 'finished))
              ((symbol-function 'remove-hook)
               (lambda (hook fn)
                 (when (eq hook 'elpaca-post-queue-hook)
                   (setq hook-removed fn))))
              ((symbol-function 'elpaca-extras-reload)
               (lambda (pkg) (setq reloaded pkg)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-result (apply #'format fmt args)))))
      (setq token-status
            (elpaca-extras--handle-build-complete
             'my-pkg fake-callback "Updated" "test-token"))
      (should (eq hook-removed fake-callback))
      (should (eq reloaded 'my-pkg))
      (should (eq token-status 'finished))
      (should (eq (plist-get (elpaca-extras-build-reload-status "test-token")
                             :state)
                  'finished))
      (should (string-match-p "Updated and reloaded: my-pkg" message-result)))))

(ert-deftest elpaca-extras-test-handle-build-complete-failed ()
  "On failed status, the callback is removed and a failure message is emitted."
  (let* ((hook-removed nil)
         (reloaded nil)
         (message-result nil)
         token-status
         (fake-callback (lambda () nil))
         ;; Build a fake elpaca struct: a list with log at index 20.
         (fake-elpaca (make-list 21 nil)))
    (setf (nth 20 fake-elpaca) '((nil nil "dependency conflict")))
    (cl-letf (((symbol-function 'elpaca-get)
               (lambda (_pkg) fake-elpaca))
              ((symbol-function 'elpaca--status)
               (lambda (_e) 'failed))
              ((symbol-function 'remove-hook)
               (lambda (hook fn)
                 (when (eq hook 'elpaca-post-queue-hook)
                   (setq hook-removed fn))))
              ((symbol-function 'elpaca-extras-reload)
               (lambda (_pkg) (setq reloaded t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-result (apply #'format fmt args)))))
      (setq token-status
            (elpaca-extras--handle-build-complete
             'broken-pkg fake-callback "Updated" "failed-token"))
      (should (eq hook-removed fake-callback))
      (should-not reloaded)
      (should (eq token-status 'failed))
      (should (eq (plist-get (elpaca-extras-build-reload-status "failed-token")
                             :state)
                  'failed))
      (should (string-match-p "Build failed for broken-pkg" message-result))
      (should (string-match-p "dependency conflict" message-result)))))

(ert-deftest elpaca-extras-test-handle-build-complete-pending ()
  "When status is neither finished nor failed, hook is not removed."
  (let ((hook-removed nil)
        (fake-callback (lambda () nil)))
    (cl-letf (((symbol-function 'elpaca-get)
               (lambda (_pkg) 'fake-elpaca))
              ((symbol-function 'elpaca--status)
               (lambda (_e) 'queued))
              ((symbol-function 'remove-hook)
               (lambda (_hook _fn) (setq hook-removed t))))
      (elpaca-extras--handle-build-complete 'my-pkg fake-callback "Updated")
      (should-not hook-removed))))

(ert-deftest elpaca-extras-test-handle-build-complete-nil-elpaca ()
  "When `elpaca-get' returns nil, the callback is not removed."
  (let ((hook-removed nil)
        (fake-callback (lambda () nil)))
    (cl-letf (((symbol-function 'elpaca-get)
               (lambda (_pkg) nil))
              ((symbol-function 'remove-hook)
               (lambda (_hook _fn) (setq hook-removed t))))
      (elpaca-extras--handle-build-complete 'missing-pkg fake-callback "Updated")
      (should-not hook-removed))))

(ert-deftest elpaca-extras-test-rebuild-and-reload-records-queued-token ()
  "Scheduling a rebuild records a queued status under the returned token."
  (let ((added-callback nil)
        (rebuilt nil))
    (cl-letf (((symbol-function 'add-hook)
               (lambda (hook callback)
                 (when (eq hook 'elpaca-post-queue-hook)
                   (setq added-callback callback))))
              ((symbol-function 'elpaca-rebuild)
               (lambda (pkg _force) (setq rebuilt pkg))))
      (let* ((token (elpaca-extras-rebuild-and-reload 'my-pkg))
             (entry (elpaca-extras-build-reload-status token)))
        (should (stringp token))
        (should added-callback)
        (should (eq rebuilt 'my-pkg))
        (should (eq (plist-get entry :package) 'my-pkg))
        (should (eq (plist-get entry :state) 'queued))))))

(ert-deftest elpaca-extras-test-build-reload-status-missing-token ()
  "Unknown reload tokens return nil."
  (should-not (elpaca-extras-build-reload-status "missing-token")))

(ert-deftest elpaca-extras-test-handle-build-complete-failed-no-log ()
  "On failure with empty log, the message reports an unknown error."
  (let* ((message-result nil)
         (fake-callback (lambda () nil))
         ;; Build a fake elpaca struct with nil log at index 20.
         (fake-elpaca (make-list 21 nil)))
    (cl-letf (((symbol-function 'elpaca-get)
               (lambda (_pkg) fake-elpaca))
              ((symbol-function 'elpaca--status)
               (lambda (_e) 'failed))
              ((symbol-function 'remove-hook)
               (lambda (_hook _fn) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-result (apply #'format fmt args)))))
      (elpaca-extras--handle-build-complete 'broken-pkg fake-callback "Rebuilt")
      (should (string-match-p "unknown error" message-result)))))

;;;; elpaca-extras-reload

(ert-deftest elpaca-extras-test-reload-loads-loaded-features ()
  "Loaded package features are force-loaded again."
  (let* ((loaded nil)
         ;; `features' is not a special variable, so `let' in a
         ;; lexical-binding file creates a lexical binding that is
         ;; invisible to `elpaca-extras-reload'.  Save and restore the
         ;; global value instead.
         (saved-features features))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-file)
                   (lambda (name _path _suffixes)
                     (format "/fake/pkg/%s.el" name)))
                  ((symbol-function 'directory-files)
                   (lambda (_dir _full _pattern)
                     '("/fake/pkg/pkg-core.el" "/fake/pkg/pkg-extra.el" "/fake/pkg/pkg-utils.el")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (file &rest _)
                     (let ((feature (file-name-sans-extension
                                     (file-name-nondirectory file))))
                       (erase-buffer)
                       (insert (format "(provide '%s)" feature))
                       (goto-char (point-min)))))
                  ((symbol-function 'load)
                   (lambda (file &rest _)
                     (push (intern (file-name-sans-extension
                                    (file-name-nondirectory file)))
                           loaded)))
                  ((symbol-function 'message)
                   #'ignore))
          (setq features (list 'pkg-core 'pkg-utils 'unrelated-feature))
          (elpaca-extras-reload 'pkg)
          ;; The main package feature is always force-loaded after a rebuild.
          (should (memq 'pkg loaded))
          (should (memq 'pkg-core loaded))
          (should (memq 'pkg-utils loaded))
          (should-not (memq 'pkg-extra loaded)))
      (setq features saved-features))))

(ert-deftest elpaca-extras-test-reload-allp-loads-all-features ()
  "With ALLP non-nil, all discovered features are loaded regardless of `features'."
  (let* ((loaded nil))
    (cl-letf (((symbol-function 'locate-file)
               (lambda (name _path _suffixes)
                 (format "/fake/pkg/%s.el" name)))
              ((symbol-function 'directory-files)
               (lambda (_dir _full _pattern)
                 '("/fake/pkg/pkg-core.el" "/fake/pkg/pkg-extra.el")))
              ((symbol-function 'insert-file-contents)
               (lambda (file &rest _)
                 (let ((feature (file-name-sans-extension
                                 (file-name-nondirectory file))))
                   (erase-buffer)
                   (insert (format "(provide '%s)" feature))
                   (goto-char (point-min)))))
              ((symbol-function 'load)
               (lambda (file &rest _)
                 (push (intern (file-name-sans-extension
                                (file-name-nondirectory file)))
                       loaded)))
              ((symbol-function 'message)
               #'ignore))
      (elpaca-extras-reload 'pkg 'allp)
      ;; Both features should be processed regardless of what is in `features'.
      (should (memq 'pkg-core loaded))
      (should (memq 'pkg-extra loaded)))))

(ert-deftest elpaca-extras-test-reload-no-matching-features ()
  "When no discovered features are loaded, the main feature is loaded."
  (let* ((loaded nil)
         (saved-features features))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-file)
                   (lambda (name _path _suffixes)
                     (format "/fake/pkg/%s.el" name)))
                  ((symbol-function 'directory-files)
                   (lambda (_dir _full _pattern)
                     '("/fake/pkg/pkg-core.el")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (file &rest _)
                     (let ((feature (file-name-sans-extension
                                     (file-name-nondirectory file))))
                       (erase-buffer)
                       (insert (format "(provide '%s)" feature))
                       (goto-char (point-min)))))
                  ((symbol-function 'load)
                   (lambda (file &rest _)
                     (push (intern (file-name-sans-extension
                                    (file-name-nondirectory file)))
                           loaded)))
                  ((symbol-function 'message)
                   #'ignore))
          (setq features (list 'unrelated-feature))
          (elpaca-extras-reload 'pkg)
          (should (equal loaded '(pkg))))
      (setq features saved-features))))

(provide 'elpaca-extras-test)
;;; elpaca-extras-test.el ends here
