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

(ert-deftest elpaca-extras-test-handle-build-complete-restart-required ()
  "A class-layout change becomes a terminal status, not a hook error."
  (let ((hook-removed nil)
        (message-result nil)
        (fake-callback (lambda () nil)))
    (cl-letf (((symbol-function 'elpaca-get)
               (lambda (_pkg) 'fake-elpaca))
              ((symbol-function 'elpaca--status)
               (lambda (_e) 'finished))
              ((symbol-function 'remove-hook)
               (lambda (_hook fn) (setq hook-removed fn)))
              ((symbol-function 'elpaca-extras-reload)
               (lambda (_pkg)
                 (signal 'elpaca-extras-restart-required
                         '("restart Emacs"))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-result
                       (apply #'format format-string args)))))
      (should
       (eq 'failed
           (elpaca-extras--handle-build-complete
            'my-pkg fake-callback "Rebuilt" "restart-token")))
      (should (eq hook-removed fake-callback))
      (should
       (eq 'failed
           (plist-get
            (elpaca-extras-build-reload-status "restart-token") :state)))
      (should (string-match-p "restart Emacs" message-result)))))

(ert-deftest elpaca-extras-test-handle-build-complete-reload-error ()
  "Any reload error becomes a terminal failure instead of escaping."
  (let ((hook-removed nil)
        (message-result nil)
        (fake-callback (lambda () nil)))
    (cl-letf (((symbol-function 'elpaca-get)
               (lambda (_pkg) 'fake-elpaca))
              ((symbol-function 'elpaca--status)
               (lambda (_e) 'finished))
              ((symbol-function 'remove-hook)
               (lambda (_hook fn) (setq hook-removed fn)))
              ((symbol-function 'elpaca-extras-reload)
               (lambda (_pkg) (error "broken source parser")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-result
                       (apply #'format format-string args)))))
      (should
       (eq 'failed
           (elpaca-extras--handle-build-complete
            'my-pkg fake-callback "Rebuilt" "error-token")))
      (should (eq hook-removed fake-callback))
      (should
       (eq 'failed
           (plist-get
            (elpaca-extras-build-reload-status "error-token") :state)))
      (should (string-match-p "broken source parser" message-result)))))

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

(ert-deftest elpaca-extras-test-rebuild-and-reload-does-not-truncate-build-data ()
  "The rebuild runs with unbounded printing for serialized Elpaca commands."
  (let ((print-length 10)
        (print-level 10)
        observed-print-settings)
    (cl-letf (((symbol-function 'add-hook) #'ignore)
              ((symbol-function 'elpaca-rebuild)
               (lambda (_pkg _force)
                 (setq observed-print-settings
                       (list print-length print-level)))))
      (elpaca-extras-rebuild-and-reload 'my-pkg)
      (should (equal observed-print-settings '(nil nil))))))

(ert-deftest elpaca-extras-test-async-build-steps-do-not-truncate-commands ()
  "Printer limits stay disabled when a build step runs after enqueue returns."
  (let ((print-length 3)
        (print-level 2)
        deferred-build
        serialized)
    (cl-letf (((symbol-function 'add-hook) #'ignore)
              ((symbol-function 'elpaca-rebuild)
               (lambda (_pkg _force)
                 (setq deferred-build
                       (lambda ()
                         (setq serialized
                               (elpaca-extras--without-print-limits
                                (lambda ()
                                  (format "%S" '((1 2 3 4 5)
                                                  (6 7 8 9 10)))))))))))
      (elpaca-extras-rebuild-and-reload 'my-pkg))
    (should (functionp deferred-build))
    (funcall deferred-build)
    (should (equal serialized "((1 2 3 4 5) (6 7 8 9 10))"))
    (should (advice-member-p #'elpaca-extras--without-print-limits
                             'elpaca-build-autoloads))
    (should (advice-member-p #'elpaca-extras--without-print-limits
                             'elpaca-build-compile))))

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

(ert-deftest elpaca-extras-test-source-feature-info-finds-wrapped-class ()
  "Read wrapped classes, parents, and slot allocation from package source."
  (cl-letf (((symbol-function 'insert-file-contents)
             (lambda (_file &rest _)
               (insert
                "`(defclass quoted-class () ((ignored-slot)))\n(eval-and-compile (defclass pkg-child (pkg-parent) ((inherited-slot) (own-slot) (shared-slot :allocation :class))))\n(provide 'pkg-child)"))))
    (let* ((info (elpaca-extras--source-feature-info "/fake/pkg-child.el"))
           (layout (car (plist-get info :classes))))
      (should (equal (plist-get info :features) '(pkg-child)))
      (should (= (length (plist-get info :classes)) 1))
      (should (eq (plist-get layout :name) 'pkg-child))
      (should (equal (plist-get layout :parents) '(pkg-parent)))
      (should
       (equal (plist-get layout :slots)
              '((inherited-slot . :instance)
                (own-slot . :instance)
                (shared-slot . :class)))))))

(ert-deftest elpaca-extras-test-source-feature-info-keeps-every-provide ()
  "Associate every provided feature with its source file."
  (cl-letf (((symbol-function 'insert-file-contents)
             (lambda (_file &rest _)
               (insert "(provide 'pkg)\n(provide 'pkg-extra)"))))
    (should
     (equal (plist-get
             (elpaca-extras--source-feature-info "/fake/pkg.el") :features)
            '(pkg pkg-extra)))))

(ert-deftest elpaca-extras-test-source-class-layout-rejects-malformed-forms ()
  "Reject class declarations whose runtime layout cannot be determined."
  (should-error
   (elpaca-extras--source-class-layout
    '(defclass malformed-parent ((computed-parent)) ((slot)))))
  (should-error
   (elpaca-extras--source-class-layout
    '(defclass malformed-slot () ((42)))))
  (should-error
   (elpaca-extras--source-class-layout
    '(defclass malformed-allocation
       ()
       ((slot :allocation (if condition :class :instance)))))))

(ert-deftest elpaca-extras-test-class-layouts-reject-divergent-declarations ()
  "Fail closed when selected source defines one class two different ways."
  (let ((feature-info
         '((:features (pkg-class)
            :classes
            ((:name duplicate :parents nil
              :slots ((first . :instance)))
             (:name duplicate :parents nil
              :slots ((second . :instance))))))))
    (should-error
     (elpaca-extras--class-layouts feature-info)
     :type 'elpaca-extras-restart-required)))

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

(ert-deftest elpaca-extras-test-reload-orders-feature-dependencies ()
  "Reload package subfeatures after the package features they require."
  (let ((loaded nil)
        (saved-features features))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-file)
                   (lambda (name _path _suffixes)
                     (format "/fake/pkg/%s.el" name)))
                  ((symbol-function 'directory-files)
                   (lambda (_dir _full _pattern)
                     '("/fake/pkg/pkg-child.el"
                       "/fake/pkg/pkg-base.el")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (file &rest _)
                     (erase-buffer)
                     (if (string-match-p "child" file)
                         (insert "(require 'pkg-base)\n(provide 'pkg-child)")
                       (insert "(provide 'pkg-base)"))
                     (goto-char (point-min))))
                  ((symbol-function 'load)
                   (lambda (file &rest _)
                     (setq loaded
                           (append
                            loaded
                            (list
                             (intern
                              (file-name-sans-extension
                               (file-name-nondirectory file))))))))
                  ((symbol-function 'message) #'ignore))
          (setq features '(pkg-child pkg-base unrelated-feature))
          (elpaca-extras-reload 'pkg)
          (should (equal loaded '(pkg pkg-base pkg-child))))
      (setq features saved-features))))

(ert-deftest elpaca-extras-test-reload-refuses-live-class-layout-change ()
  "Require a restart before reloading a changed EIEIO class layout."
  (defclass elpaca-extras-test-reload-class () ((existing-slot)))
  (let ((loaded nil)
        (saved-features features))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-file)
                   (lambda (name _path _suffixes)
                     (format "/fake/pkg/%s.el" name)))
                  ((symbol-function 'directory-files)
                   (lambda (_dir _full _pattern)
                     '("/fake/pkg/pkg-class.el")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (_file &rest _)
                     (erase-buffer)
                     (insert
                      "(defclass elpaca-extras-test-reload-class ()\n  ((existing-slot) (new-slot)))\n(provide 'pkg-class)")
                     (goto-char (point-min))))
                  ((symbol-function 'load)
                   (lambda (&rest _) (setq loaded t)))
                  ((symbol-function 'message) #'ignore))
          (setq features '(pkg-class))
          (should-error
           (elpaca-extras-reload 'pkg)
           :type 'elpaca-extras-restart-required)
          (should-not loaded))
      (setq features saved-features))))

(ert-deftest elpaca-extras-test-reload-guards-file-with-trailing-new-provide ()
  "A new trailing feature cannot detach its file from class preflight."
  (defclass elpaca-extras-test-multi-provide-class () ((existing-slot)))
  (let ((loaded nil)
        (saved-features features))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-file)
                   (lambda (name _path _suffixes)
                     (format "/fake/pkg/%s.el" name)))
                  ((symbol-function 'directory-files)
                   (lambda (&rest _)
                     '("/fake/pkg/pkg.el")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (_file &rest _)
                     (insert
                      "(defclass elpaca-extras-test-multi-provide-class () ((existing-slot) (new-slot)))\n(provide 'pkg)\n(provide 'pkg-new-feature)")))
                  ((symbol-function 'load)
                   (lambda (&rest _) (setq loaded t)))
                  ((symbol-function 'message) #'ignore))
          (setq features '(pkg))
          (should-error
           (elpaca-extras-reload 'pkg)
           :type 'elpaca-extras-restart-required)
          (should-not loaded))
      (setq features saved-features))))

(defmacro elpaca-extras-test--define-class (name slots)
  "Define NAME with SLOTS through a non-literal class macro."
  `(defclass ,name () ,slots))

(ert-deftest elpaca-extras-test-reload-runtime-guards-macro-generated-class ()
  "The final runtime guard catches classes hidden behind macros."
  (defclass elpaca-extras-test-macro-class () ((existing-slot)))
  (let ((saved-features features))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-file)
                   (lambda (name _path _suffixes)
                     (format "/fake/pkg/%s.el" name)))
                  ((symbol-function 'directory-files)
                   (lambda (&rest _)
                     '("/fake/pkg/pkg.el")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (_file &rest _)
                     (insert
                      "(elpaca-extras-test--define-class elpaca-extras-test-macro-class ((existing-slot) (new-slot)))\n(provide 'pkg)")))
                  ((symbol-function 'load)
                   (lambda (&rest _)
                     (eval
                      '(elpaca-extras-test--define-class
                        elpaca-extras-test-macro-class
                        ((existing-slot) (new-slot))))))
                  ((symbol-function 'message) #'ignore))
          (setq features '(pkg))
          (should-error
           (elpaca-extras-reload 'pkg)
           :type 'elpaca-extras-restart-required)
          (should
           (equal
            (mapcar #'eieio-slot-descriptor-name
                    (eieio-class-slots
                     (find-class 'elpaca-extras-test-macro-class)))
            '(existing-slot))))
      (setq features saved-features))))

(ert-deftest elpaca-extras-test-runtime-guards-repeated-new-class-definition ()
  "Guard a new class if the same load defines it again after instantiation."
  (let (object)
    (should-error
     (elpaca-extras--call-with-class-layout-guard
      'pkg
      (lambda ()
        (eval
         '(defclass elpaca-extras-test-repeated-new-class
            () ((existing-slot :initarg :existing-slot))))
        (setq object
              (make-instance 'elpaca-extras-test-repeated-new-class
                             :existing-slot 'preserved))
        (eval
         '(defclass elpaca-extras-test-repeated-new-class
            () ((existing-slot :initarg :existing-slot) (new-slot))))))
     :type 'elpaca-extras-restart-required)
    (should (eq (oref object existing-slot) 'preserved))
    (should-not (slot-exists-p object 'new-slot))))

(ert-deftest elpaca-extras-test-preflight-refuses-uninspectable-owned-class ()
  "Refuse a selected file whose previously loaded class is not literal."
  (defclass elpaca-extras-test-owned-macro-class () ((existing-slot)))
  (let ((load-history
         (cons
          '("/fake/pkg.el"
            (define-type . elpaca-extras-test-owned-macro-class)
            (provide . pkg))
          load-history)))
    (should-error
     (elpaca-extras--assert-reloadable-class-layouts
      'pkg
      '((:source "/fake/pkg.el"
         :artifact "/fake/pkg.el"
         :features (pkg)
         :classes nil)))
     :type 'elpaca-extras-restart-required)))

(ert-deftest elpaca-extras-test-source-class-in-eval-when-compile-follows-artifact ()
  "Inspect `eval-when-compile' only when the selected artifact is source."
  (cl-letf (((symbol-function 'insert-file-contents)
             (lambda (_file &rest _)
               (insert
                "(eval-when-compile (defclass source-only-class () ((slot))))\n(provide 'pkg)"))))
    (should
     (equal
      (mapcar (lambda (layout) (plist-get layout :name))
              (plist-get
               (elpaca-extras--source-feature-info
                "/fake/pkg.el" "/fake/pkg.el")
               :classes))
      '(source-only-class)))
    (should-not
     (plist-get
      (elpaca-extras--source-feature-info
       "/fake/pkg.el" "/fake/pkg.elc")
      :classes))))

(ert-deftest elpaca-extras-test-reload-refuses-live-superclass-change ()
  "Require a restart when a loaded class changes its direct superclass."
  (defclass elpaca-extras-test-parent-a () ((inherited-slot)))
  (defclass elpaca-extras-test-parent-b () ((inherited-slot)))
  (defclass elpaca-extras-test-child
    (elpaca-extras-test-parent-a)
    ((child-slot)))
  (let ((loaded nil)
        (saved-features features))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-file)
                   (lambda (name _path _suffixes)
                     (format "/fake/pkg/%s.el" name)))
                  ((symbol-function 'directory-files)
                   (lambda (_dir _full _pattern)
                     '("/fake/pkg/pkg-class.el")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (_file &rest _)
                     (erase-buffer)
                     (insert
                      "(eval-and-compile (defclass elpaca-extras-test-child (elpaca-extras-test-parent-b) ((child-slot))))\n(provide 'pkg-class)")))
                  ((symbol-function 'load)
                   (lambda (&rest _) (setq loaded t)))
                  ((symbol-function 'message) #'ignore))
          (setq features '(pkg-class))
          (should-error
           (elpaca-extras-reload 'pkg)
           :type 'elpaca-extras-restart-required)
          (should-not loaded))
      (setq features saved-features))))

(ert-deftest elpaca-extras-test-reload-allows-safe-slot-overrides ()
  "Allow unchanged overrides and class-allocated slots during reload."
  (defclass elpaca-extras-test-override-parent () ((inherited-slot)))
  (defclass elpaca-extras-test-override-child
    (elpaca-extras-test-override-parent)
    ((inherited-slot :initform nil)
     (own-slot)
     (shared-slot :allocation :class)))
  (let ((loaded nil)
        (saved-features features))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-file)
                   (lambda (name _path _suffixes)
                     (format "/fake/pkg/%s.el" name)))
                  ((symbol-function 'directory-files)
                   (lambda (_dir _full _pattern)
                     '("/fake/pkg/pkg-class.el")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (_file &rest _)
                     (erase-buffer)
                     (insert
                      "(defclass elpaca-extras-test-override-child (elpaca-extras-test-override-parent) ((inherited-slot :initform nil) (own-slot) (shared-slot :allocation :class)))\n(provide 'pkg-class)")))
                  ((symbol-function 'load)
                   (lambda (&rest _) (setq loaded t)))
                  ((symbol-function 'message) #'ignore))
          (setq features '(pkg-class))
          (elpaca-extras-reload 'pkg)
          (should loaded))
      (setq features saved-features))))

(ert-deftest elpaca-extras-test-reload-allows-inherited-class-collision ()
  "Match EIEIO when inherited class slots suppress later instance slots."
  (defclass elpaca-extras-test-class-parent
    ()
    ((collision :allocation :class)))
  (defclass elpaca-extras-test-instance-parent () ((collision)))
  (defclass elpaca-extras-test-collision-child
    (elpaca-extras-test-class-parent elpaca-extras-test-instance-parent)
    ((collision :allocation :instance)))
  (let ((loaded nil)
        (saved-features features))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-file)
                   (lambda (name _path _suffixes)
                     (format "/fake/pkg/%s.el" name)))
                  ((symbol-function 'directory-files)
                   (lambda (_dir _full _pattern)
                     '("/fake/pkg/pkg-class.el")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (_file &rest _)
                     (erase-buffer)
                     (insert
                      "(defclass elpaca-extras-test-collision-child (elpaca-extras-test-class-parent elpaca-extras-test-instance-parent) ((collision :allocation :instance)))\n(provide 'pkg-class)")))
                  ((symbol-function 'load)
                   (lambda (&rest _) (setq loaded t)))
                  ((symbol-function 'message) #'ignore))
          (setq features '(pkg-class))
          (elpaca-extras-reload 'pkg)
          (should loaded))
      (setq features saved-features))))

(ert-deftest elpaca-extras-test-reload-detects-ancestor-class-collision ()
  "Reject a new ancestor class slot that would remove a child instance slot."
  (defclass elpaca-extras-test-new-class-parent () ())
  (defclass elpaca-extras-test-new-class-child
    (elpaca-extras-test-new-class-parent)
    ((collision :allocation :instance)))
  (let ((loaded nil)
        (saved-features features))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-file)
                   (lambda (name _path _suffixes)
                     (format "/fake/pkg/%s.el" name)))
                  ((symbol-function 'directory-files)
                   (lambda (_dir _full _pattern)
                     '("/fake/pkg/pkg-child.el" "/fake/pkg/pkg-parent.el")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (file &rest _)
                     (erase-buffer)
                     (if (string-match-p "parent" file)
                         (insert
                          "(defclass elpaca-extras-test-new-class-parent () ((collision :allocation :class)))\n(provide 'pkg-parent)")
                       (insert
                        "(require 'pkg-parent)\n(defclass elpaca-extras-test-new-class-child (elpaca-extras-test-new-class-parent) ((collision :allocation :instance)))\n(provide 'pkg-child)"))))
                  ((symbol-function 'load)
                   (lambda (&rest _) (setq loaded t)))
                  ((symbol-function 'message) #'ignore))
          (setq features '(pkg-parent pkg-child))
          (should-error
           (elpaca-extras-reload 'pkg)
           :type 'elpaca-extras-restart-required)
          (should-not loaded))
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

(ert-deftest elpaca-extras-test-reload-loads-multi-feature-file-once ()
  "Load one artifact once even when it provides two selected features."
  (let ((load-count 0))
    (cl-letf (((symbol-function 'locate-file)
               (lambda (_name _path _suffixes) "/fake/pkg/pkg.el"))
              ((symbol-function 'directory-files)
               (lambda (&rest _) '("/fake/pkg/pkg.el")))
              ((symbol-function 'insert-file-contents)
               (lambda (_file &rest _)
                 (insert "(provide 'pkg)\n(provide 'pkg-extra)")))
              ((symbol-function 'load)
               (lambda (&rest _) (cl-incf load-count)))
              ((symbol-function 'message) #'ignore))
      (elpaca-extras-reload 'pkg 'allp)
      (should (= load-count 1)))))

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
