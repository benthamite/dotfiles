;;; elpaca-source-guard-test.el --- Tests for elpaca-source-guard -*- lexical-binding: t -*-

;;; Commentary:

;; The guard on invented orders, with directories made here.  Elpaca's
;; accessors and its deletion are stood in for when Elpaca is not loaded;
;; with Elpaca, only the guard runs before its deletion, which is refused.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elpaca-source-guard)

(defvar elpaca-sources-directory)

;; Without Elpaca, stand-ins for its order accessors and its deletion; with
;; it, the tests build real orders, since its accessors are inlined.
(unless (fboundp 'elpaca<--create)
  (defun elpaca<-id (e)
    "Stand-in for Elpaca's accessor: E's id."
    (plist-get e :id))
  (defun elpaca<-source-dir (e)
    "Stand-in for Elpaca's accessor: E's source directory."
    (plist-get e :source-dir)))

(unless (fboundp 'elpaca--delete)
  (defun elpaca--delete (e)
    "Stand-in for Elpaca's deletion: delete E's source directory."
    (delete-directory (elpaca<-source-dir e) t)))

(defun elpaca-source-guard-test--order (source)
  "Return an order for the package `pkg' whose source directory is SOURCE."
  (if (fboundp 'elpaca<--create)
      (funcall 'elpaca<--create :id 'pkg :package "pkg" :source-dir source)
    (list :id 'pkg :source-dir source)))

(defmacro elpaca-source-guard-test--with-dirs (&rest body)
  "Run BODY with `sources' and `outside' bound to fresh directories.
`elpaca-sources-directory' is `sources', and an order's source directory
is made with `elpaca-source-guard-test--order'."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "elpaca-source-guard-" t))
          (sources (file-name-as-directory (expand-file-name "sources" root)))
          (outside (file-name-as-directory (expand-file-name "engine" root)))
          (elpaca-sources-directory sources))
     (ignore outside)
     (make-directory (expand-file-name "pkg" sources) t)
     (make-directory outside t)
     (unwind-protect
         (progn ,@body)
       (delete-directory root t))))

(ert-deftest elpaca-source-guard-test-refuses-outside-source ()
  "A source outside Elpaca's sources directory is refused."
  (elpaca-source-guard-test--with-dirs
    (should-error (elpaca-source-guard-refuse
                   (elpaca-source-guard-test--order outside))
                  :type 'user-error)))

(ert-deftest elpaca-source-guard-test-allows-own-source ()
  "A source inside Elpaca's sources directory is allowed."
  (elpaca-source-guard-test--with-dirs
    (should-not (elpaca-source-guard-refuse
                 (elpaca-source-guard-test--order (expand-file-name "pkg/" sources))))))

(ert-deftest elpaca-source-guard-test-link-into-sources-is-refused ()
  "A link inside the sources directory to an outside checkout is refused."
  (elpaca-source-guard-test--with-dirs
    (let ((link (expand-file-name "linked" sources)))
      (make-symbolic-link (directory-file-name outside) link)
      (should-error (elpaca-source-guard-refuse
                     (elpaca-source-guard-test--order link))
                    :type 'user-error))))

(ert-deftest elpaca-source-guard-test-mode-keeps-outside-checkout ()
  "With the mode on, deleting an outside package deletes nothing."
  (elpaca-source-guard-test--with-dirs
    (unwind-protect
        (progn
          (elpaca-source-guard-mode 1)
          (should-error (elpaca--delete (elpaca-source-guard-test--order outside))
                        :type 'user-error)
          (should (file-directory-p outside)))
      (elpaca-source-guard-mode -1))
    (should-not (advice-member-p #'elpaca-source-guard-refuse 'elpaca--delete))))

(provide 'elpaca-source-guard-test)
;;; elpaca-source-guard-test.el ends here
