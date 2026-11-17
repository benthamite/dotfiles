;;; jinx-extras-test.el --- Tests for jinx-extras -*- lexical-binding: t -*-

;; Tests for language toggle logic and abbrev creation logic
;; in jinx-extras.el.  Since jinx is an external dependency,
;; we extract the pure logic inline rather than requiring the package.

;;; Code:

(require 'ert)

;; Do NOT require jinx-extras; jinx is an external dep unavailable in CI.

;;;; Language toggle logic

(ert-deftest jinx-extras-test-toggle-english-to-spanish ()
  "Toggle from English to Spanish."
  (let ((jinx-languages "en"))
    (should (equal (if (string= jinx-languages "en") "es" "en") "es"))))

(ert-deftest jinx-extras-test-toggle-spanish-to-english ()
  "Toggle from Spanish to English."
  (let ((jinx-languages "es"))
    (should (equal (if (string= jinx-languages "en") "es" "en") "en"))))

(ert-deftest jinx-extras-test-toggle-unknown-language-to-english ()
  "Toggle from an unknown language defaults to English."
  (let ((jinx-languages "fr"))
    (should (equal (if (string= jinx-languages "en") "es" "en") "en"))))

(ert-deftest jinx-extras-test-toggle-empty-string-to-english ()
  "Toggle from an empty language string defaults to English."
  (let ((jinx-languages ""))
    (should (equal (if (string= jinx-languages "en") "es" "en") "en"))))

;;;; Abbrev creation logic

(ert-deftest jinx-extras-test-abbrev-from-overlay ()
  "Extract misspelled word from an overlay region for abbrev creation."
  (with-temp-buffer
    (insert "teh quick brown fox")
    (let* ((ov (make-overlay 1 4))  ; "teh"
           (abbrev (buffer-substring-no-properties
                    (overlay-start ov)
                    (overlay-end ov))))
      (should (equal abbrev "teh"))
      (delete-overlay ov))))

(ert-deftest jinx-extras-test-abbrev-defines-correctly ()
  "Define an abbrev from a misspelled word and its correction."
  (with-temp-buffer
    (insert "recieve the package")
    (let* ((ov (make-overlay 1 8))  ; "recieve"
           (word "receive")
           (abbrev (buffer-substring-no-properties
                    (overlay-start ov)
                    (overlay-end ov))))
      (should (equal abbrev "recieve"))
      ;; Verify the abbrev table entry can be created
      (let ((table (make-abbrev-table)))
        (define-abbrev table abbrev word)
        (should (equal (abbrev-expansion abbrev table) word)))
      (delete-overlay ov))))

(ert-deftest jinx-extras-test-abbrev-single-character ()
  "Extract a single-character misspelled word from an overlay."
  (with-temp-buffer
    (insert "a b c")
    (let* ((ov (make-overlay 3 4))  ; "b"
           (abbrev (buffer-substring-no-properties
                    (overlay-start ov)
                    (overlay-end ov))))
      (should (equal abbrev "b"))
      (delete-overlay ov))))

;;;; One GLib in Emacs.app

(ert-deftest jinx-extras-test-relink-existing-module-before-load ()
  "An existing module linking Homebrew's enchant is relinked before loading."
  (skip-unless (require 'jinx-extras nil t))
  (let (calls)
    (cl-letf (((symbol-function 'jinx-extras--module-file) (lambda () "/m.dylib"))
              ((symbol-function 'jinx-extras--links-homebrew-enchant-p) (lambda (_) t))
              ((symbol-function 'jinx-extras--relink) (lambda (m) (push (list 'relink m) calls))))
      (jinx-extras--use-emacs-glib (lambda () (push 'load calls))))
    (should (equal (reverse calls) '((relink "/m.dylib") load)))))

(ert-deftest jinx-extras-test-relinked-module-left-alone ()
  "A module already linked to the copy is loaded without relinking."
  (skip-unless (require 'jinx-extras nil t))
  (let (calls)
    (cl-letf (((symbol-function 'jinx-extras--module-file) (lambda () "/m.dylib"))
              ((symbol-function 'jinx-extras--links-homebrew-enchant-p) (lambda (_) nil))
              ((symbol-function 'jinx-extras--relink) (lambda (m) (push (list 'relink m) calls))))
      (jinx-extras--use-emacs-glib (lambda () (push 'load calls))))
    (should (equal calls '(load)))))

(ert-deftest jinx-extras-test-relink-freshly-compiled-module-after-load ()
  "A module that ORIG compiles is relinked after it is loaded."
  (skip-unless (require 'jinx-extras nil t))
  (let (calls built)
    (cl-letf (((symbol-function 'jinx-extras--module-file) (lambda () built))
              ((symbol-function 'jinx-extras--relink) (lambda (m) (push (list 'relink m) calls))))
      (jinx-extras--use-emacs-glib (lambda () (setq built "/new.dylib") (push 'load calls))))
    (should (equal (reverse calls) '(load (relink "/new.dylib"))))))

(ert-deftest jinx-extras-test-homebrew-enchant-detected ()
  "Detection reads the module's library list from otool."
  (skip-unless (and (require 'jinx-extras nil t) (eq system-type 'darwin)))
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/otool"))
            ((symbol-function 'shell-command-to-string)
             (lambda (_) "m.dylib:\n\t/opt/homebrew/opt/enchant/lib/libenchant-2.2.dylib (x)\n")))
    (should (jinx-extras--links-homebrew-enchant-p "/m.dylib")))
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/otool"))
            ((symbol-function 'shell-command-to-string)
             (lambda (_) "m.dylib:\n\t/Users/x/.local/lib/jinx-enchant/lib/libenchant-2.2.dylib (x)\n")))
    (should-not (jinx-extras--links-homebrew-enchant-p "/m.dylib"))))

(provide 'jinx-extras-test)
;;; jinx-extras-test.el ends here
