;;; bibtex-completion-extras-test.el --- Tests for bibtex-completion-extras -*- lexical-binding: t -*-

;; Tests for bibtex-completion patches in bibtex-completion-extras.el.

;;; Code:

(require 'ert)
(require 'bibtex-completion-extras)

;;;; Feature and function existence

(ert-deftest bibtex-completion-extras-test-feature-provided ()
  "The `bibtex-completion-extras' feature should be provided."
  (should (featurep 'bibtex-completion-extras)))

(ert-deftest bibtex-completion-extras-test-init-defined ()
  "The patched `bibtex-completion-init' function should be defined."
  (should (fboundp 'bibtex-completion-init)))

;;;; Patched behavior: no file watchers

(ert-deftest bibtex-completion-extras-test-init-no-file-watchers ()
  "The patched `bibtex-completion-init' should not set up file watchers.
We mock the bibliography normalization to return a dummy file list and
verify that `file-notify-add-watch' is never called."
  (let ((watch-called nil)
        (bibtex-completion-bibliography nil)
        (bibtex-completion-file-watch-descriptors nil)
        (bibtex-completion-watch-bibliography t)
        (bibtex-completion-display-formats '((t . "${author} ${title} ${year}"))))
    (cl-letf (((symbol-function 'bibtex-completion-normalize-bibliography)
               (lambda () (list "/tmp/fake.bib")))
              ((symbol-function 'file-notify-add-watch)
               (lambda (&rest _) (setq watch-called t) nil))
              ((symbol-function 'file-notify-rm-watch)
               #'ignore)
              ((symbol-function 'f-file?)
               (lambda (_) t)))
      (bibtex-completion-init)
      (should-not watch-called))))

(provide 'bibtex-completion-extras-test)
;;; bibtex-completion-extras-test.el ends here
