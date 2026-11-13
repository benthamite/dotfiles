;;; org-linear-extras-test.el --- Tests for org-linear-extras -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the model-backed merge of description edits.  `gptel-request'
;; is replaced, so no request leaves Emacs.

;;; Code:

(require 'ert)
(require 'org-linear-extras)

(ert-deftest org-linear-extras-test-merge-result-strips-fence ()
  "A fenced reply yields the text inside the fence."
  (should (equal (org-linear-extras--merge-result "```org\n* not a heading\nText.\n```\n")
                 "* not a heading\nText."))
  (should (equal (org-linear-extras--merge-result "Plain text.\n") "Plain text.")))

(ert-deftest org-linear-extras-test-merge-result-empty-is-nil ()
  "An empty or non-string reply yields nil."
  (should-not (org-linear-extras--merge-result nil))
  (should-not (org-linear-extras--merge-result "  \n"))
  (should-not (org-linear-extras--merge-result 'abort)))

(ert-deftest org-linear-extras-test-merge-body-sends-all-three-texts ()
  "The request carries the three texts and the reply reaches the callback."
  (let (prompt system result)
    (cl-letf (((symbol-function 'gptel-get-backend) (lambda (_name) 'backend))
              ((symbol-function 'gptel-request)
               (lambda (text &rest args)
                 (setq prompt text
                       system (plist-get args :system))
                 (funcall (plist-get args :callback) "Merged." nil))))
      (org-linear-extras-merge-body "ORG BODY" "OLD MD" "NEW MD"
                                    (lambda (merged) (setq result merged))))
    (should (string-match-p "ORG BODY" prompt))
    (should (string-match-p "OLD MD" prompt))
    (should (string-match-p "NEW MD" prompt))
    (should (equal system org-linear-extras-merge-system))
    (should (equal result "Merged."))))

(ert-deftest org-linear-extras-test-merge-body-failure-gives-nil ()
  "A failed request calls back with nil."
  (let ((result 'unset))
    (cl-letf (((symbol-function 'gptel-get-backend) (lambda (_name) 'backend))
              ((symbol-function 'gptel-request)
               (lambda (_text &rest args) (funcall (plist-get args :callback) nil '(:status "error")))))
      (org-linear-extras-merge-body "a" "b" "c" (lambda (merged) (setq result merged))))
    (should-not result)))

(provide 'org-linear-extras-test)
;;; org-linear-extras-test.el ends here
