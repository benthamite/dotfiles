;;; org-web-tools-extras-test.el --- Tests for org-web-tools-extras -*- lexical-binding: t -*-

;; Tests for web tool helpers in org-web-tools-extras.el.

;;; Code:

(require 'ert)
(require 'org-web-tools-extras)

;;;; Feature and function existence

(ert-deftest org-web-tools-extras-test-feature-provided ()
  "The `org-web-tools-extras' feature should be provided."
  (should (featurep 'org-web-tools-extras)))

(ert-deftest org-web-tools-extras-test-insert-link-defined ()
  "The `org-web-tools-extras-insert-link-for-clipboard-url' function should be defined."
  (should (fboundp 'org-web-tools-extras-insert-link-for-clipboard-url)))

(ert-deftest org-web-tools-extras-test-org-title-for-url-defined ()
  "The `org-web-tools-extras-org-title-for-url' function should be defined."
  (should (fboundp 'org-web-tools-extras-org-title-for-url)))

;;;; org-web-tools-extras-org-title-for-url

(ert-deftest org-web-tools-extras-test-org-title-for-url-returns-title ()
  "The function should extract and return the title from a parsed HTML DOM."
  (let ((mock-dom '(html nil
                         (head nil
                               (title nil "Test Page Title"))
                         (body nil
                               (p nil "content")))))
    (cl-letf (((symbol-function 'plz)
               (lambda (_method _url &rest _args) mock-dom))
              ((symbol-function 'org-web-tools--cleanup-title)
               #'identity))
      (should (equal (org-web-tools-extras-org-title-for-url "https://example.com")
                     "Test Page Title")))))

(ert-deftest org-web-tools-extras-test-org-title-for-url-fallback-on-error ()
  "The function should return \"Downloaded webpage\" when plz signals an error."
  (cl-letf (((symbol-function 'plz)
             (lambda (_method _url &rest _args) (error "Network error")))
            ((symbol-function 'org-web-tools--cleanup-title)
             #'identity))
    (should (equal (org-web-tools-extras-org-title-for-url "https://example.com")
                   "Downloaded webpage"))))

(ert-deftest org-web-tools-extras-test-org-title-for-url-no-title-element ()
  "When plz succeeds but DOM has no title element, the result is nil.
The `if-let*' only gates on the plz result being non-nil; when the DOM
lacks a title, `cl-caddr' of nil yields nil, which is passed to the
cleanup function."
  (let ((mock-dom '(html nil
                         (head nil)
                         (body nil (p nil "no title here")))))
    (cl-letf (((symbol-function 'plz)
               (lambda (_method _url &rest _args) mock-dom))
              ((symbol-function 'org-web-tools--cleanup-title)
               #'identity))
      (should-not (org-web-tools-extras-org-title-for-url "https://example.com")))))

(provide 'org-web-tools-extras-test)
;;; org-web-tools-extras-test.el ends here
