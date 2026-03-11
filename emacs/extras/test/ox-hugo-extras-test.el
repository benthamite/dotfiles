;;; ox-hugo-extras-test.el --- Tests for ox-hugo-extras -*- lexical-binding: t -*-

;; Tests for Hugo citation export processors in ox-hugo-extras.el.

;;; Code:

(require 'ert)
(require 'ox-hugo-extras)

;;;; Helper functions

(defun ox-hugo-extras-test--make-reference (key &optional suffix)
  "Build a mock org-element citation-reference with KEY and optional SUFFIX."
  (let ((ref (org-element-create 'citation-reference)))
    (org-element-put-property ref :key key)
    (when suffix
      (org-element-put-property ref :suffix (list suffix)))
    ref))

(defun ox-hugo-extras-test--make-citation (references &optional style-variant)
  "Build a mock org-element citation containing REFERENCES.
STYLE-VARIANT, if non-nil, is the citation style string (e.g. \"t\")."
  (let ((citation (org-element-create 'citation)))
    (when style-variant
      (org-element-put-property citation :style style-variant))
    (dolist (ref references)
      (org-element-adopt citation ref))
    citation))

(defun ox-hugo-extras-test--export-citation (processor-name citation style)
  "Call the export-citation function of PROCESSOR-NAME on CITATION with STYLE."
  (let* ((proc (org-cite-get-processor processor-name))
         (fn (org-cite-processor-export-citation proc)))
    (funcall fn citation style nil nil)))

;;;; Processor registration

(ert-deftest ox-hugo-extras-test-hugo-cite-registered ()
  "The `hugo-cite' processor should be registered with org-cite."
  (should (org-cite-get-processor 'hugo-cite)))

(ert-deftest ox-hugo-extras-test-hugo-cite-noop-registered ()
  "The `hugo-cite-noop' processor should be registered with org-cite."
  (should (org-cite-get-processor 'hugo-cite-noop)))

;;;; hugo-cite export-citation

(ert-deftest ox-hugo-extras-test-hugo-cite-basic ()
  "A citation with no style should produce a basic Hugo cite shortcode."
  (let* ((ref (ox-hugo-extras-test--make-reference "Smith2020"))
         (citation (ox-hugo-extras-test--make-citation (list ref)))
         (result (ox-hugo-extras-test--export-citation
                  'hugo-cite citation '(nil))))
    (should (equal result "{{< cite \"Smith2020\" >}}"))))

(ert-deftest ox-hugo-extras-test-hugo-cite-short-style-t ()
  "Style \"t\" should produce a short citation with a dash prefix."
  (let* ((ref (ox-hugo-extras-test--make-reference "Smith2020"))
         (citation (ox-hugo-extras-test--make-citation (list ref)))
         (result (ox-hugo-extras-test--export-citation
                  'hugo-cite citation '("t"))))
    (should (equal result "{{< cite \"-Smith2020\" >}}"))))

(ert-deftest ox-hugo-extras-test-hugo-cite-short-style-short ()
  "Style \"short\" should produce a short citation with a dash prefix."
  (let* ((ref (ox-hugo-extras-test--make-reference "Jones2021"))
         (citation (ox-hugo-extras-test--make-citation (list ref)))
         (result (ox-hugo-extras-test--export-citation
                  'hugo-cite citation '("short"))))
    (should (equal result "{{< cite \"-Jones2021\" >}}"))))

(ert-deftest ox-hugo-extras-test-hugo-cite-with-suffix ()
  "A citation with suffix text should include it as a second argument."
  (let* ((ref (ox-hugo-extras-test--make-reference "Key2022" "p. 42"))
         (citation (ox-hugo-extras-test--make-citation (list ref)))
         (result (ox-hugo-extras-test--export-citation
                  'hugo-cite citation '(nil))))
    (should (equal result "{{< cite \"Key2022\" \"p. 42\" >}}"))))

(ert-deftest ox-hugo-extras-test-hugo-cite-multiple-refs ()
  "Multiple references should be joined with a comma separator."
  (let* ((ref1 (ox-hugo-extras-test--make-reference "Alpha2020"))
         (ref2 (ox-hugo-extras-test--make-reference "Beta2021"))
         (citation (ox-hugo-extras-test--make-citation (list ref1 ref2)))
         (result (ox-hugo-extras-test--export-citation
                  'hugo-cite citation '(nil))))
    (should (equal result "{{< cite \"Alpha2020\" >}}, {{< cite \"Beta2021\" >}}"))))

;;;; hugo-cite-noop export-citation

(ert-deftest ox-hugo-extras-test-hugo-cite-noop-returns-empty ()
  "The noop processor should return an empty string for any citation."
  (let* ((ref (ox-hugo-extras-test--make-reference "Anything2020"))
         (citation (ox-hugo-extras-test--make-citation (list ref)))
         (result (ox-hugo-extras-test--export-citation
                  'hugo-cite-noop citation '(nil))))
    (should (equal result ""))))

(provide 'ox-hugo-extras-test)
;;; ox-hugo-extras-test.el ends here
