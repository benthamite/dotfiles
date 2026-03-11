;;; bbdb-extras-test.el --- Tests for bbdb-extras -*- lexical-binding: t -*-

;; Tests for contact management helpers in bbdb-extras.el.
;; bbdb-extras depends on bbdb, which requires bbdb-site (unavailable
;; in batch mode), so we inline the browse-url logic for testing.

;;; Code:

(require 'ert)

;;;; Inlined pure logic

;; Extracted from `bbdb-extras-browse-url' for standalone testing.
;; The original iterates over records, looks up the URL xfield, selects
;; the nth entry, and browses it unless it is empty.
(defun bbdb-extras-test--browse-url (records which record-list-fn xfield-split-fn browse-fn)
  "Test harness mirroring `bbdb-extras-browse-url' logic.
RECORDS is passed to RECORD-LIST-FN.  XFIELD-SPLIT-FN extracts URLs.
WHICH selects the nth URL (default 0).  BROWSE-FN is called with the URL."
  (unless which (setq which 0))
  (dolist (record (funcall record-list-fn records))
    (let ((url (funcall xfield-split-fn record 'url)))
      (when url
        (setq url (nth which url))
        (unless (string= "" url)
          (funcall browse-fn url))))))

;;;; browse-url - URL selection logic

(ert-deftest bbdb-extras-test-browse-url-first-url ()
  "With which=0, the first URL is browsed."
  (let ((browsed-url nil))
    (bbdb-extras-test--browse-url
     '(fake-record) 0
     (lambda (records) records)
     (lambda (_record _field) '("https://example.com" "https://other.com"))
     (lambda (url) (setq browsed-url url)))
    (should (equal browsed-url "https://example.com"))))

(ert-deftest bbdb-extras-test-browse-url-second-url ()
  "With which=1, the second URL is browsed."
  (let ((browsed-url nil))
    (bbdb-extras-test--browse-url
     '(fake-record) 1
     (lambda (records) records)
     (lambda (_record _field) '("https://example.com" "https://other.com"))
     (lambda (url) (setq browsed-url url)))
    (should (equal browsed-url "https://other.com"))))

(ert-deftest bbdb-extras-test-browse-url-empty-string-skipped ()
  "An empty URL string is skipped and the browse function is not called."
  (let ((browsed-url nil))
    (bbdb-extras-test--browse-url
     '(fake-record) 0
     (lambda (records) records)
     (lambda (_record _field) '(""))
     (lambda (url) (setq browsed-url url)))
    (should-not browsed-url)))

(ert-deftest bbdb-extras-test-browse-url-no-url-field ()
  "When the record has no URL field, nothing is browsed."
  (let ((browsed-url nil))
    (bbdb-extras-test--browse-url
     '(fake-record) 0
     (lambda (records) records)
     (lambda (_record _field) nil)
     (lambda (url) (setq browsed-url url)))
    (should-not browsed-url)))

(ert-deftest bbdb-extras-test-browse-url-defaults-to-first ()
  "When which is nil, it defaults to 0 (first URL)."
  (let ((browsed-url nil))
    (bbdb-extras-test--browse-url
     '(fake-record) nil
     (lambda (records) records)
     (lambda (_record _field) '("https://default.com" "https://second.com"))
     (lambda (url) (setq browsed-url url)))
    (should (equal browsed-url "https://default.com"))))

(ert-deftest bbdb-extras-test-browse-url-multiple-records ()
  "All records are iterated; the last record's URL wins for single capture."
  (let ((browsed-urls nil))
    (bbdb-extras-test--browse-url
     '(record-1 record-2) 0
     (lambda (records) records)
     (lambda (record _field)
       (if (eq record 'record-1)
           '("https://first.com")
         '("https://second.com")))
     (lambda (url) (push url browsed-urls)))
    (should (equal (nreverse browsed-urls)
                   '("https://first.com" "https://second.com")))))

;;;; Functions are defined (when bbdb loads)

(ert-deftest bbdb-extras-test-functions-are-defined ()
  "All public bbdb-extras functions are defined when the package loads."
  (skip-unless (require 'bbdb-extras nil t))
  (should (fboundp 'bbdb-extras-browse-url))
  (should (fboundp 'bbdb-extras-create-quick))
  (should (fboundp 'bbdb-extras-read-record-quick))
  (should (fboundp 'bbdb-extras-delete-field-or-record-no-confirm))
  (should (fboundp 'bbdb-extras-export-vcard))
  (should (fboundp 'bbdb-extras-pop-up-window)))

(provide 'bbdb-extras-test)
;;; bbdb-extras-test.el ends here
