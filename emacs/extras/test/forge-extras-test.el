;;; forge-extras-test.el --- Tests for forge-extras -*- lexical-binding: t -*-

;; Tests for GitHub issue parsing, GraphQL data transformation,
;; and project field extraction in forge-extras.el.

;;; Code:

(require 'ert)
(require 'forge-extras)

;;;; Parse gh issue JSON

(ert-deftest forge-extras-test-parse-gh-issue-json-basic ()
  "Parse-gh-issue-json extracts issue plists from JSON array."
  (let* ((json "[{\"number\":1,\"title\":\"Bug fix\",\"url\":\"https://github.com/owner/repo/issues/1\"}]")
         (result (forge-extras--parse-gh-issue-json json "owner/repo")))
    (should (= (length result) 1))
    (should (equal (plist-get (car result) :repo) "owner/repo"))
    (should (= (plist-get (car result) :number) 1))
    (should (equal (plist-get (car result) :title) "Bug fix"))
    (should (equal (plist-get (car result) :url) "https://github.com/owner/repo/issues/1"))))

(ert-deftest forge-extras-test-parse-gh-issue-json-multiple ()
  "Parse-gh-issue-json handles multiple issues."
  (let* ((json "[{\"number\":1,\"title\":\"First\",\"url\":\"u1\"},{\"number\":2,\"title\":\"Second\",\"url\":\"u2\"}]")
         (result (forge-extras--parse-gh-issue-json json "test/repo")))
    (should (= (length result) 2))
    (should (= (plist-get (car result) :number) 1))
    (should (= (plist-get (cadr result) :number) 2))))

(ert-deftest forge-extras-test-parse-gh-issue-json-empty ()
  "Parse-gh-issue-json returns nil for empty array."
  (let ((result (forge-extras--parse-gh-issue-json "[]" "test/repo")))
    (should (null result))))

(ert-deftest forge-extras-test-parse-gh-issue-json-missing-number ()
  "Parse-gh-issue-json skips entries without a number."
  (let* ((json "[{\"title\":\"No number\",\"url\":\"u1\"},{\"number\":2,\"title\":\"Has number\",\"url\":\"u2\"}]")
         (result (forge-extras--parse-gh-issue-json json "test/repo")))
    (should (= (length result) 1))
    (should (= (plist-get (car result) :number) 2))))

(ert-deftest forge-extras-test-parse-gh-issue-json-invalid ()
  "Parse-gh-issue-json returns nil for invalid JSON."
  (let ((result (forge-extras--parse-gh-issue-json "not json" "test/repo")))
    (should (null result))))

;;;; Parse gh issue table

(ert-deftest forge-extras-test-parse-gh-issue-table-basic ()
  "Parse-gh-issue-table parses standard gh output lines."
  (let* ((table "#1\tFix login bug\tOpen")
         (result (forge-extras--parse-gh-issue-table table "owner/repo")))
    (should (= (length result) 1))
    (should (= (plist-get (car result) :number) 1))
    (should (equal (plist-get (car result) :title) "Fix login bug"))
    (should (equal (plist-get (car result) :repo) "owner/repo"))
    (should (string-match-p "owner/repo/issues/1" (plist-get (car result) :url)))))

(ert-deftest forge-extras-test-parse-gh-issue-table-multiple ()
  "Parse-gh-issue-table parses multiple lines."
  (let* ((table "1\tFirst issue\tOpen\n2\tSecond issue\tClosed\n3\tThird issue\tOpen")
         (result (forge-extras--parse-gh-issue-table table "test/repo")))
    (should (= (length result) 3))
    (should (= (plist-get (car result) :number) 1))
    (should (= (plist-get (caddr result) :number) 3))))

(ert-deftest forge-extras-test-parse-gh-issue-table-strips-state ()
  "Parse-gh-issue-table strips trailing state words from title."
  (let* ((table "1\tSome issue title\tOpen")
         (result (forge-extras--parse-gh-issue-table table "test/repo")))
    (should (equal (plist-get (car result) :title) "Some issue title"))))

(ert-deftest forge-extras-test-parse-gh-issue-table-empty ()
  "Parse-gh-issue-table returns nil for empty string."
  (let ((result (forge-extras--parse-gh-issue-table "" "test/repo")))
    (should (null result))))

(ert-deftest forge-extras-test-parse-gh-issue-table-constructs-url ()
  "Parse-gh-issue-table constructs proper GitHub URLs."
  (let* ((table "42\tTest issue\tOpen")
         (result (forge-extras--parse-gh-issue-table table "owner/repo")))
    (should (equal (plist-get (car result) :url) "https://github.com/owner/repo/issues/42"))))

(provide 'forge-extras-test)
;;; forge-extras-test.el ends here
