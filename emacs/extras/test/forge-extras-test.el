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

;;;; Parse issue fields (GraphQL)

(ert-deftest forge-extras-test-parse-issue-fields-basic ()
  "Parse a complete issue fields response with status and effort."
  (let* ((forge-extras-project-number 1)
         (raw
          `((data
             (repository
              (issue
               (id . "I_abc123")
               (title . "Fix the widget")
               (assignees (nodes ((login . "alice")) ((login . "bob"))))
               (labels (nodes ((name . "bug")) ((name . "urgent"))))
               (projectItems
                (nodes
                 ((project (number . 1))
                  (id . "PVTI_item1")
                  (fieldValues
                   (nodes
                    ((field (name . "Status") (id . "FID_status"))
                     (singleSelectValue . "In Progress")
                     (optionId . "OPT_inprog"))
                    ((field (name . "Estimate") (id . "FID_estimate"))
                     (numberValue . 5)))))))))))))
    (let ((result (forge-extras-gh-parse-issue-fields raw)))
      (should (equal (plist-get result :issue-node-id) "I_abc123"))
      (should (equal (plist-get result :title) "Fix the widget"))
      (should (equal (plist-get result :assignees) '("alice" "bob")))
      (should (equal (plist-get result :labels) '("bug" "urgent")))
      (should (equal (plist-get result :effort) 5))
      (should (equal (plist-get result :status) "In Progress"))
      (should (equal (plist-get result :project-item-id) "PVTI_item1"))
      (should (equal (plist-get result :status-field-id) "FID_status"))
      (should (equal (plist-get result :selected-status-option-id) "OPT_inprog")))))

(ert-deftest forge-extras-test-parse-issue-fields-no-matching-project ()
  "Return nil project fields when no project matches the configured number."
  (let* ((forge-extras-project-number 99)
         (raw
          `((data
             (repository
              (issue
               (id . "I_abc123")
               (title . "Orphan issue")
               (assignees (nodes))
               (labels (nodes))
               (projectItems
                (nodes
                 ((project (number . 1))
                  (id . "PVTI_item1")
                  (fieldValues (nodes)))))))))))
    (let ((result (forge-extras-gh-parse-issue-fields raw)))
      (should (equal (plist-get result :issue-node-id) "I_abc123"))
      (should (equal (plist-get result :title) "Orphan issue"))
      (should (null (plist-get result :project-item-id)))
      (should (null (plist-get result :status)))
      (should (null (plist-get result :effort))))))

(ert-deftest forge-extras-test-parse-issue-fields-empty-field-values ()
  "Handle a matching project item that has no field values."
  (let* ((forge-extras-project-number 5)
         (raw
          `((data
             (repository
              (issue
               (id . "I_xyz")
               (title . "Empty fields")
               (assignees (nodes))
               (labels (nodes))
               (projectItems
                (nodes
                 ((project (number . 5))
                  (id . "PVTI_item5")
                  (fieldValues (nodes)))))))))))
    (let ((result (forge-extras-gh-parse-issue-fields raw)))
      (should (equal (plist-get result :project-item-id) "PVTI_item5"))
      (should (null (plist-get result :status)))
      (should (null (plist-get result :effort))))))

;;;; Parse project fields

(ert-deftest forge-extras-test-parse-project-fields-basic ()
  "Parse a standard project fields response into cons cells."
  (let* ((raw
          `((data
             (node
              (fields
               (nodes ((name . "Title") (id . "FID_1"))
                      ((name . "Status") (id . "FID_2"))
                      ((name . "Estimate") (id . "FID_3")))))))))
    (let ((result (forge-extras-gh-parse-project-fields raw)))
      (should (= (length result) 3))
      (should (equal (car result) '("Title" . "FID_1")))
      (should (equal (cadr result) '("Status" . "FID_2")))
      (should (equal (caddr result) '("Estimate" . "FID_3"))))))

(ert-deftest forge-extras-test-parse-project-fields-empty-nodes ()
  "Return nil when field nodes list is empty."
  (should (null (forge-extras-gh-parse-project-fields '((data (node (fields (nodes)))))))))

(ert-deftest forge-extras-test-parse-project-fields-missing-data ()
  "Return nil when the response has no data key."
  (should (null (forge-extras-gh-parse-project-fields '((errors . "something"))))))

(ert-deftest forge-extras-test-parse-project-fields-missing-node ()
  "Return nil when data has no node key."
  (should (null (forge-extras-gh-parse-project-fields '((data (other . "stuff")))))))

;;;; Get field property value

(ert-deftest forge-extras-test-get-field-property-value-single-pair ()
  "Extract value from a single cons pair when key matches."
  (should (equal (forge-extras--get-field-property-value '(name . "Status") 'name)
                 "Status")))

(ert-deftest forge-extras-test-get-field-property-value-single-pair-mismatch ()
  "Return nil from a single cons pair when key does not match."
  (should (null (forge-extras--get-field-property-value '(name . "Status") 'id))))

(ert-deftest forge-extras-test-get-field-property-value-alist ()
  "Extract value from an alist by key."
  (let ((alist '((name . "Status") (id . "FID_42"))))
    (should (equal (forge-extras--get-field-property-value alist 'name) "Status"))
    (should (equal (forge-extras--get-field-property-value alist 'id) "FID_42"))))

(ert-deftest forge-extras-test-get-field-property-value-alist-missing-key ()
  "Return nil from an alist when key is absent."
  (should (null (forge-extras--get-field-property-value '((name . "Status") (id . "FID_42")) 'color))))

(ert-deftest forge-extras-test-get-field-property-value-nil ()
  "Return nil when field-obj is nil."
  (should (null (forge-extras--get-field-property-value nil 'name))))

;;;; Parse project items

(ert-deftest forge-extras-test-parse-project-items-basic ()
  "Parse a response with one open issue and extract fields."
  (let* ((raw
          `((data
             (node
              (items
               (pageInfo (hasNextPage . :json-false) (endCursor . "cur1"))
               (nodes
                ((content
                  (__typename . "Issue")
                  (state . "OPEN")
                  (repository (nameWithOwner . "owner/repo"))
                  (number . 42)
                  (title . "Fix bug")
                  (url . "https://github.com/owner/repo/issues/42"))
                 (fieldValues
                  (nodes
                   ((field (name . "Status"))
                    (singleSelectValue . "Todo"))
                   ((field (name . "Estimate"))
                    (numberValue . 3))))))))))))
    (let* ((result (forge-extras--parse-project-items raw))
           (items (car result))
           (page-info (cdr result))
           (item (car items)))
      (should (= (length items) 1))
      (should (equal (plist-get item :type) 'issue))
      (should (equal (plist-get item :number) 42))
      (should (equal (plist-get item :title) "Fix bug"))
      (should (equal (plist-get item :url) "https://github.com/owner/repo/issues/42"))
      (should (equal (plist-get item :repo) "owner/repo"))
      (should (equal (plist-get item :state) "OPEN"))
      (should (equal (plist-get item :status) "Todo"))
      (should (equal (plist-get item :estimate) 3))
      (should (equal (cdr (assoc 'hasNextPage page-info)) :json-false))
      (should (equal (cdr (assoc 'endCursor page-info)) "cur1")))))

(ert-deftest forge-extras-test-parse-project-items-filters-closed ()
  "Filter out closed issues when include-closed-p is nil."
  (let* ((raw
          `((data
             (node
              (items
               (pageInfo (hasNextPage . :json-false))
               (nodes
                ((content (__typename . "Issue") (state . "OPEN")
                          (repository (nameWithOwner . "o/r"))
                          (number . 1) (title . "Open") (url . "u1"))
                 (fieldValues (nodes)))
                ((content (__typename . "Issue") (state . "CLOSED")
                          (repository (nameWithOwner . "o/r"))
                          (number . 2) (title . "Closed") (url . "u2"))
                 (fieldValues (nodes))))))))))
    (let* ((result (forge-extras--parse-project-items raw nil nil))
           (items (car result)))
      (should (= (length items) 1))
      (should (= (plist-get (car items) :number) 1)))))

(ert-deftest forge-extras-test-parse-project-items-includes-closed ()
  "Include closed issues when include-closed-p is non-nil."
  (let* ((raw
          `((data
             (node
              (items
               (pageInfo (hasNextPage . :json-false))
               (nodes
                ((content (__typename . "Issue") (state . "OPEN")
                          (repository (nameWithOwner . "o/r"))
                          (number . 1) (title . "Open") (url . "u1"))
                 (fieldValues (nodes)))
                ((content (__typename . "Issue") (state . "CLOSED")
                          (repository (nameWithOwner . "o/r"))
                          (number . 2) (title . "Closed") (url . "u2"))
                 (fieldValues (nodes))))))))))
    (let* ((result (forge-extras--parse-project-items raw nil t))
           (items (car result)))
      (should (= (length items) 2)))))

(ert-deftest forge-extras-test-parse-project-items-filters-by-repo ()
  "Filter items to a specific repository."
  (let* ((raw
          `((data
             (node
              (items
               (pageInfo (hasNextPage . :json-false))
               (nodes
                ((content (__typename . "Issue") (state . "OPEN")
                          (repository (nameWithOwner . "owner/repo-a"))
                          (number . 1) (title . "A") (url . "u1"))
                 (fieldValues (nodes)))
                ((content (__typename . "Issue") (state . "OPEN")
                          (repository (nameWithOwner . "owner/repo-b"))
                          (number . 2) (title . "B") (url . "u2"))
                 (fieldValues (nodes))))))))))
    (let* ((result (forge-extras--parse-project-items raw "owner/repo-a"))
           (items (car result)))
      (should (= (length items) 1))
      (should (equal (plist-get (car items) :repo) "owner/repo-a")))))

(ert-deftest forge-extras-test-parse-project-items-pull-request ()
  "Parse PullRequest items and filter merged PRs when not including closed."
  (let* ((raw
          `((data
             (node
              (items
               (pageInfo (hasNextPage . :json-false))
               (nodes
                ((content (__typename . "PullRequest") (state . "OPEN")
                          (repository (nameWithOwner . "o/r"))
                          (number . 10) (title . "Open PR") (url . "u1"))
                 (fieldValues (nodes)))
                ((content (__typename . "PullRequest") (state . "MERGED")
                          (repository (nameWithOwner . "o/r"))
                          (number . 11) (title . "Merged PR") (url . "u2"))
                 (fieldValues (nodes))))))))))
    (let* ((result (forge-extras--parse-project-items raw nil nil))
           (items (car result)))
      (should (= (length items) 1))
      (should (equal (plist-get (car items) :type) 'pullreq))
      (should (= (plist-get (car items) :number) 10)))))

(ert-deftest forge-extras-test-parse-project-items-empty-response ()
  "Return empty items and nil page-info for a nil response."
  (let* ((result (forge-extras--parse-project-items nil)))
    (should (null (car result)))
    (should (null (cdr result)))))

(ert-deftest forge-extras-test-parse-project-items-skips-non-issue-types ()
  "Skip items whose __typename is not Issue or PullRequest."
  (let* ((raw
          `((data
             (node
              (items
               (pageInfo (hasNextPage . :json-false))
               (nodes
                ((content (__typename . "DraftIssue") (state . "OPEN")
                          (repository (nameWithOwner . "o/r"))
                          (number . 1) (title . "Draft") (url . "u1"))
                 (fieldValues (nodes))))))))))
    (let* ((result (forge-extras--parse-project-items raw))
           (items (car result)))
      (should (= (length items) 0)))))

;;;; Parse project status options

(ert-deftest forge-extras-test-parse-project-status-options-basic ()
  "Parse status options into (name . id) cons cells."
  (let* ((raw
          `((data
             (node
              (options ((name . "Todo") (id . "OPT_1"))
                       ((name . "In Progress") (id . "OPT_2"))
                       ((name . "Done") (id . "OPT_3"))))))))
    (let ((result (forge-extras--parse-project-status-options raw)))
      (should (= (length result) 3))
      (should (equal (car result) '("Todo" . "OPT_1")))
      (should (equal (cadr result) '("In Progress" . "OPT_2")))
      (should (equal (caddr result) '("Done" . "OPT_3"))))))

(ert-deftest forge-extras-test-parse-project-status-options-empty ()
  "Return nil when options list is empty."
  (should (null (forge-extras--parse-project-status-options '((data (node (options))))))))

(ert-deftest forge-extras-test-parse-project-status-options-missing-data ()
  "Return nil when the response has no data key."
  (should (null (forge-extras--parse-project-status-options '((errors . "bad"))))))

(ert-deftest forge-extras-test-parse-project-status-options-missing-node ()
  "Return nil when data has no node key."
  (should (null (forge-extras--parse-project-status-options '((data (other . "stuff")))))))

(ert-deftest forge-extras-test-parse-project-status-options-single ()
  "Parse a response with a single status option."
  (let* ((raw
          `((data
             (node
              (options ((name . "Backlog") (id . "OPT_only"))))))))
    (let ((result (forge-extras--parse-project-status-options raw)))
      (should (= (length result) 1))
      (should (equal (car result) '("Backlog" . "OPT_only"))))))

(provide 'forge-extras-test)
;;; forge-extras-test.el ends here
