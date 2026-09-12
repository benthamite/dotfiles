;;; gptel-extras-test.el --- Tests for gptel-extras -*- lexical-binding: t -*-

;; Tests for pure and near-pure helper functions in gptel-extras.el.

;;; Code:

(require 'ert)
(require 'gptel-extras)

;;;; Generate next heading

(ert-deftest gptel-extras-test-generate-next-heading-no-number ()
  "Append \" 2\" when heading has no numeric suffix."
  (should (equal (gptel-extras--generate-next-heading "My conversation")
                 "My conversation 2")))

(ert-deftest gptel-extras-test-generate-next-heading-increment ()
  "Increment the trailing number when present."
  (should (equal (gptel-extras--generate-next-heading "My conversation 2")
                 "My conversation 3")))

(ert-deftest gptel-extras-test-generate-next-heading-large-number ()
  "Increment a large trailing number correctly."
  (should (equal (gptel-extras--generate-next-heading "Chat 99")
                 "Chat 100")))

(ert-deftest gptel-extras-test-generate-next-heading-number-one ()
  "Increment from 1 to 2."
  (should (equal (gptel-extras--generate-next-heading "Session 1")
                 "Session 2")))

(ert-deftest gptel-extras-test-generate-next-heading-number-in-middle ()
  "A number in the middle of the heading is not treated as a suffix."
  (should (equal (gptel-extras--generate-next-heading "Chapter 3 notes")
                 "Chapter 3 notes 2")))

(ert-deftest gptel-extras-test-generate-next-heading-empty-string ()
  "Empty string gets \" 2\" appended."
  (should (equal (gptel-extras--generate-next-heading "")
                 " 2")))

;;;; Markdown repository metadata

(ert-deftest gptel-extras-test-get-markdown-repo ()
  "Read GPTEL_REPO from Markdown YAML front matter."
  (with-temp-buffer
    (insert "---\nGPTEL_REPO: /tmp/repo\n---\n\n# Chat\n")
    (should (equal (gptel-extras-get-markdown-repo) "/tmp/repo"))))

(ert-deftest gptel-extras-test-set-markdown-repo-adds-front-matter ()
  "Add Markdown YAML front matter when absent."
  (with-temp-buffer
    (insert "# Chat\n")
    (gptel-extras-set-markdown-repo "/tmp/repo")
    (should (string-prefix-p "---\nGPTEL_REPO: /tmp/repo\n---\n\n# Chat"
			     (buffer-string)))))

(ert-deftest gptel-extras-test-set-markdown-repo-updates-existing-key ()
  "Update an existing Markdown GPTEL_REPO key."
  (with-temp-buffer
    (insert "---\nGPTEL_REPO: /old\n---\n")
    (gptel-extras-set-markdown-repo "/new")
    (should (string-match-p "^GPTEL_REPO: /new$" (buffer-string)))))

;;;; Normalize whitespace

(ert-deftest gptel-extras-test-normalize-whitespace-basic ()
  "Collapse multiple spaces to a single space."
  (should (equal (gptel-extras-normalize-whitespace "hello   world")
                 "hello world")))

(ert-deftest gptel-extras-test-normalize-whitespace-tabs-newlines ()
  "Collapse tabs and newlines into a single space."
  (should (equal (gptel-extras-normalize-whitespace "hello\t\n\rworld")
                 "hello world")))

(ert-deftest gptel-extras-test-normalize-whitespace-trims ()
  "Trim leading and trailing whitespace."
  (should (equal (gptel-extras-normalize-whitespace "  hello world  ")
                 "hello world")))

(ert-deftest gptel-extras-test-normalize-whitespace-already-normal ()
  "Return string unchanged when already normalized."
  (should (equal (gptel-extras-normalize-whitespace "already normal")
                 "already normal")))

(ert-deftest gptel-extras-test-normalize-whitespace-empty ()
  "Return empty string for whitespace-only input."
  (should (equal (gptel-extras-normalize-whitespace "   ") "")))

(ert-deftest gptel-extras-test-normalize-whitespace-single-word ()
  "Return single word unchanged."
  (should (equal (gptel-extras-normalize-whitespace "word") "word")))

;;;; Create fuzzy regex

(ert-deftest gptel-extras-test-create-fuzzy-regex-simple ()
  "Fuzzy regex matches the original string literally."
  (let ((regex (gptel-extras-create-fuzzy-regex "hello world")))
    (should (string-match-p regex "hello world"))))

(ert-deftest gptel-extras-test-create-fuzzy-regex-exact-whitespace ()
  "Fuzzy regex matches the exact whitespace in the original string.
The fuzzy regex only flexes escaped whitespace from `regexp-quote',
so a plain space matches exactly one space."
  (let ((regex (gptel-extras-create-fuzzy-regex "hello world")))
    (should (string-match-p regex "hello world"))
    ;; Two spaces do not match because the regex has a single literal space.
    (should-not (string-match-p regex "hello  world"))))

(ert-deftest gptel-extras-test-create-fuzzy-regex-no-false-match ()
  "Fuzzy regex does not match unrelated strings."
  (let ((regex (gptel-extras-create-fuzzy-regex "hello world")))
    (should-not (string-match-p regex "goodbye world"))))

(ert-deftest gptel-extras-test-create-fuzzy-regex-special-chars ()
  "Fuzzy regex properly escapes regex special characters."
  (let ((regex (gptel-extras-create-fuzzy-regex "foo.bar")))
    (should (string-match-p regex "foo.bar"))
    (should-not (string-match-p regex "fooXbar"))))

(ert-deftest gptel-extras-test-create-fuzzy-regex-multiple-spaces ()
  "Fuzzy regex preserves multiple spaces literally.
`regexp-quote' does not escape spaces, so multiple spaces remain
literal in the pattern."
  (let ((regex (gptel-extras-create-fuzzy-regex "a  b")))
    (should (string-match-p regex "a  b"))
    (should-not (string-match-p regex "a b"))))

;;;; Create flexible regex

(ert-deftest gptel-extras-test-create-flexible-regex-matches-exact ()
  "Flexible regex matches the original string."
  (let ((regex (gptel-extras-create-flexible-regex "abc")))
    (should (string-match-p regex "abc"))))

(ert-deftest gptel-extras-test-create-flexible-regex-matches-with-spaces ()
  "Flexible regex matches with whitespace between characters."
  (let ((regex (gptel-extras-create-flexible-regex "abc")))
    (should (string-match-p regex "a b c"))
    (should (string-match-p regex "a  b  c"))))

(ert-deftest gptel-extras-test-create-flexible-regex-no-false-match ()
  "Flexible regex does not match when characters differ."
  (let ((regex (gptel-extras-create-flexible-regex "abc")))
    (should-not (string-match-p regex "xyz"))))

(ert-deftest gptel-extras-test-create-flexible-regex-special-chars ()
  "Flexible regex escapes regex metacharacters."
  (let ((regex (gptel-extras-create-flexible-regex "a.b")))
    (should (string-match-p regex "a.b"))
    ;; The dot is escaped, so it should not match arbitrary chars,
    ;; but flexible regex inserts optional whitespace between chars
    ;; so "aXb" should still not match since X is not whitespace or dot.
    (should-not (string-match-p regex "aXb"))))

;;;; Extract parts text

(ert-deftest gptel-extras-test-extract-parts-text-strings ()
  "Return strings from a list of plain string parts."
  (should (equal (gptel-extras--extract-parts-text '("hello" "world"))
                 '("hello" "world"))))

(ert-deftest gptel-extras-test-extract-parts-text-hash-tables ()
  "Extract text from hash-table parts."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "text" "from hash" ht)
    (should (equal (gptel-extras--extract-parts-text (list ht))
                   '("from hash")))))

(ert-deftest gptel-extras-test-extract-parts-text-mixed ()
  "Handle a mix of strings, hash-tables, and non-text items."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "text" "hash-text" ht)
    (let ((ht2 (make-hash-table :test 'equal)))
      (puthash "image" "data" ht2)
      (should (equal (gptel-extras--extract-parts-text
                      (list "plain" ht ht2 42))
                     '("plain" "hash-text" nil nil))))))

(ert-deftest gptel-extras-test-extract-parts-text-empty ()
  "Return nil for an empty parts list."
  (should (null (gptel-extras--extract-parts-text '()))))

;;;; Collect message nodes

(ert-deftest gptel-extras-test-collect-message-nodes-basic ()
  "Collect user and assistant messages from a mapping hash table."
  (let ((mapping (make-hash-table :test 'equal))
        (node1 (make-hash-table :test 'equal))
        (msg1 (make-hash-table :test 'equal))
        (author1 (make-hash-table :test 'equal))
        (content1 (make-hash-table :test 'equal))
        (node2 (make-hash-table :test 'equal))
        (msg2 (make-hash-table :test 'equal))
        (author2 (make-hash-table :test 'equal))
        (content2 (make-hash-table :test 'equal)))
    (puthash "role" "user" author1)
    (puthash "parts" '("Hello") content1)
    (puthash "author" author1 msg1)
    (puthash "content" content1 msg1)
    (puthash "message" msg1 node1)
    (puthash "role" "assistant" author2)
    (puthash "parts" '("Hi there") content2)
    (puthash "author" author2 msg2)
    (puthash "content" content2 msg2)
    (puthash "message" msg2 node2)
    (puthash "node-1" node1 mapping)
    (puthash "node-2" node2 mapping)
    (let ((result (gptel-extras--collect-message-nodes mapping)))
      (should (= 2 (length result)))
      (should (cl-every #'hash-table-p result)))))

(ert-deftest gptel-extras-test-collect-message-nodes-skips-system ()
  "Skip messages with roles other than user and assistant."
  (let ((mapping (make-hash-table :test 'equal))
        (node (make-hash-table :test 'equal))
        (msg (make-hash-table :test 'equal))
        (author (make-hash-table :test 'equal))
        (content (make-hash-table :test 'equal)))
    (puthash "role" "system" author)
    (puthash "parts" '("System prompt") content)
    (puthash "author" author msg)
    (puthash "content" content msg)
    (puthash "message" msg node)
    (puthash "sys-node" node mapping)
    (should (null (gptel-extras--collect-message-nodes mapping)))))

(ert-deftest gptel-extras-test-collect-message-nodes-skips-no-content ()
  "Skip nodes where message has no content."
  (let ((mapping (make-hash-table :test 'equal))
        (node (make-hash-table :test 'equal))
        (msg (make-hash-table :test 'equal))
        (author (make-hash-table :test 'equal)))
    (puthash "role" "user" author)
    (puthash "author" author msg)
    ;; No "content" key
    (puthash "message" msg node)
    (puthash "node-1" node mapping)
    (should (null (gptel-extras--collect-message-nodes mapping)))))

(ert-deftest gptel-extras-test-collect-message-nodes-skips-no-message ()
  "Skip nodes that have no message key at all."
  (let ((mapping (make-hash-table :test 'equal))
        (node (make-hash-table :test 'equal)))
    ;; Node with no "message" key
    (puthash "some-key" "some-value" node)
    (puthash "node-1" node mapping)
    (should (null (gptel-extras--collect-message-nodes mapping)))))

(ert-deftest gptel-extras-test-collect-message-nodes-empty-mapping ()
  "Return nil for an empty mapping hash table."
  (let ((mapping (make-hash-table :test 'equal)))
    (should (null (gptel-extras--collect-message-nodes mapping)))))

;;;; Sort messages by timestamp

(ert-deftest gptel-extras-test-sort-messages-by-timestamp-ascending ()
  "Sort messages in ascending order by create_time."
  (let ((m1 (make-hash-table :test 'equal))
        (m2 (make-hash-table :test 'equal))
        (m3 (make-hash-table :test 'equal)))
    (puthash "create_time" 300 m1)
    (puthash "create_time" 100 m2)
    (puthash "create_time" 200 m3)
    (let ((sorted (gptel-extras--sort-messages-by-timestamp (list m1 m2 m3))))
      (should (= 100 (gethash "create_time" (nth 0 sorted))))
      (should (= 200 (gethash "create_time" (nth 1 sorted))))
      (should (= 300 (gethash "create_time" (nth 2 sorted)))))))

(ert-deftest gptel-extras-test-sort-messages-by-timestamp-nil-last ()
  "Messages with nil create_time sort to the end."
  (let ((m1 (make-hash-table :test 'equal))
        (m2 (make-hash-table :test 'equal)))
    (puthash "create_time" 100 m1)
    ;; m2 has no create_time, so gethash returns nil
    (let ((sorted (gptel-extras--sort-messages-by-timestamp (list m2 m1))))
      (should (= 100 (gethash "create_time" (nth 0 sorted))))
      (should (null (gethash "create_time" (nth 1 sorted)))))))

(ert-deftest gptel-extras-test-sort-messages-by-timestamp-single ()
  "A single-element list is returned unchanged."
  (let ((m1 (make-hash-table :test 'equal)))
    (puthash "create_time" 42 m1)
    (let ((sorted (gptel-extras--sort-messages-by-timestamp (list m1))))
      (should (= 1 (length sorted)))
      (should (= 42 (gethash "create_time" (car sorted)))))))

(ert-deftest gptel-extras-test-sort-messages-by-timestamp-empty ()
  "An empty list returns nil."
  (should (null (gptel-extras--sort-messages-by-timestamp nil))))

(ert-deftest gptel-extras-test-sort-messages-by-timestamp-equal-times ()
  "Messages with equal timestamps preserve count."
  (let ((m1 (make-hash-table :test 'equal))
        (m2 (make-hash-table :test 'equal)))
    (puthash "create_time" 100 m1)
    (puthash "create_time" 100 m2)
    (let ((sorted (gptel-extras--sort-messages-by-timestamp (list m1 m2))))
      (should (= 2 (length sorted)))
      (should (= 100 (gethash "create_time" (nth 0 sorted))))
      (should (= 100 (gethash "create_time" (nth 1 sorted)))))))

;;;; Filter conversations

(ert-deftest gptel-extras-test-filter-conversations-exclude-archived ()
  "Exclude archived conversations when include-archived is nil."
  (let* ((conv1 (make-hash-table :test 'equal))
         (conv2 (make-hash-table :test 'equal))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (tmp (make-temp-file "gptel-test-" nil ".json")))
    (puthash "title" "Active" conv1)
    (puthash "title" "Archived" conv2)
    (puthash "is_archived" t conv2)
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (json-encode (list conv1 conv2))))
          (let ((result (gptel-extras--filter-conversations tmp nil)))
            (should (= 1 (length result)))))
      (delete-file tmp))))

(ert-deftest gptel-extras-test-filter-conversations-include-archived ()
  "Include all conversations when include-archived is non-nil."
  (let* ((conv1 (make-hash-table :test 'equal))
         (conv2 (make-hash-table :test 'equal))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (tmp (make-temp-file "gptel-test-" nil ".json")))
    (puthash "title" "Active" conv1)
    (puthash "title" "Archived" conv2)
    (puthash "is_archived" t conv2)
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (json-encode (list conv1 conv2))))
          (let ((result (gptel-extras--filter-conversations tmp t)))
            (should (= 2 (length result)))))
      (delete-file tmp))))

(ert-deftest gptel-extras-test-filter-conversations-no-archived-flag ()
  "Keep conversations that have no is_archived key."
  (let* ((conv (make-hash-table :test 'equal))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (tmp (make-temp-file "gptel-test-" nil ".json")))
    (puthash "title" "No flag" conv)
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (json-encode (list conv))))
          (let ((result (gptel-extras--filter-conversations tmp nil)))
            (should (= 1 (length result)))))
      (delete-file tmp))))

(ert-deftest gptel-extras-test-filter-conversations-json-false ()
  "Keep conversations where is_archived is json-false."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-false :json-false)
         (tmp (make-temp-file "gptel-test-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            ;; JSON false maps to json-false in Emacs
            (insert "[{\"title\": \"Not archived\", \"is_archived\": false}]"))
          (let ((result (gptel-extras--filter-conversations tmp nil)))
            (should (= 1 (length result)))))
      (delete-file tmp))))

(ert-deftest gptel-extras-test-filter-conversations-empty ()
  "Return nil for an empty JSON array."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (tmp (make-temp-file "gptel-test-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "[]"))
          (let ((result (gptel-extras--filter-conversations tmp nil)))
            (should (null result))))
      (delete-file tmp))))

;;;; Fuzzy search

(ert-deftest gptel-extras-test-fuzzy-search-exact-match ()
  "Find an exact match in a buffer."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird line\n")
    (should (gptel-extras-fuzzy-search "second line"))))

(ert-deftest gptel-extras-test-fuzzy-search-not-found ()
  "Return nil when target string is not in buffer."
  (with-temp-buffer
    (insert "first line\nsecond line\n")
    (should-not (gptel-extras-fuzzy-search "nonexistent"))))

(ert-deftest gptel-extras-test-fuzzy-search-whitespace-normalization ()
  "Match despite extra whitespace in the buffer."
  (with-temp-buffer
    (insert "hello   world\n")
    (should (gptel-extras-fuzzy-search "hello world"))))

(ert-deftest gptel-extras-test-fuzzy-search-case-insensitive-fallback ()
  "Fall back to case-insensitive match when exact match fails."
  (with-temp-buffer
    (insert "Hello World\n")
    (should (gptel-extras-fuzzy-search "hello world"))))

(ert-deftest gptel-extras-test-fuzzy-search-flexible-fallback ()
  "Fall back to flexible regex when other strategies fail."
  (with-temp-buffer
    ;; Insert text with whitespace between characters that the flexible
    ;; regex can match
    (insert "a b c\n")
    (should (gptel-extras-fuzzy-search "abc"))))

(ert-deftest gptel-extras-test-fuzzy-search-positions-point ()
  "Point is positioned after the match on success."
  (with-temp-buffer
    (insert "aaa target bbb\n")
    (gptel-extras-fuzzy-search "target")
    (should (> (point) (point-min)))))

(ert-deftest gptel-extras-test-fuzzy-search-empty-buffer ()
  "Return nil when searching in an empty buffer."
  (with-temp-buffer
    (should-not (gptel-extras-fuzzy-search "anything"))))

;;;; Write conversation to file

(ert-deftest gptel-extras-test-write-conversation-basic ()
  "Write a conversation with title and messages to a file."
  (let* ((msg (make-hash-table :test 'equal))
         (author (make-hash-table :test 'equal))
         (content (make-hash-table :test 'equal))
         (tmp (make-temp-file "gptel-conv-" nil ".org")))
    (puthash "role" "user" author)
    (puthash "parts" '("Hello GPT") content)
    (puthash "author" author msg)
    (puthash "content" content msg)
    (unwind-protect
        (progn
          ;; Mock org-extras-convert-markdown-to-org to identity
          (cl-letf (((symbol-function 'org-extras-convert-markdown-to-org)
                     #'identity))
            (gptel-extras--write-conversation-to-file
             "Test Chat" "conv-123" (list msg) tmp))
          (with-temp-buffer
            (insert-file-contents tmp)
            (let ((text (buffer-string)))
              (should (string-search "#+title: Test Chat" text))
              (should (string-search "* Test Chat" text))
              (should (string-search "chatgpt.com/c/conv-123" text))
              (should (string-search "** User" text))
              (should (string-search "Hello GPT" text)))))
      (delete-file tmp))))

(ert-deftest gptel-extras-test-write-conversation-no-conv-id ()
  "Omit the ChatGPT link when conv-id is nil."
  (let* ((msg (make-hash-table :test 'equal))
         (author (make-hash-table :test 'equal))
         (content (make-hash-table :test 'equal))
         (tmp (make-temp-file "gptel-conv-" nil ".org")))
    (puthash "role" "assistant" author)
    (puthash "parts" '("Hi!") content)
    (puthash "author" author msg)
    (puthash "content" content msg)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'org-extras-convert-markdown-to-org)
                     #'identity))
            (gptel-extras--write-conversation-to-file
             "No ID" nil (list msg) tmp))
          (with-temp-buffer
            (insert-file-contents tmp)
            (let ((text (buffer-string)))
              (should (string-search "#+title: No ID" text))
              (should-not (string-search "chatgpt.com" text))
              (should (string-search "** Assistant" text)))))
      (delete-file tmp))))

(ert-deftest gptel-extras-test-write-conversation-multiple-messages ()
  "Write multiple messages in order."
  (let* ((msg1 (make-hash-table :test 'equal))
         (author1 (make-hash-table :test 'equal))
         (content1 (make-hash-table :test 'equal))
         (msg2 (make-hash-table :test 'equal))
         (author2 (make-hash-table :test 'equal))
         (content2 (make-hash-table :test 'equal))
         (tmp (make-temp-file "gptel-conv-" nil ".org")))
    (puthash "role" "user" author1)
    (puthash "parts" '("Question") content1)
    (puthash "author" author1 msg1)
    (puthash "content" content1 msg1)
    (puthash "role" "assistant" author2)
    (puthash "parts" '("Answer") content2)
    (puthash "author" author2 msg2)
    (puthash "content" content2 msg2)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'org-extras-convert-markdown-to-org)
                     #'identity))
            (gptel-extras--write-conversation-to-file
             "Multi" "id-1" (list msg1 msg2) tmp))
          (with-temp-buffer
            (insert-file-contents tmp)
            (let ((text (buffer-string)))
              (should (string-search "** User" text))
              (should (string-search "** Assistant" text))
              (should (string-search "Question" text))
              (should (string-search "Answer" text))
              ;; User should appear before Assistant
              (should (< (string-search "User" text)
                         (string-search "Assistant" text))))))
      (delete-file tmp))))

(ert-deftest gptel-extras-test-write-conversation-empty-messages ()
  "Write file with title but no message bodies when messages list is empty."
  (let ((tmp (make-temp-file "gptel-conv-" nil ".org")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'org-extras-convert-markdown-to-org)
                     #'identity))
            (gptel-extras--write-conversation-to-file
             "Empty" "id-0" nil tmp))
          (with-temp-buffer
            (insert-file-contents tmp)
            (let ((text (buffer-string)))
              (should (string-search "#+title: Empty" text))
              (should (string-search "* Empty" text))
              ;; No role headings
              (should-not (string-search "** User" text))
              (should-not (string-search "** Assistant" text)))))
      (delete-file tmp))))

(ert-deftest gptel-extras-test-write-conversation-skips-nil-parts ()
  "Skip messages whose parts filter to all nil."
  (let* ((msg (make-hash-table :test 'equal))
         (author (make-hash-table :test 'equal))
         (content (make-hash-table :test 'equal))
         (tmp (make-temp-file "gptel-conv-" nil ".org")))
    (puthash "role" "user" author)
    ;; Parts that are all non-text (e.g., numbers)
    (puthash "parts" '(42) content)
    (puthash "author" author msg)
    (puthash "content" content msg)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'org-extras-convert-markdown-to-org)
                     #'identity))
            (gptel-extras--write-conversation-to-file
             "Nil Parts" nil (list msg) tmp))
          (with-temp-buffer
            (insert-file-contents tmp)
            (let ((text (buffer-string)))
              ;; Should have title but no User heading since parts are nil
              (should (string-search "#+title: Nil Parts" text))
              (should-not (string-search "** User" text)))))
      (delete-file tmp))))

;;;; Bibliography entry processing

(ert-deftest gptel-extras-test-bib-prompts-require-known-decisions ()
  "Do not approve unknown permissions or select arbitrary editions."
  (should-not (gptel-extras--bib-confirm "Regenerate key? "))
  (should-error (gptel-extras--bib-confirm "Overwrite existing file? ")
                :type 'user-error)
  (should-error (gptel-extras--bib-confirm "Always allow access? ")
                :type 'user-error)
  (should (equal (gptel-extras--bib-read-string "Search string: " "10.1/example")
                 "10.1/example"))
  (should-error (gptel-extras--bib-read-string "Other input: " "yes")
                :type 'user-error)
  (should (equal (gptel-extras--bib-completing-read
                  "Select a link: " '("Only edition") nil t)
                 "Only edition"))
  (should-error (gptel-extras--bib-completing-read
                 "Select a link: " '("First edition" "Second edition") nil t)
                :type 'user-error)
  (should (equal (gptel-extras--bib-completing-read
                  "Select language: " '("english" "spanish") nil t "english")
                 "english"))
  (should (equal (gptel-extras--bib-completing-read
                  "Select language: " '("english" "spanish") nil t nil nil "spanish")
                 "spanish")))

(ert-deftest gptel-extras-test-bib-import-preflight-protects-state ()
  "Reject dirty visiting buffers, dirty Ebib databases, and pending downloads."
  (require 'ebib)
  (require 'ebib-db)
  (let* ((file (make-temp-file "bib-preflight-" nil ".bib"))
         (db (ebib-db-new-database))
         (ebib--databases (list db))
         (ebib-extras--annas-archive-pending-key nil))
    (unwind-protect
        (progn
          (ebib-db-set-filename file db)
          (gptel-extras--bib-import-preflight file)
          (with-temp-buffer
            (setq buffer-file-name file)
            (insert "unsaved")
            (should-error (gptel-extras--bib-import-preflight file)
                          :type 'user-error))
          (ebib-db-set-modified t db)
          (should-error (gptel-extras--bib-import-preflight file)
                        :type 'user-error)
          (ebib-db-set-modified nil db)
          (let ((ebib-extras--annas-archive-pending-key "Prior2020Paper"))
            (should-error (gptel-extras--bib-import-preflight file)
                          :type 'user-error)))
      (delete-file file))))

(ert-deftest gptel-extras-test-bib-processing-uses-manual-key-validation ()
  "Regenerate keys rejected by Ebib and persist even without an attachment."
  (require 'ebib-extras)
  (let* ((db (ebib-db-new-database))
         (ebib--cur-db db)
         (ebib-extras--annas-archive-pending-key nil)
         (key "a1b")
         saved attached)
    (ebib-db-set-filename "/tmp/processing-test.bib" db)
    (cl-letf (((symbol-function 'ebib--get-key-at-point) (lambda () key))
              ((symbol-function 'ebib-generate-autokey)
               (lambda ()
                 (setq key "Author2020Paper")
                 (ebib-db-set-entry key '(("title" . "Paper")) db)
                 (ebib-db-set-current-entry-key key db)))
              ((symbol-function 'ebib-extras-get-or-set-language) #'ignore)
              ((symbol-function 'ebib-extras-attach-files)
               (lambda (entry-key) (setq attached entry-key)))
              ((symbol-function 'ebib-extras-check-crossref) #'ignore)
              ((symbol-function 'ebib-save-current-database)
               (lambda (&rest _) (push key saved)))
              ((symbol-function 'gptel-extras--wait-for-bib-attachments)
               (lambda (&rest _) nil))
              ((symbol-function 'ebib-extras-get-field) (lambda (&rest _) nil)))
      (let ((result (gptel-extras--process-bib-entry-headless 0)))
        (should (equal (plist-get result :key) "Author2020Paper"))
        (should (equal attached "Author2020Paper"))
        (should (member "Author2020Paper" saved))
        (should-not (plist-get result :files))))))

(ert-deftest gptel-extras-test-add-bib-entry-and-process-orchestrates ()
  "Add-and-process imports metadata, opens the key, then processes attachments."
  (let (calls
        (old-key-bound (boundp 'zotra-extras-most-recent-bibkey))
        (old-key (and (boundp 'zotra-extras-most-recent-bibkey)
                      zotra-extras-most-recent-bibkey))
        (old-require (symbol-function 'require)))
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (if (memq feature '(zotra-extras ebib-extras))
                         t
                       (funcall old-require feature filename noerror))))
                  ((symbol-function 'zotra-extras-add-entry)
                   (lambda (identifier entry-format bibfile do-not-open)
                     (should (equal zotra-extras-most-recent-bibfile bibfile))
                     (push (list 'add identifier entry-format bibfile do-not-open) calls)
                     (setq zotra-extras-most-recent-bibkey "smith2024test")))
                  ((symbol-function 'gptel-extras--bib-import-preflight) #'ignore)
                  ((symbol-function 'gptel-extras--open-bib-entry-for-processing)
                   (lambda (bibfile key)
                     (push (list 'open bibfile key) calls)))
                  ((symbol-function 'gptel-extras--process-bib-entry-headless)
                   (lambda (timeout)
                     (push (list 'process timeout) calls)
                     '(:key "smith2024test" :files ("/tmp/smith2024test.pdf")))))
          (should
           (equal (gptel-extras-add-bib-entry-and-process "10.123/example" "new.bib" 7)
                  '(:key "smith2024test" :files ("/tmp/smith2024test.pdf"))))
          (should
           (equal (nreverse calls)
                  '((add "10.123/example" nil "new.bib" t)
                    (open "new.bib" "smith2024test")
                    (process 7)))))
      (if old-key-bound
          (setq zotra-extras-most-recent-bibkey old-key)
        (makunbound 'zotra-extras-most-recent-bibkey)))))

;;;; Redact credentials

(defun gptel-extras-test--fake-key (prefix body)
  "Build a credential-shaped string from PREFIX and BODY.
Assembled at runtime so no literal credential pattern appears in the source,
where secret scanners would flag it."
  (concat prefix body))

(ert-deftest gptel-extras-test-redacts-credential-from-query ()
  "A credential in the outgoing query is replaced with a placeholder."
  (with-temp-buffer
    (insert "please review this: "
            (gptel-extras-test--fake-key
             "sk-ant-" "api03-AAAAAAAAAAAAAAAAAAAAAAAAAA")
            " thanks")
    (gptel-extras-redact-secrets-in-query)
    (should (string-match-p "\\[REDACTED-CREDENTIAL\\]" (buffer-string)))
    (should-not (string-match-p "sk-ant-api03" (buffer-string)))
    (should (string-match-p "please review this" (buffer-string)))))

(ert-deftest gptel-extras-test-redacts-every-credential ()
  "Every credential in the query is replaced, not only the first."
  (with-temp-buffer
    (insert (gptel-extras-test--fake-key "sk-ant-" "api03-AAAAAAAAAAAAAAAAAAAAAAAAAA")
            "\n"
            (gptel-extras-test--fake-key "ghp" "_012345678901234567890123456789012345")
            "\n"
            (gptel-extras-test--fake-key "xox" "b-33980812345-abcdefghij"))
    (gptel-extras-redact-secrets-in-query)
    (should (= 3 (cl-count "[REDACTED-CREDENTIAL]" (split-string (buffer-string) "\n")
                           :test #'string=)))))

(ert-deftest gptel-extras-test-leaves-ordinary-text-alone ()
  "A query with no credential is passed through unchanged."
  (with-temp-buffer
    (insert "summarise risk-account-on-april-14 and the other tasks")
    (let ((before (buffer-string)))
      (gptel-extras-redact-secrets-in-query)
      (should (equal before (buffer-string))))))

(ert-deftest gptel-extras-test-redaction-can-be-disabled ()
  "Setting `gptel-extras-redact-secrets' to nil disables redaction."
  (with-temp-buffer
    (insert (gptel-extras-test--fake-key "sk-ant-" "api03-AAAAAAAAAAAAAAAAAAAAAAAAAA"))
    (let ((gptel-extras-redact-secrets nil)
          (before (buffer-string)))
      (gptel-extras-redact-secrets-in-query)
      (should (equal before (buffer-string))))))

(ert-deftest gptel-extras-test-redaction-runs-after-context ()
  "Redaction is ordered after gptel's own context insertion.
Context pulled from other buffers and files must be redacted too, so this
function has to run once that text is already in the query buffer."
  (let ((hook gptel-prompt-transform-functions))
    (should (memq 'gptel-extras-redact-secrets-in-query hook))
    (when (memq 'gptel--transform-add-context hook)
      (should (> (cl-position 'gptel-extras-redact-secrets-in-query hook)
                 (cl-position 'gptel--transform-add-context hook))))))

(provide 'gptel-extras-test)
;;; gptel-extras-test.el ends here
