;;; simple-extras-test.el --- Tests for simple-extras -*- lexical-binding: t -*-

;; Tests for string manipulation, text editing operations, and utility
;; functions in simple-extras.el.

;;; Code:

(require 'ert)
(require 'simple-extras)

;;;; Slugify

(ert-deftest simple-extras-test-slugify-basic-ascii ()
  "Slugify converts ASCII text to lowercase hyphenated form."
  (should (equal (simple-extras-slugify "Hello World") "hello-world"))
  (should (equal (simple-extras-slugify "foo bar baz") "foo-bar-baz")))

(ert-deftest simple-extras-test-slugify-removes-accents ()
  "Slugify converts accented characters to ASCII equivalents."
  (should (equal (simple-extras-slugify "Cafe") "cafe"))
  (should (equal (simple-extras-slugify "naive") "naive"))
  (should (equal (simple-extras-slugify "Zurich") "zurich")))

(ert-deftest simple-extras-test-slugify-handles-unicode-punctuation ()
  "Slugify removes Unicode punctuation and symbols."
  (should (equal (simple-extras-slugify
                  (concat "\u3010" "Test" "\u3011" "4" "\uff1a" "End"))
                 "test-4-end"))
  (should (equal (simple-extras-slugify
                  (concat "Caf" "\u00e9" " & croissants: a " "\u201c" "guide" "\u201d"))
                 "cafe-croissants-a-guide")))

(ert-deftest simple-extras-test-slugify-empty-string ()
  "Slugify handles empty string."
  (should (equal (simple-extras-slugify "") "")))

(ert-deftest simple-extras-test-slugify-already-a-slug ()
  "Slugify is idempotent on valid slugs."
  (should (equal (simple-extras-slugify "already-a-slug") "already-a-slug")))

(ert-deftest simple-extras-test-slugify-multiple-spaces ()
  "Slugify collapses multiple spaces into single hyphens."
  (should (equal (simple-extras-slugify "too   many    spaces") "too-many-spaces")))

(ert-deftest simple-extras-test-slugify-leading-trailing-punctuation ()
  "Slugify strips leading and trailing hyphens from punctuation."
  (should (equal (simple-extras-slugify "...hello...") "hello"))
  (should (equal (simple-extras-slugify "(test)") "test")))

;;;; Asciify

(ert-deftest simple-extras-test-asciify-string-accented-vowels ()
  "Asciify converts accented vowels to plain ASCII."
  (should (equal (simple-extras-asciify-string "\u00e1\u00e9\u00ed\u00f3\u00fa") "aeiou"))
  (should (equal (simple-extras-asciify-string "\u00e0\u00e8\u00ec\u00f2\u00f9") "aeiou"))
  (should (equal (simple-extras-asciify-string "\u00e2\u00ea\u00ee\u00f4\u00fb") "aeiou"))
  (should (equal (simple-extras-asciify-string "\u00e4\u00eb\u00ef\u00f6\u00fc") "aeiou")))

(ert-deftest simple-extras-test-asciify-string-special-chars ()
  "Asciify converts special European characters."
  (should (equal (simple-extras-asciify-string "\u00df") "ss"))
  (should (equal (simple-extras-asciify-string "\u00e6") "ae"))
  (should (equal (simple-extras-asciify-string "\u00f1") "n"))
  (should (equal (simple-extras-asciify-string "\u00e7") "c"))
  (should (equal (simple-extras-asciify-string "\u00fe") "th")))

(ert-deftest simple-extras-test-asciify-string-dashes ()
  "Asciify converts various dash types."
  (should (equal (simple-extras-asciify-string "\u2013") "-"))   ; en dash
  (should (equal (simple-extras-asciify-string "\u2014") "--"))) ; em dash

(ert-deftest simple-extras-test-asciify-string-plain-ascii-unchanged ()
  "Asciify leaves plain ASCII unchanged."
  (should (equal (simple-extras-asciify-string "hello world") "hello world"))
  (should (equal (simple-extras-asciify-string "123") "123")))

(ert-deftest simple-extras-test-asciify-string-mixed ()
  "Asciify handles mixed ASCII and accented text."
  (should (equal (simple-extras-asciify-string "caf\u00e9") "cafe"))
  (should (equal (simple-extras-asciify-string "na\u00efve") "naive"))
  (should (equal (simple-extras-asciify-string "Z\u00fcrich") "Zurich")))

;;;; Slug hyphenate

(ert-deftest simple-extras-test-slug-hyphenate-spaces ()
  "Slug-hyphenate converts spaces to hyphens."
  (should (equal (simple-extras-slug-hyphenate "hello world") "hello-world")))

(ert-deftest simple-extras-test-slug-hyphenate-underscores ()
  "Slug-hyphenate converts underscores to hyphens."
  (should (equal (simple-extras-slug-hyphenate "hello_world") "hello-world")))

(ert-deftest simple-extras-test-slug-hyphenate-dots ()
  "Slug-hyphenate converts dots to hyphens."
  (should (equal (simple-extras-slug-hyphenate "hello.world") "hello-world")))

(ert-deftest simple-extras-test-slug-hyphenate-multiple-hyphens ()
  "Slug-hyphenate collapses multiple hyphens into one."
  (should (equal (simple-extras-slug-hyphenate "hello---world") "hello-world")))

(ert-deftest simple-extras-test-slug-hyphenate-leading-trailing ()
  "Slug-hyphenate removes leading and trailing hyphens."
  (should (equal (simple-extras-slug-hyphenate "-hello-") "hello"))
  (should (equal (simple-extras-slug-hyphenate "--hello--") "hello")))

(ert-deftest simple-extras-test-slug-hyphenate-multiple-spaces ()
  "Slug-hyphenate collapses multiple spaces into one hyphen."
  (should (equal (simple-extras-slug-hyphenate "hello   world") "hello-world")))

;;;; Strip unicode punctuation

(ert-deftest simple-extras-test-strip-unicode-punctuation-brackets ()
  "Strip-unicode-punctuation replaces CJK brackets with spaces."
  (should (equal (simple-extras--strip-unicode-punctuation
                  (concat "\u3010" "hello" "\u3011"))
                 " hello ")))

(ert-deftest simple-extras-test-strip-unicode-punctuation-curly-quotes ()
  "Strip-unicode-punctuation replaces curly quotes with spaces."
  (should (equal (simple-extras--strip-unicode-punctuation
                  (concat "\u201c" "hello" "\u201d"))
                 " hello ")))

(ert-deftest simple-extras-test-strip-unicode-punctuation-preserves-letters ()
  "Strip-unicode-punctuation leaves letters and digits unchanged."
  (should (equal (simple-extras--strip-unicode-punctuation "hello123") "hello123"))
  (should (equal (simple-extras--strip-unicode-punctuation
                  (concat "\u30ad\u30e3\u30ea\u30a2"))
                 (concat "\u30ad\u30e3\u30ea\u30a2"))))

(ert-deftest simple-extras-test-strip-unicode-punctuation-ascii-punct ()
  "Strip-unicode-punctuation replaces ASCII punctuation."
  (should (equal (simple-extras--strip-unicode-punctuation "a,b.c!d") "a b c d"))
  (should (equal (simple-extras--strip-unicode-punctuation "a:b;c") "a b c")))

;;;; URL functions

(ert-deftest simple-extras-test-remove-trailing-slash ()
  "Remove-trailing-slash strips trailing / from strings."
  (should (equal (simple-extras-remove-trailing-slash "https://example.com/")
                 "https://example.com"))
  (should (equal (simple-extras-remove-trailing-slash "https://example.com")
                 "https://example.com"))
  (should (equal (simple-extras-remove-trailing-slash "/") ""))
  (should (equal (simple-extras-remove-trailing-slash "") "")))

(ert-deftest simple-extras-test-simplify-url ()
  "Simplify-url strips protocol and www prefix."
  (should (equal (simple-extras-simplify-url "https://www.example.com/path/")
                 "example.com/path"))
  (should (equal (simple-extras-simplify-url "http://example.com")
                 "example.com"))
  (should (equal (simple-extras-simplify-url "https://example.com/")
                 "example.com"))
  (should (equal (simple-extras-simplify-url "http://www.example.com/")
                 "example.com")))

(ert-deftest simple-extras-test-simplify-url-no-protocol ()
  "Simplify-url handles URLs without protocol."
  (should (equal (simple-extras-simplify-url "www.example.com")
                 "example.com"))
  (should (equal (simple-extras-simplify-url "example.com")
                 "example.com")))

(ert-deftest simple-extras-test-string-is-url-p ()
  "String-is-url-p detects valid URLs."
  (should (simple-extras-string-is-url-p "https://example.com"))
  (should (simple-extras-string-is-url-p "http://example.com/path"))
  (should-not (simple-extras-string-is-url-p "not a url"))
  (should-not (simple-extras-string-is-url-p "")))

;;;; Get next element

(ert-deftest simple-extras-test-get-next-element-basic ()
  "Get-next-element returns the element after the given one."
  (should (equal (simple-extras-get-next-element 'b '(a b c)) 'c))
  (should (equal (simple-extras-get-next-element 'a '(a b c)) 'b)))

(ert-deftest simple-extras-test-get-next-element-wraps ()
  "Get-next-element wraps around to the first element."
  (should (equal (simple-extras-get-next-element 'c '(a b c)) 'a)))

(ert-deftest simple-extras-test-get-next-element-strings ()
  "Get-next-element works with string lists."
  (should (equal (simple-extras-get-next-element "foo" '("foo" "bar" "baz")) "bar"))
  (should (equal (simple-extras-get-next-element "baz" '("foo" "bar" "baz")) "foo")))

(ert-deftest simple-extras-test-get-next-element-single ()
  "Get-next-element returns the single element when list has one item."
  (should (equal (simple-extras-get-next-element 'a '(a)) 'a)))

;;;; Buffer text operations

(ert-deftest simple-extras-test-delete-word-removes-text ()
  "Delete-word removes the word without adding to kill ring."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (let ((kill-ring-before (copy-sequence kill-ring)))
      (simple-extras-delete-word 1)
      (should (equal (buffer-string) " world"))
      (should (equal kill-ring kill-ring-before)))))

(ert-deftest simple-extras-test-backward-delete-word ()
  "Backward-delete-word removes the previous word without killing."
  (with-temp-buffer
    (insert "hello world")
    (let ((kill-ring-before (copy-sequence kill-ring)))
      (simple-extras-backward-delete-word 1)
      (should (equal (buffer-string) "hello "))
      (should (equal kill-ring kill-ring-before)))))

(ert-deftest simple-extras-test-copy-word-preserves-text ()
  "Copy-word copies the word to kill ring without removing text."
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-min))
    (simple-extras-copy-word 1)
    (should (equal (buffer-string) "hello world"))
    (should (equal (car kill-ring) "hello"))))

(ert-deftest simple-extras-test-delete-line ()
  "Delete-line removes the full line including newline without killing."
  (with-temp-buffer
    (insert "first line\nsecond line")
    (goto-char (point-min))
    (let ((kill-ring-before (copy-sequence kill-ring)))
      (simple-extras-delete-line 1)
      (should (equal (buffer-string) "second line"))
      (should (equal kill-ring kill-ring-before)))))

(ert-deftest simple-extras-test-copy-line ()
  "Copy-line copies the full line including newline without removing text."
  (with-temp-buffer
    (insert "first line\nsecond line")
    (goto-char (point-min))
    (simple-extras-copy-line 1)
    (should (equal (buffer-string) "first line\nsecond line"))
    (should (equal (car kill-ring) "first line\n"))))

(ert-deftest simple-extras-test-kill-whole-word ()
  "Kill-whole-word kills the entire word at point."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 3) ; inside "hello"
    (simple-extras-kill-whole-word)
    (should (equal (buffer-string) " world"))
    (should (equal (car kill-ring) "hello"))))

(ert-deftest simple-extras-test-delete-whole-line ()
  "Delete-whole-line removes the entire line."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char (point-min))
    (forward-line 1)
    (let ((kill-ring-before (copy-sequence kill-ring)))
      (simple-extras-delete-whole-line)
      (should (string-match-p "first" (buffer-string)))
      (should (string-match-p "third" (buffer-string)))
      (should (equal kill-ring kill-ring-before)))))

(ert-deftest simple-extras-test-smart-kill-region-with-region ()
  "Smart-kill-region kills region when active."
  (with-temp-buffer
    (insert "hello world")
    (set-mark 1)
    (goto-char 6)
    (activate-mark)
    (simple-extras-smart-kill-region)
    (should (equal (buffer-string) " world"))
    (should (equal (car kill-ring) "hello"))))

;;;; New buffer detection

(ert-deftest simple-extras-test-is-new-buffer-p-untitled ()
  "Is-new-buffer-p returns t for untitled buffers."
  (with-temp-buffer
    (rename-buffer "untitled<1>" t)
    (should (simple-extras-is-new-buffer-p))))

(ert-deftest simple-extras-test-is-new-buffer-p-named ()
  "Is-new-buffer-p returns nil for named buffers."
  (with-temp-buffer
    (rename-buffer "my-file.el" t)
    (should-not (simple-extras-is-new-buffer-p))))

(ert-deftest simple-extras-test-is-new-buffer-p-file-visiting ()
  "Is-new-buffer-p returns nil for file-visiting buffers."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test.txt")
    (should-not (simple-extras-is-new-buffer-p))))

;;;; Fill or unfill

(ert-deftest simple-extras-test-fill-or-unfill-paragraph ()
  "Fill-or-unfill fills a long line."
  (with-temp-buffer
    (let ((fill-column 20))
      (insert "This is a rather long sentence that should be filled.")
      (simple-extras-fill-or-unfill-paragraph)
      (should (string-match-p "\n" (buffer-string))))))

;;;; Sexp operations

(ert-deftest simple-extras-test-delete-sexp ()
  "Delete-sexp removes the sexp at point without killing."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo) (bar)")
    (goto-char (point-min))
    (let ((kill-ring-before (copy-sequence kill-ring)))
      (simple-extras-delete-sexp 1)
      (should (equal (buffer-string) " (bar)"))
      (should (equal kill-ring kill-ring-before)))))

(ert-deftest simple-extras-test-copy-sexp ()
  "Copy-sexp copies the sexp at point without removing it."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo) (bar)")
    (goto-char (point-min))
    (simple-extras-copy-sexp 1)
    (should (equal (buffer-string) "(foo) (bar)"))
    (should (equal (car kill-ring) "(foo)"))))

;;;; Zap operations

(ert-deftest simple-extras-test-zap-copy-to-char ()
  "Zap-copy-to-char copies up to and including the target char."
  (with-temp-buffer
    (insert "hello, world!")
    (goto-char (point-min))
    (simple-extras-zap-copy-to-char 1 ?,)
    (should (equal (buffer-string) "hello, world!"))
    (should (equal (car kill-ring) "hello,"))))

(ert-deftest simple-extras-test-zap-delete-to-char ()
  "Zap-delete-to-char deletes up to and including the target char."
  (with-temp-buffer
    (insert "hello, world!")
    (goto-char (point-min))
    (let ((kill-ring-before (copy-sequence kill-ring)))
      (simple-extras-zap-delete-to-char 1 ?,)
      (should (equal (buffer-string) " world!"))
      (should (equal kill-ring kill-ring-before)))))

;;;; Transpose operations

(ert-deftest simple-extras-test-transpose-words-backward ()
  "Transpose-words-backward swaps the current word with the previous one."
  (with-temp-buffer
    (insert "hello world")
    (simple-extras-transpose-words-backward)
    (should (equal (buffer-string) "world hello"))))

(ert-deftest simple-extras-test-transpose-chars-backward ()
  "Transpose-chars-backward swaps adjacent characters."
  (with-temp-buffer
    (insert "abc")
    (goto-char 3) ; between 'b' and 'c'
    (simple-extras-transpose-chars-backward)
    (should (equal (buffer-string) "bac"))))

;;;; String at point

(ert-deftest simple-extras-test-string-at-point ()
  "String-at-point extracts the string between quote delimiters."
  (with-temp-buffer
    (insert "some \"quoted text\" here")
    (goto-char 10) ; inside "quoted text"
    (should (equal (simple-extras-string-at-point) "quoted text"))))

;;;; Yank and pop

(ert-deftest simple-extras-test-yank-and-pop ()
  "Yank-and-pop inserts text and removes it from kill ring."
  (with-temp-buffer
    (kill-new "test-kill")
    (let ((ring-length (length kill-ring)))
      (simple-extras-yank-and-pop)
      (should (equal (buffer-string) "test-kill"))
      (should (= (length kill-ring) (1- ring-length))))))

(provide 'simple-extras-test)
;;; simple-extras-test.el ends here
