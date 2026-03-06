;;; orderless-extras-test.el --- Tests for orderless-extras -*- lexical-binding: t -*-

;; Tests for dispatcher functions in orderless-extras.el.

;;; Code:

(require 'ert)
(require 'orderless-extras)

;;;; Flex dispatcher (~)

(ert-deftest orderless-extras-test-flex-dispatcher-matches-suffix ()
  "Flex dispatcher returns (orderless-flex . pattern) when ~ is the suffix."
  (should (equal (orderless-extras-flex-dispatcher "foo~" 0 1)
                 '(orderless-flex . "foo"))))

(ert-deftest orderless-extras-test-flex-dispatcher-strips-suffix ()
  "Flex dispatcher strips the trailing ~ from the pattern."
  (let ((result (orderless-extras-flex-dispatcher "hello world~" 0 1)))
    (should (equal (cdr result) "hello world"))))

(ert-deftest orderless-extras-test-flex-dispatcher-no-suffix ()
  "Flex dispatcher returns nil when pattern does not end with ~."
  (should (null (orderless-extras-flex-dispatcher "foo" 0 1))))

(ert-deftest orderless-extras-test-flex-dispatcher-only-suffix ()
  "Flex dispatcher returns empty stripped pattern for lone ~."
  (should (equal (orderless-extras-flex-dispatcher "~" 0 1)
                 '(orderless-flex . ""))))

(ert-deftest orderless-extras-test-flex-dispatcher-tilde-not-at-end ()
  "Flex dispatcher returns nil when ~ is not at the end."
  (should (null (orderless-extras-flex-dispatcher "~foo" 0 1)))
  (should (null (orderless-extras-flex-dispatcher "fo~o" 0 1))))

;;;; Initialism dispatcher (,)

(ert-deftest orderless-extras-test-initialism-dispatcher-matches-suffix ()
  "Initialism dispatcher returns (orderless-initialism . pattern) when , is the suffix."
  (should (equal (orderless-extras-initialism-dispatcher "abc," 0 1)
                 '(orderless-initialism . "abc"))))

(ert-deftest orderless-extras-test-initialism-dispatcher-strips-suffix ()
  "Initialism dispatcher strips the trailing comma from the pattern."
  (let ((result (orderless-extras-initialism-dispatcher "xyz," 0 1)))
    (should (equal (cdr result) "xyz"))))

(ert-deftest orderless-extras-test-initialism-dispatcher-no-suffix ()
  "Initialism dispatcher returns nil when pattern does not end with comma."
  (should (null (orderless-extras-initialism-dispatcher "foo" 0 1))))

(ert-deftest orderless-extras-test-initialism-dispatcher-only-suffix ()
  "Initialism dispatcher returns empty stripped pattern for lone comma."
  (should (equal (orderless-extras-initialism-dispatcher "," 0 1)
                 '(orderless-initialism . ""))))

(ert-deftest orderless-extras-test-initialism-dispatcher-comma-not-at-end ()
  "Initialism dispatcher returns nil when comma is not at the end."
  (should (null (orderless-extras-initialism-dispatcher ",foo" 0 1)))
  (should (null (orderless-extras-initialism-dispatcher "fo,o" 0 1))))

;;;; Prefixes dispatcher (;)

(ert-deftest orderless-extras-test-prefixes-dispatcher-matches-suffix ()
  "Prefixes dispatcher returns (orderless-prefixes . pattern) when ; is the suffix."
  (should (equal (orderless-extras-prefixes-dispatcher "abc;" 0 1)
                 '(orderless-prefixes . "abc"))))

(ert-deftest orderless-extras-test-prefixes-dispatcher-strips-suffix ()
  "Prefixes dispatcher strips the trailing semicolon from the pattern."
  (let ((result (orderless-extras-prefixes-dispatcher "hello;" 0 1)))
    (should (equal (cdr result) "hello"))))

(ert-deftest orderless-extras-test-prefixes-dispatcher-no-suffix ()
  "Prefixes dispatcher returns nil when pattern does not end with semicolon."
  (should (null (orderless-extras-prefixes-dispatcher "foo" 0 1))))

(ert-deftest orderless-extras-test-prefixes-dispatcher-only-suffix ()
  "Prefixes dispatcher returns empty stripped pattern for lone semicolon."
  (should (equal (orderless-extras-prefixes-dispatcher ";" 0 1)
                 '(orderless-prefixes . ""))))

(ert-deftest orderless-extras-test-prefixes-dispatcher-semicolon-not-at-end ()
  "Prefixes dispatcher returns nil when semicolon is not at the end."
  (should (null (orderless-extras-prefixes-dispatcher ";foo" 0 1)))
  (should (null (orderless-extras-prefixes-dispatcher "fo;o" 0 1))))

;;;; Exclusion dispatcher (!)

(ert-deftest orderless-extras-test-exclusion-dispatcher-matches-suffix ()
  "Exclusion dispatcher returns (orderless-without-literal . pattern) when ! is the suffix."
  (should (equal (orderless-extras-exclusion-dispatcher "abc!" 0 1)
                 '(orderless-without-literal . "abc"))))

(ert-deftest orderless-extras-test-exclusion-dispatcher-strips-suffix ()
  "Exclusion dispatcher strips the trailing ! from the pattern."
  (let ((result (orderless-extras-exclusion-dispatcher "hello!" 0 1)))
    (should (equal (cdr result) "hello"))))

(ert-deftest orderless-extras-test-exclusion-dispatcher-no-suffix ()
  "Exclusion dispatcher returns nil when pattern does not end with !."
  (should (null (orderless-extras-exclusion-dispatcher "foo" 0 1))))

(ert-deftest orderless-extras-test-exclusion-dispatcher-only-suffix ()
  "Exclusion dispatcher returns empty stripped pattern for lone !."
  (should (equal (orderless-extras-exclusion-dispatcher "!" 0 1)
                 '(orderless-without-literal . ""))))

(ert-deftest orderless-extras-test-exclusion-dispatcher-bang-not-at-end ()
  "Exclusion dispatcher returns nil when ! is not at the end."
  (should (null (orderless-extras-exclusion-dispatcher "!foo" 0 1)))
  (should (null (orderless-extras-exclusion-dispatcher "fo!o" 0 1))))

;;;; Cross-dispatcher: no false positives between dispatchers

(ert-deftest orderless-extras-test-dispatchers-do-not-match-other-suffixes ()
  "Each dispatcher only responds to its own suffix character."
  (should (null (orderless-extras-flex-dispatcher "foo," 0 1)))
  (should (null (orderless-extras-flex-dispatcher "foo;" 0 1)))
  (should (null (orderless-extras-flex-dispatcher "foo!" 0 1)))
  (should (null (orderless-extras-initialism-dispatcher "foo~" 0 1)))
  (should (null (orderless-extras-initialism-dispatcher "foo;" 0 1)))
  (should (null (orderless-extras-initialism-dispatcher "foo!" 0 1)))
  (should (null (orderless-extras-prefixes-dispatcher "foo~" 0 1)))
  (should (null (orderless-extras-prefixes-dispatcher "foo," 0 1)))
  (should (null (orderless-extras-prefixes-dispatcher "foo!" 0 1)))
  (should (null (orderless-extras-exclusion-dispatcher "foo~" 0 1)))
  (should (null (orderless-extras-exclusion-dispatcher "foo," 0 1)))
  (should (null (orderless-extras-exclusion-dispatcher "foo;" 0 1))))

(provide 'orderless-extras-test)
;;; orderless-extras-test.el ends here
