;;; avy-extras-test.el --- Tests for avy-extras -*- lexical-binding: t -*-

;; Tests for pure logic functions in avy-extras.el.
;; avy-extras depends on avy and el-patch, which are unavailable in CI,
;; so we extract and inline the pure logic for testing.

;;; Code:

(require 'ert)

;;;; Inlined pure logic

(defun avy-extras-test--action-mark-to-char (pt)
  "Mark the region from point to PT."
  (activate-mark)
  (goto-char pt))

(defun avy-extras-test--goto-char-search (char direction)
  "Search for CHAR in DIRECTION, return new point or nil if not found.
DIRECTION is `forward' or `backward'."
  (let ((search-fn (if (eq direction 'forward) 'search-forward 'search-backward)))
    (funcall search-fn (char-to-string char) nil t 1)))

(defun avy-extras-test--repeat-search-direction (direction)
  "Return the search function for DIRECTION, or signal error for invalid values."
  (pcase direction
    ('forward 'search-forward)
    ('backward 'search-backward)
    (_ (user-error "Invalid value of `direction': `%s'" direction))))

;;;; action-mark-to-char

(ert-deftest avy-extras-test-action-mark-to-char-activates-mark ()
  "After calling action-mark-to-char, mark should be active."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 1)
    (push-mark (point) t nil)
    (avy-extras-test--action-mark-to-char 6)
    (should mark-active)))

(ert-deftest avy-extras-test-action-mark-to-char-moves-point ()
  "After calling action-mark-to-char, point should be at the target position."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 1)
    (push-mark (point) t nil)
    (avy-extras-test--action-mark-to-char 6)
    (should (= (point) 6))))

(ert-deftest avy-extras-test-action-mark-to-char-region ()
  "The region between mark and point should span the expected range."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 1)
    (push-mark (point) t t)
    (avy-extras-test--action-mark-to-char 6)
    (should (= (region-beginning) 1))
    (should (= (region-end) 6))))

;;;; goto-char search logic

(ert-deftest avy-extras-test-forward-search-finds-first-occurrence ()
  "Forward search finds the first occurrence of a character after point."
  (with-temp-buffer
    (insert "abcabc")
    (goto-char 1)
    (should (avy-extras-test--goto-char-search ?b 'forward))
    (should (= (point) 3))))

(ert-deftest avy-extras-test-backward-search-finds-previous-occurrence ()
  "Backward search finds the previous occurrence of a character before point."
  (with-temp-buffer
    (insert "abcabc")
    (goto-char (point-max))
    (should (avy-extras-test--goto-char-search ?a 'backward))
    (should (= (point) 4))))

(ert-deftest avy-extras-test-forward-search-returns-nil-when-not-found ()
  "Forward search returns nil when character is not present after point."
  (with-temp-buffer
    (insert "abcdef")
    (goto-char 1)
    (should-not (avy-extras-test--goto-char-search ?z 'forward))))

(ert-deftest avy-extras-test-backward-search-returns-nil-when-not-found ()
  "Backward search returns nil when character is not present before point."
  (with-temp-buffer
    (insert "abcdef")
    (goto-char 1)
    (should-not (avy-extras-test--goto-char-search ?z 'backward))))

;;;; repeat-search direction validation

(ert-deftest avy-extras-test-repeat-search-forward-returns-search-forward ()
  "Forward direction maps to `search-forward'."
  (should (eq (avy-extras-test--repeat-search-direction 'forward) 'search-forward)))

(ert-deftest avy-extras-test-repeat-search-backward-returns-search-backward ()
  "Backward direction maps to `search-backward'."
  (should (eq (avy-extras-test--repeat-search-direction 'backward) 'search-backward)))

(ert-deftest avy-extras-test-repeat-search-invalid-direction-errors ()
  "Invalid direction value signals a user-error."
  (should-error (avy-extras-test--repeat-search-direction 'sideways)
                :type 'user-error))

(provide 'avy-extras-test)
;;; avy-extras-test.el ends here
