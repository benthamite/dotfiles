;;; frame-extras-test.el --- Tests for frame-extras -*- lexical-binding: t -*-

;; Tests for frame dimension variables and frame state predicates
;; in frame-extras.el.

;;; Code:

(require 'ert)

;; frame-extras only requires built-in `frame', safe to load directly.
(load (expand-file-name "../frame-extras.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;;;; Default variable values

(ert-deftest frame-extras-test-maximized-frame-width-default ()
  "Default maximized frame width is 244."
  (should (= frame-extras-maximized-frame-width 244)))

(ert-deftest frame-extras-test-maximized-frame-width-is-integer ()
  "Maximized frame width is an integer."
  (should (integerp frame-extras-maximized-frame-width)))

;;;; Frame state in batch mode

(ert-deftest frame-extras-test-frame-not-maximized-in-batch ()
  "In batch mode, the frame fullscreen parameter is nil (not maximized)."
  (should (null (frame-parameter nil 'fullscreen))))

(ert-deftest frame-extras-test-frame-width-positive ()
  "Frame width is a positive integer even in batch mode."
  (should (> (frame-width) 0)))

(provide 'frame-extras-test)
;;; frame-extras-test.el ends here
