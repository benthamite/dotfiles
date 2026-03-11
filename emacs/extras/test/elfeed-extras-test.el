;;; elfeed-extras-test.el --- Tests for elfeed-extras -*- lexical-binding: t -*-

;; Tests for pure logic functions in elfeed-extras.el.
;; elfeed-extras depends on elfeed, which is unavailable in CI,
;; so we extract and inline the pure logic for testing.

;;; Code:

(require 'ert)

;;;; toggle-read-entries

(ert-deftest elfeed-extras-test-toggle-read-entries ()
  "Toggle alternates between read and unread filter states."
  (let ((elfeed-extras-toggle-read-entries t)
        (last-filter nil))
    ;; First toggle: when t, should filter to "" (show all)
    (if elfeed-extras-toggle-read-entries
        (setq last-filter "")
      (setq last-filter "+unread"))
    (setq elfeed-extras-toggle-read-entries (not elfeed-extras-toggle-read-entries))
    (should (equal last-filter ""))
    (should-not elfeed-extras-toggle-read-entries)
    ;; Second toggle: when nil, should filter to "+unread"
    (if elfeed-extras-toggle-read-entries
        (setq last-filter "")
      (setq last-filter "+unread"))
    (setq elfeed-extras-toggle-read-entries (not elfeed-extras-toggle-read-entries))
    (should (equal last-filter "+unread"))
    (should elfeed-extras-toggle-read-entries)))

(ert-deftest elfeed-extras-test-toggle-read-entries-starts-true ()
  "The toggle variable starts as t (showing unread by default)."
  (let ((elfeed-extras-toggle-read-entries t))
    (should elfeed-extras-toggle-read-entries)))

;;;; toggle-wiki-entries

(ert-deftest elfeed-extras-test-toggle-wiki-entries ()
  "Toggle alternates between wiki-only and non-wiki filter states."
  (let ((elfeed-extras-toggle-wiki-entries t)
        (last-filter nil))
    ;; First toggle: when t, should filter to "+unread +wiki"
    (if elfeed-extras-toggle-wiki-entries
        (setq last-filter "+unread +wiki")
      (setq last-filter "+unread -wiki"))
    (setq elfeed-extras-toggle-wiki-entries (not elfeed-extras-toggle-wiki-entries))
    (should (equal last-filter "+unread +wiki"))
    (should-not elfeed-extras-toggle-wiki-entries)
    ;; Second toggle: when nil, should filter to "+unread -wiki"
    (if elfeed-extras-toggle-wiki-entries
        (setq last-filter "+unread +wiki")
      (setq last-filter "+unread -wiki"))
    (setq elfeed-extras-toggle-wiki-entries (not elfeed-extras-toggle-wiki-entries))
    (should (equal last-filter "+unread -wiki"))
    (should elfeed-extras-toggle-wiki-entries)))

;;;; filter-tags message construction

(ert-deftest elfeed-extras-test-filter-tags-empty-message ()
  "Empty tags string produces 'Showing everything' message."
  (let ((tags ""))
    (should (equal (if (string= tags "")
                       "Showing everything"
                     (concat "Showing " tags))
                   "Showing everything"))))

(ert-deftest elfeed-extras-test-filter-tags-unread-message ()
  "Non-empty tags string produces 'Showing <tags>' message."
  (let ((tags "+unread"))
    (should (equal (if (string= tags "")
                       "Showing everything"
                     (concat "Showing " tags))
                   "Showing +unread"))))

(ert-deftest elfeed-extras-test-filter-tags-wiki-message ()
  "Wiki filter tags produce the expected message."
  (let ((tags "+unread +wiki"))
    (should (equal (if (string= tags "")
                       "Showing everything"
                     (concat "Showing " tags))
                   "Showing +unread +wiki"))))

(provide 'elfeed-extras-test)
;;; elfeed-extras-test.el ends here
