;;; ediff-extras-test.el --- Tests for ediff-extras -*- lexical-binding: t -*-

;; Tests for ediff buffer cleanup in ediff-extras.el.

;;; Code:

(require 'ert)
(require 'ediff)

;; Load the source file directly since it only requires ediff.
(load (expand-file-name "../ediff-extras.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;;;; Cleanup buffers

(ert-deftest ediff-extras-test-cleanup-kills-buffers ()
  "Cleanup kills all ediff buffers."
  (let ((ediff-buffer-A (generate-new-buffer " *test-A*"))
        (ediff-buffer-B (generate-new-buffer " *test-B*"))
        (ediff-buffer-C nil)
        (ediff-control-buffer (generate-new-buffer " *test-ctrl*")))
    (ediff-extras-cleanup-buffers)
    (should-not (buffer-live-p ediff-buffer-A))
    (should-not (buffer-live-p ediff-buffer-B))
    (should-not (buffer-live-p ediff-control-buffer))))

(ert-deftest ediff-extras-test-cleanup-handles-nil-buffers ()
  "Cleanup handles nil buffer slots gracefully."
  (let ((ediff-buffer-A nil)
        (ediff-buffer-B nil)
        (ediff-buffer-C nil)
        (ediff-control-buffer nil))
    ;; Should not signal an error.
    (ediff-extras-cleanup-buffers)))

(ert-deftest ediff-extras-test-cleanup-handles-already-killed-buffers ()
  "Cleanup handles already-killed buffer slots gracefully."
  (let* ((buf (generate-new-buffer " *test-already-killed*"))
         (ediff-buffer-A buf)
         (ediff-buffer-B nil)
         (ediff-buffer-C nil)
         (ediff-control-buffer nil))
    (kill-buffer buf)
    ;; Should not signal an error even though buf is already dead.
    (ediff-extras-cleanup-buffers)
    (should-not (buffer-live-p buf))))

(ert-deftest ediff-extras-test-cleanup-kills-all-four-slots ()
  "Cleanup kills buffers in all four slots including C."
  (let ((ediff-buffer-A (generate-new-buffer " *test-A*"))
        (ediff-buffer-B (generate-new-buffer " *test-B*"))
        (ediff-buffer-C (generate-new-buffer " *test-C*"))
        (ediff-control-buffer (generate-new-buffer " *test-ctrl*")))
    (ediff-extras-cleanup-buffers)
    (should-not (buffer-live-p ediff-buffer-A))
    (should-not (buffer-live-p ediff-buffer-B))
    (should-not (buffer-live-p ediff-buffer-C))
    (should-not (buffer-live-p ediff-control-buffer))))

(ert-deftest ediff-extras-test-cleanup-mixed-nil-and-live ()
  "Cleanup handles a mix of nil, killed, and live buffers."
  (let* ((live-buf (generate-new-buffer " *test-live*"))
         (dead-buf (generate-new-buffer " *test-dead*"))
         (ediff-buffer-A live-buf)
         (ediff-buffer-B nil)
         (ediff-buffer-C dead-buf)
         (ediff-control-buffer nil))
    (kill-buffer dead-buf)
    (ediff-extras-cleanup-buffers)
    (should-not (buffer-live-p live-buf))
    (should-not (buffer-live-p dead-buf))))

(provide 'ediff-extras-test)
;;; ediff-extras-test.el ends here
