;;; window-extras-test.el --- Tests for window-extras -*- lexical-binding: t -*-

;; Tests for window management functions in window-extras.el.

;;; Code:

(require 'ert)
(require 'window-extras)

;;;; User options

(ert-deftest window-extras-test-frame-split-width-threshold-default ()
  "Default frame split width threshold is 350."
  (should (= window-extras-frame-split-width-threshold 350)))

;;;; Select side window

(ert-deftest window-extras-test-select-side-window-invalid-direction ()
  "Selecting a side window with an invalid direction signals an error."
  (should-error (window-extras-select-side-window "up")
                :type 'error)
  (should-error (window-extras-select-side-window "down")
                :type 'error)
  (should-error (window-extras-select-side-window "center")
                :type 'error))

(ert-deftest window-extras-test-select-side-window-left ()
  "Selecting the left side window does not signal an error."
  (should (window-extras-select-side-window "left")))

(ert-deftest window-extras-test-select-side-window-right ()
  "Selecting the right side window does not signal an error."
  (should (window-extras-select-side-window "right")))

;;;; Remove buffer from window history

(ert-deftest window-extras-test-remove-buffer-from-window-history ()
  "Removing a buffer from window history filters it from prev and next lists."
  (let ((buf (generate-new-buffer " *test-remove-history*")))
    (unwind-protect
        (let ((win (selected-window)))
          ;; Manually set prev-buffers to include our test buffer.
          (set-window-prev-buffers win (list (list buf nil nil)))
          (set-window-next-buffers win (list buf))
          ;; Remove it.
          (window-extras--remove-buffer-from-window-history buf win)
          ;; Both prev and next lists should no longer contain the buffer.
          (should (null (cl-find buf (window-prev-buffers win) :key #'car)))
          (should (null (cl-find buf (window-next-buffers win)))))
      (kill-buffer buf))))

(ert-deftest window-extras-test-remove-buffer-preserves-other-entries ()
  "Removing a buffer from window history preserves entries for other buffers."
  (let ((buf-remove (generate-new-buffer " *test-remove*"))
        (buf-keep (generate-new-buffer " *test-keep*")))
    (unwind-protect
        (let ((win (selected-window)))
          (set-window-prev-buffers win (list (list buf-remove nil nil)
                                             (list buf-keep nil nil)))
          (set-window-next-buffers win (list buf-remove buf-keep))
          (window-extras--remove-buffer-from-window-history buf-remove win)
          ;; buf-keep should still be present.
          (should (cl-find buf-keep (window-prev-buffers win) :key #'car))
          (should (cl-find buf-keep (window-next-buffers win)))
          ;; buf-remove should be gone.
          (should-not (cl-find buf-remove (window-prev-buffers win) :key #'car))
          (should-not (cl-find buf-remove (window-next-buffers win))))
      (kill-buffer buf-remove)
      (kill-buffer buf-keep))))

(ert-deftest window-extras-test-remove-buffer-empty-history ()
  "Removing a buffer from an already-empty window history does not error."
  (let ((buf (generate-new-buffer " *test-empty-history*")))
    (unwind-protect
        (let ((win (selected-window)))
          (set-window-prev-buffers win nil)
          (set-window-next-buffers win nil)
          ;; Should not signal an error.
          (window-extras--remove-buffer-from-window-history buf win)
          (should (null (window-prev-buffers win)))
          (should (null (window-next-buffers win))))
      (kill-buffer buf))))

(provide 'window-extras-test)
;;; window-extras-test.el ends here
