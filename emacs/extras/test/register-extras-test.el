;;; register-extras-test.el --- Tests for register-extras -*- lexical-binding: t -*-

;; Tests for register operations and buffer association
;; in register-extras.el.

;;; Code:

(require 'ert)
(require 'register-extras)

;;;; Kill to register

(ert-deftest register-extras-test-kill-to-register ()
  "Kill-to-register stores text in the specified register."
  (let ((register-alist nil))
    (register-extras-kill-to-register "hello world" ?a)
    (should (equal (get-register ?a) "hello world"))))

(ert-deftest register-extras-test-kill-to-register-overwrites ()
  "Kill-to-register overwrites existing register content."
  (let ((register-alist nil))
    (register-extras-kill-to-register "first" ?a)
    (register-extras-kill-to-register "second" ?a)
    (should (equal (get-register ?a) "second"))))

;;;; Buffer to register

(ert-deftest register-extras-test-buffer-to-register ()
  "Buffer-to-register associates a key with the current buffer."
  (let ((register-extras-keys-alist nil))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (register-extras-buffer-to-register ?x)
        (should (eq (alist-get ?x register-extras-keys-alist) buf))))))

(ert-deftest register-extras-test-buffer-to-register-overwrites ()
  "Buffer-to-register overwrites previous association for same key."
  (let ((register-extras-keys-alist nil))
    (with-temp-buffer
      (register-extras-buffer-to-register ?x))
    (with-temp-buffer
      (let ((buf2 (current-buffer)))
        (register-extras-buffer-to-register ?x)
        (should (eq (alist-get ?x register-extras-keys-alist) buf2))))))

;;;; Jump to buffer

(ert-deftest register-extras-test-jump-to-buffer ()
  "Jump-to-buffer switches to the associated buffer."
  (let ((register-extras-keys-alist nil))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (register-extras-buffer-to-register ?y)
        ;; Switch away
        (switch-to-buffer (get-buffer-create "*register-test-other*"))
        (register-extras-jump-to-buffer ?y)
        (should (eq (current-buffer) buf))))
    (when (get-buffer "*register-test-other*")
      (kill-buffer "*register-test-other*"))))

(ert-deftest register-extras-test-jump-to-buffer-no-association ()
  "Jump-to-buffer handles missing key gracefully."
  (let ((register-extras-keys-alist nil))
    ;; Should not error, just message
    (register-extras-jump-to-buffer ?z)))

(provide 'register-extras-test)
;;; register-extras-test.el ends here
