;;; tab-bar-extras-test.el --- Tests for tab-bar-extras -*- lexical-binding: t -*-

;; Tests for notification state management and global-mode-string
;; functions in tab-bar-extras.el.

;;; Code:

(require 'ert)
(require 'tab-bar-extras)

;;;; Get state

(ert-deftest tab-bar-extras-test-get-state-enable ()
  "Get-state returns t when action is `enable'."
  (should (eq (tab-bar-extras-get-state 'enable) t)))

(ert-deftest tab-bar-extras-test-get-state-disable ()
  "Get-state returns nil when action is `disable'."
  (should (eq (tab-bar-extras-get-state 'disable) nil)))

(ert-deftest tab-bar-extras-test-get-state-toggle-from-true ()
  "Get-state toggles to nil when var is currently t."
  (let ((tab-bar-extras-notifications-enabled t))
    (should (eq (tab-bar-extras-get-state nil 'tab-bar-extras-notifications-enabled) nil))))

(ert-deftest tab-bar-extras-test-get-state-toggle-from-false ()
  "Get-state toggles to t when var is currently nil."
  (let ((tab-bar-extras-notifications-enabled nil))
    (should (eq (tab-bar-extras-get-state nil 'tab-bar-extras-notifications-enabled) t))))

(ert-deftest tab-bar-extras-test-get-state-enable-ignores-var ()
  "Get-state returns t for `enable' regardless of var value."
  (let ((tab-bar-extras-notifications-enabled nil))
    (should (eq (tab-bar-extras-get-state 'enable 'tab-bar-extras-notifications-enabled) t))))

(ert-deftest tab-bar-extras-test-get-state-disable-ignores-var ()
  "Get-state returns nil for `disable' regardless of var value."
  (let ((tab-bar-extras-notifications-enabled t))
    (should (eq (tab-bar-extras-get-state 'disable 'tab-bar-extras-notifications-enabled) nil))))

;;;; Set global mode string

(ert-deftest tab-bar-extras-test-set-global-mode-string ()
  "Set-global-mode-string assigns the custom value."
  (let ((tab-bar-extras-global-mode-string '("test-element"))
        (global-mode-string nil))
    (tab-bar-extras-set-global-mode-string)
    (should (equal global-mode-string '("test-element")))))

(ert-deftest tab-bar-extras-test-set-global-mode-string-nil ()
  "Set-global-mode-string sets nil when custom value is nil."
  (let ((tab-bar-extras-global-mode-string nil)
        (global-mode-string '("something")))
    (tab-bar-extras-set-global-mode-string)
    (should (null global-mode-string))))

;;;; Toggle individual notifications

(ert-deftest tab-bar-extras-test-toggle-telega-notifications ()
  "Toggle-telega-notifications toggles the variable."
  (let ((tab-bar-extras-telega-notifications-enabled t))
    (tab-bar-extras-toggle-telega-notifications)
    (should-not tab-bar-extras-telega-notifications-enabled))
  (let ((tab-bar-extras-telega-notifications-enabled nil))
    (tab-bar-extras-toggle-telega-notifications)
    (should tab-bar-extras-telega-notifications-enabled)))

(ert-deftest tab-bar-extras-test-toggle-github-notifications ()
  "Toggle-github-notifications toggles the variable."
  (let ((tab-bar-extras-github-notifications-enabled t))
    (tab-bar-extras-toggle-github-notifications)
    (should-not tab-bar-extras-github-notifications-enabled))
  (let ((tab-bar-extras-github-notifications-enabled nil))
    (tab-bar-extras-toggle-github-notifications)
    (should tab-bar-extras-github-notifications-enabled)))

(ert-deftest tab-bar-extras-test-toggle-telega-enable ()
  "Toggle-telega-notifications with `enable' sets to t."
  (let ((tab-bar-extras-telega-notifications-enabled nil))
    (tab-bar-extras-toggle-telega-notifications 'enable)
    (should tab-bar-extras-telega-notifications-enabled)))

(ert-deftest tab-bar-extras-test-toggle-telega-disable ()
  "Toggle-telega-notifications with `disable' sets to nil."
  (let ((tab-bar-extras-telega-notifications-enabled t))
    (tab-bar-extras-toggle-telega-notifications 'disable)
    (should-not tab-bar-extras-telega-notifications-enabled)))

(provide 'tab-bar-extras-test)
;;; tab-bar-extras-test.el ends here
