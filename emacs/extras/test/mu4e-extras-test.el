;;; mu4e-extras-test.el --- Tests for mu4e-extras -*- lexical-binding: t -*-

;; Tests for message classification predicates in mu4e-extras.el.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; mu4e may not be available in CI; skip all tests if so.
(defvar mu4e-extras-test--loadable
  (condition-case nil
      (progn (require 'mu4e-extras) t)
    (error nil))
  "Non-nil when mu4e-extras loaded successfully.")

;;;; msg-is-personal-p

(ert-deftest mu4e-extras-test-personal-p-personal-gmail ()
  "Message addressed to PERSONAL_GMAIL is classified as personal."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com"))
        (personal-gmail (getenv "PERSONAL_GMAIL"))
        (personal-email (getenv "PERSONAL_EMAIL")))
    (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
               (lambda (_msg field addr)
                 (and (eq field :to)
                      (equal addr personal-gmail)))))
      (should (mu4e-extras-msg-is-personal-p msg)))))

(ert-deftest mu4e-extras-test-personal-p-personal-email ()
  "Message addressed to PERSONAL_EMAIL is classified as personal."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com"))
        (personal-email (getenv "PERSONAL_EMAIL")))
    (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
               (lambda (_msg field addr)
                 (and (eq field :to)
                      (equal addr personal-email)))))
      (should (mu4e-extras-msg-is-personal-p msg)))))

(ert-deftest mu4e-extras-test-personal-p-work-address ()
  "Message addressed to a work email is not classified as personal."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
               (lambda (_msg _field _addr) nil)))
      (should-not (mu4e-extras-msg-is-personal-p msg)))))

;;;; msg-is-work-p

(ert-deftest mu4e-extras-test-work-p-work-email ()
  "Message addressed to WORK_EMAIL is classified as work."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com"))
        (work-email (getenv "WORK_EMAIL")))
    (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
               (lambda (_msg field addr)
                 (and (eq field :to)
                      (equal addr work-email)))))
      (should (mu4e-extras-msg-is-work-p msg)))))

(ert-deftest mu4e-extras-test-work-p-reply-to-tlon ()
  "Message with reply-to tlon-team is classified as work."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:reply-to "tlon-team@googlegroups.com")))
    (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
               (lambda (_msg field addr)
                 (and (eq field :reply-to)
                      (equal addr "tlon-team@googlegroups.com")))))
      (should (mu4e-extras-msg-is-work-p msg)))))

(ert-deftest mu4e-extras-test-work-p-personal-address ()
  "Message addressed to a personal email is not classified as work."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com")))
    (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
               (lambda (_msg _field _addr) nil)))
      (should-not (mu4e-extras-msg-is-work-p msg)))))

;;;; msg-is-personal-and-html-p

(ert-deftest mu4e-extras-test-personal-and-html-p-both-true ()
  "Message that is both personal and HTML returns non-nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) t))
              ((symbol-function 'mu4e-extras-msg-is-personal-p)
               (lambda (_msg) t)))
      (should (mu4e-extras-msg-is-personal-and-html-p msg)))))

(ert-deftest mu4e-extras-test-personal-and-html-p-not-html ()
  "Message that is personal but not HTML returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) nil))
              ((symbol-function 'mu4e-extras-msg-is-personal-p)
               (lambda (_msg) t)))
      (should-not (mu4e-extras-msg-is-personal-and-html-p msg)))))

(ert-deftest mu4e-extras-test-personal-and-html-p-not-personal ()
  "Message that is HTML but not personal returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) t))
              ((symbol-function 'mu4e-extras-msg-is-personal-p)
               (lambda (_msg) nil)))
      (should-not (mu4e-extras-msg-is-personal-and-html-p msg)))))

(ert-deftest mu4e-extras-test-personal-and-html-p-nil-msg ()
  "Nil message returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (should-not (mu4e-extras-msg-is-personal-and-html-p nil)))

;;;; msg-is-personal-and-plain-text-p

(ert-deftest mu4e-extras-test-personal-and-plain-text-p-both-true ()
  "Message that is personal and plain text returns non-nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) nil))
              ((symbol-function 'mu4e-extras-msg-is-personal-p)
               (lambda (_msg) t)))
      (should (mu4e-extras-msg-is-personal-and-plain-text-p msg)))))

(ert-deftest mu4e-extras-test-personal-and-plain-text-p-is-html ()
  "Message that is personal but HTML returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) t))
              ((symbol-function 'mu4e-extras-msg-is-personal-p)
               (lambda (_msg) t)))
      (should-not (mu4e-extras-msg-is-personal-and-plain-text-p msg)))))

(ert-deftest mu4e-extras-test-personal-and-plain-text-p-not-personal ()
  "Message that is plain text but not personal returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) nil))
              ((symbol-function 'mu4e-extras-msg-is-personal-p)
               (lambda (_msg) nil)))
      (should-not (mu4e-extras-msg-is-personal-and-plain-text-p msg)))))

(ert-deftest mu4e-extras-test-personal-and-plain-text-p-nil-msg ()
  "Nil message returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (should-not (mu4e-extras-msg-is-personal-and-plain-text-p nil)))

;;;; msg-is-work-and-html-p

(ert-deftest mu4e-extras-test-work-and-html-p-both-true ()
  "Message that is both work and HTML returns non-nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) t))
              ((symbol-function 'mu4e-extras-msg-is-work-p)
               (lambda (_msg) t)))
      (should (mu4e-extras-msg-is-work-and-html-p msg)))))

(ert-deftest mu4e-extras-test-work-and-html-p-not-html ()
  "Message that is work but not HTML returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) nil))
              ((symbol-function 'mu4e-extras-msg-is-work-p)
               (lambda (_msg) t)))
      (should-not (mu4e-extras-msg-is-work-and-html-p msg)))))

(ert-deftest mu4e-extras-test-work-and-html-p-not-work ()
  "Message that is HTML but not work returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) t))
              ((symbol-function 'mu4e-extras-msg-is-work-p)
               (lambda (_msg) nil)))
      (should-not (mu4e-extras-msg-is-work-and-html-p msg)))))

(ert-deftest mu4e-extras-test-work-and-html-p-nil-msg ()
  "Nil message returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (should-not (mu4e-extras-msg-is-work-and-html-p nil)))

;;;; msg-is-work-and-plain-text-p

(ert-deftest mu4e-extras-test-work-and-plain-text-p-both-true ()
  "Message that is work and plain text returns non-nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) nil))
              ((symbol-function 'mu4e-extras-msg-is-work-p)
               (lambda (_msg) t)))
      (should (mu4e-extras-msg-is-work-and-plain-text-p msg)))))

(ert-deftest mu4e-extras-test-work-and-plain-text-p-is-html ()
  "Message that is work but HTML returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) t))
              ((symbol-function 'mu4e-extras-msg-is-work-p)
               (lambda (_msg) t)))
      (should-not (mu4e-extras-msg-is-work-and-plain-text-p msg)))))

(ert-deftest mu4e-extras-test-work-and-plain-text-p-not-work ()
  "Message that is plain text but not work returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) nil))
              ((symbol-function 'mu4e-extras-msg-is-work-p)
               (lambda (_msg) nil)))
      (should-not (mu4e-extras-msg-is-work-and-plain-text-p msg)))))

(ert-deftest mu4e-extras-test-work-and-plain-text-p-nil-msg ()
  "Nil message returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (should-not (mu4e-extras-msg-is-work-and-plain-text-p nil)))

;;;; set-shortcuts

(ert-deftest mu4e-extras-test-set-shortcuts-populates-list ()
  "Setting shortcuts populates `mu4e-maildir-shortcuts' with inbox and daily."
  (skip-unless mu4e-extras-test--loadable)
  (let ((mu4e-extras-inbox-folder "/INBOX")
        (mu4e-extras-daily-folder "/Daily")
        (mu4e-maildir-shortcuts nil))
    (mu4e-extras-set-shortcuts)
    (should (= 2 (length mu4e-maildir-shortcuts)))
    ;; add-to-list prepends, so daily (added second) is car
    (should (equal "/Daily" (plist-get (car mu4e-maildir-shortcuts) :maildir)))
    (should (equal ?y (plist-get (car mu4e-maildir-shortcuts) :key)))
    (should (equal "/INBOX" (plist-get (cadr mu4e-maildir-shortcuts) :maildir)))
    (should (equal ?i (plist-get (cadr mu4e-maildir-shortcuts) :key)))))

(ert-deftest mu4e-extras-test-set-shortcuts-idempotent ()
  "Calling `mu4e-extras-set-shortcuts' twice does not duplicate entries."
  (skip-unless mu4e-extras-test--loadable)
  (let ((mu4e-extras-inbox-folder "/INBOX")
        (mu4e-extras-daily-folder "/Daily")
        (mu4e-maildir-shortcuts nil))
    (mu4e-extras-set-shortcuts)
    (mu4e-extras-set-shortcuts)
    (should (= 2 (length mu4e-maildir-shortcuts)))))

(ert-deftest mu4e-extras-test-set-shortcuts-uses-custom-folders ()
  "Shortcuts use the values of the custom folder variables."
  (skip-unless mu4e-extras-test--loadable)
  (let ((mu4e-extras-inbox-folder "/CustomInbox")
        (mu4e-extras-daily-folder "/CustomDaily")
        (mu4e-maildir-shortcuts nil))
    (mu4e-extras-set-shortcuts)
    ;; add-to-list prepends, so daily (added second) is car
    (should (equal "/CustomDaily" (plist-get (car mu4e-maildir-shortcuts) :maildir)))
    (should (equal "/CustomInbox" (plist-get (cadr mu4e-maildir-shortcuts) :maildir)))))

(provide 'mu4e-extras-test)
;;; mu4e-extras-test.el ends here
