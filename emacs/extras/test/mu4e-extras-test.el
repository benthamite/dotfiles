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

;;;; Chrome helpers

(ert-deftest mu4e-extras-test-browse-url-in-chrome-profile-opens-new-tab-directly ()
  "Chrome profile opening calls Chrome directly with a new-tab request."
  (skip-unless mu4e-extras-test--loadable)
  (let (process-args)
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest args)
                 (setq process-args args)
                 'process)))
      (mu4e-extras-browse-url-in-chrome-profile
       "https://mail.google.com/mail/u/0/#inbox" "Profile 2"))
    (should-not (equal (nth 2 process-args) "open"))
    (should (member "--new-tab" process-args))
    (should (member "--profile-directory=Profile 2" process-args))
    (should (member "https://mail.google.com/mail/u/0/#inbox" process-args))))

;;;; update-mail-and-index

(ert-deftest mu4e-extras-test-update-mail-and-index-keeps-live-default-directory ()
  "Mail updates keep an existing `default-directory'."
  (skip-unless mu4e-extras-test--loadable)
  (let ((default-directory temporary-file-directory)
        seen-directory)
    (mu4e-extras--update-mail-and-index-with-live-directory
     (lambda (&rest _args)
       (setq seen-directory default-directory))
     nil)
    (should (equal seen-directory temporary-file-directory))))

(ert-deftest mu4e-extras-test-update-mail-and-index-replaces-missing-directory ()
  "Mail updates use the home directory when `default-directory' is missing."
  (skip-unless mu4e-extras-test--loadable)
  (let ((default-directory (make-temp-name
                            (expand-file-name "missing-" temporary-file-directory)))
        seen-directory)
    (should-not (file-directory-p default-directory))
    (mu4e-extras--update-mail-and-index-with-live-directory
     (lambda (&rest _args)
       (setq seen-directory default-directory))
     nil)
    (should (equal seen-directory (expand-file-name "~/")))))

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

;;;; msg-is-tlon-p

(ert-deftest mu4e-extras-test-tlon-p-work-email ()
  "Message addressed to WORK_EMAIL is classified as Tlon."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com"))
        (work-email (getenv "WORK_EMAIL")))
    (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
               (lambda (_msg field addr)
                 (and (eq field :to)
                      (equal addr work-email)))))
      (should (mu4e-extras-msg-is-tlon-p msg)))))

(ert-deftest mu4e-extras-test-tlon-p-reply-to-tlon ()
  "Message with reply-to tlon-team is classified as Tlon."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:reply-to "tlon-team@googlegroups.com")))
    (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
               (lambda (_msg field addr)
                 (and (eq field :reply-to)
                      (equal addr "tlon-team@googlegroups.com")))))
      (should (mu4e-extras-msg-is-tlon-p msg)))))

(ert-deftest mu4e-extras-test-tlon-p-personal-address ()
  "Message addressed to a personal email is not classified as Tlon."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com")))
    (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
               (lambda (_msg _field _addr) nil)))
      (should-not (mu4e-extras-msg-is-tlon-p msg)))))

;;;; msg-is-epoch-p

(ert-deftest mu4e-extras-test-epoch-p-epoch-email ()
  "Message addressed to EPOCH_EMAIL is classified as Epoch."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@epoch.example"))
        (epoch-email (getenv "EPOCH_EMAIL")))
    (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
               (lambda (_msg field addr)
                 (and (eq field :to)
                      (equal addr epoch-email)))))
      (should (mu4e-extras-msg-is-epoch-p msg)))))

(ert-deftest mu4e-extras-test-epoch-p-other-address ()
  "Message not addressed to EPOCH_EMAIL is not classified as Epoch."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
               (lambda (_msg _field _addr) nil)))
      (should-not (mu4e-extras-msg-is-epoch-p msg)))))

(ert-deftest mu4e-extras-test-epoch-html-and-plain-text-split ()
  "Epoch HTML and plain-text predicates split on the HTML check."
  (skip-unless mu4e-extras-test--loadable)
  (dolist (html '(t nil))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p) (lambda (_msg) html))
              ((symbol-function 'mu4e-extras-msg-is-epoch-p) (lambda (_msg) t)))
      (should (eq (and (mu4e-extras-msg-is-epoch-and-html-p '(:to "x")) t) html))
      (should (eq (and (mu4e-extras-msg-is-epoch-and-plain-text-p '(:to "x")) t)
                  (not html))))))

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

;;;; msg-is-tlon-and-html-p

(ert-deftest mu4e-extras-test-tlon-and-html-p-both-true ()
  "Message that is both Tlon and HTML returns non-nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) t))
              ((symbol-function 'mu4e-extras-msg-is-tlon-p)
               (lambda (_msg) t)))
      (should (mu4e-extras-msg-is-tlon-and-html-p msg)))))

(ert-deftest mu4e-extras-test-tlon-and-html-p-not-html ()
  "Message that is Tlon but not HTML returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) nil))
              ((symbol-function 'mu4e-extras-msg-is-tlon-p)
               (lambda (_msg) t)))
      (should-not (mu4e-extras-msg-is-tlon-and-html-p msg)))))

(ert-deftest mu4e-extras-test-tlon-and-html-p-not-work ()
  "Message that is HTML but not Tlon returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) t))
              ((symbol-function 'mu4e-extras-msg-is-tlon-p)
               (lambda (_msg) nil)))
      (should-not (mu4e-extras-msg-is-tlon-and-html-p msg)))))

(ert-deftest mu4e-extras-test-tlon-and-html-p-nil-msg ()
  "Nil message returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (should-not (mu4e-extras-msg-is-tlon-and-html-p nil)))

;;;; msg-is-tlon-and-plain-text-p

(ert-deftest mu4e-extras-test-tlon-and-plain-text-p-both-true ()
  "Message that is Tlon and plain text returns non-nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) nil))
              ((symbol-function 'mu4e-extras-msg-is-tlon-p)
               (lambda (_msg) t)))
      (should (mu4e-extras-msg-is-tlon-and-plain-text-p msg)))))

(ert-deftest mu4e-extras-test-tlon-and-plain-text-p-is-html ()
  "Message that is Tlon but HTML returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@work.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) t))
              ((symbol-function 'mu4e-extras-msg-is-tlon-p)
               (lambda (_msg) t)))
      (should-not (mu4e-extras-msg-is-tlon-and-plain-text-p msg)))))

(ert-deftest mu4e-extras-test-tlon-and-plain-text-p-not-work ()
  "Message that is plain text but not Tlon returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (let ((msg '(:to "user@personal.com")))
    (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p)
               (lambda (_msg) nil))
              ((symbol-function 'mu4e-extras-msg-is-tlon-p)
               (lambda (_msg) nil)))
      (should-not (mu4e-extras-msg-is-tlon-and-plain-text-p msg)))))

(ert-deftest mu4e-extras-test-tlon-and-plain-text-p-nil-msg ()
  "Nil message returns nil."
  (skip-unless mu4e-extras-test--loadable)
  (should-not (mu4e-extras-msg-is-tlon-and-plain-text-p nil)))

;;;; set-shortcuts

(defmacro mu4e-extras-test--with-shortcut-folders (&rest body)
  "Run BODY with distinct personal and Epoch folders and no shortcuts."
  `(let ((mu4e-extras-inbox-folder "/INBOX")
         (mu4e-extras-daily-folder "/Daily")
         (mu4e-extras-epoch-inbox-folder "/epoch/INBOX")
         (mu4e-extras-epoch-sent-folder "/epoch/Sent")
         (mu4e-extras-epoch-drafts-folder "/epoch/Drafts")
         (mu4e-extras-epoch-refiled-folder "/epoch/Refiled")
         (mu4e-extras-epoch-trash-folder "/epoch/Trash")
         (mu4e-maildir-shortcuts nil))
     ,@body))

(defun mu4e-extras-test--shortcut-alist ()
  "Return `mu4e-maildir-shortcuts' as a key-sorted (KEY . MAILDIR) alist."
  (sort (mapcar (lambda (s) (cons (plist-get s :key) (plist-get s :maildir)))
                mu4e-maildir-shortcuts)
        (lambda (a b) (< (car a) (car b)))))

(ert-deftest mu4e-extras-test-set-shortcuts-populates-list ()
  "Setting shortcuts maps each personal and Epoch folder to its key."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-shortcut-folders
   (mu4e-extras-set-shortcuts)
   (should (equal (mu4e-extras-test--shortcut-alist)
                  (sort (list (cons ?i "/INBOX") (cons ?y "/Daily")
                              (cons ?I "/epoch/INBOX") (cons ?T "/epoch/Sent")
                              (cons ?D "/epoch/Drafts") (cons ?R "/epoch/Refiled")
                              (cons ?X "/epoch/Trash"))
                        (lambda (a b) (< (car a) (car b))))))))

(ert-deftest mu4e-extras-test-set-shortcuts-idempotent ()
  "Calling `mu4e-extras-set-shortcuts' twice does not duplicate entries."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-shortcut-folders
   (mu4e-extras-set-shortcuts)
   (mu4e-extras-set-shortcuts)
   (should (= 7 (length mu4e-maildir-shortcuts)))))

(ert-deftest mu4e-extras-test-set-shortcuts-uses-custom-folders ()
  "Shortcuts use the values of the custom folder variables."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-shortcut-folders
   (let ((mu4e-extras-inbox-folder "/CustomInbox")
         (mu4e-extras-daily-folder "/CustomDaily"))
     (mu4e-extras-set-shortcuts)
     (should (equal "/CustomInbox" (alist-get ?i (mu4e-extras-test--shortcut-alist))))
     (should (equal "/CustomDaily" (alist-get ?y (mu4e-extras-test--shortcut-alist)))))))

(provide 'mu4e-extras-test)
;;; mu4e-extras-test.el ends here
