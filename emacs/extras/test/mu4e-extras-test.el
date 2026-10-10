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
  "Run BODY with distinct personal, Epoch and Forethought folders."
  `(let ((mu4e-extras-inbox-folder "/INBOX")
         (mu4e-extras-daily-folder "/Daily")
         (mu4e-extras-epoch-inbox-folder "/epoch/INBOX")
         (mu4e-extras-epoch-sent-folder "/epoch/Sent")
         (mu4e-extras-epoch-drafts-folder "/epoch/Drafts")
         (mu4e-extras-epoch-refiled-folder "/epoch/Refiled")
         (mu4e-extras-epoch-trash-folder "/epoch/Trash")
         (mu4e-extras-forethought-inbox-folder "/forethought/Inbox")
         (mu4e-extras-forethought-sent-folder "/forethought/Sent")
         (mu4e-extras-forethought-drafts-folder "/forethought/Drafts")
         (mu4e-extras-forethought-refiled-folder "/forethought/Refiled")
         (mu4e-extras-forethought-trash-folder "/forethought/Trash")
         (mu4e-maildir-shortcuts nil))
     ,@body))

(defun mu4e-extras-test--shortcut-alist ()
  "Return `mu4e-maildir-shortcuts' as a key-sorted (KEY . MAILDIR) alist."
  (sort (mapcar (lambda (s) (cons (plist-get s :key) (plist-get s :maildir)))
                mu4e-maildir-shortcuts)
        (lambda (a b) (< (car a) (car b)))))

(ert-deftest mu4e-extras-test-set-shortcuts-populates-list ()
  "Setting shortcuts maps each personal, Epoch and Forethought folder to its key."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-shortcut-folders
   (mu4e-extras-set-shortcuts)
   (should (equal (mu4e-extras-test--shortcut-alist)
                  (sort (list (cons ?i "/INBOX") (cons ?y "/Daily")
                              (cons ?I "/epoch/INBOX") (cons ?T "/epoch/Sent")
                              (cons ?D "/epoch/Drafts") (cons ?R "/epoch/Refiled")
                              (cons ?X "/epoch/Trash")
                              (cons ?f "/forethought/Inbox")
                              (cons ?s "/forethought/Sent")
                              (cons ?w "/forethought/Drafts")
                              (cons ?F "/forethought/Refiled")
                              (cons ?z "/forethought/Trash"))
                        (lambda (a b) (< (car a) (car b))))))))

(ert-deftest mu4e-extras-test-set-shortcuts-idempotent ()
  "Calling `mu4e-extras-set-shortcuts' twice does not duplicate entries."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-shortcut-folders
   (mu4e-extras-set-shortcuts)
   (mu4e-extras-set-shortcuts)
   (should (= 12 (length mu4e-maildir-shortcuts)))))

(ert-deftest mu4e-extras-test-set-shortcuts-uses-custom-folders ()
  "Shortcuts use the values of the custom folder variables."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-shortcut-folders
   (let ((mu4e-extras-inbox-folder "/CustomInbox")
         (mu4e-extras-daily-folder "/CustomDaily"))
     (mu4e-extras-set-shortcuts)
     (should (equal "/CustomInbox" (alist-get ?i (mu4e-extras-test--shortcut-alist))))
     (should (equal "/CustomDaily" (alist-get ?y (mu4e-extras-test--shortcut-alist)))))))

;;;; Gmail API accounts

(defvar org-msg-extras-personal-html-signature "personal-html")
(defvar org-msg-extras-personal-plain-text-signature "personal-plain")
(defvar org-msg-extras-epoch-html-signature "epoch-html")
(defvar org-msg-extras-epoch-plain-text-signature "epoch-plain")
(defvar org-msg-extras-forethought-html-signature "forethought-html")
(defvar org-msg-extras-forethought-plain-text-signature "forethought-plain")
(defvar org-msg-extras-tlon-html-signature "tlon-html")
(defvar org-msg-extras-tlon-plain-text-signature "tlon-plain")

(defmacro mu4e-extras-test--with-addresses (&rest body)
  "Run BODY with fixed personal, Epoch and Tlon addresses in the environment."
  `(let ((process-environment
          (append '("PERSONAL_GMAIL=me@gmail.test"
                    "PERSONAL_EMAIL=me@personal.test"
                    "EPOCH_EMAIL=me@epoch.test"
                    "WORK_EMAIL=me@tlon.test")
                  process-environment)))
     ,@body))

(defun mu4e-extras-test--msg (maildir &rest to)
  "Return a message plist stored in MAILDIR and addressed to TO."
  (list :maildir maildir :to (mapcar (lambda (email) (list :email email)) to)))

(defun mu4e-extras-test--contact-matches (msg field address)
  "Return non-nil if ADDRESS is among the FIELD contacts of MSG."
  (and address
       (cl-some (lambda (contact) (equal (plist-get contact :email) address))
                (plist-get msg field))))

(ert-deftest mu4e-extras-test-msg-account-by-maildir ()
  "The owning Gmail API account is resolved from the maildir prefix."
  (skip-unless mu4e-extras-test--loadable)
  (should (equal "epoch" (plist-get (mu4e-extras-msg-account '(:maildir "/epoch/Inbox")) :name)))
  (should (equal "forethought"
                 (plist-get (mu4e-extras-msg-account '(:maildir "/forethought/Sent")) :name)))
  (should-not (mu4e-extras-msg-account '(:maildir "/Inbox")))
  (should-not (mu4e-extras-msg-account '(:maildir "/epochs/Inbox")))
  (should-not (mu4e-extras-msg-account nil)))

(ert-deftest mu4e-extras-test-account-for-address ()
  "The sending account is resolved from its email address."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-addresses
   (should (equal "epoch" (plist-get (mu4e-extras-account-for-address "me@epoch.test") :name)))
   (should (equal "forethought"
                  (plist-get (mu4e-extras-account-for-address "Pablo@Forethought.org") :name)))
   (should-not (mu4e-extras-account-for-address "me@gmail.test"))
   (should-not (mu4e-extras-account-for-address nil))))

(ert-deftest mu4e-extras-test-refile-and-trash-folders-per-account ()
  "Refile and trash folders live in the owning account's maildir."
  (skip-unless mu4e-extras-test--loadable)
  (should (equal "/epoch/Refiled" (mu4e-extras-refile-folder '(:maildir "/epoch/Inbox"))))
  (should (equal "/epoch/Trash" (mu4e-extras-trash-folder '(:maildir "/epoch/Inbox"))))
  (should (equal "/forethought/Refiled"
                 (mu4e-extras-refile-folder '(:maildir "/forethought/Inbox"))))
  (should (equal "/forethought/Trash"
                 (mu4e-extras-trash-folder '(:maildir "/forethought/Inbox"))))
  (should (equal "/Refiled" (mu4e-extras-refile-folder '(:maildir "/Inbox"))))
  (should (equal "/Trash" (mu4e-extras-trash-folder '(:maildir "/Inbox")))))

(ert-deftest mu4e-extras-test-chrome-profile-per-account ()
  "Each account opens Gmail in its own Chrome profile."
  (skip-unless mu4e-extras-test--loadable)
  (let ((mu4e-extras-chrome-profile-personal "Default"))
    (should (equal "Profile 2" (mu4e-extras-chrome-profile-for-msg '(:maildir "/epoch/Inbox"))))
    (should (equal "Profile 10"
                   (mu4e-extras-chrome-profile-for-msg '(:maildir "/forethought/Inbox"))))
    (should (equal "Default" (mu4e-extras-chrome-profile-for-msg '(:maildir "/Inbox"))))
    (should (equal "Default" (mu4e-extras-chrome-profile-for-msg nil)))))

(ert-deftest mu4e-extras-test-gmail-thread-id-queries-account-state-db ()
  "Thread lookup queries the state database of the given account."
  (skip-unless mu4e-extras-test--loadable)
  (let ((db (make-temp-file "mu4e-extras-state-" nil ".db"))
        queried-db)
    (unwind-protect
        (cl-letf (((symbol-function 'call-process)
                   (lambda (&rest args)
                     (setq queried-db (nth 4 args))
                     (insert "thread123\n")
                     0)))
          (should (equal "thread123"
                         (mu4e-extras-gmail-thread-id "abc" (list :state-db db))))
          (should (equal db queried-db))
          (should-not (mu4e-extras-gmail-thread-id "abc" nil))
          (should-not (mu4e-extras-gmail-thread-id
                       "abc" (list :state-db (concat db ".missing")))))
      (delete-file db))))

(ert-deftest mu4e-extras-test-account-state-db-paths ()
  "Each Gmail API account has its own state database."
  (skip-unless mu4e-extras-test--loadable)
  (should (equal "~/.local/share/gmail-maildir-sync/state.db"
                 (plist-get (mu4e-extras-msg-account '(:maildir "/epoch/Inbox")) :state-db)))
  (should (equal "~/.local/share/gmail-maildir-sync/forethought/state.db"
                 (plist-get (mu4e-extras-msg-account '(:maildir "/forethought/Inbox"))
                            :state-db))))

(ert-deftest mu4e-extras-test-gmail-fix-flags-skips-api-accounts ()
  "IMAP flag fixes are skipped for every Gmail API account."
  (skip-unless mu4e-extras-test--loadable)
  (let (retagged)
    (cl-letf (((symbol-function 'mu4e-action-retag-message)
               (lambda (msg _tags) (push (plist-get msg :maildir) retagged))))
      (mu4e-extras-gmail-fix-flags 'refile '(:maildir "/epoch/Inbox"))
      (mu4e-extras-gmail-fix-flags 'refile '(:maildir "/forethought/Inbox"))
      (mu4e-extras-gmail-fix-flags 'refile '(:maildir "/Inbox")))
    (should (equal '("/Inbox") retagged))))

;;;; Bookmarks

(ert-deftest mu4e-extras-test-set-bookmarks-includes-all-inboxes-once ()
  "The \"All inboxes\" bookmark covers every inbox and is not duplicated."
  (skip-unless mu4e-extras-test--loadable)
  (let ((mu4e-extras-inbox-folder "/Inbox")
        (mu4e-extras-epoch-inbox-folder "/epoch/Inbox")
        (mu4e-extras-forethought-inbox-folder "/forethought/Inbox")
        (mu4e-bookmarks '((:name "Unread messages" :query "flag:unread" :key ?u))))
    (mu4e-extras-set-bookmarks)
    (let ((mu4e-extras-forethought-inbox-folder "/forethought/INBOX"))
      (mu4e-extras-set-bookmarks))
    (mu4e-extras-set-bookmarks)
    (let ((all (seq-filter (lambda (bookmark)
                             (equal (plist-get bookmark :name) "All inboxes"))
                           mu4e-bookmarks)))
      (should (= 1 (length all)))
      (should (equal "maildir:/Inbox OR maildir:/epoch/Inbox OR maildir:/forethought/Inbox"
                     (plist-get (car all) :query)))
      (should (= 2 (length mu4e-bookmarks))))))

;;;; Contexts

(defun mu4e-extras-test--context-name-for (msg)
  "Return the name of the first context in `mu4e-contexts' matching MSG."
  (when-let* ((context (cl-find-if (lambda (context)
                                     (funcall (mu4e-context-match-func context) msg))
                                   mu4e-contexts)))
    (mu4e-context-name context)))

(defmacro mu4e-extras-test--with-contexts (html &rest body)
  "Run BODY with contexts set, messages being HTML iff HTML is non-nil."
  (declare (indent 1))
  `(mu4e-extras-test--with-addresses
    (let (mu4e-contexts)
      (cl-letf (((symbol-function 'mu4e-extras-msg-is-html-p) (lambda (_msg) ,html))
                ((symbol-function 'mu4e-message-contact-field-matches)
                 #'mu4e-extras-test--contact-matches))
        (mu4e-extras-set-contexts)
        ,@body))))

(ert-deftest mu4e-extras-test-contexts-names-and-vars ()
  "Contexts are numbered in order and Forethought sends through the Gmail API."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-contexts t
    (should (equal '("1 Personal HTML" "2 Personal plain text"
                     "3 Epoch HTML" "4 Epoch plain text"
                     "5 Forethought HTML" "6 Forethought plain text"
                     "7 Tlon HTML" "8 Tlon plain text")
                   (mapcar #'mu4e-context-name mu4e-contexts)))
    (let ((vars (mu4e-context-vars (nth 5 mu4e-contexts))))
      (should (equal "pablo@forethought.org" (alist-get 'user-mail-address vars)))
      (should (equal "/forethought/Sent" (alist-get 'mu4e-sent-folder vars)))
      (should (equal "/forethought/Drafts" (alist-get 'mu4e-drafts-folder vars)))
      (should (eq 'mu4e-extras-send-via-gmail-api
                  (alist-get 'message-send-mail-function vars)))
      (should (equal "forethought-plain" (alist-get 'org-msg-signature vars))))
    (let ((vars (mu4e-context-vars (nth 2 mu4e-contexts))))
      (should (equal "me@epoch.test" (alist-get 'user-mail-address vars)))
      (should (equal "/epoch/Sent" (alist-get 'mu4e-sent-folder vars)))
      (should (equal "epoch-html" (alist-get 'org-msg-signature vars))))))

(ert-deftest mu4e-extras-test-contexts-maildir-ownership-wins ()
  "The owning maildir decides the context of Gmail API account messages."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-contexts t
    (should (equal "5 Forethought HTML"
                   (mu4e-extras-test--context-name-for
                    (mu4e-extras-test--msg "/forethought/Inbox"
                                           "me@epoch.test" "pablo@forethought.org"))))
    (should (equal "5 Forethought HTML"
                   (mu4e-extras-test--context-name-for
                    (mu4e-extras-test--msg "/forethought/Inbox" "me@gmail.test"))))
    (should (equal "5 Forethought HTML"
                   (mu4e-extras-test--context-name-for
                    (mu4e-extras-test--msg "/forethought/Sent" "someone@else.test"))))
    (should (equal "3 Epoch HTML"
                   (mu4e-extras-test--context-name-for
                    (mu4e-extras-test--msg "/epoch/Inbox"
                                           "pablo@forethought.org" "me@gmail.test"))))))

(ert-deftest mu4e-extras-test-contexts-recipient-rules-for-mbsync-mail ()
  "Recipient headers still decide the context of non-API mail."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-contexts nil
    (should (equal "2 Personal plain text"
                   (mu4e-extras-test--context-name-for
                    (mu4e-extras-test--msg "/Inbox" "me@gmail.test"))))
    (should (equal "4 Epoch plain text"
                   (mu4e-extras-test--context-name-for
                    (mu4e-extras-test--msg "/Inbox" "me@epoch.test"))))
    (should (equal "6 Forethought plain text"
                   (mu4e-extras-test--context-name-for
                    (mu4e-extras-test--msg "/Inbox" "pablo@forethought.org"))))
    (should (equal "8 Tlon plain text"
                   (mu4e-extras-test--context-name-for
                    (mu4e-extras-test--msg "/Inbox" "me@tlon.test"))))
    (should-not (mu4e-extras-test--context-name-for
                 (mu4e-extras-test--msg "/Inbox" "someone@else.test")))))

;;;; Sending

(defun mu4e-extras-test--send-args (sender)
  "Send a message as SENDER via the Gmail API and return the process arguments."
  (let ((user-mail-address sender)
        (mail-header-separator "--text follows this line--")
        args)
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (&rest call-args)
                 (setq args call-args)
                 0)))
      (with-temp-buffer
        (insert "From: " sender "\nSubject: hi\n--text follows this line--\nbody\n")
        (mu4e-extras-send-via-gmail-api)
        (should (equal (buffer-string) (concat "From: " sender "\nSubject: hi\n\nbody\n")))))
    args))

(ert-deftest mu4e-extras-test-send-via-gmail-api-passes-account ()
  "Sending passes the account of the sender address to `gmail-maildir-sync'."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-addresses
   (should (equal '("--account" "forethought" "send")
                  (last (mu4e-extras-test--send-args "pablo@forethought.org") 3)))
   (should (equal '("--account" "epoch" "send")
                  (last (mu4e-extras-test--send-args "me@epoch.test") 3)))
   (should-error (mu4e-extras-test--send-args "me@gmail.test"))))

(ert-deftest mu4e-extras-test-reaction-send-uses-message-account ()
  "Reactions to Gmail API account messages go through that account."
  (skip-unless mu4e-extras-test--loadable)
  (let (args smtp-to)
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (&rest call-args) (setq args call-args) 0))
              ((symbol-function 'mu4e-extras-reaction-send-via-smtp)
               (lambda (to) (setq smtp-to to))))
      (mu4e-extras-reaction-send '(:maildir "/forethought/Inbox") "x@y.test" "mime")
      (should (equal '("--account" "forethought" "send") (last args 3)))
      (mu4e-extras-reaction-send '(:maildir "/Inbox") "x@y.test" "mime")
      (should (equal "x@y.test" smtp-to)))))

(ert-deftest mu4e-extras-test-reaction-from-address-uses-owning-account ()
  "The reaction sender is the owning account's address."
  (skip-unless mu4e-extras-test--loadable)
  (mu4e-extras-test--with-addresses
   (cl-letf (((symbol-function 'mu4e-message-contact-field-matches)
              #'mu4e-extras-test--contact-matches))
     (should (equal "pablo@forethought.org"
                    (mu4e-extras-reaction-from-address
                     (mu4e-extras-test--msg "/forethought/Inbox" "me@epoch.test"))))
     (should (equal "me@epoch.test"
                    (mu4e-extras-reaction-from-address
                     (mu4e-extras-test--msg "/Inbox" "me@epoch.test"))))
     (should (equal "me@gmail.test"
                    (mu4e-extras-reaction-from-address
                     (mu4e-extras-test--msg "/Inbox" "someone@else.test")))))))

;;;; All-mail sync

(ert-deftest mu4e-extras-test-all-mail-command-pulls-all-accounts ()
  "The all-mail command pulls every account and waits on each process."
  (skip-unless mu4e-extras-test--loadable)
  (let ((command (mu4e-extras-all-mail-command)))
    (should (string-match-p "mbsync gmail-all & " command))
    (should (string-match-p "gmail-maildir-sync --account epoch pull --include-all & " command))
    (should (string-match-p "gmail-maildir-sync --account forethought pull --include-all & "
                            command))
    (should (string-search "for pid in $pids; do wait $pid || status=$?; done; exit $status"
                           command))
    (should-not (string-match-p "& wait'" command))))

(ert-deftest mu4e-extras-test-all-mail-command-propagates-failure ()
  "The all-mail shell script exits non-zero when any child fails."
  (skip-unless mu4e-extras-test--loadable)
  (let ((mu4e-extras-gmail-api-accounts '((:name "a") (:name "b"))))
    (cl-flet ((run (script)
                (call-process "sh" nil nil nil "-c"
                              (replace-regexp-in-string
                               "\\`sh -c '\\(.*\\)'\\'" "\\1" script))))
      (let ((base (mu4e-extras-all-mail-command)))
        (should (= 0 (run (replace-regexp-in-string
                           "mbsync gmail-all\\|gmail-maildir-sync --account [ab] pull --include-all"
                           "true" base))))
        (should-not (= 0 (run (replace-regexp-in-string
                               "gmail-maildir-sync --account a pull --include-all" "false"
                               (replace-regexp-in-string
                                "mbsync gmail-all\\|gmail-maildir-sync --account b pull --include-all"
                                "true" base)))))))))

(provide 'mu4e-extras-test)
;;; mu4e-extras-test.el ends here
