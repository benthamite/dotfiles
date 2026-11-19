;;; protean-extras-test.el --- Tests for protean-extras -*- lexical-binding: t -*-

;;; Commentary:

;; The openers with their clients stubbed, on invented messages.  Only
;; the email test uses the real client, mu4e, and is skipped without it;
;; it renders a message file written here and never asks mu for one.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'protean-extras)

(defmacro protean-extras-test--with-features (features &rest body)
  "Run BODY with `require' loading nothing for FEATURES.
The clients' functions are stubbed in BODY, and loading a client would
put its own definitions back."
  (declare (indent 1) (debug t))
  (let ((original (make-symbol "original")))
    `(let ((,original (symbol-function 'require)))
       (cl-letf (((symbol-function 'require)
                  (lambda (feature &rest rest)
                    (if (memq feature ,features) feature (apply ,original feature rest)))))
         ,@body))))

(defvar protean-extras-test--bounds nil
  "The BOUNDS the last opener called in a test gave DONE.")

(defun protean-extras-test--opened (opener message)
  "Call OPENER on MESSAGE; return (TAKEN BUFFER REASON) as it answered.
The BOUNDS it gave, if any, are in `protean-extras-test--bounds'."
  (let* ((answer nil)
         (taken (funcall opener message (lambda (buffer &optional reason bounds)
                                          (setq protean-extras-test--bounds bounds)
                                          (setq answer (list buffer reason))))))
    (cons taken answer)))

(ert-deftest protean-extras-test-each-opener-leaves-other-channels ()
  "An opener returns nil, and answers nothing, for another channel's message."
  (dolist (opener protean-extras-openers)
    (should-not (car (protean-extras-test--opened opener '(:channel "whatsapp" :ref "x/y"))))))

(ert-deftest protean-extras-test-email-is-rendered-as-mu4e-renders-it ()
  "An email is rendered by mu4e's renderer in a buffer of its own.
mu4e's own view buffer is not touched, and no window is shown."
  (skip-unless (locate-library "mu4e"))
  (let* ((file (make-temp-file "protean-extras-mail-"))
         (windows (window-list)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "From: Ann <ann@example.com>\nTo: me@example.com\n"
                    "Subject: The lease\nMessage-ID: <lease@example.com>\n"
                    "Date: Wed, 18 Nov 2026 17:35:26 +0000\n"
                    "Content-Type: text/plain; charset=utf-8\n\nShall we renew?\n"))
          (cl-letf (((symbol-function 'protean-extras--mu-message)
                     (lambda (id)
                       (should (equal id "lease@example.com"))
                       (list :path file :subject "The lease" :message-id id
                             :from '((:name "Ann" :email "ann@example.com"))
                             :date (current-time) :flags '(seen)))))
            (pcase-let ((`(,taken ,buffer ,reason)
                         (protean-extras-test--opened
                          #'protean-extras-open-email
                          '(:channel "email" :ref "<lease@example.com>"))))
              (should taken)
              (should-not reason)
              (should (eq buffer (get-buffer protean-extras-mail-buffer-name)))
              (with-current-buffer buffer
                (should (derived-mode-p 'mu4e-view-mode))
                (should (string-search "Shall we renew?" (buffer-string))))
              (should (equal windows (window-list))))))
      (delete-file file)
      (when (get-buffer protean-extras-mail-buffer-name)
        (kill-buffer protean-extras-mail-buffer-name)))))

(ert-deftest protean-extras-test-an-unknown-email-is-an-error ()
  "An email mu does not have signals, so Protean shows why."
  (cl-letf (((symbol-function 'protean-extras--mu-message) #'ignore))
    (should-error (protean-extras-open-email '(:channel "email" :ref "<gone@example.com>")
                                             #'ignore))))

(ert-deftest protean-extras-test-slack-opens-the-room-or-the-thread ()
  "A Slack message opens its room at it; a reply opens its thread.
The team is the one set for the account; the buffer goes to DONE once
the message is there."
  (protean-extras-test--with-features '(slack)
    (let ((protean-extras-slack-teams '(("forethought" . "Forethought")))
          (opened nil)
          (went nil))
      (cl-letf (((symbol-function 'protean-extras--slack-team)
                 (lambda (name) (and (equal name "Forethought") 'team)))
                ((symbol-function 'slack-team-connectedp) (lambda (team) (eq team 'team)))
                ((symbol-function 'slack-room-find) (lambda (id _team) (and (equal id "C1") 'room)))
                ((symbol-function 'slack-buffer-goto) (lambda (ts) (setq went ts)))
                ((symbol-function 'slack-open-message--open-channel)
                 (lambda (ts _room _team found _failed)
                   (push (list 'channel ts) opened)
                   (with-temp-buffer (rename-buffer "*slack room*" t) (funcall found))))
                ((symbol-function 'slack-open-message--open-thread)
                 (lambda (_room thread _team found _failed)
                   (push (list 'thread thread) opened)
                   (with-temp-buffer (funcall found)))))
        (pcase-let ((`(,taken ,buffer ,_reason)
                     (protean-extras-test--opened
                      #'protean-extras-open-slack
                      '(:channel "slack" :account "forethought" :ref "C1/100.1" :thread "100.1"))))
          (should taken)
          (should (bufferp buffer))
          (should (equal (car opened) '(channel "100.1")))
          (should (equal went "100.1"))
          ;; The message is the stretch whose `ts' is its timestamp.
          (require 'protean-preview)
          (with-temp-buffer
            (insert (propertize "other\n" 'ts "99.9")
                    (propertize "the message\n" 'ts (copy-sequence "100.1"))
                    (propertize "later\n" 'ts "200.2"))
            (goto-char (point-min))
            (should (equal (funcall protean-extras-test--bounds) '(7 . 19)))))
        (protean-extras-test--opened
         #'protean-extras-open-slack
         '(:channel "slack" :account "forethought" :ref "C1/200.2" :thread "100.1"))
        (should (equal (car opened) '(thread "100.1")))
        (should (equal went "200.2"))
        (should-error (protean-extras-open-slack
                       '(:channel "slack" :account "becas" :ref "C1/1.1") #'ignore))))))

(ert-deftest protean-extras-test-slack-team-is-found-where-emacs-slack-keeps-it ()
  "A team registered with emacs-slack is found by its name."
  (skip-unless (locate-library "slack"))
  (require 'slack)
  (let ((slack-teams-by-token (make-hash-table :test 'equal))
        (team (make-instance 'slack-team :name "Forethought" :token "xoxc-test")))
    (puthash "xoxc-test" team slack-teams-by-token)
    (should (eq team (protean-extras--slack-team "Forethought")))
    (should-not (protean-extras--slack-team "Epoch AI"))))

(ert-deftest protean-extras-test-signal-and-telegram-hand-over-their-buffers ()
  "Signal's message goes to sgn by its id; Telegram's to telega, once shown there."
  (protean-extras-test--with-features '(sgn telega)
    (let ((sgn-buffer (get-buffer-create " *sgn chat*"))
          (telega-buffer (get-buffer-create " *telega chat*"))
          (asked nil))
      (unwind-protect
          (cl-letf (((symbol-function 'sgn-parse-message-id)
                     (lambda (id) (and (equal id "g/s/12") (list "g" "s" 12))))
                    ((symbol-function 'sgn-message-buffer)
                     (lambda (chat sender timestamp)
                       (push (list chat sender timestamp) asked) sgn-buffer))
                    ((symbol-function 'telega-server-live-p) (lambda () t))
                    ((symbol-function 'telega-chat-get) (lambda (id _offline) (and (= id 77) 'chat)))
                    ((symbol-function 'telega-chatbuf--get-create) (lambda (_chat _no) telega-buffer))
                    ((symbol-function 'telega-chatbuf--goto-msg)
                     (lambda (id _highlight callback)
                       (push (list 'goto id) asked) (funcall callback))))
            (should (equal (cdr (protean-extras-test--opened
                                 #'protean-extras-open-signal '(:channel "signal" :ref "g/s/12")))
                           (list sgn-buffer nil)))
            (should (equal (car asked) '("g" "s" 12)))
            (should (equal (cdr (protean-extras-test--opened
                                 #'protean-extras-open-telegram '(:channel "telegram" :ref "77/5")))
                           (list telega-buffer nil)))
            (should (equal (car asked) '(goto 5))))
        (kill-buffer sgn-buffer)
        (kill-buffer telega-buffer)))))

(ert-deftest protean-extras-test-telegram-not-running-is-an-error ()
  "With telega's server down, opening a Telegram message says so."
  (protean-extras-test--with-features '(telega)
    (cl-letf (((symbol-function 'telega-server-live-p) #'ignore))
      (should (string-search "not running"
                             (cadr (should-error (protean-extras-open-telegram
                                                  '(:channel "telegram" :ref "77/5") #'ignore))))))))

(provide 'protean-extras-test)
;;; protean-extras-test.el ends here
