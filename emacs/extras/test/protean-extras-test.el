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
    (should-not (car (protean-extras-test--opened opener '(:channel "carrier-pigeon" :ref "x/y"))))))

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

;; Stagers.

(defun protean-extras-test--staged (stager message text)
  "Call STAGER on MESSAGE and TEXT; return (TAKEN BUFFER REASON) as it answered."
  (let* ((answer nil)
         (taken (funcall stager message text
                         (lambda (buffer &optional reason)
                           (setq answer (list buffer reason))))))
    (cons taken answer)))

(ert-deftest protean-extras-test-each-stager-leaves-other-channels ()
  "A stager returns nil, and answers nothing, for another channel's message."
  (dolist (stager protean-extras-stagers)
    (should-not (funcall stager '(:channel "carrier-pigeon" :ref "x") "Hi"
                         (lambda (&rest _) (error "Answered"))))))

(ert-deftest protean-extras-test-whatsapp-opens-in-wsp ()
  "A WhatsApp message goes to wsp by its reference, and is marked by its id."
  (protean-extras-test--with-features '(wsp)
    (let ((chat (get-buffer-create " *wsp chat*")))
      (unwind-protect
          (cl-letf (((symbol-function 'wsp-message-buffer)
                     (lambda (ref)
                       (should (equal ref "123@s.whatsapp.net/ABC"))
                       (with-current-buffer chat
                         (erase-buffer)
                         (insert (propertize "other\n" 'wsp-id "XYZ")
                                 (propertize "the message\n" 'wsp-id "ABC"))
                         (goto-char 7))
                       chat)))
            (pcase-let ((`(,taken ,buffer ,_reason)
                         (protean-extras-test--opened
                          #'protean-extras-open-whatsapp
                          '(:channel "whatsapp" :ref "123@s.whatsapp.net/ABC"))))
              (should taken)
              (should (eq buffer chat))
              (require 'protean-preview)
              (with-current-buffer chat
                (should (equal (funcall protean-extras-test--bounds) '(7 . 19))))))
        (kill-buffer chat)))))

(ert-deftest protean-extras-test-whatsapp-takes-a-reply-and-keeps-what-is-typed ()
  "The draft goes in wsp's input as a reply to the message, with no typing sent.
Input already there is left alone, and the stager says why."
  (protean-extras-test--with-features '(wsp)
    (let ((chat (get-buffer-create " *wsp chat*"))
          (input "")
          (replied nil)
          (typing 'unset))
      (unwind-protect
          (cl-letf (((symbol-function 'wsp-message-buffer) (lambda (_ref) chat))
                    ((symbol-function 'wsp-chat--input-text) (lambda () input))
                    ((symbol-function 'wsp-chat-reply) (lambda () (setq replied t)))
                    ((symbol-function 'wsp-chat--set-input)
                     (lambda (text) (setq input text typing wsp-send-typing))))
            (let ((wsp-send-typing t))
              (should (equal (cdr (protean-extras-test--staged
                                   #'protean-extras-stage-whatsapp
                                   '(:channel "whatsapp" :ref "c/1") "Sí, mañana."))
                             (list chat nil)))
              (should replied)
              (should (equal input "Sí, mañana."))
              (should-not typing)
              (setq replied nil)
              (pcase-let ((`(,taken ,buffer ,reason)
                           (protean-extras-test--staged
                            #'protean-extras-stage-whatsapp
                            '(:channel "whatsapp" :ref "c/1") "Otra cosa.")))
                (should taken)
                (should-not buffer)
                (should (string-search "already has text" reason))
                (should-not replied)
                (should (equal input "Sí, mañana.")))))
        (kill-buffer chat)))))

(ert-deftest protean-extras-test-signal-takes-a-reply ()
  "The draft goes in sgn's input as a reply to the message."
  (protean-extras-test--with-features '(sgn)
    (let ((chat (get-buffer-create " *sgn chat*"))
          (input "")
          (replied nil))
      (unwind-protect
          (cl-letf (((symbol-function 'sgn-parse-message-id) (lambda (_id) (list "g" "s" 12)))
                    ((symbol-function 'sgn-message-buffer) (lambda (&rest _) chat))
                    ((symbol-function 'sgn-chat--input-text) (lambda () input))
                    ((symbol-function 'sgn-reply) (lambda () (setq replied t)))
                    ((symbol-function 'sgn-chat--set-input) (lambda (text) (setq input text))))
            (should (equal (cdr (protean-extras-test--staged
                                 #'protean-extras-stage-signal
                                 '(:channel "signal" :ref "g/s/12") "Yes."))
                           (list chat nil)))
            (should replied)
            (should (equal input "Yes.")))
        (kill-buffer chat)))))

(ert-deftest protean-extras-test-slack-replies-in-the-thread-or-the-room ()
  "A message in a thread is answered in the thread's compose buffer, any other in
the room's; a compose buffer with text in it is left alone."
  (protean-extras-test--with-features '(slack)
    (let ((protean-extras-slack-teams '(("forethought" . "Forethought")))
          (compose (get-buffer-create " *slack compose*"))
          (made nil))
      (unwind-protect
          (cl-letf (((symbol-function 'protean-extras--slack-team) (lambda (_name) 'team))
                    ((symbol-function 'slack-team-connectedp) (lambda (_team) t))
                    ((symbol-function 'slack-room-find) (lambda (_id _team) 'room))
                    ((symbol-function 'slack-create-thread-message-compose-buffer)
                     (lambda (_room ts _team) (push (list 'thread ts) made) 'object))
                    ((symbol-function 'slack-create-room-message-compose-buffer)
                     (lambda (_room _team) (push (list 'room) made) 'object))
                    ((symbol-function 'slack-buffer-buffer) (lambda (_object) compose)))
            (should (equal (cdr (protean-extras-test--staged
                                 #'protean-extras-stage-slack
                                 '(:channel "slack" :account "forethought" :ref "C1/100.1"
                                            :thread "100.1")
                                 "Done, Ben."))
                           (list compose nil)))
            (should (equal (car made) '(thread "100.1")))
            (should (equal (with-current-buffer compose (buffer-string)) "Done, Ben."))
            (with-current-buffer compose (erase-buffer))
            (protean-extras-test--staged
             #'protean-extras-stage-slack
             '(:channel "slack" :account "forethought" :ref "C1/300.3") "Hi all.")
            (should (equal (car made) '(room)))
            (should-error (protean-extras-stage-slack
                           '(:channel "slack" :account "forethought" :ref "C1/300.3")
                           "Again." #'ignore))
            (should (equal (with-current-buffer compose (buffer-string)) "Hi all.")))
        (kill-buffer compose)))))

(ert-deftest protean-extras-test-a-client-that-cannot-open-the-message-says-why ()
  "When the message does not open, the stager gives its opener's reason."
  (protean-extras-test--with-features '(telega)
    (cl-letf (((symbol-function 'protean-extras-open-telegram)
               (lambda (_message done) (funcall done nil "Telegram has no chat 77") t)))
      (should (equal (cdr (protean-extras-test--staged
                           #'protean-extras-stage-telegram
                           '(:channel "telegram" :ref "77/5") "Hi."))
                     (list nil "Telegram has no chat 77"))))))

(provide 'protean-extras-test)
;;; protean-extras-test.el ends here
