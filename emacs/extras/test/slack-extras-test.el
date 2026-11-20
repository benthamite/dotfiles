;;; slack-extras-test.el --- Local Slack context tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-extras)

(defun slack-extras-test--activity-data (room &optional thread)
  "Return captured metadata for fixture ROOM, optionally in a THREAD buffer."
  (let* ((team (slack-team :id "T-AW-TEST" :name "AW test workspace"))
         (slack-buffer--team-cache (make-hash-table :test 'eq))
         (slack-teams-by-token (make-hash-table :test 'equal))
         (slack-current-buffer
          (if thread
              (slack-thread-message-buffer :team-id nil :room-id (oref room id)
                                           :thread-ts "123.456" :has-more nil)
            (slack-message-buffer :team-id nil :room-id (oref room id)))))
    (puthash (oref room id) room (oref team channels))
    (slack-buffer-cache-team slack-current-buffer team)
    (slack-extras-activity-watch-data)))

(ert-deftest slack-extras-activity-watch-channel-and-thread ()
  (dolist (thread '(nil t))
    (let* ((room (slack-channel :id "C-AW-TEST" :name "aw-test"))
           (data (slack-extras-test--activity-data room thread)))
      (should (equal (alist-get 'channel data) "aw-test"))
      (should (equal (alist-get 'channel_id data) "C-AW-TEST"))
      (should (equal (alist-get 'workspace_id data) "T-AW-TEST"))
      (should (equal (alist-get 'workspace data) "AW test workspace")))))

(ert-deftest slack-extras-activity-watch-private-channel ()
  (let* ((room (slack-group :id "G-AW-TEST" :name "private-aw-test"))
         (data (slack-extras-test--activity-data room)))
    (should (equal (alist-get 'channel data) "private-aw-test"))))

(ert-deftest slack-extras-activity-watch-direct-messages-retain-workspace ()
  (dolist (room (list (slack-im :id "D-AW-TEST")
                      (slack-group :id "G-AW-TEST" :name "aw-test"
                                   :is_mpim t)
                      (slack-channel :id "C-AW-TEST" :name "aw-test"
                                     :is_im t)))
    (let ((data (slack-extras-test--activity-data room)))
      (should (equal (alist-get 'app data) "Slack"))
      (should (equal (alist-get 'workspace data) "AW test workspace"))
      (should-not (assq 'channel data))
      (should-not (assq 'channel_id data)))))

(ert-deftest slack-extras-activity-watch-excludes-non-room-context ()
  (with-temp-buffer
    (let ((slack-current-buffer nil))
      (should-not (slack-extras-activity-watch-data)))))

;;;; Team registration

(defmacro slack-extras-test--with-registry (&rest body)
  "Run BODY with empty Slack team registries and stubbed network calls.
Credentials resolve to token \"xoxc-new\", and registering a team adds it to
`slack-teams-by-token' without connecting."
  (declare (indent 0))
  `(let ((slack-current-team nil)
         (slack-teams-by-token (make-hash-table :test 'equal))
         (slack-tokens-by-id (make-hash-table :test 'equal))
         (slack-buffer--team-cache (make-hash-table :test 'eq)))
     (cl-letf (((symbol-function 'slack-extras-chrome-credentials)
                (lambda (_domain) (cons "xoxc-new" "cookie")))
               ((symbol-function 'slack-team-connectedp) #'ignore)
               ((symbol-function 'slack-register-team)
                (lambda (&rest plist)
                  (puthash (plist-get plist :token)
                           (slack-team :name (plist-get plist :name)
                                       :token (plist-get plist :token))
                           slack-teams-by-token))))
       ,@body)))

(defun slack-extras-test--register (token)
  "Register a test team called \"Forethought\" under TOKEN and return it."
  (let ((team (slack-team :id "T-REG-TEST" :name "Forethought" :token token)))
    (puthash token team slack-teams-by-token)
    (puthash "T-REG-TEST" token slack-tokens-by-id)
    team))

(ert-deftest slack-extras-register-team-removes-every-stale-copy ()
  (slack-extras-test--with-registry
    (slack-extras-test--register "xoxc-old")
    (slack-extras-test--register "xoxc-older")
    (slack-extras--register-team '(:name "Forethought" :domain "ft.slack.com"))
    (should (equal (hash-table-keys slack-teams-by-token) '("xoxc-new")))
    (should-not (gethash "T-REG-TEST" slack-tokens-by-id))))

(ert-deftest slack-extras-register-team-kills-stale-team-buffers ()
  (slack-extras-test--with-registry
    (let ((stale (slack-extras-test--register "xoxc-old"))
          (buffer (generate-new-buffer " *slack-extras-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local slack-current-buffer
                          (slack-message-buffer :team-id "T-REG-TEST"
                                                :room-id "C-REG-TEST")))
            (slack-extras--register-team
             '(:name "Forethought" :domain "ft.slack.com"))
            (should-not (buffer-live-p buffer))
            (should-not (memq stale (hash-table-values slack-teams-by-token))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest slack-extras-register-team-replaces-stale-current-team ()
  (slack-extras-test--with-registry
    (setq slack-current-team (slack-extras-test--register "xoxc-old"))
    (slack-extras--register-team '(:name "Forethought" :domain "ft.slack.com"))
    (should (equal (oref slack-current-team token) "xoxc-new"))))

;;;; Reachability before connecting

(defmacro slack-extras-test--with-team (team ws &rest body)
  "Run BODY with TEAM registered and WS its websocket, probes stubbed.
In BODY, `probed' holds the probed URLs, `pending' the probe's callback,
`opened' the arguments `slack-ws-open' got, and `reconnects' the number of
reconnects scheduled."
  (declare (indent 2))
  `(let* ((slack-teams-by-token (make-hash-table :test 'equal))
          (slack-tokens-by-id (make-hash-table :test 'equal))
          (,ws (slack-team-ws))
          (,team (slack-team :id "T-RE-TEST" :token "xoxc-test"))
          probed opened (reconnects 0) pending)
     (oset ,team ws ,ws)
     (puthash "xoxc-test" ,team slack-teams-by-token)
     (puthash "T-RE-TEST" "xoxc-test" slack-tokens-by-id)
     (cl-letf (((symbol-function 'simple-extras-probe-url)
                (lambda (url _key callback)
                  (push url probed)
                  (setq pending callback)))
               ((symbol-function 'slack-ws-reconnect)
                (lambda (&rest _) (cl-incf reconnects)))
               ((symbol-function 'slack-log) #'ignore))
       ,@body)))

(ert-deftest slack-extras-open-when-reachable-opens-after-probe ()
  "Probe the host of the given URL without its token, then open."
  (slack-extras-test--with-team team ws
    (slack-extras--open-when-reachable
     (lambda (&rest a) (setq opened a)) ws team
     :ws-url "wss://wss-backup.slack.com/link/?token=secret")
    (should (equal probed '("https://wss-backup.slack.com/")))
    (should-not opened)
    (funcall pending t)
    (should (equal opened (list ws team :ws-url
                                "wss://wss-backup.slack.com/link/?token=secret")))
    (should (= reconnects 0))))

(ert-deftest slack-extras-open-when-reachable-default-host ()
  "Probe the default websocket host when no URL is given."
  (slack-extras-test--with-team team ws
    (slack-extras--open-when-reachable #'ignore ws team)
    (should (equal probed '("https://wss-primary.slack.com/")))))

(ert-deftest slack-extras-open-when-reachable-retries-when-down ()
  "Schedule a reconnect instead of opening when the host does not answer."
  (slack-extras-test--with-team team ws
    (slack-extras--open-when-reachable
     (lambda (&rest a) (setq opened a)) ws team)
    (funcall pending nil)
    (should-not opened)
    (should (= reconnects 1))))

(ert-deftest slack-extras-open-when-reachable-drops-stale-calls ()
  "Neither open nor reconnect once the open is no longer wanted."
  (dolist (change (list (lambda (_team _ws) (remhash "xoxc-test" slack-teams-by-token))
                        (lambda (team _ws) (oset team ws (slack-team-ws)))
                        (lambda (_team ws) (oset ws inhibit-reconnection t))
                        (lambda (_team ws) (oset ws connected t))
                        (lambda (_team ws) (oset ws conn 'newer))))
    (dolist (result '(t nil))
      (slack-extras-test--with-team team ws
        (slack-extras--open-when-reachable
         (lambda (&rest a) (setq opened a)) ws team)
        (funcall change team ws)
        (funcall pending result)
        (should-not opened)
        (should (= reconnects 0))))))

;;;; Keeping to-dos in step with Protean's tasks

(defconst slack-extras-test--todo-item
  '(:ticket 7 :workspace "forethought" :channel "C12345678"
    :ts "1710000000.000001" :mark "1700")
  "A to-do as `protean-slack-todo-function' passes it.")

(defun slack-extras-test--todo-running-p ()
  "Return non-nil while a to-do write is running."
  (seq-some (lambda (process)
              (string-prefix-p "slack-extras-todo" (process-name process)))
            (process-list)))

(defmacro slack-extras-test--with-fake-client (exit-code &rest body)
  "Run BODY with a fake Slack client that exits with EXIT-CODE.
Return the arguments the client got, one call per line, once every
write BODY started has ended."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "slack-extras-todo" t))
          (args-file (expand-file-name "args" dir))
          (slack-extras-slack-program (expand-file-name "fake-slack" dir))
          (slack-extras--todo-attempts (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (with-temp-file slack-extras-slack-program
             (insert (format "#!/bin/sh\necho \"$@\" >> %s\necho '{\"status\": \"done\"}'\nexit %d\n"
                             (shell-quote-argument args-file) ,exit-code)))
           (set-file-modes slack-extras-slack-program #o700)
           ,@body
           (with-timeout (10 (error "Fake client did not finish"))
             (while (slack-extras-test--todo-running-p)
               (accept-process-output nil 0.05)))
           (when (file-exists-p args-file)
             (with-temp-buffer (insert-file-contents args-file)
                               (string-trim (buffer-string)))))
       (delete-directory dir t))))

(ert-deftest slack-extras-todo-complete-runs-complete-todo ()
  (should (equal (slack-extras-test--with-fake-client 0
                   (slack-extras-protean-slack-todo slack-extras-test--todo-item 'complete))
                 "-w forethought complete-todo C12345678 1710000000.000001 --mark=1700")))

(ert-deftest slack-extras-todo-restore-runs-uncomplete-todo ()
  (should (equal (slack-extras-test--with-fake-client 0
                   (slack-extras-protean-slack-todo slack-extras-test--todo-item 'restore))
                 "-w forethought uncomplete-todo C12345678 1710000000.000001")))

(ert-deftest slack-extras-todo-repeated-request-waits-for-retry-time ()
  (should (equal (slack-extras-test--with-fake-client 1
                   (dotimes (_ 3)
                     (slack-extras-protean-slack-todo slack-extras-test--todo-item 'complete)))
                 "-w forethought complete-todo C12345678 1710000000.000001 --mark=1700")))

(ert-deftest slack-extras-todo-request-after-retry-time-runs-again ()
  (let ((slack-extras-todo-retry-seconds 0))
    (should (equal (length (split-string
                            (slack-extras-test--with-fake-client 1
                              (dotimes (_ 2)
                                (slack-extras-protean-slack-todo
                                 slack-extras-test--todo-item 'complete)))
                            "\n"))
                   2))))

(ert-deftest slack-extras-todo-complete-and-restore-are-separate-writes ()
  (should (equal (length (split-string
                          (slack-extras-test--with-fake-client 0
                            (slack-extras-protean-slack-todo slack-extras-test--todo-item 'complete)
                            (slack-extras-protean-slack-todo slack-extras-test--todo-item 'restore))
                          "\n"))
                 2)))

(ert-deftest slack-extras-todo-ignores-what-names-no-write ()
  (should-not (slack-extras-test--with-fake-client 0
                (slack-extras-protean-slack-todo slack-extras-test--todo-item 'delete)
                (slack-extras-protean-slack-todo '(:ticket 7 :workspace "forethought") 'complete)
                (slack-extras-protean-slack-todo
                 (plist-put (copy-sequence slack-extras-test--todo-item) :mark "") 'complete))))

(provide 'slack-extras-test)
;;; slack-extras-test.el ends here
