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
  `(let ((slack-teams-by-token (make-hash-table :test 'equal))
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

(provide 'slack-extras-test)
;;; slack-extras-test.el ends here
