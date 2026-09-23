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

(provide 'slack-extras-test)
;;; slack-extras-test.el ends here
