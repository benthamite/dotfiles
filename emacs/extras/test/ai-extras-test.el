;;; ai-extras-test.el --- Tests for ai-extras -*- lexical-binding: t -*-

;; Tests for pure and near-pure helper functions in ai-extras.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-extras)

(defun ai-extras-test--backend (&rest keys)
  "Return a minimal valid backend plist extended with KEYS."
  (append
   keys
   (list :buffer-p (lambda (_buffer) nil)
         :find-all-buffers (lambda () nil)
         :extract-instance-name (lambda (_buffer-name) nil)
         :start-new #'ignore
         :label "Test")))

;;;; Theme sync

(ert-deftest ai-extras-test-sync-theme-dispatches-to-backends ()
  "Dispatch theme sync to all registered backend handlers."
  (let ((ai-extras-backends nil)
        (seen nil))
    (ai-extras-register-backend
     'one
     (ai-extras-test--backend
      :sync-theme (lambda (theme) (push (cons 'one theme) seen))))
    (ai-extras-register-backend
     'two
     (ai-extras-test--backend
      :sync-theme (lambda (theme) (push (cons 'two theme) seen))))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame param)
                 (when (eq param 'background-mode) 'dark))))
      (ai-extras--do-sync-theme t)
      (should (equal (sort seen (lambda (a b)
                                  (string< (symbol-name (car a))
                                           (symbol-name (car b)))))
                     '((one . "dark") (two . "dark")))))))

(ert-deftest ai-extras-test-sync-theme-before-start-respects-toggle ()
  "Do not sync immediately when `ai-extras-sync-theme' is disabled."
  (let ((ai-extras-sync-theme nil)
        (called nil))
    (cl-letf (((symbol-function 'ai-extras--do-sync-theme)
               (lambda () (setq called t))))
      (ai-extras-sync-theme-now)
      (should-not called))))

;;;; Backend registration

(ert-deftest ai-extras-test-register-backend-requires-session-keys ()
  "Reject backend registrations that are missing required keys."
  (let ((ai-extras-backends nil))
    (should-error
     (ai-extras-register-backend 'bad (list :buffer-p #'ignore)))))

;;;; Session keys and display names

(ert-deftest ai-extras-test-ensure-session-keys-assigns-home-row-keys ()
  "Assign home-row keys to all active backend buffers."
  (let ((ai-extras-backends nil)
        (ai-extras--session-keys (make-hash-table :test 'eq)))
    (with-temp-buffer
      (rename-buffer "*one:~/repo/a/:default*" t)
      (let ((one (current-buffer)))
        (with-temp-buffer
          (rename-buffer "*one:~/repo/b/:default*" t)
          (let ((two (current-buffer)))
            (ai-extras-register-backend
             'one
             (ai-extras-test--backend
              :buffer-p (lambda (buf)
                          (string-prefix-p "*one:" (buffer-name buf)))
              :find-all-buffers (lambda () (list one two))))
            (ai-extras--ensure-all-session-keys)
            (should (equal (gethash one ai-extras--session-keys) "a"))
            (should (equal (gethash two ai-extras--session-keys) "s"))))))))

(ert-deftest ai-extras-test-display-name-appends-backend-suffix ()
  "Append backend display suffixes after the shared base name."
  (let ((ai-extras-backends nil)
        (ai-extras--session-keys (make-hash-table :test 'eq)))
    (with-temp-buffer
      (rename-buffer "*one:~/repo/project/:default*" t)
      (let ((buf (current-buffer)))
        (ai-extras-register-backend
         'one
         (ai-extras-test--backend
          :buffer-p (lambda (candidate) (eq candidate buf))
          :find-all-buffers (lambda () (list buf))
          :display-name-suffix (lambda (_buffer) "branch")))
        (should (equal (ai-extras-display-name buf) "project:branch"))))))

(ert-deftest ai-extras-test-session-groups-use-account-key ()
  "Group session switcher suffixes by backend account."
  (let ((ai-extras-backends nil)
        (ai-extras--session-keys (make-hash-table :test 'eq)))
    (with-temp-buffer
      (rename-buffer "*one:~/repo/a/:default*" t)
      (let ((buf (current-buffer)))
        (ai-extras-register-backend
         'one
         (ai-extras-test--backend
          :buffer-p (lambda (candidate) (eq candidate buf))
          :find-all-buffers (lambda () (list buf))
          :account (lambda (_buffer) "work")))
        (puthash buf "a" ai-extras--session-keys)
        (should (equal (mapcar #'car (ai-extras--group-sessions-by-account))
                       '("work")))))))

(ert-deftest ai-extras-test-waiting-face-detects-background-work ()
  "Use the background-work face when the backend reports work."
  (let ((ai-extras-backends nil))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (ai-extras-register-backend
         'one
         (ai-extras-test--backend
          :buffer-p (lambda (candidate) (eq candidate buf))
          :find-all-buffers (lambda () (list buf))
          :has-background-tasks-p (lambda (_buffer) t)))
        (should (eq (ai-extras--waiting-face buf 'one)
                    'ai-extras-waiting-with-background))))))

;;;; Skills

(ert-deftest ai-extras-test-run-skill-distinguishes-backends ()
  "Run the selected backend skill when names collide."
  (let ((ai-extras-backends nil)
        (ran nil))
    (ai-extras-register-backend
     'one
     (ai-extras-test--backend
      :label "One"
      :discover-skills (lambda () (list (list :name "audit")))
      :run-skill (lambda (name args) (setq ran (list 'one name args)))))
    (ai-extras-register-backend
     'two
     (ai-extras-test--backend
      :label "Two"
      :discover-skills (lambda () (list (list :name "audit")))
      :run-skill (lambda (name args) (setq ran (list 'two name args)))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "audit [Two]")))
      (ai-extras-run-skill)
      (should (equal ran '(two "audit" nil))))))

(ert-deftest ai-extras-test-parse-skill-frontmatter-argument-metadata ()
  "Parse shared skill argument metadata from frontmatter."
  (let ((file (make-temp-file "skill" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "---\n")
            (insert "name: convert\n")
            (insert "description: Convert citations\n")
            (insert "argument-hint: FILE\n")
            (insert "argument-choices: a, b\n")
            (insert "argument-default: a\n")
            (insert "argument-multiple: false\n")
            (insert "user-invocable: false\n")
            (insert "model: gpt-5.5\n")
            (insert "---\n"))
          (let ((meta (ai-extras-parse-skill-frontmatter file)))
            (should (equal (plist-get meta :name) "convert"))
            (should (equal (plist-get meta :argument-choices) '("a" "b")))
            (should (equal (plist-get meta :argument-default) "a"))
            (should-not (plist-get meta :argument-multiple))
            (should-not (plist-get meta :user-invocable))
            (should (equal (plist-get meta :model) "gpt-5.5"))))
      (delete-file file))))

(provide 'ai-extras-test)
;;; ai-extras-test.el ends here
