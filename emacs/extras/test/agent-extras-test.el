;;; agent-extras-test.el --- Tests for agent-extras -*- lexical-binding: t -*-

;; Tests for the post-push CI command, the project audit and the
;; orchestration run view in agent-extras.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-extras)

(defun agent-extras-test--backend (&rest keys)
  "Return a minimal valid backend plist extended with KEYS."
  (append
   keys
   (list :buffer-p (lambda (_buffer) nil)
         :find-all-buffers (lambda () nil)
         :start-session #'ignore
         :label "Test")))

;;;; Post-push CI

(ert-deftest agent-extras-test-post-push-ci-runs-skill-for-head ()
  "Run post-push CI through the selected backend with the current HEAD."
  (let* ((agent-backends nil)
         (root (make-temp-file "agent-skills" t))
         (skill-file (expand-file-name "post-push-ci/SKILL.md" root))
         ran)
    (unwind-protect
        (progn
          (make-directory (file-name-directory skill-file) t)
          (with-temp-file skill-file
            (insert "---\nname: post-push-ci\n---\nClose the loop.\n"))
          (apply #'agent-register-backend
           'one
           (agent-extras-test--backend
            :skill-roots (lambda () (list (cons root 'file)))
            :run-prompt (cl-function
                         (lambda (prompt &key directory callback)
                           (ignore directory callback)
                           (setq ran prompt)))))
          (cl-letf (((symbol-function 'process-file)
                     (lambda (&rest _args)
                       (insert "abc123\n")
                       0)))
            (agent-extras-post-push-ci)
            (should (string-match-p (regexp-quote skill-file) ran))
            (should (string-match-p "--no-push --commit abc123" ran))))
      (delete-directory root t))))

;;;; Project audit

(ert-deftest agent-extras-test-audit-commits-after-successful-skill ()
  "Auto-commit after each successful audit skill and not after failures."
  (let ((agent-backends nil)
        (agent-extras-audit-skills '("a" "b"))
        commits)
    (apply #'agent-register-backend
     'one
     (agent-extras-test--backend
      :skill-roots (lambda () nil)
      :run-prompt (cl-function
                   (lambda (prompt &key directory callback)
                     (ignore directory)
                     (funcall callback "out"
                              :error (when (string-match-p "/b" prompt)
                                       "exit code 1"))))))
    (cl-letf (((symbol-function 'agent-extras--audit-commit-changes)
               (lambda (_dir title) (push title commits)))
              ((symbol-function 'agent-extras--audit-finish) #'ignore))
      (agent-extras--audit-run-next (list :backend 'one :queue agent-extras-audit-skills
                                   :results nil :dir "/tmp/"
                                   :start-time (current-time))))
    (should (equal commits '("a")))))

(ert-deftest agent-extras-test-audit-strips-leading-slash-from-skill-names ()
  "Resolve legacy slash-format audit skill names like plain names."
  (let ((agent-backends nil)
        prompts)
    (apply #'agent-register-backend
     'one
     (agent-extras-test--backend
      :skill-roots (lambda () nil)
      :run-prompt (cl-function
                   (lambda (prompt &key directory callback)
                     (ignore directory)
                     (push prompt prompts)
                     (funcall callback "out" :error nil)))))
    (cl-letf (((symbol-function 'agent-extras--audit-commit-changes) #'ignore)
              ((symbol-function 'agent-extras--audit-finish) #'ignore))
      (dolist (queue '(("/code-audit") ("code-audit")))
        (agent-extras--audit-run-next (list :backend 'one :queue queue
                                     :results nil :dir "/tmp/"
                                     :start-time (current-time)))))
    (should (equal prompts
                   '("/code-audit --accept" "/code-audit --accept")))))

;;;; Orchestration run view

(defun agent-extras-run-test--state ()
  "Return an independent recorded-run fixture."
  '((version . 3) (stage . "5") (repo . "/tmp/")
    (status . "implementation-active") (active_phase . "implementation")
    (agent1 . ((buffer . "*absent-author*")))
    (agent2 . ((buffer . "*absent-reviewer*")))))

(ert-deftest agent-extras-run-test-refresh-and-error ()
  (let ((file (make-temp-file "agent-extras-run-test-")))
    (unwind-protect
        (with-temp-buffer
          (agent-extras-run-mode)
          (setq agent-extras-run--file file)
          (with-temp-file file (insert (json-encode (agent-extras-run-test--state))))
          (with-temp-file (concat file ".progress") (insert "First\nTesting menus\n"))
          (agent-extras-run--revert)
          (should (string-match-p "Stage 5 — implementation" (buffer-string)))
          (should (string-match-p "Testing menus" (buffer-string)))
          (should-not (string-match-p "First" (buffer-string)))
          (with-temp-file file (insert "invalid"))
          (agent-extras-run--revert)
          (should (string-match-p "Run unavailable" (buffer-string)))
          (should-not (string-match-p "Testing menus" (buffer-string))))
      (delete-file file)
      (delete-file (concat file ".progress")))))

(ert-deftest agent-extras-run-test-attention-not-completion ()
  (with-temp-buffer
    (let ((agent-extras-run--file "/nonexistent/run")
          (run (agent-extras-run-test--state)))
      (setf (alist-get 'status run) "implementation-returned"
            (alist-get 'pending_submission run) '((kind . "steering")))
      (agent-extras-run--render run)
      (should (string-match-p "Recorded status: implementation-returned" (buffer-string)))
      (should (string-match-p "delivery needs reconciliation" (buffer-string)))
      (should (string-match-p "Progress: not published" (buffer-string))))))

(ert-deftest agent-extras-run-test-reused-buffer-not-linked ()
  (with-temp-buffer
    (let* ((buffer (current-buffer))
           (session (agent-session-create :backend 'codex :id "current"
                                          :directory "/tmp/"))
           (actor `((buffer . ,(buffer-name))
                    (identity . ((session_id . "old") (backend . "codex")
                                 (directory . "/tmp"))))))
      (cl-letf (((symbol-function 'agent-session) (lambda (&optional _) session))
                ((symbol-function 'agent-session-buffers) (lambda () (list buffer))))
        (should-not (agent-extras-run--actor-buffer actor))
        (setf (alist-get 'session_id (alist-get 'identity actor)) "current")
        (should (eq buffer (agent-extras-run--actor-buffer actor)))))))

(ert-deftest agent-extras-run-test-unsupported-version ()
  (let ((file (make-temp-file "agent-extras-run-test-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "{\"version\":99}"))
          (should-error (agent-extras-run--read file) :type 'user-error))
      (delete-file file))))

(ert-deftest agent-extras-run-test-timer-cleanup ()
  (let ((buffer (generate-new-buffer " *agent-extras-run-test*")) timer)
    (with-current-buffer buffer
      (agent-extras-run-mode)
      (setq timer (run-with-timer 100 100 #'ignore)
            agent-extras-run--timer timer))
    (kill-buffer buffer)
    (should-not (memq timer timer-list))))

(ert-deftest agent-extras-run-test-mode-change-cleans-timer ()
  (with-temp-buffer
    (agent-extras-run-mode)
    (let ((timer (run-with-timer 100 100 #'ignore)))
      (setq agent-extras-run--timer timer)
      (fundamental-mode)
      (should-not (memq timer timer-list)))))

(ert-deftest agent-extras-run-test-directory-identity ()
  (should (agent-extras-run--same-directory-p "~/" (expand-file-name "~")))
  (should-not (agent-extras-run--same-directory-p "" "/tmp"))
  (should-not (agent-extras-run--same-directory-p "/ssh:host:/tmp" "/tmp")))

;;;; Menu

(ert-deftest agent-extras-test-menu-entries-are-added-once ()
  "Bind the three commands in `agent-menu', and tolerate a second call."
  (agent-extras-add-menu-entries)
  (dolist (binding '(("t a" . agent-extras-audit-project)
                     ("t c" . agent-extras-post-push-ci)
                     ("t o" . agent-extras-run-open)))
    (should (eq (plist-get (cdr (transient-get-suffix 'agent-menu (car binding)))
                           :command)
                (cdr binding)))))

(provide 'agent-extras-test)
;;; agent-extras-test.el ends here
